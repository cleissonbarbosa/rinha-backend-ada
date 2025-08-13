with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Streams; use Ada.Streams;
with UDP_Socket_Utils; use UDP_Socket_Utils;
with Properties; use Properties;
with LB_Event_Types;
with Waiter;
with Payment_DB; use Payment_DB;
with Payment_Processor_Client; use Payment_Processor_Client;
with Ada.Numerics.Discrete_Random;
with Payment_Queue;
with Ada.Exceptions;

package body Backend_UDP_Router is
   Socket_In       : Socket_Type;  -- binds UDP_CHANNEL_PORT for LB
   Socket_Internal : Socket_Type;  -- binds UDP_CHANNEL_INTERNAL_PORT for peer
   Peer_Addr       : Sock_Addr_Type; -- EXTERNAL_UDP_HOST:EXTERNAL_UDP_PORT

   Peer_Wait : Waiter.Single_Slot;

   -- Random number generation for UUID
   subtype Hex_Digit is Natural range 0 .. 15;
   package Random_Hex is new Ada.Numerics.Discrete_Random (Hex_Digit);
   Gen : Random_Hex.Generator;

   function Generate_Valid_UUID return String is
      Hex_Chars : constant String := "0123456789abcdef";
      UUID : String (1 .. 36);
   begin
      -- Don't reset the generator here, it's done once in Start
      
      -- Generate hex digits and insert hyphens at correct positions
      for I in 1 .. 36 loop
         if I = 9 or I = 14 or I = 19 or I = 24 then
            UUID (I) := '-';
         else
            UUID (I) := Hex_Chars (Random_Hex.Random (Gen) + 1);
         end if;
      end loop;
      
      return UUID;
   end Generate_Valid_UUID;

   procedure Start is
   begin
      Put_Line ("Initializing UDP sockets...");
      -- Initialize random generator for UUID generation with time-based seed
      Random_Hex.Reset (Gen);
      UDP_Socket_Utils.Init_Sockets;
      Put_Line ("Setting up bound socket for LB communication on port" & Integer'Image (Udp_Channel_Port));
      Setup_Bound_Socket (Socket_In, Udp_Channel_Port);
      Put_Line ("Setting up bound socket for peer communication on port" & Integer'Image (Udp_Channel_Internal_Port));
      Setup_Bound_Socket (Socket_Internal, Udp_Channel_Internal_Port);
      Put_Line ("Building peer address: " & External_Udp_Host & ":" & Integer'Image (External_Udp_Port));
      Peer_Addr := Build_Addr (External_Udp_Host, External_Udp_Port);
      
      -- Initialize payment queue system (Java-style)
      Put_Line ("Initializing payment queue system...");
      Payment_Queue.Initialize;
      Payment_Queue.Start_Workers;
      
      Put_Line ("Backend UDP Router started on LB:" & Integer'Image (Udp_Channel_Port)
                & " peer:" & Integer'Image (Udp_Channel_Internal_Port));
      -- Small delay to ensure sockets are properly initialized
      delay 0.1;
   end Start;

   function Extract_Amount (JSON : String) return Long_Long_Float is
      Pos : Natural := Index (JSON, '"' & "amount" & '"');
   begin
      if Pos = 0 then
         return 0.0;
      end if;
      declare
         Rest : constant String := JSON (Pos .. JSON'Last);
         Colon : Natural := Index (Rest, ":");
         Sub   : String := Rest (Colon + 1 .. Rest'Last);
         -- trim non-number leading spaces
         I     : Natural := Sub'First;
      begin
         while I <= Sub'Last and then (Sub (I) = ' ' or else Sub (I) = '"') loop
            I := I + 1;
         end loop;
         declare
            J : Natural := I;
         begin
            while J <= Sub'Last and then (Sub (J) in '0' .. '9' or else Sub (J) = '.' ) loop
               J := J + 1;
            end loop;
            return Long_Long_Float'Value (Sub (I .. J - 1));
         end;
      end;
   exception
      when others => return 0.0;
   end Extract_Amount;

   function Is_Valid_UUID (UUID : String) return Boolean is
   begin
      -- UUID format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx (36 chars)
      if UUID'Length /= 36 then
         return False;
      end if;
      
      -- Check positions of hyphens
      if UUID (UUID'First + 8) /= '-' or
         UUID (UUID'First + 13) /= '-' or
         UUID (UUID'First + 18) /= '-' or
         UUID (UUID'First + 23) /= '-' then
         return False;
      end if;
      
      -- Check that other characters are hex digits
      for I in UUID'Range loop
         if I /= UUID'First + 8 and I /= UUID'First + 13 and 
            I /= UUID'First + 18 and I /= UUID'First + 23 then
            if not (UUID (I) in '0' .. '9' or UUID (I) in 'a' .. 'f' or UUID (I) in 'A' .. 'F') then
               return False;
            end if;
         end if;
      end loop;
      
      return True;
   end Is_Valid_UUID;

   function Extract_Correlation (JSON : String) return String is
      Key : constant String := '"' & "correlationId" & '"';
      Pos : Natural := Index (JSON, Key);
   begin
      if Pos = 0 then
         -- No correlationId found - this should be an error according to specs
         -- But for now return empty string to indicate missing
         return "";
      end if;
      declare
         Rest   : constant String := JSON (Pos .. JSON'Last);
         Colon  : Natural := Index (Rest, ":");
         First_Q : Natural := Index (Rest, """", From => Colon + 1);
         Second_Q: Natural := (if First_Q = 0 then 0 else Index (Rest, """", From => First_Q + 1));
      begin
         if First_Q = 0 or else Second_Q = 0 then
            -- Invalid format
            return "";
         else
            declare
               Extracted_UUID : constant String := Rest (First_Q + 1 .. Second_Q - 1);
            begin
               -- Validate the extracted UUID
               if Is_Valid_UUID (Extracted_UUID) then
                  return Extracted_UUID;
               else
                  -- Invalid UUID
                  return "";
               end if;
            end;
         end if;
      end;
   exception
      when others => 
         -- Error occurred, return empty string
         return "";
   end Extract_Correlation;

   function Build_Payment_JSON (Correlation : String; Amount : Long_Long_Float) return String is
      function Trim_Img (X : Long_Long_Float) return String is
         I : constant String := Long_Long_Float'Image (X);
      begin
         if I'Length > 0 and then I (I'First) = ' ' then
            return I (I'First + 1 .. I'Last);
         else
            return I;
         end if;
      end Trim_Img;
   begin
      return "{""correlationId"":""" & Correlation & """,""amount"":" & Trim_Img (Amount) &
             ",""requestedAt"":""2000-01-01T00:00:00.000Z""}";
   end Build_Payment_JSON;

   procedure Process_Incoming is
   Buf   : Stream_Element_Array (1 .. 4096);
   Last  : Stream_Element_Offset := 0;
      From  : Sock_Addr_Type;

      -- Start peer listener task
      task Peer_Receiver;
      task body Peer_Receiver is
         PBuf   : Stream_Element_Array (1 .. 4096);
         PLast  : Stream_Element_Offset := 0;
         PFrom  : Sock_Addr_Type;
      begin
         Put_Line ("Peer_Receiver task started");
         loop
            -- receive into stream buffer
            Receive_Socket (Socket_Internal, PBuf, PLast, PFrom);
            Put_Line ("Backend received peer message, length:" & Natural'Image(Natural(PLast)));
            declare
               Len : constant Natural := Natural (PLast);
               S   : String (1 .. Len);
            begin
               for I in 1 .. Len loop
                  S (I) := Character'Val (Integer (PBuf (Stream_Element_Offset (I))));
               end loop;
               Put_Line ("Peer message content: " & S);
               declare
                  Kind : constant Character := S (S'Last);
               begin
                  Put_Line ("Peer message type: " & Kind);
                  if Kind = LB_Event_Types.PAYMENT_SUMMARY then
                     Put_Line ("Processing peer PAYMENT_SUMMARY request");
                     -- Peer asked our summary; reply with merge payload 'b' as CSV
                     declare
                        Sum : constant Summary := DB.Get_Summary;
                        CSV : constant String := Integer'Image (Sum.Default_Total_Requests) & "," & Long_Long_Float'Image (Sum.Default_Total_Amount) & "," &
                                                  Integer'Image (Sum.Fallback_Total_Requests) & "," & Long_Long_Float'Image (Sum.Fallback_Total_Amount) & "b";
                     begin
                        Put_Line ("Sending CSV response to peer: " & CSV);
                        Send_To (Socket_Internal, CSV, Peer_Addr);
                     end;
                  elsif Kind = LB_Event_Types.PAYMENT_SUMMARY_MERGE then
                     Put_Line ("Processing peer PAYMENT_SUMMARY_MERGE response");
                     -- Complete waiter
                     Peer_Wait.Put (S);
                     Put_Line ("Completed peer wait with response");
                  elsif Kind = LB_Event_Types.PURGE then
                     Put_Line ("Processing peer PURGE request");
                     DB.Purge;
                  end if;
               end;
            end;
         end loop;
      end Peer_Receiver;
   begin

      Put_Line ("Backend entering main message processing loop...");
      loop
         -- Remove verbose "waiting" message for performance
         Receive_Socket (Socket_In, Buf, Last, From);
         -- Only log when actually processing, not for every message
         declare
            Len : constant Natural := Natural (Last);
            S   : String (1 .. Len);
            Kind : Character;
         begin
            for I in 1 .. Len loop
               S (I) := Character'Val (Integer (Buf (Stream_Element_Offset (I))));
            end loop;
            Kind := S (S'Last);
            -- Remove verbose logging for performance under load
            declare
               Payload : constant String := S (S'First .. S'Last - 1);
               Correlation_Id : constant String := Extract_Correlation (Payload);
               Amount : constant Long_Long_Float := Extract_Amount (Payload);
               Enhanced_Payload : String := Build_Payment_JSON (Correlation_Id, Amount);
            begin
            if Kind = LB_Event_Types.PAYMENT_POST then
               -- Validate that correlationId was successfully extracted
               if Correlation_Id = "" then
                  -- Send error acknowledgment to LB for missing/invalid correlationId
                  declare
                     LB_Reply_Addr : Sock_Addr_Type;
                  begin
                     LB_Reply_Addr.Addr := From.Addr;  -- Same IP as sender
                     LB_Reply_Addr.Port := 9998;       -- LB listening port
                     Send_To (Socket_In, "ERR", LB_Reply_Addr);
                  end;
               else
                  -- Fast enqueue without logging
                  -- Enqueue payment for asynchronous processing (Java-style)
                  begin
                     Payment_Queue.Enqueue_Payment (Enhanced_Payload);
                  exception
                     when E : others =>
                        null; -- Silent failure for performance
                  end;
                  -- Send immediate acknowledgment to LB (like Java)
                  declare
                     LB_Reply_Addr : Sock_Addr_Type;
                  begin
                     LB_Reply_Addr.Addr := From.Addr;  -- Same IP as sender
                     LB_Reply_Addr.Port := 9998;       -- LB listening port
                     Send_To (Socket_In, "ACK", LB_Reply_Addr);
                  end;
               end if;
            elsif Kind = LB_Event_Types.PURGE then
               DB.Purge;
               -- propagate to peer
               Send_To (Socket_Internal, String'(1 => LB_Event_Types.PURGE), Peer_Addr);
            elsif Kind = LB_Event_Types.PAYMENT_SUMMARY then
               Put_Line ("Backend processing PAYMENT_SUMMARY request");
               -- ask peer and await CSV, merge and reply JSON to LB
               Put_Line ("Sending summary request to peer");
               Send_To (Socket_Internal, Payload & LB_Event_Types.PAYMENT_SUMMARY, Peer_Addr);
               Put_Line ("Waiting for peer response...");
               declare
                  Res : Unbounded_String;
               begin
                  Peer_Wait.Take (Res);
                  Put_Line ("Received peer response: " & To_String(Res));
                  declare
                     CSV : constant String := To_String (Res);
                     -- parse CSV: dReq,dAmt,fReq,fAmt,'b'
                     P1  : Natural := Index (CSV, ",");
                     P2  : Natural := Index (CSV, ",", P1 + 1);
                     P3  : Natural := Index (CSV, ",", P2 + 1);
                     DReq_Peer : Integer := Integer'Value (CSV (CSV'First .. P1 - 1));
                     DAmt_Peer : Long_Long_Float := Long_Long_Float'Value (CSV (P1 + 1 .. P2 - 1));
                     FReq_Peer : Integer := Integer'Value (CSV (P2 + 1 .. P3 - 1));
                     FAmt_Peer : Long_Long_Float := Long_Long_Float'Value (CSV (P3 + 1 .. CSV'Last - 1));
                     Sum_Self  : constant Summary := DB.Get_Summary;
                     Merged    : Summary := Sum_Self;
                  begin
                     Merged.Default_Total_Requests  := Sum_Self.Default_Total_Requests + DReq_Peer;
                     Merged.Default_Total_Amount    := Sum_Self.Default_Total_Amount + DAmt_Peer;
                     Merged.Fallback_Total_Requests := Sum_Self.Fallback_Total_Requests + FReq_Peer;
                     Merged.Fallback_Total_Amount   := Sum_Self.Fallback_Total_Amount + FAmt_Peer;
                     declare
                        Reply : constant String := Summary_To_JSON (Merged);
                        LB_Addr : Sock_Addr_Type;
                     begin
                        Put_Line ("Sending JSON reply to LB: " & Reply);
                        Put_Line ("Original LB address: " & Image(From.Addr) & ":" & Port_Type'Image(From.Port));
                        -- Respond to LB listening port (9998), not the ephemeral port
                        LB_Addr.Addr := From.Addr;  -- Same IP
                        LB_Addr.Port := 9998;       -- LB listening port
                        Put_Line ("Sending to LB listening port: " & Image(LB_Addr.Addr) & ":9998");
                        Send_To (Socket_In, Reply, LB_Addr);
                        Put_Line ("Reply sent to LB successfully");
                     end;
                  end;
               end;
            end if;
            end;
         end;
      end loop;
   end Process_Incoming;

   procedure Send_Summary_To_LB (Payload : String; Address : Sock_Addr_Type) is
      Sock : Socket_Type;
   begin
      Create_Socket (Sock, Family_Inet, Socket_Datagram);
      Connect_Socket (Sock, Address);
      Send_Bytes (Sock, Payload);
      Close_Socket (Sock);
   end Send_Summary_To_LB;

end Backend_UDP_Router;
