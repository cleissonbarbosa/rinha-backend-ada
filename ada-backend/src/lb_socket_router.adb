with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with UDP_Socket_Utils; use UDP_Socket_Utils;
with GNAT.Sockets; use GNAT.Sockets;
with Properties; use Properties;
with LB_Event_Types;
with Waiter;
with Ada.Exceptions;
with Ada.Calendar; use Ada.Calendar;

package body LB_Socket_Router is
   package U renames UDP_Socket_Utils;

   Socket_Unified : Socket_Type;  -- ONE socket for both send/receive (like Java)
   LB_Wait        : Waiter.Single_Slot;
   
   -- Global round-robin counter (persistent between calls)
   Global_RR_Index : Natural := 0;

   procedure Start is
   begin
      U.Init_Sockets;
      -- Create unified socket like Java DatagramSocket() - bound to receive port but can send anywhere
      Create_Socket (Socket_Unified, Family_Inet, Socket_Datagram);
      Set_Socket_Option (Socket_Unified, Socket_Level, (Reuse_Address, True));
      -- Increase buffer sizes for better performance
      Set_Socket_Option (Socket_Unified, Socket_Level, (Send_Buffer, 1024 * 1024));
      Set_Socket_Option (Socket_Unified, Socket_Level, (Receive_Buffer, 1024 * 1024));
      -- Bind to port 9998 for receiving (like Java implicitly does)
      declare
         Addr : Sock_Addr_Type;
      begin
         Addr.Addr := Any_Inet_Addr;
         Addr.Port := Port_Type (9998);
         Bind_Socket (Socket_Unified, Addr);
      end;
      Put_Line ("LB socket router started");
   end Start;

   procedure Send_To_Any_Backend (Data : String) is
   begin
      -- Fast round-robin with debug logs
      declare
         Current_Index : constant Natural := Global_RR_Index;
         Host  : constant String := (if Current_Index mod 2 = 0 then Properties.Backend1_Host else Properties.Backend2_Host);
         Port  : constant Positive := (if Current_Index mod 2 = 0 then Properties.Backend1_Port else Properties.Backend2_Port);
         Dest  : constant Sock_Addr_Type := U.Build_Addr (Host, Port);
      begin
         Global_RR_Index := Global_RR_Index + 1;
         -- Optimized for performance - minimal logging
         declare
            Addr : constant Sock_Addr_Type := U.Build_Addr (Host, Port);
         begin
            U.Send_To (Socket_Unified, Data, Addr);
         end;
         -- Put_Line ("LB: Message sent successfully");
      end;
   exception
      when E : others =>
         Put_Line ("LB: ERROR sending message: " & Ada.Exceptions.Exception_Information (E));
         raise;
   end Send_To_Any_Backend;

   procedure Process_Incoming is
      Buf  : String (1 .. 8192);  -- Larger buffer for performance
      Last : Natural := 0;
   begin
      Put_Line ("Starting to process incoming socket messages...");
      loop
         begin
            -- Fast receive without excessive logging
            U.Receive_Bytes (Socket_Unified, Buf, Last);
            declare
               S : constant String := Buf (1 .. Last);
            begin
               -- Only log in debug mode, not in production
               Put_Line ("LB: Received response: " & S);
               LB_Wait.Put (S);
            end;
         exception
            when E : others =>
               Put_Line ("LB: Error in Process_Incoming: " & Ada.Exceptions.Exception_Information (E));
               null;
         end;
      end loop;
   end Process_Incoming;

   procedure Await_Response (Out_S : out Unbounded_String) is
      Retry_Count : Natural := 0;
      Max_ACK_Count : constant := 50000; -- Handle very high payment volume during k6 tests
      Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Max_Wait_Time : constant Duration := 5.0;  -- Maximum wait time in seconds
   begin
      loop
         select
            LB_Wait.Take (Out_S);
            -- Filter out ACK messages - only accept JSON responses
            declare
               Response : constant String := To_String(Out_S);
            begin
               if Response /= "ACK" and Response /= "ERR" then
                  -- Valid response (likely JSON)
                  Put_Line ("LB: Received valid response, length:" & Response'Length'Image);
                  exit; 
               else
                  Retry_Count := Retry_Count + 1;
                  -- Check if we've exceeded time limit
                  if Ada.Calendar.Clock - Start_Time > Max_Wait_Time then
                     Put_Line ("LB: Time limit exceeded (" & Max_Wait_Time'Image & "s), using fallback");
                     Out_S := To_Unbounded_String ("{""default"":{""totalRequests"": 0,""totalAmount"":0.0},""fallback"":{""totalRequests"": 0,""totalAmount"":0.0}}");
                     exit;
                  end if;
                  -- If we've seen too many ACKs, something is wrong
                  if Retry_Count > Max_ACK_Count then
                     Put_Line ("LB: Too many ACKs received (" & Retry_Count'Image & "), using fallback");
                     Out_S := To_Unbounded_String ("{""default"":{""totalRequests"": 0,""totalAmount"":0.0},""fallback"":{""totalRequests"": 0,""totalAmount"":0.0}}");
                     exit;
                  end if;
               end if;
            end;
         or
            delay 0.5;  -- Short polling interval
            -- Check timeout
            if Ada.Calendar.Clock - Start_Time > Max_Wait_Time then
               Put_Line ("LB: Summary request timed out after " & Max_Wait_Time'Image & "s, using fallback");
               Out_S := To_Unbounded_String ("{""default"":{""totalRequests"": 0,""totalAmount"":0.0},""fallback"":{""totalRequests"": 0,""totalAmount"":0.0}}");
               exit;
            end if;
         end select;
      end loop;
   end Await_Response;
end LB_Socket_Router;
