with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Streams;
with Ada.Environment_Variables;
with AWS.Response;
with AWS.Status;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Client;
with Payment_Handler;
with Database_Handler;

package body Rinha_Web_Server is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use type AWS.Status.Request_Method;

   procedure Initialize is
   begin
   -- Minimal logs during init
   Put_Line ("Initializing Rinha Web Server components...");
      Payment_Handler.Initialize;
      Database_Handler.Initialize;
   -- Put_Line ("All components initialized successfully");
   end Initialize;

   function Service (Request : AWS.Status.Data) return AWS.Response.Data is
      URI    : constant String := AWS.Status.URI (Request);
      Method : constant AWS.Status.Request_Method :=
        AWS.Status.Method (Request);
   begin
      -- Route requests based on URI and method with optimized string comparison
      case Method is
         when AWS.Status.POST =>
            if URI = "/payments" then
               return Handle_Payment_Request (Request);
               elsif URI = "/internal/replicate" then
                  return Handle_Replicate_Request (Request);
            else
               goto Not_Found;
            end if;
         
   when AWS.Status.GET =>
            if URI = "/health" then
               return Handle_Health_Request (Request);
            elsif URI = "/payments-summary" or else 
                  (URI'Length > 17 and then URI (1..17) = "/payments-summary") then
      return Handle_Summary_Request (Request);
      elsif URI = "/internal/summary" or else
         (URI'Length > 17 and then URI (1..17) = "/internal/summary") then
         return Handle_Internal_Summary_Request (Request);
            else
               goto Not_Found;
            end if;
         
         when others =>
            goto Not_Found;
      end case;

      <<Not_Found>>
      -- 404 Not Found
      return
        AWS.Response.Build
          (Content_Type => AWS.MIME.Application_JSON,
              Message_Body => "{""error"":""Not found""}",
              Status_Code  => AWS.Messages.S404);
      end Service;

   function Handle_Replicate_Request (Request : AWS.Status.Data) return AWS.Response.Data is
      begin
         declare
            Binary_Body  : constant Ada.Streams.Stream_Element_Array :=
              AWS.Status.Binary_Data (Request);
      Payload : String (1 .. Natural (Binary_Body'Length));
            J    : Natural := 1;
         begin
            for I in Binary_Body'Range loop
         Payload (J) := Character'Val (Binary_Body (I));
               J := J + 1;
            end loop;
            declare
         Corr  : constant String := Payment_Handler.Parse_JSON_Field (Payload, "correlationId");
         AmtS  : constant String := Payment_Handler.Parse_JSON_Field (Payload, "amount");
         Proc  : constant String := Payment_Handler.Parse_JSON_Field (Payload, "processor");
         ReqAt : constant String := Payment_Handler.Parse_JSON_Field (Payload, "requestedAt");
               Amt   : Long_Float;
            begin
               if Corr = "" or else AmtS = "" or else Proc = "" or else ReqAt = "" then
                           return AWS.Response.Build (Content_Type => AWS.MIME.Application_JSON,
                              Message_Body => "{""error"":""invalid replicate payload""}", Status_Code => AWS.Messages.S400);
               end if;
               begin
                  Amt := Long_Float'Value (AmtS);
               exception when others =>
                           return AWS.Response.Build (Content_Type => AWS.MIME.Application_JSON,
                              Message_Body => "{""error"":""invalid amount""}", Status_Code => AWS.Messages.S400);
               end;

               if Database_Handler.Get_Processor_Type (Corr) = "" then
                  Database_Handler.Store_Payment (Corr, Amt, Proc, ReqAt);
               end if;
                      return AWS.Response.Build (Content_Type => AWS.MIME.Application_JSON,
                         Message_Body => "{""message"":""ok""}", Status_Code => AWS.Messages.S200);
            end;
         end;
   end Handle_Replicate_Request;

      function Handle_Internal_Summary_Request (Request : AWS.Status.Data) return AWS.Response.Data is
      begin
         -- If called internally by peer, return local summary
         declare
            URI : constant String := AWS.Status.URI (Request);
         begin
            if URI'Length > 9 and then URI (1..9) = "/internal" then
               return Handle_Summary_Request (Request);
            end if;
            -- External call: always aggregate peers quickly; do best-effort with short time budget
         end;

         -- External call: fan-out to peers + self and aggregate
         declare
            Params    : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
            From_Time : constant String := AWS.Parameters.Get (Params, "from");
            To_Time   : constant String := AWS.Parameters.Get (Params, "to");
            Peers     : constant String := Ada.Environment_Variables.Value ("REPLICA_PEERS", "");
            function Sum_Field (JSON : String; Field : String) return Long_Float is
               -- naive extractor: finds "Field":{"totalRequests":X,"totalAmount":Y}
               function Find (S, P : String) return Natural is
               begin
                  return Ada.Strings.Fixed.Index (S, P);
               end;
               I : Natural := Find (JSON, '"' & Field & '"');
               R : Long_Float := 0.0;
               A : Long_Float := 0.0;
               P1, P2 : Natural;
            begin
               if I = 0 then
                  return 0.0;
               end if;
               P1 := Find (JSON (I .. JSON'Last), '"' & "totalAmount" & '"' & ":");
               if P1 = 0 then
                  return 0.0;
               end if;
               P1 := I + P1 + 14; -- move after "totalAmount":
               P2 := P1;
               while P2 <= JSON'Last and then JSON (P2) /= '}' and then JSON (P2) /= ',' loop
                  P2 := P2 + 1;
               end loop;
               begin
                  A := Long_Float'Value (JSON (P1 .. P2 - 1));
               exception when others => A := 0.0; end;
               return A;
            end Sum_Field;

            Self_Resp : constant String := Database_Handler.Get_Summary (From_Time, To_Time);
            Default_Sum  : Long_Float := Sum_Field (Self_Resp, "default");
            Fallback_Sum : Long_Float := Sum_Field (Self_Resp, "fallback");
            Default_Req  : Integer := 0;
            Fallback_Req : Integer := 0;
            -- crude request count parser
            function Sum_Req (JSON : String; Field : String) return Integer is
               I : Natural := Ada.Strings.Fixed.Index (JSON, '"' & Field & '"');
               P : Natural;
               J : Natural;
               V : Integer := 0;
            begin
               if I = 0 then return 0; end if;
               P := Ada.Strings.Fixed.Index (JSON (I .. JSON'Last), '"' & "totalRequests" & '"' & ":");
               if P = 0 then return 0; end if;
               P := I + P + 16;
               J := P;
               while J <= JSON'Last and then JSON (J) in '0' .. '9' loop
                  J := J + 1;
               end loop;
               begin V := Integer'Value (JSON (P .. J - 1)); exception when others => V := 0; end;
               return V;
            end Sum_Req;

         begin
            -- include self first
            Default_Req := Default_Req + Sum_Req (Self_Resp, "default");
            Fallback_Req := Fallback_Req + Sum_Req (Self_Resp, "fallback");
            -- query peers
            declare
               I : Positive := Peers'First;
               J : Positive := Peers'First;
            begin
               if Peers'Length > 0 then
                  while J <= Peers'Last + 1 loop
                     if J = Peers'Last + 1 or else Peers (J) = ',' then
                        declare
                           Peer : String := Peers (I .. J - 1);
                           URL  : constant String := Peer & "/internal/summary?from=" & From_Time & "&to=" & To_Time;
                           R    : AWS.Response.Data;
                           S    : String := "";
                        begin
                           while Peer'Length > 0 and then Peer (Peer'First) = ' ' loop
                              Peer := Peer (Peer'First + 1 .. Peer'Last);
                           end loop;
                           while Peer'Length > 0 and then Peer (Peer'Last) = ' ' loop
                              Peer := Peer (Peer'First .. Peer'Last - 1);
                           end loop;
                           if Peer'Length > 0 then
                              begin
                                 -- Best-effort with a very short timeout budget overall
                                 -- AWS.Client.Get doesn't accept a timeout parameter directly; rely on fast peers or ignore failures
                                 R := AWS.Client.Get (URL);
                                 S := AWS.Response.Message_Body (R);
                                 Default_Sum  := Default_Sum  + Sum_Field (S, "default");
                                 Fallback_Sum := Fallback_Sum + Sum_Field (S, "fallback");
                                 Default_Req  := Default_Req  + Sum_Req (S, "default");
                                 Fallback_Req := Fallback_Req + Sum_Req (S, "fallback");
                              exception when others => null; end;
                           end if;
                        end;
                        I := J + 1;
                     end if;
                     J := J + 1;
                  end loop;
               end if;
            end;

            declare
               JSON : Unbounded_String := To_Unbounded_String ("{");
            begin
               Append (JSON, '"' & "default" & '"' & ":{" & '"' & "totalRequests" & '"' & ":" & Ada.Strings.Fixed.Trim (Integer'Image (Default_Req), Ada.Strings.Left) & "," & '"' & "totalAmount" & '"' & ":" & Ada.Strings.Fixed.Trim (Long_Float'Image (Default_Sum), Ada.Strings.Left) & "}");
               Append (JSON, ",");
               Append (JSON, '"' & "fallback" & '"' & ":{" & '"' & "totalRequests" & '"' & ":" & Ada.Strings.Fixed.Trim (Integer'Image (Fallback_Req), Ada.Strings.Left) & "," & '"' & "totalAmount" & '"' & ":" & Ada.Strings.Fixed.Trim (Long_Float'Image (Fallback_Sum), Ada.Strings.Left) & "}");
               Append (JSON, "}");
               return AWS.Response.Build (Content_Type => AWS.MIME.Application_JSON, Message_Body => To_String (JSON), Status_Code => AWS.Messages.S200);
            end;
         end;
      end Handle_Internal_Summary_Request;

   function Handle_Payment_Request
     (Request : AWS.Status.Data) return AWS.Response.Data is
   begin
      -- Try to get request body using Binary_Data
      declare
         Binary_Body  : constant Ada.Streams.Stream_Element_Array :=
           AWS.Status.Binary_Data (Request);
         Request_Body : String (1 .. Natural (Binary_Body'Length));
         J            : Natural := 1;
      begin
         -- Convert binary data to string
         for I in Binary_Body'Range loop
            Request_Body (J) := Character'Val (Binary_Body (I));
            J := J + 1;
         end loop;

         declare
            Response_Body : constant String :=
              Payment_Handler.Process_Payment (Request_Body);
         begin
            if Ada.Strings.Fixed.Index (Response_Body, "error") > 0 then
               return
                 AWS.Response.Build
                   (Content_Type => AWS.MIME.Application_JSON,
                    Message_Body => Response_Body,
                    Status_Code  => AWS.Messages.S400);
            else
               return
                 AWS.Response.Build
                   (Content_Type => AWS.MIME.Application_JSON,
                    Message_Body => Response_Body,
                    Status_Code  => AWS.Messages.S200);
            end if;
         end;
      end;
   end Handle_Payment_Request;

   function Handle_Summary_Request
     (Request : AWS.Status.Data) return AWS.Response.Data
   is
      Parameters : constant AWS.Parameters.List :=
        AWS.Status.Parameters (Request);
      From_Time  : constant String := AWS.Parameters.Get (Parameters, "from");
      To_Time    : constant String := AWS.Parameters.Get (Parameters, "to");
   begin
      declare
         Response_Body : constant String :=
           Database_Handler.Get_Summary (From_Time, To_Time);
      begin
         return
           AWS.Response.Build
             (Content_Type => AWS.MIME.Application_JSON,
              Message_Body => Response_Body,
              Status_Code  => AWS.Messages.S200);
      end;
   end Handle_Summary_Request;

   function Handle_Health_Request
     (Request : AWS.Status.Data) return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return
        AWS.Response.Build
          (Content_Type => AWS.MIME.Application_JSON,
           Message_Body =>
             "{""status"":""healthy""," & """service"":""rinha-ada-backend""}",
           Status_Code  => AWS.Messages.S200);
   end Handle_Health_Request;

end Rinha_Web_Server;
