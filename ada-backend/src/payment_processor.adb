with Ada.Text_IO;
with Ada.Calendar;
with Ada.Environment_Variables;
with GNAT.OS_Lib;
with GNAT.Sockets;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Database;

package body Payment_Processor is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Fixed;
   use GNAT.Sockets;

   Default_Processor_URL  : Unbounded_String;
   Fallback_Processor_URL : Unbounded_String;

   Last_Health_Check_Default  : Ada.Calendar.Time := Ada.Calendar.Clock;
   Last_Health_Check_Fallback : Ada.Calendar.Time := Ada.Calendar.Clock;

   Default_Health  : Health_Status :=
     (Failing => False, Min_Response_Time => 50);
   Fallback_Health : Health_Status :=
     (Failing => False, Min_Response_Time => 100);

   procedure Initialize is
   begin
      Default_Processor_URL :=
        To_Unbounded_String
          (Ada.Environment_Variables.Value
             ("PAYMENT_PROCESSOR_DEFAULT",
              "http://payment-processor-default:8080"));

      Fallback_Processor_URL :=
        To_Unbounded_String
          (Ada.Environment_Variables.Value
             ("PAYMENT_PROCESSOR_FALLBACK",
              "http://payment-processor-fallback:8080"));

      Put_Line ("Payment processors initialized");
      Put_Line ("Default: " & To_String (Default_Processor_URL));
      Put_Line ("Fallback: " & To_String (Fallback_Processor_URL));
   end Initialize;

   function Make_HTTP_Request
     (URL : String; JSON_Body : String := "") return Boolean
   is
      Socket          : Socket_Type;
      Address         : Sock_Addr_Type;
      Request         : Unbounded_String;
      Response_Buffer : String (1 .. 4096);
      Last            : Natural := 0;

      -- Parse URL: http://host:port/path
      Scheme_End  : Natural := Index (URL, "://");
      Host_Start  : Natural := (if Scheme_End > 0 then Scheme_End + 3 else URL'First);
      Rest        : constant String := URL (Host_Start .. URL'Last);
      Slash_Pos   : Natural := Index (Rest, "/");
      Host_Port   : constant String := (if Slash_Pos > 0 then Rest (Rest'First .. Rest'First + Slash_Pos - 2) else Rest);
      Path        : constant String := (if Slash_Pos > 0 then Rest (Rest'First + Slash_Pos - 1 .. Rest'Last) else "/");
      Colon_Pos   : Natural := Index (Host_Port, ":");
      Host        : constant String := (if Colon_Pos > 0 then Host_Port (Host_Port'First .. Host_Port'First + Colon_Pos - 2) else Host_Port);
      Port_Str    : constant String := (if Colon_Pos > 0 then Host_Port (Host_Port'First + Colon_Pos .. Host_Port'Last) else "80");
      Port        : Natural := 80;
   begin
      -- Parse port
      begin
         Port := Natural'Value (Port_Str);
      exception
         when others => Port := 80;
      end;

      -- Resolve host in docker network
      begin
         declare
            H : constant Host_Ent := Get_Host_By_Name (Host);
         begin
            Address.Addr := H.Addresses (H.Addresses'First);
         end;
      exception
         when others => Address.Addr := Inet_Addr (Host);
      end;
      Address.Port := Port_Type (Port);

      Create_Socket (Socket, Family_Inet, Socket_Stream);
      Connect_Socket (Socket, Address);

      if JSON_Body /= "" then
         declare
            use Ada.Strings.Fixed;
            Len : constant String := Trim (Integer'Image (JSON_Body'Length), Both);
         begin
            Request :=
              To_Unbounded_String
                ("POST " & Path & " HTTP/1.1" & ASCII.CR & ASCII.LF
                 & "Host: " & Host & ASCII.CR & ASCII.LF
                 & "Content-Type: application/json" & ASCII.CR & ASCII.LF
                 & "Connection: close" & ASCII.CR & ASCII.LF
                 & "Content-Length: " & Len & ASCII.CR & ASCII.LF
                 & ASCII.CR & ASCII.LF
                 & JSON_Body);
         end;
      else
         Request :=
           To_Unbounded_String
             ("GET " & Path & " HTTP/1.1" & ASCII.CR & ASCII.LF
              & "Host: " & Host & ASCII.CR & ASCII.LF
              & "Connection: close" & ASCII.CR & ASCII.LF
              & ASCII.CR & ASCII.LF);
      end if;

      declare
         Req_Str : constant String := To_String (Request);
         SEA     : constant Stream_Element_Array := Stream_Element_Array (Req_Str);
      begin
         Send_Socket (Socket, SEA);
      end;

      declare
         Buf : Stream_Element_Array (1 .. Response_Buffer'Length);
         Got : Stream_Element_Offset;
      begin
         Receive_Socket (Socket, Buf, Got);
         Last := Natural (Got);
         if Last > 0 then
            Response_Buffer (1 .. Last) := String (Buf (1 .. Got));
         end if;
      end;

      Close_Socket (Socket);

      -- Check 2xx
      return Last >= 12 and then Response_Buffer (1 .. 12) = "HTTP/1.1 200";
   exception
      when others =>
         if Is_Open (Socket) then
            Close_Socket (Socket);
         end if;
         return False;
   end Make_HTTP_Request;

   function Try_Default_Processor
     (Correlation_Id : String; Amount : Long_Float; Requested_At : String)
      return Boolean
   is
            JSON_Body : constant String :=
               "{""correlationId"":"""
               & Correlation_Id
               & """,""amount"":"
               & Long_Float'Image (Amount)
               & ",""requestedAt"":"""
               & Requested_At & """}";
   begin
      Put_Line ("Trying default processor for: " & Correlation_Id);

      if Make_HTTP_Request
           (To_String (Default_Processor_URL) & "/payments", JSON_Body)
      then
         -- Store in database as default processor
         declare
            Payment : Payment_Types.Payment_Record;
         begin
            Payment.Correlation_Id := To_Unbounded_String (Correlation_Id);
            Payment.Amount := Amount;
            Payment.Requested_At := Ada.Calendar.Clock;
            Payment.Processor_Type := To_Unbounded_String ("default");
            Database.Store_Payment (Payment);
         end;
         return True;
      end if;

      return False;
   end Try_Default_Processor;

   function Try_Fallback_Processor
     (Correlation_Id : String; Amount : Long_Float; Requested_At : String)
      return Boolean
   is
            JSON_Body : constant String :=
               "{""correlationId"":"""
               & Correlation_Id
               & """,""amount"":"
               & Long_Float'Image (Amount)
               & ",""requestedAt"":"""
               & Requested_At & """}";
   begin
      Put_Line ("Trying fallback processor for: " & Correlation_Id);

      if Make_HTTP_Request
           (To_String (Fallback_Processor_URL) & "/payments", JSON_Body)
      then
         -- Store in database as fallback processor
         declare
            Payment : Payment_Types.Payment_Record;
         begin
            Payment.Correlation_Id := To_Unbounded_String (Correlation_Id);
            Payment.Amount := Amount;
            Payment.Requested_At := Ada.Calendar.Clock;
            Payment.Processor_Type := To_Unbounded_String ("fallback");
            Database.Store_Payment (Payment);
         end;
         return True;
      end if;

      return False;
   end Try_Fallback_Processor;

   function Process_Payment
     (Correlation_Id : String; Amount : Long_Float; Requested_At : String)
      return Boolean is
   begin
      -- Try default processor first (lower fees)
      if Try_Default_Processor (Correlation_Id, Amount, Requested_At) then
         return True;
      end if;

      Put_Line ("Default processor failed, trying fallback");

      -- If default fails, try fallback
      if Try_Fallback_Processor (Correlation_Id, Amount, Requested_At) then
         return True;
      end if;

      Put_Line ("Both processors failed for: " & Correlation_Id);
      return False;
   end Process_Payment;

   function Can_Check_Health (Last_Check : Ada.Calendar.Time) return Boolean is
      use Ada.Calendar;
      Current_Time : constant Time := Clock;
   begin
      return Current_Time - Last_Check >= 5.0; -- 5 seconds rate limit
   end Can_Check_Health;

   function Get_Health_Status (Processor : String) return Health_Status is
      use Ada.Calendar;
   begin
      if Processor = "default" then
         if Can_Check_Health (Last_Health_Check_Default) then
            -- In real implementation, make HTTP GET to /payments/service-health
            Put_Line ("Checking default processor health");
            Last_Health_Check_Default := Clock;
            -- Simulate health check response
            Default_Health.Failing := False;
            Default_Health.Min_Response_Time := 50;
         end if;
         return Default_Health;
      elsif Processor = "fallback" then
         if Can_Check_Health (Last_Health_Check_Fallback) then
            -- In real implementation, make HTTP GET to /payments/service-health
            Put_Line ("Checking fallback processor health");
            Last_Health_Check_Fallback := Clock;
            -- Simulate health check response
            Fallback_Health.Failing := False;
            Fallback_Health.Min_Response_Time := 100;
         end if;
         return Fallback_Health;
      else
         return (Failing => True, Min_Response_Time => 999);
      end if;
   end Get_Health_Status;

   procedure Cleanup is
   begin
      Put_Line ("Payment processor cleanup completed");
   end Cleanup;

end Payment_Processor;
