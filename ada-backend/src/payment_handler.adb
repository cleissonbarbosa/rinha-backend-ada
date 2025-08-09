with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with AWS.Client;
with AWS.Response;
with AWS.Messages;
with Database_Handler;

package body Payment_Handler is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Fixed;
   use AWS.Messages;

   Default_Processor_URL  : Unbounded_String;
   Fallback_Processor_URL : Unbounded_String;

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

      Put_Line ("Payment processors initialized:");
      Put_Line ("   Default: " & To_String (Default_Processor_URL));
      Put_Line ("   Fallback: " & To_String (Fallback_Processor_URL));
   end Initialize;

   -- Format current UTC time as strict ISO8601 with milliseconds and Z
   function Now_ISO8601_UTC return String is
      T   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      TZ  : constant Ada.Calendar.Time_Zones.Time_Offset := Ada.Calendar.Time_Zones.UTC_Time_Offset;
      Str : constant String := Ada.Calendar.Formatting.Image (T, Time_Zone => TZ);
      -- Image usually returns "YYYY-MM-DD HH:MM:SS" (no fractional). We'll convert to "YYYY-MM-DDTHH:MM:SS.000Z"
      S   : String := Str;
   begin
      -- Replace space with 'T'
      for I in S'Range loop
         if S(I) = ' ' then
            S(I) := 'T';
            exit;
         end if;
      end loop;
      return S & ".000Z";
   end Now_ISO8601_UTC;

   function Parse_JSON_Field (JSON : String; Field : String) return String is
      Field_Pattern : constant String := """" & Field & """:";
      Start_Pos     : Natural;
      End_Pos       : Natural;
      Is_String     : Boolean := False;
   begin
      Start_Pos := Index (JSON, Field_Pattern);
      if Start_Pos = 0 then
         return "";
      end if;

      Start_Pos := Start_Pos + Field_Pattern'Length;

      -- Skip whitespace
      while Start_Pos <= JSON'Last and then JSON (Start_Pos) = ' ' loop
         Start_Pos := Start_Pos + 1;
      end loop;

      -- Check if value is a string (starts with quote)
      if Start_Pos <= JSON'Last and then JSON (Start_Pos) = '"' then
         Is_String := True;
         Start_Pos := Start_Pos + 1; -- Skip opening quote
      end if;

      End_Pos := Start_Pos;
      
      if Is_String then
         -- For strings, look for closing quote
         while End_Pos <= JSON'Last and then JSON (End_Pos) /= '"' loop
            End_Pos := End_Pos + 1;
         end loop;
      else
         -- For numbers/booleans, look for comma, closing brace, or end
         while End_Pos <= JSON'Last
           and then JSON (End_Pos) /= ','
           and then JSON (End_Pos) /= '}'
           and then JSON (End_Pos) /= ' '
         loop
            End_Pos := End_Pos + 1;
         end loop;
      end if;

      if End_Pos > Start_Pos then
         return JSON (Start_Pos .. End_Pos - 1);
      end if;

      return "";
   end Parse_JSON_Field;

   function Try_Payment_Processor
     (URL : String; Correlation_Id : String; Amount : String; Requested_At : String) return Boolean
   is
      Processor_URL : constant String := URL & "/payments";
      JSON_Payload  : constant String :=
        "{""correlationId"":"""
        & Correlation_Id
        & """,""amount"":"
        & Amount
        & ",""requestedAt"":"""
        & Requested_At
        & """}";
   begin
      Put_Line ("Trying processor: " & URL);
      Put_Line ("Payload: " & JSON_Payload);

      -- Try to make HTTP request using AWS.Client
      declare
         Response : AWS.Response.Data;
      begin
         Response :=
           AWS.Client.Post
             (URL          => Processor_URL,
              Data         => JSON_Payload,
              Content_Type => "application/json");

         if AWS.Response.Status_Code (Response) = AWS.Messages.S200 then
            Put_Line ("Payment processed successfully by " & URL);
            return True;
         else
            Put_Line
              ("Payment failed - Status: "
               & AWS.Messages.Status_Code'Image
                   (AWS.Response.Status_Code (Response)));
            return False;
         end if;
      exception
         when others =>
            Put_Line ("Exception calling processor: " & URL);
            return False;
      end;
   end Try_Payment_Processor;

   function Process_Payment (Request_Body : String) return String is
      Correlation_Id : constant String :=
        Parse_JSON_Field (Request_Body, "correlationId");
      Amount_Str     : constant String :=
        Parse_JSON_Field (Request_Body, "amount");
      Req_At_Str     : String := Parse_JSON_Field (Request_Body, "requestedAt");
      Amount         : Long_Float;
      Success        : Boolean := False;
      Processor_Used : Unbounded_String;
   begin
      Put_Line ("Processing payment request");
      Put_Line ("Correlation ID: " & Correlation_Id);
      Put_Line ("Amount: " & Amount_Str);

   -- Validate input (correlationId and amount must exist)
   if Correlation_Id = "" or else Amount_Str = "" then
         Put_Line ("Invalid request - missing fields");
         return "{""error"":""correlationId and amount are required""}";
      end if;

      -- Validate UUID format (basic check)
      if Correlation_Id'Length /= 36 then
         Put_Line ("Invalid correlation ID format - wrong length");
         return "{""error"":""Invalid correlationId format""}";
      end if;
      
      -- Check UUID dashes in correct positions
      if Correlation_Id (Correlation_Id'First + 8) /= '-'
         or else Correlation_Id (Correlation_Id'First + 13) /= '-'
         or else Correlation_Id (Correlation_Id'First + 18) /= '-'
         or else Correlation_Id (Correlation_Id'First + 23) /= '-' then
         Put_Line ("Invalid correlation ID format - wrong dash positions");
         return "{""error"":""Invalid correlationId format""}";
      end if;

      -- Validate amount
      begin
         Amount := Long_Float'Value (Amount_Str);
         if Amount <= 0.0 then
            Put_Line ("Invalid amount - must be positive");
            return "{""error"":""Amount must be positive""}";
         end if;
      exception
         when others =>
            Put_Line ("Invalid amount format");
            return "{""error"":""Invalid amount format""}";
      end;

      -- If requestedAt absent, generate now in ISO 8601 UTC Z
      if Req_At_Str = "" then
         Req_At_Str := Now_ISO8601_UTC;
      end if;

      -- Check if payment already exists (for idempotency)
      declare
         Check_SQL : constant String := 
           "SELECT processor_type FROM payments WHERE correlation_id = '" & 
           Correlation_Id & "';";
         Existing_Result : constant String := 
           Database_Handler.Execute_SQL_Query (Check_SQL);
      begin
         if Existing_Result'Length > 1 then  -- Not empty
            declare
               Existing_Processor : constant String := 
                 Ada.Strings.Fixed.Trim (Existing_Result, Ada.Strings.Both);
            begin
               Put_Line ("Payment already exists with processor: " & Existing_Processor);
               return
                 "{""message"":""Payment processed successfully"","
                 & """processor"":"""
                 & Existing_Processor
                 & """}";
            end;
         end if;
      end;

      -- Try default processor first (lower fees)
         Success :=
            Try_Payment_Processor
               (To_String (Default_Processor_URL), Correlation_Id, Amount_Str, Req_At_Str);
      if Success then
         Processor_Used := To_Unbounded_String ("default");
      else
         Put_Line ("Default processor failed, trying fallback...");
         Success :=
                Try_Payment_Processor
                   (To_String (Fallback_Processor_URL), Correlation_Id, Amount_Str, Req_At_Str);
         if Success then
            Processor_Used := To_Unbounded_String ("fallback");
         end if;
      end if;

      if Success then
             -- Store payment in database with the same requestedAt used
             Database_Handler.Store_Payment
                (Correlation_Id, Amount, To_String (Processor_Used), Req_At_Str);
         Put_Line ("Payment processed and stored successfully");
         return
           "{""message"":""Payment processed successfully"","
           & """processor"":"""
           & To_String (Processor_Used)
           & """}";
      else
         Put_Line ("All payment processors failed");
         return
           "{""error"":"
           & """Payment processing failed - all processors unavailable""}";
      end if;
   end Process_Payment;

end Payment_Handler;
