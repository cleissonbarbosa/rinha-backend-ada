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
      Replica_Peers          : Unbounded_String;

   -- Simple in-flight limiter to avoid overload and reduce lag
   protected type Inflight_Limiter is
      procedure Try_Acquire (Acquired : out Boolean);
      procedure Release;
      procedure Set_Max (Value : Natural);
   private
      In_Flight   : Natural := 0;
      Max_Allowed : Natural := 64;
   end Inflight_Limiter;

   protected body Inflight_Limiter is
      procedure Try_Acquire (Acquired : out Boolean) is
      begin
         if In_Flight < Max_Allowed then
            In_Flight := In_Flight + 1;
            Acquired := True;
         else
            Acquired := False;
         end if;
      end Try_Acquire;

      procedure Release is
      begin
         if In_Flight > 0 then
            In_Flight := In_Flight - 1;
         end if;
      end Release;

      procedure Set_Max (Value : Natural) is
      begin
         if Value = 0 then
            Max_Allowed := 1;
         else
            Max_Allowed := Value;
         end if;
      end Set_Max;
   end Inflight_Limiter;

   Limiter : Inflight_Limiter;

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

         Replica_Peers := To_Unbounded_String (
            Ada.Environment_Variables.Value ("REPLICA_PEERS", ""));

      -- Configure limiter from env INFLIGHT_LIMIT (per-instance)
      declare
         MaxS : constant String := Ada.Environment_Variables.Value ("INFLIGHT_LIMIT", "64");
         MaxV : Natural := 64;
      begin
         begin
            MaxV := Natural'Value (MaxS);
         exception when others => MaxV := 64; end;
         Limiter.Set_Max (MaxV);
      end;
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
            return True;
         else
            return False;
         end if;
      exception
         when others =>
            return False;
      end;
   end Try_Payment_Processor;

   -- Best-effort replication to peer instances
   procedure Replicate_Payment (
     Correlation_Id : String;
     Amount_Str     : String;
     Processor      : String;
     Requested_At   : String) is
   begin
      if Length (Replica_Peers) = 0 then
         return;
      end if;

      declare
         Peers : constant String := To_String (Replica_Peers);
         I     : Positive := Peers'First;
         J     : Positive := Peers'First;
      begin
         while J <= Peers'Last + 1 loop
            if J = Peers'Last + 1 or else Peers (J) = ',' then
               declare
                  Peer : String := Peers (I .. J - 1);
                  Payload : constant String :=
                    '{' & '"' & "correlationId" & '"' & ":""" & Correlation_Id & '"' &
                    "," & '"' & "amount" & '"' & ":" & Amount_Str &
                    "," & '"' & "processor" & '"' & ":""" & Processor & '"' &
                    "," & '"' & "requestedAt" & '"' & ":""" & Requested_At & '"' & '}';
               begin
                  while Peer'Length > 0 and then Peer (Peer'First) = ' ' loop
                     Peer := Peer (Peer'First + 1 .. Peer'Last);
                  end loop;
                  while Peer'Length > 0 and then Peer (Peer'Last) = ' ' loop
                     Peer := Peer (Peer'First .. Peer'Last - 1);
                  end loop;

                  if Peer'Length > 0 then
                     declare
                        Resp : AWS.Response.Data;
                        URL  : constant String := Peer & "/internal/replicate";
                     begin
                        Resp := AWS.Client.Post (URL => URL, Data => Payload, Content_Type => "application/json");
                        pragma Unreferenced (Resp);
                     exception
                        when others => null;
                     end;
                  end if;
               end;
               I := J + 1;
            end if;
            J := J + 1;
         end loop;
      end;
   end Replicate_Payment;

   function Process_Payment (Request_Body : String) return String is
      Correlation_Id : constant String :=
        Parse_JSON_Field (Request_Body, "correlationId");
      Amount_Str     : constant String :=
        Parse_JSON_Field (Request_Body, "amount");
      Req_At_Str     : Unbounded_String := To_Unbounded_String(Parse_JSON_Field (Request_Body, "requestedAt"));
      Amount         : Long_Float;
      Success        : Boolean := False;
      Processor_Used : Unbounded_String;
   begin
   -- Validate input (correlationId and amount must exist)
   if Correlation_Id = "" or else Amount_Str = "" then
         return "{""error"":""correlationId and amount are required""}";
      end if;

      -- Validate UUID format (basic check)
      if Correlation_Id'Length /= 36 then
         return "{""error"":""Invalid correlationId format""}";
      end if;
      
      -- Check UUID dashes in correct positions
      if Correlation_Id (Correlation_Id'First + 8) /= '-'
         or else Correlation_Id (Correlation_Id'First + 13) /= '-'
         or else Correlation_Id (Correlation_Id'First + 18) /= '-'
         or else Correlation_Id (Correlation_Id'First + 23) /= '-' then
         return "{""error"":""Invalid correlationId format""}";
      end if;

      -- Validate amount
      begin
         Amount := Long_Float'Value (Amount_Str);
         if Amount <= 0.0 then
            return "{""error"":""Amount must be positive""}";
         end if;
      exception
         when others =>
            return "{""error"":""Invalid amount format""}";
      end;

      -- If requestedAt absent, generate now in ISO 8601 UTC Z
      if Length(Req_At_Str) = 0 then
         Req_At_Str := To_Unbounded_String(Now_ISO8601_UTC);
      end if;

      -- Check if payment already exists (for idempotency)
      declare
         Existing_Processor : constant String := Database_Handler.Get_Processor_Type (Correlation_Id);
      begin
         if Existing_Processor /= "" then
            return
              "{""message"":""Payment processed successfully""," &
              '"' & "processor" & '"' & ":""" & Existing_Processor & '"' & "}";
         end if;
      end;

      -- Try default processor first (lower fees)
      -- Apply simple backpressure to avoid overload
      declare
         Acq : Boolean := False;
      begin
         Limiter.Try_Acquire (Acq);
         if not Acq then
            return "{""error"":""too many requests""}";
         end if;
      end;

      Success :=
         Try_Payment_Processor
            (To_String (Default_Processor_URL), Correlation_Id, Amount_Str, To_String(Req_At_Str));
      if Success then
         Processor_Used := To_Unbounded_String ("default");
      else
         Success :=
                Try_Payment_Processor
                   (To_String (Fallback_Processor_URL), Correlation_Id, Amount_Str, To_String(Req_At_Str));
         if Success then
            Processor_Used := To_Unbounded_String ("fallback");
         end if;
      end if;

         if Success then
                   -- Use commit time (success moment) to align with processors' summary window
                   declare
                      Commit_At : constant String := Now_ISO8601_UTC;
                   begin
                      Database_Handler.Store_Payment
                           (Correlation_Id, Amount, To_String (Processor_Used), Commit_At);
                      Replicate_Payment (Correlation_Id, Amount_Str, To_String (Processor_Used), Commit_At);
                   end;
            Limiter.Release;
         return
           "{""message"":""Payment processed successfully"","
           & """processor"":"""
           & To_String (Processor_Used)
           & """}";
      else
         Limiter.Release;
         return
           "{""error"":"
           & """Payment processing failed - all processors unavailable""}";
      end if;
   end Process_Payment;

end Payment_Handler;
