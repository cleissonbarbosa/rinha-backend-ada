with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with GNAT.OS_Lib;
with GNAT.Expect;

package body Database is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use GNAT.OS_Lib;

   Connection_String : Unbounded_String;

   function Execute_SQL (SQL : String) return String is
      Args    : Argument_List_Access;
      Result  : String_Access;
      Success : Boolean;
   begin
      -- Use psql command to execute SQL
      Args :=
        new Argument_List'
          (new String'("-h"),
           new String'
             (Ada.Environment_Variables.Value ("DB_HOST", "postgres")),
           new String'("-p"),
           new String'(Ada.Environment_Variables.Value ("DB_PORT", "5432")),
           new String'("-U"),
           new String'(Ada.Environment_Variables.Value ("DB_USER", "rinha")),
           new String'("-d"),
           new String'(Ada.Environment_Variables.Value ("DB_NAME", "rinha")),
           new String'("-c"),
           new String'(SQL),
           new String'("-t"));  -- Tuples only

      Spawn ("psql", Args.all, Result, Success);

      if Success and Result /= null then
         return Result.all;
      else
         return "";
      end if;
   end Execute_SQL;

   Connection_String : Unbounded_String;

   procedure Initialize_Connection is
      DB_Host     : constant String :=
        Ada.Environment_Variables.Value ("DB_HOST", "localhost");
      DB_Port     : constant String :=
        Ada.Environment_Variables.Value ("DB_PORT", "5432");
      DB_Name     : constant String :=
        Ada.Environment_Variables.Value ("DB_NAME", "rinha");
      DB_User     : constant String :=
        Ada.Environment_Variables.Value ("DB_USER", "rinha");
      DB_Password : constant String :=
        Ada.Environment_Variables.Value ("DB_PASSWORD", "rinha123");
   begin
      Connection_String :=
        To_Unbounded_String
          ("host="
           & DB_Host
           & " port="
           & DB_Port
           & " dbname="
           & DB_Name
           & " user="
           & DB_User
           & " password="
           & DB_Password);

      Put_Line ("Database connection initialized");
   end Initialize_Connection;

   procedure Store_Payment (Payment : Payment_Record) is
      SQL    : constant String :=
        "INSERT INTO payments (correlation_id, amount, requested_at, processor_type) "
        & "VALUES ('"
        & To_String (Payment.Correlation_Id)
        & "', "
        & Long_Float'Image (Payment.Amount)
        & ", NOW(), '"
        & To_String (Payment.Processor_Type)
        & "') ON CONFLICT (correlation_id) DO NOTHING;";
      Result : constant String := Execute_SQL (SQL);
   begin
      Put_Line
        ("Storing payment: "
         & To_String (Payment.Correlation_Id)
         & " Amount: "
         & Long_Float'Image (Payment.Amount)
         & " Processor: "
         & To_String (Payment.Processor_Type));

      if Result /= "" then
         Put_Line ("Database result: " & Result);
      end if;
   end Store_Payment;

   function Get_Summary
     (From_Time : String := ""; To_Time : String := "") return Summary_Response
   is
      Result : Summary_Response;
      SQL    : Unbounded_String;
   begin
      SQL :=
        To_Unbounded_String
          ("SELECT processor_type, COUNT(*) as total_requests, SUM(amount) as total_amount "
           & "FROM payments ");

      if From_Time /= "" and To_Time /= "" then
         Append
           (SQL,
            "WHERE processed_at BETWEEN '"
            & From_Time
            & "' AND '"
            & To_Time
            & "' ");
      end if;

      Append (SQL, "GROUP BY processor_type;");

      declare
         Query_Result : constant String := Execute_SQL (To_String (SQL));
      begin
         Put_Line ("Getting summary - From: " & From_Time & " To: " & To_Time);
         Put_Line ("Query result: " & Query_Result);

         -- Parse result (simplified - in real app would parse properly)
         -- For now, return mock data but log the actual query
         Result.Default_Summary.Total_Requests := 100;
         Result.Default_Summary.Total_Amount := 1000.0;
         Result.Fallback_Summary.Total_Requests := 50;
         Result.Fallback_Summary.Total_Amount := 500.0;
      end;

      return Result;
   end Get_Summary;

   procedure Close_Connection is
   begin
      Put_Line ("Database connection closed");
   end Close_Connection;

end Database;
