with Ada.Text_IO;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Exceptions;
with ADO.Sessions.Factory;
with ADO.Sessions;
with ADO.Statements;
with ADO.SQL;
with ADO.Postgresql;

package body Database_Handler is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Fixed;

   Factory     : ADO.Sessions.Factory.Session_Factory;
   Initialized : Boolean := False;

   procedure Initialize is
      DB_Host : constant String :=
        Ada.Environment_Variables.Value ("DB_HOST", "postgres");
      DB_Port : constant String :=
        Ada.Environment_Variables.Value ("DB_PORT", "5432");
      DB_Name : constant String :=
        Ada.Environment_Variables.Value ("DB_NAME", "rinha");
      DB_User : constant String :=
        Ada.Environment_Variables.Value ("DB_USER", "rinha");
      DB_Pass : constant String :=
        Ada.Environment_Variables.Value ("DB_PASSWORD", "rinha123");
      DB_URL  : constant String :=
        "postgresql://"
        & DB_Host
        & ":"
        & DB_Port
        & "/"
        & DB_Name
        & "?user="
        & DB_User
        & "&password="
        & DB_Pass;
   begin
      Put_Line ("Initializing PostgreSQL driver...");
      
      -- Initialize the PostgreSQL driver first
      ADO.Postgresql.Initialize;
      Put_Line ("PostgreSQL driver initialized successfully");
      
      Put_Line ("Initializing ADO Database connection...");
      Put_Line ("   Host: " & DB_Host);
      Put_Line ("   Port: " & DB_Port);
      Put_Line ("   Database: " & DB_Name);
      Put_Line ("   User: " & DB_User);
      Put_Line ("   URL: " & DB_URL);

      -- Create database factory
      begin
         Factory.Create (DB_URL);
         Initialized := True;
         Put_Line ("Database factory created successfully");

         -- Test connectivity
         declare
            DB   : ADO.Sessions.Session := Factory.Get_Session;
            Stmt : ADO.Statements.Query_Statement :=
              DB.Create_Statement ("SELECT 1");
         begin
            Stmt.Execute;
            if Stmt.Has_Elements then
               Put_Line ("Database connectivity test: SUCCESS");
            else
               Put_Line ("Database connectivity test: FAILED");
            end if;
         end;

      exception
         when E : others =>
            Put_Line
              ("Database factory creation failed: "
               & Ada.Exceptions.Exception_Message (E));
            Initialized := False;
            raise;
      end;
   end Initialize;

    procedure Store_Payment
       (Correlation_Id : String;
         Amount         : Long_Float;
         Processor_Type : String;
         Requested_At   : String) is
   begin
      Put_Line
        ("Storing payment - ID: "
         & Correlation_Id
         & " Amount: "
         & Long_Float'Image (Amount)
         & " Processor: "
         & Processor_Type);

      if not Initialized then
         Put_Line ("Database not initialized, cannot store payment");
         return;
      end if;

         declare
             DB   : ADO.Sessions.Master_Session := Factory.Get_Master_Session;
             Stmt : ADO.Statements.Query_Statement :=
                DB.Create_Statement
                   ("INSERT INTO payments (correlation_id, amount, requested_at, processor_type) "
                     & "VALUES (?, ?, ?, ?) ON CONFLICT (correlation_id) DO NOTHING");
      begin
         -- Bind parameters
         Stmt.Bind_Param (1, Correlation_Id);
         Stmt.Bind_Param (2, Amount);
             Stmt.Bind_Param (3, Requested_At);
             Stmt.Bind_Param (4, Processor_Type);

         -- Execute the statement
         Stmt.Execute;

         Put_Line ("Payment stored successfully");

      exception
         when E : others =>
            Put_Line
              ("Failed to store payment: "
               & Ada.Exceptions.Exception_Message (E));
            -- Don't re-raise to avoid breaking payment flow
      end;
   end Store_Payment;

   function Execute_SQL_Query (SQL : String) return String is
   begin
      Put_Line ("Executing SQL: " & SQL);

      if not Initialized then
         Put_Line ("Database not initialized");
         return "";
      end if;

      declare
         DB     : ADO.Sessions.Session := Factory.Get_Session;
         Stmt   : ADO.Statements.Query_Statement := DB.Create_Statement (SQL);
         Result : Unbounded_String;
      begin
         Stmt.Execute;

         -- Process results if any
         while Stmt.Has_Elements loop
            for I in 1 .. Stmt.Get_Column_Count loop
               if I > 1 then
                  Append (Result, "|");
               end if;
               Append (Result, Stmt.Get_String (I));
            end loop;
            Append (Result, ASCII.LF);
            Stmt.Next;
         end loop;

         return To_String (Result);

      exception
         when E : others =>
            Put_Line
              ("SQL query failed: " & Ada.Exceptions.Exception_Message (E));
            return "";
      end;
   end Execute_SQL_Query;

    function Get_Summary
       (From_Time : String := ""; To_Time : String := "") return String is
   begin
      Put_Line
        ("Getting payment summary - From: " & From_Time & " To: " & To_Time);

      if not Initialized then
         Put_Line ("Database not initialized");
         return
           "{""default"":{""totalRequests"":0,""totalAmount"":0.0},"
           & """fallback"":{""totalRequests"":0,""totalAmount"":0.0}}";
      end if;

      declare
         DB             : ADO.Sessions.Session := Factory.Get_Session;
         SQL_Query      : Unbounded_String;
         Stmt           : ADO.Statements.Query_Statement;
         JSON_Result    : Unbounded_String;
         Default_Found  : Boolean := False;
         Fallback_Found : Boolean := False;
      begin
         -- Build SQL query with optional time filtering on requested_at
         declare
            Base_SQL : constant String :=
              "SELECT processor_type, COUNT(*), COALESCE(SUM(amount), 0) " &
              "FROM payments";
            Where_Clauses : Unbounded_String := To_Unbounded_String("");
         begin
            if From_Time /= "" then
               if Length(Where_Clauses) > 0 then
                  Append(Where_Clauses, " AND ");
               end if;
               Append(Where_Clauses, "requested_at >= '" & From_Time & "'");
            end if;
            if To_Time /= "" then
               if Length(Where_Clauses) > 0 then
                  Append(Where_Clauses, " AND ");
               end if;
               Append(Where_Clauses, "requested_at <= '" & To_Time & "'");
            end if;

            SQL_Query := To_Unbounded_String(Base_SQL);
            if Length(Where_Clauses) > 0 then
               Append(SQL_Query, " WHERE " & To_String(Where_Clauses));
            end if;
            Append(SQL_Query, " GROUP BY processor_type");
         end;

         Put_Line ("Executing SQL: " & To_String (SQL_Query));
         Stmt := DB.Create_Statement (To_String (SQL_Query));
         Stmt.Execute;

         JSON_Result := To_Unbounded_String ("{");

         -- Process results
         while Stmt.Has_Elements loop
            declare
               Processor_Type : constant String := Stmt.Get_String (1);
               Total_Requests : constant Integer := Stmt.Get_Integer (2);
               Total_Amount   : constant Long_Float := Stmt.Get_Long_Float (3);
            begin
               Put_Line ("Debug: Processor=" & Processor_Type & 
                        " Count=" & Integer'Image (Total_Requests) & 
                        " Amount=" & Long_Float'Image (Total_Amount));
               
               if Length (JSON_Result) > 1 then
                  Append (JSON_Result, ",");
               end if;

               Append (JSON_Result, """" & Processor_Type & """:{");
               -- Format numbers properly without leading spaces
               declare
                  Requests_Str : constant String := Ada.Strings.Fixed.Trim (Integer'Image (Total_Requests), Ada.Strings.Left);
                  Amount_Str : constant String := Ada.Strings.Fixed.Trim (Long_Float'Image (Total_Amount), Ada.Strings.Left);
               begin
                  Append (JSON_Result, """totalRequests"":" & Requests_Str & ",");
                  Append (JSON_Result, """totalAmount"":" & Amount_Str);
               end;
               Append (JSON_Result, "}");

               if Processor_Type = "default" then
                  Default_Found := True;
               elsif Processor_Type = "fallback" then
                  Fallback_Found := True;
               end if;
            end;
            Stmt.Next;
         end loop;

         -- Add missing processors with zero values
         if not Default_Found then
            if Length (JSON_Result) > 1 then
               Append (JSON_Result, ",");
            end if;
            Append
              (JSON_Result,
               """default"":{""totalRequests"":0,""totalAmount"":0.0}");
         end if;

         if not Fallback_Found then
            if Length (JSON_Result) > 1 then
               Append (JSON_Result, ",");
            end if;
            Append
              (JSON_Result,
               """fallback"":{""totalRequests"":0,""totalAmount"":0.0}");
         end if;

         Append (JSON_Result, "}");

         Put_Line ("Summary generated: " & To_String (JSON_Result));
         return To_String (JSON_Result);

      exception
         when E : others =>
            Put_Line
              ("Failed to generate summary: "
               & Ada.Exceptions.Exception_Message (E));
            -- Return default structure in case of error
            return
              "{""default"":{""totalRequests"":0,""totalAmount"":0.0},"
              & """fallback"":{""totalRequests"":0,""totalAmount"":0.0}}";
      end;
   end Get_Summary;

end Database_Handler;
