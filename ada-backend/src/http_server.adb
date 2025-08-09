with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
with Ada.Exceptions;
with GNAT.Sockets;
with Payment_Processor;
with Database;
with Payment_Types;

package body HTTP_Server is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Fixed;
   use GNAT.Sockets;

   procedure Initialize_Server is
   begin
      Put_Line ("HTTP Server initializing...");
   end Initialize_Server;

   function Handle_Summary_Request (Query_Params : String) return String is
      From_Time : constant String := Parse_Query_Param (Query_Params, "from");
      To_Time   : constant String := Parse_Query_Param (Query_Params, "to");
      Summary   : constant Summary_Response :=
        Database.Get_Summary (From_Time, To_Time);

      JSON_Response : Unbounded_String;
   begin
      Put_Line ("Processing summary request with params: " & Query_Params);

      JSON_Response := To_Unbounded_String ("{""default"":{");
      Append
        (JSON_Response,
         """totalRequests"":"
         & Natural'Image (Summary.Default_Summary.Total_Requests)
         & ",");
      Append
        (JSON_Response,
         """totalAmount"":"
         & Long_Float'Image (Summary.Default_Summary.Total_Amount));
      Append (JSON_Response, "},""fallback"":{");
      Append
        (JSON_Response,
         """totalRequests"":"
         & Natural'Image (Summary.Fallback_Summary.Total_Requests)
         & ",");
      Append
        (JSON_Response,
         """totalAmount"":"
         & Long_Float'Image (Summary.Fallback_Summary.Total_Amount));
      Append (JSON_Response, "}}");

      declare
         Response_Body  : constant String := To_String (JSON_Response);
             Content_Length : constant String :=
                Ada.Strings.Fixed.Trim (Integer'Image (Response_Body'Length), Ada.Strings.Both);
      begin
         return
           "HTTP/1.1 200 OK"
           & ASCII.CR
           & ASCII.LF
           & "Content-Type: application/json"
           & ASCII.CR
           & ASCII.LF
                & "Content-Length: "
           & Content_Length
           & ASCII.CR
           & ASCII.LF
           & ASCII.CR
           & ASCII.LF
           & Response_Body;
      end;
   end Handle_Summary_Request;

   function Handle_Health_Request return String is
   begin
      return
        "HTTP/1.1 200 OK"
        & ASCII.CR
        & ASCII.LF
        & "Content-Type: application/json"
        & ASCII.CR
        & ASCII.LF
        & "Content-Length: 21"
        & ASCII.CR
        & ASCII.LF
        & ASCII.CR
        & ASCII.LF
        & "{""status"":""healthy""}";
   end Handle_Health_Request;

   procedure Process_HTTP_Request (Socket : Socket_Type) is
      Method   : Unbounded_String;
      Path     : Unbounded_String;
      Response : Unbounded_String;

      procedure Parse_Request_Line (Line : String) is
         Space1, Space2 : Natural;
      begin
         Space1 := Index (Line, " ");
         if Space1 > 0 then
            Method := To_Unbounded_String (Line (Line'First .. Space1 - 1));
            Space2 := Index (Line (Space1 + 1 .. Line'Last), " ");
            if Space2 > 0 then
               Path := To_Unbounded_String (Line (Space1 + 1 .. Space1 + Space2 - 1));
            end if;
         end if;
      end Parse_Request_Line;

   begin
      -- Read headers and initial body chunk
      declare
         Buffer      : Stream_Element_Array (1 .. 8192);
         Read_To     : Stream_Element_Offset := 0;
         Data        : String (1 .. 8192);
         CRLFCRLF    : constant String := ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF;
         Header_End  : Natural := 0;
         Body_Start  : Natural := 0;
      begin
         Receive_Socket (Socket, Buffer, Read_To);
         if Read_To = 0 then
            Close_Socket (Socket);
            return;
         end if;

         Data (1 .. Natural (Read_To)) := String (Buffer (1 .. Read_To));
         Header_End := Index (Data (1 .. Natural (Read_To)), CRLFCRLF);
         if Header_End = 0 then
            Header_End := Natural (Read_To);
         end if;
         Body_Start := Header_End + CRLFCRLF'Length;

         -- Parse request line (first line)
         declare
            First_Line_End : Natural := Index (Data (1 .. Header_End), ASCII.CR & ASCII.LF);
         begin
            if First_Line_End = 0 then
               First_Line_End := Header_End;
            end if;
            Parse_Request_Line (Data (1 .. First_Line_End));
         end;

         -- Compute body already available
         declare
            Body_Already  : Natural := (if Body_Start <= Natural (Read_To) then Natural (Read_To) - Body_Start + 1 else 0);
            Body_Content  : Unbounded_String := To_Unbounded_String ("");
         begin
            if Body_Already > 0 then
               Body_Content := To_Unbounded_String (Data (Body_Start .. Natural (Read_To)));
            end if;

            -- Route
            if To_String (Method) = "POST" and then To_String (Path) = "/payments" then
               Response := To_Unbounded_String (Handle_Payment_Request (To_String (Body_Content)));
            elsif To_String (Method) = "GET" and then Index (To_String (Path), "/payments-summary") = 1 then
               declare
                  Q_Pos : Natural := Index (To_String (Path), "?");
                  Query : constant String := (if Q_Pos > 0 then To_String (Path) (Q_Pos + 1 .. Length (Path)) else "");
               begin
                  Response := To_Unbounded_String (Handle_Summary_Request (Query));
               end;
            elsif To_String (Method) = "GET" and then To_String (Path) = "/health" then
               Response := To_Unbounded_String (Handle_Health_Request);
            else
               Response := To_Unbounded_String
                 ("HTTP/1.1 404 Not Found" & ASCII.CR & ASCII.LF
                  & "Content-Type: application/json" & ASCII.CR & ASCII.LF
                  & "Content-Length: 25" & ASCII.CR & ASCII.LF & ASCII.CR & ASCII.LF
                  & "{""error"":""Not found""}");
            end if;
         end;
      end;

      -- Send response
      declare
         Response_String : constant String := To_String (Response);
         Response_Buffer : constant Stream_Element_Array := Stream_Element_Array (Response_String);
      begin
         Send_Socket (Socket, Response_Buffer);
      end;

      Close_Socket (Socket);
   exception
      when others =>
         Close_Socket (Socket);
   end Process_HTTP_Request;

   procedure Start_Server (Port : Natural) is
      Server_Socket : Socket_Type;
      Client_Socket : Socket_Type;
      Server_Addr   : Sock_Addr_Type;
      Client_Addr   : Sock_Addr_Type;
   begin
      Put_Line ("Starting HTTP server on port" & Natural'Image (Port));

      Initialize (Process => True);

      Create_Socket (Server_Socket, Family_Inet, Socket_Stream);

      Server_Addr.Addr := Any_Inet_Addr;
      Server_Addr.Port := Port_Type (Port);

      Set_Socket_Option (Server_Socket, Socket_Level, (Reuse_Address, True));
      Bind_Socket (Server_Socket, Server_Addr);
      Listen_Socket (Server_Socket);

      Put_Line ("HTTP server listening on port" & Natural'Image (Port));

      loop
         Accept_Socket (Server_Socket, Client_Socket, Client_Addr);
         Put_Line ("Client connected from " & Image (Client_Addr));

         -- Process request (in real app, would use tasks for concurrency)
         Process_HTTP_Request (Client_Socket);
      end loop;
   exception
      when E : others =>
         Put_Line ("Server error: " & Ada.Exceptions.Exception_Message (E));
         Close_Socket (Server_Socket);
   end Start_Server;

   procedure Stop_Server is
   begin
      Put_Line ("Stopping HTTP server");
      Payment_Processor.Cleanup;
      Database.Close_Connection;
   end Stop_Server;

end HTTP_Server;
