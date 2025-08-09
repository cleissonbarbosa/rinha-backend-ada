with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Streams;
with AWS.Response;
with AWS.Status;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with Payment_Handler;
with Database_Handler;

package body Rinha_Web_Server is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use type AWS.Status.Request_Method;

   procedure Initialize is
   begin
      Put_Line ("Initializing Rinha Web Server components...");
      Payment_Handler.Initialize;
      Database_Handler.Initialize;
      Put_Line ("All components initialized successfully");
   end Initialize;

   function Service (Request : AWS.Status.Data) return AWS.Response.Data is
      URI    : constant String := AWS.Status.URI (Request);
      Method : constant AWS.Status.Request_Method :=
        AWS.Status.Method (Request);
   begin
      Put_Line (AWS.Status.Request_Method'Image (Method) & " " & URI);

      -- Route requests based on URI and method
      if Method = AWS.Status.POST and then URI = "/payments" then
         return Handle_Payment_Request (Request);

      elsif Method = AWS.Status.GET
        and then (URI = "/payments-summary"
                  or else Ada.Strings.Fixed.Index (URI, "/payments-summary?")
                          = 1)
      then
         return Handle_Summary_Request (Request);

      elsif Method = AWS.Status.GET and then URI = "/health" then
         return Handle_Health_Request (Request);

      else
         -- 404 Not Found
         Put_Line ("404 - Resource not found: " & URI);
         return
           AWS.Response.Build
             (Content_Type => AWS.MIME.Application_JSON,
              Message_Body => "{""error"":""Not found""}",
              Status_Code  => AWS.Messages.S404);
      end if;
   end Service;

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

         Put_Line ("Processing payment request: " & Request_Body);
         Put_Line
           ("Request body length: " & Natural'Image (Request_Body'Length));

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
      Put_Line
        ("Processing summary request - From: "
         & From_Time
         & " To: "
         & To_Time);

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
      Put_Line ("Health check requested");
      return
        AWS.Response.Build
          (Content_Type => AWS.MIME.Application_JSON,
           Message_Body =>
             "{""status"":""healthy""," & """service"":""rinha-ada-backend""}",
           Status_Code  => AWS.Messages.S200);
   end Handle_Health_Request;

end Rinha_Web_Server;
