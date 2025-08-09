with Ada.Text_IO;
with Ada.Environment_Variables;
with Ada.Command_Line;
with Ada.Exceptions;
with AWS.Server;
with AWS.Config.Set;
with AWS.Config;
with Rinha_Web_Server;

procedure Rinha is

   use Ada.Text_IO;

   WS          : AWS.Server.HTTP;
   Config      : AWS.Config.Object;
   Server_Port : Natural := 8080;

begin
   Put_Line ("=== Rinha de Backend 2025 - Ada Web Server Implementation ===");

   -- Get port from environment or use default
   begin
      Server_Port :=
        Natural'Value
          (Ada.Environment_Variables.Value ("SERVER_PORT", "8080"));
   exception
      when others =>
         Put_Line ("Warning: Invalid SERVER_PORT, using default 8080");
         Server_Port := 8080;
   end;

   Put_Line ("Starting Ada Web Server on port" & Natural'Image (Server_Port));

   -- Configure AWS
   Config := AWS.Config.Get_Current;
   AWS.Config.Set.Server_Port (Config, Server_Port);
   AWS.Config.Set.Max_Connection (Config, 100);
   AWS.Config.Set.Server_Name (Config, "Rinha-Ada-Backend");
   AWS.Config.Set.Server_Host (Config, "0.0.0.0"); -- Bind to all interfaces

   -- Initialize and start the web server
   begin
      Put_Line ("Initializing Rinha Web Server components...");
      Rinha_Web_Server.Initialize;
      
      Put_Line ("Starting AWS server...");
      AWS.Server.Start (WS, Rinha_Web_Server.Service'Access, Config);

      Put_Line
        ("Ada Web Server listening on port" & Natural'Image (Server_Port));
      Put_Line ("Endpoints available:");
      Put_Line ("   POST /payments - Process payment requests");
      Put_Line ("   GET /payments-summary - Get payments summary");
      Put_Line ("   GET /health - Health check");
      Put_Line ("");
      Put_Line ("Server ready to accept connections!");

      -- Wait for shutdown signal (use Forever instead of Q_Key_Pressed for container environments)
      Put_Line ("Waiting for connections...");
      AWS.Server.Wait (AWS.Server.Forever);

   exception
      when E : others =>
         Put_Line
           ("Error starting server: " & Ada.Exceptions.Exception_Message (E));
         Ada.Command_Line.Set_Exit_Status (1);
   end;

   -- Cleanup
   AWS.Server.Shutdown (WS);
   Put_Line ("Server shutdown complete");

end Rinha;
