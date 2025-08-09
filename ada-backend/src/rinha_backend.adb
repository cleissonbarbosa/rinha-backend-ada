with Ada.Text_IO;
with Ada.Environment_Variables;
with Ada.Command_Line;
with Ada.Exceptions;
with HTTP_Server;

procedure Rinha_Backend is

   use Ada.Text_IO;

   Server_Port : Natural := 8080;

begin
   Put_Line ("=== Rinha de Backend 2025 - Ada Implementation ===");

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

   Put_Line ("Starting server on port" & Natural'Image (Server_Port));

   -- Initialize and start the HTTP server
   begin
      HTTP_Server.Initialize_Server;
      HTTP_Server.Start_Server (Server_Port);
   exception
      when E : others =>
         Put_Line
           ("Error starting server: " & Ada.Exceptions.Exception_Message (E));
         Ada.Command_Line.Set_Exit_Status (1);
   end;

   -- Cleanup
   HTTP_Server.Stop_Server;
   Put_Line ("Server shutdown complete");

end Rinha_Backend;
