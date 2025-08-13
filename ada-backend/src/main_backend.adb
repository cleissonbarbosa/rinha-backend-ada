with Backend_UDP_Router; use Backend_UDP_Router;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main_Backend is
begin
   Put_Line ("Starting Backend UDP Router...");
   Backend_UDP_Router.Start;
   Put_Line ("Backend UDP Router started successfully");
   Put_Line ("Starting to process incoming UDP messages...");
   Backend_UDP_Router.Process_Incoming;
   Put_Line ("Backend message processing ended (should never reach here)");
end Main_Backend;
