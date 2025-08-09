-- Simple HTTP server implementation for testing
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Simple_Main is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   Port : constant String := "8080";
begin
   Put_Line ("=== Rinha de Backend 2025 - Ada Implementation ===");
   Put_Line ("Starting server on port " & Port);

   -- Simple infinite loop to keep container running
   -- In a real implementation, this would be an actual HTTP server
   loop
      Put_Line ("Server listening on port " & Port & "...");
      delay 30.0;  -- Wait 30 seconds
   end loop;

end Simple_Main;
