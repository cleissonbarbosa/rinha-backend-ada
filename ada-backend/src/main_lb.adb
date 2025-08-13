with LB_HTTP_Server; use LB_HTTP_Server;
with LB_Socket_Router; use LB_Socket_Router;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main_LB is
begin
   Put_Line ("Starting LB Socket Router...");
   LB_Socket_Router.Start;
   Put_Line ("LB Socket Router started successfully");
   
   -- Start socket processor in background task
   declare
      task Receiver;
      task body Receiver is
      begin
         Put_Line ("Starting to process incoming socket messages...");
         LB_Socket_Router.Process_Incoming;
      end Receiver;
   begin
      -- Give socket processor time to initialize
      delay 0.5;
      Put_Line ("Starting LB HTTP Server...");
      LB_HTTP_Server.Start;
      Put_Line ("Load Balancer fully started and ready");
   end;
   
   -- Keep running forever
   delay 1.0E9;
end Main_LB;
