with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Sockets; use GNAT.Sockets;

package Backend_UDP_Router is
   procedure Start;
   procedure Process_Incoming;
   procedure Send_Summary_To_LB (Payload : String; Address : Sock_Addr_Type);
end Backend_UDP_Router;
