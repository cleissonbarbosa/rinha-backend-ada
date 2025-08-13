with GNAT.Sockets; use GNAT.Sockets;

package UDP_Socket_Utils is
   procedure Init_Sockets;

   procedure Setup_Bound_Socket (Sock : in out Socket_Type; Port : Positive);
   procedure Setup_Connected_Socket (Sock : in out Socket_Type; Remote_Host : String; Remote_Port : Positive);

   procedure Send_Bytes (Sock : Socket_Type; Data : String);
   procedure Receive_Bytes (Sock : Socket_Type; Buffer : in out String; Last : out Natural);
   function Build_Addr (Host : String; Port : Positive) return Sock_Addr_Type;
   procedure Send_To (Sock : Socket_Type; Data : String; To : Sock_Addr_Type);
end UDP_Socket_Utils;
