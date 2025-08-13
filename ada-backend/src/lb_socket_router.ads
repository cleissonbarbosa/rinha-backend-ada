with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package LB_Socket_Router is
   procedure Start;
   procedure Send_To_Any_Backend (Data : String);
   procedure Process_Incoming;
   procedure Await_Response (Out_S : out Unbounded_String);
end LB_Socket_Router;
