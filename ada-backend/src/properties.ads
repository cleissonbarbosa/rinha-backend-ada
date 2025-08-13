with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Properties is
   function Get_Env (Name : String; Default : String := "") return String;

   function Backend1_Host return String;
   function Backend1_Port return Positive;
   function Backend2_Host return String;
   function Backend2_Port return Positive;

   function Udp_Channel_Port return Positive;           -- inbound from LB
   function Udp_Channel_Internal_Port return Positive;  -- inbound from peer backend
   function External_Udp_Host return String;            -- peer backend host
   function External_Udp_Port return Positive;          -- peer backend port

   function Processor_Default_URL return String;
   function Processor_Fallback_URL return String;
   function Retry_Default return Natural;
   function Handler_Udp_Pool_Size return Natural;
end Properties;
