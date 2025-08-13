with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Properties is

   function Get_Env (Name : String; Default : String := "") return String is
      Val : constant String := Value (Name, Default);
   begin
      return Val;
   end Get_Env;

   function To_Positive (S : String; Default : Positive) return Positive is
   begin
      if S'Length = 0 then
         return Default;
      end if;
      declare
         N : Integer := Integer'Value (S);
      begin
         if N <= 0 then
            return Default;
         else
            return Positive (N);
         end if;
      exception
         when others => return Default;
      end;
   end To_Positive;

   function To_Natural (S : String; Default : Natural) return Natural is
   begin
      if S'Length = 0 then
         return Default;
      end if;
      declare
         N : Integer := Integer'Value (S);
      begin
         if N < 0 then
            return Default;
         else
            return Natural (N);
         end if;
      exception
         when others => return Default;
      end;
   end To_Natural;

   function Backend1_Host return String is (Get_Env ("BACK_END_1_URL", "backend-1"));
   function Backend1_Port return Positive is (To_Positive (Get_Env ("BACK_END_1_PORT", "9001"), 9001));
   function Backend2_Host return String is (Get_Env ("BACK_END_2_URL", "backend-2"));
   function Backend2_Port return Positive is (To_Positive (Get_Env ("BACK_END_2_PORT", "9002"), 9002));

   function Udp_Channel_Port return Positive is (To_Positive (Get_Env ("UDP_CHANNEL_PORT", "9001"), 9001));
   function Udp_Channel_Internal_Port return Positive is (To_Positive (Get_Env ("UDP_CHANNEL_INTERNAL_PORT", "9003"), 9003));
   function External_Udp_Host return String is (Get_Env ("EXTERNAL_UDP_HOST", "backend-2"));
   function External_Udp_Port return Positive is (To_Positive (Get_Env ("EXTERNAL_UDP_PORT", "9004"), 9004));

   function Processor_Default_URL return String is (Get_Env ("URL_PROCESSOR_DEFAULT", "http://payment-processor-default:8080/payments"));
   function Processor_Fallback_URL return String is (Get_Env ("URL_PROCESSOR_FALLBACK", "http://payment-processor-fallback:8080/payments"));
   function Retry_Default return Natural is (To_Natural (Get_Env ("RETRY_API_DEFAULT", "10"), 10));
   function Handler_Udp_Pool_Size return Natural is (To_Natural (Get_Env ("HANDLER_UDP_POOL_SIZE", "4"), 4));

end Properties;
