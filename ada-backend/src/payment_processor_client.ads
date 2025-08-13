package Payment_Processor_Client is
   type Send_Result is (Ok, Fail);
   function Send_Payment_Default (JSON : String; Retries : Natural) return Send_Result;
   function Send_Payment_Fallback (JSON : String) return Send_Result;
   function Send_Payment_Default (JSON : String) return Boolean;
   function Send_Payment_Fallback (JSON : String) return Boolean;
end Payment_Processor_Client;
