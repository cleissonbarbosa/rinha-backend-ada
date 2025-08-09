package Payment_Handler is

   procedure Initialize;
   
   function Process_Payment (Request_Body : String) return String;
   
   function Parse_JSON_Field (JSON : String; Field : String) return String;

    -- Try to send payment to given processor URL using provided requestedAt (ISO8601 UTC)
    function Try_Payment_Processor (
       URL            : String;
       Correlation_Id : String;
       Amount         : String;
       Requested_At   : String) return Boolean;

end Payment_Handler;
