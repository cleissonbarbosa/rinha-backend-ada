with AWS.Client; use AWS.Client;
with AWS.Response; use AWS.Response;
with AWS.Messages; use AWS.Messages;
with AWS.MIME; use AWS.MIME;
with Properties; use Properties;

package body Payment_Processor_Client is

   function Do_Post (URL : String; Payload : String) return Boolean is
      Resp : AWS.Response.Data;
   begin
      Resp := AWS.Client.Post (URL, Payload, Content_Type => AWS.MIME.Application_JSON);
   -- Treat 200 OK as success; fall back to False on exceptions
   return AWS.Response.Status_Code (Resp) = AWS.Messages.S200;
   exception
      when others => return False;
   end Do_Post;

   function Send_Payment_Default (JSON : String; Retries : Natural) return Send_Result is
      URL : constant String := Processor_Default_URL;
   begin
      for I in 1 .. Natural'Max (1, Retries) loop
         if Do_Post (URL, JSON) then
            return Ok;
         end if;
      end loop;
      return Fail;
   end Send_Payment_Default;

   function Send_Payment_Fallback (JSON : String) return Send_Result is
      URL : constant String := Processor_Fallback_URL;
   begin
      if Do_Post (URL, JSON) then
         return Ok;
      else
         return Fail;
      end if;
   end Send_Payment_Fallback;

   -- Boolean versions for the queue system
   function Send_Payment_Default (JSON : String) return Boolean is
   begin
      return Send_Payment_Default (JSON, 1) = Ok;
   end Send_Payment_Default;

   function Send_Payment_Fallback (JSON : String) return Boolean is
   begin
      return Send_Payment_Fallback (JSON) = Ok;
   end Send_Payment_Fallback;

end Payment_Processor_Client;
