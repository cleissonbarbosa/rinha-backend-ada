with AWS.Response;
with AWS.Status;

package Rinha_Web_Server is

   procedure Initialize;

   function Service (Request : AWS.Status.Data) return AWS.Response.Data;

   -- Internal handlers
   function Handle_Payment_Request
     (Request : AWS.Status.Data) return AWS.Response.Data;
   function Handle_Summary_Request
     (Request : AWS.Status.Data) return AWS.Response.Data;
   function Handle_Health_Request
     (Request : AWS.Status.Data) return AWS.Response.Data;

   function Handle_Replicate_Request
     (Request : AWS.Status.Data) return AWS.Response.Data;

   function Handle_Internal_Summary_Request
     (Request : AWS.Status.Data) return AWS.Response.Data;

end Rinha_Web_Server;
