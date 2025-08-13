with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with AWS.Server;
with AWS.Status;
with AWS.Response;
with AWS.MIME;
with AWS.Messages;
with LB_Socket_Router;
with LB_Event_Types;

package body LB_HTTP_Server is
   HTTP : AWS.Server.HTTP;

   function Root (Req : in AWS.Status.Data) return AWS.Response.Data is
   begin
      return AWS.Response.Build (AWS.MIME.Text_Plain, "OK");
   end Root;

   function Payments (Req : in AWS.Status.Data) return AWS.Response.Data is
      Req_Body : constant String := Ada.Strings.Unbounded.To_String (AWS.Status.Binary_Data (Req));
   begin
      Put_Line ("LB: Processing payments request, body: " & Req_Body);
      -- Direct call instead of async task for debugging
      Put_Line ("LB: About to call Send_To_Any_Backend directly");
      LB_Socket_Router.Send_To_Any_Backend (Req_Body & LB_Event_Types.PAYMENT_POST);
      Put_Line ("LB: Send_To_Any_Backend completed");
      Put_Line ("LB: Returning payments response");
      return AWS.Response.Build (AWS.MIME.Text_Plain, "ok");
   end Payments;

   function Purge (Req : in AWS.Status.Data) return AWS.Response.Data is
   begin
      LB_Socket_Router.Send_To_Any_Backend (String'(1 => LB_Event_Types.PURGE));
      return AWS.Response.Build (AWS.MIME.Text_Plain, "ok");
   end Purge;

    function Summary (Req : in AWS.Status.Data) return AWS.Response.Data is
         From_U   : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
         To_U     : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Null_Unbounded_String;
      function Get_Param (Key : String) return String is
      begin
         return AWS.Status.Parameter (Req, Key);
      end Get_Param;
   begin
      -- Remove verbose logging for performance
      declare
         F : constant String := Get_Param ("from");
         T : constant String := Get_Param ("to");
      begin
         if F = "" and T = "" then
                  From_U := Ada.Strings.Unbounded.To_Unbounded_String ("2000-01-01T00:00:00.000Z");
                  To_U   := From_U;
         else
                  From_U := Ada.Strings.Unbounded.To_Unbounded_String (
                     (if F = "" then "2000-01-01T00:00:00.000Z" else F));
                  To_U   := Ada.Strings.Unbounded.To_Unbounded_String (
                     (if T = "" then Ada.Strings.Unbounded.To_String (From_U) else T));
         end if;
      end;
      declare
             From    : constant String := Ada.Strings.Unbounded.To_String (From_U);
             To_Val  : constant String := Ada.Strings.Unbounded.To_String (To_U);
             Payload : constant String := From & "@" & To_Val & LB_Event_Types.PAYMENT_SUMMARY;
         R      : Ada.Strings.Unbounded.Unbounded_String;
      begin
         Put_Line ("LB: Processing summary request, payload: " & Payload);
         LB_Socket_Router.Send_To_Any_Backend (Payload);
         Put_Line ("LB: Waiting for summary response...");
         LB_Socket_Router.Await_Response (R);
         Put_Line ("LB: Received summary response: " & Ada.Strings.Unbounded.To_String (R));
         return AWS.Response.Build (AWS.MIME.Application_JSON, Ada.Strings.Unbounded.To_String (R));
      end;
   end Summary;

   function Dispatch (Req : AWS.Status.Data) return AWS.Response.Data is
      Path : constant String := AWS.Status.URI (Req);
   begin
      -- Remove verbose logging for performance
      if Path = "/" then
         return Root (Req);
      elsif Path = "/payments" then
         return Payments (Req);
      elsif Path = "/purge-payments" then
         return Purge (Req);
      elsif Path = "/payments-summary" then
         return Summary (Req);
      else
         return AWS.Response.Build (AWS.MIME.Text_Plain, "not found", Status_Code => AWS.Messages.S404);
      end if;
   end Dispatch;

   procedure Start is
   begin
      AWS.Server.Start (HTTP, "LB_Ada", Dispatch'Access, Port => 9999);
      Ada.Text_IO.Put_Line ("LB HTTP server started on :9999");
   end Start;
end LB_HTTP_Server;
