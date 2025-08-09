package HTTP_Server is

   procedure Initialize_Server;

   procedure Start_Server (Port : Natural);

   procedure Stop_Server;

   -- Endpoint handlers
   function Handle_Payment_Request (Request_Body : String) return String;

   function Handle_Summary_Request (Query_Params : String) return String;

   function Handle_Health_Request return String;

end HTTP_Server;
