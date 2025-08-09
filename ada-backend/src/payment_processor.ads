with Ada.Strings.Unbounded;
with Payment_Types;

package Payment_Processor is

   use Ada.Strings.Unbounded;
   use Payment_Types;

   procedure Initialize;

   function Process_Payment
     (Correlation_Id : String; Amount : Long_Float; Requested_At : String)
      return Boolean;

   function Get_Health_Status (Processor : String) return Health_Status;

   procedure Cleanup;

end Payment_Processor;
