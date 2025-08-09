with Ada.Strings.Unbounded;
with Ada.Calendar;

package Payment_Types is

   use Ada.Strings.Unbounded;

   type Payment_Record is record
      Correlation_Id : Unbounded_String;
      Amount         : Long_Float;
      Requested_At   : Ada.Calendar.Time;
      Processor_Type : Unbounded_String; -- "default" or "fallback"
   end record;

   type Payment_Summary is record
      Total_Requests : Natural;
      Total_Amount   : Long_Float;
   end record;

   type Summary_Response is record
      Default_Summary  : Payment_Summary;
      Fallback_Summary : Payment_Summary;
   end record;

   type Health_Status is record
      Failing           : Boolean;
      Min_Response_Time : Natural;
   end record;

end Payment_Types;
