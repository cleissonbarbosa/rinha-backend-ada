with Ada.Strings.Unbounded;
with Payment_Types;

package Database is

   use Ada.Strings.Unbounded;
   use Payment_Types;

   procedure Initialize_Connection;

   procedure Store_Payment (Payment : Payment_Record);

   function Get_Summary
     (From_Time : String := ""; To_Time : String := "")
      return Summary_Response;

   procedure Close_Connection;

end Database;
