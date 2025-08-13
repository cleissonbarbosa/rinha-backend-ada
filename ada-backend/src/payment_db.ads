with Ada.Numerics; use Ada.Numerics;

package Payment_DB is
   type Summary is record
      Default_Total_Requests  : Natural := 0;
      Default_Total_Amount    : Long_Long_Float := 0.0;
      Fallback_Total_Requests : Natural := 0;
      Fallback_Total_Amount   : Long_Long_Float := 0.0;
   end record;

   protected DB is
      procedure Purge;
      procedure Add_Default (Amount : Long_Long_Float);
      procedure Add_Fallback (Amount : Long_Long_Float);
      function Get_Summary return Summary;
   private
      S : Summary;
   end DB;

   function Summary_To_JSON (S : Summary) return String;
end Payment_DB;
