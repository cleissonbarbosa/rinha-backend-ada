with Ada.Text_IO; use Ada.Text_IO;

package body Payment_DB is

   protected body DB is
      procedure Purge is
      begin
         S := (others => <>);
      end Purge;
      procedure Add_Default (Amount : Long_Long_Float) is
      begin
         S.Default_Total_Requests := S.Default_Total_Requests + 1;
         S.Default_Total_Amount   := S.Default_Total_Amount + Amount;
      end Add_Default;
      procedure Add_Fallback (Amount : Long_Long_Float) is
      begin
         S.Fallback_Total_Requests := S.Fallback_Total_Requests + 1;
         S.Fallback_Total_Amount   := S.Fallback_Total_Amount + Amount;
      end Add_Fallback;
      function Get_Summary return Summary is
      begin
         return S;
      end Get_Summary;
   end DB;

   function Summary_To_JSON (S : Summary) return String is
      function Img (X : Long_Long_Float) return String is (Long_Long_Float'Image (X));
      function Trim_Img (X : Long_Long_Float) return String is
         I : constant String := Img (X);
      begin
         if I'Length > 0 and then I (I'First) = ' ' then
            return I (I'First + 1 .. I'Last);
         else
            return I;
         end if;
      end Trim_Img;
   begin
      return "{""default"":{""totalRequests"":" & Integer'Image (S.Default_Total_Requests) &
        ",""totalAmount"":" & Trim_Img (S.Default_Total_Amount) & "},""fallback"":{""totalRequests"":" &
        Integer'Image (S.Fallback_Total_Requests) & ",""totalAmount"":" & Trim_Img (S.Fallback_Total_Amount) & "}}";
   end Summary_To_JSON;

end Payment_DB;
