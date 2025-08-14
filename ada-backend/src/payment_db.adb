with Ada.Text_IO; use Ada.Text_IO;

package body Payment_DB is

   protected body DB is
      procedure Purge is
      begin
         S := (others => <>);
         Duplicate_Map.Clear;
      end Purge;
      
      procedure Add_Default (Amount : Float; Correlation_Id : String; Is_New : out Boolean) is
         ID : constant Unbounded_String := To_Unbounded_String (Correlation_Id);
      begin
         if Duplicate_Map.Contains (ID) then
            Is_New := False;  -- Pagamento duplicado
            return;
         end if;
         Duplicate_Map.Insert (ID, True);
         S.Default_Total_Requests := S.Default_Total_Requests + 1;
         S.Default_Total_Amount := S.Default_Total_Amount + Long_Long_Float(Amount);
         Is_New := True;
      end Add_Default;
      
      procedure Add_Fallback (Amount : Float; Correlation_Id : String; Is_New : out Boolean) is
         ID : constant Unbounded_String := To_Unbounded_String (Correlation_Id);
      begin
         if Duplicate_Map.Contains (ID) then
            Is_New := False;  -- Pagamento duplicado
            return;
         end if;
         Duplicate_Map.Insert (ID, True);
         S.Fallback_Total_Requests := S.Fallback_Total_Requests + 1;
         S.Fallback_Total_Amount := S.Fallback_Total_Amount + Long_Long_Float(Amount);
         Is_New := True;
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
