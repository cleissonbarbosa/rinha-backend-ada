with Ada.Numerics; use Ada.Numerics;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

package Payment_DB is
   type Summary is record
      Default_Total_Requests  : Natural := 0;
      Default_Total_Amount    : Long_Long_Float := 0.0;
      Fallback_Total_Requests : Natural := 0;
      Fallback_Total_Amount   : Long_Long_Float := 0.0;
   end record;

   -- Map for tracking processed correlation IDs to prevent duplicates
   package Correlation_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Boolean,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   protected DB is
      procedure Purge;
      procedure Add_Default (Amount : Float; Correlation_Id : String; Is_New : out Boolean);
      procedure Add_Fallback (Amount : Float; Correlation_Id : String; Is_New : out Boolean);
      function Get_Summary return Summary;
   private
      S : Summary;
      Duplicate_Map : Correlation_Maps.Map;
   end DB;

   function Summary_To_JSON (S : Summary) return String;
end Payment_DB;
