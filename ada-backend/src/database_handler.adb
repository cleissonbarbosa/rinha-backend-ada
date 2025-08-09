with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package body Database_Handler is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Ada.Strings.Fixed;

   type Payment_Info is record
      Amount         : Long_Float;
      Processor_Type : Unbounded_String; -- "default" | "fallback"
      Requested_At   : Unbounded_String; -- ISO8601 string, lexicographically sortable
   end record;

   function Hash_String (S : String) return Ada.Containers.Hash_Type renames Ada.Strings.Hash;

   package Payment_Map is new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
         Element_Type    => Payment_Info,
         Hash            => Hash_String,
         Equivalent_Keys => "=");

   protected type Payments_Store is
      procedure Put (Correlation_Id : String; Info : Payment_Info);
      function Exists (Correlation_Id : String) return Boolean;
      function Get_Processor (Correlation_Id : String) return String;
      function Summary (From_Time_Str, To_Time_Str : String) return String;
   private
      By_Id : Payment_Map.Map;
   end Payments_Store;

   protected body Payments_Store is
      procedure Put (Correlation_Id : String; Info : Payment_Info) is
      begin
         if not By_Id.Contains (Correlation_Id) then
            By_Id.Insert (Correlation_Id, Info);
         end if;
      end Put;

      function Exists (Correlation_Id : String) return Boolean is
      begin
         return By_Id.Contains (Correlation_Id);
      end Exists;

      function Get_Processor (Correlation_Id : String) return String is
      begin
         if By_Id.Contains (Correlation_Id) then
            return To_String (By_Id.Element (Correlation_Id).Processor_Type);
         else
            return "";
         end if;
      end Get_Processor;

      function Summary (From_Time_Str, To_Time_Str : String) return String is
         Count_Default  : Natural := 0;
         Count_Fallback : Natural := 0;
         Sum_Default    : Long_Float := 0.0;
         Sum_Fallback   : Long_Float := 0.0;

         procedure Consider (Info : Payment_Info) is
            T : constant String := To_String (Info.Requested_At);
         begin
            if (From_Time_Str = "" or else T >= From_Time_Str)
              and then (To_Time_Str = "" or else T <= To_Time_Str)
            then
               if To_String (Info.Processor_Type) = "default" then
                  Count_Default := Count_Default + 1;
                  Sum_Default := Sum_Default + Info.Amount;
               else
                  Count_Fallback := Count_Fallback + 1;
                  Sum_Fallback := Sum_Fallback + Info.Amount;
               end if;
            end if;
         end Consider;

         C : Payment_Map.Cursor := By_Id.First;
      begin
         while Payment_Map.Has_Element (C) loop
            Consider (Payment_Map.Element (C));
            C := Payment_Map.Next (C);
         end loop;

         declare
            JSON : Unbounded_String := To_Unbounded_String ("{");
            CDef : constant String := Trim (Integer'Image (Integer (Count_Default)), Ada.Strings.Left);
            CFbk : constant String := Trim (Integer'Image (Integer (Count_Fallback)), Ada.Strings.Left);
            SDef : constant String := Trim (Long_Float'Image (Sum_Default), Ada.Strings.Left);
            SFbk : constant String := Trim (Long_Float'Image (Sum_Fallback), Ada.Strings.Left);
         begin
            Append (JSON, '"' & "default" & '"' & ":{" & '"' & "totalRequests" & '"' & ":" & CDef & "," & '"' & "totalAmount" & '"' & ":" & SDef & "}");
            Append (JSON, ",");
            Append (JSON, '"' & "fallback" & '"' & ":{" & '"' & "totalRequests" & '"' & ":" & CFbk & "," & '"' & "totalAmount" & '"' & ":" & SFbk & "}");
            Append (JSON, "}");
            return To_String (JSON);
         end;
      end Summary;
   end Payments_Store;

   Store       : Payments_Store;
   Initialized : Boolean := False;

   procedure Initialize is
   begin
      if not Initialized then
         Put_Line ("Initializing in-memory store...");
         Initialized := True;
      end if;
   end Initialize;

   procedure Store_Payment
     (Correlation_Id : String;
      Amount         : Long_Float;
      Processor_Type : String;
      Requested_At   : String) is
   begin
      if not Initialized then
         Put_Line ("In-memory store not initialized, cannot store payment");
         return;
      end if;

      declare
         Info : Payment_Info := (
           Amount         => Amount,
           Processor_Type => To_Unbounded_String (Processor_Type),
           Requested_At   => To_Unbounded_String (Requested_At)
         );
      begin
         Store.Put (Correlation_Id, Info);
      end;
   end Store_Payment;

   function Execute_SQL_Query (SQL : String) return String is
   begin
      pragma Unreferenced (SQL);
      return "";
   end Execute_SQL_Query;

   function Get_Summary
     (From_Time : String := ""; To_Time : String := "") return String is
   begin
   if not Initialized then
      return "{""default"":{""totalRequests"":0,""totalAmount"":0.0},""fallback"":{""totalRequests"":0,""totalAmount"":0.0}}";
      end if;

   return Store.Summary (From_Time, To_Time);
   end Get_Summary;

   function Get_Processor_Type (Correlation_Id : String) return String is
   begin
      if not Initialized then
         return "";
      end if;
   return Store.Get_Processor (Correlation_Id);
   end Get_Processor_Type;

end Database_Handler;
