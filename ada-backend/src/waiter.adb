with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Waiter is
   task body Single_Slot is
      Slot : Unbounded_String := To_Unbounded_String("");
      Full : Boolean := False;
   begin
      loop
         select
            when not Full =>
               accept Put (S : String) do
                  Slot := To_Unbounded_String (S);
                  Full := True;
               end Put;
         or
            when Full =>
               accept Take (S : out Unbounded_String) do
                  S := Slot;
                  Slot := To_Unbounded_String("");
                  Full := False;
               end Take;
         end select;
      end loop;
   end Single_Slot;
end Waiter;
