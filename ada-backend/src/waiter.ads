with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Waiter is
   task type Single_Slot is
      entry Put (S : String);
      entry Take (S : out Unbounded_String);
   end Single_Slot;
end Waiter;
