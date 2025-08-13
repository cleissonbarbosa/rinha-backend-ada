with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Payment_Queue is
   procedure Initialize;
   procedure Enqueue_Payment (Payment_JSON : String);
   procedure Start_Workers;
   procedure Stop;
end Payment_Queue;
