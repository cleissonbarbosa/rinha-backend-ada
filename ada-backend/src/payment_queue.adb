with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Payment_Processor_Client; use Payment_Processor_Client;
with Payment_DB; use Payment_DB;

package body Payment_Queue is

   -- Queue configuration similar to Java
   Queue_Size : constant := 12000;
   Worker_Count : constant := 16;
   
   -- Queue storage
   type Payment_Array is array (1 .. Queue_Size) of Unbounded_String;
   Queue : Payment_Array;
   Queue_Head : Natural := 1;
   Queue_Tail : Natural := 1;
   Queue_Count : Natural := 0;
   
   -- Synchronization
   protected Queue_Manager is
      procedure Enqueue (Item : String; Success : out Boolean);
      entry Dequeue (Item : out Unbounded_String);
      function Is_Empty return Boolean;
      function Count return Natural;
   private
      Queue_Full : Boolean := False;
   end Queue_Manager;
   
   -- Worker tasks
   task type Payment_Worker;
   type Worker_Array is array (1 .. Worker_Count) of Payment_Worker;
   Workers : Worker_Array;
   
   -- Queue Manager implementation
   protected body Queue_Manager is
      procedure Enqueue (Item : String; Success : out Boolean) is
      begin
         if Queue_Count < Queue_Size then
            Queue (Queue_Tail) := To_Unbounded_String (Item);
            Queue_Tail := (Queue_Tail mod Queue_Size) + 1;
            Queue_Count := Queue_Count + 1;
            Success := True;
         else
            Success := False;
         end if;
      end Enqueue;
      
      entry Dequeue (Item : out Unbounded_String) when Queue_Count > 0 is
      begin
         Item := Queue (Queue_Head);
         Queue_Head := (Queue_Head mod Queue_Size) + 1;
         Queue_Count := Queue_Count - 1;
      end Dequeue;
      
      function Is_Empty return Boolean is
      begin
         return Queue_Count = 0;
      end Is_Empty;
      
      function Count return Natural is
      begin
         return Queue_Count;
      end Count;
   end Queue_Manager;
   
   -- Worker task implementation
   task body Payment_Worker is
      Payment_JSON : Unbounded_String;
      Correlation_ID : Unbounded_String;
      Amount : Long_Long_Float;
      Success : Boolean;
      Retry_Count : Natural;
      Max_Retries : constant := 3; -- Reduced from 15 for better performance
   begin
      loop
         begin
            -- Get next payment from queue
            Put_Line ("Worker: Waiting for payment from queue...");
            Queue_Manager.Dequeue (Payment_JSON);
            Put_Line ("Worker: Dequeued payment: " & To_String (Payment_JSON));
            
            Retry_Count := 0;
            Success := False;
            
            -- Extract payment data
            declare
               JSON_Str : constant String := To_String (Payment_JSON);
            begin
               -- Extract correlation ID and amount from JSON
               -- This uses the same extraction logic as the existing code
               declare
                  Pos : Natural := JSON_Str'First;
                  Correlation_Start, Correlation_End : Natural;
                  Amount_Start, Amount_End : Natural;
               begin
                  -- Find correlationId
                  Pos := Index (JSON_Str, """correlationId"":");
                  if Pos > 0 then
                     Correlation_Start := Index (JSON_Str, """", From => Pos + 15) + 1;
                     Correlation_End := Index (JSON_Str, """", From => Correlation_Start) - 1;
                     Correlation_ID := To_Unbounded_String (JSON_Str (Correlation_Start .. Correlation_End));
                  else
                     Correlation_ID := To_Unbounded_String ("unknown");
                  end if;
                  
                  -- Find amount
                  Pos := Index (JSON_Str, """amount"":");
                  if Pos > 0 then
                     Amount_Start := Pos + 9;
                     while Amount_Start <= JSON_Str'Last and then JSON_Str (Amount_Start) in ' ' | ':' loop
                        Amount_Start := Amount_Start + 1;
                     end loop;
                     Amount_End := Amount_Start;
                     while Amount_End <= JSON_Str'Last and then JSON_Str (Amount_End) in '0' .. '9' | '.' loop
                        Amount_End := Amount_End + 1;
                     end loop;
                     Amount := Long_Long_Float'Value (JSON_Str (Amount_Start .. Amount_End - 1));
                  else
                     Amount := 0.0;
                  end if;
               end;
            end;
            
            -- Try to process payment with retries (like Java)
            Put_Line ("Worker: Starting payment processing with " & Max_Retries'Image & " max retries");
            while Retry_Count < Max_Retries and not Success loop
               -- Try default processor first
               Put_Line ("Worker: Calling Send_Payment_Default, retry=" & Retry_Count'Image);
               -- Put_Line ("Worker: Payload being sent: " & To_String (Payment_JSON));
               if Send_Payment_Default (To_String (Payment_JSON)) then
                  -- Save to database as DEFAULT with deduplication
                  -- Put_Line ("Worker: Payment sent successfully to DEFAULT processor");
                  declare
                     Is_New : Boolean;
                  begin
                     DB.Add_Default (Float(Amount), To_String (Correlation_ID), Is_New);
                     if Is_New then
                        -- Put_Line ("Worker: DB.Add_Default called successfully with amount: " & Long_Long_Float'Image(Amount));
                        null;
                     else
                        -- Put_Line ("Worker: Payment with correlation ID " & To_String (Correlation_ID) & " already processed, skipping");
                        null;
                     end if;
                  end;
                  Success := True;
                  exit;
               end if;
               
               -- Put_Line ("Worker: Default processor failed, retry_count=" & Retry_Count'Image);
               Retry_Count := Retry_Count + 1;
               
               -- Small delay between retries
               delay 0.001; -- Reduced delay for better performance
            end loop;
            
            -- If default failed after retries, try fallback
            if not Success then
               -- Put_Line ("Worker: Trying fallback processor");
               if Send_Payment_Fallback (To_String (Payment_JSON)) then
                  -- Save to database as FALLBACK with deduplication
                  -- Put_Line ("Worker: Payment sent successfully to FALLBACK processor");
                  declare
                     Is_New : Boolean;
                  begin
                     DB.Add_Fallback (Float(Amount), To_String (Correlation_ID), Is_New);
                     if Is_New then
                        -- Put_Line ("Worker: DB.Add_Fallback called successfully with amount: " & Long_Long_Float'Image(Amount));
                        null;
                     else
                        -- Put_Line ("Worker: Payment with correlation ID " & To_String (Correlation_ID) & " already processed, skipping");
                        null;
                     end if;
                  end;
                  Success := True;
               end if;
            end if;
            
            -- If still failed, re-enqueue for later retry
            if not Success then
               declare
                  Enqueue_Success : Boolean;
               begin
                  Queue_Manager.Enqueue (To_String (Payment_JSON), Enqueue_Success);
                  if not Enqueue_Success then
                     Put_Line ("Payment queue full, dropping payment: " & To_String (Correlation_ID));
                  end if;
               end;
            end if;
            
         exception
            when others =>
               Put_Line ("Worker error processing payment");
         end;
      end loop;
   end Payment_Worker;
   
   procedure Initialize is
   begin
      Put_Line ("Payment Queue initialized with " & Worker_Count'Image & " workers");
   end Initialize;
   
   procedure Enqueue_Payment (Payment_JSON : String) is
      Success : Boolean;
   begin
      Put_Line ("Enqueuing payment: " & Payment_JSON (Payment_JSON'First .. Natural'Min(Payment_JSON'Last, Payment_JSON'First + 50)) & "...");
      Queue_Manager.Enqueue (Payment_JSON, Success);
      if not Success then
         Put_Line ("Payment queue full, payment dropped");
      else
         Put_Line ("Payment enqueued successfully");
      end if;
   end Enqueue_Payment;
   
   procedure Start_Workers is
   begin
      Put_Line ("Payment workers started");
      -- Workers start automatically when package is initialized
   end Start_Workers;
   
   procedure Stop is
   begin
      Put_Line ("Payment queue stopped");
   end Stop;

end Payment_Queue;
