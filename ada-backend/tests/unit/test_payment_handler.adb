with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Payment_Handler;

procedure Test_Payment_Handler is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   procedure Test_Parse_JSON_Field is
      JSON : constant String := 
        "{""correlationId"":""123e4567-e89b-12d3-a456-426614174000"",""amount"":100.50}";
   begin
      Put_Line ("=== Testing Parse_JSON_Field ===");
      
      -- Test parsing correlationId
      declare
         Result : constant String := Payment_Handler.Parse_JSON_Field (JSON, "correlationId");
      begin
         if Result = "123e4567-e89b-12d3-a456-426614174000" then
            Put_Line ("✅ PASS: correlationId parsed correctly");
         else
            Put_Line ("❌ FAIL: correlationId parsing - Expected: 123e4567-e89b-12d3-a456-426614174000, Got: " & Result);
         end if;
      end;

      -- Test parsing amount
      declare
         Result : constant String := Payment_Handler.Parse_JSON_Field (JSON, "amount");
      begin
         if Result = "100.50" then
            Put_Line ("✅ PASS: amount parsed correctly");
         else
            Put_Line ("❌ FAIL: amount parsing - Expected: 100.50, Got: " & Result);
         end if;
      end;

      -- Test parsing non-existent field
      declare
         Result : constant String := Payment_Handler.Parse_JSON_Field (JSON, "nonexistent");
      begin
         if Result = "" then
            Put_Line ("✅ PASS: non-existent field returns empty string");
         else
            Put_Line ("❌ FAIL: non-existent field - Expected empty, Got: " & Result);
         end if;
      end;
   end Test_Parse_JSON_Field;

   procedure Test_Payment_Validation is
      Valid_Request : constant String := 
        "{""correlationId"":""123e4567-e89b-12d3-a456-426614174000"",""amount"":100.50}";
      Invalid_UUID : constant String := 
        "{""correlationId"":""invalid-uuid"",""amount"":100.50}";
      Invalid_Amount : constant String := 
        "{""correlationId"":""123e4567-e89b-12d3-a456-426614174000"",""amount"":-50}";
      Missing_Fields : constant String := 
        "{""correlationId"":""123e4567-e89b-12d3-a456-426614174000""}";
   begin
      Put_Line ("=== Testing Payment Validation ===");
      
      -- Note: We can't directly test Process_Payment without setting up the full environment
      -- But we can test the JSON parsing which is part of the validation
      
      -- Test valid request structure
      declare
         Correlation_Id : constant String := Payment_Handler.Parse_JSON_Field (Valid_Request, "correlationId");
         Amount_Str : constant String := Payment_Handler.Parse_JSON_Field (Valid_Request, "amount");
      begin
         if Correlation_Id'Length = 36 and Amount_Str = "100.50" then
            Put_Line ("✅ PASS: Valid request structure parsed correctly");
         else
            Put_Line ("❌ FAIL: Valid request parsing failed");
         end if;
      end;

      -- Test invalid UUID length
      declare
         Correlation_Id : constant String := Payment_Handler.Parse_JSON_Field (Invalid_UUID, "correlationId");
      begin
         if Correlation_Id'Length /= 36 then
            Put_Line ("✅ PASS: Invalid UUID detected (length check)");
         else
            Put_Line ("❌ FAIL: Invalid UUID not detected");
         end if;
      end;

      -- Test missing amount field
      declare
         Amount_Str : constant String := Payment_Handler.Parse_JSON_Field (Missing_Fields, "amount");
      begin
         if Amount_Str = "" then
            Put_Line ("✅ PASS: Missing amount field detected");
         else
            Put_Line ("❌ FAIL: Missing amount field not detected");
         end if;
      end;
   end Test_Payment_Validation;

begin
   Put_Line ("Starting Payment Handler Unit Tests");
   Put_Line ("===================================");
   
   Test_Parse_JSON_Field;
   Test_Payment_Validation;
   
   Put_Line ("===================================");
   Put_Line ("Payment Handler Unit Tests Complete");
end Test_Payment_Handler;
