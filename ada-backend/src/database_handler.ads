-- Database Handler for Payment Storage
-- This package provides database persistence using ADO (Ada Database Objects)

package Database_Handler is

  procedure Initialize;

   procedure Store_Payment
     (Correlation_Id : String;
      Amount         : Long_Float;
      Processor_Type : String;
      Requested_At   : String);

   function Get_Summary
     (From_Time : String := ""; To_Time : String := "") return String;

   -- Returns processor type for an existing correlation id, or empty string if not found
   function Get_Processor_Type (Correlation_Id : String) return String;

  -- Legacy stub kept for compatibility (no-op in memory mode)
  function Execute_SQL_Query (SQL : String) return String;

end Database_Handler;
