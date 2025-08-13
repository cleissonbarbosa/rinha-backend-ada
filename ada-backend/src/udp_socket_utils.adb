with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Streams; use Ada.Streams;
with GNAT.Sockets; use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

package body UDP_Socket_Utils is

   function Resolve_Hostname (Hostname : String) return Inet_Addr_Type is
   begin
      -- Check for known Docker hostnames first (static IP mapping)
      if Hostname = "backend-1" then
         return Inet_Addr ("172.20.0.10");
      elsif Hostname = "backend-2" then
         return Inet_Addr ("172.20.0.11");
      end if;
      
      -- Try direct IP parsing first for performance
      begin
         declare
            Addr : constant Inet_Addr_Type := Inet_Addr (Hostname);
         begin
            return Addr;
         end;
      exception
         when Constraint_Error =>
            -- If not an IP, resolve hostname
            begin
               declare
                  Host_Entry : constant Host_Entry_Type := Get_Host_By_Name (Hostname);
               begin
                  return Addresses (Host_Entry, 1);
               end;
            exception
               when Name_Error =>
                  -- Return any address as fallback
                  return Any_Inet_Addr;
               when others =>
                  return Any_Inet_Addr;
            end;
      end;
   end Resolve_Hostname;

   procedure Init_Sockets is
   begin
      GNAT.Sockets.Initialize;
   end Init_Sockets;

   procedure Setup_Bound_Socket (Sock : in out Socket_Type; Port : Positive) is
      Addr : Sock_Addr_Type;
   begin
      Create_Socket (Sock, Family_Inet, Socket_Datagram);
      Set_Socket_Option (Sock, Socket_Level, (Reuse_Address, True));
      Addr.Addr := Any_Inet_Addr;
      Addr.Port := Port_Type (Port);
      Bind_Socket (Sock, Addr);
   end Setup_Bound_Socket;

   procedure Setup_Connected_Socket (Sock : in out Socket_Type; Remote_Host : String; Remote_Port : Positive) is
      Addr : Sock_Addr_Type;
   begin
      Create_Socket (Sock, Family_Inet, Socket_Datagram);
      Addr := Build_Addr (Remote_Host, Remote_Port);
      Connect_Socket (Sock, Addr);
   end Setup_Connected_Socket;

   procedure Send_Bytes (Sock : Socket_Type; Data : String) is
      Stream_Data : Stream_Element_Array (1 .. Data'Length);
      Last        : Stream_Element_Offset;
   begin
      for I in Data'Range loop
         Stream_Data (Stream_Element_Offset (I - Data'First + 1)) := Stream_Element (Character'Pos (Data (I)));
      end loop;
      Send_Socket (Sock, Stream_Data, Last);
   end Send_Bytes;

   procedure Receive_Bytes (Sock : Socket_Type; Buffer : in out String; Last : out Natural) is
      SEA  : Stream_Element_Array (1 .. Buffer'Length);
      Last_SE : Stream_Element_Offset := 0;
   begin
      Receive_Socket (Sock, SEA, Last_SE);
      Last := Natural (Last_SE);
      for I in 1 .. Last loop
         Buffer (I) := Character'Val (Integer (SEA (Stream_Element_Offset (I))));
      end loop;
   end Receive_Bytes;

   function Build_Addr (Host : String; Port : Positive) return Sock_Addr_Type is
      Addr : Sock_Addr_Type;
   begin
      Addr.Addr := Resolve_Hostname (Host);
      Addr.Port := Port_Type (Port);
      return Addr;
   exception
      when E : others =>
         Put_Line ("UDP: ERROR building address: " & Ada.Exceptions.Exception_Information (E));
         Ada.Text_IO.Flush;
         -- Silent error handling for performance
         Addr.Addr := Any_Inet_Addr;
         Addr.Port := Port_Type (Port);
         return Addr;
   end Build_Addr;

   procedure Send_To (Sock : Socket_Type; Data : String; To : Sock_Addr_Type) is
      Buf  : Stream_Element_Array (1 .. Data'Length);
      Last : Stream_Element_Offset := Buf'Last;
   begin
      -- Fast conversion without logging in critical path
      for I in Data'Range loop
         Buf (Stream_Element_Offset (I - Data'First + 1)) := Stream_Element (Character'Pos (Data (I)));
      end loop;
      Send_Socket (Sock, Buf, Last, To);
   exception
      when E : others =>
         Put_Line ("UDP: ERROR in Send_To: " & Ada.Exceptions.Exception_Information (E));
         Ada.Text_IO.Flush;
         raise;
   end Send_To;

end UDP_Socket_Utils;
