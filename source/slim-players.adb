--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

with Slim.Players.Displays;
with Slim.Message_Decoders;
with Slim.Message_Visiters;
with Slim.Messages.strm;
with Slim.Players.Connected_State_Visiters;
with Slim.Players.Idle_State_Visiters;
with Slim.Players.Play_State_Visiters;

package body Slim.Players is

   Hearbeat_Period : constant := 5.0;

   procedure Read_Message
     (Socket  : GNAT.Sockets.Socket_Type;
      Message : out Slim.Messages.Message_Access);

   procedure Send_Hearbeat (Self : in out Player'Class);

   procedure Read_Splash
     (Splash : out League.Stream_Element_Vectors.Stream_Element_Vector;
      File   : League.Strings.Universal_String);

   procedure Free is new Ada.Unchecked_Deallocation
     (Slim.Messages.Message'Class, Slim.Messages.Message_Access);

   -----------------
   -- Get_Display --
   -----------------

   function Get_Display
     (Self   : Player;
      Height : Positive := 32;
      Width  : Positive := 160) return Slim.Players.Displays.Display
   is
      Size : constant Ada.Streams.Stream_Element_Offset :=
       Ada.Streams.Stream_Element_Offset (Height * Width / 8);
   begin
      return Result : Slim.Players.Displays.Display (Size) do
         Slim.Players.Displays.Initialize (Result, Self);
      end return;
   end Get_Display;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self   : in out Player;
      Socket : GNAT.Sockets.Socket_Type;
      Font   : League.Strings.Universal_String;
      Splash : League.Strings.Universal_String) is
   begin
      Self.Socket := Socket;

      GNAT.Sockets.Set_Socket_Option
        (Self.Socket,
         GNAT.Sockets.Socket_Level,
         (GNAT.Sockets.Receive_Timeout, Hearbeat_Period));

      Slim.Fonts.Read (Self.Font, Font);
      Read_Splash (Self.Splash, Splash);
   end Initialize;

   ---------------------
   -- Process_Message --
   ---------------------

   not overriding procedure Process_Message (Self : in out Player) is
      use type Ada.Calendar.Time;

      function Get_Visiter return Slim.Message_Visiters.Visiter'Class;

      function Get_Visiter return Slim.Message_Visiters.Visiter'Class is
      begin
         case Self.State.Kind is
            when Connected =>
               return V : Connected_State_Visiters.Visiter
                 (Self'Unchecked_Access);
            when Idle =>
               return V : Idle_State_Visiters.Visiter
                 (Self'Unchecked_Access);
            when Play =>
               return V : Play_State_Visiters.Visiter
                 (Self'Unchecked_Access);
         end case;
      end Get_Visiter;

      V : Slim.Message_Visiters.Visiter'Class := Get_Visiter;
   begin
      if Self.State.Kind /= Connected
        and then Ada.Calendar.Clock - Self.Ping > 5.0
      then
         Send_Hearbeat (Self);
      end if;

      declare
         Message : Slim.Messages.Message_Access;
      begin
         Read_Message (Self.Socket, Message);
         Message.Visit (V);
         Free (Message);
      exception
         when E : GNAT.Sockets.Socket_Error =>
            case GNAT.Sockets.Resolve_Exception (E) is
               when GNAT.Sockets.Resource_Temporarily_Unavailable =>
                  Send_Hearbeat (Self);
               when others =>
                  raise;
            end case;
      end;
   end Process_Message;

   ------------------
   -- Read_Message --
   ------------------

   procedure Read_Message
     (Socket  : GNAT.Sockets.Socket_Type;
      Message : out Slim.Messages.Message_Access)
   is
      use type Ada.Streams.Stream_Element_Offset;
      Tag     : Slim.Messages.Message_Tag;
      Raw_Tag : Ada.Streams.Stream_Element_Array (1 .. 4)
        with Address => Tag'Address;
      Word    : Ada.Streams.Stream_Element_Array (1 .. 4);
      Length  : Ada.Streams.Stream_Element_Offset := 0;
      Last    : Ada.Streams.Stream_Element_Offset;
      Data    : aliased League.Stream_Element_Vectors.Stream_Element_Vector;
      Decoder : Slim.Message_Decoders.Decoder;
   begin
      GNAT.Sockets.Receive_Socket (Socket, Raw_Tag, Last);

      if Last = 0 then
         --  Timeout
         return;
      end if;

      pragma Assert (Last = Raw_Tag'Length);
      GNAT.Sockets.Receive_Socket (Socket, Word, Last);
      pragma Assert (Last = Word'Length);

      for Byte of Word loop
         Length := Length * 256 + Ada.Streams.Stream_Element_Offset (Byte);
      end loop;

      while Length > 0 loop
         declare
            Piece : constant Ada.Streams.Stream_Element_Offset :=
              Ada.Streams.Stream_Element_Offset'Min (Length, 256);
            Input : Ada.Streams.Stream_Element_Array (1 .. Piece);
         begin
            GNAT.Sockets.Receive_Socket (Socket, Input, Last);
            pragma Assert (Last = Input'Length);
            Data.Append (Input);
            Length := Length - Last;
         end;
      end loop;

      Decoder.Decode (Tag, Data'Unchecked_Access, Message);
   end Read_Message;

   -----------------
   -- Read_Splash --
   -----------------

   procedure Read_Splash
     (Splash : out League.Stream_Element_Vectors.Stream_Element_Vector;
      File   : League.Strings.Universal_String)
   is
      Size : constant := 32 * 160 / 8;
      Data : Ada.Streams.Stream_Element_Array (1 .. Size);
      Last : Ada.Streams.Stream_Element_Offset;
      Input : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (Input,
         Mode => Ada.Streams.Stream_IO.In_File,
         Name => File.To_UTF_8_String);
      Ada.Streams.Stream_IO.Read (Input, Data, Last);
      pragma Assert (Last in Data'Last);
      Ada.Streams.Stream_IO.Close (Input);
      Splash.Clear;
      Splash.Append (Data);
   end Read_Splash;

   -------------------
   -- Send_Hearbeat --
   -------------------

   procedure Send_Hearbeat (Self : in out Player'Class) is
      strm : Slim.Messages.strm.Strm_Message;
   begin
      strm.Simple_Command
        (Command => Slim.Messages.strm.Status);
      Write_Message (Self.Socket, strm);
      Self.Ping := Ada.Calendar.Clock;
   end Send_Hearbeat;

   -------------------
   -- Write_Message --
   -------------------

   procedure Write_Message
     (Socket  : GNAT.Sockets.Socket_Type;
      Message : Slim.Messages.Message'Class)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Array;
      Data    : League.Stream_Element_Vectors.Stream_Element_Vector;
      Tag     : Slim.Messages.Message_Tag;
      Raw_Tag : Ada.Streams.Stream_Element_Array (1 .. 4)
        with Address => Tag'Address;
      Word    : Ada.Streams.Stream_Element_Array (1 .. 2);
      Length  : Ada.Streams.Stream_Element_Offset;
      Last    : Ada.Streams.Stream_Element_Offset;
   begin
      Message.Write (Tag, Data);
      Length := Raw_Tag'Length + Data.Length;

      Word (1) := Ada.Streams.Stream_Element
        (Length / Ada.Streams.Stream_Element'Modulus);
      Word (2) := Ada.Streams.Stream_Element
        (Length mod Ada.Streams.Stream_Element'Modulus);

      GNAT.Sockets.Send_Socket
        (Socket, Word & Raw_Tag & Data.To_Stream_Element_Array, Last);
      pragma Assert (Last = Length + Word'Length);
   end Write_Message;

end Slim.Players;