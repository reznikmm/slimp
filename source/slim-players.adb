--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO;

with League.IRIs;

with Slim.Menu_Models.JSON;
with Slim.Message_Decoders;
with Slim.Message_Visiters;
with Slim.Messages.audg;
with Slim.Messages.strm;
with Slim.Players.Connected_State_Visiters;
with Slim.Players.Displays;
with Slim.Players.Idle_State_Visiters;
with Slim.Players.Play_Radio_Visiters;
with Slim.Players.Play_Files_Visiters;
with League.String_Vectors;

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

   ----------------
   -- First_Menu --
   ----------------

   function First_Menu
     (Self : Player'Class) return Slim.Menu_Views.Menu_View
   is
      Ignore : Boolean;
   begin
      return Result : Slim.Menu_Views.Menu_View do
         Result.Initialize
           (Menu => Slim.Menu_Models.Menu_Model_Access (Self.Menu),
            Font => Self.Font'Unchecked_Access);
      end return;
   end First_Menu;

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

   procedure Initialize
     (Self   : in out Player'Class;
      Socket : GNAT.Sockets.Socket_Type;
      Font   : League.Strings.Universal_String;
      Splash : League.Strings.Universal_String;
      Menu   : League.Strings.Universal_String) is
   begin
      Self.Socket := Socket;

      GNAT.Sockets.Set_Socket_Option
        (Self.Socket,
         GNAT.Sockets.Socket_Level,
         (GNAT.Sockets.Receive_Timeout, Hearbeat_Period));

      Slim.Fonts.Read (Self.Font, Font);
      Read_Splash (Self.Splash, Splash);

      Self.Menu :=
        new Slim.Menu_Models.JSON.JSON_Menu_Model (Self'Unchecked_Access);

      Slim.Menu_Models.JSON.JSON_Menu_Model (Self.Menu.all).Initialize
        (File => Menu);
   end Initialize;

   ----------------
   -- Play_Files --
   ----------------

   procedure Play_Files
     (Self : in out Player'Class;
      List : Song_Array)
   is
      use type Ada.Calendar.Time;

      Playlist : Song_Vectors.Vector;
   begin
      for X of List loop
         Playlist.Append (X);
      end loop;

      Self.State :=
        (Play_Files,
         (Volume          => 30,
          Volume_Set_Time => Ada.Calendar.Clock - 60.0,
          Current_Song    => List (1).Title,
          Paused          => False,
          Seconds         => 0),
         Playlist, 1);

      Self.Request_Next_File;
   end Play_Files;

   --------------------
   -- Play_Next_File --
   --------------------

   procedure Play_Next_File
     (Self      : in out Player'Class;
      Immediate : Boolean := True) is
   begin
      if Self.State.Kind /= Play_Files then
         return;
      elsif Immediate then
         declare
            strm : Slim.Messages.strm.Strm_Message;
         begin
            strm.Simple_Command (Command => Slim.Messages.strm.Flush);
            Write_Message (Self.Socket, strm);
         end;
      end if;

      if Self.State.Index < Self.State.Playlist.Last_Index then
         Self.State.Index := Self.State.Index + 1;
         Self.Request_Next_File;
      else
         Self.Stop;
      end if;
   end Play_Next_File;

   ------------------------
   -- Play_Previous_File --
   ------------------------

   procedure Play_Previous_File (Self      : in out Player'Class;
                                 Immediate : Boolean := True) is
   begin
      if Self.State.Kind /= Play_Files then
         return;
      elsif Immediate then
         declare
            strm : Slim.Messages.strm.Strm_Message;
         begin
            strm.Simple_Command (Command => Slim.Messages.strm.Flush);
            Write_Message (Self.Socket, strm);
         end;
      end if;

      if Self.State.Index > 1 then
         Self.State.Index := Self.State.Index - 1;
         Self.Request_Next_File;
      else
         Self.Stop;
      end if;
   end Play_Previous_File;

   ----------------
   -- Play_Radio --
   ----------------

   procedure Play_Radio
     (Self : in out Player'Class;
      URL  : League.Strings.Universal_String)
   is
      use type Ada.Calendar.Time;
      IRI : constant League.IRIs.IRI :=
        League.IRIs.From_Universal_String (URL);

      Host       : League.Strings.Universal_String := IRI.Get_Host;
      Port       : constant Natural := IRI.Get_Port;
      Port_Image : Wide_Wide_String := Integer'Wide_Wide_Image (Port);
      Addr       : GNAT.Sockets.Inet_Addr_Type;
      Strm    : Slim.Messages.strm.Strm_Message;
      Request : League.String_Vectors.Universal_String_Vector;
      Line    : League.Strings.Universal_String;
   begin
      Self.Stop;

      declare
         Host_Entry : constant GNAT.Sockets.Host_Entry_Type :=
           GNAT.Sockets.Get_Host_By_Name (Host.To_UTF_8_String);
      begin
         for J in 1 .. Host_Entry.Addresses_Length loop
            Addr := GNAT.Sockets.Addresses (Host_Entry, J);
            exit when Addr.Family in GNAT.Sockets.Family_Inet;
         end loop;
      end;

      if Addr.Family not in GNAT.Sockets.Family_Inet then
         return;
      end if;

      if Port /= 0 then
         Port_Image (1) := ':';
         Host.Append (Port_Image);
      end if;

      Line := +"GET /";
      Line.Append (IRI.Get_Path.Join ('/'));
      Line.Append (" HTTP/1.0");
      Ada.Wide_Wide_Text_IO.Put_Line (Line.To_Wide_Wide_String);
      Request.Append (Line);

      Line := +"Host: ";
      Line.Append (Host);
      Request.Append (Line);

      Request.Append (+"Icy-Metadata: 1");
      Request.Append (+"");
      Request.Append (+"");
      Strm.Start
        (Server      => (GNAT.Sockets.Family_Inet,
                         Addr,
                         Port => GNAT.Sockets.Port_Type (Port)),
         Request     => Request);

      Write_Message (Self.Socket, Strm);

      Self.State :=
        (Play_Radio,
         (Volume          => 30,
          Volume_Set_Time => Ada.Calendar.Clock - 60.0,
          Current_Song    => League.Strings.Empty_Universal_String,
          Paused          => False,
          Seconds         => 0));

   exception
      when GNAT.Sockets.Host_Error =>
         return;
   end Play_Radio;

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
            when Play_Radio =>
               return V : Play_Radio_Visiters.Visiter
                 (Self'Unchecked_Access);
            when Play_Files =>
               return V : Play_Files_Visiters.Visiter
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

   -----------------------
   -- Request_Next_File --
   -----------------------

   procedure Request_Next_File (Self : in out Player'Class) is
      Item    : constant Song := Self.State.Playlist (Self.State.Index);
      File    : constant League.Strings.Universal_String := Item.File;
      Strm    : Slim.Messages.strm.Strm_Message;
      Request : League.String_Vectors.Universal_String_Vector;
      Line    : League.Strings.Universal_String;
   begin
      Line.Append ("GET /Music/");
      Line.Append (File);
      Line.Append (" HTTP/1.0");
      Ada.Wide_Wide_Text_IO.Put_Line (Line.To_Wide_Wide_String);
      Request.Append (Line);
      Request.Append (+"");
      Request.Append (+"");

      Strm.Start
        (Server      => (GNAT.Sockets.Family_Inet,
                         GNAT.Sockets.Inet_Addr ("0.0.0.0"),
                         Port => 8080),
         Request     => Request);

      Write_Message (Self.Socket, Strm);

      Self.State.Play_State.Current_Song := Item.Title;
   end Request_Next_File;

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

   ----------
   -- Stop --
   ----------

   procedure Stop (Self : in out Player'Class) is
      use type Ada.Calendar.Time;

      Strm   : Slim.Messages.strm.Strm_Message;
   begin
      if Self.State.Kind in Play_Radio | Play_Files then
         Strm.Simple_Command (Slim.Messages.strm.Stop);
         Write_Message (Self.Socket, Strm);
         Self.State :=
           (Idle,
            Ada.Calendar.Clock - 60.0,
            Self.First_Menu);
         Self.Send_Hearbeat;  --  A reply to this will update display
      end if;
   end Stop;

   ------------
   -- Volume --
   ------------

   procedure Volume
     (Self  : in out Player'Class;
      Value : Natural)
   is
      Audg   : Slim.Messages.audg.Audg_Message;
   begin
      if Self.State.Kind in Play_Radio | Play_Files then
         Self.State.Play_State.Volume := Value;
         Self.State.Play_State.Volume_Set_Time := Ada.Calendar.Clock;
      end if;

      Audg.Set_Volume (Value);
      Write_Message (Self.Socket, Audg);
   end Volume;

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
