--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO;
with Interfaces;
with System;

with League.IRIs;
with League.Settings;
with League.String_Vectors;
with League.Holders;

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

   function Seconds_To_Bytes
     (File : League.Strings.Universal_String;
      Skip : Positive) return League.Strings.Universal_String;

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

   ------------------
   -- Get_Position --
   ------------------

   procedure Get_Position
     (Self  : in out Player'Class;
      M3U   : League.Strings.Universal_String;
      Index : out Positive;
      Skip  : out Natural)
   is
      pragma Unreferenced (Self);
      Setting : League.Settings.Settings;
      Key     : League.Strings.Universal_String;
      Value   : League.Holders.Holder;
   begin
      Key.Append ("Pause/Index.");
      Key.Append (M3U);
      Value := Setting.Value (Key);

      if League.Holders.Is_Universal_String (Value) then
         Key := League.Holders.Element (Value);
         Index := Positive'Wide_Wide_Value (Key.To_Wide_Wide_String);
      else
         Index := 1;
      end if;

      Key.Clear;
      Key.Append ("Pause/Skip.");
      Key.Append (M3U);
      Value := Setting.Value (Key);

      if League.Holders.Is_Universal_String (Value) then
         Key := League.Holders.Element (Value);
         Skip := Natural'Wide_Wide_Value (Key.To_Wide_Wide_String);
      else
         Skip := 0;
      end if;
   end Get_Position;

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
      Root : League.Strings.Universal_String;
      M3U  : League.Strings.Universal_String;
      List : Song_Array;
      From : Positive;
      Skip : Natural)
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
          Current_Song    => List (From).Title,
          Paused          => False,
          Seconds         => 0),
         Root, M3U, Playlist, From, Skip);

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
         Self.State.Offset := 0;
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
      use type League.Strings.Universal_String;
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

      if Self.State.Offset > 0 then
         Line.Clear;
         Line.Append ("Range: ");
         Line.Append
           (Seconds_To_Bytes
              (Self.State.Root & File, Self.State.Offset));
         Request.Append (Line);

         Ada.Wide_Wide_Text_IO.Put_Line (Line.To_Wide_Wide_String);
      end if;

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
   -- Save_Position --
   -------------------

   procedure Save_Position (Self : in out Player'Class) is
      use type League.Strings.Universal_String;
      Setting : League.Settings.Settings;
      Value   : League.Strings.Universal_String;
      Skip    : constant Natural :=
        Self.State.Offset + Self.State.Play_State.Seconds;
   begin
      if Self.State.Kind /= Play_Files
        or else Self.State.M3U_Name.Is_Empty
      then
         return;
      end if;

      Value.Append (Integer'Wide_Wide_Image (Self.State.Index));
      Value := Value.Tail_From (2);

      Setting.Set_Value
        ("Pause/Index." & Self.State.M3U_Name,
         League.Holders.To_Holder (Value));

      Value.Clear;
      Value.Append (Integer'Wide_Wide_Image (Skip));
      Value := Value.Tail_From (2);

      Setting.Set_Value
        ("Pause/Skip." & Self.State.M3U_Name,
         League.Holders.To_Holder (Value));
   end Save_Position;

   ----------------------
   -- Seconds_To_Bytes --
   ----------------------

   function Seconds_To_Bytes
     (File : League.Strings.Universal_String;
      Skip : Positive) return League.Strings.Universal_String
   is
      procedure Read_ID3 (Input : in out Ada.Streams.Stream_IO.File_Type);
      --  Skip ID3 header if any

      procedure Read_MP3_Header
        (Input    : in out Ada.Streams.Stream_IO.File_Type;
         Bit_Rate : out Ada.Streams.Stream_Element_Count);

      function Image (Value : Ada.Streams.Stream_Element_Count)
        return Wide_Wide_String;

      -----------
      -- Image --
      -----------

      function Image (Value : Ada.Streams.Stream_Element_Count)
        return Wide_Wide_String
      is
         Img : constant Wide_Wide_String :=
           Ada.Streams.Stream_Element_Count'Wide_Wide_Image (Value);
      begin
         return Img (2 .. Img'Last);
      end Image;

      --------------
      -- Read_ID3 --
      --------------

      procedure Read_ID3 (Input : in out Ada.Streams.Stream_IO.File_Type) is
         use type Ada.Streams.Stream_Element;
         use type Ada.Streams.Stream_Element_Count;
         use type Ada.Streams.Stream_IO.Count;
         Index : constant Ada.Streams.Stream_IO.Positive_Count :=
           Ada.Streams.Stream_IO.Index (Input);
         Data : Ada.Streams.Stream_Element_Array (1 .. 10);
         Last : Ada.Streams.Stream_Element_Count;
         Skip : Ada.Streams.Stream_IO.Count := 0;
      begin
         Ada.Streams.Stream_IO.Read (Input, Data, Last);
         if Last = Data'Last
           and then Data (1) = Character'Pos ('I')
           and then Data (2) = Character'Pos ('D')
           and then Data (3) = Character'Pos ('3')
         then
            for J of Data (7 .. 10) loop
               Skip := Skip * 128 + Ada.Streams.Stream_IO.Count (J);
            end loop;

            Ada.Streams.Stream_IO.Set_Index
              (Input, Index + Data'Length + Skip);
         else
            Ada.Streams.Stream_IO.Set_Index (Input, Index);
         end if;
      end Read_ID3;

      ---------------------
      -- Read_MP3_Header --
      ---------------------

      procedure Read_MP3_Header
        (Input    : in out Ada.Streams.Stream_IO.File_Type;
         Bit_Rate : out Ada.Streams.Stream_Element_Count)
      is
         use type Interfaces.Unsigned_16;
         use type Ada.Streams.Stream_IO.Count;
         type MPEG_Version is (MPEG_2_5, Wrong, MPEG_2, MPEG_1);
         pragma Unreferenced (Wrong);
         for MPEG_Version use (0, 1, 2, 3);
         type MPEG_Layer is (Wrong, Layer_III, Layer_II, Layer_I);
         pragma Unreferenced (Wrong);
         for MPEG_Layer use (0, 1, 2, 3);
         type MPEG_Mode is (Stereo, Joint_Stereo, Dual_Channel, Mono);
         pragma Unreferenced (Stereo, Joint_Stereo, Dual_Channel, Mono);
         for MPEG_Mode use (0, 1, 2, 3);
         type MP3_Header is record
            Sync_Word  : Interfaces.Unsigned_16 range 0 .. 2 ** 11 - 1;
            Version    : MPEG_Version;
            Layer      : MPEG_Layer;
            Protection : Boolean;
            Bit_Rate   : Interfaces.Unsigned_8 range 0 .. 15;
            Frequency  : Interfaces.Unsigned_8 range 0 .. 3;
            Padding    : Boolean;
            Is_Private : Boolean;
            Mode       : MPEG_Mode;
            Extension  : Interfaces.Unsigned_8 range 0 .. 3;
            Copy       : Boolean;
            Original   : Boolean;
            Emphasis   : Interfaces.Unsigned_8 range 0 .. 3;
         end record;
         for MP3_Header'Object_Size use 32;
         for MP3_Header'Bit_Order use System.High_Order_First;
         for MP3_Header use record
            Sync_Word  at 0 range 0 .. 10;
            Version    at 0 range 11 .. 12;
            Layer      at 0 range 13 .. 14;
            Protection at 0 range 15 .. 15;
            Bit_Rate   at 0 range 16 .. 19;
            Frequency  at 0 range 20 .. 21;
            Padding    at 0 range 22 .. 22;
            Is_Private at 0 range 23 .. 23;
            Mode       at 0 range 24 .. 25;
            Extension  at 0 range 26 .. 27;
            Copy       at 0 range 28 .. 28;
            Original   at 0 range 29 .. 29;
            Emphasis   at 0 range 30 .. 31;
         end record;

         procedure Read_Header (Header : out MP3_Header);

         MPEG_1_Bit_Rate : constant array
           (MPEG_Layer range Layer_III .. Layer_I,
            Interfaces.Unsigned_8 range 1 .. 14) of
              Ada.Streams.Stream_Element_Count :=
             (Layer_I => (32_000, 64_000, 96_000, 128_000, 160_000, 192_000,
                          224_000, 256_000, 288_000, 320_000, 352_000, 384_000,
                          416_000, 448_000),
              Layer_II => (32_000, 48_000, 56_000, 64_000, 80_000, 96_000,
                           112_000, 128_000, 160_000, 192_000, 224_000,
                           256_000, 320_000, 384_000),
              Layer_III => (32_000, 40_000, 48_000, 56_000, 64_000, 80_000,
                            96_000, 112_000, 128_000, 160_000, 192_000,
                            224_000, 256_000, 320_000));

         MPEG_2_Bit_Rate : constant array
           (MPEG_Layer range Layer_II .. Layer_I,
            Interfaces.Unsigned_8 range 1 .. 14) of
              Ada.Streams.Stream_Element_Count :=
             (Layer_I => (32_000, 48_000, 56_000, 64_000, 80_000, 96_000,
                          112_000, 128_000, 144_000, 160_000, 176_000, 192_000,
                          224_000, 256_000),
              Layer_II => (8_000, 16_000, 24_000, 32_000, 40_000, 48_000,
                           56_000, 64_000, 80_000, 96_000, 112_000,
                           128_000, 144_000, 160_000));

         -----------------
         -- Read_Header --
         -----------------

         procedure Read_Header (Header : out MP3_Header) is
            use type System.Bit_Order;
            Stream : constant Ada.Streams.Stream_IO.Stream_Access :=
              Ada.Streams.Stream_IO.Stream (Input);
            Data   : Ada.Streams.Stream_Element_Array (1 .. 4)
              with Import, Address => Header'Address;
         begin
            if MP3_Header'Bit_Order = System.Default_Bit_Order then
               Ada.Streams.Stream_Element_Array'Read (Stream, Data);
            else
               for X of reverse Data loop
                  Ada.Streams.Stream_Element'Read (Stream, X);
               end loop;
            end if;
         end Read_Header;

         Header : MP3_Header := (Sync_Word => 0, others => <>);
      begin
         while not Ada.Streams.Stream_IO.End_Of_File (Input) loop
            Read_Header (Header);

            exit when Header.Sync_Word = 16#7FF#;

            Ada.Streams.Stream_IO.Set_Index
              (Input,
               Ada.Streams.Stream_IO.Index (Input) - 3);
         end loop;

         if Header.Sync_Word /= 16#7FF# then
            Bit_Rate := 0;
         elsif Header.Version = MPEG_1 and
           Header.Layer in MPEG_1_Bit_Rate'Range (1) and
           Header.Bit_Rate in MPEG_1_Bit_Rate'Range (2)
         then
            Bit_Rate := MPEG_1_Bit_Rate (Header.Layer, Header.Bit_Rate);
         elsif Header.Version in MPEG_2 .. MPEG_2_5 and
           Header.Layer in Layer_III .. Layer_I and
           Header.Bit_Rate in MPEG_2_Bit_Rate'Range (2)
         then
            Bit_Rate := MPEG_2_Bit_Rate
              (MPEG_Layer'Max (Header.Layer, Layer_II),
               Header.Bit_Rate);
         else
            Bit_Rate := 0;
         end if;
      end Read_MP3_Header;

      use type Ada.Streams.Stream_Element_Count;

      Input    : Ada.Streams.Stream_IO.File_Type;
      Bit_Rate : Ada.Streams.Stream_Element_Count;
      Offset   : Ada.Streams.Stream_Element_Count;
      Result   : League.Strings.Universal_String;
   begin
      Ada.Streams.Stream_IO.Open
        (Input, Ada.Streams.Stream_IO.In_File, File.To_UTF_8_String);
      Read_ID3 (Input);
      Read_MP3_Header (Input, Bit_Rate);
      Offset := Bit_Rate * Ada.Streams.Stream_Element_Count (Skip) / 8;
      Result.Append ("bytes=");
      Result.Append (Image (Offset));
      Result.Append ("-");
      Ada.Streams.Stream_IO.Close (Input);

      return Result;
   end Seconds_To_Bytes;

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
