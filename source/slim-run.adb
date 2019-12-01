--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO;

with GNAT.Sockets;
with League.Stream_Element_Vectors;
with League.Strings;
with League.String_Vectors;

with Slim.Fonts;
with Slim.Message_Decoders;
with Slim.Message_Visiters;
with Slim.Messages.aude;
with Slim.Messages.audg;
with Slim.Messages.bdac;
with Slim.Messages.bled;
with Slim.Messages.BUTN;
with Slim.Messages.cont;
with Slim.Messages.grfb;
with Slim.Messages.grfe;
with Slim.Messages.HELO;
with Slim.Messages.META;
with Slim.Messages.RESP;
with Slim.Messages.rtcs;
with Slim.Messages.Server_setd;
with Slim.Messages.SETD;
with Slim.Messages.STAT;
with Slim.Messages.strm;
with Slim.Messages.vers;
with Slim.Messages.visu;

procedure Slim.Run is
   use GNAT.Sockets;

   function "+" (X : Wide_Wide_String) return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   procedure Read_Message
     (Socket  : Socket_Type;
      Message : out Slim.Messages.Message_Access);

   procedure Write_Message
     (Socket  : Socket_Type;
      Message : Slim.Messages.Message'Class);

   procedure Free is new Ada.Unchecked_Deallocation
     (Slim.Messages.Message'Class, Slim.Messages.Message_Access);

   type Player_State is
     (Initialization,
      Player_Ready,
      Play);

   type Player is record
      Socket : Socket_Type;
      State  : Player_State := Initialization;
      Volume : Slim.Messages.audg.Volume := 50;
      Ping   : Ada.Calendar.Time := Ada.Calendar.Clock;
      Font   : Slim.Fonts.Font;
   end record;
--
   package Visiters is
      type Visiter (Player : access Run.Player) is
        new Slim.Message_Visiters.Visiter with null record;

      overriding procedure BUTN
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.BUTN.BUTN_Message);

      overriding procedure HELO
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.HELO.HELO_Message);

      overriding procedure META
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.META.META_Message);

      overriding procedure RESP
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.RESP.RESP_Message);

      overriding procedure SETD
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.SETD.SETD_Message);

      overriding procedure STAT
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.STAT.STAT_Message);

   end Visiters;

   task Discovery is
      entry Start;
   end Discovery;

   task type Server is
      entry Start (Value : Socket_Type);
   end Server;

   type Server_Access is access all Server;

   Splash : League.Stream_Element_Vectors.Stream_Element_Vector;
   --  Splash screen

   procedure Read_Splash
     (Splash : out League.Stream_Element_Vectors.Stream_Element_Vector);

   procedure Send_Hearbeat (Self : in out Player);

   --------------
   -- Visiters --
   --------------

   package body Visiters is

      ----------
      -- BUTN --
      ----------

      overriding procedure BUTN
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.BUTN.BUTN_Message)
      is
         Player : Run.Player renames Self.Player.all;
         Button : constant Slim.Messages.BUTN.Button_Kind := Message.Button;
      begin
         case Button is
            when Slim.Messages.BUTN.Knob_Left =>
               declare
                  Audg   : Slim.Messages.audg.Audg_Message;
               begin
                  Player.Volume := Natural'Max (0, Player.Volume - 5);
                  Audg.Set_Volume (Player.Volume);
                  Write_Message (Player.Socket, Audg);
               end;
            when Slim.Messages.BUTN.Knob_Right =>
               declare
                  Audg   : Slim.Messages.audg.Audg_Message;
               begin
                  Player.Volume := Natural'Min (100, Player.Volume + 5);
                  Audg.Set_Volume (Player.Volume);
                  Write_Message (Player.Socket, Audg);
               end;
            when Slim.Messages.BUTN.Preset_1 =>
               declare
                  Strm : Slim.Messages.strm.Strm_Message;
                  Request : League.String_Vectors.Universal_String_Vector;
               begin
                  Request.Append (+"GET /stream-128kmp3-YogaChill HTTP/1.0");
                  Request.Append (+"Host: 178.32.111.41:8027");
                  Request.Append (+"Icy-Metadata: 1");
                  Request.Append (+"");
                  Request.Append (+"");
                  Strm.Start
                    (Server_IP   => (178, 32, 111, 41),
                     Server_Port => 8027,
                     Request     => Request);
                  Write_Message (Player.Socket, Strm);
               end;
            when Slim.Messages.BUTN.Something_Else =>
               null;
         end case;
      end BUTN;

      ----------
      -- HELO --
      ----------

      overriding procedure HELO
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.HELO.HELO_Message)
      is
         pragma Unreferenced (Message);
         Player : Run.Player renames Self.Player.all;
         Vers : Slim.Messages.vers.Vers_Message;
      begin
         Vers.Set_Version (League.Strings.To_Universal_String ("7.7.3"));
         Write_Message (Player.Socket, Vers);
      end HELO;

      ----------
      -- META --
      ----------

      overriding procedure META
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.META.META_Message)
      is
         use type Ada.Streams.Stream_Element_Offset;

         procedure Draw_Pixel (X, Y : Positive);

         procedure Draw_Text is new Slim.Fonts.Draw_Text
           (Coordinate => Integer,
            Draw_Pixel => Draw_Pixel);

         Player : Run.Player renames Self.Player.all;
         Text   : League.Strings.Universal_String := Message.Value;
         Grfe   : Slim.Messages.grfe.Grfe_Message;
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 32 * 160 / 8) :=
           (others => 0);

         ----------------
         -- Draw_Pixel --
         ----------------

         procedure Draw_Pixel (X, Y : Positive) is
            use type Ada.Streams.Stream_Element;
            Index : constant Ada.Streams.Stream_Element_Offset :=
              Ada.Streams.Stream_Element_Offset ((X - 1) * 4 + (32 - Y) / 8);
            Mask : constant Ada.Streams.Stream_Element :=
              2 ** ((Y - 1) mod 8);
         begin
            Buffer (Index) := Buffer (Index) or Mask;
         end Draw_Pixel;

         Prefix : constant Wide_Wide_String := "StreamTitle='";
         Suffix : constant Wide_Wide_String := "';";
      begin
         if Text.Starts_With (Prefix) then
            Text := Text.Tail_From (Prefix'Length + 1);
         end if;

         if Text.Ends_With (Suffix) then
            Text := Text.Head_To (Text.Length - Suffix'Length);
         end if;

         while Fonts.Size (Player.Font, Text).Width > 160 loop
            Text := Text.Head_To (Text.Length - 1);
         end loop;

         Draw_Text (Player.Font, Text, 0, 5);
         Grfe.Initialize (0, Buffer);
         Write_Message (Player.Socket, Grfe);

         Ada.Wide_Wide_Text_IO.Put_Line (Message.Value.To_Wide_Wide_String);
      end META;

      ----------
      -- RESP --
      ----------

      overriding procedure RESP
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.RESP.RESP_Message)
      is
         Player : Run.Player renames Self.Player.all;
         List : constant League.String_Vectors.Universal_String_Vector :=
           Message.Headers;
         Line : League.Strings.Universal_String;
         Metaint_Header : constant Wide_Wide_String := "icy-metaint:";
         Metaint        : Natural := 0;
         Cont : Slim.Messages.cont.Cont_Message;
      begin
         for J in 1 .. List.Length loop
            Line := List.Element (J);

            if Line.Starts_With (Metaint_Header) then
               Line := Line.Tail_From (Metaint_Header'Length + 1);
               begin
                  Metaint :=
                    Natural'Wide_Wide_Value (Line.To_Wide_Wide_String);
               exception
                  when Constraint_Error =>
                     null;
               end;
               exit;
            end if;
         end loop;

         Cont.Set_Metaint (Metaint);
         Write_Message (Player.Socket, Cont);
      end RESP;

      ----------
      -- SETD --
      ----------

      overriding procedure SETD
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.SETD.SETD_Message)
      is
         pragma Unreferenced (Self);
         Value : constant Slim.Messages.SETD.Setting := Message.Get_Setting;
      begin
         case Value.Kind is
            when Slim.Messages.SETD.Player_Name =>
               Ada.Wide_Wide_Text_IO.Put_Line
                 ("Player:" & Value.Player.To_Wide_Wide_String);
            when others =>
               null;
         end case;
      end SETD;

      ----------
      -- STAT --
      ----------

      overriding procedure STAT
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.STAT.STAT_Message)
      is
         Player : Run.Player renames Self.Player.all;
         Event : constant Slim.Messages.STAT.Event_Kind := Message.Event;
      begin
         --  Got initial STAT or reply to server message
         case Player.State is
            when Initialization =>
               declare
                  Time : constant Natural := Natural
                    (Ada.Calendar.Seconds (Ada.Calendar.Clock));
                  Strm : Slim.Messages.strm.Strm_Message;
                  Grfb : Slim.Messages.grfb.Grfb_Message;
                  Grfe : Slim.Messages.grfe.Grfe_Message;
                  Visu : Slim.Messages.visu.Visu_Message;
                  Bled : Slim.Messages.bled.Bled_Message;
                  RTC1 : Slim.Messages.rtcs.Rtcs_Message;
                  RTC2 : Slim.Messages.rtcs.Rtcs_Message;
                  Bdac : Slim.Messages.bdac.Bdac_Message;
                  Aude : Slim.Messages.aude.Aude_Message;
                  Audg : Slim.Messages.audg.Audg_Message;
                  Setd : Slim.Messages.Server_setd.Setd_Message;
               begin
                  Strm.Simple_Command
                    (Command => Slim.Messages.strm.Stop);
                  Write_Message (Player.Socket, Strm);

                  --  Set dynamic brightness - minimum in range 1 .. 7
                  --  21 - coefficient in range (1 .. 20)
                  Grfb.Set_Brightness (16#0b02#);
                  Write_Message (Player.Socket, Grfb);

                  --  Send splash screen
                  Grfe.Initialize (0, Splash.To_Stream_Element_Array);
                  Write_Message (Player.Socket, Grfe);

                  --  deactivate visualizer
                  Visu.Deactivate;
                  Write_Message (Player.Socket, Visu);

                  --  Enable backlight leds
                  Bled.Enable_LED;
                  Write_Message (Player.Socket, Bled);
                  --  Set 24hours clock mode
                  RTC1.Set_Format;
                  Write_Message (Player.Socket, RTC1);
                  --  Set clock
                  RTC2.Set_Time
                    (Hours   => Time / 60 / 60,
                     Minutes => (Time / 60) mod 60,
                     Seconds => Time mod 60);
                  Write_Message (Player.Socket, RTC2);

                  --  Send a DAC settings to the client.
                  Bdac.Initialize
                    (6,
                     (16#09#, 16#00#, 16#00#, 16#02#, 16#92#, 16#00#,
                      16#00#, 16#03#, 16#d4#, 16#00#, 16#00#, 16#06#,
                      16#c1#, 16#00#, 16#00#, 16#0b#, 16#00#, 16#00#,
                      16#00#, 16#14#, 16#00#, 16#00#, 16#00#, 16#23#,
                      16#00#, 16#8f#, 16#ff#, 16#ff#, 16#ff#, 16#8f#,
                      16#ff#, 16#ff#, 16#ff#, 16#8f#, 16#ff#, 16#ff#,
                      16#ff#));
                  Write_Message (Player.Socket, Bdac);
                  --  Send a DAC settings to the client. Part 2
                  Bdac.Initialize
                    (7,
                     (16#09#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#8f#,
                      16#ff#, 16#ff#, 16#ff#, 16#8f#, 16#ff#, 16#ff#,
                      16#ff#));
                  Write_Message (Player.Socket, Bdac);
                  --  Send a DAC settings to the client. Part 3
                  Bdac.Initialize
                    (4, (16#05#, 16#37#, 16#00#, 16#00#, 16#17#, 16#2e#));
                  Write_Message (Player.Socket, Bdac);
                  --  Send a DAC settings to the client. Part 4
                  Bdac.Initialize
                    (4,
                     (16#11#, 16#29#, 16#00#, 16#00#, 16#00#, 16#00#,
                      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                      16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#));
                  Write_Message (Player.Socket, Bdac);
                  --  Send a DAC settings to the client. Part 5
                  Bdac.Initialize
                    (8, (16#00#, 16#00#, 16#26#, 16#00#));
                  Write_Message (Player.Socket, Bdac);

                  --  Enable the audio output.
                  Aude.Enable_Output;
                  Write_Message (Player.Socket, Aude);
                  --  Adjust the volume level
                  Audg.Set_Volume (50);
                  Write_Message (Player.Socket, Audg);

                  --  Ask player name
                  Setd.Request_Player_Name;
                  Write_Message (Player.Socket, Setd);
                  Player.State := Player_Ready;
               end;

            when Player_Ready =>
               if Event (1 .. 3) = "STM" then
                  case Event (4) is
                     when 't' =>
                        Ada.Wide_Wide_Text_IO.Put_Line ("Status");
                     when others =>
                        null;
                  end case;
               end if;
            when Play =>
               null;
         end case;
      end STAT;

   end Visiters;

   ---------------
   -- Discovery --
   ---------------

   task body Discovery is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
      Address  : Sock_Addr_Type;
      Listener : Socket_Type;
      From     : Sock_Addr_Type;
      Last     : Ada.Streams.Stream_Element_Offset;
      Request  : Ada.Streams.Stream_Element_Array (1 .. 128);
   begin
      accept Start;
      Address.Addr := Any_Inet_Addr;
      Address.Port := 3483;
      Create_Socket (Listener, Mode => Socket_Datagram);
      Bind_Socket (Listener, Address);

      loop
         Receive_Socket (Listener, Request, Last, From);

         if Last = 18 and then Request (1) = Character'Pos ('d') then
            --  Discovery request
            declare
               Reply : constant Ada.Streams.Stream_Element_Array (1 .. 18) :=
                 (Character'Pos ('D'),
                  Character'Pos ('O'),
                  Character'Pos ('p'),
                  Character'Pos ('e'),
                  Character'Pos ('n'),
                  Character'Pos ('W'),
                  Character'Pos ('R'),
                  Character'Pos ('T'),
                  others => 0);
            begin
               Send_Socket (Listener, Reply, Last, To => From);
            end;
         end if;
      end loop;
   end Discovery;

   ------------------
   -- Read_Message --
   ------------------

   procedure Read_Message
     (Socket  : Socket_Type;
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
      Receive_Socket (Socket, Raw_Tag, Last);

      if Last = 0 then
         --  Timeout
         return;
      end if;

      pragma Assert (Last = Raw_Tag'Length);
      Receive_Socket (Socket, Word, Last);
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
            Receive_Socket (Socket, Input, Last);
            pragma Assert (Last = Input'Length);
            Data.Append (Input);
            Length := Length - Last;
         end;
      end loop;

      Decoder.Decode (Tag, Data'Unchecked_Access, Message);
   end Read_Message;

   ------------
   -- Server --
   ------------

   task body Server is
      use type Ada.Calendar.Time;

      Player : aliased Run.Player;
   begin
      accept Start (Value : Socket_Type) do
         Player.Socket := Value;
      end Start;

      Fonts.Read (Player.Font, +"10x20-ISO8859-5");

      GNAT.Sockets.Set_Socket_Option
        (Player.Socket,
         GNAT.Sockets.Socket_Level,
         (GNAT.Sockets.Receive_Timeout, 5.0));

      loop
         if Player.State /= Initialization
           and then Ada.Calendar.Clock - Player.Ping > 5.0
         then
            Send_Hearbeat (Player);
         end if;

         declare
            Message : Slim.Messages.Message_Access;
            Visiter : Visiters.Visiter (Player'Access);
         begin
            Read_Message (Player.Socket, Message);
            Message.Visit (Visiter);
            Free (Message);
         exception
            when E : GNAT.Sockets.Socket_Error =>
               case GNAT.Sockets.Resolve_Exception (E) is
                  when GNAT.Sockets.Resource_Temporarily_Unavailable =>
                     Send_Hearbeat (Player);
                  when others =>
                     raise;
               end case;
         end;
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Name (E) & ": "
            & Ada.Exceptions.Exception_Message (E));
   end Server;

   -------------------
   -- Send_Hearbeat --
   -------------------

   procedure Send_Hearbeat (Self : in out Player) is
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
     (Socket  : Socket_Type;
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

      Send_Socket
        (Socket, Word & Raw_Tag & Data.To_Stream_Element_Array, Last);
      pragma Assert (Last = Length + Word'Length);
   end Write_Message;

   -----------------
   -- Read_Splash --
   -----------------

   procedure Read_Splash
     (Splash : out League.Stream_Element_Vectors.Stream_Element_Vector)
   is
      Size : constant := 32 * 160 / 8;
      Data : Ada.Streams.Stream_Element_Array (1 .. Size);
      Last : Ada.Streams.Stream_Element_Offset;
      Input : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (Input,
         Mode => Ada.Streams.Stream_IO.In_File,
         Name => "data/splash.dat");
      Ada.Streams.Stream_IO.Read (Input, Data, Last);
      pragma Assert (Last in Data'Last);
      Ada.Streams.Stream_IO.Close (Input);
      Splash.Clear;
      Splash.Append (Data);
   end Read_Splash;

   Address  : Sock_Addr_Type;
   Listener : Socket_Type;
   Client   : Socket_Type;
begin
   Read_Splash (Splash);
   Address.Addr := Any_Inet_Addr;
   Address.Port := 3483;
   Create_Socket (Listener);
   Bind_Socket (Listener, Address);
   Listen_Socket (Listener);

   Discovery.Start;

   loop
      declare
         Next : Server_Access;
      begin
         Accept_Socket (Listener, Client, Address);
         Next := new Server;
         Next.Start (Client);
      end;
   end loop;
--     Close_Socket (Client);
--     Close_Socket (Listener);
end Slim.Run;
