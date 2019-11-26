--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams;
with Ada.Unchecked_Deallocation;
with GNAT.Sockets;
with League.Stream_Element_Vectors;
with League.Strings;
with Slim.Message_Decoders;
with Slim.Message_Visiters;
with Slim.Messages.aude;
with Slim.Messages.audg;
with Slim.Messages.bdac;
with Slim.Messages.bled;
with Slim.Messages.grfb;
with Slim.Messages.grfe;
with Slim.Messages.HELO;
with Slim.Messages.rtcs;
with Slim.Messages.STAT;
with Slim.Messages.strm;
with Slim.Messages.vers;
with Slim.Messages.visu;

procedure Slim.Run is
   use GNAT.Sockets;

   procedure Read_Message
     (Socket  : Socket_Type;
      Message : out Slim.Messages.Message_Access);

   procedure Write_Message
     (Socket  : Socket_Type;
      Message : Slim.Messages.Message'Class);

   procedure Free is new Ada.Unchecked_Deallocation
     (Slim.Messages.Message'Class, Slim.Messages.Message_Access);

   type Player_State is
     (Init_Stop,
      Init_Adjust_Settings);

   type Player is record
      State  : Player_State := Init_Stop;
   end record;
--
   package Visiters is
      type Visiter (Player : access Run.Player) is
        new Slim.Message_Visiters.Visiter with
      record
         Socket : Socket_Type;
      end record;

      overriding procedure HELO
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.HELO.HELO_Message);

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

   --------------
   -- Visiters --
   --------------

   package body Visiters is

      overriding procedure HELO
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.HELO.HELO_Message)
      is
         pragma Unreferenced (Message);
         Vers : Slim.Messages.vers.Vers_Message;
      begin
         Vers.Set_Version (League.Strings.To_Universal_String ("7.7.3"));
         Write_Message (Self.Socket, Vers);
      end HELO;

      ----------
      -- STAT --
      ----------

      overriding procedure STAT
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.STAT.STAT_Message)
      is
         Event : constant Slim.Messages.STAT.Event_Kind := Message.Event;
      begin
         --  Got initial STAT or reply to server message
         if Event (1) in Character'Val (0) | 'a' .. 'z' then
            case Self.Player.State is
               when Init_Stop =>
                  declare
                     Strm : Slim.Messages.strm.Strm_Message;
                  begin
                     Strm.Initialize
                       (Command => Slim.Messages.strm.Stop,
                        Format  => Slim.Messages.strm.MP3,
                        Request => League.Strings.Empty_Universal_String);
                     Write_Message (Self.Socket, Strm);
                     Self.Player.State := Init_Adjust_Settings;
                  end;
               when Init_Adjust_Settings =>
                  declare
                     Grfb : Slim.Messages.grfb.Grfb_Message;
                     Grfe : Slim.Messages.grfe.Grfe_Message;
                     Visu : Slim.Messages.visu.Visu_Message;
                     Bled : Slim.Messages.bled.Bled_Message;
                     RTC1 : Slim.Messages.rtcs.Rtcs_Message;
                     RTC2 : Slim.Messages.rtcs.Rtcs_Message;
                     Bdac : Slim.Messages.bdac.Bdac_Message;
                     Aude : Slim.Messages.aude.Aude_Message;
                     Audg : Slim.Messages.audg.Audg_Message;
                  begin
                     --  Set dynamic brightness - minimum in range 1 .. 7
                     --  21 - coefficient in range (1 .. 20)
                     Grfb.Set_Brightness (16#0b02#);
                     Write_Message (Self.Socket, Grfb);
                     Grfe.Initialize
                       (0, (1 .. 8 => 255, 9 .. 320 => 1, 321 .. 640 => 0));
                     Write_Message (Self.Socket, Grfe);
                     Visu.Deactivate;
                     Write_Message (Self.Socket, Visu);
                     Bled.Enable_LED;
                     Write_Message (Self.Socket, Bled);
                     RTC1.Set_Format;
                     Write_Message (Self.Socket, RTC1);
                     RTC2.Set_Time (1, 2, 3);
                     Write_Message (Self.Socket, RTC2);
                     Bdac.Initialize
                       (6,
                        (16#09#, 16#00#, 16#00#, 16#02#, 16#92#, 16#00#,
                         16#00#, 16#03#, 16#d4#, 16#00#, 16#00#, 16#06#,
                         16#c1#, 16#00#, 16#00#, 16#0b#, 16#00#, 16#00#,
                         16#00#, 16#14#, 16#00#, 16#00#, 16#00#, 16#23#,
                         16#00#, 16#8f#, 16#ff#, 16#ff#, 16#ff#, 16#8f#,
                         16#ff#, 16#ff#, 16#ff#, 16#8f#, 16#ff#, 16#ff#,
                         16#ff#));
                     Write_Message (Self.Socket, Bdac);
                     Bdac.Initialize
                       (7,
                        (16#09#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#8f#,
                         16#ff#, 16#ff#, 16#ff#, 16#8f#, 16#ff#, 16#ff#,
                         16#ff#));
                     Write_Message (Self.Socket, Bdac);
                     Bdac.Initialize
                       (4, (16#05#, 16#37#, 16#00#, 16#00#, 16#17#, 16#2e#));
                     Write_Message (Self.Socket, Bdac);
                     Bdac.Initialize
                       (4,
                        (16#11#, 16#29#, 16#00#, 16#00#, 16#00#, 16#00#,
                         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
                         16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#));
                     Write_Message (Self.Socket, Bdac);
                     Bdac.Initialize
                       (8, (16#00#, 16#00#, 16#26#, 16#00#));
                     Write_Message (Self.Socket, Bdac);
                     Aude.Enable_Output;
                     Write_Message (Self.Socket, Aude);
                     Audg.Set_Volume (50);
                     Write_Message (Self.Socket, Audg);
                     Self.Player.State := Init_Adjust_Settings;
                  end;
            end case;
         end if;
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
      Socket : Socket_Type;
      Player : aliased Run.Player;
   begin
      accept Start (Value : Socket_Type) do
         Socket := Value;
      end Start;
      loop
         declare
            Message : Slim.Messages.Message_Access;
            Visiter : Visiters.Visiter := (Player'Access, Socket);
         begin
            Read_Message (Socket, Message);
            Message.Visit (Visiter);
            Free (Message);
         end;
      end loop;
   end Server;

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

   Address  : Sock_Addr_Type;
   Listener : Socket_Type;
   Client   : Socket_Type;
begin
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
