--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar;
with Ada.Containers.Vectors;

with GNAT.Sockets;

with League.Stream_Element_Vectors;
with League.Strings;

with Slim.Fonts;
with Slim.Messages;
with Slim.Menu_Views;

limited with Slim.Players.Displays;
with Slim.Menu_Models;

package Slim.Players is

   type Player is tagged limited private;

   type Player_Access is access all Player'Class;

   procedure Initialize
     (Self   : in out Player'Class;
      Socket : GNAT.Sockets.Socket_Type;
      Font   : League.Strings.Universal_String;
      Splash : League.Strings.Universal_String;
      Menu   : League.Strings.Universal_String);

   procedure Play_Radio
     (Self : in out Player'Class;
      URL  : League.Strings.Universal_String);

   type Song is record
      File  : League.Strings.Universal_String;
      Title : League.Strings.Universal_String;
   end record;

   type Song_Array is array (Positive range <>) of Song;

   procedure Play_Files
     (Self : in out Player'Class;
      Root : League.Strings.Universal_String;
      M3U  : League.Strings.Universal_String;
      List : Song_Array;
      From : Positive;
      Skip : Natural);
   --  Start playing given playlist. Skip given number of seconds of the first
   --  song

   procedure Play_Next_File
     (Self      : in out Player'Class;
      Immediate : Boolean := True);

   procedure Play_Previous_File
     (Self      : in out Player'Class;
      Immediate : Boolean := True);

   procedure Stop (Self : in out Player'Class);
   procedure Save_Position (Self : in out Player'Class);
   --  If play a playlist, then remember current position
   procedure Get_Position
     (Self  : in out Player'Class;
      M3U   : League.Strings.Universal_String;
      Index : out Positive;
      Skip  : out Natural);
   --  Find saved position for given playlist. Return (1,0) if not found

   procedure Volume
     (Self  : in out Player'Class;
      Value : Natural);

   not overriding procedure Process_Message (Self : in out Player);

private

   type State_Kind is (Connected, Idle, Play_Radio, Play_Files);

   type Play_State is record
      Volume          : Natural range 0 .. 100;
      Volume_Set_Time : Ada.Calendar.Time;
      Current_Song    : League.Strings.Universal_String;
      Seconds         : Natural;
      Paused          : Boolean;
   end record;

   package Song_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Song);

   type Player_State (Kind : State_Kind := Connected) is record
      case Kind is
         when Connected =>
            null;
         when Idle =>
            Time       : Ada.Calendar.Time;  --  Time on players display
            Menu_View  : Slim.Menu_Views.Menu_View;
         when Play_Radio | Play_Files =>
            Play_State : Slim.Players.Play_State;

            case Kind is
               when Play_Files =>
                  Root     : League.Strings.Universal_String;
                  M3U_Name : League.Strings.Universal_String;
                  Playlist : Song_Vectors.Vector;
                  Index    : Positive;
                  Offset   : Natural;
                  --  If current file started playing with Offset
               when others =>
                  null;
            end case;
      end case;
   end record;

   type Menu_Model_Access is access all
     Slim.Menu_Models.Menu_Model'Class;

   type Player is tagged limited record
      Socket   : GNAT.Sockets.Socket_Type := GNAT.Sockets.No_Socket;
      State    : Player_State;
      Ping     : Ada.Calendar.Time := Ada.Calendar.Clock;
      Font     : aliased Slim.Fonts.Font;
      Splash   : League.Stream_Element_Vectors.Stream_Element_Vector;
      Menu     : Menu_Model_Access;
      --  Splash screen
      WiFi     : Natural;  --  Wireless Signal Strength (0-100)
--      Elapsed  : Natural := 0;  --  elapsed seconds of the current stream
   end record;

   procedure Write_Message
     (Socket  : GNAT.Sockets.Socket_Type;
      Message : Slim.Messages.Message'Class);

   function Get_Display
     (Self   : Player;
      Height : Positive := 32;
      Width  : Positive := 160) return Slim.Players.Displays.Display;

   function First_Menu (Self : Player'Class) return Slim.Menu_Views.Menu_View;

   function "+" (X : Wide_Wide_String) return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   procedure Request_Next_File (Self : in out Player'Class);

end Slim.Players;
