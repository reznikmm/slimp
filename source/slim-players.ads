--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar;
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

   procedure Initialize
     (Self   : in out Player'Class;
      Socket : GNAT.Sockets.Socket_Type;
      Font   : League.Strings.Universal_String;
      Splash : League.Strings.Universal_String;
      Menu   : League.Strings.Universal_String);

   procedure Play_Radio
     (Self : in out Player'Class;
      URL  : League.Strings.Universal_String);

   procedure Stop (Self : in out Player'Class);

   procedure Volume
     (Self  : in out Player'Class;
      Value : Natural);

   not overriding procedure Process_Message (Self : in out Player);

private

   type State_Kind is (Connected, Idle, Play);

   type Player_State (Kind : State_Kind := Connected) is record
      case Kind is
         when Connected =>
            null;
         when Idle =>
            Time       : Ada.Calendar.Time;  --  Time on players display
            Menu_View  : Slim.Menu_Views.Menu_View;
         when Play =>
            Volume          : Natural range 0 .. 100;
            Volume_Set_Time : Ada.Calendar.Time;
            Song            : League.Strings.Universal_String;
            Paused          : Boolean;
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

end Slim.Players;
