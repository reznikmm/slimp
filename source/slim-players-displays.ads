--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams;

package Slim.Players.Displays is

   type Display (Size : Ada.Streams.Stream_Element_Offset) is private;

   procedure Clear (Self : in out Display);

   procedure Draw_Text
     (Self : in out Display;
      X, Y : Positive;
      Font : Slim.Fonts.Font;
      Text : League.Strings.Universal_String);

   procedure Draw_Pixel
     (Self : in out Display;
      X, Y : Positive;
      Set  : Boolean := True) with Inline;

   procedure Send_Message (Self   : Display);

   procedure Initialize
     (Self   : in out Display;
      Player : Players.Player);
   --  For internal purposes

private

   use type Ada.Streams.Stream_Element_Offset;

   type Display (Size : Ada.Streams.Stream_Element_Offset) is record
      Socket : GNAT.Sockets.Socket_Type;
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Size);
   end record;

end Slim.Players.Displays;
