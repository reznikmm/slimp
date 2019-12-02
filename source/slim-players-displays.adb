--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Messages.grfe;

package body Slim.Players.Displays is

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : in out Display) is
   begin
      Self.Buffer := (1 .. Self.Size => 0);
   end Clear;

   ---------------
   -- Draw_Text --
   ---------------

   procedure Draw_Text
     (Self : in out Display;
      X, Y : Positive;
      Font : Slim.Fonts.Font;
      Text : League.Strings.Universal_String)
   is
      procedure Draw_Pixel (X, Y : Positive);

      procedure Draw_Pixel (X, Y : Positive) is
      begin
         Draw_Pixel (Self, X, Y);
      end Draw_Pixel;

      procedure Draw_Text is new Slim.Fonts.Draw_Text
        (Coordinate => Integer,
         Draw_Pixel => Draw_Pixel);

      Line : League.Strings.Universal_String := Text;
   begin
      while Fonts.Size (Font, Line).Width > 160 loop
         Line := Line.Head_To (Line.Length - 1);
      end loop;

      Draw_Text (Font, Line, X - 1, Y - 1);
   end Draw_Text;

   ----------------
   -- Draw_Pixel --
   ----------------

   procedure Draw_Pixel
     (Self : in out Display;
      X, Y : Positive;
      Set  : Boolean := True)
   is
      use type Ada.Streams.Stream_Element;

      Index : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Offset ((X - 1) * 4 + (32 - Y) / 8);
      Mask : constant Ada.Streams.Stream_Element :=
        2 ** ((Y - 1) mod 8);
   begin
      if Set then
         Self.Buffer (Index) := Self.Buffer (Index) or Mask;
      else
         Self.Buffer (Index) := Self.Buffer (Index) and not Mask;
      end if;
   end Draw_Pixel;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Display;
      Player : Players.Player) is
   begin
      Self.Socket := Player.Socket;
   end Initialize;

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message (Self   : Display) is
      Grfe : Slim.Messages.grfe.Grfe_Message;
   begin
      Grfe.Initialize (0, Self.Buffer);
      Write_Message (Self.Socket, Grfe);
   end Send_Message;

end Slim.Players.Displays;
