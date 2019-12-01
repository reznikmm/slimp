--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Hashed_Maps;

with League.Strings;

package Slim.Fonts is

   type Font is limited private;

   procedure Read
     (Self : in out Font;
      Name : League.Strings.Universal_String);

   type Bounding_Box is record
      Width  : Natural;
      Height : Natural;
   end record;

   function Size
     (Self : Font;
      Text : League.Strings.Universal_String) return Bounding_Box;

   generic
      type Coordinate is range <>;
      with procedure Draw_Pixel (X, Y : Coordinate);
   procedure Draw_Text
     (Self : Font;
      Text : League.Strings.Universal_String;
      Offset_X : Coordinate;
      Offset_Y : Coordinate);

private

   type Bit_Count is range 0 .. 64;
   type Bit_Matrix is array (Bit_Count range <>, Bit_Count range <>) of Boolean
     with Pack;

   type Glyph_Bitmap (Width, Height : Bit_Count) is record
      Offset_X  : Bit_Count'Base;
      Offset_Y  : Bit_Count'Base;
      Dev_Width : Bit_Count;  --  Device Width of a glyph
      V         : Bit_Matrix (1 .. Height, 1 .. Width);
   end record;

   type Glyph_Bitmap_Access is access all Glyph_Bitmap;

   function Hash
     (Value : Wide_Wide_Character) return Ada.Containers.Hash_Type;

   package Glyph_Bitmap_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Wide_Wide_Character,
      Element_Type    => Glyph_Bitmap_Access,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => "=");

   type Font is record
      Encoding : League.Strings.Universal_String;
      Map      : Glyph_Bitmap_Maps.Map;
   end record;
end Slim.Fonts;
