--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams;
with Ada.Wide_Wide_Text_IO;
with Interfaces;

with League.Regexps;
with League.String_Vectors;
with League.Text_Codecs;

package body Slim.Fonts is

   Unknown_Char_Width : constant := 2;

   ---------------
   -- Draw_Text --
   ---------------

   procedure Draw_Text
     (Self     : Font;
      Text     : League.Strings.Universal_String;
      Offset_X : Coordinate;
      Offset_Y : Coordinate)
   is
      procedure Draw_Char
        (Char : not null Glyph_Bitmap_Access;
         X    : in out Coordinate;
         Y    : Coordinate);

      ---------------
      -- Draw_Char --
      ---------------

      procedure Draw_Char
        (Char : not null Glyph_Bitmap_Access;
         X    : in out Coordinate;
         Y    : Coordinate)
      is
         Row : Coordinate := Y + Coordinate (Char.Height + Char.Offset_Y);
         Col : constant Coordinate := X + Coordinate (Char.Offset_X);
      begin
         for J in 1 .. Char.Height loop
            for K in 1 .. Char.Width loop
               if Char.V (J, K) then
                  Draw_Pixel (X => Col + Coordinate (K - 1), Y => Row);
               end if;
            end loop;

            Row := Row - 1;
         end loop;
         X := X + Coordinate (Char.Dev_Width);
      end Draw_Char;

      X    : Coordinate := Offset_X;
      Y    : constant Coordinate := Offset_Y;
   begin
      for J in 1 .. Text.Length loop
         declare
            Cursor : constant Glyph_Bitmap_Maps.Cursor :=
              Self.Map.Find (Text.Element (J).To_Wide_Wide_Character);
         begin
            if Glyph_Bitmap_Maps.Has_Element (Cursor) then
               Draw_Char (Glyph_Bitmap_Maps.Element (Cursor), X, Y);
            else
               X := X + Unknown_Char_Width;
            end if;
         end;
      end loop;
   end Draw_Text;

   ----------
   -- Hash --
   ----------

   function Hash
     (Value : Wide_Wide_Character) return Ada.Containers.Hash_Type is
   begin
      return Wide_Wide_Character'Pos (Value);
   end Hash;

   ----------
   -- Read --
   ----------

   procedure Read
     (Self : in out Font;
      Name : League.Strings.Universal_String)
   is
      use type League.Strings.Universal_String;

      function "+"
        (Text : Wide_Wide_String) return League.Strings.Universal_String
           renames League.Strings.To_Universal_String;

      function Get_Line
        (Input : Ada.Wide_Wide_Text_IO.File_Type)
         return League.Strings.Universal_String;

      function Skip (Value : Bit_Count) return Natural;

      function Hex
        (Value : League.Strings.Universal_String)
           return Interfaces.Unsigned_64;

      function Split (Text : League.Strings.Universal_String)
        return League.String_Vectors.Universal_String_Vector;
      --  Like Text.Split but return Vector.Length >=1

      --------------
      -- Get_Line --
      --------------

      function Get_Line
        (Input : Ada.Wide_Wide_Text_IO.File_Type)
         return League.Strings.Universal_String is
      begin
         return League.Strings.To_Universal_String
           (Ada.Wide_Wide_Text_IO.Get_Line (Input));
      end Get_Line;

      ---------
      -- Hex --
      ---------

      function Hex
        (Value : League.Strings.Universal_String)
           return Interfaces.Unsigned_64 is
      begin
         return Interfaces.Unsigned_64'Wide_Wide_Value
           ("16#" & Value.To_Wide_Wide_String & "#");
      end Hex;

      ----------
      -- Skip --
      ----------

      function Skip (Value : Bit_Count) return Natural is
      begin
         return 64 - Natural ((Value - 1) / 8 + 1) * 8;
      end Skip;

      -----------
      -- Split --
      -----------

      function Split (Text : League.Strings.Universal_String)
        return League.String_Vectors.Universal_String_Vector is
         Result : League.String_Vectors.Universal_String_Vector :=
           Text.Split (' ');
      begin
         if Result.Is_Empty then
            Result.Append (League.Strings.Empty_Universal_String);
         end if;

         return Result;
      end Split;

      Input : Ada.Wide_Wide_Text_IO.File_Type;
      Char  : Wide_Wide_Character;
      Last  : Glyph_Bitmap_Access;

      Dev_Width : Bit_Count;
      Code_Page : League.Strings.Universal_String;

      Codec     : League.Text_Codecs.Text_Codec :=
        League.Text_Codecs.Codec (+"utf-8");

      Pattern : constant League.Regexps.Regexp_Pattern :=
        League.Regexps.Compile (+"uni([0-9a-fA-F]{4})");

      STARTCHAR : constant League.Strings.Universal_String := +"STARTCHAR";
      ENCODING : constant League.Strings.Universal_String := +"ENCODING";
--      SWIDTH : League.Strings.Universal_String := +"SWIDTH";
      DWIDTH : constant League.Strings.Universal_String := +"DWIDTH";
      BBX : constant League.Strings.Universal_String := +"BBX";
      BITMAP : constant League.Strings.Universal_String := +"BITMAP";
      ENDCHAR : constant League.Strings.Universal_String := +"ENDCHAR";

      CHARSET_REGISTRY : constant League.Strings.Universal_String :=
        +"CHARSET_REGISTRY";

      CHARSET_ENCODING : constant League.Strings.Universal_String :=
        +"CHARSET_ENCODING";

   begin
      Ada.Wide_Wide_Text_IO.Open
        (Input,
         Ada.Wide_Wide_Text_IO.In_File,
         Name => "data/" & Name.To_UTF_8_String & ".bdf");

      --  Skip headers
      loop
         declare
            Line : constant League.Strings.Universal_String :=
              Get_Line (Input);
         begin
            if Line.Starts_With (CHARSET_REGISTRY) then
               Code_Page := Line.Split ('"') (2);
            elsif Line.Starts_With (CHARSET_ENCODING) then
               Code_Page.Append (Line.Split ('"') (2));
            end if;

            exit when Line.Starts_With ("CHARS ");
         end;
      end loop;

      if not Code_Page.Is_Empty then
         Codec := League.Text_Codecs.Codec (Code_Page);
      end if;

      --  Read Glyphs
      while not Ada.Wide_Wide_Text_IO.End_Of_File (Input) loop
         declare
            Line : constant League.Strings.Universal_String :=
              Get_Line (Input);
            Fields : constant League.String_Vectors.Universal_String_Vector :=
              Split (Line);
            First  : constant League.Strings.Universal_String :=
              Fields.Element (1);
            W, H   : Bit_Count;
            Row    : Bit_Count;
         begin
            if First = STARTCHAR then
               declare
                  Match : constant League.Regexps.Regexp_Match :=
                    Pattern.Find_Match (Fields.Element (2));
               begin
                  if Match.Is_Matched then
                     Char := Wide_Wide_Character'Val
                       (Hex (Match.Capture (1)));
                  else
                     Char := ' ';  --  To be defined in ENCODING
                  end if;
               end;

            elsif First = ENCODING then
               if Char = ' ' then
                  declare
                     X : constant Ada.Streams.Stream_Element_Array (1 .. 1) :=
                       (1 => Ada.Streams.Stream_Element'Wide_Wide_Value
                          (Fields.Element (2).To_Wide_Wide_String));
                  begin
                     Char := Codec.Decode (X)
                       .Element (1).To_Wide_Wide_Character;
                  end;
               end if;

            elsif First = BBX then
               W := Bit_Count'Wide_Wide_Value
                 (Fields.Element (2).To_Wide_Wide_String);
               H := Bit_Count'Wide_Wide_Value
                 (Fields.Element (3).To_Wide_Wide_String);
               Last := new Glyph_Bitmap (Width => W, Height => H);
               Last.Offset_X := Bit_Count'Wide_Wide_Value
                 (Fields.Element (4).To_Wide_Wide_String);
               Last.Offset_Y := Bit_Count'Wide_Wide_Value
                 (Fields.Element (5).To_Wide_Wide_String);
               Last.V := (1 .. H => (1 .. W => False));
               Self.Map.Insert (Char, Last);

            elsif First = DWIDTH then
               Dev_Width := Bit_Count'Wide_Wide_Value
                 (Fields.Element (2).To_Wide_Wide_String);

            elsif First = BITMAP then
               Row := 1;
               Last.Dev_Width := Dev_Width;

               loop
                  declare
                     use type Interfaces.Unsigned_64;

                     Bits : constant League.Strings.Universal_String :=
                       Get_Line (Input);

                     Value : Interfaces.Unsigned_64;

                     Column    : Bit_Count := 1;
                     Upper_Bit : constant := 16#8000_0000_0000_0000#;
                  begin
                     exit when Bits = ENDCHAR;

                     Value := Interfaces.Shift_Left
                       (Hex (Bits), Skip (Last.Width));

                     while Value /= 0 loop
                        if (Value and Upper_Bit) /= 0 then
                           Last.V (Row, Column) := True;
                        end if;


                        Column := Column + 1;
                        Value := Interfaces.Shift_Left (Value, 1);
                     end loop;
                  end;

                  Row := Row + 1;
               end loop;
            end if;
         end;
      end loop;

      Ada.Wide_Wide_Text_IO.Close (Input);
   end Read;

   ----------
   -- Size --
   ----------

   function Size
     (Self : Font; Text : League.Strings.Universal_String) return Bounding_Box
   is
      Result : Bounding_Box := (others => 0);
      Char   : Wide_Wide_Character;
      Cursor : Glyph_Bitmap_Maps.Cursor;
      Glyph  : Glyph_Bitmap_Access;
      Width  : Natural := 0;
      First  : Boolean := True;
   begin
      if Text.Length = 0 then
         return Result;
      end if;

      for J in 1 .. Text.Length loop
         Char := Text.Element (J).To_Wide_Wide_Character;
         Cursor := Self.Map.Find (Char);

         if Glyph_Bitmap_Maps.Has_Element (Cursor) then
            Glyph := Glyph_Bitmap_Maps.Element (Cursor);

            if First then
               Result.Top := Integer (Glyph.Offset_Y + Glyph.Height);
               Result.Bottom := Integer (Glyph.Offset_Y + 1);
               Result.Left := Width + Integer (Glyph.Offset_X + 1);
               Result.Right := Width + Integer (Glyph.Offset_X + Glyph.Width);
               First := False;
            else
               Result.Top := Integer'Max
                 (Result.Top,
                  Integer (Glyph.Offset_Y + Glyph.Height));
               Result.Bottom := Integer'Min
                 (Result.Bottom,
                  Integer (Glyph.Offset_Y + 1));
               Result.Right := Width + Integer (Glyph.Offset_X + Glyph.Width);
            end if;

            Width := Width + Natural (Glyph.Dev_Width);
         else
            Width := Width + Unknown_Char_Width;
         end if;
      end loop;

      return Result;
   end Size;

end Slim.Fonts;
