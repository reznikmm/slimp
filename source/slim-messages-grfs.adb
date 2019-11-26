--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.grfs is

   List : constant Field_Description_Array :=
     ((Uint_8_Field, 1),   --  Screen
      (Uint_8_Field, 1),   --  Direction
      (Uint_32_Field, 1),   --  Pause
      (Uint_32_Field, 1),   --  Refresh
      (Uint_16_Field, 1),   --  Pixels
      (Uint_16_Field, 1),   --  Repeat
      (Uint_16_Field, 1),   --  Width
      (Uint_16_Field, 1),   --  Offset
      (Custom_Field, 1));  --  Data

   ----------
   -- Data --
   ----------

   not overriding function Data
     (Self : Grfs_Message) return Ada.Streams.Stream_Element_Array is
   begin
      return Self.Data.To_Stream_Element_Array;
   end Data;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Grfs_Message is
   begin
      return Result : Grfs_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------------------
   -- Read_Custom_Field --
   -----------------------

   overriding procedure Read_Custom_Field
     (Self  : in out Grfs_Message;
      Index : Positive;
      Input : in out Ada.Streams.Stream_Element_Offset;
      Data  : League.Stream_Element_Vectors.Stream_Element_Vector)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Content : constant Ada.Streams.Stream_Element_Array (1 .. Data.Length) :=
        Data.To_Stream_Element_Array;
   begin
      pragma Assert (Index = 1);

      Self.Data.Clear;
      Self.Data.Append (Content (Input .. Content'Last));
      Input := Content'Last + 1;
   end Read_Custom_Field;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Grfs_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.grfs (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Grfs_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "grfs";
      Write_Fields (Self, List, Data);
   end Write;

   ------------------------
   -- Write_Custom_Field --
   ------------------------

   overriding procedure Write_Custom_Field
     (Self  : Grfs_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector)
   is
   begin
      pragma Assert (Index = 1);
      Data.Append (Self.Data);
   end Write_Custom_Field;

end Slim.Messages.grfs;
