--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.grfg is

   List : constant Field_Description_Array :=
     ((Uint_16_Field, 1),   --  Screen
      (Uint_16_Field, 1),   --  width of scrollable area
      (Custom_Field, 1));   --  Data

   ----------
   -- Data --
   ----------

   not overriding function Data
     (Self : Grfg_Message) return Ada.Streams.Stream_Element_Array is
   begin
      return Self.Data.To_Stream_Element_Array;
   end Data;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Grfg_Message is
   begin
      return Result : Grfg_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------------------
   -- Read_Custom_Field --
   -----------------------

   overriding procedure Read_Custom_Field
     (Self  : in out Grfg_Message;
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
     (Self    : not null access Grfg_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.grfg (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Grfg_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "grfg";
      Write_Fields (Self, List, Data);
   end Write;

   ------------------------
   -- Write_Custom_Field --
   ------------------------

   overriding procedure Write_Custom_Field
     (Self  : Grfg_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector)
   is
   begin
      pragma Assert (Index = 1);
      Data.Append (Self.Data);
   end Write_Custom_Field;

end Slim.Messages.grfg;
