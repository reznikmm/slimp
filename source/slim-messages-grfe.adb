--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.grfe is
   List : constant Field_Description_Array :=
     ((Uint_16_Field, 1),   --  Offset
      (Uint_8_Field, 1),   --  Transition
      (Uint_8_Field, 1),   --  Param
      (Custom_Field, 1));  --  Data

   ----------
   -- Data --
   ----------

   not overriding function Data
     (Self : Grfe_Message) return Ada.Streams.Stream_Element_Array is
   begin
      return Self.Data.To_Stream_Element_Array;
   end Data;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Grfe_Message is
   begin
      return Result : Grfe_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------------------
   -- Read_Custom_Field --
   -----------------------

   overriding procedure Read_Custom_Field
     (Self  : in out Grfe_Message;
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

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self   : in out Grfe_Message;
      Offset : Natural;
      Value  : Ada.Streams.Stream_Element_Array) is
   begin
      Self.Data_16 (1) := Interfaces.Unsigned_16 (Offset);
      Self.Data_8 (1) := Character'Pos ('c');  --  Transition
      Self.Data_8 (2) := 0;  --  Param
      Self.Data.Clear;
      Self.Data.Append (Value);
   end Initialize;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Grfe_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.grfe (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Grfe_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "grfe";
      Write_Fields (Self, List, Data);
   end Write;

   ------------------------
   -- Write_Custom_Field --
   ------------------------

   overriding procedure Write_Custom_Field
     (Self  : Grfe_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector)
   is
   begin
      pragma Assert (Index = 1);
      Data.Append (Self.Data);
   end Write_Custom_Field;

end Slim.Messages.grfe;
