--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.bdac is
   List : constant Field_Description_Array :=
     ((Uint_8_Field, 1),   --  Kind
      (Custom_Field, 1));  --  Data

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Bdac_Message is
   begin
      return Result : Bdac_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------------------
   -- Read_Custom_Field --
   -----------------------

   overriding procedure Read_Custom_Field
     (Self  : in out Bdac_Message;
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
     (Self  : in out Bdac_Message;
      Kind  : Natural;
      Value : Ada.Streams.Stream_Element_Array) is
   begin
      Self.Data.Clear;
      Self.Data.Append (Value);
      Self.Data_8 := (1 => Interfaces.Unsigned_8 (Kind));
   end Initialize;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Bdac_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.bdac (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Bdac_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "bdac";
      Write_Fields (Self, List, Data);
   end Write;

   ------------------------
   -- Write_Custom_Field --
   ------------------------

   overriding procedure Write_Custom_Field
     (Self  : Bdac_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector)
   is
   begin
      pragma Assert (Index = 1);
      Data.Append (Self.Data);
   end Write_Custom_Field;

end Slim.Messages.bdac;
