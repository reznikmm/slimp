--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams;
with League.Stream_Element_Vectors;
private with Interfaces;
limited with Slim.Message_Visiters;

package Slim.Messages is

   type Message is abstract tagged null record;
   type Message_Access is access all Message'Class;

   subtype Message_Tag is String (1 .. 4);

   not overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Message is abstract;

   not overriding procedure Write
     (Self : Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector)
        is abstract;

   not overriding procedure Visit
     (Self    : not null access Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is abstract;

private

   generic
      Data : in out League.Stream_Element_Vectors.Stream_Element_Vector;
   procedure Slice
     (Result : out Ada.Streams.Stream_Element_Array;
      From   : Ada.Streams.Stream_Element_Offset);

   type Field_Kinds is
     (Uint_8_Field, Uint_16_Field, Uint_32_Field, Uint_64_Field,
      Custom_Field);

   type Field_Description is record
      Kind  : Field_Kinds;
      Count : Positive := 1;
   end record;

   type Field_Description_Array is
     array (Positive range <>) of Field_Description;

   type Uint_8_Array is array (Positive range <>) of Interfaces.Unsigned_8;
   type Uint_16_Array is array (Positive range <>) of Interfaces.Unsigned_16;
   type Uint_32_Array is array (Positive range <>) of Interfaces.Unsigned_32;
   type Uint_64_Array is array (Positive range <>) of Interfaces.Unsigned_64;

   type Base_Message (Max_8, Max_16, Max_32, Max_64 : Natural) is abstract
     new Message with
   record
      Data_8  : Uint_8_Array  (1 .. Max_8);
      Data_16 : Uint_16_Array (1 .. Max_16);
      Data_32 : Uint_32_Array (1 .. Max_32);
      Data_64 : Uint_64_Array (1 .. Max_64);
   end record;

   not overriding procedure Read_Custom_Field
     (Self  : in out Base_Message;
      Index : Positive;
      Input : in out Ada.Streams.Stream_Element_Offset;
      Data  : League.Stream_Element_Vectors.Stream_Element_Vector) is null;

   not overriding procedure Write_Custom_Field
     (Self  : Base_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector)
        is null;

   procedure Read_Fields
     (Self : in out Base_Message'Class;
      List : Field_Description_Array;
      Data : League.Stream_Element_Vectors.Stream_Element_Vector);

   procedure Write_Fields
     (Self : Base_Message'Class;
      List : Field_Description_Array;
      Data : in out League.Stream_Element_Vectors.Stream_Element_Vector);

end Slim.Messages;
