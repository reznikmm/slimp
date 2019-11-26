--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.bled is

   List : constant Field_Description_Array :=
     (1 => (Uint_16_Field, 1));  --  LEDs on

   ----------------
   -- Enable_LED --
   ----------------

   not overriding procedure Enable_LED (Self : in out Bled_Message) is
   begin
      Self.Data_16 := (1 => 16#FFFF#);
   end Enable_LED;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Bled_Message is
   begin
      return Result : Bled_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Bled_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.bled (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Bled_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "bled";
      Write_Fields (Self, List, Data);
   end Write;

end Slim.Messages.bled;
