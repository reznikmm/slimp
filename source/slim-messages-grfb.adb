--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.grfb is

   List : constant Field_Description_Array :=
     (1 => (Uint_16_Field, 1));  --  Brightness

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Grfb_Message is
   begin
      return Result : Grfb_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   --------------------
   -- Set_Brightness --
   --------------------

   not overriding procedure Set_Brightness
     (Self  : in out Grfb_Message;
      Value : Natural) is
   begin
      Self.Data_16 (1) := Interfaces.Unsigned_16 (Value);
   end Set_Brightness;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Grfb_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.grfb (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Grfb_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "grfb";
      Write_Fields (Self, List, Data);
   end Write;

end Slim.Messages.grfb;
