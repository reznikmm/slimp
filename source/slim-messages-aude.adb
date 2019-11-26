--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.aude is

   List : constant Field_Description_Array :=
     ((Uint_8_Field, 1),   --  Register
      (Uint_8_Field, 1));  --  Mode or Hours BCD

   -------------------
   -- Enable_Output --
   -------------------

   procedure Enable_Output (Self : in out Aude_Message) is
   begin
      Self.Data_8 := (1, 1);
   end Enable_Output;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Aude_Message is
   begin
      return Result : Aude_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Aude_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.aude (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Aude_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "aude";
      Write_Fields (Self, List, Data);
   end Write;

end Slim.Messages.aude;
