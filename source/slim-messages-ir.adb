--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.IR is

   List : constant Field_Description_Array :=
     ((Uint_32_Field, 1),   --  Time since player startup in ticks (@1kHz)
      (Uint_8_Field, 1),   --  Code Format
      (Uint_8_Field, 1),   --  Length of IR Code
      (Uint_32_Field, 1));  --  the IR Code itself (upto 32 bits)

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return IR_Message is
   begin
      return Result : IR_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access IR_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.IR (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : IR_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "IR  ";
      Write_Fields (Self, List, Data);
   end Write;

end Slim.Messages.IR;
