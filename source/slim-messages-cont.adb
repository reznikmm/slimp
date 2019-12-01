--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.cont is

   List : constant Field_Description_Array :=
     ((Uint_32_Field, 1),   --  Metaint
      (Uint_8_Field, 1),    --  Loop
      (Uint_16_Field, 1));  --  Guids count

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access League.Stream_Element_Vectors
        .Stream_Element_Vector)
      return Cont_Message is
   begin
      return Result : Cont_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------------
   -- Set_Metaint --
   -----------------

   not overriding procedure Set_Metaint
     (Self : in out Cont_Message; Value : Natural)
   is
   begin
      Self.Data_32 := (1 => Interfaces.Unsigned_32 (Value));
      Self.Data_16 := (1 => 0);  --  No guids
      Self.Data_8 := (1 => 0);  --  No loop
   end Set_Metaint;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    :        not null access Cont_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.cont (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self :     Cont_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector)
   is
   begin
      Tag := "cont";
      Write_Fields (Self, List, Data);
   end Write;

end Slim.Messages.cont;
