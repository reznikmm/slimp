--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.visu is
   List : constant Field_Description_Array :=
     ((Uint_8_Field, 1),   --  Which visualizer to use
      (Uint_8_Field, 1));  --  How many parameters there are

   ----------------
   -- Deactivate --
   ----------------

   not overriding procedure Deactivate (Self : in out Visu_Message) is
   begin
      Self.Data_8 := (0, 0);
   end Deactivate;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Visu_Message is
   begin
      return Result : Visu_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Visu_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.visu (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Visu_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "visu";
      Write_Fields (Self, List, Data);
   end Write;

end Slim.Messages.visu;
