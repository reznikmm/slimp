--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.audp is

   List : constant Field_Description_Array :=
     (1 => (Uint_8_Field, 1));  --  Line In on/off

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Audp_Message is
   begin
      return Result : Audp_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Audp_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.audp (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Audp_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "audp";
      Write_Fields (Self, List, Data);
   end Write;

end Slim.Messages.audp;
