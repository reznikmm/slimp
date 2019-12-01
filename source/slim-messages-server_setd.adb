--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.Server_setd is

   List : constant Field_Description_Array :=
     (1 => (Uint_8_Field, 1));  --  Setting's code

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Setd_Message is
   begin
      return Result : Setd_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -------------------------
   -- Request_Player_Name --
   -------------------------

   not overriding procedure Request_Player_Name (Self : in out Setd_Message) is
   begin
      Self.Data_8 := (1 => 0);
   end Request_Player_Name;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Setd_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.setd (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Setd_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "setd";
      Write_Fields (Self, List, Data);
   end Write;

end Slim.Messages.Server_setd;
