--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.HELO is

   List : constant Field_Description_Array :=
     ((Uint_8_Field, 1),   --  Device kind
      (Uint_8_Field, 1),   --  Firmware Revision
      (Uint_8_Field, 6),   --  MAC address
      (Uint_8_Field, 16),  --  UID
      (Uint_16_Field, 1),  --  WLAN
      (Uint_64_Field, 1),  --  Received
      (Uint_8_Field, 2));  --  Lang

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return HELO_Message is
   begin
      return Result : HELO_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access HELO_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.HELO (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : HELO_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "HELO";
      Write_Fields (Self, List, Data);
   end Write;

end Slim.Messages.HELO;
