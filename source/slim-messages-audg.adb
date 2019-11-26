--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;
with Ada.Numerics.Elementary_Functions;

package body Slim.Messages.audg is

   List : constant Field_Description_Array :=
     ((Uint_32_Field, 1),   --  old_left
      (Uint_32_Field, 1),   --  old_right
      (Uint_8_Field, 1),    --  Digital volume control 0/1
      (Uint_8_Field, 1),    --  Preamp (byte 255-0)
      (Uint_32_Field, 1),   --  new_left is 16.16 fixed point
      (Uint_32_Field, 1),   --  new_right is 16.16 fixed point
      (Uint_32_Field, 1));  --  sequence

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Audg_Message is
   begin
      return Result : Audg_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   ----------------
   -- Set_Volume --
   ----------------

   not overriding procedure Set_Volume
     (Self  : in out Audg_Message;
      Value : Volume)
   is
      use Ada.Numerics.Elementary_Functions;
      use type Interfaces.Unsigned_32;
      Old_Gain : constant Interfaces.Unsigned_32 :=
        Interfaces.Unsigned_32 (Value) * 128 / 100;
      dB : constant Float range -50.0 .. 0.0 :=
        50.0 / 101.0 * Float (Value - 100);
      Mult : constant Float := 10.0 ** (dB / 20.0);
      New_Gain : constant Interfaces.Unsigned_32 :=
        (if dB >= -30.0 then
            Interfaces.Unsigned_32 (Mult * 256.0) * 256
         else
            Interfaces.Unsigned_32 (Mult * 65536.0));
   begin
      Self.Data_32 := (Old_Gain, Old_Gain, New_Gain, New_Gain, 0);
      Self.Data_8 := (1, 255);
   end Set_Volume;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Audg_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.audg (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Audg_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "audg";
      Write_Fields (Self, List, Data);
   end Write;

end Slim.Messages.audg;
