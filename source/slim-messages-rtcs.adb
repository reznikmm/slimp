--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.rtcs is

   List_0 : constant Field_Description_Array :=
     ((Uint_8_Field, 1),   --  Register
      (Uint_8_Field, 1));  --  Mode or Hours BCD
   List_1 : constant Field_Description_Array :=
     ((Uint_8_Field, 1),   --  Register #3
      (Uint_8_Field, 1),  --  Hours BCD
      (Uint_8_Field, 1),   --  Minutes BCS
      (Uint_8_Field, 1));  --  Seconds BCD

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Rtcs_Message is
   begin
      return Result : Rtcs_Message do
         Read_Fields (Result, List_0, Data.all);

         if Result.Data_8 (1) in 3 then  --  Set time
            Result.Data_8 (3) := Interfaces.Unsigned_8 (Data.Element (3));
            Result.Data_8 (4) := Interfaces.Unsigned_8 (Data.Element (4));
         end if;
      end return;
   end Read;

   ----------------
   -- Set_Format --
   ----------------

   not overriding procedure Set_Format (Self : in out Rtcs_Message) is
   begin
      Self.Data_8 := (0, 16#0E#, 0, 0);  --  24hours mode
   end Set_Format;

   --------------
   -- Set_Time --
   --------------

   not overriding procedure Set_Time
     (Self    : in out Rtcs_Message;
      Hours   : Natural;
      Minutes : Natural;
      Seconds : Natural)
   is
      function To_BCD (Value : Natural) return Interfaces.Unsigned_8;

      ------------
      -- To_BCD --
      ------------

      function To_BCD (Value : Natural) return Interfaces.Unsigned_8 is
         Upper : constant Natural := Value / 10;
         Lower : constant Natural := Value mod 10;
      begin
         return Interfaces.Unsigned_8 (Upper * 16 + Lower);
      end To_BCD;
   begin
      Self.Data_8 :=
        (3,  --  Set time
         To_BCD (Hours),
         To_BCD (Minutes),
         To_BCD (Seconds));
   end Set_Time;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Rtcs_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.rtcs (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Rtcs_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "rtcs";

      if Self.Data_8 (1) in 3 then  --  Set time
         Write_Fields (Self, List_1, Data);
      else
         Write_Fields (Self, List_0, Data);
      end if;
   end Write;

end Slim.Messages.rtcs;
