--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.BUTN is

   List : constant Field_Description_Array :=
     ((Uint_32_Field, 1),   --  Time
      (Uint_8_Field, 4));  --  Button code

   ------------
   -- Button --
   ------------

   not overriding function Button (Self : BUTN_Message) return Button_Kind is
   begin
      if Self.Data_8 (1 .. 3) = (0, 1, 0) then
         case Self.Data_8 (4) is
            when 16#5B# =>
               return Knob_Right;
            when 16#5A# =>
               return Knob_Left;
            when 16#23# =>
               return Preset_1;
            when 16#17# =>
               return Pause;
            when 16#0E# =>
               return Knob_Push;
            when 16#0D# =>
               return Back;
            when others =>
               return Something_Else;
         end case;
      end if;

      return Something_Else;
   end Button;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access League.Stream_Element_Vectors
        .Stream_Element_Vector)
      return BUTN_Message
   is
   begin
      return Result : BUTN_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    :        not null access BUTN_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class)
   is
   begin
      Visiter.BUTN (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self :     BUTN_Message; Tag : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector)
   is
   begin
      Tag := "BUTN";
      Write_Fields (Self, List, Data);
   end Write;

end Slim.Messages.BUTN;
