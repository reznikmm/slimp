--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Text_Codecs;

with Slim.Message_Visiters;

package body Slim.Messages.META is

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access League.Stream_Element_Vectors
        .Stream_Element_Vector) return META_Message
   is
      Value : constant Ada.Streams.Stream_Element_Array :=
        Data.To_Stream_Element_Array;
      Last : Ada.Streams.Stream_Element_Offset := 0;
   begin
      for J in reverse Value'Range loop
         if Value (J) not in 0 then
            Last := J;
            exit;
         end if;
      end loop;

      return Result : META_Message do
         Result.Value := League.Text_Codecs.Codec_For_Application_Locale.Decode
           (Value (Value'First .. Last));
      end return;
   end Read;

   -----------
   -- Value --
   -----------

   not overriding function Value (Self : META_Message)
     return League.Strings.Universal_String is
   begin
      return Self.Value;
   end Value;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    :        not null access META_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.META (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self :     META_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "META";
      Data := League.Text_Codecs.Codec_For_Application_Locale.Encode
        (Self.Value);
   end Write;

end Slim.Messages.META;
