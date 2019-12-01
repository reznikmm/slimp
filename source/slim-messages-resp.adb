--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;
with Ada.Wide_Wide_Text_IO;

with League.Text_Codecs;

with Slim.Message_Visiters;

package body Slim.Messages.RESP is

   -------------
   -- Headers --
   -------------

   not overriding function Headers (Self : RESP_Message)
     return League.String_Vectors.Universal_String_Vector
   is
      Result : League.String_Vectors.Universal_String_Vector;
      Split  : constant League.String_Vectors.Universal_String_Vector :=
        Self.Value.Split (Ada.Characters.Wide_Wide_Latin_1.LF);
      Line   : League.Strings.Universal_String;
   begin
      for J in 1 .. Split.Length loop
         Line := Split.Element (J);
         Ada.Wide_Wide_Text_IO.Put_Line (Line.To_Wide_Wide_String);

         if not Line.Is_Empty then
            Result.Append (Line.Head_To (Line.Length - 1));
         end if;
      end loop;

      return Result;
   end Headers;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return RESP_Message
   is
   begin
      return Result : RESP_Message do
         Result.Value := League.Text_Codecs.Codec_For_Application_Locale.Decode
           (Data.all);
      end return;
   end Read;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access RESP_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.RESP (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : RESP_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "RESP";
      Data := League.Text_Codecs.Codec_For_Application_Locale.Encode
        (Self.Value);
   end Write;

end Slim.Messages.RESP;
