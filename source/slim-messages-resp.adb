--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Text_Codecs;

with Slim.Message_Visiters;

package body Slim.Messages.RESP is

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
