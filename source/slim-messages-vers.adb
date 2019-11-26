--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Text_Codecs;

with Slim.Message_Visiters;

package body Slim.Messages.vers is

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Vers_Message
   is
   begin
      return Result : Vers_Message do
         Result.Version :=
           League.Text_Codecs.Codec_For_Application_Locale.Decode (Data.all);
      end return;
   end Read;

   -----------------
   -- Set_Version --
   -----------------

   not overriding procedure Set_Version
     (Self : in out Vers_Message;
      Value : League.Strings.Universal_String) is
   begin
      Self.Version := Value;
   end Set_Version;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Vers_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.vers (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Vers_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "vers";
      Data :=
        League.Text_Codecs.Codec_For_Application_Locale.Encode (Self.Version);
   end Write;

end Slim.Messages.vers;
