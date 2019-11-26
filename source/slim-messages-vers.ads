--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Strings;

package Slim.Messages.vers is
   type Vers_Message is new Message with private;

   not overriding procedure Set_Version
     (Self  : in out Vers_Message;
      Value : League.Strings.Universal_String);

private

   subtype Byte is Ada.Streams.Stream_Element;

   type Vers_Message is new Message with record
      Version : League.Strings.Universal_String;
   end record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Vers_Message;

   overriding procedure Write
     (Self : Vers_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Vers_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.vers;
