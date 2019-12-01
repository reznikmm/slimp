--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Strings;

package Slim.Messages.META is
   type META_Message is new Message with private;

   not overriding function Value (Self : META_Message)
     return League.Strings.Universal_String;

private

   subtype Byte is Ada.Streams.Stream_Element;

   type META_Message is new Message with record
      Value : League.Strings.Universal_String;
   end record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return META_Message;

   overriding procedure Write
     (Self : META_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access META_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.META;
