--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Strings;

package Slim.Messages.RESP is
   type RESP_Message is new Message with private;

private

   subtype Byte is Ada.Streams.Stream_Element;

   type RESP_Message is new Message with record
      Value : League.Strings.Universal_String;
   end record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return RESP_Message;

   overriding procedure Write
     (Self : RESP_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access RESP_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.RESP;
