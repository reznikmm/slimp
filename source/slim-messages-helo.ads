--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.HELO is
   type HELO_Message is new Message with private;

private

   subtype Byte is Ada.Streams.Stream_Element;

   type HELO_Message is new Base_Message
     (Max_8 => 1 + 1 + 6 + 16 + 2,
      Max_16 => 1,
      Max_32 => 0,
      Max_64 => 1)
   with null record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return HELO_Message;

   overriding procedure Write
     (Self : HELO_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access HELO_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.HELO;
