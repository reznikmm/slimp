--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.STAT is
   type STAT_Message is new Message with private;

   subtype Event_Kind is String (1 .. 4);

   not overriding function Event (Self : STAT_Message) return Event_Kind;

private

   subtype Byte is Ada.Streams.Stream_Element;

   type STAT_Message is new Base_Message
     (Max_8  => 4 + 3,
      Max_16 => 3,
      Max_32 => 8,
      Max_64 => 1)
   with null record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return STAT_Message;

   overriding procedure Write
     (Self : STAT_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access STAT_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.STAT;
