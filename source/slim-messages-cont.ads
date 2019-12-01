--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.cont is
   type Cont_Message is new Message with private;
   --  Tell the client content information

   not overriding procedure Set_Metaint
     (Self  : in out Cont_Message;
      Value : Natural);

private

   subtype Byte is Ada.Streams.Stream_Element;

   type Cont_Message is new Base_Message
     (Max_8  => 1,
      Max_16 => 1,
      Max_32 => 1,
      Max_64 => 0)
   with null record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Cont_Message;

   overriding procedure Write
     (Self : Cont_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Cont_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.cont;
