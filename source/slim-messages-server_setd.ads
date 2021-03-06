--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.Server_setd is
   type Setd_Message is new Message with private;

   not overriding procedure Request_Player_Name (Self : in out Setd_Message);

private

   subtype Byte is Ada.Streams.Stream_Element;

   type Setd_Message is new Base_Message
     (Max_8  => 1,
      Max_16 => 0,
      Max_32 => 0,
      Max_64 => 0)
   with null record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Setd_Message;

   overriding procedure Write
     (Self : Setd_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Setd_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.Server_setd;
