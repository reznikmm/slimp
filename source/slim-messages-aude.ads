--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.aude is
   type Aude_Message is new Message with private;
   --  Tell the client to enable/disable the audio outputs.

   procedure Enable_Output (Self : in out Aude_Message);

private

   subtype Byte is Ada.Streams.Stream_Element;

   type Aude_Message is new Base_Message
     (Max_8  => 2,
      Max_16 => 0,
      Max_32 => 0,
      Max_64 => 0)
   with null record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Aude_Message;

   overriding procedure Write
     (Self : Aude_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Aude_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.aude;
