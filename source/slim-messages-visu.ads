--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.visu is
   type Visu_Message is new Message with private;
   --  activate/deactivate visualizer for the music.

   not overriding procedure Deactivate (Self : in out Visu_Message);

private

   subtype Byte is Ada.Streams.Stream_Element;

   type Visu_Message is new Base_Message
     (Max_8  => 2,
      Max_16 => 0,
      Max_32 => 0,
      Max_64 => 0)
   with null record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Visu_Message;

   overriding procedure Write
     (Self : Visu_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Visu_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.visu;
