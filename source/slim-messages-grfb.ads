--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.grfb is
   type Grfb_Message is new Message with private;
   --  Tells the client to adjust the brightness of the display.

   not overriding procedure Set_Brightness
     (Self  : in out Grfb_Message;
      Value : Natural);

private

   type Grfb_Message is new Base_Message
     (Max_8  => 0,
      Max_16 => 1,
      Max_32 => 0,
      Max_64 => 0)
   with null record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Grfb_Message;

   overriding procedure Write
     (Self : Grfb_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Grfb_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.grfb;
