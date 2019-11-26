--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.bled is
   type Bled_Message is new Message with private;
   --  Set 16 backlight leds
   not overriding procedure Enable_LED (Self : in out Bled_Message);

private

   subtype Byte is Ada.Streams.Stream_Element;

   type Bled_Message is new Base_Message
     (Max_8  => 0,
      Max_16 => 1,
      Max_32 => 0,
      Max_64 => 0)
   with null record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Bled_Message;

   overriding procedure Write
     (Self : Bled_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Bled_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.bled;
