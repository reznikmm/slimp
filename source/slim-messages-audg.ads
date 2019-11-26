--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.audg is
   type Audg_Message is new Message with private;
   --  Tell the client to adjust the audio gain (volume level)

   subtype Volume is Natural range 0 .. 100;

   not overriding procedure Set_Volume
     (Self  : in out Audg_Message;
      Value : Volume);

private

   subtype Byte is Ada.Streams.Stream_Element;

   type Audg_Message is new Base_Message
     (Max_8  => 2,
      Max_16 => 0,
      Max_32 => 5,
      Max_64 => 0)
   with null record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Audg_Message;

   overriding procedure Write
     (Self : Audg_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Audg_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.audg;
