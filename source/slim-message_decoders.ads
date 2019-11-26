--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Stream_Element_Vectors;

with Slim.Messages;

package Slim.Message_Decoders is

   type Decoder is tagged limited private;

   not overriding procedure Decode
     (Self   : Decoder;
      Tag    : Slim.Messages.Message_Tag;
      Data   : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector;
      Result : out Slim.Messages.Message_Access);
private

   type Decoder is tagged limited null record;

end Slim.Message_Decoders;
