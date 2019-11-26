--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.rtcs is
   type Rtcs_Message is new Message with private;

   not overriding procedure Set_Format (Self : in out Rtcs_Message);

   not overriding procedure Set_Time
     (Self    : in out Rtcs_Message;
      Hours   : Natural;
      Minutes : Natural;
      Seconds : Natural);

private

   subtype Byte is Ada.Streams.Stream_Element;

   type Rtcs_Message is new Base_Message
     (Max_8 => 4,
      Max_16 => 0,
      Max_32 => 0,
      Max_64 => 0)
   with null record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Rtcs_Message;

   overriding procedure Write
     (Self : Rtcs_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Rtcs_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.rtcs;
