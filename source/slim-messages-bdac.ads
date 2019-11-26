--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.bdac is
   type Bdac_Message is new Message with private;
   --  Sends a DAC settings to the client.

   not overriding procedure Initialize
     (Self  : in out Bdac_Message;
      Kind  : Natural;
      Value : Ada.Streams.Stream_Element_Array);

private

   type Bdac_Message is new Base_Message
     (Max_8  => 1,
      Max_16 => 0,
      Max_32 => 0,
      Max_64 => 0)
   with record
      Data : League.Stream_Element_Vectors.Stream_Element_Vector;
   end record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Bdac_Message;

   overriding procedure Write
     (Self : Bdac_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Read_Custom_Field
     (Self  : in out Bdac_Message;
      Index : Positive;
      Input : in out Ada.Streams.Stream_Element_Offset;
      Data  : League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Write_Custom_Field
     (Self  : Bdac_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Bdac_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.bdac;
