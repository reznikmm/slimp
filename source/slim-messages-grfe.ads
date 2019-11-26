--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.grfe is
   type Grfe_Message is new Message with private;
   --  Sends a bitmap to the client for display.

   not overriding procedure Initialize
     (Self   : in out Grfe_Message;
      Offset : Natural;
      Value  : Ada.Streams.Stream_Element_Array);

   not overriding function Data
     (Self : Grfe_Message) return Ada.Streams.Stream_Element_Array;

private

   subtype Byte is Ada.Streams.Stream_Element;

   type Grfe_Message is new Base_Message
     (Max_8 => 1 + 1,
      Max_16 => 1,
      Max_32 => 0,
      Max_64 => 0)
   with record
      Data : League.Stream_Element_Vectors.Stream_Element_Vector;
   end record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Grfe_Message;

   overriding procedure Read_Custom_Field
     (Self  : in out Grfe_Message;
      Index : Positive;
      Input : in out Ada.Streams.Stream_Element_Offset;
      Data  : League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Write_Custom_Field
     (Self  : Grfe_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Write
     (Self : Grfe_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Grfe_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.grfe;
