--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.grfg is
   type Grfg_Message is new Message with private;
   --  Set background frame and start scrolling.

   not overriding function Data
     (Self : Grfg_Message) return Ada.Streams.Stream_Element_Array;

private

   type Grfg_Message is new Base_Message
     (Max_8  => 0,
      Max_16 => 2,
      Max_32 => 0,
      Max_64 => 0)
   with record
      Data : League.Stream_Element_Vectors.Stream_Element_Vector;
   end record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Grfg_Message;

   overriding procedure Write
     (Self : Grfg_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Read_Custom_Field
     (Self  : in out Grfg_Message;
      Index : Positive;
      Input : in out Ada.Streams.Stream_Element_Offset;
      Data  : League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Write_Custom_Field
     (Self  : Grfg_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Grfg_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.grfg;
