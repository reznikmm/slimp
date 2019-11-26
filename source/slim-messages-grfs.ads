--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.grfs is
   type Grfs_Message is new Message with private;
   --  Sends a bitmap to the client to scroll.

   not overriding function Data
     (Self : Grfs_Message) return Ada.Streams.Stream_Element_Array;

private

   type Grfs_Message is new Base_Message
     (Max_8  => 2,
      Max_16 => 4,
      Max_32 => 2,
      Max_64 => 0)
   with record
      Data : League.Stream_Element_Vectors.Stream_Element_Vector;
   end record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Grfs_Message;

   overriding procedure Write
     (Self : Grfs_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Read_Custom_Field
     (Self  : in out Grfs_Message;
      Index : Positive;
      Input : in out Ada.Streams.Stream_Element_Offset;
      Data  : League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Write_Custom_Field
     (Self  : Grfs_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Grfs_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.grfs;
