--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Strings;

package Slim.Messages.strm is
   type Strm_Message is new Message with private;

   type Play_Command is
     (Start, Pause, Unpause, Stop, Status, Flush, Skip_Ahead);

   type Play_Format is (MP3);

   not overriding procedure Initialize
     (Self    : in out Strm_Message;
      Command : Play_Command;
      Format  : Play_Format;
      Request : League.Strings.Universal_String);

private

   subtype Byte is Ada.Streams.Stream_Element;

   type Strm_Message is new Base_Message
     (Max_8 => 14,
      Max_16 => 1,
      Max_32 => 2,
      Max_64 => 0)
   with record
      Request : League.Strings.Universal_String;
   end record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return Strm_Message;

   overriding procedure Write
     (Self : Strm_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Read_Custom_Field
     (Self  : in out Strm_Message;
      Index : Positive;
      Input : in out Ada.Streams.Stream_Element_Offset;
      Data  : League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Write_Custom_Field
     (Self  : Strm_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access Strm_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.strm;
