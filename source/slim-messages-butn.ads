--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Messages.BUTN is
   type BUTN_Message is new Message with private;

   type Button_Kind is
     (Preset_1,
      Pause,
      Back,
      Knob_Left, Knob_Right, Knob_Push,
      Volume_Up, Volume_Down,
      Rewind, Forward, Play, Something_Else);

   not overriding function Button
     (Self : BUTN_Message) return Button_Kind;

private

   subtype Byte is Ada.Streams.Stream_Element;

   type BUTN_Message is new Base_Message
     (Max_8 => 4,
      Max_16 => 0,
      Max_32 => 1,
      Max_64 => 0)
   with null record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return BUTN_Message;

   overriding procedure Write
     (Self : BUTN_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access BUTN_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.BUTN;
