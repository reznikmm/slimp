--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

package body Slim.Messages.STAT is
   List : constant Field_Description_Array :=
     ((Uint_8_Field, 4),   --  Event Code (a 4 byte string)
      (Uint_8_Field, 1),   --  Number of headers
      (Uint_8_Field, 1),   --  MAS Initalized - 'm' or 'p'
      (Uint_8_Field, 1),   --  MAS Mode - serdes mode?
      (Uint_32_Field, 1),  --  buffer size - in bytes, of the player's buffer
      (Uint_32_Field, 1),  --  fullness - data bytes in the player's buffer
      (Uint_64_Field, 1),  --  Bytes Recieved
      (Uint_16_Field, 1),  --  Wireless Signal Strength (0-100)
      (Uint_32_Field, 1),  --  jiffies - a timestamp from the player (@1kHz)
      (Uint_32_Field, 1),  --  output buffer size - the decoded audio data
      (Uint_32_Field, 1),  --  output buffer fullness - in decoded audio data
      (Uint_32_Field, 1),  --  elapsed seconds - of the current stream
      (Uint_16_Field, 1),  --  Voltage
      (Uint_32_Field, 1),  --  elapsed milliseconds - of the current stream
      (Uint_32_Field, 1),  --  server timestamp - reflected from an strm-t cmd
      (Uint_16_Field, 1));  --  error code - used with STMn

   -----------
   -- Event --
   -----------

   not overriding function Event (Self : STAT_Message) return Event_Kind is
   begin
      return (Character'Val (Self.Data_8 (1)),
              Character'Val (Self.Data_8 (2)),
              Character'Val (Self.Data_8 (3)),
              Character'Val (Self.Data_8 (4)));
   end Event;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return STAT_Message is
   begin
      return Result : STAT_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access STAT_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.STAT (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : STAT_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "STAT";
      Write_Fields (Self, List, Data);
   end Write;

end Slim.Messages.STAT;
