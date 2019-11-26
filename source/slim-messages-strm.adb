--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Text_Codecs;

with Slim.Message_Visiters;

package body Slim.Messages.strm is

   List : constant Field_Description_Array :=
     ((Uint_8_Field, 1),   --  Command
      (Uint_8_Field, 1),   --  Auto_Start
      (Uint_8_Field, 1),   --  Format
      (Uint_8_Field, 1),   --  PCM_Sample_Size
      (Uint_8_Field, 1),   --  PCM_Sample_Rate
      (Uint_8_Field, 1),   --  PCM_Channels
      (Uint_8_Field, 1),   --  PCM_Endian
      (Uint_8_Field, 1),   --  Threshold
      (Uint_8_Field, 1),   --  Spdif
      (Uint_8_Field, 1),   --  Transition_Period
      (Uint_8_Field, 1),   --  Transition_Type
      (Uint_8_Field, 1),   --  Flags
      (Uint_8_Field, 1),   --  Output_Threshold
      (Uint_8_Field, 1),   --  Reserved
      (Uint_32_Field, 1),  --  Replay_Gain
      (Uint_16_Field, 1),  --  Server_Port
      (Uint_32_Field, 1),  --  Code
      (Custom_Field, 1));  --  Server_IP

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (Self    : in out Strm_Message;
      Command : Play_Command;
      Format  : Play_Format;
      Request : League.Strings.Universal_String)
   is
   begin
      Self.Data_8 :=
        ((case Command is
            when Start => Character'Pos ('s'),
            when Pause => Character'Pos ('p'),
            when Unpause => Character'Pos ('u'),
            when Stop => Character'Pos ('q'),
            when Status  => Character'Pos ('t'),
            when Flush  => Character'Pos ('f'),
            when Skip_Ahead => Character'Pos ('a')),
         Character'Pos ('0'),   --  Auto_Start
         (case Format is when MP3 => Character'Pos ('m')),   --  Format
         Character'Pos ('?'),   --  PCM_Sample_Size
         Character'Pos ('?'),   --  PCM_Sample_Rate
         Character'Pos ('?'),   --  PCM_Channels
         Character'Pos ('?'),   --  PCM_Endian
         0,   --  Threshold
         0,   --  Spdif
         0,   --  Transition_Period
         Character'Pos ('0'),   --  Transition_Type
         0,   --  Flags
         0,   --  Output_Threshold
         0);   --  Reserved
      Self.Request := Request;
   end Initialize;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return Strm_Message is
   begin
      return Result : Strm_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------------------
   -- Read_Custom_Field --
   -----------------------

   overriding procedure Read_Custom_Field
     (Self  : in out Strm_Message;
      Index : Positive;
      Input : in out Ada.Streams.Stream_Element_Offset;
      Data  : League.Stream_Element_Vectors.Stream_Element_Vector)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Content : constant Ada.Streams.Stream_Element_Array (1 .. Data.Length) :=
        Data.To_Stream_Element_Array;
   begin
      pragma Assert (Index = 1);

      Self.Request := League.Text_Codecs.Codec_For_Application_Locale.Decode
        (Content (Input .. Content'Last));

      Input := Content'Last + 1;
   end Read_Custom_Field;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access Strm_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.strm (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : Strm_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "strm";
      Write_Fields (Self, List, Data);
   end Write;

   ------------------------
   -- Write_Custom_Field --
   ------------------------

   overriding procedure Write_Custom_Field
     (Self  : Strm_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      pragma Assert (Index = 1);

      Data.Append
        (League.Text_Codecs.Codec_For_Application_Locale.Encode
           (Self.Request));
   end Write_Custom_Field;

end Slim.Messages.strm;
