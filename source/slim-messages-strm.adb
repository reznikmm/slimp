--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Characters.Wide_Wide_Latin_1;

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
      (Uint_8_Field, 4),   --  Server_IP
      (Custom_Field, 1));  --  Request

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

   ----------
   -- Stop --
   ----------

   not overriding procedure Simple_Command
     (Self    : in out Strm_Message;
      Command : Play_Command) is
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
         Character'Pos ('m'),   --  Format
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
         0,   --  Reserved
         0, 0, 0, 0);  --  Server_IP
      Self.Request.Clear;
   end Simple_Command;

   -----------
   -- Start --
   -----------

   not overriding procedure Start
     (Self      : in out Strm_Message;
      Server    : Server_Address;
      Request   : League.String_Vectors.Universal_String_Vector;
      Auto_Play : Boolean := True)
   is
      subtype X is Interfaces.Unsigned_8;

      Auto_Start : Character := '2';  --  direct streaming

      Image : constant League.String_Vectors.Universal_String_Vector :=
        League.Strings.From_UTF_8_String
          (GNAT.Sockets.Image (Server.Addr)).Split ('.');

      CR_LF : constant Wide_Wide_String :=
        (Ada.Characters.Wide_Wide_Latin_1.CR,
         Ada.Characters.Wide_Wide_Latin_1.LF);
   begin
      if Auto_Play then
         Auto_Start := '3';  --  direct+auto
      end if;

      Self.Data_8 :=
        (Character'Pos ('s'),
         Character'Pos (Auto_Start),   --  Auto_Start
         Character'Pos ('m'),   --  Format MP3
         Character'Pos ('?'),   --  PCM_Sample_Size
         Character'Pos ('?'),   --  PCM_Sample_Rate
         Character'Pos ('?'),   --  PCM_Channels
         Character'Pos ('?'),   --  PCM_Endian
         20,   --  Threshold, KB input buffer data before autostart or notify
         0,   --  Spdif
         0,   --  Transition_Period
         Character'Pos ('0'),   --  Transition_Type
         0,   --  Flags
         1,   --  Output Threshold, output buffer before playback starts 0.1s
         0,   --  Reserved
         X'Wide_Wide_Value (Image (1).To_Wide_Wide_String),
         X'Wide_Wide_Value (Image (2).To_Wide_Wide_String),
         X'Wide_Wide_Value (Image (3).To_Wide_Wide_String),
         X'Wide_Wide_Value (Image (4).To_Wide_Wide_String));

      Self.Data_16 := (1 => Interfaces.Unsigned_16 (Server.Port)); --  Port
      Self.Data_32 := (1 => 0); --  replay gain
      Self.Request := Request.Join (CR_LF);
   end Start;

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
