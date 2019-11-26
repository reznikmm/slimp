--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with Slim.Messages;
with Slim.Messages.grfe;
with Slim.Messages.grfg;
with Slim.Messages.grfs;
with Slim.Message_Decoders;
with Slim.Message_Visiters;
with League.Application;
with League.Stream_Element_Vectors;
with League.Strings;

procedure Slim.Read_File is

   procedure Client_Message
     (Client : access Ada.Streams.Root_Stream_Type'Class);

   procedure Server_Message
     (Server : access Ada.Streams.Root_Stream_Type'Class);

   package Dump is

      type Visiter is new Slim.Message_Visiters.Visiter with record
         Count : Positive := 1;
      end record;

      not overriding procedure Dump_File
        (Self    : in out Visiter;
         Tag     : Slim.Messages.Message_Tag;
         Data    : Ada.Streams.Stream_Element_Array);

      overriding procedure grfe
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.grfe.Grfe_Message);

      overriding procedure grfg
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.grfg.Grfg_Message);

      overriding procedure grfs
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.grfs.Grfs_Message);

   end Dump;

   package body Dump is

      overriding procedure grfe
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.grfe.Grfe_Message) is
      begin
         Self.Dump_File ("grfe", Message.Data);
      end grfe;

      overriding procedure grfg
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.grfg.Grfg_Message) is
      begin
         Self.Dump_File ("grfg", Message.Data);
      end grfg;

      overriding procedure grfs
        (Self    : in out Visiter;
         Message : not null access Slim.Messages.grfs.Grfs_Message) is
      begin
         Self.Dump_File ("grfs", Message.Data);
      end grfs;

      not overriding procedure Dump_File
        (Self    : in out Visiter;
         Tag     : Slim.Messages.Message_Tag;
         Data    : Ada.Streams.Stream_Element_Array)
      is
--  convert -size 32x160 -depth 1 gray:F1.dat -flop -rotate -90 image.png
         Output : Ada.Streams.Stream_IO.File_Type;
         Image  : String := Positive'Image (Self.Count);
      begin
         Image (1) := 'F';
         Self.Count := Self.Count + 1;
         Ada.Streams.Stream_IO.Create
           (File => Output,
            Name => "/tmp/" & Image & Tag & ".dat");
         Ada.Streams.Stream_IO.Write (Output, Data);
         Ada.Streams.Stream_IO.Close (Output);
      end Dump_File;

   end Dump;

   Visiter : Dump.Visiter;

   --------------------
   -- Client_Message --
   --------------------

   procedure Client_Message
     (Client : access Ada.Streams.Root_Stream_Type'Class)
   is
      use type Ada.Streams.Stream_Element_Offset;
      Tag     : Slim.Messages.Message_Tag;
      Raw_Tag : Ada.Streams.Stream_Element_Array (1 .. 4)
        with Address => Tag'Address;
      Word    : Ada.Streams.Stream_Element_Array (1 .. 4);
      Length  : Ada.Streams.Stream_Element_Offset := 0;
      Last    : Ada.Streams.Stream_Element_Offset;
      Data    : aliased League.Stream_Element_Vectors.Stream_Element_Vector;
   begin
      Client.Read (Raw_Tag, Last);
      pragma Assert (Last = Raw_Tag'Length);
      Ada.Text_IO.Put_Line (Tag);
      Client.Read (Word, Last);
      pragma Assert (Last = Word'Length);

      for Byte of Word loop
         Length := Length * 256 + Ada.Streams.Stream_Element_Offset (Byte);
      end loop;

      while Length > 0 loop
         declare
            Piece : constant Ada.Streams.Stream_Element_Offset :=
              Ada.Streams.Stream_Element_Offset'Min (Length, 256);
            Input : Ada.Streams.Stream_Element_Array (1 .. Piece);
         begin
            Client.Read (Input, Last);
            pragma Assert (Last = Input'Length);
            Data.Append (Input);
            Length := Length - Last;
         end;
      end loop;

      declare
         Decoder : Slim.Message_Decoders.Decoder;
         Message : Slim.Messages.Message_Access;
         pragma Unreferenced (Message);
      begin
         Decoder.Decode (Tag, Data'Unchecked_Access, Message);
         Data.Clear;
      end;
   end Client_Message;

   --------------------
   -- Server_Message --
   --------------------

   procedure Server_Message
     (Server : access Ada.Streams.Root_Stream_Type'Class)
   is
      use type Ada.Streams.Stream_Element_Offset;
      Tag     : Slim.Messages.Message_Tag;
      Raw_Tag : Ada.Streams.Stream_Element_Array (1 .. 4)
        with Address => Tag'Address;
      Word    : Ada.Streams.Stream_Element_Array (1 .. 2);
      Length  : Ada.Streams.Stream_Element_Offset := 0;
      Last    : Ada.Streams.Stream_Element_Offset;
      Data    : aliased League.Stream_Element_Vectors.Stream_Element_Vector;
   begin
      Server.Read (Word, Last);
      pragma Assert (Last = Word'Length);
      Server.Read (Raw_Tag, Last);
      pragma Assert (Last = Raw_Tag'Length);
      Ada.Text_IO.Put_Line (Tag);

      for Byte of Word loop
         Length := Length * 256 + Ada.Streams.Stream_Element_Offset (Byte);
      end loop;

      Length := Length - 4;

      while Length > 0 loop
         declare
            Piece : constant Ada.Streams.Stream_Element_Offset :=
              Ada.Streams.Stream_Element_Offset'Min (Length, 256);
            Input : Ada.Streams.Stream_Element_Array (1 .. Piece);
         begin
            Server.Read (Input, Last);
            pragma Assert (Last = Input'Length);
            Data.Append (Input);
            Length := Length - Last;
         end;
      end loop;

      declare
         Decoder : Slim.Message_Decoders.Decoder;
         Message : Slim.Messages.Message_Access;
      begin
         Decoder.Decode (Tag, Data'Unchecked_Access, Message);
         Message.Visit (Visiter);
         Data.Clear;
      end;
   end Server_Message;

   File_Name : constant League.Strings.Universal_String :=
     League.Application.Arguments.Element (1);
   Is_Client : constant Boolean := File_Name.Starts_With ("cli");
   Input     : Ada.Streams.Stream_IO.File_Type;
   Stream    : Ada.Streams.Stream_IO.Stream_Access;
begin
   Ada.Streams.Stream_IO.Open
     (File => Input,
      Mode => Ada.Streams.Stream_IO.In_File,
      Name => File_Name.To_UTF_8_String);

   Stream := Ada.Streams.Stream_IO.Stream (Input);

   while not Ada.Streams.Stream_IO.End_Of_File (Input) loop
      if Is_Client then
         Client_Message (Stream);
      else
         Server_Message (Stream);
      end if;
   end loop;
end Slim.Read_File;
