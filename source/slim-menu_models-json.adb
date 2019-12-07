--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.JSON.Arrays;
with League.JSON.Documents;
with League.JSON.Values;
with Ada.Streams.Stream_IO;

package body Slim.Menu_Models.JSON is

   function Read_File
     (File   : League.Strings.Universal_String)
      return League.JSON.Documents.JSON_Document;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out JSON_Menu_Model'Class;
      File : League.Strings.Universal_String)
   is
      Document : constant League.JSON.Documents.JSON_Document :=
        Read_File (File);
   begin
      Self.Root := Document.To_JSON_Object;
      Self.Nested := League.Strings.To_Universal_String ("nested");
      Self.Label := League.Strings.To_Universal_String ("label");
   end Initialize;

   ----------------
   -- Item_Count --
   ----------------

   overriding function Item_Count
     (Self : JSON_Menu_Model;
      Path : Slim.Menu_Models.Menu_Path)
      return Natural
   is
      Item : League.JSON.Arrays.JSON_Array :=
        Self.Root.Value (Self.Nested).To_Array;
   begin
      for J of Path.List loop
         Item := Item.Element (J).To_Object.Value (Self.Nested).To_Array;
      end loop;

      return Item.Length;
   end Item_Count;

   -----------
   -- Label --
   -----------

   overriding function Label
     (Self : JSON_Menu_Model; Path : Slim.Menu_Models.Menu_Path)
      return League.Strings.Universal_String
   is
      Item : League.JSON.Objects.JSON_Object := Self.Root;
   begin
      for J of Path.List loop
         Item := Item.Value (Self.Nested).To_Array.Element (J).To_Object;
      end loop;

      return Item.Value (Self.Label).To_String;
   end Label;

   function Read_File
     (File   : League.Strings.Universal_String)
      return League.JSON.Documents.JSON_Document
   is
      Input : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Open
        (Input, Ada.Streams.Stream_IO.In_File, File.To_UTF_8_String);

      declare
         Size : constant Ada.Streams.Stream_Element_Count :=
           Ada.Streams.Stream_Element_Count
             (Ada.Streams.Stream_IO.Size (Input));
         Buffer : Ada.Streams.Stream_Element_Array (1 .. Size);
         Last   : Ada.Streams.Stream_Element_Count;
      begin
         Ada.Streams.Stream_IO.Read (Input, Buffer, Last);

         return Result : constant League.JSON.Documents.JSON_Document :=
           League.JSON.Documents.From_JSON (Buffer (1 .. Last))
         do
            Ada.Streams.Stream_IO.Close (Input);
         end return;
      end;
   end Read_File;

end Slim.Menu_Models.JSON;
