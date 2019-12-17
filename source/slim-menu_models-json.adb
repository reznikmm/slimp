--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.JSON.Arrays;
with League.JSON.Documents;
with League.JSON.Values;
with Ada.Streams.Stream_IO;
with Slim.Menu_Commands.Play_Radio_Commands;

package body Slim.Menu_Models.JSON is

   function Read_File
     (File   : League.Strings.Universal_String)
      return League.JSON.Documents.JSON_Document;

   -------------------
   -- Enter_Command --
   -------------------

   overriding function Enter_Command
     (Self : JSON_Menu_Model;
      Path : Menu_Path) return Slim.Menu_Commands.Menu_Command_Access
   is
      Item : League.JSON.Objects.JSON_Object := Self.Root;
   begin
      if Starts_With (Path, Self.File_Path) then
         return Self.File_Menu.Enter_Command (Suffix (Path, Self.File_Path));
      end if;

      for J of Path.List loop
         Item := Item.Value (Self.Nested).To_Array.Element (J).To_Object;
      end loop;

      if Item.Contains (Self.URL) then
         return new Slim.Menu_Commands.Play_Radio_Commands.Play_Radio_Command'
           (Player => Self.Player,
            URL    => Item.Value (Self.URL).To_String);
      else
         return null;
      end if;
   end Enter_Command;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out JSON_Menu_Model'Class;
      File : League.Strings.Universal_String)
   is
      procedure Find_File_Menu
        (Path   : Menu_Path;
         Object : League.JSON.Objects.JSON_Object);

      --------------------
      -- Find_File_Menu --
      --------------------

      procedure Find_File_Menu
        (Path   : Menu_Path;
         Object : League.JSON.Objects.JSON_Object) is
      begin
         if Object.Contains (Self.Path) then
            Self.File_Path := Path;
            Self.File_Menu.Initialize
              (Object.Value (Self.Path).To_String);
         elsif Object.Contains (Self.Nested) then
            declare
               List : constant League.JSON.Arrays.JSON_Array :=
                 Object.Value (Self.Nested).To_Array;
               Next :  Menu_Path := (Path.Length + 1, Path.List & 1);
            begin
               for J in 1 .. List.Length loop
                  Next.List (Next.Length) := J;
                  Find_File_Menu (Next, List.Element (J).To_Object);
               end loop;
            end;
         end if;
      end Find_File_Menu;

      Document : constant League.JSON.Documents.JSON_Document :=
        Read_File (File);
   begin
      Self.Root := Document.To_JSON_Object;
      Self.Nested := League.Strings.To_Universal_String ("nested");
      Self.Label := League.Strings.To_Universal_String ("label");
      Self.URL := League.Strings.To_Universal_String ("url");
      Self.Path := League.Strings.To_Universal_String ("path");
      Find_File_Menu (Menu_Models.Root (Self), Self.Root);
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
      if Starts_With (Path, Self.File_Path) then
         return Self.File_Menu.Item_Count (Suffix (Path, Self.File_Path));
      end if;

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
      if Starts_With (Path, Self.File_Path)
        and then Path.Length > Self.File_Path.Length
      then
         return Self.File_Menu.Label (Suffix (Path, Self.File_Path));
      end if;

      for J of Path.List loop
         Item := Item.Value (Self.Nested).To_Array.Element (J).To_Object;
      end loop;

      return Item.Value (Self.Label).To_String;
   end Label;

   ---------------
   -- Read_File --
   ---------------

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
