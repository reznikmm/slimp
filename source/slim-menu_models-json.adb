--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Numerics.Discrete_Random;
with Ada.Streams.Stream_IO;
with League.JSON.Arrays;
with League.JSON.Documents;
with League.JSON.Values;
with League.String_Vectors;
with Slim.Menu_Commands.Play_File_Commands;
with Slim.Menu_Commands.Play_Radio_Commands;

package body Slim.Menu_Models.JSON is

   function Read_File
     (File : League.Strings.Universal_String)
      return League.JSON.Documents.JSON_Document;

   function Play_Recursive
     (Self : JSON_Menu_Model'Class;
      Object : League.JSON.Objects.JSON_Object)
      return Slim.Menu_Commands.Menu_Command_Access;

   -------------------
   -- Enter_Command --
   -------------------

   overriding function Enter_Command
     (Self : JSON_Menu_Model;
      Path : Menu_Path) return Slim.Menu_Commands.Menu_Command_Access
   is
      String : League.Strings.Universal_String;
      Object : League.JSON.Objects.JSON_Object;
      Item   : League.JSON.Arrays.JSON_Array :=
        Self.Root.Value (Self.Nested).To_Array;
   begin
      for J in 1 .. Path.Length loop
         Object := Item.Element (Path.List (J)).To_Object;

         if Object.Contains (Self.Playlist) then
            --  Delegate Enter_Command to playlist model
            String := Object.Value (Self.Playlist).To_String;

            return Self.Playlists (String).all.Enter_Command
              ((Length => Path.Length - J,
                List   => Path.List (J + 1 .. Path.Length)));
         elsif J = Path.Length then
            if not Object.Contains (Self.URL) then
               return null;
            end if;

            return
              new Slim.Menu_Commands.Play_Radio_Commands.Play_Radio_Command'
                (Player => Self.Player,
                 URL    => Object.Value (Self.URL).To_String);
         else
            Item := Object.Value (Self.Nested).To_Array;
         end if;
      end loop;

      return null;
   end Enter_Command;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out JSON_Menu_Model'Class;
      File : League.Strings.Universal_String)
   is
      procedure Read_Playlists
        (Path   : Menu_Path;
         Object : League.JSON.Objects.JSON_Object);

      --------------------
      -- Read_Playlists --
      --------------------

      procedure Read_Playlists
        (Path   : Menu_Path;
         Object : League.JSON.Objects.JSON_Object) is
      begin
         if Object.Contains (Self.Playlist) then
            declare
               Root : constant League.Strings.Universal_String :=
                 Object.Value (Self.Path).To_String;

               Label : constant League.Strings.Universal_String :=
                 Object.Value (Self.Label).To_String;

               Value : constant League.Strings.Universal_String :=
                 Object.Value (Self.Playlist).To_String;

               Next : constant Play_List_Access := new
                 Slim.Menu_Models.Play_Lists.Play_List_Menu_Model
                   (Self.Player);
            begin
               Next.Initialize (Label => Label, Root => Root, File => Value);
               Self.Playlists.Insert (Value, Next);
            end;
         elsif Object.Contains (Self.Nested) then
            declare
               List : constant League.JSON.Arrays.JSON_Array :=
                 Object.Value (Self.Nested).To_Array;
               Next :  Menu_Path := (Path.Length + 1, Path.List & 1);
            begin
               for J in 1 .. List.Length loop
                  Next.List (Next.Length) := J;
                  Read_Playlists (Next, List.Element (J).To_Object);
               end loop;
            end;
         end if;
      end Read_Playlists;

      Document : constant League.JSON.Documents.JSON_Document :=
        Read_File (File);
   begin
      Self.Root := Document.To_JSON_Object;
      Self.Nested := League.Strings.To_Universal_String ("nested");
      Self.Label := League.Strings.To_Universal_String ("label");
      Self.URL := League.Strings.To_Universal_String ("url");
      Self.Path := League.Strings.To_Universal_String ("path");
      Self.Playlist := League.Strings.To_Universal_String ("playlist");
      Read_Playlists (Menu_Models.Root (Self), Self.Root);
   end Initialize;

   ----------------
   -- Item_Count --
   ----------------

   overriding function Item_Count
     (Self : JSON_Menu_Model;
      Path : Slim.Menu_Models.Menu_Path)
      return Natural
   is
      String : League.Strings.Universal_String;
      Object : League.JSON.Objects.JSON_Object;
      Item   : League.JSON.Arrays.JSON_Array :=
        Self.Root.Value (Self.Nested).To_Array;
      Result : Natural;
   begin
      for J in 1 .. Path.Length loop
         Object := Item.Element (Path.List (J)).To_Object;

         if Object.Contains (Self.Playlist) then
            --  Delegate Item_Count to playlist model
            String := Object.Value (Self.Playlist).To_String;

            Result := Self.Playlists (String).all.Item_Count
              ((Length => Path.Length - J,
                List   => Path.List (J + 1 .. Path.Length)));

            return Result;
         else
            Item := Object.Value (Self.Nested).To_Array;
         end if;
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
      String : League.Strings.Universal_String;
      Object : League.JSON.Objects.JSON_Object;
      Item   : League.JSON.Arrays.JSON_Array :=
        Self.Root.Value (Self.Nested).To_Array;
   begin
      for J in 1 .. Path.Length loop
         Object := Item.Element (Path.List (J)).To_Object;

         if Object.Contains (Self.Playlist) then
            --  Delegate Label to playlist model
            String := Object.Value (Self.Playlist).To_String;

            String := Self.Playlists (String).all.Label
              ((Length => Path.Length - J,
                List   => Path.List (J + 1 .. Path.Length)));

            return String;
         elsif J = Path.Length then
            String := Object.Value (Self.Label).To_String;

            return String;
         else
            Item := Object.Value (Self.Nested).To_Array;
         end if;
      end loop;

      return String;
   end Label;

   ------------------
   -- Play_Command --
   ------------------

   overriding function Play_Command
     (Self : JSON_Menu_Model;
      Path : Menu_Path) return Slim.Menu_Commands.Menu_Command_Access
   is
      String : League.Strings.Universal_String;
      Object : League.JSON.Objects.JSON_Object;
      Item   : League.JSON.Arrays.JSON_Array :=
        Self.Root.Value (Self.Nested).To_Array;
   begin
      for J in 1 .. Path.Length loop
         Object := Item.Element (Path.List (J)).To_Object;

         if Object.Contains (Self.Playlist) then
            --  Delegate Play_Command to playlist model
            String := Object.Value (Self.Playlist).To_String;

            return Self.Playlists (String).all.Play_Command
              ((Length => Path.Length - J,
                List   => Path.List (J + 1 .. Path.Length)));
         elsif J = Path.Length then
            if not Object.Contains (Self.URL) then
               return Play_Recursive (Self, Object);
            end if;

            return
              new Slim.Menu_Commands.Play_Radio_Commands.Play_Radio_Command'
                (Player => Self.Player,
                 URL    => Object.Value (Self.URL).To_String);
         else
            Item := Object.Value (Self.Nested).To_Array;
         end if;
      end loop;

      return null;
   end Play_Command;

   --------------------
   -- Play_Recursive --
   --------------------

   function Play_Recursive
     (Self   : JSON_Menu_Model'Class;
      Object : League.JSON.Objects.JSON_Object)
      return Slim.Menu_Commands.Menu_Command_Access
   is
      procedure Collect (Object : League.JSON.Objects.JSON_Object);

      procedure Shuffle
        (Origin_Paths  : League.String_Vectors.Universal_String_Vector;
         Origin_Titles : League.String_Vectors.Universal_String_Vector;
         Paths         : out League.String_Vectors.Universal_String_Vector;
         Titles        : out League.String_Vectors.Universal_String_Vector);

      -------------
      -- Shuffle --
      -------------

      procedure Shuffle
        (Origin_Paths  : League.String_Vectors.Universal_String_Vector;
         Origin_Titles : League.String_Vectors.Universal_String_Vector;
         Paths         : out League.String_Vectors.Universal_String_Vector;
         Titles        : out League.String_Vectors.Universal_String_Vector)
      is
         package Randoms is new Ada.Numerics.Discrete_Random (Positive);

         Generator : Randoms.Generator;
         Map       : array (1 .. Origin_Paths.Length) of Positive;
         Last      : Natural := Map'Last;
         Index     : Positive;
      begin
         Randoms.Reset (Generator);

         for J in Map'Range loop
            Map (J) := J;
         end loop;

         while Last > 0 loop
            Index := (Randoms.Random (Generator) mod Last) + 1;
            Paths.Append (Origin_Paths (Map (Index)));
            Titles.Append (Origin_Titles (Map (Index)));
            Map (Index) := Map (Last);
            Last := Last - 1;
         end loop;
      end Shuffle;

      Relative_Path_List : League.String_Vectors.Universal_String_Vector;
      Title_List         : League.String_Vectors.Universal_String_Vector;

      -------------
      -- Collect --
      -------------

      procedure Collect (Object : League.JSON.Objects.JSON_Object) is
         String : League.Strings.Universal_String;
         List   : constant League.JSON.Arrays.JSON_Array :=
           Object.Value (Self.Nested).To_Array;
      begin
         if Object.Contains (Self.Playlist) then
            --  Delegate Collect to playlist model
            String := Object.Value (Self.Playlist).To_String;

            Self.Playlists (String).all.Collect
              (Relative_Path_List, Title_List);
            return;
         end if;

         for J in 1 .. List.Length loop
            Collect (List (J).To_Object);
         end loop;
      end Collect;
   begin
      Collect (Object);

      if Relative_Path_List.Is_Empty then
         return null;
      end if;

      declare
         use Slim.Menu_Commands.Play_File_Commands;

         Result : constant Play_File_Command_Access :=
           new Play_File_Command (Self.Player);
      begin
         Shuffle
           (Relative_Path_List,
            Title_List,
            Result.Relative_Path_List,
            Result.Title_List);

         return Slim.Menu_Commands.Menu_Command_Access (Result);
      end;
   end Play_Recursive;

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
