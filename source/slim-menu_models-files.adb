--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Directories;
with Ada.Containers.Generic_Anonymous_Array_Sort;
with League.String_Vectors;

with Slim.Menu_Commands.Play_File_Commands;

package body Slim.Menu_Models.Files is

   function Path_To_String
     (Self : File_Menu_Model'Class;
      Path : Menu_Path) return League.Strings.Universal_String;

   Filter : constant Ada.Directories.Filter_Type :=
     (Ada.Directories.Directory     => True,
      Ada.Directories.Ordinary_File => True,
      Ada.Directories.Special_File  => False);

   -------------------
   -- Enter_Command --
   -------------------

   overriding function Enter_Command
     (Self : File_Menu_Model;
      Path : Menu_Path) return Slim.Menu_Commands.Menu_Command_Access
   is
      File   : constant League.Strings.Universal_String :=
        Self.Path_To_String (Path);
   begin
      if Ada.Directories.Kind (File.To_UTF_8_String) in
        Ada.Directories.Ordinary_File
      then
         return new Slim.Menu_Commands.Play_File_Commands.Play_File_Command'
           (Player        => Self.Player,
            Relative_Path => File.Tail_From (Self.Root.Length + 1));
      else
         return null;
      end if;
   end Enter_Command;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out File_Menu_Model'Class;
      Root : League.Strings.Universal_String)
   is
   begin
      Self.Root := Root;

      if not Self.Root.Ends_With ("/") then
         Self.Root.Append ("/");
      end if;
   end Initialize;

   ----------------
   -- Item_Count --
   ----------------

   overriding function Item_Count
     (Self : File_Menu_Model;
      Path : Slim.Menu_Models.Menu_Path) return Natural
   is
      procedure Count (Item : Ada.Directories.Directory_Entry_Type);

      File   : constant League.Strings.Universal_String :=
        Self.Path_To_String (Path);
      Name   : constant String := File.To_UTF_8_String;
      Result : Natural := 0;

      -----------
      -- Count --
      -----------

      procedure Count (Item : Ada.Directories.Directory_Entry_Type) is
         Full_Name : constant String := Ada.Directories.Full_Name (Item);
      begin
         if Ada.Directories.Base_Name (Full_Name) /= "" then
            Result := Result + 1;
         end if;
      end Count;
   begin
      if Ada.Directories.Kind (Name) in Ada.Directories.Directory then

         Ada.Directories.Search
           (Directory => Name,
            Pattern   => "",
            Filter    => Filter,
            Process   => Count'Access);
      end if;

      return Result;
   end Item_Count;

   -----------
   -- Label --
   -----------

   overriding function Label
     (Self : File_Menu_Model;
      Path : Slim.Menu_Models.Menu_Path)
      return League.Strings.Universal_String
   is
      File   : constant League.Strings.Universal_String :=
        Self.Path_To_String (Path);
      Name   : constant String := File.To_UTF_8_String;
      Result : constant String := Ada.Directories.Base_Name (Name);
   begin
      return League.Strings.From_UTF_8_String (Result);
   end Label;

   --------------------
   -- Path_To_String --
   --------------------

   function Path_To_String
     (Self : File_Menu_Model'Class;
      Path : Menu_Path) return League.Strings.Universal_String
   is
      use type League.Strings.Universal_String;
      procedure Append (Item : Ada.Directories.Directory_Entry_Type);

      Value  : Menu_Path := Slim.Menu_Models.Root (Self);
      Result : League.Strings.Universal_String := Self.Root;
      List   : League.String_Vectors.Universal_String_Vector;

      function Less (Left, Right : Positive) return Boolean is
         (List (Left) < List (Right));
      procedure Swap (Left, Right : Positive);
      procedure Sort_List is new Ada.Containers.Generic_Anonymous_Array_Sort
        (Index_Type => Positive,
         Less       => Less,
         Swap       => Swap);

      ------------
      -- Append --
      ------------

      procedure Append (Item : Ada.Directories.Directory_Entry_Type) is
         Full_Name : constant String := Ada.Directories.Full_Name (Item);
      begin
         if Ada.Directories.Base_Name (Full_Name) /= "" then
            List.Append (League.Strings.From_UTF_8_String (Full_Name));
         end if;
      end Append;

      ----------
      -- Swap --
      ----------

      procedure Swap (Left, Right : Positive) is
         Temp : constant League.Strings.Universal_String := List (Left);
      begin
         List.Replace (Left, List (Right));
         List.Replace (Right, Temp);
      end Swap;
   begin
      while Value.Length /= Path.Length loop
         Value := (Value.Length + 1, Path.List (1 .. Value.Length + 1));
         List.Clear;

         Ada.Directories.Search
           (Directory => Result.To_UTF_8_String,
            Pattern   => "",
            Filter    => Filter,
            Process   => Append'Access);

         Sort_List (1, List.Length);

         Result := List (Value.List (Value.Length));
      end loop;

      return Result;
   end Path_To_String;

end Slim.Menu_Models.Files;
