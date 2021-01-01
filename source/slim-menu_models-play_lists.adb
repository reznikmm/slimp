--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with Slim.Menu_Commands.Play_File_Commands;

package body Slim.Menu_Models.Play_Lists is

   function Create_Play_Command
     (Self : Play_List_Menu_Model'Class;
      Path : Menu_Path;
      Skip : Natural := 0) return Slim.Menu_Commands.Menu_Command_Access;

   -------------
   -- Collect --
   -------------

   procedure Collect
     (Self       : Play_List_Menu_Model'Class;
      Path_List  : in out League.String_Vectors.Universal_String_Vector;
      Title_List : in out League.String_Vectors.Universal_String_Vector)
   is
      use type League.Strings.Universal_String;
   begin
      for J in 1 .. Self.Items.Last_Index loop
         declare
            Item : constant Play_List_Item := Self.Items (J);
         begin
            Path_List.Append (Self.Path & Item.URI);
            Title_List.Append (Item.Label);
         end;
      end loop;
   end Collect;

   -------------------------
   -- Create_Play_Command --
   -------------------------

   function Create_Play_Command
     (Self : Play_List_Menu_Model'Class;
      Path : Menu_Path;
      Skip : Natural := 0) return Slim.Menu_Commands.Menu_Command_Access
   is
      use type League.Strings.Universal_String;
      use Slim.Menu_Commands.Play_File_Commands;

      Result : constant Play_File_Command_Access :=
        new Play_File_Command (Self.Player);

   begin
      Result.Root := Self.Root;
      Result.M3U_Name := Self.M3U;
      Result.Start := Path.List (1);
      Result.Skip := Skip;

      for J in 1 .. Self.Items.Last_Index loop
         declare
            Item : constant Play_List_Item := Self.Items (J);
         begin
            Result.Relative_Path_List.Append (Self.Path & Item.URI);
            Result.Title_List.Append (Item.Label);
         end;
      end loop;

      return Slim.Menu_Commands.Menu_Command_Access (Result);
   end Create_Play_Command;

   -------------------
   -- Enter_Command --
   -------------------

   overriding function Enter_Command
     (Self : Play_List_Menu_Model;
      Path : Menu_Path) return Slim.Menu_Commands.Menu_Command_Access is
   begin
      return Self.Create_Play_Command (Path);
   end Enter_Command;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : in out Play_List_Menu_Model'Class;
      Label : League.Strings.Universal_String;
      Root  : League.Strings.Universal_String;
      File  : League.Strings.Universal_String)
   is
      Name  : constant String := File.To_UTF_8_String;
      Input : Ada.Wide_Wide_Text_IO.File_Type;
      Next  : League.Strings.Universal_String;
   begin
      Self.Label := Label;
      Self.M3U := File;
      Self.Root := Root;
      Self.Path := File.Tail_From (Root.Length + 1);
      Self.Path := Self.Path.Head (Self.Path.Last_Index ('/'));

      Ada.Wide_Wide_Text_IO.Open
        (Input, Ada.Wide_Wide_Text_IO.In_File, Name);

      while not Ada.Wide_Wide_Text_IO.End_Of_File (Input) loop
         declare
            Line : constant League.Strings.Universal_String :=
              League.Strings.To_Universal_String
                (Ada.Wide_Wide_Text_IO.Get_Line (Input));
         begin
            if Line.Starts_With ("#EXTINF:") then
               declare
                  List : constant League.String_Vectors.Universal_String_Vector
                    := Line.Split (',');
               begin
                  if List.Length > 1 then
                     Next := List.Element (2);
                  end if;
               end;
            elsif Line.Is_Empty or else Line.Starts_With ("#") then
               null;  --  Skip other tags
            else
               if Next.Is_Empty then
                  Next := Line;
               end if;

               Self.Items.Append ((URI => Line, Label => Next));

               Next.Clear;
            end if;
         end;
      end loop;

      Ada.Wide_Wide_Text_IO.Close (Input);
   end Initialize;

   ----------------
   -- Item_Count --
   ----------------

   overriding function Item_Count
     (Self : Play_List_Menu_Model;
      Path : Slim.Menu_Models.Menu_Path) return Natural
   is
   begin
      if Path.Length = 0 then
         return Self.Items.Last_Index;
      else
         return 0;  --  No nested items in a play list
      end if;
   end Item_Count;

   -----------
   -- Label --
   -----------

   overriding function Label
     (Self : Play_List_Menu_Model;
      Path : Slim.Menu_Models.Menu_Path)
      return League.Strings.Universal_String is
   begin
      if Path.Length = 0 then
         return Self.Label;
      else
         return Self.Items (Path.List (1)).Label;
      end if;
   end Label;

   ------------------
   -- Play_Command --
   ------------------

   overriding function Play_Command
     (Self : Play_List_Menu_Model;
      Path : Menu_Path) return Slim.Menu_Commands.Menu_Command_Access
   is
      Index : Positive;
      Skip  : Natural;
   begin
      if Path.Length = 0 then
         Self.Player.Get_Position (Self.M3U, Index, Skip);
         --  Try to continue from saved prosition
         return Self.Create_Play_Command
           ((Length => 1, List => (1 => Index)), Skip);
      else
         return Self.Create_Play_Command (Path);
      end if;
   end Play_Command;

end Slim.Menu_Models.Play_Lists;
