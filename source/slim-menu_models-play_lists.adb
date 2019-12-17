--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with League.String_Vectors;

with Slim.Menu_Commands.Play_File_Commands;

package body Slim.Menu_Models.Play_Lists is

   -------------------
   -- Enter_Command --
   -------------------

   overriding function Enter_Command
     (Self : Play_List_Menu_Model;
      Path : Menu_Path) return Slim.Menu_Commands.Menu_Command_Access
   is
      use type League.Strings.Universal_String;
      use Slim.Menu_Commands.Play_File_Commands;

      Result : constant Play_File_Command_Access :=
        new Play_File_Command (Self.Player);

      Index : Positive := Path.List (1);
   begin
      for J in 1 .. Self.Items.Last_Index loop
         declare
            Item : constant Play_List_Item := Self.Items (Index);
         begin
            Result.Relative_Path_List.Append (Self.Root & Item.URI);
            Result.Title_List.Append (Item.Label);

            Index := Index + 1;

            if Index > Self.Items.Last_Index then
               Index := 1;
            end if;
         end;
      end loop;

      return Slim.Menu_Commands.Menu_Command_Access (Result);
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
      Self.Root := File.Tail_From (Root.Length + 1);
      Self.Root := Self.Root.Head (Self.Root.Last_Index ('/'));

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
         Ada.Wide_Wide_Text_IO.Put_Line (Self.Label.To_Wide_Wide_String);
         return Self.Label;
      else
         Ada.Wide_Wide_Text_IO.Put_Line
           (Self.Items (Path.List (1)).Label.To_Wide_Wide_String);
         return Self.Items (Path.List (1)).Label;
      end if;
   end Label;

end Slim.Menu_Models.Play_Lists;
