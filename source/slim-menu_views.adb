--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with League.Strings;

with Slim.Menu_Commands;
with Slim.Players.Displays;

package body Slim.Menu_Views is

   ----------
   -- Back --
   ----------

   procedure Back (Self : in out Menu_View'Class) is
      use type Slim.Menu_Models.Menu_Path;

      Ignore : Boolean;
   begin
      Self.Current_Menu := Self.Menu.Parent (Self.Current_Menu);

      if Self.Current_Menu = Self.Menu.Root then
         Self.Current_Menu := Self.Menu.Root;
         Ignore := Self.Menu.Child (Self.Current_Menu);
      end if;
   end Back;

   ----------
   -- Down --
   ----------

   procedure Down (Self : in out Menu_View'Class) is
      Ignore : Boolean;
   begin
      Ignore := Self.Menu.Next (Self.Current_Menu);
   end Down;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Self    : in out Menu_View'Class;
      Display : in out Slim.Players.Displays.Display)
   is
      Text : constant League.Strings.Universal_String :=
        Self.Menu.Label (Self.Current_Menu);
   begin
      Slim.Players.Displays.Draw_Text
        (Self => Display,
         X    => 1,
         Y    => 2 - Slim.Fonts.Size (Self.Font.all, Text).Bottom,
         Font => Self.Font.all,
         Text => Text);
   end Draw;

   -----------
   -- Enter --
   -----------

   procedure Enter (Self : in out Menu_View'Class) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Slim.Menu_Commands.Menu_Command'Class,
         Slim.Menu_Commands.Menu_Command_Access);

      Command : Slim.Menu_Commands.Menu_Command_Access;
   begin
      if not Self.Menu.Child (Self.Current_Menu) then
         Command := Self.Menu.Enter_Command (Self.Current_Menu);

         if Command not in null then
            Command.Run;
            Free (Command);
         end if;
      end if;
   end Enter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Menu_View'Class;
      Menu : Slim.Menu_Models.Menu_Model_Access;
      Font : Slim.Fonts.Font_Access)
   is
      Ignore : Boolean;
   begin
      Self.Font := Font;
      Self.Menu := Menu;
      Self.Current_Menu := Menu.Root;
      Ignore := Menu.Child (Self.Current_Menu);
   end Initialize;

   ----------
   -- Play --
   ----------

   procedure Play (Self : in out Menu_View'Class) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Slim.Menu_Commands.Menu_Command'Class,
         Slim.Menu_Commands.Menu_Command_Access);

      Command : Slim.Menu_Commands.Menu_Command_Access;
   begin
      Command := Self.Menu.Play_Command (Self.Current_Menu);

      if Command not in null then
         Command.Run;
         Free (Command);
      end if;
   end Play;

   --------
   -- Up --
   --------

   procedure Up (Self : in out Menu_View'Class) is
      Ignore : Boolean;
   begin
      Ignore := Self.Menu.Previous (Self.Current_Menu);
   end Up;

end Slim.Menu_Views;
