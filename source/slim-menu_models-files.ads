--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Finalization;

with League.Strings;
with Slim.Players;

package Slim.Menu_Models.Files is

   type File_Menu_Model (Player : Slim.Players.Player_Access) is
     limited new Slim.Menu_Models.Menu_Model with private;

   procedure Initialize
     (Self : in out File_Menu_Model'Class;
      Root : League.Strings.Universal_String);

private

   type File_Menu_Model (Player : Slim.Players.Player_Access) is
     limited new Ada.Finalization.Limited_Controlled
       and Slim.Menu_Models.Menu_Model with
   record
      Root   : League.Strings.Universal_String;
   end record;

   overriding function Label
     (Self : File_Menu_Model;
      Path : Slim.Menu_Models.Menu_Path)
      return League.Strings.Universal_String;

   overriding function Item_Count
     (Self : File_Menu_Model;
      Path : Slim.Menu_Models.Menu_Path) return Natural;

   overriding function Enter_Command
     (Self : File_Menu_Model;
      Path : Menu_Path) return Slim.Menu_Commands.Menu_Command_Access;

end Slim.Menu_Models.Files;
