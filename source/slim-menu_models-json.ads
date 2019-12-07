--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Strings;
with League.JSON.Objects;
with Slim.Players;

package Slim.Menu_Models.JSON is

   type JSON_Menu_Model (Player : Slim.Players.Player_Access) is
     new Slim.Menu_Models.Menu_Model with private;

   procedure Initialize
     (Self : in out JSON_Menu_Model'Class;
      File : League.Strings.Universal_String);

private

   type JSON_Menu_Model (Player : Slim.Players.Player_Access) is
     new Slim.Menu_Models.Menu_Model with
   record
      Root   : League.JSON.Objects.JSON_Object;
      Nested : League.Strings.Universal_String;
      Label  : League.Strings.Universal_String;
      URL    : League.Strings.Universal_String;
   end record;

   overriding function Label
     (Self : JSON_Menu_Model;
      Path : Slim.Menu_Models.Menu_Path)
      return League.Strings.Universal_String;

   overriding function Item_Count
     (Self : JSON_Menu_Model;
      Path : Slim.Menu_Models.Menu_Path) return Natural;

   overriding function Enter_Command
     (Self : JSON_Menu_Model;
      Path : Menu_Path) return Slim.Menu_Commands.Menu_Command_Access;

end Slim.Menu_Models.JSON;
