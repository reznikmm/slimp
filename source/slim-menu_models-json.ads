--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Hashed_Maps;

with League.Strings.Hash;
with League.JSON.Objects;
with Slim.Players;
with Slim.Menu_Models.Files;
with Slim.Menu_Models.Play_Lists;

package Slim.Menu_Models.JSON is

   type JSON_Menu_Model (Player : Slim.Players.Player_Access) is
     limited new Slim.Menu_Models.Menu_Model with private;

   procedure Initialize
     (Self : in out JSON_Menu_Model'Class;
      File : League.Strings.Universal_String);

private

   type Play_List_Access is access all
     Slim.Menu_Models.Play_Lists.Play_List_Menu_Model;

   package Playlist_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => Play_List_Access,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=");

   type JSON_Menu_Model (Player : Slim.Players.Player_Access) is
     limited new Slim.Menu_Models.Menu_Model with
   record
      Root     : League.JSON.Objects.JSON_Object;
      Nested   : League.Strings.Universal_String;
      Label    : League.Strings.Universal_String;
      URL      : League.Strings.Universal_String;
      Path     : League.Strings.Universal_String;
      Playlist : League.Strings.Universal_String;

      File_Path : Menu_Path;
      Playlists : Playlist_Maps.Map;
      File_Menu : Slim.Menu_Models.Files.File_Menu_Model (Player);
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
