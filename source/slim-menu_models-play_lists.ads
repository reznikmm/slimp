--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Containers.Vectors;

with League.Strings;
with Slim.Players;

package Slim.Menu_Models.Play_Lists is

   type Play_List_Menu_Model (Player : Slim.Players.Player_Access) is
     limited new Slim.Menu_Models.Menu_Model with private;

   procedure Initialize
     (Self  : in out Play_List_Menu_Model'Class;
      Label : League.Strings.Universal_String;
      Root  : League.Strings.Universal_String;
      File  : League.Strings.Universal_String)
     with Pre => File.Starts_With (Root);
   --  Read playlist in M3U format, as described in rfc8216.
   --  Root is HTTP server folder.

private

   type Play_List_Item is record
      URI   : League.Strings.Universal_String;
      Label : League.Strings.Universal_String;
   end record;

   package Play_List_Item_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Play_List_Item);

   type Play_List_Menu_Model (Player : Slim.Players.Player_Access) is
     limited new Slim.Menu_Models.Menu_Model with
   record
      Root  : League.Strings.Universal_String;
      Label : League.Strings.Universal_String;
      Items : Play_List_Item_Vectors.Vector;
   end record;

   overriding function Label
     (Self : Play_List_Menu_Model;
      Path : Slim.Menu_Models.Menu_Path)
      return League.Strings.Universal_String;

   overriding function Item_Count
     (Self : Play_List_Menu_Model;
      Path : Slim.Menu_Models.Menu_Path) return Natural;

   overriding function Enter_Command
     (Self : Play_List_Menu_Model;
      Path : Menu_Path) return Slim.Menu_Commands.Menu_Command_Access;

end Slim.Menu_Models.Play_Lists;
