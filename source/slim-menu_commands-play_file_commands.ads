--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Players;
with League.Strings;

package Slim.Menu_Commands.Play_File_Commands is

   type Play_File_Command
     (Player : Slim.Players.Player_Access) is new Menu_Command with
   record
      Relative_Path : League.Strings.Universal_String;
   end record;

   overriding procedure Run (Self : Play_File_Command);

end Slim.Menu_Commands.Play_File_Commands;
