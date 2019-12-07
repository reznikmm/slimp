--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Players;
with League.Strings;

package Slim.Menu_Commands.Play_Radio_Commands is

   type Play_Radio_Command
     (Player : Slim.Players.Player_Access) is new Menu_Command with
   record
      URL : League.Strings.Universal_String;
   end record;

   overriding procedure Run (Self : Play_Radio_Command);

end Slim.Menu_Commands.Play_Radio_Commands;
