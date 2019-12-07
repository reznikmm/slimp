--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Slim.Menu_Commands.Play_Radio_Commands is

   ---------
   -- Run --
   ---------

   overriding procedure Run (Self : Play_Radio_Command) is
   begin
      Self.Player.Play_Radio (Self.URL);
   end Run;

end Slim.Menu_Commands.Play_Radio_Commands;
