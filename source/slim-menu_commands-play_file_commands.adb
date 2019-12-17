--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Slim.Menu_Commands.Play_File_Commands is

   ---------
   -- Run --
   ---------

   overriding procedure Run (Self : Play_File_Command) is
   begin
      Self.Player.Play_File (Self.Relative_Path);
   end Run;

end Slim.Menu_Commands.Play_File_Commands;
