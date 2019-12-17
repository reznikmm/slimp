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
      Value : Slim.Players.Song_Array (1 .. Self.Title_List.Length);
   begin
      for J in Value'Range loop
         Value (J) := (Self.Relative_Path_List (J), Self.Title_List (J));
      end loop;

      Self.Player.Play_Files (Value);
   end Run;

end Slim.Menu_Commands.Play_File_Commands;
