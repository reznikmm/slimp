--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Players;
with League.String_Vectors;

package Slim.Menu_Commands.Play_File_Commands is

   type Play_File_Command
     (Player : Slim.Players.Player_Access) is new Menu_Command with
   record
      Relative_Path_List : League.String_Vectors.Universal_String_Vector;
      Title_List         : League.String_Vectors.Universal_String_Vector;
   end record;

   type Play_File_Command_Access is access all Play_File_Command'Class;

   overriding procedure Run (Self : Play_File_Command);

end Slim.Menu_Commands.Play_File_Commands;
