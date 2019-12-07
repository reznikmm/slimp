--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package Slim.Menu_Commands is

   type Menu_Command is limited interface;
   type Menu_Command_Access is access all Menu_Command'Class;

   not overriding procedure Run (Self : Menu_Command) is abstract;

end Slim.Menu_Commands;
