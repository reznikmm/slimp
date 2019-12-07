--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Fonts;
with Slim.Menu_Models;
limited with Slim.Players.Displays;

package Slim.Menu_Views is

   type Menu_View is tagged private;

   procedure Initialize
     (Self : in out Menu_View'Class;
      Menu : Slim.Menu_Models.Menu_Model_Access;
      Font : Slim.Fonts.Font_Access);

   procedure Up (Self : in out Menu_View'Class);
   procedure Down (Self : in out Menu_View'Class);
   procedure Enter (Self : in out Menu_View'Class);
   procedure Back (Self : in out Menu_View'Class);
   procedure Play (Self : in out Menu_View'Class);

   procedure Draw
     (Self    : in out Menu_View'Class;
      Display : in out Slim.Players.Displays.Display);

private

   type Menu_View is tagged record
      Font         : Slim.Fonts.Font_Access;
      Menu         : Slim.Menu_Models.Menu_Model_Access;
      Current_Menu : Slim.Menu_Models.Menu_Path;
   end record;

end Slim.Menu_Views;
