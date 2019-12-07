--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Strings;
with Slim.Menu_Commands;

package Slim.Menu_Models is

   type Menu_Path is private;

   type Menu_Model is limited interface;

   type Menu_Model_Access is access all Menu_Model'Class;

   function Root (Self : Menu_Model'Class) return Menu_Path
     with Inline;

   function Next
     (Self : Menu_Model'Class;
      Path : in out Menu_Path) return Boolean
     with Inline;

   function Previous
     (Self : Menu_Model'Class;
      Path : in out Menu_Path) return Boolean
     with Inline;

   function Parent
     (Self : Menu_Model'Class;
      Path : Menu_Path) return Menu_Path
     with Inline;

   function Child
     (Self : Menu_Model'Class;
      Path : in out Menu_Path) return Boolean
     with Inline;

   not overriding function Label
     (Self : Menu_Model;
      Path : Menu_Path) return League.Strings.Universal_String is abstract;

   not overriding function Item_Count
     (Self : Menu_Model;
      Path : Menu_Path) return Natural is abstract;

   not overriding function Enter_Command
     (Self : Menu_Model;
      Path : Menu_Path) return Slim.Menu_Commands.Menu_Command_Access
        is abstract;

private

   type Menu_Index_Array is array (Positive range <>) of Positive;

   subtype Menu_Depth is Natural range 0 .. 5;

   type Menu_Path (Length : Menu_Depth := 0) is record
      List : Menu_Index_Array (1 .. Length);
   end record;

end Slim.Menu_Models;
