--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

package body Slim.Menu_Models is

   -----------
   -- Child --
   -----------

   function Child
     (Self : Menu_Model'Class;
      Path : in out Menu_Path) return Boolean is
   begin
      if Self.Item_Count (Path) > 0 then
         Path := (Path.Length + 1, Path.List & 1);
         return True;
      else
         return False;
      end if;
   end Child;

   ----------
   -- Next --
   ----------

   function Next
     (Self : Menu_Model'Class; Path : in out Menu_Path) return Boolean is
   begin
      if Path.Length > 0
        and then Path.List (Path.Length) < Self.Item_Count (Self.Parent (Path))
      then
         Path.List (Path.Length) := Path.List (Path.Length) + 1;
         return True;
      else
         return False;
      end if;
   end Next;

   ------------
   -- Parent --
   ------------

   function Parent
     (Self : Menu_Model'Class; Path : Menu_Path) return Menu_Path
   is
      pragma Unreferenced (Self);
   begin
      if Path.Length = 0 then
         return Path;
      else
         return (Path.Length - 1, Path.List (1 .. Path.Length - 1));
      end if;
   end Parent;

   --------------
   -- Previous --
   --------------

   function Previous
     (Self : Menu_Model'Class;
      Path : in out Menu_Path) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      if Path.Length > 0 and then Path.List (Path.Length) > 1 then
         Path.List (Path.Length) := Path.List (Path.Length) - 1;
         return True;
      else
         return False;
      end if;
   end Previous;

   ----------
   -- Root --
   ----------

   function Root (Self : Menu_Model'Class) return Menu_Path is
      pragma Unreferenced (Self);
   begin
      return (Length => 0, List => <>);
   end Root;

end Slim.Menu_Models;
