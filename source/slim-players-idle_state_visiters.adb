--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;


with Slim.Players.Displays;

package body Slim.Players.Idle_State_Visiters is

   procedure Update_Display (Self : in out Player);

   ----------
   -- BUTN --
   ----------

   overriding procedure BUTN
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.BUTN.BUTN_Message)
   is
      Player : Players.Player renames Self.Player.all;
      Button : constant Slim.Messages.BUTN.Button_Kind := Message.Button;
   begin
      case Button is
         when Slim.Messages.BUTN.Back =>
            Player.State.Menu_View.Back;
            Update_Display (Player);

         when Slim.Messages.BUTN.Knob_Left =>
            Player.State.Menu_View.Up;
            Update_Display (Player);

         when Slim.Messages.BUTN.Knob_Right =>
            Player.State.Menu_View.Down;
            Update_Display (Player);

         when Slim.Messages.BUTN.Knob_Push =>
            Player.State.Menu_View.Enter;
            Update_Display (Player);

         when Slim.Messages.BUTN.Preset_1 =>
            Player.Play_Radio (+"http://files.yogaradio.fm:8000/YOGAr256");
         when others =>
            null;
      end case;
   end BUTN;

   ----------
   -- SETD --
   ----------

   overriding procedure SETD
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.SETD.SETD_Message)
   is
      Player : Players.Player renames Self.Player.all;
      Value : constant Slim.Messages.SETD.Setting := Message.Get_Setting;
   begin
      case Value.Kind is
         when Slim.Messages.SETD.Player_Name =>
            declare
               Display : Slim.Players.Displays.Display := Player.Get_Display;
            begin
               Ada.Wide_Wide_Text_IO.Put_Line
                 ("Player:" & Value.Player.To_Wide_Wide_String);

               Slim.Players.Displays.Clear (Display);

               Slim.Players.Displays.Draw_Text
                 (Self => Display,
                  X    => 1,
                  Y    => 5,
                  Font => Player.Font,
                  Text => Value.Player);

               Slim.Players.Displays.Send_Message (Display);
            end;
         when others =>
            null;
      end case;
   end SETD;

   ----------
   -- STAT --
   ----------

   overriding procedure STAT
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.STAT.STAT_Message)
   is
      Player : Players.Player renames Self.Player.all;
      Time   : Ada.Calendar.Time;
   begin
      Player.WiFi := Message.WiFi_Level;

      if Message.Event = "STMt" then  --  Reply to Hearbeat
         Time := Ada.Calendar.Clock;

         if Ada.Calendar.Seconds (Player.State.Time) / 60
           /= Ada.Calendar.Seconds (Time) / 60
         then
            Player.State.Time := Time;
            Update_Display (Player);
         end if;
      end if;
   end STAT;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display (Self : in out Player) is
      Display : Slim.Players.Displays.Display := Self.Get_Display;
      Seconds : constant Natural :=
        Natural (Ada.Calendar.Seconds (Self.State.Time));
      Hour    : constant Natural := Seconds / 3600;
      Min     : constant Natural := (Seconds / 60) mod 60;

      H : Wide_Wide_String := Natural'Wide_Wide_Image (Hour);
      M : Wide_Wide_String := Natural'Wide_Wide_Image (Min);

      Text : League.Strings.Universal_String;
      Box  : Slim.Fonts.Bounding_Box;
   begin
      H (1) := '0';
      M (1) := '0';
      Text.Append (H (H'Last - 1 .. H'Last));
      Text.Append (":");
      Text.Append (M (M'Last - 1 .. M'Last));
      Box := Slim.Fonts.Size (Self.Font, Text);

      Slim.Players.Displays.Clear (Display);

      Slim.Players.Displays.Draw_Text
        (Self => Display,
         X    => 161 - Box.Right,
         Y    => 33 - Box.Top,
         Font => Self.Font,
         Text => Text);

      Self.State.Menu_View.Draw (Display);

      Slim.Players.Displays.Send_Message (Display);
   end Update_Display;

end Slim.Players.Idle_State_Visiters;
