--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;

with League.String_Vectors;

with Slim.Players.Displays;
with Slim.Messages.strm;

package body Slim.Players.Idle_State_Visiters is

   function "+" (X : Wide_Wide_String) return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   procedure Update_Display (Self : Player);

   ----------
   -- BUTN --
   ----------

   overriding procedure BUTN
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.BUTN.BUTN_Message)
   is
      use type Ada.Calendar.Time;

      Player : Players.Player renames Self.Player.all;
      Button : constant Slim.Messages.BUTN.Button_Kind := Message.Button;
   begin
      case Button is
         when Slim.Messages.BUTN.Preset_1 =>
            declare
               Strm    : Slim.Messages.strm.Strm_Message;
               Request : League.String_Vectors.Universal_String_Vector;
            begin
               Request.Append (+"GET /stream-128kmp3-YogaChill HTTP/1.0");
               Request.Append (+"Host: 178.32.111.41:8027");
               Request.Append (+"Icy-Metadata: 1");
               Request.Append (+"");
               Request.Append (+"");
               Strm.Start
                 (Server_IP   => (178, 32, 111, 41),
                  Server_Port => 8027,
                  Request     => Request);
               Write_Message (Player.Socket, Strm);

               Player.State :=
                 (Play,
                  Volume          => 30,
                  Volume_Set_Time => Ada.Calendar.Clock - 60.0,
                  Song            => League.Strings.Empty_Universal_String,
                  Paused          => False);
            end;
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

   procedure Update_Display (Self : Player) is
      Display : Slim.Players.Displays.Display := Self.Get_Display;
      Seconds : constant Natural :=
        Natural (Ada.Calendar.Seconds (Self.State.Time));
      Hour    : constant Natural := Seconds / 3600;
      Min     : constant Natural := (Seconds / 60) mod 60;

      H : Wide_Wide_String := Natural'Wide_Wide_Image (Hour);
      M : Wide_Wide_String := Natural'Wide_Wide_Image (Min);

      Text    : League.Strings.Universal_String;
   begin
      H (1) := '0';
      M (1) := '0';
      Text.Append (H (H'Last - 1 .. H'Last));
      Text.Append (":");
      Text.Append (M (M'Last - 1 .. M'Last));

      Slim.Players.Displays.Clear (Display);

      Slim.Players.Displays.Draw_Text
        (Self => Display,
         X    => 1,
         Y    => 6,
         Font => Self.Font,
         Text => Text);

      Slim.Players.Displays.Send_Message (Display);
   end Update_Display;

end Slim.Players.Idle_State_Visiters;
