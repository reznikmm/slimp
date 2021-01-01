--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar;

with Slim.Messages.strm;

with Slim.Players.Displays;

package body Slim.Players.Common_Play_Visiters is

   function Image (Seconds : Natural) return League.Strings.Universal_String;

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
         when Slim.Messages.BUTN.Knob_Left | Slim.Messages.BUTN.Volume_Down =>
            Player.Volume
              (Natural'Max (0, Player.State.Play_State.Volume - 5));
            Update_Display (Player);

         when Slim.Messages.BUTN.Knob_Right | Slim.Messages.BUTN.Volume_Up =>
            Player.Volume
              (Natural'Min (100, Player.State.Play_State.Volume + 5));
            Update_Display (Player);

         when Slim.Messages.BUTN.Pause =>
            declare
               Map : constant array (Boolean)
                 of Slim.Messages.strm.Play_Command :=
                   (False => Slim.Messages.strm.Pause,
                    True  => Slim.Messages.strm.Unpause);
               Strm    : Slim.Messages.strm.Strm_Message;
            begin
               Strm.Simple_Command (Map (Player.State.Play_State.Paused));
               Write_Message (Player.Socket, Strm);
               Player.State.Play_State.Paused :=
                 not Player.State.Play_State.Paused;
            end;

         when Slim.Messages.BUTN.Back =>
            Player.Stop;

         when others =>
            null;
      end case;
   end BUTN;

   -----------
   -- Image --
   -----------

   function Image (Seconds : Natural) return League.Strings.Universal_String is
      Min : constant Wide_Wide_String :=
        Natural'Wide_Wide_Image (Seconds / 60);
      Sec : constant Wide_Wide_String :=
        Natural'Wide_Wide_Image (Seconds mod 60);
      Result : League.Strings.Universal_String;
   begin
      Result.Append (Min (2 .. Min'Last));
      Result.Append (":");

      if Sec'Length <= 2 then
         Result.Append ("0");
      end if;

      Result.Append (Sec (2 .. Sec'Last));
      return Result;
   end Image;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display (Self : in out Player) is
      use type Ada.Calendar.Time;

      Display : Slim.Players.Displays.Display := Self.Get_Display;
      State   : Player_State renames Self.State;
      Time    : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Text    : League.Strings.Universal_String;
      Song    : League.Strings.Universal_String;
      Seconds : Natural;

      function Volume return Wide_Wide_String is
        (Natural'Wide_Wide_Image (State.Play_State.Volume));

   begin
      if State.Kind not in Play_Radio | Play_Files then
         return;
      end if;

      Song := State.Play_State.Current_Song;
      Slim.Players.Displays.Clear (Display);

      if Time - State.Play_State.Volume_Set_Time < 3.0
        or Song.Is_Empty
      then
         Text.Append ("Volume:");
         Text.Append (Volume);
         Text.Append ("%");

         Slim.Players.Displays.Draw_Text
           (Self => Display,
            X    => 1,
            Y    => 6,
            Font => Self.Font,
            Text => Text);
      elsif State.Play_State.Paused then

         Text.Append ("Pause");

         Slim.Players.Displays.Draw_Text
           (Self => Display,
            X    => 1,
            Y    => 2 - Slim.Fonts.Size (Self.Font, Song).Bottom,
            Font => Self.Font,
            Text => Text);
      elsif State.Kind = Play_Files then
         Seconds := State.Play_State.Seconds + State.Offset;

         Text.Append (Image (Seconds));

         Slim.Players.Displays.Draw_Text
           (Self => Display,
            X    => 158 - Slim.Fonts.Size (Self.Font, Text).Right,
            Y    => 6,
            Font => Self.Font,
            Text => Text);
      end if;

      Slim.Players.Displays.Draw_Text
        (Self => Display,
         X    => 1,
         Y    => 33 - Slim.Fonts.Size (Self.Font, Song).Top,
         Font => Self.Font,
         Text => Song);

      Slim.Players.Displays.Send_Message (Display);
   end Update_Display;

end Slim.Players.Common_Play_Visiters;
