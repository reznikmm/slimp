--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Text_IO;

with League.String_Vectors;

with Slim.Messages.audg;
with Slim.Messages.cont;
with Slim.Messages.strm;

with Slim.Players.Displays;

package body Slim.Players.Play_State_Visiters is

   procedure Update_Display (Self : Player);

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
         when Slim.Messages.BUTN.Knob_Left =>
            declare
               Audg   : Slim.Messages.audg.Audg_Message;
            begin
               Player.State.Volume_Set_Time := Ada.Calendar.Clock;
               Player.State.Volume := Natural'Max (0, Player.State.Volume - 5);
               Audg.Set_Volume (Player.State.Volume);
               Write_Message (Player.Socket, Audg);
               Update_Display (Player);
            end;

         when Slim.Messages.BUTN.Knob_Right =>
            declare
               Audg   : Slim.Messages.audg.Audg_Message;
            begin
               Player.State.Volume_Set_Time := Ada.Calendar.Clock;
               Player.State.Volume :=
                 Natural'Min (100, Player.State.Volume + 5);
               Audg.Set_Volume (Player.State.Volume);
               Write_Message (Player.Socket, Audg);
               Update_Display (Player);
            end;

         when Slim.Messages.BUTN.Pause =>
            declare
               Map : constant array (Boolean)
                 of Slim.Messages.strm.Play_Command :=
                   (False => Slim.Messages.strm.Pause,
                    True  => Slim.Messages.strm.Unpause);
               Strm    : Slim.Messages.strm.Strm_Message;
            begin
               Strm.Simple_Command (Map (Player.State.Paused));
               Write_Message (Player.Socket, Strm);
               Player.State.Paused := not Player.State.Paused;
            end;
         when others =>
            null;
      end case;
   end BUTN;

   ----------
   -- META --
   ----------

   overriding procedure META
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.META.META_Message)
   is
      Player : Players.Player renames Self.Player.all;
      Text   : League.Strings.Universal_String := Message.Value;
      Prefix : constant Wide_Wide_String := "StreamTitle='";
      Suffix : constant Wide_Wide_String := "';";
   begin
      if Text.Starts_With (Prefix) then
         Text := Text.Tail_From (Prefix'Length + 1);
      end if;

      if Text.Ends_With (Suffix) then
         Text := Text.Head_To (Text.Length - Suffix'Length);
      end if;

      Player.State.Song := Text;
      Update_Display (Player);
   end META;

   ----------
   -- RESP --
   ----------

   overriding procedure RESP
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.RESP.RESP_Message)
   is
      List : constant League.String_Vectors.Universal_String_Vector :=
        Message.Headers;

      Player         : Players.Player renames Self.Player.all;
      Line           : League.Strings.Universal_String;
      Metaint_Header : constant Wide_Wide_String := "icy-metaint:";
      Metaint        : Natural := 0;
      Cont           : Slim.Messages.cont.Cont_Message;
   begin
      for J in 1 .. List.Length loop
         Line := List.Element (J);

         if Line.Starts_With (Metaint_Header) then
            Line := Line.Tail_From (Metaint_Header'Length + 1);
            begin
               Metaint :=
                 Natural'Wide_Wide_Value (Line.To_Wide_Wide_String);
            exception
               when Constraint_Error =>
                  null;
            end;
            exit;
         end if;
      end loop;

      if Metaint /= 0 then
         Cont.Set_Metaint (Metaint);
         Write_Message (Player.Socket, Cont);
      end if;
   end RESP;

   ----------
   -- STAT --
   ----------

   overriding procedure STAT
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.STAT.STAT_Message)
   is
      Player : Players.Player renames Self.Player.all;
   begin
      Player.WiFi := Message.WiFi_Level;

      if Message.Event (1 .. 3) /= "STM" then
         return;
      elsif Message.Event = "STMc" then
         Player.State.Song.Clear;
         Player.State.Song.Append ("Connecting...");
      elsif Message.Event = "STMe" then
         Player.State.Song.Clear;
         Player.State.Song.Append ("Connected");
      end if;

      Update_Display (Player);

      if Message.Event /= "STMt" then
         Ada.Text_IO.Put_Line (Message.Event);
      end if;
   end STAT;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display (Self : Player) is
      use type Ada.Calendar.Time;

      Display : Slim.Players.Displays.Display := Self.Get_Display;
      Time    : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Text    : League.Strings.Universal_String;
      Song    : constant League.Strings.Universal_String := Self.State.Song;
      Volume  : constant Wide_Wide_String :=
        Natural'Wide_Wide_Image (Self.State.Volume);

   begin
      Slim.Players.Displays.Clear (Display);

      if Time - Self.State.Volume_Set_Time < 3.0 or Song.Is_Empty then
         Text.Append ("Volume:");
         Text.Append (Volume);
         Text.Append ("%");

         Slim.Players.Displays.Draw_Text
           (Self => Display,
            X    => 1,
            Y    => 6,
            Font => Self.Font,
            Text => Text);
      elsif Self.State.Paused then

         Text.Append ("Pause");

         Slim.Players.Displays.Draw_Text
           (Self => Display,
            X    => 1,
            Y    => 2 - Slim.Fonts.Size (Self.Font, Song).Bottom,
            Font => Self.Font,
            Text => Text);
      end if;

      Slim.Players.Displays.Draw_Text
        (Self => Display,
         X    => 1,
         Y    => 32 - Slim.Fonts.Size (Self.Font, Song).Top,
         Font => Self.Font,
         Text => Song);

      Slim.Players.Displays.Send_Message (Display);
   end Update_Display;

end Slim.Players.Play_State_Visiters;
