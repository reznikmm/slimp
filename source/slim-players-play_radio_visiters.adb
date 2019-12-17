--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Calendar;
with Ada.Text_IO;

with League.String_Vectors;

with Slim.Messages.cont;

--  with Slim.Players.Displays;

package body Slim.Players.Play_Radio_Visiters is

   ----------
   -- DSCO --
   ----------

   overriding procedure DSCO
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.DSCO.DSCO_Message)
   is
      pragma Unreferenced (Message);

      use type Ada.Calendar.Time;

      Player : Players.Player renames Self.Player.all;
   begin
      --  got disconnection on the data channel
      Player.State := (Idle, Ada.Calendar.Clock - 60.0, Player.First_Menu);
   end DSCO;

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

      Player.State.Play_State.Current_Song := Text;
      Slim.Players.Common_Play_Visiters.Update_Display (Player);
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

      Cont.Set_Metaint (Metaint);
      Write_Message (Player.Socket, Cont);
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
      elsif not Player.State.Play_State.Current_Song.Is_Empty then
         null;
      elsif Message.Event = "STMc" then
         Player.State.Play_State.Current_Song.Clear;
         Player.State.Play_State.Current_Song.Append ("Connecting...");
      elsif Message.Event = "STMe" then
         Player.State.Play_State.Current_Song.Clear;
         Player.State.Play_State.Current_Song.Append ("Connected");
      end if;

      Slim.Players.Common_Play_Visiters.Update_Display (Player);

      if Message.Event /= "STMt" then
         Ada.Text_IO.Put_Line (Message.Event);
      end if;
   end STAT;

end Slim.Players.Play_Radio_Visiters;
