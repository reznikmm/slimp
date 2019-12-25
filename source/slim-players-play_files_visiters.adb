--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Text_IO;

with Slim.Messages.cont;

package body Slim.Players.Play_Files_Visiters is

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
         when Slim.Messages.BUTN.Forward =>
            Player.Play_Next_File;
         when Slim.Messages.BUTN.Rewind =>
            Player.Play_Previous_File;
         when others =>
            Slim.Players.Common_Play_Visiters.Visiter (Self).BUTN (Message);
      end case;
   end BUTN;

   ----------
   -- RESP --
   ----------

   overriding procedure RESP
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.RESP.RESP_Message)
   is
      pragma Unreferenced (Message);
      Player : Players.Player renames Self.Player.all;

      Cont : Slim.Messages.cont.Cont_Message;
   begin
      Cont.Set_Metaint (0);
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
      elsif Message.Event = "STMd" then
         --  Decoder ready. Instruct server that we are ready for the next
         --  track (if any).
         Player.Play_Next_File (Immediate => False);
      elsif Message.Event = "STMu" then
         --  Underrun. Normal end of playback.
         Player.Stop;
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

end Slim.Players.Play_Files_Visiters;
