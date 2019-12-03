--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Messages.vers;
with Slim.Messages.strm;
with Slim.Messages.grfb;
with Slim.Messages.grfe;
with Slim.Messages.visu;
with Slim.Messages.bled;
with Slim.Messages.rtcs;
with Slim.Messages.bdac;
with Slim.Messages.aude;
with Slim.Messages.audg;
with Slim.Messages.Server_setd;

package body Slim.Players.Connected_State_Visiters is

   ----------
   -- HELO --
   ----------

   overriding procedure HELO
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.HELO.HELO_Message)
   is
      pragma Unreferenced (Message);
      Player : Players.Player renames Self.Player.all;
      Vers   : Slim.Messages.vers.Vers_Message;
   begin
      Vers.Set_Version (League.Strings.To_Universal_String ("7.7.3"));
      Write_Message (Player.Socket, Vers);
   end HELO;

   ----------
   -- STAT --
   ----------

   overriding procedure STAT
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.STAT.STAT_Message)
   is
      pragma Unreferenced (Message);
      use type Ada.Calendar.Time;

      Player : Players.Player renames Self.Player.all;

      Time : constant Natural := Natural
        (Ada.Calendar.Seconds (Ada.Calendar.Clock));
      Strm : Slim.Messages.strm.Strm_Message;
      Grfb : Slim.Messages.grfb.Grfb_Message;
      Grfe : Slim.Messages.grfe.Grfe_Message;
      Visu : Slim.Messages.visu.Visu_Message;
      Bled : Slim.Messages.bled.Bled_Message;
      RTC1 : Slim.Messages.rtcs.Rtcs_Message;
      RTC2 : Slim.Messages.rtcs.Rtcs_Message;
      Bdac : Slim.Messages.bdac.Bdac_Message;
      Aude : Slim.Messages.aude.Aude_Message;
      Audg : Slim.Messages.audg.Audg_Message;
      Setd : Slim.Messages.Server_setd.Setd_Message;
   begin
      Strm.Simple_Command
        (Command => Slim.Messages.strm.Stop);
      Write_Message (Player.Socket, Strm);

      --  Set dynamic brightness - minimum in range 1 .. 7
      --  21 - coefficient in range (1 .. 20)
      Grfb.Set_Brightness (16#0b02#);
      Write_Message (Player.Socket, Grfb);

      --  Send splash screen
      Grfe.Initialize (Player.Splash.To_Stream_Element_Array);
      Write_Message (Player.Socket, Grfe);

      --  deactivate visualizer
      Visu.Deactivate;
      Write_Message (Player.Socket, Visu);

      --  Enable backlight leds
      Bled.Enable_LED;
      Write_Message (Player.Socket, Bled);
      --  Set 24hours clock mode
      RTC1.Set_Format;
      Write_Message (Player.Socket, RTC1);
      --  Set clock
      RTC2.Set_Time
        (Hours   => Time / 60 / 60,
         Minutes => (Time / 60) mod 60,
         Seconds => Time mod 60);
      Write_Message (Player.Socket, RTC2);

      --  Send a DAC settings to the client.
      Bdac.Initialize
        (6,
         (16#09#, 16#00#, 16#00#, 16#02#, 16#92#, 16#00#,
          16#00#, 16#03#, 16#d4#, 16#00#, 16#00#, 16#06#,
          16#c1#, 16#00#, 16#00#, 16#0b#, 16#00#, 16#00#,
          16#00#, 16#14#, 16#00#, 16#00#, 16#00#, 16#23#,
          16#00#, 16#8f#, 16#ff#, 16#ff#, 16#ff#, 16#8f#,
          16#ff#, 16#ff#, 16#ff#, 16#8f#, 16#ff#, 16#ff#,
          16#ff#));
      Write_Message (Player.Socket, Bdac);
      --  Send a DAC settings to the client. Part 2
      Bdac.Initialize
        (7,
         (16#09#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#8f#,
          16#ff#, 16#ff#, 16#ff#, 16#8f#, 16#ff#, 16#ff#,
          16#ff#));
      Write_Message (Player.Socket, Bdac);
      --  Send a DAC settings to the client. Part 3
      Bdac.Initialize
        (4, (16#05#, 16#37#, 16#00#, 16#00#, 16#17#, 16#2e#));
      Write_Message (Player.Socket, Bdac);
      --  Send a DAC settings to the client. Part 4
      Bdac.Initialize
        (4,
         (16#11#, 16#29#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#,
          16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#));
      Write_Message (Player.Socket, Bdac);
      --  Send a DAC settings to the client. Part 5
      Bdac.Initialize
        (8, (16#00#, 16#00#, 16#26#, 16#00#));
      Write_Message (Player.Socket, Bdac);

      --  Enable the audio output.
      Aude.Enable_Output;
      Write_Message (Player.Socket, Aude);
      --  Adjust the volume level
      Audg.Set_Volume (50);
      Write_Message (Player.Socket, Audg);

      --  Ask player name
      Setd.Request_Player_Name;
      Write_Message (Player.Socket, Setd);
      Player.State := (Idle, Ada.Calendar.Clock - 60.0);
   end STAT;

end Slim.Players.Connected_State_Visiters;
