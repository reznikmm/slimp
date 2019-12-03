--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

with Slim.Messages.BUTN;
with Slim.Messages.DSCO;
with Slim.Messages.META;
with Slim.Messages.RESP;

with Slim.Messages.STAT;

package Slim.Players.Play_State_Visiters is

   type Visiter (Player : not null access Players.Player) is
     new Slim.Message_Visiters.Visiter with null record;

   overriding procedure BUTN
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.BUTN.BUTN_Message);

   overriding procedure DSCO
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.DSCO.DSCO_Message);

   overriding procedure META
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.META.META_Message);

   overriding procedure RESP
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.RESP.RESP_Message);

   overriding procedure STAT
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.STAT.STAT_Message);

end Slim.Players.Play_State_Visiters;
