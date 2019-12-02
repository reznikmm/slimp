--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;
with Slim.Messages.BUTN;
with Slim.Messages.SETD;
with Slim.Messages.STAT;

package Slim.Players.Idle_State_Visiters is

   type Visiter (Player : not null access Players.Player) is
     new Slim.Message_Visiters.Visiter with null record;

   overriding procedure BUTN
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.BUTN.BUTN_Message);

   overriding procedure SETD
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.SETD.SETD_Message);

   overriding procedure STAT
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.STAT.STAT_Message);

end Slim.Players.Idle_State_Visiters;
