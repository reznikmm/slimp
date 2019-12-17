--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Messages.RESP;
with Slim.Messages.STAT;

with Slim.Players.Common_Play_Visiters;

package Slim.Players.Play_Files_Visiters is

   type Visiter (Player : not null access Players.Player) is
     new Slim.Players.Common_Play_Visiters.Visiter (Player) with null record;

   overriding procedure RESP
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.RESP.RESP_Message);

   overriding procedure STAT
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.STAT.STAT_Message);

end Slim.Players.Play_Files_Visiters;
