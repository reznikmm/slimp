--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Message_Visiters;

with Slim.Messages.BUTN;

package Slim.Players.Common_Play_Visiters is

   type Visiter (Player : not null access Players.Player) is
     abstract new Slim.Message_Visiters.Visiter with null record;

   overriding procedure BUTN
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.BUTN.BUTN_Message);

   procedure Update_Display (Self : in out Player);
end Slim.Players.Common_Play_Visiters;
