--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Slim.Messages.ANIC;
with Slim.Messages.HELO;
with Slim.Messages.IR;
with Slim.Messages.RESP;
with Slim.Messages.SETD;
with Slim.Messages.STAT;

with Slim.Messages.aude;
with Slim.Messages.audg;
with Slim.Messages.audp;
with Slim.Messages.bdac;
with Slim.Messages.bled;
with Slim.Messages.grfb;
with Slim.Messages.grfe;
with Slim.Messages.grfg;
with Slim.Messages.grfs;
with Slim.Messages.rtcs;
with Slim.Messages.Server_setd;
with Slim.Messages.strm;
with Slim.Messages.vers;
with Slim.Messages.visu;

package Slim.Message_Visiters is

   type Visiter is limited interface;

   not overriding procedure ANIC
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.ANIC.ANIC_Message) is null;

   not overriding procedure HELO
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.HELO.HELO_Message) is null;

   not overriding procedure IR
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.IR.IR_Message) is null;

   not overriding procedure RESP
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.RESP.RESP_Message) is null;

   not overriding procedure SETD
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.SETD.SETD_Message) is null;

   not overriding procedure STAT
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.STAT.STAT_Message) is null;

   not overriding procedure aude
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.aude.Aude_Message) is null;

   not overriding procedure audg
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.audg.Audg_Message) is null;

   not overriding procedure audp
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.audp.Audp_Message) is null;

   not overriding procedure bdac
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.bdac.Bdac_Message) is null;

   not overriding procedure bled
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.bled.Bled_Message) is null;

   not overriding procedure grfb
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.grfb.Grfb_Message) is null;

   not overriding procedure grfe
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.grfe.Grfe_Message) is null;

   not overriding procedure grfg
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.grfg.Grfg_Message) is null;

   not overriding procedure grfs
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.grfs.Grfs_Message) is null;

   not overriding procedure rtcs
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.rtcs.Rtcs_Message) is null;

   not overriding procedure setd
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.Server_setd.Setd_Message)
        is null;

   not overriding procedure strm
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.strm.Strm_Message) is null;

   not overriding procedure vers
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.vers.Vers_Message) is null;

   not overriding procedure visu
     (Self    : in out Visiter;
      Message : not null access Slim.Messages.visu.Visu_Message) is null;

end Slim.Message_Visiters;
