--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Tags.Generic_Dispatching_Constructor;

with Slim.Messages.ANIC;
with Slim.Messages.BUTN;
with Slim.Messages.HELO;
with Slim.Messages.IR;
with Slim.Messages.META;
with Slim.Messages.RESP;
with Slim.Messages.SETD;
with Slim.Messages.STAT;
with Slim.Messages.aude;
with Slim.Messages.audg;
with Slim.Messages.audp;
with Slim.Messages.bdac;
with Slim.Messages.bled;
with Slim.Messages.cont;
with Slim.Messages.grfb;
with Slim.Messages.grfe;
with Slim.Messages.grfg;
with Slim.Messages.grfs;
with Slim.Messages.rtcs;
with Slim.Messages.Server_setd;
with Slim.Messages.strm;
with Slim.Messages.vers;
with Slim.Messages.visu;

package body Slim.Message_Decoders is

   type Map_Item is record
      Label : Slim.Messages.Message_Tag;
      Tag   : Ada.Tags.Tag;
   end record;

   Map : constant array (Positive range <>) of Map_Item :=
     (("ANIC", Slim.Messages.ANIC.ANIC_Message'Tag),
      ("BUTN", Slim.Messages.BUTN.BUTN_Message'Tag),
      ("HELO", Slim.Messages.HELO.HELO_Message'Tag),
      ("IR  ", Slim.Messages.IR.IR_Message'Tag),
      ("META", Slim.Messages.META.META_Message'Tag),
      ("RESP", Slim.Messages.RESP.RESP_Message'Tag),
      ("SETD", Slim.Messages.SETD.SETD_Message'Tag),
      ("STAT", Slim.Messages.STAT.STAT_Message'Tag),
      ("aude", Slim.Messages.aude.Aude_Message'Tag),
      ("audg", Slim.Messages.audg.Audg_Message'Tag),
      ("audp", Slim.Messages.audp.Audp_Message'Tag),
      ("bdac", Slim.Messages.bdac.Bdac_Message'Tag),
      ("bled", Slim.Messages.bled.Bled_Message'Tag),
      ("cont", Slim.Messages.cont.Cont_Message'Tag),
      ("grfb", Slim.Messages.grfb.Grfb_Message'Tag),
      ("grfe", Slim.Messages.grfe.Grfe_Message'Tag),
      ("grfg", Slim.Messages.grfg.Grfg_Message'Tag),
      ("grfs", Slim.Messages.grfs.Grfs_Message'Tag),
      ("rtcs", Slim.Messages.rtcs.Rtcs_Message'Tag),
      ("setd", Slim.Messages.Server_setd.Setd_Message'Tag),
      ("strm", Slim.Messages.strm.Strm_Message'Tag),
      ("vers", Slim.Messages.vers.Vers_Message'Tag),
      ("visu", Slim.Messages.visu.Visu_Message'Tag));

   ------------
   -- Decode --
   ------------

   not overriding procedure Decode
     (Self   : Decoder;
      Tag    : Slim.Messages.Message_Tag;
      Data   : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector;
      Result : out Slim.Messages.Message_Access)
   is
      pragma Unreferenced (Self);
      use type Ada.Tags.Tag;

      function Constructor is new Ada.Tags.Generic_Dispatching_Constructor
        (T           => Slim.Messages.Message,
         Parameters  => League.Stream_Element_Vectors.Stream_Element_Vector,
         Constructor => Slim.Messages.Read);

      Raw_Tag : Ada.Tags.Tag := Ada.Tags.No_Tag;
   begin
      for J of Map loop
         if Tag = J.Label then
            Raw_Tag := J.Tag;
         end if;
      end loop;

      if Raw_Tag = Ada.Tags.No_Tag then
         raise Program_Error with "Unknown tag: " & Tag;
      end if;

      Result := new Slim.Messages.Message'Class'(Constructor (Raw_Tag, Data));
   end Decode;

end Slim.Message_Decoders;
