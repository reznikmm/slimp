--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Strings;

package Slim.Messages.SETD is
   type SETD_Message is new Message with private;
   --  Setting reply

   type Setting_Kind is (Player_Name, Something_Else);

   type Setting (Kind : Setting_Kind := Player_Name) is record
      case Kind is
         when Player_Name =>
            Player : League.Strings.Universal_String;
         when Something_Else =>
            null;
      end case;
   end record;

   not overriding function Get_Setting (Self : SETD_Message) return Setting;

private

   type SETD_Message is new Base_Message
     (Max_8  => 1,
      Max_16 => 0,
      Max_32 => 0,
      Max_64 => 0)
   with record
      Player : League.Strings.Universal_String;
      Value  : Ada.Streams.Stream_Element;  --  values other then player name
   end record;

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
      return SETD_Message;

   overriding procedure Write
     (Self : SETD_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Read_Custom_Field
     (Self  : in out SETD_Message;
      Index : Positive;
      Input : in out Ada.Streams.Stream_Element_Offset;
      Data  : League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Write_Custom_Field
     (Self  : SETD_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector);

   overriding procedure Visit
     (Self    : not null access SETD_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class);

end Slim.Messages.SETD;
