--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with League.Text_Codecs;

with Slim.Message_Visiters;

package body Slim.Messages.SETD is
   List : constant Field_Description_Array :=
     ((Uint_8_Field, 1),   --  Code
      (Custom_Field, 1));  --  Value

   ----------
   -- Read --
   ----------

   overriding function Read
     (Data : not null access
        League.Stream_Element_Vectors.Stream_Element_Vector)
          return SETD_Message is
   begin
      return Result : SETD_Message do
         Read_Fields (Result, List, Data.all);
      end return;
   end Read;

   -----------------------
   -- Read_Custom_Field --
   -----------------------

   overriding procedure Read_Custom_Field
     (Self  : in out SETD_Message;
      Index : Positive;
      Input : in out Ada.Streams.Stream_Element_Offset;
      Data  : League.Stream_Element_Vectors.Stream_Element_Vector)
   is
      use type Interfaces.Unsigned_8;
      use type Ada.Streams.Stream_Element_Offset;

      Content : constant Ada.Streams.Stream_Element_Array (1 .. Data.Length) :=
        Data.To_Stream_Element_Array;
   begin
      pragma Assert (Index = 1);

      if Self.Data_8 (1) = 0 then  --  player name
         Self.Player := League.Text_Codecs.Codec_For_Application_Locale.Decode
           (Content (Input .. Content'Last));

         Input := Content'Last + 1;
      elsif Self.Data_8 (1) = 4 then  --  disable dac
         Self.Value := Data.Element (Input);
         Input := Input + 1;
      else
         raise Program_Error;
      end if;
   end Read_Custom_Field;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (Self    : not null access SETD_Message;
      Visiter : in out Slim.Message_Visiters.Visiter'Class) is
   begin
      Visiter.SETD (Self);
   end Visit;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self : SETD_Message;
      Tag  : out Message_Tag;
      Data : out League.Stream_Element_Vectors.Stream_Element_Vector) is
   begin
      Tag := "SETD";
      Write_Fields (Self, List, Data);
   end Write;

   ------------------------
   -- Write_Custom_Field --
   ------------------------

   overriding procedure Write_Custom_Field
     (Self  : SETD_Message;
      Index : Positive;
      Data  : in out League.Stream_Element_Vectors.Stream_Element_Vector)
   is
      use type Interfaces.Unsigned_8;
   begin
      pragma Assert (Index = 1);

      if Self.Data_8 (1) = 0 then  --  player name
         Data.Append
           (League.Text_Codecs.Codec_For_Application_Locale.Encode
              (Self.Player));
      elsif Self.Data_8 (1) = 4 then  --  disable dac
         Data.Append (Self.Value);
      else
         raise Program_Error;
      end if;
   end Write_Custom_Field;

end Slim.Messages.SETD;
