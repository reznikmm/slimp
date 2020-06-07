--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with System.Storage_Elements;
with Ada.Storage_IO;

package body Slim.Messages is

   -----------------
   -- Read_Fields --
   -----------------

   procedure Read_Fields
     (Self : in out Base_Message'Class;
      List : Field_Description_Array;
      Data : League.Stream_Element_Vectors.Stream_Element_Vector)
   is
      use type Ada.Streams.Stream_Element_Offset;

      generic
         type Element is private;
      procedure Read (Result : out Element);

      Input  : Ada.Streams.Stream_Element_Offset := 1;

      ----------
      -- Read --
      ----------

      procedure Read (Result : out Element) is
         package IO is new Ada.Storage_IO (Element);
         Buffer : IO.Buffer_Type;
      begin
         for J in reverse Buffer'Range loop
            Buffer (J) := System.Storage_Elements.Storage_Element
              (Data.Element (Input));
            Input := Input + 1;
         end loop;

         IO.Read (Buffer, Result);
      end Read;

      procedure Read_8  is new Read (Interfaces.Unsigned_8);
      procedure Read_16 is new Read (Interfaces.Unsigned_16);
      procedure Read_32 is new Read (Interfaces.Unsigned_32);
      procedure Read_64 is new Read (Interfaces.Unsigned_64);

      Kind   : Field_Kinds;
      Fields : Field_Description_Array := List;
      Index  : Positive := Fields'First;
      Counts : array (Field_Kinds) of Natural := (others => 0);
   begin
      while Index <= Fields'Last loop
         Kind := Fields (Index).Kind;
         Counts (Kind) := Counts (Kind) + 1;

         case Kind is
            when Uint_8_Field =>
               Read_8 (Self.Data_8 (Counts (Kind)));
            when Uint_16_Field =>
               Read_16 (Self.Data_16 (Counts (Kind)));
            when Uint_32_Field =>
               Read_32 (Self.Data_32 (Counts (Kind)));
            when Uint_64_Field =>
               Read_64 (Self.Data_64 (Counts (Kind)));
            when others =>
               Self.Read_Custom_Field
                 (Index => Counts (Kind),
                  Input => Input,
                  Data  => Data);
         end case;

         if Fields (Index).Count = 1  then
            Index := Index + 1;
         else
            Fields (Index).Count := Fields (Index).Count - 1;
         end if;
      end loop;
   end Read_Fields;

   -----------
   -- Slice --
   -----------

   procedure Slice
     (Result : out Ada.Streams.Stream_Element_Array;
      From   : Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      for J in Result'Range loop
         Result (J) := Data.Element (From + J - Result'First);
      end loop;
   end Slice;

   ------------------
   -- Write_Fields --
   ------------------

   procedure Write_Fields
     (Self : Base_Message'Class;
      List : Field_Description_Array;
      Data : in out League.Stream_Element_Vectors.Stream_Element_Vector)
   is
      generic
         type Element is private;
      procedure Write (Result : Element);

      -----------
      -- Write --
      -----------

      procedure Write (Result : Element) is
         package IO is new Ada.Storage_IO (Element);
         Buffer : IO.Buffer_Type;
      begin
         IO.Write (Buffer, Result);

         --  Use network byte order, so use 'reverse'
         for J in reverse Buffer'Range loop
            Data.Append (Ada.Streams.Stream_Element (Buffer (J)));
         end loop;
      end Write;

      procedure Write_8  is new Write (Interfaces.Unsigned_8);
      procedure Write_16 is new Write (Interfaces.Unsigned_16);
      procedure Write_32 is new Write (Interfaces.Unsigned_32);
      procedure Write_64 is new Write (Interfaces.Unsigned_64);

      Kind   : Field_Kinds;
      Fields : Field_Description_Array := List;
      Index  : Positive := Fields'First;
      Counts : array (Field_Kinds) of Natural := (others => 0);
   begin
      while Index <= Fields'Last loop
         Kind := Fields (Index).Kind;
         Counts (Kind) := Counts (Kind) + 1;

         case Kind is
            when Uint_8_Field =>
               Write_8 (Self.Data_8 (Counts (Kind)));
            when Uint_16_Field =>
               Write_16 (Self.Data_16 (Counts (Kind)));
            when Uint_32_Field =>
               Write_32 (Self.Data_32 (Counts (Kind)));
            when Uint_64_Field =>
               Write_64 (Self.Data_64 (Counts (Kind)));
            when others =>
               Self.Write_Custom_Field
                 (Index => Counts (Kind),
                  Data  => Data);
         end case;

         if Fields (Index).Count = 1  then
            Index := Index + 1;
         else
            Fields (Index).Count := Fields (Index).Count - 1;
         end if;
      end loop;
   end Write_Fields;

end Slim.Messages;
