--  Copyright (c) 2019 Maxim Reznik <reznikmm@gmail.com>
--
--  SPDX-License-Identifier: MIT
--  License-Filename: LICENSE
-------------------------------------------------------------

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

with GNAT.Sockets;
with League.Strings;

with Slim.Players;

procedure Slim.Run is
   use GNAT.Sockets;

   function "+" (X : Wide_Wide_String) return League.Strings.Universal_String
     renames League.Strings.To_Universal_String;

   task Discovery is
      entry Start;
   end Discovery;

   task type Server is
      entry Start (Value : Socket_Type);
   end Server;

   type Server_Access is access all Server;

   --------------
   -- Visiters --
   --------------

   ---------------
   -- Discovery --
   ---------------

   task body Discovery is
      use type Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;
      Address  : Sock_Addr_Type;
      Listener : Socket_Type;
      From     : Sock_Addr_Type;
      Last     : Ada.Streams.Stream_Element_Offset;
      Request  : Ada.Streams.Stream_Element_Array (1 .. 128);
   begin
      accept Start;
      Address.Addr := Any_Inet_Addr;
      Address.Port := 3483;
      Create_Socket (Listener, Mode => Socket_Datagram);
      Bind_Socket (Listener, Address);

      loop
         Receive_Socket (Listener, Request, Last, From);

         if Last = 18 and then Request (1) = Character'Pos ('d') then
            --  Discovery request
            declare
               Reply : constant Ada.Streams.Stream_Element_Array (1 .. 18) :=
                 (Character'Pos ('D'),
                  Character'Pos ('O'),
                  Character'Pos ('p'),
                  Character'Pos ('e'),
                  Character'Pos ('n'),
                  Character'Pos ('W'),
                  Character'Pos ('R'),
                  Character'Pos ('T'),
                  others => 0);
            begin
               Send_Socket (Listener, Reply, Last, To => From);
            end;
         end if;
      end loop;
   end Discovery;

   ------------
   -- Server --
   ------------

   task body Server is
      Player : aliased Slim.Players.Player;
   begin
      accept Start (Value : Socket_Type) do
         Player.Initialize
           (Socket => Value,
            Font   => +"10x20-ISO8859-5",
            Splash => +"data/splash.dat");
      end Start;

      loop
         Player.Process_Message;
         --  Read and process one message from socket if any
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           (Ada.Exceptions.Exception_Name (E) & ": "
            & Ada.Exceptions.Exception_Message (E));
   end Server;

   Address  : Sock_Addr_Type;
   Listener : Socket_Type;
   Client   : Socket_Type;
begin
   Address.Addr := Any_Inet_Addr;
   Address.Port := 3483;
   Create_Socket (Listener);
   Bind_Socket (Listener, Address);
   Listen_Socket (Listener);

   Discovery.Start;

   loop
      declare
         Next : Server_Access;
      begin
         Accept_Socket (Listener, Client, Address);
         Next := new Server;
         Next.Start (Client);
      end;
   end loop;
--     Close_Socket (Client);
--     Close_Socket (Listener);
end Slim.Run;
