------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         I N T E R F A C E S . J A V A . L A N G . T H R E A D            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.5 $
--                                                                          --
--              Copyright (C) 1999 Ada Core Technologies, Inc.              --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- JGNAT -  The GNAT Ada 95 tool chain for the Java (TM) Virtual Machine is --
--          maintained by Ada Core Technologies, Inc. - http://www.gnat.com --
--                                                                          --
------------------------------------------------------------------------------

--  This child package corresponds to the Java class java.lang.Thread.
--  Please consult package Interfaces.Java for an explanation of the
--  mapping between a Java Class and an Ada package.

with Interfaces.Java.Lang.Object;

package Interfaces.Java.Lang.Thread is
   pragma Preelaborate;

   type Typ is new Object.Typ with null record;

   type Ref       is access all Typ;
   type Ref_Class is access all Typ'Class;

   MIN_PRIORITY : int;
   MAX_PRIORITY : int;

   procedure Set_Priority (This   : access Typ; New_Priority : int);
   function  Get_Priority (This   : access Typ) return int;
   procedure Set_Daemon   (This   : access Typ; Status : boolean);
   procedure Start        (This   : access Typ);
   procedure Sleep        (Millis : long);
   procedure Yield;
   function  Current_Thread return Ref_Class;

private
   pragma Convention (Java, Typ);

   pragma Import (Java, MIN_PRIORITY, "MIN_PRIORITY");
   pragma Import (Java, MAX_PRIORITY, "MAX_PRIORITY");

   pragma Import (Java, Set_Priority,   "setPriority");
   pragma Import (Java, Get_Priority,   "getPriority");
   pragma Import (Java, Sleep,          "sleep");
   pragma Import (Java, Yield,          "yield");
   pragma Import (Java, Current_Thread, "currentThread");
   pragma Import (Java, Set_Daemon,     "setDaemon");
   pragma Import (Java, Start,          "start");

end Interfaces.Java.Lang.Thread;

pragma Import (Java, Interfaces.Java.Lang.Thread, "java.lang.Thread");
