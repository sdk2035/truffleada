------------------------------------------------------------------------------
--                                                                          --
--                  JGNAT RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M . O S _ I N T E R F A C E                  --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.5 $
--                                                                          --
--           Copyright (C) 1998-1999 Ada Core Technologies, Inc.            --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- JGNAT -  The GNAT Ada 95 tool chain for the Java (TM) Virtual Machine is --
--          maintained by Ada Core Technologies, Inc. - http://www.gnat.com --
--                                                                          --
------------------------------------------------------------------------------

--  This is a Java version of this package.

--  This package includes all direct interfaces to OS services
--  that are needed by children of System.

--  PLEASE DO NOT add any with-clauses to this package
--  or remove the pragma Elaborate_Body.
--  It is designed to be a bottom-level (leaf) package.

with Interfaces.Java.Lang.Thread;

package System.OS_Interface is

   type Thread    is new Interfaces.Java.Lang.Thread.Typ with null record;
   type Thread_Id is access all Thread;

   --  Note: System.Address is used below instead of Task_ID to avoid a
   --  circular dependemcy. The GNULLI is responsible for converting from and
   --  to a Task_ID this value.

   function new_Thread (Self_Id : System.Address) return Thread_Id;
   function Get_Self_Id return System.Address;
   --  what do these do ??? - needs documentation.

private
   pragma Convention (Java, Thread);
   pragma Java_Constructor (new_Thread);
   pragma Import (Java, Get_Self_Id, "getSelfId");
end System.OS_Interface;

pragma Import (Java, System.OS_Interface, "jgnat.adalib.ada_wrapper");
