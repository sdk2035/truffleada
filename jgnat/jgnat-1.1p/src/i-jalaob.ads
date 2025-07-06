------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         I N T E R F A C E S . J A V A . L A N G . O B J E C T            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.3 $                             --
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

--  This child package corresponds to the Java class java.lang.Object.
--  Please consult package Interfaces.Java for an explanation of the
--  mapping between a Java Class and an Ada package.

package Interfaces.Java.Lang.Object is
   pragma Preelaborate;

   type Typ (<>) is tagged limited private;

   type Ref       is access all Typ;
   type Ref_Class is access all Typ'Class;

   procedure Wait   (This : access Typ);
   procedure Wait   (This : access Typ; Timeout : long; Nanos : int);
   procedure Notify (This : access Typ);

   function new_Object return Ref;

   package J_Object renames Interfaces.Java.Lang.Object;

private
   type Typ is tagged limited null record;
   pragma Convention (Java, Typ);

   pragma Import (Java, Wait,   "wait");
   pragma Import (Java, Notify, "notify");

   pragma Java_Constructor (new_Object);

end Interfaces.Java.Lang.Object;

pragma Import (Java, Interfaces.Java.Lang.Object, "java.lang.Object");
