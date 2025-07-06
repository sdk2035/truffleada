------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J X _ D R I V E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.2 $                             --
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
-- JGNAT -  The GNAT Ada 95 tool chain for the Java (TM) Virtual Machine is --
--          maintained by Ada Core Technologies, Inc. - http://www.gnat.com --
--                                                                          --
------------------------------------------------------------------------------

--  This package is the main driver for the JGNAT back end and is invoked
--  by the gnat1 driver.

with Types; use Types;

package Jx_Drive is

   Java_Byte_Code : Boolean := False;
   --  Global option for enabling J-code generation. Should eventually
   --  be moved to opt.ads. Set by the -gnatJ switch (see switch.adb).
   --  (Actually this switch is not used at present, and gnat1drv checks
   --  the -gnatJ switch to determine whether to call GNAT_To_JVM. ???)

   procedure GNAT_To_JVM (GNAT_Root : Node_Id);
   --  Translates an entire GNAT tree for a compilation unit into
   --  a set of JVM class files. This is the main driver for the
   --  Ada-to-JVM back end and is invoked by Gnat1drv.

   procedure Translate (Node : Node_Id);
   --  This is the top-level translation routine which is applied to
   --  declarations. We export this so it can be called recursively
   --  from other JVM back-end packages.

end Jx_Drive;
