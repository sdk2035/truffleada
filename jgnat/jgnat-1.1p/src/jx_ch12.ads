------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J X _ C H 1 2                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.1 $                             --
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

--  This package implements translation of Ada generic library units into
--  their equivalent JVM class file.

with Types;  use Types;

package Jx_Ch12 is

   procedure Generate_Generic_Unit_Class (Comp_Unit : Node_Id);
   --  Generates a class file for the generic library unit designated by
   --  the compilation unit Comp_Unit. Generic library units require a
   --  class file in cases where they have associated elaboration code
   --  (in particular, for the intialization of elaboration check Booleans).

end Jx_Ch12;

