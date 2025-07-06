------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J X _ S W T C H                              --
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

--  This package provides common support for generation of switch statements
--  for case statements and record variant initializations.

with JVM;   use JVM;
with Types; use Types;

package Jx_Swtch is

   type Switch_Action is
     access procedure (Altern : Node_Id; Obj : Local_Var_Id := Null_Local_Var);
   --  Subprogram access type for representing an action to perform
   --  as part of a switch alternative. The second parameter is optional
   --  but needed to support component allocation within variant records.

   procedure Generate_Switch
     (Alterns : List_Id;
      Action  : Switch_Action;
      Obj     : Local_Var_Id := Null_Local_Var);
   --  Generates a JVM switch instruction corresponding to the given list
   --  of Ada case or variant part alternatives, and invokes Action to
   --  generate code for each alternative. Assumes that a value to which
   --  the switch will apply is on the top of stack. The optional Obj
   --  parameter is used for supporting component allocation within
   --  variant records.

end Jx_Swtch;
