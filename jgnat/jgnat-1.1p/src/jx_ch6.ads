------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 6                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.3 $                             --
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

with Types; use Types;

package Jx_Ch6 is

   --  Processing for subprograms

   procedure Generate_Subprogram_Class (Comp_Unit : Node_Id);
   --  Generates a class file for a subprogram compilation unit.
   --  The class will have the name of the subprogram and the
   --  subprogram's method will be named '$'. If the subprogram
   --  is a parameterless procedure then a 'main' method will
   --  also be created that has the signature of a standard
   --  Java main method (public static void main (String [] args)).

   procedure Generate_Method (Subp_Body : Node_Id);
   --  Generates the subprogram body's method and its J-code.

   procedure Translate_Subprogram_Call (Call : Node_Id);
   --  Generates the J-code for a subprogram call.

end Jx_Ch6;
