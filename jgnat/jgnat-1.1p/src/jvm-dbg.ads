------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J V M . D B G                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.4 $                             --
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

--  This package provides a set of debugging routines that print the
--  contents of the various forms of JVM entity.

with Types; use Types;

package JVM.Dbg is

   procedure Print_Class (C : Class_Id);
   --  Prints the basic information associated with a JVM class
   --  entity, including its name, superclass, and list of fields
   --  and methods (but not the code associated with the methods).

   procedure Print_Type (T : Type_Id);
   --  Prints information associated with a JVM type entity:
   --  the type's name, kind (int, float, array, class, etc.),
   --  and other relevant characteristics (e.g., element type
   --  and dimensions for an array type).

   procedure Print_Field (F : Field_Id);
   --  Prints information associated with a JVM class field entity,
   --  including the field's associated class and the field's type.

   procedure Print_Method (M : Method_Id);
   --  Prints information associated with a JVM class method entity,
   --  including the method's associated class, its parameters,
   --  and the method's result type.

   procedure Print_Jcode (M : Method_Id);
   --  Prints the symbolic J-code associated with the given method.

   procedure Print_Local_Var (L : Local_Var_Id);
   --  Prints information associated with a JVM local variable entity,
   --  including the variable's type and word offset (index) in its
   --  method.

   procedure PC (C : Class_Id);
   --  The same as Print_Class (shortened name for debugging convenience)

   procedure PT (T : Type_Id);
   --  The same as Print_Type (shortened name for debugging convenience)

   procedure PF (F : Field_Id);
   --  The same as Print_Field (shortened name for debugging convenience)

   procedure PM (M : Method_Id);
   --  The same as Print_Method (shortened name for debugging convenience)

   procedure PJ (M : Method_Id);
   --  The same as Print_Jcode (shortened name for debugging convenience)

   procedure PL (L : Local_Var_Id);
   --  The same as Print_Local_Var (shortened name for debugging convenience)

   ----------------------------------------
   -- General Utility Debugging Routines --
   ----------------------------------------

   procedure Print (S : String);
   --  Writes S as debugging output

   procedure Print (N : Name_Id);
   --  Writes N's associated string as debugging output

   procedure Print_Line (S : String := "");
   --  Writes S following by a new line as debugging output

   procedure Init_Source_Line_Output (Node : Node_Id);
   --  If debugging is enabled, then allows output of source line
   --  information from the source file associated with Node via
   --  calls to Print_Source_Line. Must be called before any calls
   --  occur to Print_Source_Line.

   procedure Print_Source_Line (Node : Node_Id);
   --  If debugging is enabled, then prints the source line corresponding
   --  to Node preceded by the associated source file name and Sloc. If
   --  there was a preceding call to this procedure with a node having
   --  the same source line as Node then nothing is output.

end JVM.Dbg;
