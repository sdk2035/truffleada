------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J X _ U P L E V                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.6 $
--                                                                          --
--           Copyright (C) 1998-2000 Ada Core Technologies, Inc.            --
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

--  This package provides support for nested subprograms. It enables the
--  generation of activation record classes and their fields. Also manages
--  the use of static links to support up-level addressing of AR fields
--  from nested subprograms.

with JVM;      use JVM;
with Types;    use Types;
with J_Stack;

package Jx_Uplev is

   type AR_Field_Rec;

   type AR_Field_Ref is access AR_Field_Rec;

   type AR_Field_Rec is record
      Field : Field_Id;
      Next  : AR_Field_Ref;
   end record;
   --  Objects of this type denote fields of Activation Record (AR)
   --  objects. Such fields represent the home location of a local
   --  variable that is referenced up-level from a nested method.

   type Active_Rec;

   type AR_Access is access Active_Rec;

   type Active_Rec is record
      Parent   : AR_Access;
      Method   : Method_Id;
      AR_Class : Class_Id;
      AR_Obj   : Local_Var_Id;
      Fields   : AR_Field_Ref;
   end record;
   --  Objects of this type hold information about a method's Activation
   --  Record, including a reference Parent to a further enclosing
   --  method's AR (if any), the method's AR class and object, and
   --  the list of fields for local variables referenced up-level
   --  from subprograms nested within Method.

   package AR_Stack is new J_Stack (AR_Access, 100);
   --  The stack of active activation records, which represents the static
   --  nesting of ARs for methods enclosing the current method.

   procedure Make_Activation_Record
     (Method   : Method_Id;
      Name     : Name_Id;
      Pkg_Name : String_Id := No_String);
   --  Creates an activation record (AR) class for Method whose name
   --  is constructed using Name ("__AR_" & Name), associated with the
   --  Java package given by Pkg_Name (unless Pkg_Name = No_String).

   procedure Make_Activation_Record (Subp : Entity_Id);
   --  Creates an activation record (AR) class for the method associated
   --  with Subp. (Convenient shorthand for calling the preceding procedure.)

   procedure End_Activation_Record (Method : Method_Id);
   --  Completes generation of the AR class associated with Method.

   procedure Add_AR_Field (AR : AR_Access; Field : Field_Id);
   --  Adds a field to an AR class that represents the location of
   --  a variable addressed up-level by a nested subprogram.

   function AR_Field (AR : AR_Access; Ada_Obj : Entity_Id) return Field_Id;
   --  Returns the Field_Id associated with an up-level AR field with
   --  the given entity.

   function AR_Field (AR : AR_Access; Name : Name_Id) return Field_Id;
   --  Returns the Field_Id associated with an up-level AR field with
   --  the given name.

   procedure Register_Up_Level_Reference (Ada_Obj : Entity_Id);
   --  Registers an Ada object (Ada_Obj) as being referenced up-level
   --  from a nested subprogram (the current method). If this is the
   --  first up-level reference to the object, then code is generated
   --  to copy the original value of the variable into its corresponding
   --  field in the AR object of the associated parent method.

   procedure Register_Up_Level_Reference
     (Method : Method_Id; LV : Local_Var_Id);
   --  Registers a local variable LV of Method as being referenced up-level
   --  from the current method. If this is the first up-level reference to
   --  the variable, then code is generated to copy the original value of
   --  the variable into its corresponding field in the AR object of Method.

   function Access_From_Current_AR (Ada_Obj : Entity_Id) return Boolean;
   --  Returns True if Ada_Obj must be accessed from the current
   --  method's activation record. This is only valid to call for
   --  object's that are local to the current method. Note that
   --  only scalar and access variables ever need to be retrieved
   --  from their method's activation record.

   function Access_AR_Field (Ada_Obj : Entity_Id) return Field_Id;
   --  Loads the current method's static link parameter and,
   --  if necessary, generates code to chain up the static links
   --  of any enclosing methods' activation records until reaching
   --  the object's associated AR. Returns a Field_Id for the
   --  the local's corresponding AR field.

   procedure Load_Static_Link (Parent_Method : Method_Id);
   --  Generates code to load a reference to the activation record
   --  associated with the parent method of some entity being
   --  referenced by the current method (the entity must be either
   --  a local variable or subprogram of Parent_Method). This may
   --  cause generation of code to chain up a set of static links
   --  for methods enclosing the current method.

   procedure Load_Up_Level_Field (Method : Method_Id; Name : Name_Id);
   --  Generates code to load an up-level field with the given name that
   --  is contained within the AR associated with Method.

   function Enclosing_Method (E : Entity_Id) return Method_Id;
   --  Utility function to return the JVM method entity of the subprogram
   --  enclosing the entity E, if any (returns a null id if no enclosing
   --  subprogram).

   function Is_Global_Entity (E : Entity_Id) return Boolean;
   --  Returns true if and only if E is a library level entity or an
   --  entity declared within a block statement that is at the library
   --  level (such as a block within the statement part of a package body).

end Jx_Uplev;
