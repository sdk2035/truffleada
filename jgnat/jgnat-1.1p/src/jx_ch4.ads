------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 4                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.9 $                             --
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

--  This package implements translation of Ada names and expressions
--  into their corresponding JVM equivalents, as well as providing
--  various utility routines and types used by other packages.

with JVM;   use JVM;
with Types; use Types;

package Jx_Ch4 is

   type Address_Kind is
     (No_Address,
      Local_Address,
      Field_Address,
      Indexed_Address,
      Object_Address,
      Array_Address);

   type Address_Descriptor (Addr_Kind : Address_Kind := No_Address) is record
      case Addr_Kind is
         when No_Address =>
            null;
         when Local_Address =>
            Local_Var : Local_Var_Id;
         when Field_Address =>
            Field : Field_Id;
         when Indexed_Address =>
            Comp_Type : Type_Id;
         when Array_Address =>
            Is_Slice : Boolean;
            Descriptor_Class : Class_Id;
         when Object_Address =>
            null;
      end case;
   end record;

   function JVM_Expr_Type (Expr : Node_Id) return Type_Id;
   --  Returns the JVM type entity associated with the type of the
   --  expression Expr.

   function Evaluate_Addr (Obj_Name : Node_Id) return Address_Descriptor;
   --  Generates J-code to evaluate the given object name (if needed) and
   --  returns an Address_Descriptor describing the location of the object.

   procedure Evaluate_Array_Address (Arr : Node_Id);
   --  Generates code to load the array reference denoted by the name Arr.
   --  If Arr is an N_Explicit_Dereference for an access-to-unconstrained
   --  array then this will load the 'all' field from the referenced array
   --  descriptor. This routine should only be called if such a dereferencing
   --  is appropriate (i.e., no bound information will be needed by the
   --  context), otherwise either Evaluate_Addr or Evaluate_Expr should
   --  be called. Raises an exception if Arr does not denote an array.

   procedure Evaluate_Expr (Expr : Node_Id);
   --  Generates J-code to evaluate Expr.

   procedure Evaluate_Expr (Expr : Node_Id; Check_Subtype : Entity_Id);
   --  Generates J-code to evaluate Expr and perform a scalar range
   --  check against the range of Check_Subtype when Do_Range_Check
   --  is set on Expr.

   procedure Evaluate_Expr
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean);
   --  Generates J-code to evaluate Expr. Label is used to provide a target
   --  for the evaluation of a conditional expression (Label = Null_Label
   --  if no target available). If Label /= Null_Label, then True_Branch
   --  indicates whether the branch should occur when the condition is
   --  True or when it's False.

   procedure Evaluate_With_Copy (Expr : Node_Id);
   --  Generates code to evaluate Expr, performing a copy of the value
   --  in the case of an expression with a composite type (except in
   --  the case of an expression given by an aggregate or function call,
   --  in which case no copy is needed). This form of expression evaluation
   --  can be used in contexts that require a new object to be created, such
   --  as initialized object declarations, aggregate associations, and
   --  function return statements.

   procedure Store_Elementary_Value (Target : Address_Descriptor);
   --  Generates J-code to store the top-of-stack value into the
   --  object whose location is described by Target.

   procedure Load_Index_Length
     (Index  : Node_Id;
      Obj_LV : Local_Var_Id := Null_Local_Var);
   --  Generates J-code to load the length of the given array index
   --  (computed as Max (0, high bound - low bound + 1)). If Obj_LV
   --  is not null, then any discriminant bounds will be obtained
   --  by loading the discriminant from the object denoted by the
   --  the local variable.

   function Array_Index_First (Index : Node_Id) return Node_Id;
   --  Returns the low bound associated with the index of an array subtype

   function Array_Index_Last (Index : Node_Id) return Node_Id;
   --  Returns the high bound associated with the index of an array subtype

   function Index_First (Array_Subtype : Entity_Id) return Node_Id;
   --  Returns the low bound associated with the first dimension of
   --  the given array subtype.

   function Index_Last (Array_Subtype : Entity_Id) return Node_Id;
   --  Returns the high bound associated with the first dimension of
   --  the given array subtype.

   procedure Gen_Array_Subscript
     (Prefix          : Node_Id;
      First_Subscript : Node_Id);
   --  Generates code to evaluate the array index expression and adjust
   --  it by the lower bound of the array object denoted by Prefix. If
   --  this is a case of indexing a multidimensional array, then the
   --  remainder of the subscripts (successors of First_Subscript) will
   --  also be evaluated to produce the address of the final indexed
   --  array element.

   procedure Gen_Scalar_Subtype_Check (Scalar_Subt : Entity_Id);
   --  Generate code to check the top-of-stack value against the constraint
   --  (if any) of the scalar subtype Scalar_Subt.

   procedure Test_For_Slice
     (N     : Node_Id;
      Slice : out Boolean;
      Prfix : out Node_Id;
      Subt  : out Entity_Id);
   --  Tests if the expression denoted by Arr is a slice or and qualified
   --  expression or conversion with a slice argument, and if so returns
   --  True in Slice, the slice prefix in Prfix, and the subtype of the
   --  slice in Subt. Otherwise, returns False in Slice, N in Prfix, and
   --  Etype (N) in Subt.

   procedure Evaluate_String_Literal
     (Str_Lit  : String_Id;
      Str_Type : Entity_Id);
   --  Creates a constant pool entry for Str_Lit of type Str_Type and
   --  generates code to load its value and convert it to an Ada string.

   procedure Load_Array_Bounds (Arr_Expr : Node_Id);
   --  Loads the bounds of the array denoted by Arr_Expr. Assumes that
   --  Arr_Expr has already been evaluated, with the resulting reference
   --  currently on the top of the stack.

end Jx_Ch4;
