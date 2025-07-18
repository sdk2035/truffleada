------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              R E P I N F O                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.11 $
--                                                                          --
--             Copyright (C) 1999 Free Software Foundation, Inc.            --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the routines to handle back annotation of the
--  tree to fill in representation information, and also the routine used
--  by -gnatR to print this information. This unit is used both in the
--  compiler and in ASIS (it is used in ASIS as part of the implementation
--  of the data decomposition annex.

with Types; use Types;
with Uintp; use Uintp;

package Repinfo is

   --------------------------------
   -- Representation Information --
   --------------------------------

   --  The representation information of interest here is size and
   --  component information for arrays and records. For primitive
   --  types, the front end computes the Esize and RM_Size fields of
   --  the corresponding entities as constant positive integers, and
   --  the Uint values are stored directly in these fields.

   --  For arrays and records, this information is sometimes available
   --  in the front end, as a result of the use of representation clauses
   --  or pragmas, but most often, the front end leaves gigi to layout the
   --  types, and hence this representation information is not available
   --  prior to the call to gigi.

   --  As part of the processing in gigi, the types are laid out and
   --  appropriate values computed for the sizes and component positions
   --  and sizes of records and arrays.

   --  The back-annotation circuit in gigi is responsible for updating the
   --  releavnt fields in the tree to reflect these computations, as follows:

   --    For E_Array_Type entities, the Component_Size field

   --    For all record and array types and subtypes, the Esize field,
   --    which contains the Size (more accurately the Object_SIze) value
   --    for the type or subtype.

   --    For E_Component and E_Distriminant entities, the Esize (size
   --    of component) and Component_First_Bit fields.

   --  There are three cases to consider:

   --    1. The value is constant. In this case, the back annotation works
   --       by simply storing the non-negative universal integer value in
   --       the appropriate field corresponding to this constant size.

   --    2. The value depends on variables other than discriminants of the
   --       current record. In this case, the value is not known, even if
   --       the complete data of the record is available, and gigi marks
   --       this situation by storing the special value No_Uint.

   --    3. The value depends on the discriminant values for the current
   --       record. In this case, gigi back annotates the field with a
   --       representation of the expression for computing the value in
   --       terms of the discriminants. A negative Uint value is used to
   --       represent the value of such an expression, as explained in
   --       the following section.

   --  GCC expressions are represented with a Uint value that is negative.
   --  See the body of this package for details on the representation used.

   --  One other case in which gigi back annotates GCC expressions is in
   --  the Present_Expr field of an N_Variant node. This expression which
   --  will always depend on discriminants, and hence always be represented
   --  as a negative Uint value, provides an expression which, when evaluated
   --  with a given set of discriminant values, indicates whether the variant
   --  is present for that set of values (result is True, i.e. non-zero) or
   --  not present (result is False, i.e. zero).

   subtype Node_Ref is Uint;
   --  Subtype used for negative uint values used to represent nodes

   subtype Node_Ref_Or_Val is Uint;
   --  Subtype used for values that can either be a Node_Ref (negative)
   --  or a value (non-negative)

   type TCode is range 0 .. 27;
   --  Type used on Ada side to represent DEFTREECODE values defined in
   --  tree.def. Only a subset of these tree codes can actually appear.
   --  The names are the names from tree.def in Ada casing.

   --  name                             code   description           operands

   Cond_Expr        : constant TCode :=  1; -- conditional              3
   Plus_Expr        : constant TCode :=  2; -- addition                 2
   Minus_Expr       : constant TCode :=  3; -- subtraction              2
   Mult_Expr        : constant TCode :=  4; -- multiplication           2
   Trunc_Div_Expr   : constant TCode :=  5; -- truncating division      2
   Ceil_Div_Expr    : constant TCode :=  6; -- division rounding up     2
   Floor_Div_Expr   : constant TCode :=  7; -- division rounding down   2
   Trunc_Mod_Expr   : constant TCode :=  8; -- mod for trunc_div        2
   Ceil_Mod_Expr    : constant TCode :=  9; -- mod for ceil_div         2
   Floor_Mod_Expr   : constant TCode := 10; -- mod for floor_div        2
   Exact_Div_Expr   : constant TCode := 11; -- exact div                2
   Negate_Expr      : constant TCode := 12; -- negation                 1
   Min_Expr         : constant TCode := 13; -- minimum                  2
   Max_Expr         : constant TCode := 14; -- maximum                  2
   Abs_Expr         : constant TCode := 15; -- absolute value           1
   Truth_Andif_Expr : constant TCode := 16; -- Boolean and then         2
   Truth_Orif_Expr  : constant TCode := 17; -- Boolean or else          2
   Truth_And_Expr   : constant TCode := 18; -- Boolean and              2
   Truth_Or_Expr    : constant TCode := 19; -- Boolean or               2
   Truth_Xor_Expr   : constant TCode := 20; -- Boolean xor              2
   Truth_Not_Expr   : constant TCode := 21; -- Boolean not              1
   Lt_Expr          : constant TCode := 22; -- comparision <            2
   Le_Expr          : constant TCode := 23; -- comparision <=           2
   Gt_Expr          : constant TCode := 24; -- comparision >            2
   Ge_Expr          : constant TCode := 25; -- comparision >=           2
   Eq_Expr          : constant TCode := 26; -- comparision =            2
   Ne_Expr          : constant TCode := 27; -- comparision /=           2

   --  The following entry is used to represent a discriminant value in
   --  the tree. It has a special tree code that does not correspond
   --  directly to a gcc node. The single operand is the number of the
   --  discriminant in the record (1 = first discriminant).

   Discrim_Val      : constant TCode := 0;  -- discriminant value       1

   ------------------------
   -- The gigi Interface --
   ------------------------

   --  The following declarations are for use by gigi for back annotation

   function Create_Node
     (Expr : TCode;
      Op1  : Node_Ref_Or_Val;
      Op2  : Node_Ref_Or_Val := No_Uint;
      Op3  : Node_Ref_Or_Val := No_Uint)
      return  Node_Ref;
   --  Creates a node with using the tree code defined by Expr and from
   --  1-3 operands as required (unused operands set as shown to No_Uint)
   --  Note that this call can be used to create a discriminant reference
   --  by using (Expr => Discrim_Val, Op1 => discriminant_number).

   function Create_Discrim_Ref
     (Discr : Entity_Id)
      return  Node_Ref;
   --  Creates a refrerence to the discriminant whose entity is Discr.

   --------------------
   -- ASIS_Interface --
   --------------------

   type Discrim_List is array (Pos range <>) of Uint;
   --  Type used to represent list of discriminant values

   function Rep_Value
     (Val  : Node_Ref_Or_Val;
      D    : Discrim_List)
      return Uint;
   --  Given the contents of a First_Bit_Position or Esize field containing
   --  a node reference (i.e. a negative Uint value) and D, the list of
   --  discriminant values, returns the interpreted value of this field.
   --  For convenience, Rep_Value will take a non-negative Uint value
   --  as an argument value, and return it unmodified. A No_Uint value is
   --  also returned unmodified.

   procedure Tree_Read;
   --  Read in the value of the Rep_Table

   ------------------------
   -- Compiler Interface --
   ------------------------

   procedure List_Rep_Info;
   --  Procedure to list representation information

   procedure Tree_Write;
   --  Write out the value of the Rep_Table

   --------------------------
   -- Debugging Procedures --
   --------------------------

   procedure List_GCC_Expression (U : Node_Ref_Or_Val);
   --  Prints out given expression in symbolic form. Constants are listed
   --  in decimal numeric form, Discriminants are listed with a # followed
   --  by the discriminant number, and operators are output in appropriate
   --  symbolic form No_Uint displays as two question marks. The output is
   --  on a single line, followed by a line return.

   procedure lgx (U : Node_Ref_Or_Val) renames List_GCC_Expression;
   --  Renamed for easy use from gdb.


end Repinfo;
