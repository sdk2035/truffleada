------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                U I N T P                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.54 $
--                                                                          --
--          Copyright (C) 1992-1999, Free Software Foundation, Inc.         --
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

--  Support for universal integer arithmetic

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file sinfo.h

with Alloc;
with Table;
with Types; use Types;

package Uintp is

   -------------------------------------------------
   -- Basic Types and Constants for Uintp Package --
   -------------------------------------------------

   type Uint is private;
   --  The basic universal integer type

   No_Uint : constant Uint;
   --  A constant value indicating a missing or unset Uint value

   Uint_0   : constant Uint;
   Uint_1   : constant Uint;
   Uint_2   : constant Uint;
   Uint_3   : constant Uint;
   Uint_4   : constant Uint;
   Uint_5   : constant Uint;
   Uint_6   : constant Uint;
   Uint_7   : constant Uint;
   Uint_8   : constant Uint;
   Uint_9   : constant Uint;
   Uint_10  : constant Uint;
   Uint_12  : constant Uint;
   Uint_15  : constant Uint;
   Uint_16  : constant Uint;
   Uint_24  : constant Uint;
   Uint_32  : constant Uint;
   Uint_63  : constant Uint;
   Uint_64  : constant Uint;
   Uint_128 : constant Uint;

   Uint_Minus_1   : constant Uint;
   Uint_Minus_2   : constant Uint;
   Uint_Minus_3   : constant Uint;
   Uint_Minus_4   : constant Uint;
   Uint_Minus_5   : constant Uint;
   Uint_Minus_6   : constant Uint;
   Uint_Minus_7   : constant Uint;
   Uint_Minus_8   : constant Uint;
   Uint_Minus_9   : constant Uint;
   Uint_Minus_12  : constant Uint;
   Uint_Minus_128 : constant Uint;

   -----------------
   -- Subprograms --
   -----------------

   procedure Initialize;
   --  Initialize Uint tables. Note that Initialize must not be called if
   --  Tree_Read is used. Note also that there is no lock routine in this
   --  unit, these are among the few tables that can be expanded during
   --  gigi processing.

   procedure Tree_Read;
   --  Initializes internal tables from current tree file using Tree_Read.
   --  Note that Initialize should not be called if Tree_Read is used.
   --  Tree_Read includes all necessary initialization.

   procedure Tree_Write;
   --  Writes out internal tables to current tree file using Tree_Write

   function UI_Abs (Right : Uint) return Uint;
   pragma Inline (UI_Abs);
   --  Returns abs function of universal integer.

   function UI_Add (Left : Uint; Right : Uint) return Uint;
   function UI_Add (Left : Int;  Right : Uint) return Uint;
   function UI_Add (Left : Uint; Right : Int)  return Uint;
   --  Returns sum of two integer values.

   function UI_Decimal_Digits_Hi (U : Uint) return Nat;
   --  Returns an estimate of the number of decimal digits required to
   --  represent the absolute value of U. This estimate is correct or high,
   --  i.e. it never returns a value that is too low. The accuracy of the
   --  estimate affects only the effectiveness of comparison optimizations
   --  in Urealp.

   function UI_Decimal_Digits_Lo (U : Uint) return Nat;
   --  Returns an estimate of the number of decimal digits required to
   --  represent the absolute value of U. This estimate is correct or low,
   --  i.e. it never returns a value that is too high. The accuracy of the
   --  estimate affects only the effectiveness of comparison optimizations
   --  in Urealp.

   function UI_Div (Left : Uint; Right : Uint) return Uint;
   function UI_Div (Left : Int;  Right : Uint) return Uint;
   function UI_Div (Left : Uint; Right : Int)  return Uint;
   --  Returns quotient of two integer values. Fatal error if Right = 0

   function UI_Eq (Left : Uint; Right : Uint) return Boolean;
   function UI_Eq (Left : Int;  Right : Uint) return Boolean;
   function UI_Eq (Left : Uint; Right : Int)  return Boolean;
   pragma Inline (UI_Eq);
   --  Compares integer values for equality.

   function UI_Expon (Left : Uint; Right : Uint) return Uint;
   function UI_Expon (Left : Int;  Right : Uint) return Uint;
   function UI_Expon (Left : Uint; Right : Int)  return Uint;
   function UI_Expon (Left : Int;  Right : Int)  return Uint;
   --  Returns result of exponentiating two integer values
   --  Fatal error if Right is negative.

   function UI_GCD (Uin, Vin : Uint) return Uint;
   --  Computes GCD of input values. Assumes Uin >= Vin >= 0.

   function UI_Ge (Left : Uint; Right : Uint) return Boolean;
   function UI_Ge (Left : Int;  Right : Uint) return Boolean;
   function UI_Ge (Left : Uint; Right : Int)  return Boolean;
   pragma Inline (UI_Ge);
   --  Compares integer values for greater than or equal.

   function UI_Gt (Left : Uint; Right : Uint) return Boolean;
   function UI_Gt (Left : Int;  Right : Uint) return Boolean;
   function UI_Gt (Left : Uint; Right : Int)  return Boolean;
   pragma Inline (UI_Gt);
   --  Compares integer values for greater than.

   function UI_Halve (Arg : Uint) return Uint;
   --  Returns the Uint obtained by dividing the given value by 2. Same
   --  result as Arg / 2, but computed with a much faster algorithm

   function UI_Is_In_Int_Range (Input : Uint) return Boolean;
   pragma Inline (UI_Is_In_Int_Range);
   --  Determines if universal integer is in Int range.

   function UI_Le (Left : Uint; Right : Uint) return Boolean;
   function UI_Le (Left : Int;  Right : Uint) return Boolean;
   function UI_Le (Left : Uint; Right : Int)  return Boolean;
   pragma Inline (UI_Le);
   --  Compares integer values for less than or equal.

   function UI_Lt (Left : Uint; Right : Uint) return Boolean;
   function UI_Lt (Left : Int;  Right : Uint) return Boolean;
   function UI_Lt (Left : Uint; Right : Int)  return Boolean;
   --  Compares integer values for less than.

   function UI_Max (Left : Uint; Right : Uint) return Uint;
   function UI_Max (Left : Int;  Right : Uint) return Uint;
   function UI_Max (Left : Uint; Right : Int)  return Uint;
   --  Returns maximum of two integer values

   function UI_Min (Left : Uint; Right : Uint) return Uint;
   function UI_Min (Left : Int;  Right : Uint) return Uint;
   function UI_Min (Left : Uint; Right : Int)  return Uint;
   --  Returns minimum of two integer values.

   function UI_Mod (Left : Uint; Right : Uint) return Uint;
   function UI_Mod (Left : Int;  Right : Uint) return Uint;
   function UI_Mod (Left : Uint; Right : Int)  return Uint;
   pragma Inline (UI_Mod);
   --  Returns mod function of two integer values.

   function UI_Mul (Left : Uint; Right : Uint) return Uint;
   function UI_Mul (Left : Int;  Right : Uint) return Uint;
   function UI_Mul (Left : Uint; Right : Int)  return Uint;
   --  Returns product of two integer values

   function UI_Ne (Left : Uint; Right : Uint) return Boolean;
   function UI_Ne (Left : Int;  Right : Uint) return Boolean;
   function UI_Ne (Left : Uint; Right : Int)  return Boolean;
   pragma Inline (UI_Ne);
   --  Compares integer values for inequality.

   function UI_Negate (Right : Uint) return Uint;
   pragma Inline (UI_Negate);
   --  Returns negative of universal integer.

   function UI_Rem (Left : Uint; Right : Uint) return Uint;
   function UI_Rem (Left : Int;  Right : Uint) return Uint;
   function UI_Rem (Left : Uint; Right : Int)  return Uint;
   --  Returns rem of two integer values.

   function UI_Sub (Left : Uint; Right : Uint) return Uint;
   function UI_Sub (Left : Int;  Right : Uint) return Uint;
   function UI_Sub (Left : Uint; Right : Int)  return Uint;
   pragma Inline (UI_Sub);
   --  Returns difference of two integer values

   function UI_From_Int (Input : Int) return Uint;
   --  Converts Int value to universal integer form.

   function UI_To_Int (Input : Uint) return Int;
   --  Converts universal integer value to Int. Fatal error
   --  if value is not in appropriate range.

   function Num_Bits (Input : Uint) return Nat;
   --  Approximate number of binary bits in given universal integer.
   --  This function is used for capacity checks, and it can be one
   --  bit off without affecting its usage.

   ---------------------
   -- Output Routines --
   ---------------------

   type UI_Format is (Hex, Decimal, Auto);
   --  Used to determine whether UI_Image/UI_Write output is in hexadecimal
   --  or decimal format. Auto, the default setting, lets the routine make
   --  a decision based on the value.

   UI_Image_Max    : constant := 32;
   UI_Image_Buffer : String (1 .. UI_Image_Max);
   UI_Image_Length : Natural;
   --  Buffer used for UI_Image as described below

   procedure UI_Image (Input : Uint; Format : UI_Format := Auto);
   --  Places a representation of Uint, consisting of a possible minus sign,
   --  followed by the value in UI_Image_Buffer. The form of the value is an
   --  integer literal in either decimal (no base) or hexadecimal (base 16)
   --  format. If Hex is True on entry, then hex mode is forced, otherwise
   --  UI_Image makes a guess at which output format is more convenient. The
   --  value must fit in UI_Image_Buffer. If necessary, the result is an
   --  approximation of the proper value, using an exponential format. The
   --  image of No_Uint is output as a single question mark.

   procedure UI_Write (Input : Uint; Format : UI_Format := Auto);
   --  Writes a representation of Uint, consisting of a possible minus sign,
   --  followed by the value to the output file. The form of the value is an
   --  integer literal in either decimal (no base) or hexadecimal (base 16)
   --  format as appropriate. UI_Format shows which format to use. Auto,
   --  the default, asks UI_Write to make a guess at which output format
   --  will be more convenient to read.

   procedure pid (Input : Uint);
   --  Writes representation of Uint in decimal with a terminating line
   --  return. This is intended for use from the debugger.

   procedure pih (Input : Uint);
   --  Writes representation of Uint in hex with a terminating line return.
   --  This is intended for use from the debugger.

   ------------------------
   -- Operator Renamings --
   ------------------------

   function "+" (Left : Uint; Right : Uint) return Uint renames UI_Add;
   function "+" (Left : Int;  Right : Uint) return Uint renames UI_Add;
   function "+" (Left : Uint; Right : Int)  return Uint renames UI_Add;

   function "/" (Left : Uint; Right : Uint) return Uint renames UI_Div;
   function "/" (Left : Int;  Right : Uint) return Uint renames UI_Div;
   function "/" (Left : Uint; Right : Int)  return Uint renames UI_Div;

   function "*" (Left : Uint; Right : Uint) return Uint renames UI_Mul;
   function "*" (Left : Int;  Right : Uint) return Uint renames UI_Mul;
   function "*" (Left : Uint; Right : Int)  return Uint renames UI_Mul;

   function "-" (Left : Uint; Right : Uint) return Uint renames UI_Sub;
   function "-" (Left : Int;  Right : Uint) return Uint renames UI_Sub;
   function "-" (Left : Uint; Right : Int)  return Uint renames UI_Sub;

   function "**"  (Left : Uint; Right : Uint) return Uint renames UI_Expon;
   function "**"  (Left : Uint; Right : Int)  return Uint renames UI_Expon;
   function "**"  (Left : Int;  Right : Uint) return Uint renames UI_Expon;
   function "**"  (Left : Int;  Right : Int)  return Uint renames UI_Expon;

   function "abs" (Real : Uint) return Uint renames UI_Abs;

   function "mod" (Left : Uint; Right : Uint) return Uint renames UI_Mod;
   function "mod" (Left : Int;  Right : Uint) return Uint renames UI_Mod;
   function "mod" (Left : Uint; Right : Int)  return Uint renames UI_Mod;

   function "rem" (Left : Uint; Right : Uint) return Uint renames UI_Rem;
   function "rem" (Left : Int;  Right : Uint) return Uint renames UI_Rem;
   function "rem" (Left : Uint; Right : Int)  return Uint renames UI_Rem;

   function "-"   (Real : Uint) return Uint renames UI_Negate;

   function "="   (Left : Uint; Right : Uint) return Boolean renames UI_Eq;
   function "="   (Left : Int;  Right : Uint) return Boolean renames UI_Eq;
   function "="   (Left : Uint; Right : Int)  return Boolean renames UI_Eq;

   function ">="  (Left : Uint; Right : Uint) return Boolean renames UI_Ge;
   function ">="  (Left : Int;  Right : Uint) return Boolean renames UI_Ge;
   function ">="  (Left : Uint; Right : Int)  return Boolean renames UI_Ge;

   function ">"   (Left : Uint; Right : Uint) return Boolean renames UI_Gt;
   function ">"   (Left : Int;  Right : Uint) return Boolean renames UI_Gt;
   function ">"   (Left : Uint; Right : Int)  return Boolean renames UI_Gt;

   function "<="  (Left : Uint; Right : Uint) return Boolean renames UI_Le;
   function "<="  (Left : Int;  Right : Uint) return Boolean renames UI_Le;
   function "<="  (Left : Uint; Right : Int)  return Boolean renames UI_Le;

   function "<"   (Left : Uint; Right : Uint) return Boolean renames UI_Lt;
   function "<"   (Left : Int;  Right : Uint) return Boolean renames UI_Lt;
   function "<"   (Left : Uint; Right : Int)  return Boolean renames UI_Lt;

   -----------------------------
   -- Mark/Release Processing --
   -----------------------------

   --  The space used by Uint data is not automatically reclaimed. However,
   --  a mark-release regime is implemented which allows storage to be
   --  released back to a previously noted mark. This is used for example
   --  when doing comparisons, where only intermediate results get stored
   --  that do not need to be saved for future use.

   type Save_Mark is private;

   function Mark return Save_Mark;
   --  Note mark point for future release

   procedure Release (M : Save_Mark);
   --  Release storage allocated since mark was noted

   procedure Release_And_Save (M : Save_Mark; UI : in out Uint);
   --  Like Release, except that the given Uint value (which is typically
   --  among the data being released) is recopied after the release, so
   --  that it is the most recent item, and UI is updated to point to
   --  its copied location.

   procedure Release_And_Save (M : Save_Mark; UI1, UI2 : in out Uint);
   --  Like Release, except that the given Uint values (which are typically
   --  among the data being released) are recopied after the release, so
   --  that they are the most recent items, and UI1 and UI2 are updated if
   --  necessary to point to the copied locations. This routine is careful
   --  to do things in the right order, so that the values do not clobber
   --  one another.

   -----------------------------------
   -- Representation of Uint Values --
   -----------------------------------

private

   type Uint is new Int range Uint_Low_Bound .. Uint_High_Bound;
   for Uint'Size use 32;

   --  Uint values are represented as multiple precision integers stored in
   --  a multi-digit format using Base as the base. This value is chosen so
   --  that the product Base*Base is within the range of allowed Int values.

   Base_Bits : constant := 15;
   --  Number of bits in base value

   Base : constant Int := 2 ** Base_Bits;
   --  Base is defined to allow the primitive operations (a0, b0, c0)
   --  defined in the section "The Classical Algorithms" (sec. 4.3.1)
   --  of Knuth's "The Art of Computer Programming", Vol. 2. It is these
   --  algorithms that are used in this package.

   --  Values in the range -(Base-1)..+(Base-1), i.e. one-digit values,
   --  are encoded directly as Uint values by adding a bias value.

   --  The following  values define the bias used to store Uint values which
   --  are in the range -(Base-1)..+(Base-1), as well as the biased values
   --  for the first and last values in this range. We use a new derived type
   --  for these constants to avoid accidental use of Uint arithmetic on
   --  these values, which is never correct.

   type Ctrl is new Int;

   Uint_Direct_Bias  : constant Ctrl :=
                         Ctrl (Int (Uint_Low_Bound) + Base);

   Uint_Direct_First : constant Ctrl :=
                         Ctrl (Int (Uint_Direct_Bias) - (Base - Int (1)));

   Uint_Direct_Last  : constant Ctrl :=
                         Ctrl (Int (Uint_Direct_Bias) + (Base - Int (1)));

   No_Uint : constant Uint := Uint (Uint_Low_Bound);

   Uint_0   : constant Uint := Uint (Uint_Direct_Bias);
   Uint_1   : constant Uint := Uint (Uint_Direct_Bias + 1);
   Uint_2   : constant Uint := Uint (Uint_Direct_Bias + 2);
   Uint_3   : constant Uint := Uint (Uint_Direct_Bias + 3);
   Uint_4   : constant Uint := Uint (Uint_Direct_Bias + 4);
   Uint_5   : constant Uint := Uint (Uint_Direct_Bias + 5);
   Uint_6   : constant Uint := Uint (Uint_Direct_Bias + 6);
   Uint_7   : constant Uint := Uint (Uint_Direct_Bias + 7);
   Uint_8   : constant Uint := Uint (Uint_Direct_Bias + 8);
   Uint_9   : constant Uint := Uint (Uint_Direct_Bias + 9);
   Uint_10  : constant Uint := Uint (Uint_Direct_Bias + 10);
   Uint_12  : constant Uint := Uint (Uint_Direct_Bias + 12);
   Uint_15  : constant Uint := Uint (Uint_Direct_Bias + 15);
   Uint_16  : constant Uint := Uint (Uint_Direct_Bias + 16);
   Uint_24  : constant Uint := Uint (Uint_Direct_Bias + 24);
   Uint_32  : constant Uint := Uint (Uint_Direct_Bias + 32);
   Uint_63  : constant Uint := Uint (Uint_Direct_Bias + 63);
   Uint_64  : constant Uint := Uint (Uint_Direct_Bias + 64);
   Uint_128 : constant Uint := Uint (Uint_Direct_Bias + 128);

   Uint_Minus_1   : constant Uint := Uint (Uint_Direct_Bias - 1);
   Uint_Minus_2   : constant Uint := Uint (Uint_Direct_Bias - 2);
   Uint_Minus_3   : constant Uint := Uint (Uint_Direct_Bias - 3);
   Uint_Minus_4   : constant Uint := Uint (Uint_Direct_Bias - 4);
   Uint_Minus_5   : constant Uint := Uint (Uint_Direct_Bias - 5);
   Uint_Minus_6   : constant Uint := Uint (Uint_Direct_Bias - 6);
   Uint_Minus_7   : constant Uint := Uint (Uint_Direct_Bias - 7);
   Uint_Minus_8   : constant Uint := Uint (Uint_Direct_Bias - 8);
   Uint_Minus_9   : constant Uint := Uint (Uint_Direct_Bias - 9);
   Uint_Minus_12  : constant Uint := Uint (Uint_Direct_Bias - 12);
   Uint_Minus_128 : constant Uint := Uint (Uint_Direct_Bias - 128);

   type Save_Mark is record
      Save_Uint   : Uint;
      Save_Udigit : Int;
   end record;

   --  Values outside the range that is represented directly are stored
   --  using two tables. The secondary table Udigits contains sequences of
   --  Int values consisting of the digits of the number in a radix Base
   --  system. The digits are stored from most significant to least significant
   --  with the first digit only carrying the sign.

   --  There is one entry in the primary Uints table for each distinct Uint
   --  value. This table entry contains the length (number of digits) and
   --  a starting offset of the value in the Udigits table.

   Uint_First_Entry : constant Uint := Uint (Uint_Direct_Last + 1);

   --  Some subprograms defined in this package manipulate the Udigits
   --  table directly, while for others it is more convenient to work with
   --  locally defined arrays of the digits of the the Universal Integers.
   --  The type UI_Vector is defined for this purpose and some internal
   --  subprograms used for converting from one to the other are defined.

   type UI_Vector is array (Pos range <>) of Int;
   --  Vector containing the integer values of a Uint value

   --  Note: An earlier version of this package used pointers of arrays
   --  of Ints (dynamically allocated) for the Uint type. The change
   --  leads to a few less natural idioms used throughout this code, but
   --  eliminates all uses of the heap except for the table package itself.
   --  For example, Uint parameters are often converted to UI_Vectors for
   --  internal manipulation. This is done by creating the local UI_Vector
   --  using the function N_Digits on the Uint to find the size needed for
   --  the vector, and then calling Init_Operand to copy the values out
   --  of the table into the vector.

   type Uint_Entry is record
      Length : Pos;
      --  Length of entry in Udigits table in digits (i.e. in words)

      Loc : Int;
      --  Starting location in Udigits table of this Uint value
   end record;

   package Uints is new Table.Table (
     Table_Component_Type => Uint_Entry,
     Table_Index_Type     => Uint,
     Table_Low_Bound      => Uint_First_Entry,
     Table_Initial        => Alloc.Uints_Initial,
     Table_Increment      => Alloc.Uints_Increment,
     Table_Name           => "Uints");

   package Udigits is new Table.Table (
     Table_Component_Type => Int,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Udigits_Initial,
     Table_Increment      => Alloc.Udigits_Increment,
     Table_Name           => "Udigits");

   --  Note: the reason these tables are defined here in the private part of
   --  the spec, rather than in the body, is that they are refrerenced
   --  directly by gigi.

end Uintp;
