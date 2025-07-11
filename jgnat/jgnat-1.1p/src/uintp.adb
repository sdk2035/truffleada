------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                U I N T P                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.69 $
--                                                                          --
--          Copyright (C) 1992-1999 Free Software Foundation, Inc.          --
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

with Output;  use Output;
with Tree_IO; use Tree_IO;

package body Uintp is

   ------------------------
   -- Local Declarations --
   ------------------------

   Uint_Int_First : Uint := Uint_0;
   --  Uint value containing Int'First value, set by Initialize. The initial
   --  value of Uint_0 is used for an assertion check that ensures that this
   --  value is not used before it is initialized. This value is used in the
   --  UI_Is_In_Int_Range predicate, and it is right that this is a host
   --  value, since the issue is host representation of integer values.

   Uint_Int_Last : Uint;
   --  Uint value containing Int'Last value set by Initialize.

   UI_Power_2 : array (Int range 0 .. 64) of Uint;
   --  This table is used to memoize exponentiations by powers of 2. The Nth
   --  entry, if set, contains the Uint value 2 ** N. Initially UI_Power_2_Set
   --  is zero and only the 0'th entry is set, the invariant being that all
   --  entries in the range 0 .. UI_Power_2_Set are initialized.

   UI_Power_2_Set : Nat;
   --  Number of entries set in UI_Power_2;

   UI_Power_10 : array (Int range 0 .. 64) of Uint;
   --  This table is used to memoize exponentiations by powers of 10 in the
   --  same manner as described above for UI_Power_2.

   UI_Power_10_Set : Nat;
   --  Number of entries set in UI_Power_10;

   Uints_Min   : Uint;
   Udigits_Min : Int;
   --  These values are used to make sure that the mark/release mechanism
   --  does not destroy values saved in the U_Power tables. Whenever an
   --  entry is made in the U_Power tables, Uints_Min and Udigits_Min are
   --  updated to protect the entry, and Release never cuts back beyond
   --  these minimum values.

   Int_0 : constant Int := 0;
   Int_1 : constant Int := 1;
   Int_2 : constant Int := 2;
   --  These values are used in some cases where the use of numeric literals
   --  would cause ambiguities (integer vs Uint).

   -----------------------
   -- Local Subprograms --
   -----------------------

   function GCD (Jin, Kin : Int) return Int;
   --  Compute GCD of two integers. Assumes that Jin >= Kin >= 0

   procedure Image_Out
     (Input     : Uint;
      To_Buffer : Boolean;
      Format    : UI_Format);
   --  Common processing for UI_Image and UI_Write, To_Buffer is set
   --  True for UI_Image, and false for UI_Write, and Format is copied
   --  from the Format parameter to UI_Image or UI_Write.

   procedure Init_Operand (UI : Uint; Vec : out UI_Vector);
   pragma Inline (Init_Operand);
   --  This procedure copies the digits from UI or the table into the
   --  vector parameter. The parameter should be of the correct size
   --  as determined by a previous call to N_Digits with UI.

   function Least_Sig_Digit (Arg : Uint) return Int;
   pragma Inline (Least_Sig_Digit);
   --  Returns the Least Significant Digit of Arg quickly. When the given
   --  Uint is less than 2**15, the value returned is the input value, in
   --  this case the result may be negative. It is expected that any use
   --  will mask off unnecessary bits. This is used for finding Arg mod B
   --  where B is a power of two. Hence the actual base is irrelevent as
   --  long as it is a power of two.

   procedure Most_Sig_2_Digits
     (Left,     Right     : Uint;
      Left_Hat, Right_Hat : out Int);
   --  Returns leading two significant digits from the given pair of Uint's.
   --  Mathematically: returns Left / (Base ** K) and Right / (Base ** K)
   --  where K is as small as possible S.T. Right_Hat < Base * Base.
   --  It is required that Left > Right for the algorithm to work.

   function N_Digits (Input : Uint) return Int;
   pragma Inline (N_Digits);
   --  Returns number of "digits" in a Uint

   function Sum_Digits (Left : Uint; Sign : Int) return Int;
   --  If Sign = 1 return the sum of the "digits" of Abs (Left). If the
   --  total has more then one digit then return Sum_Digits of total.

   function Sum_Double_Digits (Left : Uint; Sign : Int) return Int;
   --  Same as above but work in New_Base = Base * Base

   function Vector_To_Uint
     (In_Vec   : UI_Vector;
      Negative : Boolean)
      return     Uint;
   --  Functions that calculate values in UI_Vectors, call this function
   --  to create and return the Uint value.

   ---------
   -- GCD --
   ---------

   function GCD (Jin, Kin : Int) return Int is
      J, K, Tmp : Int;

   begin
      pragma Assert (Jin >= Kin);
      pragma Assert (Kin >= Uint_0);

      J := Jin;
      K := Kin;

      while K /= Uint_0 loop
         Tmp := J mod K;
         J := K;
         K := Tmp;
      end loop;

      return J;
   end GCD;

   ---------------
   -- Image_Out --
   ---------------

   procedure Image_Out
     (Input     : Uint;
      To_Buffer : Boolean;
      Format    : UI_Format)
   is
      Marks  : constant Uintp.Save_Mark := Uintp.Mark;
      Base   : Uint;
      Ainput : Uint;

      Digs_Output : Natural := 0;
      --  Counts digits output. In hex mode, but not in decimal mode, we
      --  put an underline after every four hex digits that are output.

      Exponent : Natural := 0;
      --  If the number is too long to fit in the buffer, we switch to an
      --  approximate output format with an exponent. This variable records
      --  the exponent value.

      function Better_In_Hex return Boolean;
      --  Determines if it is better to generate digits in base 16 (result
      --  is true) or base 10 (result is false). The choice is purely a
      --  matter of convenience and aesthetics, so it does not matter which
      --  value is returned from a correctness point of view.

      procedure Image_Char (C : Character);
      --  Internal procedure to output one character

      procedure Image_Exponent (N : Natural);
      --  Output non-zero exponent. Note that we only use the exponent
      --  form in the buffer case, so we know that To_Buffer is true.

      procedure Image_Uint (U : Uint);
      --  Internal procedure to output characters of non-negative Uint

      -------------------
      -- Better_In_Hex --
      -------------------

      function Better_In_Hex return Boolean is
         T16 : constant Uint := Uint_2 ** Int'(16);
         A   : Uint;

      begin
         A := UI_Abs (Input);

         --  Small values up to 2**16 can always be in decimal

         if A < T16 then
            return False;
         end if;

         --  Otherwise, see if we are a power of 2 or one less than a power
         --  of 2. For the moment these are the only cases printed in hex.

         if A mod Uint_2 = Uint_1 then
            A := A + Uint_1;
         end if;

         loop
            if A mod T16 /= Uint_0 then
               return False;

            else
               A := A / T16;
            end if;

            exit when A < T16;
         end loop;

         while A > Uint_2 loop
            if A mod Uint_2 /= Uint_0 then
               return False;

            else
               A := A / Uint_2;
            end if;
         end loop;

         return True;
      end Better_In_Hex;

      ----------------
      -- Image_Char --
      ----------------

      procedure Image_Char (C : Character) is
      begin
         if To_Buffer then
            if UI_Image_Length + 6 > UI_Image_Max then
               Exponent := Exponent + 1;
            else
               UI_Image_Length := UI_Image_Length + 1;
               UI_Image_Buffer (UI_Image_Length) := C;
            end if;
         else
            Write_Char (C);
         end if;
      end Image_Char;

      --------------------
      -- Image_Exponent --
      --------------------

      procedure Image_Exponent (N : Natural) is
      begin
         if N >= 10 then
            Image_Exponent (N / 10);
         end if;

         UI_Image_Length := UI_Image_Length + 1;
         UI_Image_Buffer (UI_Image_Length) :=
           Character'Val (Character'Pos ('0') + N mod 10);
      end Image_Exponent;

      ----------------
      -- Image_Uint --
      ----------------

      procedure Image_Uint (U : Uint) is
         H : array (Int range 0 .. 15) of Character := "0123456789ABCDEF";

      begin
         if U >= Base then
            Image_Uint (U / Base);
         end if;

         if Digs_Output = 4 and then Base = Uint_16 then
            Image_Char ('_');
            Digs_Output := 0;
         end if;

         Image_Char (H (UI_To_Int (U rem Base)));

         Digs_Output := Digs_Output + 1;
      end Image_Uint;

   --  Start of processing for Image_Out

   begin
      if Input = No_Uint then
         Image_Char ('?');
         return;
      end if;

      UI_Image_Length := 0;

      if Input < Uint_0 then
         Image_Char ('-');
         Ainput := -Input;
      else
         Ainput := Input;
      end if;

      if Format = Hex
        or else (Format = Auto and then Better_In_Hex)
      then
         Base := Uint_16;
         Image_Char ('1');
         Image_Char ('6');
         Image_Char ('#');
         Image_Uint (Ainput);
         Image_Char ('#');

      else
         Base := Uint_10;
         Image_Uint (Ainput);
      end if;

      if Exponent /= 0 then
         UI_Image_Length := UI_Image_Length + 1;
         UI_Image_Buffer (UI_Image_Length) := 'E';
         Image_Exponent (Exponent);
      end if;

      Uintp.Release (Marks);
   end Image_Out;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Uints.Init;
      Udigits.Init;

      Uint_Int_First := UI_From_Int (Int'First);
      Uint_Int_Last  := UI_From_Int (Int'Last);

      UI_Power_2 (0) := Uint_1;
      UI_Power_2_Set := 0;

      UI_Power_10 (0) := Uint_1;
      UI_Power_10_Set := 0;

      Uints_Min := Uints.Last;
      Udigits_Min := Udigits.Last;

   end Initialize;

   -------------------
   -- Init_Operand --
   -------------------

   procedure Init_Operand (UI : Uint; Vec : out UI_Vector) is
      Loc : Int;

   begin
      if Int (UI) <= Int (Uint_Direct_Last) then
         Vec (1) := Int (UI) - Int (Uint_Direct_Bias);
      else
         Loc := Uints.Table (UI).Loc;

         for J in 1 .. Uints.Table (UI).Length loop
            Vec (J) := Udigits.Table (Loc + J - 1);
         end loop;
      end if;
   end Init_Operand;

   ---------------------
   -- Least_Sig_Digit --
   ---------------------

   function Least_Sig_Digit (Arg : Uint) return Int is
   begin
      if Int (Arg) <= Int (Uint_Direct_Last) then

         --  This is a bit more then the "Least Significant Digit"
         --  and might be negative.

         return Int (Arg) - Int (Uint_Direct_Bias);

      else
         return Udigits.Table
            (Uints.Table (Arg).Loc + Uints.Table (Arg).Length - 1);
      end if;
   end Least_Sig_Digit;

   ----------
   -- Mark --
   ----------

   function Mark return Save_Mark is
   begin
      return (Save_Uint => Uints.Last, Save_Udigit => Udigits.Last);
   end Mark;

   -----------------------
   -- Most_Sig_2_Digits --
   -----------------------

   procedure Most_Sig_2_Digits (Left, Right : Uint;
      Left_Hat, Right_Hat : out Int) is

   begin
      pragma Assert (Left >= Right);
      if (Int (Left) <= Int (Uint_Direct_Last)) then
         Left_Hat := (Int (Left) - Int (Uint_Direct_Bias));
         Right_Hat := (Int (Right) - Int (Uint_Direct_Bias));
         return;
      else
         declare
            L1 : constant Int :=
                   Udigits.Table (Uints.Table (Left).Loc);
            L2 : constant Int :=
                   Udigits.Table (Uints.Table (Left).Loc + 1);

         begin
            --  It is not so clear what to return when Arg is negative???

            Left_Hat := abs (L1) * Base + L2;
         end;
      end if;

      declare
         Length_L : constant Int := Uints.Table (Left).Length;
         Length_R : Int;
         R1 : Int;
         R2 : Int;
         T  : Int;

      begin
         if (Int (Right) <= Int (Uint_Direct_Last)) then
            T := (Int (Left) - Int (Uint_Direct_Bias));
            R1 := abs (T / Base);
            R2 := T rem Base;
            Length_R := 2;

         else
            R1 := abs (Udigits.Table (Uints.Table (Right).Loc));
            R2 := Udigits.Table (Uints.Table (Right).Loc + 1);
            Length_R := Uints.Table (Right).Length;
         end if;

         if Length_L = Length_R then
            Right_Hat := R1 * Base + R2;
         elsif Length_L = Length_R + Int_1 then
            Right_Hat := R1;
         else
            Right_Hat := 0;
         end if;
      end;
   end Most_Sig_2_Digits;

   ---------------
   -- N_Digits --
   ---------------

   --  Note: N_Digits returns 1 for No_Uint

   function N_Digits (Input : Uint) return Int is
   begin
      if Int (Input) <= Int (Uint_Direct_Last) then
         return 1;
      else
         return Uints.Table (Input).Length;
      end if;
   end N_Digits;

   --------------
   -- Num_Bits --
   --------------

   function Num_Bits (Input : Uint) return Nat is
      Bits : Nat;
      Num  : Nat;

   begin
      if UI_Is_In_Int_Range (Input) then
         Num := UI_To_Int (Input);
         Bits := 0;

      else
         Bits := Base_Bits * (Uints.Table (Input).Length - 1);
         Num  := abs (Udigits.Table (Uints.Table (Input).Loc));
      end if;

      while Types.">" (Num, 0) loop
         Num := Num / 2;
         Bits := Bits + 1;
      end loop;

      return Bits;
   end Num_Bits;
   ---------
   -- pid --
   ---------

   procedure pid (Input : Uint) is
   begin
      UI_Write (Input, Decimal);
      Write_Eol;
   end pid;

   ---------
   -- pih --
   ---------

   procedure pih (Input : Uint) is
   begin
      UI_Write (Input, Hex);
      Write_Eol;
   end pih;

   -------------
   -- Release --
   -------------

   procedure Release (M : Save_Mark) is
   begin
      Uints.Set_Last   (Uint'Max (M.Save_Uint,   Uints_Min));
      Udigits.Set_Last (Int'Max  (M.Save_Udigit, Udigits_Min));
   end Release;

   ----------------------
   -- Release_And_Save --
   ----------------------

   procedure Release_And_Save (M : Save_Mark; UI : in out Uint) is
   begin
      if Int (UI) <= Int (Uint_Direct_Last) then
         Release (M);

      else
         declare
            UE_Len : Pos := Uints.Table (UI).Length;
            UE_Loc : Int := Uints.Table (UI).Loc;

            UD : Udigits.Table_Type (1 .. UE_Len) :=
                   Udigits.Table (UE_Loc .. UE_Loc + UE_Len - 1);

         begin
            Release (M);

            Uints.Increment_Last;
            UI := Uints.Last;

            Uints.Table (UI) := (UE_Len, Udigits.Last + 1);

            for J in 1 .. UE_Len loop
               Udigits.Increment_Last;
               Udigits.Table (Udigits.Last) := UD (J);
            end loop;
         end;
      end if;
   end Release_And_Save;

   procedure Release_And_Save (M : Save_Mark; UI1, UI2 : in out Uint) is
   begin
      if Int (UI1) <= Int (Uint_Direct_Last) then
         Release_And_Save (M, UI2);

      elsif Int (UI2) <= Int (Uint_Direct_Last) then
         Release_And_Save (M, UI1);

      else
         declare
            UE1_Len : Pos := Uints.Table (UI1).Length;
            UE1_Loc : Int := Uints.Table (UI1).Loc;

            UD1 : Udigits.Table_Type (1 .. UE1_Len) :=
                    Udigits.Table (UE1_Loc .. UE1_Loc + UE1_Len - 1);

            UE2_Len : Pos := Uints.Table (UI2).Length;
            UE2_Loc : Int := Uints.Table (UI2).Loc;

            UD2 : Udigits.Table_Type (1 .. UE2_Len) :=
                    Udigits.Table (UE2_Loc .. UE2_Loc + UE2_Len - 1);

         begin
            Release (M);

            Uints.Increment_Last;
            UI1 := Uints.Last;

            Uints.Table (UI1) := (UE1_Len, Udigits.Last + 1);

            for J in 1 .. UE1_Len loop
               Udigits.Increment_Last;
               Udigits.Table (Udigits.Last) := UD1 (J);
            end loop;

            Uints.Increment_Last;
            UI2 := Uints.Last;

            Uints.Table (UI2) := (UE2_Len, Udigits.Last + 1);

            for J in 1 .. UE2_Len loop
               Udigits.Increment_Last;
               Udigits.Table (Udigits.Last) := UD2 (J);
            end loop;
         end;
      end if;
   end Release_And_Save;

   ----------------
   -- Sum_Digits --
   ----------------

   --  This is done in one pass

   --  Mathematically: assume base congruent to 1 and compute an equivelent
   --  integer to Left.

   --  If Sign = -1 return the alternating sum of the "digits".

   --     D1 - D2 + D3 - D4 + D5 . . .

   --  (where D1 is Least Significant Digit)

   --  Mathematically: assume base congruent to -1 and compute an equivelent
   --  integer to Left.

   --  This is used in Rem and Base is assumed to be 2 ** 15

   --  Note: The next two functions are very similar, any style changes made
   --  to one should be reflected in both.  These would be simpler if we
   --  worked base 2 ** 32.

   function Sum_Digits (Left : Uint; Sign : Int) return Int is
   begin
      pragma Assert (Sign = Int_1 or Sign = Int (-1));

      --  First try simple case;

      if Int (Left) <= Int (Uint_Direct_Last) then
         declare
            Tmp_Int : Int;

         begin
            Tmp_Int := Int (Left) - Int (Uint_Direct_Bias);

            if Tmp_Int >= Base then
               Tmp_Int := (Tmp_Int / Base) +
                  Sign * (Tmp_Int rem Base);

                  --  Now Tmp_Int is in [-(Base - 1) .. 2 * (Base - 1)]

               if Tmp_Int >= Base then

                  --  Sign must be 1.

                  Tmp_Int := (Tmp_Int / Base) + 1;

               end if;

               --  Now Tmp_Int is in [-(Base - 1) .. (Base - 1)]

            end if;

            return Tmp_Int;
         end;

      --  Otherwise full circuit is needed

      else
         declare
            L_Length : Int := N_Digits (Left);
            L_Vec    : UI_Vector (1 .. L_Length);
            Tmp_Int  : Int;
            Carry    : Int;
            Alt      : Int;

         begin
            Init_Operand (Left, L_Vec);
            L_Vec (1) := abs L_Vec (1);
            Tmp_Int := 0;
            Carry := 0;
            Alt := 1;

            for I in reverse 1 .. L_Length loop
               Tmp_Int := Tmp_Int + Alt * (L_Vec (I) + Carry);

               --  Tmp_Int is now between [-2 * Base + 1 .. 2 * Base - 1],
               --  since old Tmp_Int is between [-(Base - 1) .. Base - 1]
               --  and L_Vec is in [0 .. Base - 1] and Carry in [-1 .. 1]

               if Tmp_Int >= Base then
                  Tmp_Int := Tmp_Int - Base;
                  Carry := 1;

               elsif Tmp_Int <= -Base then
                  Tmp_Int := Tmp_Int + Base;
                  Carry := -1;

               else
                  Carry := 0;
               end if;

               --  Tmp_Int is now between [-Base + 1 .. Base - 1]

               Alt := Alt * Sign;
            end loop;

            Tmp_Int := Tmp_Int + Alt * Carry;

            --  Tmp_Int is now between [-Base .. Base]

            if Tmp_Int >= Base then
               Tmp_Int := Tmp_Int - Base + Alt * Sign * 1;

            elsif Tmp_Int <= -Base then
               Tmp_Int := Tmp_Int + Base + Alt * Sign * (-1);
            end if;

            --  Now Tmp_Int is in [-(Base - 1) .. (Base - 1)]

            return Tmp_Int;
         end;
      end if;
   end Sum_Digits;

   -----------------------
   -- Sum_Double_Digits --
   -----------------------

   --  Note: This is used in Rem, Base is assumed to be 2 ** 15

   function Sum_Double_Digits (Left : Uint; Sign : Int) return Int is
   begin
      --  First try simple case;

      pragma Assert (Sign = Int_1 or Sign = Int (-1));

      if Int (Left) <= Int (Uint_Direct_Last) then
         declare
            Tmp_Int  : Int;
            Base_Sqr : constant Int := Base * Base;

         begin
            Tmp_Int := Int (Left) - Int (Uint_Direct_Bias);
            if Tmp_Int >= Base_Sqr then
               Tmp_Int := (Tmp_Int / Base_Sqr) +
                            Sign * (Tmp_Int rem Base_Sqr);

               --  Tmp_Int is now in [-(Base_Sqr - 1) . . 2 * (Base_Sqr - 1)]

               if Tmp_Int >= Base_Sqr then

                  --  Sign must be 1.

                  Tmp_Int := (Tmp_Int - Base_Sqr) + 1;
               end if;
            end if;

            return Tmp_Int;
         end;

      --  Otherwise full circuit is needed
      else
         declare
            L_Length      : Int := N_Digits (Left);
            L_Vec         : UI_Vector (1 .. L_Length);
            Most_Sig_Int  : Int;
            Least_Sig_Int : Int;
            Carry         : Int;
            I             : Int;
            Alt           : Int;

         begin
            Init_Operand (Left, L_Vec);
            L_Vec (1) := abs L_Vec (1);
            Most_Sig_Int := 0;
            Least_Sig_Int := 0;
            Carry := 0;
            Alt := 1;
            I := L_Length;

            while I > Int_1 loop

               Least_Sig_Int := Least_Sig_Int + Alt * (L_Vec (I) + Carry);

               --  Least is in [-2 Base + 1 .. 2 * Base - 1]
               --  Since L_Vec in [0 .. Base - 1] and Carry in [-1 .. 1]
               --  and old Least in [-Base + 1 .. Base - 1]

               if Least_Sig_Int >= Base then
                  Least_Sig_Int := Least_Sig_Int - Base;
                  Carry := 1;

               elsif Least_Sig_Int <= -Base then
                  Least_Sig_Int := Least_Sig_Int + Base;
                  Carry := -1;

               else
                  Carry := 0;
               end if;

               --  Least is now in [-Base + 1 .. Base - 1]

               Most_Sig_Int := Most_Sig_Int + Alt * (L_Vec (I - 1) + Carry);

               --  Most is in [-2 Base + 1 .. 2 * Base - 1]
               --  Since L_Vec in [0 ..  Base - 1] and Carry in  [-1 .. 1]
               --  and old Most in [-Base + 1 .. Base - 1]

               if Most_Sig_Int >= Base then
                  Most_Sig_Int := Most_Sig_Int - Base;
                  Carry := 1;

               elsif Most_Sig_Int <= -Base then
                  Most_Sig_Int := Most_Sig_Int + Base;
                  Carry := -1;
               else
                  Carry := 0;
               end if;

               --  Most is now in [-Base + 1 .. Base - 1]

               I := I - 2;
               Alt := Alt * Sign;
            end loop;

            if I = Int_1 then
               Least_Sig_Int := Least_Sig_Int + Alt * (L_Vec (I) + Carry);
            else
               Least_Sig_Int := Least_Sig_Int + Alt * Carry;
            end if;

            if Least_Sig_Int >= Base then
               Least_Sig_Int := Least_Sig_Int - Base;
               Most_Sig_Int := Most_Sig_Int + Alt * 1;

            elsif Least_Sig_Int <= -Base then
               Least_Sig_Int := Least_Sig_Int + Base;
               Most_Sig_Int := Most_Sig_Int + Alt * (-1);
            end if;

            if Most_Sig_Int >= Base then
               Most_Sig_Int := Most_Sig_Int - Base;
               Alt := Alt * Sign;
               Least_Sig_Int :=
                 Least_Sig_Int + Alt * 1; -- cannot overflow again

            elsif Most_Sig_Int <= -Base then
               Most_Sig_Int := Most_Sig_Int + Base;
               Alt := Alt * Sign;
               Least_Sig_Int :=
                 Least_Sig_Int + Alt * (-1); --  cannot overflow again.
            end if;

            return Most_Sig_Int * Base + Least_Sig_Int;
         end;
      end if;
   end Sum_Double_Digits;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      Uints.Tree_Read;
      Udigits.Tree_Read;

      Tree_Read_Int (Int (Uint_Int_First));
      Tree_Read_Int (Int (Uint_Int_Last));
      Tree_Read_Int (UI_Power_2_Set);
      Tree_Read_Int (UI_Power_10_Set);
      Tree_Read_Int (Int (Uints_Min));
      Tree_Read_Int (Udigits_Min);

      for J in 0 .. UI_Power_2_Set loop
         Tree_Read_Int (Int (UI_Power_2 (J)));
      end loop;

      for J in 0 .. UI_Power_10_Set loop
         Tree_Read_Int (Int (UI_Power_10 (J)));
      end loop;

   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Uints.Tree_Write;
      Udigits.Tree_Write;

      Tree_Write_Int (Int (Uint_Int_First));
      Tree_Write_Int (Int (Uint_Int_Last));
      Tree_Write_Int (UI_Power_2_Set);
      Tree_Write_Int (UI_Power_10_Set);
      Tree_Write_Int (Int (Uints_Min));
      Tree_Write_Int (Udigits_Min);

      for J in 0 .. UI_Power_2_Set loop
         Tree_Write_Int (Int (UI_Power_2 (J)));
      end loop;

      for J in 0 .. UI_Power_10_Set loop
         Tree_Write_Int (Int (UI_Power_10 (J)));
      end loop;

   end Tree_Write;

   -------------
   -- UI_Abs --
   -------------

   function UI_Abs (Right : Uint) return Uint is
   begin
      if Right < Uint_0 then
         return -Right;
      else
         return Right;
      end if;
   end UI_Abs;

   -------------
   -- UI_Add --
   -------------

   function UI_Add (Left : Int; Right : Uint) return Uint is
   begin
      return UI_Add (UI_From_Int (Left), Right);
   end UI_Add;

   function UI_Add (Left : Uint; Right : Int) return Uint is
   begin
      return UI_Add (Left, UI_From_Int (Right));
   end UI_Add;

   function UI_Add (Left : Uint; Right : Uint) return Uint is
   begin
      --  First try simple case where Int "+" can be used;

      if Int (Left) <= Int (Uint_Direct_Last)
        and then Int (Right) <= Int (Uint_Direct_Last)
      then
         return
           UI_From_Int
             ((Int (Left)  - Int (Uint_Direct_Bias)) +
              (Int (Right) - Int (Uint_Direct_Bias)));

      --  Otherwise full circuit is needed

      else
         declare
            L_Length   : Int := N_Digits (Left);
            R_Length   : Int := N_Digits (Right);
            L_Vec      : UI_Vector (1 .. L_Length);
            R_Vec      : UI_Vector (1 .. R_Length);
            Sum_Length : Int;
            Tmp_Int    : Int;
            Carry      : Int;
            Borrow     : Int;
            X_Bigger   : Boolean := False;
            Y_Bigger   : Boolean := False;
            Result_Neg : Boolean := False;

         begin
            Init_Operand (Left, L_Vec);
            Init_Operand (Right, R_Vec);

            --  At least one more than 1 digit, so calculation is needed. First
            --  calculate the number of digits sufficient to hold result.

            if L_Length > R_Length then
               Sum_Length := L_Length + 1;
               X_Bigger := True;
            else
               Sum_Length := R_Length + 1;
               if R_Length > L_Length then Y_Bigger := True; end if;
            end if;

            --  Make copies of the absolute values of L_Vec and R_Vec into
            --  X and Y both with lengths equal to the maximum possibly
            --  needed. This makes looping over the digits much simpler.

            declare
               X      : UI_Vector (1 .. Sum_Length);
               Y      : UI_Vector (1 .. Sum_Length);
               Tmp_UI : UI_Vector (1 .. Sum_Length);

            begin
               for I in 1 .. Sum_Length - L_Length loop
                  X (I) := 0;
               end loop;

               X (Sum_Length - L_Length + 1) := abs L_Vec (1);

               for I in 2 .. L_Length loop
                  X (I + (Sum_Length - L_Length)) := L_Vec (I);
               end loop;

               for I in 1 .. Sum_Length - R_Length loop
                  Y (I) := 0;
               end loop;

               Y (Sum_Length - R_Length + 1) := abs R_Vec (1);

               for I in 2 .. R_Length loop
                  Y (I + (Sum_Length - R_Length)) := R_Vec (I);
               end loop;

               if (L_Vec (1) < Int_0) = (R_Vec (1) < Int_0) then

                  --  Same sign so just add

                  Carry := 0;
                  for I in reverse 1 .. Sum_Length loop
                     Tmp_Int := X (I) + Y (I) + Carry;
                     if Tmp_Int >= Base then
                        Tmp_Int := Tmp_Int - Base;
                        Carry := 1;
                     else
                        Carry := 0;
                     end if;
                     X (I) := Tmp_Int;
                  end loop;

                  return Vector_To_Uint (X, L_Vec (1) < Int_0);

               else
                  --  Find which one has bigger magnitude

                  if not (X_Bigger or Y_Bigger) then
                     for I in L_Vec'Range loop
                        if abs L_Vec (I) > abs R_Vec (I) then
                           X_Bigger := True;
                           exit;
                        elsif abs R_Vec (I) > abs L_Vec (I) then
                           Y_Bigger := True;
                           exit;
                        end if;
                     end loop;
                  end if;

                  --  If they have identical magnitude, just return 0, else
                  --  swap if necessary so that X had the bigger magnitude.
                  --  Determine if result is negative at this time.

                  Result_Neg := False;

                  if not (X_Bigger or Y_Bigger) then
                     return Uint_0;

                  elsif Y_Bigger then
                     if R_Vec (1) < Int_0 then
                        Result_Neg := True;
                     end if;

                     Tmp_UI := X;
                     X := Y;
                     Y := Tmp_UI;

                  else
                     if L_Vec (1) < Int_0 then
                        Result_Neg := True;
                     end if;
                  end if;

                  --  Subtract Y from the bigger X

                  Borrow := 0;

                  for J in reverse 1 .. Sum_Length loop
                     Tmp_Int := X (J) - Y (J) + Borrow;

                     if Tmp_Int < Int_0 then
                        Tmp_Int := Tmp_Int + Base;
                        Borrow := -1;
                     else
                        Borrow := 0;
                     end if;

                     X (J) := Tmp_Int;
                  end loop;

                  return Vector_To_Uint (X, Result_Neg);

               end if;
            end;
         end;
      end if;
   end UI_Add;

   --------------------------
   -- UI_Decimal_Digits_Hi --
   --------------------------

   function UI_Decimal_Digits_Hi (U : Uint) return Nat is
   begin
      --  The maximum value of a "digit" is 32767, which is 5 decimal
      --  digits, so an N_Digit number could take up to 5 times this
      --  number of digits. This is certainly too high for large
      --  numbers but it is not worth worrying about.

      return 5 * N_Digits (U);
   end UI_Decimal_Digits_Hi;

   --------------------------
   -- UI_Decimal_Digits_Lo --
   --------------------------

   function UI_Decimal_Digits_Lo (U : Uint) return Nat is
   begin
      --  The maximum value of a "digit" is 32767, which is more than four
      --  decimal digits, but not a full five digits. The easily computed
      --  minimum number of decimal digits is thus 1 + 4 * the number of
      --  digits. This is certainly too low for large numbers but it is
      --  not worth worrying about.

      return 1 + 4 * (N_Digits (U) - 1);
   end UI_Decimal_Digits_Lo;

   ------------
   -- UI_Div --
   ------------

   function UI_Div (Left : Int; Right : Uint) return Uint is
   begin
      return UI_Div (UI_From_Int (Left), Right);
   end UI_Div;

   function UI_Div (Left : Uint; Right : Int) return Uint is
   begin
      return UI_Div (Left, UI_From_Int (Right));
   end UI_Div;

   function UI_Div (Left, Right : Uint) return Uint is
      L_Length    : constant Int := N_Digits (Left);
      R_Length    : constant Int := N_Digits (Right);
      Q_Length    : constant Int := L_Length - R_Length + 1;
      L_Vec       : UI_Vector (1 .. L_Length);
      R_Vec       : UI_Vector (1 .. R_Length);
      D           : Int;
      Remainder   : Int;
      Tmp_Divisor : Int;
      Carry       : Int;
      Tmp_Int     : Int;
      Tmp_Dig     : Int;

   begin
      pragma Assert (Right /= Uint_0);

      --  Some special cases that are simpler to compute than the general
      --  case are treated first.

      if L_Length = Int_1 and then R_Length = Int_1 then
         return UI_From_Int (UI_To_Int (Left) / UI_To_Int (Right));
      elsif  L_Length < R_Length then
         return Uint_0;
      end if;

      Init_Operand (Left, L_Vec);
      Init_Operand (Right, R_Vec);

      --  Case of right operand is single digit. Here we can simply divide
      --  each digit of the left operand by the divisor, from most to least
      --  significant, carrying the remainder to the next digit (just like
      --  ordinary long division by hand).

      if R_Length = Int_1 then
         Remainder := 0;
         Tmp_Divisor := abs R_Vec (1);

         declare
            Quotient : UI_Vector (1 .. L_Length);

         begin
            for J in L_Vec'Range loop
               Tmp_Int      := Remainder * Base + abs L_Vec (J);
               Quotient (J) := Tmp_Int / Tmp_Divisor;
               Remainder    := Tmp_Int rem Tmp_Divisor;
            end loop;

            return
              Vector_To_Uint
                (Quotient, (L_Vec (1) < Int_0 xor R_Vec (1) < Int_0));
         end;
      end if;

      --  The possible simple cases have been exhausted. Now turn to the
      --  algorithm D from the section of Knuth mentioned at the top of
      --  this package.

      Algorithm_D : declare
         Dividend     : UI_Vector (1 .. L_Length + 1);
         Divisor      : UI_Vector (1 .. R_Length);
         Quotient     : UI_Vector (1 .. Q_Length);
         Divisor_Dig1 : Int;
         Divisor_Dig2 : Int;
         Q_Guess      : Int;

      begin
         --  [ NORMALIZE ] (step D1 in the algorithm). First calculate the
         --  scale d, and then multiply Left and Right (u and v in the book)
         --  by d to get the dividend and divisor to work with.

         D := Base / (abs R_Vec (1) + 1);

         Dividend (1) := 0;
         Dividend (2) := abs L_Vec (1);

         for J in 3 .. L_Length + Int_1 loop
            Dividend (J) := L_Vec (J - 1);
         end loop;

         Divisor (1) := abs R_Vec (1);

         for J in Int_2 .. R_Length loop
            Divisor (J) := R_Vec (J);
         end loop;

         if D > Int_1 then

            --  Multiply Dividend by D

            Carry := 0;
            for J in reverse Dividend'Range loop
               Tmp_Int      := Dividend (J) * D + Carry;
               Dividend (J) := Tmp_Int rem Base;
               Carry        := Tmp_Int / Base;
            end loop;

            --  Multiply Divisor by d.

            Carry := 0;
            for J in reverse Divisor'Range loop
               Tmp_Int      := Divisor (J) * D + Carry;
               Divisor (J)  := Tmp_Int rem Base;
               Carry        := Tmp_Int / Base;
            end loop;

         end if;

         --  Main loop of long division algorithm.

         Divisor_Dig1 := Divisor (1);
         Divisor_Dig2 := Divisor (2);

         for J in Quotient'Range loop

            --  [ CALCULATE Q (hat) ] (step D3 in the algorithm).

            Tmp_Int := Dividend (J) * Base + Dividend (J + 1);

            --  Initial guess

            if Dividend (J) = Divisor_Dig1 then
               Q_Guess := Base - 1;
            else
               Q_Guess := Tmp_Int / Divisor_Dig1;
            end if;

            --  Refine the guess

            while Divisor_Dig2 * Q_Guess >
                  (Tmp_Int - Q_Guess * Divisor_Dig1) * Base + Dividend (J + 2)
            loop
               Q_Guess := Q_Guess - 1;
            end loop;

            --  [ MULTIPLY & SUBTRACT] (step D4). Q_Guess * Divisor is
            --  subtracted from the remaining dividend.

            Carry := 0;
            for K in reverse Divisor'Range loop
               Tmp_Int := Dividend (J + K) - Q_Guess * Divisor (K) + Carry;
               Tmp_Dig := Tmp_Int rem Base;
               Carry   := Tmp_Int / Base;

               if Tmp_Dig < Int_0 then
                  Tmp_Dig := Tmp_Dig + Base;
                  Carry   := Carry - 1;
               end if;

               Dividend (J + K) := Tmp_Dig;
            end loop;

            Dividend (J) := Dividend (J) + Carry;

            --  [ TEST REMAINDER ] & [ ADD BACK ] (steps D5 and D6)
            --  Here there is a slight difference from the book: the last
            --  carry is always added in above and below (cancelling each
            --  other). In fact the dividend going negative is used as
            --  the test.

            --  If the Dividend went negative, then Q_Guess was off by
            --  one, so it is decremented, and the divisor is added back
            --  into the relevant portion of the dividend.

            if Dividend (J) < Int_0 then
               Q_Guess := Q_Guess - 1;

               Carry := 0;
               for K in reverse Divisor'Range loop
                  Tmp_Int := Dividend (J + K) + Divisor (K) + Carry;

                  if Tmp_Int >= Base then
                     Tmp_Int := Tmp_Int - Base;
                     Carry := 1;
                  else
                     Carry := 0;
                  end if;

                  Dividend (J + K) := Tmp_Int;
               end loop;

               Dividend (J) := Dividend (J) + Carry;
            end if;

            --  Finally we can get the next quotient digit

            Quotient (J) := Q_Guess;

         end loop;

         return Vector_To_Uint
           (Quotient, (L_Vec (1) < Int_0 xor R_Vec (1) < Int_0));

      end Algorithm_D;

   end UI_Div;

   ------------
   -- UI_Eq --
   ------------

   function UI_Eq (Left : Int; Right : Uint) return Boolean is
   begin
      return not UI_Ne (UI_From_Int (Left), Right);
   end UI_Eq;

   function UI_Eq (Left : Uint; Right : Int) return Boolean is
   begin
      return not UI_Ne (Left, UI_From_Int (Right));
   end UI_Eq;

   function UI_Eq (Left : Uint; Right : Uint) return Boolean is
   begin
      return not UI_Ne (Left, Right);
   end UI_Eq;

   --------------
   -- UI_Expon --
   --------------

   function UI_Expon (Left : Int; Right : Uint) return Uint is
   begin
      return UI_Expon (UI_From_Int (Left), Right);
   end UI_Expon;

   function UI_Expon (Left : Uint; Right : Int) return Uint is
   begin
      return UI_Expon (Left, UI_From_Int (Right));
   end UI_Expon;

   function UI_Expon (Left : Int; Right : Int) return Uint is
   begin
      return UI_Expon (UI_From_Int (Left), UI_From_Int (Right));
   end UI_Expon;

   function UI_Expon (Left : Uint; Right : Uint) return Uint is
   begin
      pragma Assert (Right >= Uint_0);

      --  Any value raised to power of 0 is 1

      if Right = Uint_0 then
         return Uint_1;

      --  0 to any positive power is 0.

      elsif Left = Uint_0 then
         return Uint_0;

      --  1 to any power is 1

      elsif Left = Uint_1 then
         return Uint_1;

      --  Any value raised to power of 1 is that value

      elsif Right = Uint_1 then
         return Left;

      --  Cases which can be done by table lookup

      elsif Right <= Uint_64 then

         --  2 ** N for N in 2 .. 64

         if Left = Uint_2 then
            declare
               Right_Int : constant Int :=
                             Int (Right) - Int (Uint_Direct_Bias);

            begin
               if Right_Int > UI_Power_2_Set then
                  for J in UI_Power_2_Set + Int_1 .. Right_Int loop
                     UI_Power_2 (J) := UI_Power_2 (J - Int_1) * Int_2;
                     Uints_Min := Uints.Last;
                     Udigits_Min := Udigits.Last;
                  end loop;

                  UI_Power_2_Set := Right_Int;
               end if;

               return UI_Power_2 (Right_Int);
            end;

         --  10 ** N for N in 2 .. 64

         elsif Left = Uint_10 then
            declare
               Right_Int : constant Int :=
                             Int (Right) - Int (Uint_Direct_Bias);

            begin
               if Right_Int > UI_Power_10_Set then
                  for J in UI_Power_10_Set + Int_1 .. Right_Int loop
                     UI_Power_10 (J) := UI_Power_10 (J - Int_1) * Int (10);
                     Uints_Min := Uints.Last;
                     Udigits_Min := Udigits.Last;
                  end loop;

                  UI_Power_10_Set := Right_Int;
               end if;

               return UI_Power_10 (Right_Int);
            end;
         end if;
      end if;

      --  If we fall through, then we have the general case (see Knuth 4.6.3)

      declare
         N       : Uint := Right;
         Squares : Uint := Left;
         Result  : Uint := Uint_1;
         M       : constant Uintp.Save_Mark := Uintp.Mark;

      begin
         loop
            if (Least_Sig_Digit (N) mod Int_2) = Int_1 then
               Result := Result * Squares;
            end if;

            N := N / Uint_2;
            exit when N = Uint_0;
            Squares := Squares *  Squares;
         end loop;

         Uintp.Release_And_Save (M, Result);
         return Result;
      end;
   end UI_Expon;

   ------------------
   -- UI_From_Int --
   ------------------

   function UI_From_Int (Input : Int) return Uint is
   begin
      --  The case -Base < Input < Base is the usual and simple case.

      if -Base < Input and then Input < Base then
         return Uint (Int (Uint_Direct_Bias) + Input);

      --  For values of larger magnitude, compute digits into a vector and
      --  call Vector_To_Uint.

      else
         declare
            Max_For_Int : constant := 4;
            --  Base is defined so that 4 Uint digits is sufficient
            --  to hold the largest possible Int value.

            V : UI_Vector (1 .. Max_For_Int);

            Temp_Integer : Int;

         begin
            for I in V'Range loop
               V (I) := 0;
            end loop;

            Temp_Integer := Input;

            for I in reverse V'Range loop
               V (I) := abs (Temp_Integer rem Base);
               Temp_Integer := Temp_Integer / Base;
            end loop;

            return Vector_To_Uint (V, Input < Int_0);
         end;
      end if;
   end UI_From_Int;

   ------------
   -- UI_GCD --
   ------------

   --  Lehmer's algorithm for GCD.

   --  The idea is to avoid using multiple precision arithmetic wherever
   --  possible, substituting Int arithmetic instead. See Knuth volume II,
   --  Algorithm L (page 329).

   --  We use the same notation as Knuth (U_Hat standing for the obvious!)

   function UI_GCD (Uin, Vin : Uint) return Uint is
      U, V : Uint;
      --  Copies of Uin and Vin

      U_Hat, V_Hat : Int;
      --  The most Significant digits of U,V

      A, B, C, D, T, Q, Den1, Den2 : Int;

      Tmp_UI : Uint;

   begin
      pragma Assert (Uin >= Vin);
      pragma Assert (Vin >= Uint_0);

      U := Uin;
      V := Vin;

      loop
         if Int (V) <= Int (Uint_Direct_Last) then
            if V = Uint_0 then
               return U;
            else
               return UI_From_Int (GCD (UI_To_Int (V), UI_To_Int (U rem V)));
            end if;
         end if;

         Most_Sig_2_Digits (U, V, U_Hat, V_Hat);
         A := 1;
         B := 0;
         C := 0;
         D := 1;

         loop
            --  We might overflow and get division by zero here. This just
            --  means we can not take the single precision step

            Den1 := V_Hat + C;
            Den2 := V_Hat + D;
            exit when (Den1 * Den2) = Int_0;

            --  Compute Q, the trial quotient

            Q := (U_Hat + A) / Den1;

            exit when Q /= ((U_Hat + B) / Den2);

            --  A single precision step Euclid step will give same answer as
            --  a multiprecision one.

            T := A - (Q * C);
            A := C;
            C := T;

            T := B - (Q * D);
            B := D;
            D := T;

            T := U_Hat - (Q * V_Hat);
            U_Hat := V_Hat;
            V_Hat := T;

         end loop;

         --  Take a multiprecision Euclid step

         if B = Int_0 then

            --  No single precision steps take a regular Euclid step.

            Tmp_UI := U rem V;
            U := V;
            V := Tmp_UI;

         else
            --  Use prior single precision steps to compute this Euclid step.

            --  Fixed bug 1415-008 spends 80% of its time working on this
            --  step. Perhaps we need a special case Int / Uint dot
            --  product to speed things up. ???

            --  Alternatively we could increase the single precision
            --  iterations to handle Uint's of some small size ( <5
            --  digits?). Then we would have more iterations on small Uint.
            --  Fixed bug 1415-008 only gets 5 (on average) single
            --  precision iterations per large iteration. ???

            Tmp_UI := (UI_From_Int (A) * U) + (UI_From_Int (B) * V);
            V := (UI_From_Int (C) * U) + (UI_From_Int (D) * V);
            U := Tmp_UI;
         end if;
      end loop;
   end UI_GCD;

   ------------
   -- UI_Ge --
   ------------

   function UI_Ge (Left : Int; Right : Uint) return Boolean is
   begin
      return not UI_Lt (UI_From_Int (Left), Right);
   end UI_Ge;

   function UI_Ge (Left : Uint; Right : Int) return Boolean is
   begin
      return not UI_Lt (Left, UI_From_Int (Right));
   end UI_Ge;

   function UI_Ge (Left : Uint; Right : Uint) return Boolean is
   begin
      return not UI_Lt (Left, Right);
   end UI_Ge;

   ------------
   -- UI_Gt --
   ------------

   function UI_Gt (Left : Int; Right : Uint) return Boolean is
   begin
      return UI_Lt (Right, UI_From_Int (Left));
   end UI_Gt;

   function UI_Gt (Left : Uint; Right : Int) return Boolean is
   begin
      return UI_Lt (UI_From_Int (Right), Left);
   end UI_Gt;

   function UI_Gt (Left : Uint; Right : Uint) return Boolean is
   begin
      return UI_Lt (Right, Left);
   end UI_Gt;

   --------------
   -- UI_Halve --
   --------------

   function UI_Halve (Arg : Uint) return Uint is
      --  ???This still has bugs in it. Do not use yet.
      --  In particular UI_Halve (-UI_From_Int (Base)) causes
      --  segmentation violation on a pentium.

   begin
      pragma Assert (False); -- ???
      if Int (Arg) <= Int (Uint_Direct_Last) then
         return Uint (
            (Int (Arg) - Int (Uint_Direct_Bias)) / Int_2 +
               Int (Uint_Direct_Bias));

      else
         declare
            Result : Uint := Uint (Uints.Last + Int_1);
            Carry  : Int;
            Length : Int;
            Neg    : constant Boolean :=
                       Udigits.Table (Uints.Table (Arg).Loc) < Int_0;

         begin
            Uints.Increment_Last;
            Uints.Table (Result).Loc := Udigits.Last + 1;

            Carry := 0;
            for J in Int range 0 .. Uints.Table (Arg).Length - 1 loop
               Udigits.Increment_Last;

               declare
                  Input : constant Int :=
                            abs Int
                              (Udigits.Table (Uints.Table (Arg).Loc + J));

               begin
                  Udigits.Table (Uints.Table (Result).Loc + J) :=
                    Carry * (Base / 2) + Input / 2;
                  Carry := Input mod 2;
               end;
            end loop;

            --  Check result got one digit smaller. Note we leave an unused
            --  digit in the Udigits table in this case, which does not matter

            if Int (Udigits.Table (Uints.Table (Result).Loc)) = Int_0 then
               Uints.Table (Result).Loc := Uints.Table (Result).Loc + 1;
            end if;

            Length := Udigits.Last + 1 - Uints.Table (Result).Loc;

            --  Case of length got reduced to one digit

            if Length = Int_1 then

               declare
                  Result_Int : constant Int := Udigits.Table (Udigits.Last);

               begin
                  Udigits.Set_Last (Uints.Table (Result).Loc - 1);
                  Uints.Decrement_Last;

                  if Neg then
                     return Uint (Int (Uint_Direct_Bias) - Result_Int);
                  else
                     return Uint (Int (Uint_Direct_Bias) + Result_Int);
                  end if;
               end;

            --  Normal case of result length > 1


            else
               if Neg then
                  Udigits.Table (Uints.Table (Result).Loc) :=
                     -Udigits.Table (Uints.Table (Result).Loc);
               end if;
               return Result;
            end if;
         end;
      end if;
   end UI_Halve;

   ---------------
   -- UI_Image --
   ---------------

   procedure UI_Image (Input : Uint; Format : UI_Format := Auto) is
   begin
      Image_Out (Input, True, Format);
   end UI_Image;

   -------------------------
   -- UI_Is_In_Int_Range --
   -------------------------

   function UI_Is_In_Int_Range (Input : Uint) return Boolean is
   begin
      --  Make sure we don't get called before Initialize

      pragma Assert (Uint_Int_First /= Uint_0);

      return Input >= Uint_Int_First
        and then Input <= Uint_Int_Last;
   end UI_Is_In_Int_Range;

   ------------
   -- UI_Le --
   ------------

   function UI_Le (Left : Int; Right : Uint) return Boolean is
   begin
      return not UI_Lt (Right, UI_From_Int (Left));
   end UI_Le;

   function UI_Le (Left : Uint; Right : Int) return Boolean is
   begin
      return not UI_Lt (UI_From_Int (Right), Left);
   end UI_Le;

   function UI_Le (Left : Uint; Right : Uint) return Boolean is
   begin
      return not UI_Lt (Right, Left);
   end UI_Le;

   ------------
   -- UI_Lt --
   ------------

   function UI_Lt (Left : Int; Right : Uint) return Boolean is
   begin
      return UI_Lt (UI_From_Int (Left), Right);
   end UI_Lt;

   function UI_Lt (Left : Uint; Right : Int) return Boolean is
   begin
      return UI_Lt (Left, UI_From_Int (Right));
   end UI_Lt;

   function UI_Lt (Left : Uint; Right : Uint) return Boolean is
      L_Length : constant Int := N_Digits (Left);
      R_Length : constant Int := N_Digits (Right);

   begin
      --  Quick processing for identical arguments

      if Int (Left) = Int (Right) then
         return False;

      --  Quick processing for both arguments one digit long

      elsif L_Length = Int_1 and then R_Length = Int_1 then
         return Int (Left) < Int (Right);

      --  At least one argument is more than one digit long

      else
         declare
            L_Vec : UI_Vector (1 .. L_Length);
            R_Vec : UI_Vector (1 .. R_Length);

         begin
            Init_Operand (Left, L_Vec);
            Init_Operand (Right, R_Vec);

            if L_Vec (1) < Int_0 then

               --  First argument negative, second argument non-negative

               if R_Vec (1) >= Int_0 then
                  return True;

               --  Both arguments negative

               else
                  if L_Length /= R_Length then
                     return L_Length > R_Length;

                  elsif L_Vec (1) /= R_Vec (1) then
                     return L_Vec (1) < R_Vec (1);

                  else
                     for J in 2 .. L_Vec'Last loop
                        if L_Vec (J) /= R_Vec (J) then
                           return L_Vec (J) > R_Vec (J);
                        end if;
                     end loop;

                     return False;
                  end if;
               end if;

            else
               --  First argument non-negative, second argument negative

               if R_Vec (1) < Int_0 then
                  return False;

               --  Both arguments non-negative

               else
                  if L_Length /= R_Length then
                     return L_Length < R_Length;
                  else
                     for J in L_Vec'Range loop
                        if L_Vec (J) /= R_Vec (J) then
                           return L_Vec (J) < R_Vec (J);
                        end if;
                     end loop;

                     return False;
                  end if;
               end if;
            end if;
         end;
      end if;
   end UI_Lt;

   ------------
   -- UI_Max --
   ------------

   function UI_Max (Left : Int; Right : Uint) return Uint is
   begin
      return UI_Max (UI_From_Int (Left), Right);
   end UI_Max;

   function UI_Max (Left : Uint; Right : Int) return Uint is
   begin
      return UI_Max (Left, UI_From_Int (Right));
   end UI_Max;

   function UI_Max (Left : Uint; Right : Uint) return Uint is
   begin
      if Left >= Right then
         return Left;
      else
         return Right;
      end if;
   end UI_Max;

   ------------
   -- UI_Min --
   ------------

   function UI_Min (Left : Int; Right : Uint) return Uint is
   begin
      return UI_Min (UI_From_Int (Left), Right);
   end UI_Min;

   function UI_Min (Left : Uint; Right : Int) return Uint is
   begin
      return UI_Min (Left, UI_From_Int (Right));
   end UI_Min;

   function UI_Min (Left : Uint; Right : Uint) return Uint is
   begin
      if Left <= Right then
         return Left;
      else
         return Right;
      end if;
   end UI_Min;

   -------------
   -- UI_Mod --
   -------------

   function UI_Mod (Left : Int; Right : Uint) return Uint is
   begin
      return UI_Mod (UI_From_Int (Left), Right);
   end UI_Mod;

   function UI_Mod (Left : Uint; Right : Int) return Uint is
   begin
      return UI_Mod (Left, UI_From_Int (Right));
   end UI_Mod;

   function UI_Mod (Left : Uint; Right : Uint) return Uint is
      Urem : constant Uint := Left rem Right;

   begin
      if (Left < Uint_0) = (Right < Uint_0)
        or else Urem = Uint_0
      then
         return Urem;
      else
         return Right + Urem;
      end if;
   end UI_Mod;

   ------------
   -- UI_Mul --
   ------------

   function UI_Mul (Left : Int; Right : Uint) return Uint is
   begin
      return UI_Mul (UI_From_Int (Left), Right);
   end UI_Mul;

   function UI_Mul (Left : Uint; Right : Int) return Uint is
   begin
      return UI_Mul (Left, UI_From_Int (Right));
   end UI_Mul;

   function UI_Mul (Left : Uint; Right : Uint) return Uint is
      L_Length : constant Int := N_Digits (Left);
      R_Length : constant Int := N_Digits (Right);
      L_Vec    : UI_Vector (1 .. L_Length);
      R_Vec    : UI_Vector (1 .. R_Length);
      Neg      : Boolean;

   begin
      --  Simple case of single length operands. Note that we chose our base
      --  precisely to make this simple (the product always fits in Int range)

      if L_Length = Int_1 and then R_Length = Int_1 then
         return UI_From_Int
           ((Int (Left)  - Int (Uint_Direct_Bias)) *
            (Int (Right) - Int (Uint_Direct_Bias)));
      end if;

      --  Otherwise we have the general case (Algorithm M in Knuth)

      Init_Operand (Left, L_Vec);
      Init_Operand (Right, R_Vec);
      Neg := (L_Vec (1) < Int_0) xor (R_Vec (1) < Int_0);
      L_Vec (1) := abs (L_Vec (1));
      R_Vec (1) := abs (R_Vec (1));

      Algorithm_M : declare
         Product : UI_Vector (1 .. L_Length + R_Length);
         Tmp_Sum : Int;
         Carry   : Int;

      begin
         for J in Product'Range loop
            Product (J) := 0;
         end loop;

         for J in reverse R_Vec'Range loop
            Carry := 0;
            for K in reverse L_Vec'Range loop
               Tmp_Sum :=
                 L_Vec (K) * R_Vec (J) + Product (J + K) + Carry;
               Product (J + K) := Tmp_Sum rem Base;
               Carry := Tmp_Sum / Base;
            end loop;

            Product (J) := Carry;
         end loop;

         return Vector_To_Uint (Product, Neg);
      end Algorithm_M;
   end UI_Mul;

   ------------
   -- UI_Ne --
   ------------

   function UI_Ne (Left : Int; Right : Uint) return Boolean is
   begin
      return UI_Ne (UI_From_Int (Left), Right);
   end UI_Ne;

   function UI_Ne (Left : Uint; Right : Int) return Boolean is
   begin
      return UI_Ne (Left, UI_From_Int (Right));
   end UI_Ne;

   function UI_Ne (Left : Uint; Right : Uint) return Boolean is
      Size      : constant Int := N_Digits (Left);
      Left_Loc  : Int;
      Right_Loc : Int;

   begin
      --  Quick processing for identical arguments. Note that this takes
      --  care of the case of two No_Uint arguments.

      if Int (Left) = Int (Right) then
         return False;

      --  Certainly not equal if sizes are different

      elsif Size /= N_Digits (Right) then
         return True;

      --  Quick processing for one digit case. Note that this takes care
      --  of when one operand is No_Uint and the other is not.

      elsif Size = Int_1 then
         return Int (Left) /= Int (Right);

      --  Otherwise do comparison

      else
         Left_Loc  := Uints.Table (Left).Loc;
         Right_Loc := Uints.Table (Right).Loc;

         for J in Int_0 .. Size - Int_1 loop
            if Udigits.Table (Left_Loc + J) /=
               Udigits.Table (Right_Loc + J)
            then
               return True;
            end if;
         end loop;

         return False;
      end if;
   end UI_Ne;

   ----------------
   -- UI_Negate --
   ----------------

   function UI_Negate (Right : Uint) return Uint is
   begin
      --  Quick processing for single digit case. Note that the negative of
      --  a single digit value always fits in a single digit, because the
      --  range is symmetrical.

      if Int (Right) <= Int (Uint_Direct_Last) then
         return Uint (Int (Uint_Direct_Bias) -
                     (Int (Right) - Int (Uint_Direct_Bias)));

      --  Else copy the value to the end of the table, negating 1st digit

      else
         declare
            Length : Int := Uints.Table (Right).Length;
            Loc    : Int := Uints.Table (Right).Loc;

         begin
            Uints.Increment_Last;
            Uints.Table (Uints.Last).Length := Length;
            Uints.Table (Uints.Last).Loc := Udigits.Last + 1;

            Udigits.Increment_Last;
            Udigits.Table (Udigits.Last) := -Udigits.Table (Loc);

            for Idx in 2 .. Length loop
               Udigits.Increment_Last;
               Udigits.Table (Udigits.Last) := Udigits.Table (Loc + Idx - 1);
            end loop;

            return Uints.Last;
         end;
      end if;
   end UI_Negate;

   -------------
   -- UI_Rem --
   -------------

   function UI_Rem (Left : Int; Right : Uint) return Uint is
   begin
      return UI_Rem (UI_From_Int (Left), Right);
   end UI_Rem;

   function UI_Rem (Left : Uint; Right : Int) return Uint is
   begin
      return UI_Rem (Left, UI_From_Int (Right));
   end UI_Rem;

   function UI_Rem (Left, Right : Uint) return Uint is
      Sign : Int;
      Tmp  : Int;

   begin
      pragma Assert (Right /= Uint_0);

      if N_Digits (Right) = Int_1 then
         if N_Digits (Left) = Int_1 then
            return
              UI_From_Int ((Int (Left) - Int (Uint_Direct_Bias)) rem
                            (Int (Right) - Int (Uint_Direct_Bias)));

         else
            --  Special cases when Right is less than 13 and Left is larger
            --  larger than one digit. All of these algorithms depend on the
            --  base being 2 ** 15 We work with Abs (Left) and Abs(Right)
            --  then multiply result by Sign (Left)

            if (Right <= Uint_12) and then (Right >= Uint_Minus_12) then

               if (Left < Uint_0) then
                  Sign := -1;
               else
                  Sign := 1;
               end if;

               --  All cases are listed, grouped by mathematical method
               --  It is not inefficient to do have this case list out
               --  of order since GCC sorts the cases we list.

               case UI_To_Int (UI_Abs (Right)) is

                  when 1 =>
                     return Uint_0;

                  --  Powers of two are simple AND's with LS Left Digit
                  --  GCC will recognise these constants as powers of 2
                  --  and replace the rem with simpler operations where
                  --  possible.
                  --  Least_Sig_Digit might return Negative numbers.

                  when 2 =>
                     return UI_From_Int (
                        Sign * (Least_Sig_Digit (Left) mod 2));
                  when 4 =>
                     return UI_From_Int (
                        Sign * (Least_Sig_Digit (Left) mod 4));
                  when 8 =>
                     return UI_From_Int (
                        Sign * (Least_Sig_Digit (Left) mod 8));

                  --  Some number theoretic tricks:
                  --  If B Rem Right = 1 then
                  --  Left Rem Right = Sum_Of_Digits_Base_B (Left) Rem Right

                  --  Note: 2^32 mod 3 = 1

                  when 3 =>
                     return UI_From_Int (
                        Sign * (Sum_Double_Digits (Left, 1) rem Int (3)));

                  --  Note: 2^15 mod 7 = 1

                  when 7 =>
                     return UI_From_Int (
                        Sign * (Sum_Digits (Left, 1) rem Int (7)));

                  --  Note: 2^32 mod 5 = -1
                  --  Alternating sums might be negative, but rem is always
                  --  positive hence we must use mod here.

                  when 5 =>
                     Tmp := Sum_Double_Digits (Left, -1) mod Int (5);
                     return UI_From_Int (Sign * Tmp);

                  --  Note: 2^15 mod 9 = -1
                  --  Alternating sums might be negative, but rem is always
                  --  positive hence we must use mod here.

                  when 9  =>
                     Tmp := Sum_Digits (Left, -1) mod Int (9);
                     return UI_From_Int (Sign * Tmp);

                  --  Note: 2^15 mod 11 = -1
                  --  Alternating sums might be negative, but rem is always
                  --  positive hence we must use mod here.

                  when 11 =>
                     Tmp := Sum_Digits (Left, -1) mod Int (11);
                     return UI_From_Int (Sign * Tmp);

                  --  Now resort to Chinese Remainder theorem
                  --  to reduce 6, 10, 12 to previous special cases
                  --  There is no reason we could not add more cases
                  --  like these if it proves useful.
                  --  Perhaps we should go up to 16, however
                  --  I have no "trick" for 13.

                  --  To find u mod m we:
                  --  Pick m1, m2 S.T.
                  --     GCD(m1, m2) = 1 AND m = (m1 * m2).
                  --  Next we pick (Basis) M1, M2 small S.T.
                  --     (M1 mod m1) = (M2 mod m2) = 1 AND
                  --     (M1 mod m2) = (M2 mod m1) = 0

                  --  So u mod m  = (u1 * M1 + u2 * M2) mod m
                  --  Where u1 = (u mod m1) AND u2 = (u mod m2);
                  --  Under typical circumstances the last mod m
                  --  can be done with a (possible) single subtraction.

                  --  m1 = 2; m2 = 3; M1 = 3; M2 = 4;

                  when 6  =>
                     Tmp := 3 * (Least_Sig_Digit (Left) rem 2)
                        + 4 * (Sum_Double_Digits (Left, 1) rem 3);
                     return UI_From_Int (Sign * (Tmp rem 6));

                  --  m1 = 2; m2 = 5; M1 = 5; M2 = 6;

                  when 10 =>
                     Tmp := 5 * (Least_Sig_Digit (Left) rem 2)
                        + 6 * (Sum_Double_Digits (Left, -1) mod 5);
                     return UI_From_Int (Sign * (Tmp rem 10));

                  --  m1 = 3; m2 = 4; M1 = 4; M2 = 9;

                  when 12 =>
                     Tmp := 4 * (Sum_Double_Digits (Left, 1) rem 3)
                        + 9 * (Least_Sig_Digit (Left) rem 4);
                     return UI_From_Int (Sign * (Tmp rem 12));

                  --  We can't reach this because of enclosing if statement

                  when others =>
                     pragma Assert (False);
                     raise Program_Error;
               end case;

            end if;

            --  Else fall through to general case.

            --  ???This needs to be improved. We have the Rem when we do the
            --  Div. Div throws it away!

            --  The special case Length (Left) = Length(right) = 1 in Div
            --  looks slow. It uses UI_To_Int when Int should suffice. ???
         end if;
      end if;

      return Left - (Left / Right) * Right;
   end UI_Rem;

   ------------
   -- UI_Sub --
   ------------

   function UI_Sub (Left : Int; Right : Uint) return Uint is
   begin
      return UI_Add (Left, -Right);
   end UI_Sub;

   function UI_Sub (Left : Uint; Right : Int) return Uint is
   begin
      return UI_Add (Left, -Right);
   end UI_Sub;

   function UI_Sub (Left : Uint; Right : Uint) return Uint is
   begin
      return UI_Add (Left, -Right);
   end UI_Sub;

   ----------------
   -- UI_To_Int --
   ----------------

   function UI_To_Int (Input : Uint) return Int is
   begin
      if Int (Input) <= Int (Uint_Direct_Last) then
         return Int (Input) - Int (Uint_Direct_Bias);

      --  Case of input is more than one digit

      else
         declare
            In_Length : constant Int := N_Digits (Input);
            In_Vec    : UI_Vector (1 .. In_Length);
            Ret_Int   : Int;

         begin
            --  Uints of more than one digit could be outside the range for
            --  Ints. Caller should have checked for this if not certain.
            --  Fatal error to attempt to convert from value outside Int'Range.

            pragma Assert (UI_Is_In_Int_Range (Input));

            --  Otherwise, proceed ahead, we are OK

            Init_Operand (Input, In_Vec);
            Ret_Int := 0;

            --  Calculate -|Input| and then negates if value is positive.
            --  This handles our current definition of Int (based on
            --  2s complement). Is it secure enough?

            for Idx in In_Vec'Range loop
               Ret_Int := Ret_Int * Base - abs In_Vec (Idx);
            end loop;

            if In_Vec (1) < Int_0 then
               return Ret_Int;
            else
               return -Ret_Int;
            end if;
         end;
      end if;
   end UI_To_Int;

   --------------
   -- UI_Write --
   --------------

   procedure UI_Write (Input : Uint; Format : UI_Format := Auto) is
   begin
      Image_Out (Input, False, Format);
   end UI_Write;

   ---------------------
   -- Vector_To_Uint --
   ---------------------

   function Vector_To_Uint
     (In_Vec   : UI_Vector;
      Negative : Boolean)
      return     Uint
   is
      Size : Int;

   begin
      --  The vector can contain leading zeros. These are not stored in the
      --  table, so loop through the vector looking for first non-zero digit

      for J in In_Vec'Range loop
         if In_Vec (J) /= Int_0 then

            --  The length of the value is the length of the rest of the vector

            Size := In_Vec'Last - J + 1;

            if Size = Int_1 then
               if Negative then
                  return Uint (Int (Uint_Direct_Bias) - In_Vec (J));
               else
                  return Uint (Int (Uint_Direct_Bias) + In_Vec (J));
               end if;
            end if;

            --  The value takes more than one digit, so it is stored in the
            --  table. Expand the table to contain the count and digits.
            --  the index of the first new location is the return value.

            Uints.Increment_Last;
            Uints.Table (Uints.Last).Length := Size;
            Uints.Table (Uints.Last).Loc    := Udigits.Last + 1;

            Udigits.Increment_Last;

            if Negative then
               Udigits.Table (Udigits.Last) := -In_Vec (J);
            else
               Udigits.Table (Udigits.Last) := +In_Vec (J);
            end if;

            for K in 2 .. Size loop
               Udigits.Increment_Last;
               Udigits.Table (Udigits.Last) := In_Vec (J + K - 1);
            end loop;

            return Uints.Last;
         end if;
      end loop;

      --  Dropped through loop only if vector contained all zeros

      return Uint_0;
   end Vector_To_Uint;

end Uintp;
