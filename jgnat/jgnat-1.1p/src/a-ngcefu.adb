------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     A D A . N U M E R I C S . G C E F                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $                              --
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
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

with Ada.Numerics.Generic_Elementary_Functions;

package body Ada.Numerics.Generic_Complex_Elementary_Functions is

   package Elementary_Functions is new
      Ada.Numerics.Generic_Elementary_Functions (Real'Base);
   use Elementary_Functions;

   PI      : constant := 3.14159_26535_89793_23846_26433_83279_50288_41971;
   PI_2    : constant := PI / 2.0;
   Log_Two : constant := 0.69314_71805_59945_30941_72321_21458_17656_80755;

   Epsilon                 : Real'Base;
   Square_Root_Epsilon     : Real'Base;
   Inv_Square_Root_Epsilon : Real'Base;
   Root_Root_Epsilon       : Real'Base;
   Log_Inverse_Epsilon_2   : Real'Base;

   Complex_Zero : constant Complex := Compose_From_Cartesian (0.0,  0.0);
   Complex_One  : constant Complex := Compose_From_Cartesian (1.0,  0.0);
   Complex_I    : constant Complex := Compose_From_Cartesian (0.0,  1.0);
   Half_Pi      : constant Complex := Compose_From_Cartesian (PI_2, 0.0);

   ----------
   -- Sqrt --
   ----------

   function Sqrt (X : Complex) return Complex is
      ReX : constant Real'Base := Re (X);
      ImX : constant Real'Base := Im (X);
      XR  : constant Real'Base := abs Re (X);
      YR  : constant Real'Base := abs Im (X);
      R   : Real'Base;
      R_X : Real'Base;
      R_Y : Real'Base;

   begin
      --  Deal with pure real case, see (RM G.1.2(39))

      if ImX = 0.0 then
         if ReX > 0.0 then
            return
              Compose_From_Cartesian
                (Sqrt (ReX), 0.0);

         elsif ReX = 0.0 then
            return X;

         else
            return
              Compose_From_Cartesian
                (0.0, Real'Copy_Sign (Sqrt (-ReX), ImX));
         end if;

      elsif ReX = 0.0 then
         R_X := Sqrt (YR / 2.0);

         if ImX > 0.0 then
            return Compose_From_Cartesian (R_X, R_X);
         else
            return Compose_From_Cartesian (R_X, -R_X);
         end if;

      else
         R  := Sqrt (XR ** 2 + YR ** 2);

         --  If the square of the modulus overflows, try rescaling the
         --  real and imaginary parts. We cannot depend on an exception
         --  being raised on all targets.

         if R > Real'Base'Last then
            raise Constraint_Error;
         end if;

         --  We are solving the system

         --  XR = R_X ** 2 - Y_R ** 2      (1)
         --  YR = 2.0 * R_X * R_Y          (2)
         --
         --  The symmetric solution involves square roots for both R_X and
         --  R_Y, but it is more accurate to use the square root with the
         --  larger argument for either R_X or R_Y, and equation (2) for the
         --  other.

         if ReX < 0.0 then
            R_Y := Sqrt (0.5 * (R - ReX));
            R_X := YR / (2.0 * R_Y);

         else
            R_X := Sqrt (0.5 * (R + ReX));
            R_Y := YR / (2.0 * R_X);
         end if;
      end if;

      if Im (X) < 0.0 then                 -- halve angle, Sqrt of magnitude
         R_Y := -R_Y;
      end if;
      return Compose_From_Cartesian (R_X, R_Y);

   exception
      when Constraint_Error =>

         --  Rescale and try again.

         R := Modulus (Compose_From_Cartesian (Re (X / 4.0), Im (X / 4.0)));
         R_X := 2.0 * Sqrt (0.5 * R + 0.5 * Re (X / 4.0));
         R_Y := 2.0 * Sqrt (0.5 * R - 0.5 * Re (X / 4.0));

         if Im (X) < 0.0 then -- halve angle, Sqrt of magnitude
            R_Y := -R_Y;
         end if;

         return Compose_From_Cartesian (R_X, R_Y);
   end Sqrt;

   ---------
   -- Log --
   ---------

   function Log (X : Complex) return Complex is
      ReX : Real'Base;
      ImX : Real'Base;
      Z   : Complex;

   begin
      if Re (X) = 0.0 and then Im (X) = 0.0 then
         raise Constraint_Error;

      elsif abs (1.0 - Re (X)) < Root_Root_Epsilon
        and then abs Im (X) < Root_Root_Epsilon
      then
         Z := X;
         Set_Re (Z, Re (Z) - 1.0);

         return (1.0 - (1.0 / 2.0 -
                       (1.0 / 3.0 - (1.0 / 4.0) * Z) * Z) * Z) * Z;
      end if;

      begin
         ReX := Log (Modulus (X));

      exception
         when Constraint_Error =>
            ReX := Log (Modulus (X / 2.0)) - Log_Two;
      end;

      ImX := Arctan (Im (X), Re (X));

      if ImX > PI then
         ImX := ImX - 2.0 * PI;
      end if;

      return Compose_From_Cartesian (ReX, ImX);
   end Log;

   ---------
   -- Exp --
   ---------

   function Exp (X : Complex) return Complex is
      EXP_RE_X : Real'Base := Exp (Re (X));

   begin
      return Compose_From_Cartesian (EXP_RE_X * Cos (Im (X)),
                                     EXP_RE_X * Sin (Im (X)));
   end Exp;


   function Exp (X : Imaginary) return Complex is
      ImX : Real'Base := Im (X);

   begin
      return Compose_From_Cartesian (Cos (ImX), Sin (ImX));
   end Exp;

   --------
   -- ** --
   --------

   function "**" (Left : Complex; Right : Complex) return Complex is
   begin
      if Re (Right) = 0.0
        and then Im (Right) = 0.0
        and then Re (Left)  = 0.0
        and then Im (Left)  = 0.0
      then
         raise Argument_Error;

      elsif Re (Left) = 0.0
        and then Im (Left) = 0.0
        and then Re (Right) < 0.0
      then
         raise Constraint_Error;

      elsif Re (Left) = 0.0 and then Im (Left) = 0.0 then
         return Left;

      elsif Right = Complex_Zero then
         return Complex_One;

      elsif Re (Right) = 0.0 and then Im (Right) = 0.0 then
         return 1.0 + Right;

      elsif Re (Right) = 1.0 and then Im (Right) = 0.0 then
         return Left;

      else
         return Exp (Right * Log (Left));
      end if;
   end "**";

   function "**" (Left : Real'Base; Right : Complex) return Complex is
   begin
      if Re (Right) = 0.0 and then Im (Right) = 0.0 and then Left = 0.0 then
         raise Argument_Error;

      elsif Left = 0.0 and then Re (Right) < 0.0 then
         raise Constraint_Error;

      elsif Left = 0.0 then
         return Compose_From_Cartesian (Left, 0.0);

      elsif Re (Right) = 0.0 and then Im (Right) = 0.0 then
         return Complex_One;

      elsif Re (Right) = 1.0 and then Im (Right) = 0.0 then
         return Compose_From_Cartesian (Left, 0.0);

      else
         return Exp (Log (Left) * Right);
      end if;
   end "**";

   function "**" (Left : Complex; Right : Real'Base) return Complex is
   begin
      if Right = 0.0
        and then Re (Left) = 0.0
        and then Im (Left) = 0.0
      then
         raise Argument_Error;

      elsif Re (Left) = 0.0
        and then Im (Left) = 0.0
        and then Right < 0.0
      then
         raise Constraint_Error;

      elsif Re (Left) = 0.0 and then Im (Left) = 0.0 then
         return Left;

      elsif Right = 0.0 then
         return Complex_One;

      elsif Right = 1.0 then
         return Left;

      else
         return Exp (Right * Log (Left));
      end if;
   end "**";

   ---------
   -- Sin --
   ---------

   function Sin (X : Complex) return Complex is
   begin
      if abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon then
         return X;
      end if;

      return
        Compose_From_Cartesian
          (Sin (Re (X)) * Cosh (Im (X)),
           Cos (Re (X)) * Sinh (Im (X)));
   end Sin;

   ---------
   -- Cos --
   ---------

   function Cos (X : Complex) return Complex is
   begin
      return
        Compose_From_Cartesian
          (Cos (Re (X))  * Cosh (Im (X)),
           -Sin (Re (X)) * Sinh (Im (X)));
   end Cos;

   ---------
   -- Tan --
   ---------

   function Tan (X : Complex) return Complex is
   begin
      if abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon
      then
         return X;

      elsif Im (X) > Log_Inverse_Epsilon_2 then
         return Complex_I;

      elsif Im (X) < -Log_Inverse_Epsilon_2 then
         return -Complex_I;

      else
         return Sin (X) / Cos (X);
      end if;
   end Tan;


   ---------
   -- Cot --
   ---------

   function Cot (X : Complex) return Complex is
   begin
      if abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon
      then
         return Complex_One  /  X;

      elsif Im (X) > Log_Inverse_Epsilon_2 then
         return -Complex_I;

      elsif Im (X) < -Log_Inverse_Epsilon_2 then
         return Complex_I;
      end if;

      return Cos (X) / Sin (X);
   end Cot;

   ------------
   -- Arcsin --
   ------------

   function Arcsin (X : Complex) return Complex is
      Result : Complex;

   begin
      if abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon
      then
         return X;

      elsif abs Re (X) > Inv_Square_Root_Epsilon or else
            abs Im (X) > Inv_Square_Root_Epsilon
      then
         Result := -Complex_I * (Log (Complex_I * X) + Log (2.0 * Complex_I));

         if Im (Result) > PI_2 then
            Set_Im (Result, PI - Im (X));

         elsif Im (Result) < -PI_2 then
            Set_Im (Result, -(PI + Im (X)));
         end if;
      end if;

      Result := -Complex_I * Log (Complex_I * X + Sqrt (1.0 - X * X));

      if Re (X) = 0.0 then
         Set_Re (Result, Re (X));

      elsif Im (X) = 0.0 then
         Set_Im (Result, Im (X));
      end if;

      return Result;
   end Arcsin;

   ------------
   -- Arccos --
   ------------

   function Arccos (X : Complex) return Complex is
      Result : Complex;

   begin
      if X = Complex_One then
         return Complex_Zero;

      elsif abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon
      then
         return Half_Pi - X;

      elsif abs Re (X) > Inv_Square_Root_Epsilon or else
            abs Im (X) > Inv_Square_Root_Epsilon
      then
         return -2.0 * Complex_I * Log (Sqrt ((1.0 + X) / 2.0) +
                            Complex_I * Sqrt ((1.0 - X) / 2.0));
      end if;

      Result := -Complex_I * Log (X + Complex_I * Sqrt (1.0 - X * X));

      if Im (X) = 0.0 then
         Set_Im (Result, Im (X));
      end if;

      return Result;
   end Arccos;

   ------------
   -- Arctan --
   ------------

   function Arctan (X : Complex) return Complex is
   begin
      if abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon
      then
         return X;

      else
         return -Complex_I * (Log (1.0 + Complex_I * X)
                            - Log (1.0 - Complex_I * X)) / 2.0;
      end if;
   end Arctan;

   ------------
   -- Arccot --
   ------------

   function Arccot (X : Complex) return Complex is
      Xt : Complex;

   begin
      if abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon
      then
         return Half_Pi - X;

      elsif abs Re (X) > 1.0 / Epsilon or else
            abs Im (X) > 1.0 / Epsilon
      then
         Xt := Complex_One  /  X;

         if Re (X) < 0.0 then
            Set_Re (Xt, PI - Re (Xt));
            return Xt;
         else
            return Xt;
         end if;
      end if;

      Xt := Complex_I * Log ((X - Complex_I) / (X + Complex_I)) / 2.0;

      if Re (Xt) < 0.0 then
         Xt := PI + Xt;
      end if;

      return Xt;
   end Arccot;

   ----------
   -- Sinh --
   ----------

   function Sinh (X : Complex) return Complex is
   begin
      if abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon
      then
         return X;

      else
         return Compose_From_Cartesian (Sinh (Re (X)) * Cos (Im (X)),
                                        Cosh (Re (X)) * Sin (Im (X)));
      end if;
   end Sinh;

   ----------
   -- Cosh --
   ----------

   function Cosh (X : Complex) return Complex is
   begin
      return
        Compose_From_Cartesian
          (Cosh (Re (X)) * Cos (Im (X)),
           Sinh (Re (X)) * Sin (Im (X)));
   end Cosh;

   ----------
   -- Tanh --
   ----------

   function Tanh (X : Complex) return Complex is
   begin
      if abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon
      then
         return X;

      elsif Re (X) > Log_Inverse_Epsilon_2 then
         return Complex_One;

      elsif Re (X) < -Log_Inverse_Epsilon_2 then
         return -Complex_One;

      else
         return Sinh (X) / Cosh (X);
      end if;
   end Tanh;

   ----------
   -- Coth --
   ----------

   function Coth (X : Complex) return Complex is
   begin
      if abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon
      then
         return Complex_One  /  X;

      elsif Re (X) > Log_Inverse_Epsilon_2 then
         return Complex_One;

      elsif Re (X) < -Log_Inverse_Epsilon_2 then
         return -Complex_One;

      else
         return Cosh (X) / Sinh (X);
      end if;
   end Coth;

   -------------
   -- Arcsinh --
   -------------

   function Arcsinh (X : Complex) return Complex is
      Result : Complex;

   begin
      if abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon
      then
         return X;

      elsif abs Re (X) > Inv_Square_Root_Epsilon or else
            abs Im (X) > Inv_Square_Root_Epsilon
      then
         Result := Log_Two + Log (X); -- may have wrong sign

         if (Re (X) < 0.0 and Re (Result) > 0.0)
           or else (Re (X) > 0.0 and Re (Result) < 0.0)
         then
            Set_Re (Result, -Re (Result));
         end if;

         return Result;
      end if;

      Result := Log (X + Sqrt (1.0 + X * X));

      if Re (X) = 0.0 then
         Set_Re (Result, Re (X));
      elsif Im  (X) = 0.0 then
         Set_Im (Result, Im  (X));
      end if;

      return Result;
   end Arcsinh;

   -------------
   -- Arccosh --
   -------------

   function Arccosh (X : Complex) return Complex is
      Result : Complex;

   begin
      if X = Complex_One then
         return Complex_Zero;

      elsif abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon
      then
         Result := Compose_From_Cartesian (-Im (X), -PI_2 + Re (X));

      elsif abs Re (X) > Inv_Square_Root_Epsilon or else
            abs Im (X) > Inv_Square_Root_Epsilon
      then
         Result := Log_Two + Log (X);

      else
         Result := 2.0 * Log (Sqrt ((1.0 + X) / 2.0) +
                              Sqrt ((X - 1.0) / 2.0));
      end if;

      if Re (Result) <= 0.0 then
         Result := -Result;
      end if;

      return Result;
   end Arccosh;

   -------------
   -- Arctanh --
   -------------

   function Arctanh (X : Complex) return Complex is
   begin
      if abs Re (X) < Square_Root_Epsilon and then
         abs Im (X) < Square_Root_Epsilon
      then
         return X;
      else
         return (Log (1.0 + X) - Log (1.0 - X)) / 2.0;
      end if;
   end Arctanh;

   --------------
   -- Arctcoth --
   --------------

   function Arccoth (X : Complex) return Complex is
      R : Complex;

   begin
      if X = Complex_Zero then
         return Compose_From_Cartesian (0.0, PI_2);

      elsif abs Re (X) < Square_Root_Epsilon
         and then abs Im (X) < Square_Root_Epsilon
      then
         return PI_2 * Complex_I + X;

      elsif abs Re (X) > 1.0 / Epsilon or else
            abs Im (X) > 1.0 / Epsilon
      then
         if Im (X) > 0.0 then
            return Complex_Zero;
         else
            return PI * Complex_I;
         end if;

      elsif Im (X) = 0.0 and then Re (X) = 1.0 then
         raise Constraint_Error;

      elsif Im (X) = 0.0 and then Re (X) = -1.0 then
         raise Constraint_Error;
      end if;

      begin
         R := Log ((1.0 + X) / (X - 1.0)) / 2.0;

      exception
         when Constraint_Error =>
            R := (Log (1.0 + X) - Log (X - 1.0)) / 2.0;
      end;

      if Im (R) < 0.0 then
         Set_Im (R, PI + Im (R));
      end if;

      if Re (X) = 0.0 then
         Set_Re (R, Re (X));
      end if;

      return R;
   end Arccoth;

begin
   --  Initialize needed pseudo-constants

   Epsilon := Real (Real'Model_Epsilon);

   while Epsilon / Real (Real'Machine_Radix) + 1.0 /= 1.0 loop
      Epsilon := Epsilon / Real (Real'Machine_Radix);
   end loop;

   Square_Root_Epsilon     := Sqrt (Epsilon);
   Inv_Square_Root_Epsilon := 1.0 / Square_Root_Epsilon;
   Root_Root_Epsilon       := Sqrt (Square_Root_Epsilon);
   Log_Inverse_Epsilon_2   := Log (1.0 / Epsilon) / 2.0;

end Ada.Numerics.Generic_Complex_Elementary_Functions;
