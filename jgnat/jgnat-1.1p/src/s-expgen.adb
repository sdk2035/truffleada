------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . E X P _ G E N                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.10 $                             --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
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

package body System.Exp_Gen is

   --------------------
   -- Exp_Float_Type --
   --------------------

   function Exp_Float_Type
     (Left  : Type_Of_Base;
      Right : Integer)
      return  Type_Of_Base
   is
      Result : Type_Of_Base := 1.0;
      Factor : Type_Of_Base := Left;
      Exp    : Natural := Right;

   begin
      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2. For positive exponents we
      --  multiply the result by this factor, for negative exponents, we
      --  divide by this factor.

      if Exp >= 0 then

         --  For a positive exponent, if we get a constraint error during
         --  this loop, it is an overflow, and the constraint error will
         --  simply be passed on to the caller.

         loop
            if Exp rem 2 /= 0 then
               declare
                  pragma Unsuppress (All_Checks);
               begin
                  Result := Result * Factor;
               end;
            end if;

            Exp := Exp / 2;
            exit when Exp = 0;

            declare
               pragma Unsuppress (All_Checks);
            begin
               Factor := Factor * Factor;
            end;
         end loop;

         return Result;

      --  Now we know that the exponent is negative, check for case of
      --  base of 0.0 which always generates a constraint error.

      elsif Factor = 0.0 then
         raise Constraint_Error;

      --  Here we have a negative exponent with a non-zero base

      else

         --  For the negative exponent case, a constraint error during this
         --  calculation happens if Factor gets too large, and the proper
         --  response is to return 0.0, since what we essenmtially have is
         --  1.0 / infinity, and the closest model number will be zero.

         begin
            loop
               if Exp rem 2 /= 0 then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Result := Result * Factor;
                  end;
               end if;

               Exp := Exp / 2;
               exit when Exp = 0;

               declare
                  pragma Unsuppress (All_Checks);
               begin
                  Factor := Factor * Factor;
               end;
            end loop;

            declare
               pragma Unsuppress (All_Checks);
            begin
               return 1.0 / Result;
            end;

         exception

            when Constraint_Error =>
               return 0.0;
         end;
      end if;
   end Exp_Float_Type;

   ----------------------
   -- Exp_Integer_Type --
   ----------------------

   --  Note that negative exponents get a constraint error because the
   --  subtype of the Right argument (the exponent) is Natural.

   function Exp_Integer_Type
     (Left  : Type_Of_Base;
      Right : Natural)
      return  Type_Of_Base
   is
      Result : Type_Of_Base := 1;
      Factor : Type_Of_Base := Left;
      Exp    : Natural := Right;

   begin
      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2.

      --  Note: it is not worth special casing the cases of base values -1,0,+1
      --  since the expander does this when the base is a literal, and other
      --  cases will be extremely rare.

      if Exp /= 0 then
         loop
            if Exp rem 2 /= 0 then
               declare
                  pragma Unsuppress (All_Checks);
               begin
                  Result := Result * Factor;
               end;
            end if;

            Exp := Exp / 2;
            exit when Exp = 0;

            declare
               pragma Unsuppress (All_Checks);
            begin
               Factor := Factor * Factor;
            end;
         end loop;
      end if;

      return Result;
   end Exp_Integer_Type;

end System.Exp_Gen;
