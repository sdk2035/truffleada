------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--             A D A . W I D E _ T E X T _ I O . E D I T I N G              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.9 $                              --
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

with Ada.Strings.Fixed;
with Ada.Strings.Wide_Fixed;

package body Ada.Wide_Text_IO.Editing is

   package Strings            renames Ada.Strings;
   package Strings_Fixed      renames Ada.Strings.Fixed;
   package Strings_Wide_Fixed renames Ada.Strings.Wide_Fixed;
   package Wide_Text_IO       renames Ada.Wide_Text_IO;

   -----------------------
   -- Local_Subprograms --
   -----------------------

   function To_Wide (C : Character) return Wide_Character;
   pragma Inline (To_Wide);
   --  Convert Character to corresponding Wide_Character

   -------------
   -- To_Wide --
   -------------

   function To_Wide (C : Character) return Wide_Character is
   begin
      return Wide_Character'Val (Character'Pos (C));
   end To_Wide;

   -------------------------
   -- Parse_Number_String --
   -------------------------

   function Parse_Number_String (Str : String) return Number_Attributes is
      Answer : Number_Attributes;

   begin
      for I in Str'Range loop
         case Str (I) is

            when ' ' =>
               null; --  ignore

            when '1' .. '9' =>

               --  Decide if this is the start of a number.
               --  If so, figure out which one...

               if Answer.Has_Fraction then
                  Answer.End_Of_Fraction := I;
               else
                  if Answer.Start_Of_Int = Invalid_Position then
                     --  start integer
                     Answer.Start_Of_Int := I;
                  end if;
                  Answer.End_Of_Int := I;
               end if;

            when '0' =>

               --  Only count a zero before the decimal point if it follows a
               --  non-zero digit.  After the decimal point, zeros will be
               --  counted if followed by a non-zero digit.

               if not Answer.Has_Fraction then
                  if Answer.Start_Of_Int /= Invalid_Position then
                     Answer.End_Of_Int := I;
                  end if;
               end if;

            when '-' =>

               --  Set negative

               Answer.Negative := True;

            when '.' =>

               --  Close integer, start fraction

               if Answer.Has_Fraction then
                  raise Picture_Error;
               end if;

               --  Two decimal points is a no-no.

               Answer.Has_Fraction    := True;
               Answer.End_Of_Fraction := I;

               --  Could leave this at Invalid_Position, but this seems the
               --  right way to indicate a null range...

               Answer.Start_Of_Fraction := I + 1;
               Answer.End_Of_Int        := I - 1;

            when others =>
               raise Picture_Error; -- can this happen? probably not!
         end case;
      end loop;

      if Answer.Start_Of_Int = Invalid_Position then
         Answer.Start_Of_Int := Answer.End_Of_Int + 1;
      end if;

      --  No significant (intger) digits needs a null range.

      return Answer;

   end Parse_Number_String;

   ------------------
   -- Precalculate --
   ------------------

   procedure Precalculate  (Pic : in out Format_Record) is

      Computed_BWZ : Boolean := True;

      type Legality is  (Okay, Reject);
      State : Legality := Reject;
      --  Start in reject, which will reject null strings.

      Index : Pic_Index := Pic.Picture.Expanded'First;

      function At_End return Boolean;
      pragma Inline (At_End);

      procedure Set_State (L : Legality);
      pragma Inline (Set_State);

      function Look return Character;
      pragma Inline (Look);

      function Is_Insert return Boolean;
      pragma Inline (Is_Insert);

      procedure Skip;
      pragma Inline (Skip);

      procedure Trailing_Currency;
      procedure Trailing_Bracket;
      procedure Number_Fraction;
      procedure Number_Completion;
      procedure Number_Fraction_Or_Bracket;
      procedure Number_Fraction_Or_Z_Fill;
      procedure Zero_Suppression;
      procedure Floating_Bracket;
      procedure Number_Fraction_Or_Star_Fill;
      procedure Star_Suppression;
      procedure Number_Fraction_Or_Dollar;
      procedure Leading_Dollar;
      procedure Number_Fraction_Or_Pound;
      procedure Leading_Pound;
      procedure Picture;
      procedure Floating_Plus;
      procedure Floating_Minus;
      procedure Picture_Plus;
      procedure Picture_Minus;
      procedure Picture_Bracket;
      procedure Number;
      procedure Optional_RHS_Sign;
      procedure Picture_String;

      ------------
      -- At_End --
      ------------

      function At_End return Boolean is
      begin
         return Index > Pic.Picture.Length;
      end At_End;

      ---------------
      -- Set_State --
      ---------------

      procedure Set_State (L : Legality) is
      begin
         State := L;
      end Set_State;

      ----------
      -- Look --
      ----------

      function Look return Character is
      begin
         if At_End then
            raise Picture_Error;
         end if;

         return Pic.Picture.Expanded (Index);
      end Look;

      ---------------
      -- Is_Insert --
      ---------------

      function Is_Insert return Boolean is
      begin
         if At_End then
            return False;
         end if;

         case Pic.Picture.Expanded (Index) is

            when '_' | '0' | '/' => return True;

            when 'B' | 'b' =>
               Pic.Picture.Expanded (Index) := 'b'; --  canonical
               return True;

            when others => return False;
         end case;
      end Is_Insert;

      ----------
      -- Skip --
      ----------

      procedure Skip is
      begin
         Index := Index + 1;
      end Skip;

      -----------------------
      -- Trailing_Currency --
      -----------------------

      procedure Trailing_Currency is
      begin
         if At_End then
            return;
         end if;

         if Look = '$' then
            Pic.Start_Currency := Index;
            Pic.End_Currency := Index;
            Skip;

         else
            while not At_End and then Look = '#' loop
               if Pic.Start_Currency = Invalid_Position then
                  Pic.Start_Currency := Index;
               end if;

               Pic.End_Currency := Index;
               Skip;
            end loop;
         end if;

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' => Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when others => return;
            end case;
         end loop;
      end Trailing_Currency;

      ----------------------
      -- Trailing_Bracket --
      ----------------------

      procedure Trailing_Bracket is
      begin
         if Look = '>' then
            Pic.Second_Sign := Index;
            Skip;
         else
            raise Picture_Error;
         end if;
      end Trailing_Bracket;

      ---------------------
      -- Number_Fraction --
      ---------------------

      procedure Number_Fraction is
      begin
         --  Note that number fraction can be called in either State.
         --  It will set state to Valid only if a 9 is encountered.

         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '9' =>
                  Computed_BWZ := False;
                  Pic.Max_Trailing_Digits := Pic.Max_Trailing_Digits + 1;
                  Set_State (Okay); Skip;

               when others =>
                  return;
            end case;
         end loop;
      end Number_Fraction;

      -----------------------
      -- Number_Completion --
      -----------------------

      procedure Number_Completion is
      begin
         while not At_End loop
            case Look is

               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '9' =>
                  Computed_BWZ := False;
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Set_State (Okay);
                  Skip;

               when 'V' | 'v' | '.' =>
                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction;
                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Number_Completion;

      --------------------------------
      -- Number_Fraction_Or_Bracket --
      --------------------------------

      procedure Number_Fraction_Or_Bracket is
      begin
         loop
            if At_End then
               return;
            end if;

            case Look is

               when '_' | '0' | '/' => Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '<' =>
                  Pic.Max_Trailing_Digits := Pic.Max_Trailing_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;

                  loop
                     if At_End then
                        return;
                     end if;

                     case Look is
                        when '_' | '0' | '/' =>
                           Skip;

                        when 'B' | 'b'  =>
                           Pic.Picture.Expanded (Index) := 'b';
                           Skip;

                        when '<' =>
                           Pic.Max_Trailing_Digits :=
                             Pic.Max_Trailing_Digits + 1;
                           Pic.End_Float := Index;
                           Skip;

                        when others =>
                           return;
                     end case;
                  end loop;

               when others =>
                  Number_Fraction;
                  return;
            end case;
         end loop;
      end Number_Fraction_Or_Bracket;

      -------------------------------
      -- Number_Fraction_Or_Z_Fill --
      -------------------------------

      procedure Number_Fraction_Or_Z_Fill is
      begin
         loop
            if At_End then
               return;
            end if;

            case Look is

               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when 'Z' | 'z' =>
                  Pic.Max_Trailing_Digits := Pic.Max_Trailing_Digits + 1;
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'Z'; -- consistency

                  Skip;

                  loop
                     if At_End then
                        return;
                     end if;

                     case Look is

                        when '_' | '0' | '/' =>
                           Skip;

                        when 'B' | 'b'  =>
                           Pic.Picture.Expanded (Index) := 'b';
                           Skip;

                        when 'Z' | 'z' =>
                           Pic.Picture.Expanded (Index) := 'Z'; -- consistency

                           Pic.Max_Trailing_Digits :=
                             Pic.Max_Trailing_Digits + 1;
                           Pic.End_Float := Index;
                           Skip;

                        when others =>
                           return;
                     end case;
                  end loop;

               when others =>
                  Number_Fraction;
                  return;
            end case;
         end loop;
      end Number_Fraction_Or_Z_Fill;

      ----------------------
      -- Zero_Suppression --
      ----------------------

      procedure Zero_Suppression is
      begin
         Pic.Floater := 'Z';
         Pic.Start_Float := Index;
         Pic.End_Float := Index;
         Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
         Pic.Picture.Expanded (Index) := 'Z'; -- consistency

         Skip; --  Known Z

         loop
            --  Even a single Z is a valid picture

            if At_End then
               Set_State (Okay);
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when 'Z' | 'z' =>
                  Pic.Picture.Expanded (Index) := 'Z'; -- consistency

                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Set_State (Okay);
                  Skip;

               when '9' =>
                  Set_State (Okay);
                  Number_Completion;
                  return;

               when '.' | 'V' | 'v' =>
                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction_Or_Z_Fill;
                  return;

               when '#' | '$' =>
                  Trailing_Currency;
                  Set_State (Okay);
                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Zero_Suppression;

      ----------------------
      -- Floating_Bracket --
      ----------------------

      --  Note that Floating_Bracket is only called with an acceptable
      --  prefix. But we don't set Okay, because we must end with a '>'.

      procedure Floating_Bracket is
      begin
         Pic.Floater := '<';
         Pic.End_Float := Index;
         Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;

         --  First bracket wasn't counted...

         Skip; --  known '<'

         loop
            if At_End then
               return;
            end if;

            case Look is

               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '<' =>
                  Pic.End_Float := Index;
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Skip;

               when '9' =>
                  Number_Completion;

               when '$' =>
                  Leading_Dollar;

               when '#' =>
                  Leading_Pound;

               when 'V' | 'v' | '.' =>
                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction_Or_Bracket;
                  return;

               when others =>
               return;
            end case;
         end loop;
      end Floating_Bracket;

      ----------------------------------
      -- Number_Fraction_Or_Star_Fill --
      ----------------------------------

      procedure Number_Fraction_Or_Star_Fill is
      begin
         loop
            if At_End then
               return;
            end if;

            case Look is

               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '*' =>
                  Pic.Star_Fill := True;
                  Pic.Max_Trailing_Digits := Pic.Max_Trailing_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;

                  loop
                     if At_End then
                        return;
                     end if;

                     case Look is

                        when '_' | '0' | '/' =>
                           Skip;

                        when 'B' | 'b'  =>
                           Pic.Picture.Expanded (Index) := 'b';
                           Skip;

                        when '*' =>
                           Pic.Star_Fill := True;
                           Pic.Max_Trailing_Digits :=
                             Pic.Max_Trailing_Digits + 1;
                           Pic.End_Float := Index;
                           Skip;

                        when others =>
                           return;
                     end case;
                  end loop;

               when others =>
                  Number_Fraction;
                  return;

            end case;
         end loop;
      end Number_Fraction_Or_Star_Fill;

      ----------------------
      -- Star_Suppression --
      ----------------------

      procedure Star_Suppression is
      begin
         Pic.Floater := '*';
         Pic.Start_Float := Index;
         Pic.End_Float := Index;
         Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
         Set_State (Okay);

         --  Even a single * is a valid picture

         Pic.Star_Fill := True;
         Skip; --  Known *

         loop
            if At_End then
               return;
            end if;

            case Look is

               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '*' =>
                  Pic.End_Float := Index;
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Set_State (Okay); Skip;

               when '9' =>
                  Set_State (Okay);
                  Number_Completion;
                  return;

               when '.' | 'V' | 'v' =>
                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction_Or_Star_Fill;
                  return;

               when '#' | '$' =>
                  Trailing_Currency;
                  Set_State (Okay);
                  return;

               when others => raise Picture_Error;
            end case;
         end loop;
      end Star_Suppression;

      -------------------------------
      -- Number_Fraction_Or_Dollar --
      -------------------------------

      procedure Number_Fraction_Or_Dollar is
      begin
         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '$' =>
                  Pic.Max_Trailing_Digits := Pic.Max_Trailing_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;

                  loop
                     if At_End then
                        return;
                     end if;

                     case Look is
                        when '_' | '0' | '/' =>
                           Skip;

                        when 'B' | 'b'  =>
                           Pic.Picture.Expanded (Index) := 'b';
                           Skip;

                        when '$' =>
                           Pic.Max_Trailing_Digits :=
                             Pic.Max_Trailing_Digits + 1;
                           Pic.End_Float := Index;
                           Skip;

                        when others =>
                           return;
                     end case;
                  end loop;

               when others =>
                  Number_Fraction;
                  return;
            end case;
         end loop;
      end Number_Fraction_Or_Dollar;

      --------------------
      -- Leading_Dollar --
      --------------------

      --  Note that Leading_Dollar can be called in either State.
      --  It will set state to Okay only if a 9 or (second) $
      --  is encountered.

      --  Also notice the tricky bit with State and Zero_Suppression.
      --  Zero_Suppression is Picture_Error if a '$' or a '9' has been
      --  encountered, exactly the cases where State has been set.

      procedure Leading_Dollar is
      begin
         --  Treat as a floating dollar, and unwind otherwise.

         Pic.Floater := '$';
         Pic.Start_Currency := Index;
         Pic.End_Currency := Index;
         Pic.Start_Float := Index;
         Pic.End_Float := Index;

         --  Don't increment Pic.Max_Leading_Digits, we need one "real"
         --  currency place.

         Skip; --  known '$'

         loop
            if At_End then
               return;
            end if;

            case Look is

               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

                  --  A trailing insertion character is not part of the
                  --  floating currency, so need to look ahead.

                  if Look /= '$' then
                     Pic.End_Float := Pic.End_Float - 1;
                  end if;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when 'Z' | 'z' =>
                  Pic.Picture.Expanded (Index) := 'Z'; -- consistency

                  if State = Okay then
                     raise Picture_Error;
                  else
                     --  Will overwrite Floater and Start_Float

                     Zero_Suppression;
                  end if;

               when '*' =>
                  if State = Okay then
                     raise Picture_Error;
                  else
                     --  Will overwrite Floater and Start_Float

                     Star_Suppression;
                  end if;

               when '$' =>
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Pic.End_Currency := Index;
                  Set_State (Okay); Skip;

               when '9' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  --  A single dollar does not a floating make.

                  Number_Completion;
                  return;

               when 'V' | 'v' | '.' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  --  Only one dollar before the sign is okay,
                  --  but doesn't float.

                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction_Or_Dollar;
                  return;

               when others =>
                  return;

            end case;
         end loop;
      end Leading_Dollar;

      ------------------------------
      -- Number_Fraction_Or_Pound --
      ------------------------------

      procedure Number_Fraction_Or_Pound is
      begin
         loop
            if At_End then
               return;
            end if;

            case Look is

               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '#' =>
                  Pic.Max_Trailing_Digits := Pic.Max_Trailing_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;

                  loop
                     if At_End then
                        return;
                     end if;

                     case Look is

                        when '_' | '0' | '/' =>
                           Skip;

                        when 'B' | 'b'  =>
                           Pic.Picture.Expanded (Index) := 'b';
                           Skip;

                        when '#' =>
                           Pic.Max_Trailing_Digits :=
                             Pic.Max_Trailing_Digits + 1;
                           Pic.End_Float := Index;
                           Skip;

                        when others =>
                           return;

                     end case;
                  end loop;

               when others =>
                  Number_Fraction;
                  return;

            end case;
         end loop;
      end Number_Fraction_Or_Pound;

      -------------------
      -- Leading_Pound --
      -------------------

      --  This one is complex!  A Leading_Pound can be fixed or floating,
      --  but in some cases the decision has to be deferred until we leave
      --  this procedure.  Also note that Leading_Pound can be called in
      --  either State.

      --  It will set state to Okay only if a 9 or  (second) # is
      --  encountered.

      --  One Last note:  In ambiguous cases, the currency is treated as
      --  floating unless there is only one '#'.

      procedure Leading_Pound is

         Inserts : Boolean := False;
         --  Set to True if a '_', '0', '/', 'B', or 'b' is encountered

         Must_Float : Boolean := False;
         --  Set to true if a '#' occurs after an insert.

      begin
         --  Treat as a floating currency. If it isn't, this will be
         --  overwritten later.

         Pic.Floater := '#';

         Pic.Start_Currency := Index;
         Pic.End_Currency := Index;
         Pic.Start_Float := Index;
         Pic.End_Float := Index;

         --  Don't increment Pic.Max_Leading_Digits, we need one "real"
         --  currency place.

         Pic.Max_Currency_Digits := 1; --  we've seen one.

         Skip; --  known '#'

         loop
            if At_End then
               return;
            end if;

            case Look is

               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Inserts := True;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Pic.End_Float := Index;
                  Inserts := True;
                  Skip;

               when 'Z' | 'z' =>
                  Pic.Picture.Expanded (Index) := 'Z'; -- consistency

                  if Must_Float then
                     raise Picture_Error;
                  else
                     Pic.Max_Leading_Digits := 0;

                     --  Will overwrite Floater and Start_Float

                     Zero_Suppression;
                  end if;

               when '*' =>
                  if Must_Float then
                     raise Picture_Error;
                  else
                     Pic.Max_Leading_Digits := 0;

                     --  Will overwrite Floater and Start_Float

                     Star_Suppression;
                  end if;

               when '#' =>
                  if Inserts then
                     Must_Float := True;
                  end if;

                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Pic.End_Currency := Index;
                  Set_State (Okay);
                  Skip;

               when '9' =>
                  if State /= Okay then

                     --  A single '#' doesn't float.

                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  Number_Completion;
                  return;

               when 'V' | 'v' | '.' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  --  Only one pound before the sign is okay,
                  --  but doesn't float.

                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction_Or_Pound;
                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Leading_Pound;

      -------------
      -- Picture --
      -------------

      --  Note that Picture can be called in either State.

      --  It will set state to Valid only if a 9 is encountered or floating
      --  currency is called.

      procedure Picture is
      begin
         loop
            if At_End then
               return;
            end if;

            case Look is

               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '$' =>
                  Leading_Dollar;
                  return;

               when '#' =>
                  Leading_Pound;
                  return;

               when '9' =>
                  Computed_BWZ := False;
                  Set_State (Okay);
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Skip;

               when 'V' | 'v' | '.' =>
                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction;
                  Trailing_Currency;
                  return;

               when others =>
                  return;

            end case;
         end loop;
      end Picture;

      -------------------
      -- Floating_Plus --
      -------------------

      procedure Floating_Plus is
      begin
         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '+' =>
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;

               when '9' =>
                  Number_Completion;
                  return;

               when '.' | 'V' | 'v' =>
                  Pic.Radix_Position := Index;
                  Skip; --  Radix

                  while Is_Insert loop
                     Skip;
                  end loop;

                  if At_End then
                     return;
                  end if;

                  if Look = '+' then
                     loop
                        if At_End then
                           return;
                        end if;

                        case Look is

                           when '+' =>
                              Pic.Max_Trailing_Digits :=
                                Pic.Max_Trailing_Digits + 1;
                              Pic.End_Float := Index;
                              Skip;

                           when '_' | '0' | '/' =>
                              Skip;

                           when 'B' | 'b'  =>
                              Pic.Picture.Expanded (Index) := 'b';
                              Skip;

                           when others =>
                              return;

                        end case;
                     end loop;

                  else
                     Number_Completion;
                  end if;

                  return;

               when others =>
                  return;

            end case;
         end loop;
      end Floating_Plus;

      --------------------
      -- Floating_Minus --
      --------------------

      procedure Floating_Minus is
      begin
         loop
            if At_End then
               return;
            end if;

            case Look is
               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '-' =>
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;

               when '9' =>
                  Number_Completion;
                  return;

               when '.' | 'V' | 'v' =>
                  Pic.Radix_Position := Index;
                  Skip; --  Radix

                  while Is_Insert loop
                     Skip;
                  end loop;

                  if At_End then
                     return;
                  end if;

                  if Look = '-' then
                     loop
                        if At_End then
                           return;
                        end if;

                        case Look is

                           when '-' =>
                              Pic.Max_Trailing_Digits :=
                                Pic.Max_Trailing_Digits + 1;
                              Pic.End_Float := Index;
                              Skip;

                           when '_' | '0' | '/' =>
                              Skip;

                           when 'B' | 'b'  =>
                              Pic.Picture.Expanded (Index) := 'b';
                              Skip;

                           when others =>
                              return;

                        end case;
                     end loop;

                  else
                     Number_Completion;
                  end if;

                  return;

               when others =>
                  return;
            end case;
         end loop;
      end Floating_Minus;

      ------------------
      -- Picture_Plus --
      ------------------

      procedure Picture_Plus is
      begin
         Pic.Sign_Position := Index;

         --  Treat as a floating sign, and unwind otherwise.

         Pic.Floater := '+';
         Pic.Start_Float := Index;
         Pic.End_Float := Index;

         --  Don't increment Pic.Max_Leading_Digits, we need one "real"
         --  sign place.

         Skip; --  Known Plus

         loop
            case Look is

               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '+' =>
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;
                  Set_State (Okay);  --  "++" is enough.
                  Floating_Plus;
                  Trailing_Currency;
                  return;

               when '$' | '#' | '9' | '*' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  Picture;
                  Set_State (Okay);
                  return;

               when 'Z' | 'z' =>
                  if State = Okay then
                     Set_State (Reject);
                  end if;

                  --  Can't have Z and a floating sign.

                  Pic.Picture.Expanded (Index) := 'Z'; -- consistency

                  --  '+Z' is acceptable

                  Set_State (Okay);

                  Zero_Suppression;
                  Trailing_Currency;
                  Optional_RHS_Sign;
                  return;

               when '.' | 'V' | 'v' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  --  Don't assume that state is okay, haven't seen a digit.

                  Picture;
                  return;

               when others =>
                  return;

            end case;
         end loop;
      end Picture_Plus;

      -------------------
      -- Picture_Minus --
      -------------------

      procedure Picture_Minus is
      begin
         Pic.Sign_Position := Index;

         --  Treat as a floating sign, and unwind otherwise.

         Pic.Floater := '-';
         Pic.Start_Float := Index;
         Pic.End_Float := Index;

         --  Don't increment Pic.Max_Leading_Digits, we need one "real"
         --  sign place.

         Skip; --  Known Minus

         loop
            case Look is

               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '-' =>
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Pic.End_Float := Index;
                  Skip;
                  Set_State (Okay);  --  "-- " is enough.
                  Floating_Minus;
                  Trailing_Currency;
                  return;

               when '$' | '#' | '9' | '*' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  Picture;
                  Set_State (Okay);
                  return;

               when 'Z' | 'z' =>

                  --  Can't have Z and a floating sign.

                  if State = Okay then
                     Set_State (Reject);
                  end if;

                  Pic.Picture.Expanded (Index) := 'Z'; -- consistency
                  Zero_Suppression;
                  Trailing_Currency;
                  Optional_RHS_Sign;
                  return;

               when '.' | 'V' | 'v' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  --  Don't assume that state is okay, haven't seen a digit.

                  Picture;
                  return;

               when others =>
                  return;

            end case;
         end loop;
      end Picture_Minus;

      ---------------------
      -- Picture_Bracket --
      ---------------------

      procedure Picture_Bracket is
      begin
         Pic.Sign_Position := Index;
         Pic.Sign_Position := Index;

         --  Treat as a floating sign, and unwind otherwise.

         Pic.Floater := '<';
         Pic.Start_Float := Index;
         Pic.End_Float := Index;

         --  Don't increment Pic.Max_Leading_Digits, we need one "real"
         --  sign place.

         Skip; --  Known Bracket

         loop
            case Look is

               when '_' | '0' | '/' =>
                  Pic.End_Float := Index;
                  Skip;

               when 'B' | 'b'  =>
                  Pic.End_Float := Index;
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '<' =>
                  Set_State (Okay);  --  "<<>" is enough.
                  Floating_Bracket;
                  Trailing_Currency;
                  Trailing_Bracket;
                  return;

               when '$' | '#' | '9' | '*' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  Picture;
                  Trailing_Bracket;
                  Set_State (Okay);
                  return;

               when '.' | 'V' | 'v' =>
                  if State /= Okay then
                     Pic.Floater := '!';
                     Pic.Start_Float := Invalid_Position;
                     Pic.End_Float := Invalid_Position;
                  end if;

                  --  Don't assume that state is okay, haven't seen a digit

                  Picture;
                  Trailing_Bracket;
                  return;

               when others =>
                  raise Picture_Error;

            end case;
         end loop;
      end Picture_Bracket;

      ------------
      -- Number --
      ------------

      procedure Number is
      begin
         loop

            case Look is
               when '_' | '0' | '/' =>
                  Skip;

               when 'B' | 'b'  =>
                  Pic.Picture.Expanded (Index) := 'b';
                  Skip;

               when '9' =>
                  Computed_BWZ := False;
                  Pic.Max_Leading_Digits := Pic.Max_Leading_Digits + 1;
                  Set_State (Okay);
                  Skip;

               when '.' | 'V' | 'v' =>
                  Pic.Radix_Position := Index;
                  Skip;
                  Number_Fraction;
                  return;

               when others =>
                  return;

            end case;

            if At_End then
               return;
            end if;

            --  Will return in Okay state if a '9' was seen.

         end loop;
      end Number;

      -----------------------
      -- Optional_RHS_Sign --
      -----------------------

      procedure Optional_RHS_Sign is
      begin
         if At_End then
            return;
         end if;

         case Look is

            when '+' | '-' =>
               Pic.Sign_Position := Index;
               Skip;
               return;

            when 'C' | 'c' =>
               Pic.Sign_Position := Index;
               Pic.Picture.Expanded (Index) := 'C';
               Skip;

               if Look = 'R' or Look = 'r' then
                  Pic.Second_Sign := Index;
                  Pic.Picture.Expanded (Index) := 'R';
                  Skip;

               else
                  raise Picture_Error;
               end if;

               return;

            when 'D' | 'd' =>
               Pic.Sign_Position := Index;
               Pic.Picture.Expanded (Index) := 'D';
               Skip;

               if Look = 'B' or Look = 'b' then
                  Pic.Second_Sign := Index;
                  Pic.Picture.Expanded (Index) := 'B';
                  Skip;

               else
                  raise Picture_Error;
               end if;

               return;

            when '>' =>
               if Pic.Picture.Expanded (Pic.Sign_Position) = '<' then
                  Pic.Second_Sign := Index;
                  Skip;

               else
                  raise Picture_Error;
               end if;

            when others =>
               return;

         end case;
      end Optional_RHS_Sign;

      --------------------
      -- Picture_String --
      --------------------

      procedure Picture_String is
      begin
         while Is_Insert loop
            Skip;
         end loop;

         case Look is

            when '$' | '#' =>
               Picture;
               Optional_RHS_Sign;

            when '+' =>
               Picture_Plus;

            when '-' =>
               Picture_Minus;

            when '<' =>
               Picture_Bracket;

            when 'Z' | 'z' =>
               Pic.Picture.Expanded (Index) := 'Z'; -- consistency
               Zero_Suppression;
               Trailing_Currency;
               Optional_RHS_Sign;

            when '*' =>
               Star_Suppression;
               Trailing_Currency;
               Optional_RHS_Sign;

            when '9' | '.' | 'V' | 'v' =>
               Number;
               Trailing_Currency;
               Optional_RHS_Sign;

            when others =>
               raise Picture_Error;

         end case;

         --  Blank when zero either if the PIC does not contain a '9' or if
         --  requested by the user and no '*'

         Pic.Blank_When_Zero :=
           (Computed_BWZ or Pic.Blank_When_Zero) and not Pic.Star_Fill;

         --  Star fill if '*' and no '9'.

         Pic.Star_Fill := Pic.Star_Fill and Computed_BWZ;

         if not At_End then
            Set_State (Reject);
         end if;

      end Picture_String;

   --  Start of processing for Precalculate

   begin
      Picture_String;

      if State = Reject then
         raise Picture_Error;
      end if;

   exception

      when Constraint_Error =>

         --  To deal with special cases like null strings.

      raise Picture_Error;

   end Precalculate;

   -------------------
   -- Format_Number --
   -------------------


   function Format_Number
     (Pic                 : Format_Record;
      Number              : String;
      Currency_Symbol     : Wide_String;
      Fill_Character      : Wide_Character;
      Separator_Character : Wide_Character;
      Radix_Point         : Wide_Character)
      return                Wide_String
   is
      Attrs    : Number_Attributes := Parse_Number_String (Number);
      Position : Integer;
      Rounded  : String := Number;

      Sign_Position : Integer := Pic.Sign_Position; --  may float.

      Answer        : Wide_String (1 .. Pic.Picture.Length);
      Last          : Integer;
      Currency_Pos  : Integer := Pic.Start_Currency;

      Dollar : Boolean := False;
      --  Overridden immediately if necessary.

      Zero : Boolean := True;
      --  Set to False when a non-zero digit is output.

   begin

      --  If the picture has fewer decimal places than the number, the image
      --  must be rounded according to the usual rules.

      if Attrs.Has_Fraction then
         declare
            R : constant Integer :=
              (Attrs.End_Of_Fraction - Attrs.Start_Of_Fraction + 1)
                - Pic.Max_Trailing_Digits;
            R_Pos : Integer;

         begin
            if R > 0 then
               R_Pos := Rounded'Length - R;

               if Rounded (R_Pos + 1) > '4' then

                  if Rounded (R_Pos) = '.' then
                     R_Pos := R_Pos - 1;
                  end if;

                  if Rounded (R_Pos) /= '9' then
                     Rounded (R_Pos) := Character'Succ (Rounded (R_Pos));
                  else
                     Rounded (R_Pos) := '0';
                     R_Pos := R_Pos - 1;

                     while R_Pos > 1 loop
                        if Rounded (R_Pos) = '.' then
                           R_Pos := R_Pos - 1;
                        end if;

                        if Rounded (R_Pos) /= '9' then
                           Rounded (R_Pos) := Character'Succ (Rounded (R_Pos));
                           exit;
                        else
                           Rounded (R_Pos) := '0';
                           R_Pos := R_Pos - 1;
                        end if;
                     end loop;

                     --  The rounding may add a digit in front. Either the
                     --  leading blank or the sign (already captured) can
                     --  be overwritten.

                     if R_Pos = 1 then
                        Rounded (R_Pos) := '1';
                        Attrs.Start_Of_Int := Attrs.Start_Of_Int - 1;
                     end if;
                  end if;
               end if;
            end if;
         end;
      end if;
      for J in Answer'Range loop
         Answer (J) := To_Wide (Pic.Picture.Expanded (J));
      end loop;


      if Pic.Start_Currency /= Invalid_Position then
         Dollar := Answer (Pic.Start_Currency) = '$';
      end if;

      --  Fix up "direct inserts" outside the playing field. Set up as one
      --  loop to do the beginning, one (reverse) loop to do the end.

      Last := 1;
      loop
         exit when Last = Pic.Start_Float;
         exit when Last = Pic.Radix_Position;
         exit when Answer (Last) = '9';

         case Answer (Last) is

            when '_' =>
               Answer (Last) := Separator_Character;

            when 'b' =>
               Answer (Last) := ' ';

            when others =>
               null;

         end case;

         exit when Last = Answer'Last;

         Last := Last + 1;
      end loop;

      --  Now for the end...

      for I in reverse Last .. Answer'Last loop
         exit when I = Pic.Radix_Position;

         --  Do this test First, Separator_Character can equal Pic.Floater.

         if Answer (I) = Pic.Floater then
            exit;
         end if;

         case Answer (I) is

            when '_' =>
               Answer (I) := Separator_Character;

            when 'b' =>
               Answer (I) := ' ';

            when '9' =>
               exit;

            when others =>
               null;

         end case;
      end loop;

      --  Non-floating sign

      if Pic.Start_Currency /= -1
        and then Answer (Pic.Start_Currency) = '#'
        and then Pic.Floater /= '#'
      then
         if Currency_Symbol'Length >
            Pic.End_Currency - Pic.Start_Currency + 1
         then
            raise Picture_Error;

         elsif Currency_Symbol'Length =
            Pic.End_Currency - Pic.Start_Currency + 1
         then
            Answer (Pic.Start_Currency .. Pic.End_Currency) :=
              Currency_Symbol;

         elsif Pic.Radix_Position = Invalid_Position
           or else Pic.Start_Currency < Pic.Radix_Position
         then
            Answer (Pic.Start_Currency .. Pic.End_Currency) :=
                                                        (others => ' ');
            Answer (Pic.End_Currency - Currency_Symbol'Length + 1 ..
                    Pic.End_Currency) := Currency_Symbol;

         else
            Answer (Pic.Start_Currency .. Pic.End_Currency) :=
                                                        (others => ' ');
            Answer (Pic.Start_Currency ..
                    Pic.Start_Currency + Currency_Symbol'Length - 1) :=
                                                        Currency_Symbol;
         end if;
      end if;

      --  Fill in leading digits

      if Attrs.End_Of_Int - Attrs.Start_Of_Int + 1 >
                                                Pic.Max_Leading_Digits
      then
         raise Layout_Error;
      end if;

      if Pic.Radix_Position = Invalid_Position then
         Position := Answer'Last;
      else
         Position := Pic.Radix_Position - 1;
      end if;

      for I in reverse Attrs.Start_Of_Int .. Attrs.End_Of_Int loop

         while Answer (Position) /= '9'
           and Answer (Position) /= Pic.Floater
         loop
            if Answer (Position) = '_' then
               Answer (Position) := Separator_Character;

            elsif Answer (Position) = 'b' then
               Answer (Position) := ' ';
            end if;

            Position := Position - 1;
         end loop;

         Answer (Position) := To_Wide (Rounded (I));

         if Rounded (I) /= '0' then
            Zero := False;
         end if;

         Position := Position - 1;
      end loop;

      --  Do lead float

      if Pic.Start_Float = Invalid_Position then

         --  No leading floats, but need to change '9' to '0', '_' to
         --  Separator_Character and 'b' to ' '.

         for I in Last .. Position loop

            --  Last set when fixing the "uninteresting" leaders above.
            --  Don't duplicate the work.

            if Answer (I) = '9' then
               Answer (I) := '0';

            elsif Answer (I) = '_' then
               Answer (I) := Separator_Character;

            elsif Answer (I) = 'b' then
               Answer (I) := ' ';

            end if;

         end loop;

      elsif Pic.Floater = '<'
              or else
            Pic.Floater = '+'
              or else
            Pic.Floater = '-'
      then
         for I in Pic.End_Float .. Position loop --  May be null range.
            if Answer (I) = '9' then
               Answer (I) := '0';

            elsif Answer (I) = '_' then
               Answer (I) := Separator_Character;

            elsif Answer (I) = 'b' then
               Answer (I) := ' ';

            end if;
         end loop;

         if Position > Pic.End_Float then
            Position := Pic.End_Float;
         end if;

         for I in Pic.Start_Float .. Position - 1 loop
            Answer (I) := ' ';
         end loop;

         Answer (Position) := Pic.Floater;
         Sign_Position     := Position;

      elsif Pic.Floater = '$' then

         for I in Pic.End_Float .. Position loop --  May be null range.
            if Answer (I) = '9' then
               Answer (I) := '0';

            elsif Answer (I) = '_' then
               Answer (I) := ' ';   --  no separator before leftmost digit.

            elsif Answer (I) = 'b' then
               Answer (I) := ' ';
            end if;
         end loop;

         if Position > Pic.End_Float then
            Position := Pic.End_Float;
         end if;

         for I in Pic.Start_Float .. Position - 1 loop
            Answer (I) := ' ';
         end loop;

         Answer (Position) := Pic.Floater;
         Currency_Pos      := Position;

      elsif Pic.Floater = '*' then

         for I in Pic.End_Float .. Position loop --  May be null range.
            if Answer (I) = '9' then
               Answer (I) := '0';

            elsif Answer (I) = '_' then
               Answer (I) := Separator_Character;

            elsif Answer (I) = 'b' then
               Answer (I) := '*';
            end if;
         end loop;

         if Position > Pic.End_Float then
            Position := Pic.End_Float;
         end if;

         for I in Pic.Start_Float .. Position loop
            Answer (I) := '*';
         end loop;

      else
         if Pic.Floater = '#' then
            Currency_Pos := Currency_Symbol'Length;
         end if;

         for I in reverse Pic.Start_Float .. Position loop
            case Answer (I) is

               when '*' =>
                  Answer (I) := Fill_Character;

               when 'Z' | 'b' | '/' | '0' =>
                  Answer (I) := ' ';

               when '9' =>
                  Answer (I) := '0';

               when '.' | 'V' | 'v' | '<' | '$' | '+' | '-' =>
                  null;

               when '#' =>
                  if Currency_Pos = 0 then
                     Answer (I) := ' ';
                  else
                     Answer (I)   := Currency_Symbol (Currency_Pos);
                     Currency_Pos := Currency_Pos - 1;
                  end if;

               when '_' =>

                  case Pic.Floater is

                     when '*' =>
                        Answer (I) := Fill_Character;

                     when 'Z' | 'b' =>
                        Answer (I) := ' ';

                     when '#' =>
                        if Currency_Pos = 0 then
                           Answer (I) := ' ';

                        else
                           Answer (I)   := Currency_Symbol (Currency_Pos);
                           Currency_Pos := Currency_Pos - 1;
                        end if;

                     when others =>
                        null;

                  end case;

               when others =>
                  null;

            end case;
         end loop;

         if Pic.Floater = '#' and then Currency_Pos /= 0 then
            raise Layout_Error;
         end if;
      end if;

      --  Do sign

      if Sign_Position = Invalid_Position then
         if Attrs.Negative then
            raise Layout_Error;
         end if;

      else
         if Attrs.Negative then
            case Answer (Sign_Position) is
               when 'C' | 'D' | '-' =>
                  null;

               when '+' =>
                  Answer (Sign_Position) := '-';

               when '<' =>
                  Answer (Sign_Position)   := '(';
                  Answer (Pic.Second_Sign) := ')';

               when others =>
                  raise Picture_Error;

            end case;

         else --  positive

            case Answer (Sign_Position) is

               when '-' =>
                  Answer (Sign_Position) := ' ';

               when '<' | 'C' | 'D' =>
                  Answer (Sign_Position)   := ' ';
                  Answer (Pic.Second_Sign) := ' ';

               when '+' =>
                  null;

               when others =>
                  raise Picture_Error;

            end case;
         end if;
      end if;

      --  Fill in trailing digits

      if Pic.Max_Trailing_Digits > 0 then

         if Attrs.Has_Fraction then
            Position := Attrs.Start_Of_Fraction;
            Last     := Pic.Radix_Position + 1;

            for I in Last .. Answer'Last loop

               if Answer (I) = '9' or Answer (I) = Pic.Floater then
                  Answer (I) := To_Wide (Rounded (Position));

                  if Rounded (Position) /= '0' then
                     Zero := False;
                  end if;

                  Position := Position + 1;
                  Last     := I + 1;

                  --  Used up fraction but remember place in Answer

                  exit when Position > Attrs.End_Of_Fraction;

               elsif Answer (I) = 'b' then
                  Answer (I) := ' ';

               elsif Answer (I) = '_' then
                  Answer (I) := Separator_Character;

               end if;

               Last := I + 1;
            end loop;

            Position := Last;

         else
            Position := Pic.Radix_Position + 1;
         end if;

         --  Now fill remaining 9's with zeros and _ with separators

         Last := Answer'Last;

         for I in Position .. Last loop
            if Answer (I) = '9' then
               Answer (I) := '0';

            elsif Answer (I) = Pic.Floater then
               Answer (I) := '0';

            elsif Answer (I) = '_' then
               Answer (I) := Separator_Character;

            elsif Answer (I) = 'b' then
               Answer (I) := ' ';

            end if;
         end loop;

         Position := Last + 1;

      else
         if Pic.Floater = '#' and then Currency_Pos /= 0 then
            raise Layout_Error;
         end if;

         --  No trailing digits, but now I may need to stick in a currency
         --  symbol or sign.

         if Pic.Start_Currency = Invalid_Position then
            Position := Answer'Last + 1;
         else
            Position := Pic.Start_Currency;
         end if;
      end if;

      for I in Position .. Answer'Last loop

         if Pic.Start_Currency /= Invalid_Position and then
            Answer (Pic.Start_Currency) = '#' then
            Currency_Pos := 1;
         end if;

         --  Note: There are some weird cases I can imagine with 'b' or '#'
         --  in currency strings where the following code will cause
         --  glitches. The trick is to tell when the character in the
         --  answer should be checked, and when to look at the original
         --  string. Some other time. RIE 11/26/96 ???

         case Answer (I) is
            when '*' =>
               Answer (I) := Fill_Character;

            when 'b' =>
               Answer (I) := ' ';

            when '#' =>
               if Currency_Pos > Currency_Symbol'Length then
                  Answer (I) := ' ';

               else
                  Answer (I)   := Currency_Symbol (Currency_Pos);
                  Currency_Pos := Currency_Pos + 1;
               end if;

            when '_' =>

               case Pic.Floater is

                  when '*' =>
                     Answer (I) := Fill_Character;

                  when 'Z' | 'z' =>
                     Answer (I) := ' ';

                  when '#' =>
                     if Currency_Pos > Currency_Symbol'Length then
                        Answer (I) := ' ';
                     else
                        Answer (I)   := Currency_Symbol (Currency_Pos);
                        Currency_Pos := Currency_Pos + 1;
                     end if;

                  when others =>
                     null;

               end case;

            when others =>
               exit;

         end case;
      end loop;

      --  Now get rid of Blank_when_Zero and complete Star fill.

      if Zero and Pic.Blank_When_Zero then

         --  Value is zero, and blank it.

         Last := Answer'Last;

         if Dollar then
            Last := Last - 1 + Currency_Symbol'Length;
         end if;

         if Pic.Radix_Position /= Invalid_Position and then
            Answer (Pic.Radix_Position) = 'V' then
            Last := Last - 1;
         end if;

         return Wide_String'(1 .. Last => ' ');

      elsif Zero and Pic.Star_Fill then
         Last := Answer'Last;

         if Dollar then
            Last := Last - 1 + Currency_Symbol'Length;
         end if;

         if Pic.Radix_Position /= Invalid_Position then

            if Answer (Pic.Radix_Position) = 'V' then
               Last := Last - 1;

            elsif Dollar then
               if Pic.Radix_Position > Pic.Start_Currency then
                  return Wide_String' (1 .. Pic.Radix_Position - 1 => '*') &
                     Radix_Point &
                     Wide_String' (Pic.Radix_Position + 1 .. Last => '*');

               else
                  return
                     Wide_String'
                     (1 ..
                      Pic.Radix_Position + Currency_Symbol'Length - 2
                                             => '*') &
                     Radix_Point &
                     Wide_String'
                       (Pic.Radix_Position + Currency_Symbol'Length .. Last
                                             => '*');
               end if;

            else
               return
                 Wide_String'(1 .. Pic.Radix_Position - 1 => '*') &
                 Radix_Point &
                 Wide_String'(Pic.Radix_Position + 1 .. Last => '*');
            end if;
         end if;

         return Wide_String' (1 .. Last => '*');
      end if;

      --  This was once a simple return statement, now there are nine
      --  different return cases.  Not to mention the five above to deal
      --  with zeros.  Why not split things out?

      --  Processing the radix and sign expansion separately
      --  would require lots of copying--the string and some of its
      --  indicies--without really simplifying the logic.  The cases are:

      --  1) Expand $, replace '.' with Radix_Point
      --  2) No currency expansion, replace '.' with Radix_Point
      --  3) Expand $, radix blanked
      --  4) No currency expansion, radix blanked
      --  5) Elide V
      --  6) Expand $, Elide V
      --  7) Elide V, Expand $ (Two cases depending on order.)
      --  8) No radix, expand $
      --  9) No radix, no currency expansion

      if Pic.Radix_Position /= Invalid_Position then

         if Answer (Pic.Radix_Position) = '.' then
            Answer (Pic.Radix_Position) := Radix_Point;

            if Dollar then

               --  1) Expand $, replace '.' with Radix_Point

               return Answer (1 .. Currency_Pos - 1) & Currency_Symbol &
                  Answer (Currency_Pos + 1 .. Answer'Last);

            else
               --  2) No currency expansion, replace '.' with Radix_Point

               return Answer;
            end if;

         elsif Answer (Pic.Radix_Position) = ' ' then --  blanked radix.
            if Dollar then

               --  3) Expand $, radix blanked

               return Answer (1 .. Currency_Pos - 1) & Currency_Symbol &
                 Answer (Currency_Pos + 1 .. Answer'Last);

            else
               --  4) No expansion, radix blanked

               return Answer;
            end if;

         --  V cases

         else
            if not Dollar then

               --  5) Elide V

               return Answer (1 .. Pic.Radix_Position - 1) &
                  Answer (Pic.Radix_Position + 1 .. Answer'Last);

            elsif Currency_Pos < Pic.Radix_Position then

               --  6) Expand $, Elide V

               return Answer (1 .. Currency_Pos - 1) & Currency_Symbol &
                  Answer (Currency_Pos + 1 .. Pic.Radix_Position - 1) &
                  Answer (Pic.Radix_Position + 1 .. Answer'Last);

            else
               --  7) Elide V, Expand $

               return Answer (1 .. Pic.Radix_Position - 1) &
                  Answer (Pic.Radix_Position + 1 .. Currency_Pos - 1) &
                  Currency_Symbol &
                  Answer (Currency_Pos + 1 .. Answer'Last);
            end if;
         end if;

      elsif Dollar then

         --  8) No radix, expand $

         return Answer (1 .. Currency_Pos - 1) & Currency_Symbol &
            Answer (Currency_Pos + 1 .. Answer'Last);

      else
         --  9) No radix, no currency expansion

         return Answer;
      end if;

   end Format_Number;

   ------------
   -- Expand --
   ------------

   function Expand (Picture : in String) return String is
      Result        : String (1 .. MAX_PICSIZE);
      Picture_Index : Integer := Picture'First;
      Result_Index  : Integer := Result'First;
      Count         : Natural;
      Last          : Integer;

   begin
      if Picture'Length < 1 then
         raise Picture_Error;
      end if;

      if Picture (Picture'First) = '(' then
         raise Picture_Error;
      end if;

      loop
         case Picture (Picture_Index) is

            when '(' =>

               --  We now need to scan out the count after a left paren.
               --  In the non-wide version we used Integer_IO.Get, but
               --  that is not convenient here, since we don't want to
               --  drag in normal Text_IO just for this purpose. So we
               --  do the scan ourselves, with the normal validity checks.

               Last := Picture_Index + 1;
               Count := 0;

               if Picture (Last) not in '0' .. '9' then
                  raise Picture_Error;
               end if;

               Count := Character'Pos (Picture (Last)) - Character'Pos ('0');
               Last := Last + 1;

               loop
                  if Last > Picture'Last then
                     raise Picture_Error;
                  end if;

                  if Picture (Last) = '_' then
                     if Picture (Last - 1) = '_' then
                        raise Picture_Error;
                     end if;

                  elsif Picture (Last) = ')' then
                     exit;

                  elsif Picture (Last) not in '0' .. '9' then
                     raise Picture_Error;

                  else
                     Count := Count * 10
                                +  Character'Pos (Picture (Last)) -
                                   Character'Pos ('0');
                  end if;

                  Last := Last + 1;
               end loop;

               --  In what follows note that one copy of the repeated
               --  character has already been made, so a count of one is a
               --  no-op, and a count of zero erases a character.

               for I in 2 .. Count loop
                  Result (Result_Index + I - 2) := Picture (Picture_Index - 1);
               end loop;

               Result_Index := Result_Index + Count - 1;

               --  Last was a ')' throw it away too.

               Picture_Index := Last + 1;

            when ')' =>
               raise Picture_Error;

            when others =>
               Result (Result_Index) := Picture (Picture_Index);
               Picture_Index := Picture_Index + 1;
               Result_Index := Result_Index + 1;

         end case;

         exit when Picture_Index > Picture'Last;
      end loop;

      return Result (1 .. Result_Index - 1);

   exception
      when others =>
         raise Picture_Error;

   end Expand;

   -----------
   -- Valid --
   -----------

   function Valid
     (Pic_String      : in String;
      Blank_When_Zero : in Boolean := False)
      return            Boolean
   is
   begin
      declare
         Expanded_Pic : constant String := Expand (Pic_String);
         --  Raises Picture_Error if Item not well-formed

         Format_Rec : Format_Record;

      begin
         Format_Rec.Picture := (Expanded_Pic'Length, Expanded_Pic);
         Format_Rec.Blank_When_Zero := Blank_When_Zero;
         Format_Rec.Original_BWZ := Blank_When_Zero;
         Precalculate (Format_Rec);

         --  False only if Blank_When_0 is True but the pic string
         --  has a '*'

         return not Blank_When_Zero or
           Strings_Fixed.Index (Expanded_Pic, "*") = 0;
      end;

   exception
      when others => return False;

   end Valid;

   ----------------
   -- To_Picture --
   ----------------

   function To_Picture
     (Pic_String      : in String;
      Blank_When_Zero : in Boolean := False)
      return            Picture
   is
      Result : Picture;

   begin
      declare
         Item : constant String := Expand (Pic_String);

      begin
         Result.Contents.Picture         := (Item'Length, Item);
         Result.Contents.Original_BWZ := Blank_When_Zero;
         Result.Contents.Blank_When_Zero := Blank_When_Zero;
         Precalculate (Result.Contents);
         return Result;
      end;

   exception
      when others =>
         raise Picture_Error;

   end To_Picture;

   ----------------
   -- Pic_String --
   ----------------

   --  The following ensures that we return B and not b being careful not
   --  to break things which expect lower case b for blank. See CXF3A02.

   function Pic_String (Pic : in Picture) return String is
      Temp : String (1 .. Pic.Contents.Picture.Length) :=
                              Pic.Contents.Picture.Expanded;
   begin
      for I in Temp'Range loop
         if Temp (I) = 'b' then Temp (I) := 'B'; end if;
      end loop;

      return Temp;
   end Pic_String;

   ---------------------
   -- Blank_When_Zero --
   ---------------------

   function Blank_When_Zero (Pic : in Picture) return Boolean is
   begin
      return Pic.Contents.Original_BWZ;
   end Blank_When_Zero;

   --------------------
   -- Decimal_Output --
   --------------------

   package body Decimal_Output is

      ------------
      -- Length --
      ------------

      function Length
        (Pic      : in Picture;
         Currency : in Wide_String := Default_Currency)
         return     Natural
      is
         Picstr     : constant String := Pic_String (Pic);
         V_Adjust   : Integer := 0;
         Cur_Adjust : Integer := 0;

      begin
         --  Check if Picstr has 'V' or '$'

         --  If 'V', then length is 1 less than otherwise

         --  If '$', then length is Currency'Length-1 more than otherwise

         --  This should use the string handling package ???

         for I in Picstr'Range loop
            if Picstr (I) = 'V' then
               V_Adjust := -1;

            elsif Picstr (I) = '$' then
               Cur_Adjust := Currency'Length - 1;
            end if;
         end loop;

         return Picstr'Length - V_Adjust + Cur_Adjust;
      end Length;

      -----------
      -- Valid --
      -----------

      function Valid
        (Item     : Num;
         Pic      : in Picture;
         Currency : in Wide_String := Default_Currency)
         return     Boolean
      is
      begin
         declare
            Temp : constant Wide_String := Image (Item, Pic, Currency);
            pragma Warnings (Off, Temp);

         begin
            return True;
         end;

      exception
         when Layout_Error => return False;

      end Valid;

      -----------
      -- Image --
      -----------

      function Image
        (Item       : in Num;
         Pic        : in Picture;
         Currency   : in Wide_String    := Default_Currency;
         Fill       : in Wide_Character := Default_Fill;
         Separator  : in Wide_Character := Default_Separator;
         Radix_Mark : in Wide_Character := Default_Radix_Mark)
         return       Wide_String
      is
      begin
         return Format_Number
            (Pic.Contents, Num'Image (Item),
             Currency, Fill, Separator, Radix_Mark);
      end Image;

      ---------
      -- Put --
      ---------

      procedure Put
        (File       : in Wide_Text_IO.File_Type;
         Item       : in Num;
         Pic        : in Picture;
         Currency   : in Wide_String    := Default_Currency;
         Fill       : in Wide_Character := Default_Fill;
         Separator  : in Wide_Character := Default_Separator;
         Radix_Mark : in Wide_Character := Default_Radix_Mark)
      is
      begin
         Wide_Text_IO.Put (File, Image (Item, Pic,
                                   Currency, Fill, Separator, Radix_Mark));
      end Put;

      procedure Put
        (Item       : in Num;
         Pic        : in Picture;
         Currency   : in Wide_String    := Default_Currency;
         Fill       : in Wide_Character := Default_Fill;
         Separator  : in Wide_Character := Default_Separator;
         Radix_Mark : in Wide_Character := Default_Radix_Mark)
      is
      begin
         Wide_Text_IO.Put (Image (Item, Pic,
                             Currency, Fill, Separator, Radix_Mark));
      end Put;

      procedure Put
        (To         : out Wide_String;
         Item       : in Num;
         Pic        : in Picture;
         Currency   : in Wide_String    := Default_Currency;
         Fill       : in Wide_Character := Default_Fill;
         Separator  : in Wide_Character := Default_Separator;
         Radix_Mark : in Wide_Character := Default_Radix_Mark)
      is
         Result : constant Wide_String :=
           Image (Item, Pic, Currency, Fill, Separator, Radix_Mark);

      begin
         if Result'Length > To'Length then
            raise Wide_Text_IO.Layout_Error;
         else
            Strings_Wide_Fixed.Move (Source => Result, Target => To,
                                     Justify => Strings.Right);
         end if;
      end Put;

   end Decimal_Output;

end Ada.Wide_Text_IO.Editing;
