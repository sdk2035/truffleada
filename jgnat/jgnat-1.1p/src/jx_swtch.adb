------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J X _ S W T C H                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.5 $
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with JVM;      use JVM;
with JVM.API;  use JVM.API;
with Nlists;   use Nlists;
with Sem_Eval; use Sem_Eval;
with Sinfo;    use Sinfo;
with Types;    use Types;
with Uintp;    use Uintp;

package body Jx_Swtch is

   procedure Generate_Switch
     (Alterns : List_Id;
      Action  : Switch_Action;
      Obj     : Local_Var_Id := Null_Local_Var)
   is
      Default_Lbl : constant Label_Id := New_Label;
      Exit_Lbl    : constant Label_Id := New_Label;
      Altern      : Node_Id := First_Non_Pragma (Alterns);
      Altern_Lbl  : Label_Id;
      Choice      : Node_Id;
      Range_Low   : Uint;
      Range_High  : Uint;
      Case_Temp   : Local_Var_Id := Null_Local_Var;
      Empty_Alt   : Boolean;
      All_Empty   : Boolean;
      Others_Alt  : Boolean;

      Range_Test_Threshold : constant := 100;
      --  Limit on size of a single choice range; ranges in excess of
      --  this value will be handled with range test code rather than
      --  via the switch table.

      type Altern_Count is range 0 .. 1000;

      subtype Altern_Range is
        Altern_Count range Altern_Count'First + 1 .. Altern_Count'Last;

      Alt_Index : Altern_Count := 0;

      Alt_Labels : array (Altern_Range) of Label_Id;

      procedure Generate_Range_Test
        (Low     : Uint;
         High    : Uint;
         Alt_Lbl : Label_Id);
      --  Generates code to test the case expression against the range
      --  Low .. High, branching to Alt_Lbl if the test succeeds. The
      --  first time this procedure is called it will save the evaluated
      --  case expression (on top of stack) in a local temporary, from
      --  which it can be reloaded on subsequent calls (saved in variable
      --  Case_Temp declared in Generate_Switch). Requires that the case
      --  expression be on top of stack prior to being called, and leaves
      --  the case expression on top of stack after generating the range test.

      procedure Generate_Range_Test
        (Low     : Uint;
         High    : Uint;
         Alt_Lbl : Label_Id)
      is
         Continue : Label_Id := New_Label;
      begin
         --  First time for generating a range test, so save the case
         --  expression in a temporary but leave it on top of the stack
         --  for the lower bound test.

         if Case_Temp = Null_Local_Var then
            Case_Temp := New_Local_Var ("_case_tmp", Top_Type);
            Gen_Duplicate;
            Gen_Store_Local (Case_Temp);
         end if;

         --  Perform lower bound test, branching past upper bound test
         --  if the case expression is less than Low.

         Gen_Push_Int (Low);
         Gen_Compare_Branch_Less (Continue);

         --  Perform upper bound test, branching to the case alternative
         --  if the case expression is less or equal to High (meaning
         --  its in range).

         Gen_Load_Local (Case_Temp);
         Gen_Push_Int (High);
         Gen_Compare_Branch_Less_Equal (Alt_Lbl);

         --  Leave the case value on the stack, continuing on to the next
         --  range test or the switch itself.

         Gen_Label (Continue);
         Gen_Load_Local (Case_Temp);
      end Generate_Range_Test;

   --  Start of processing for Generate_Switch

   begin
      --  Generate the switch instruction and its table of switch entries

      Start_Switch_Table (Default_Lbl);

      All_Empty := True;

      while Present (Altern) loop
         Alt_Index := Alt_Index + 1;

         Altern_Lbl := New_Label;
         Alt_Labels (Alt_Index) := Altern_Lbl;

         Empty_Alt  := True;
         Others_Alt := False;

         Choice := First (Discrete_Choices (Altern));
         while Present (Choice) loop
            if Nkind (Choice) = N_Others_Choice then
               Alt_Labels (Alt_Index) := Default_Lbl;
               Others_Alt := True;

            elsif Nkind (Choice) = N_Range then
               Range_Low  := Expr_Value (Low_Bound (Choice));
               Range_High := Expr_Value (High_Bound (Choice));

               --  If the range is large, then generate a range test
               --  rather than entering the choice in the switch table.
               --  This is really just a simple-minded heuristic to try
               --  and avoid generating huge tables for case statements
               --  with very large ranges (such as 0..Integer'Last), but
               --  is also not sufficient to prevent them since it won't
               --  catch cases where the choices span a large range even
               --  after accounting for range choices. Eventually such
               --  cases should be handled (in conjunction with this
               --  heuristic) by package JVM by generating lookupswitch
               --  instructions for sparse choice sets. ???

               if Range_High - Range_Low > Range_Test_Threshold then
                  Generate_Range_Test (Range_Low, Range_High, Altern_Lbl);
                  Empty_Alt := False;

               elsif Range_High >= Range_Low then
                  Add_Switch_Pair (Range_Low, Range_High, Altern_Lbl);
                  Empty_Alt := False;
               end if;

            elsif Nkind (Choice) = N_Subtype_Indication then
               if Present (Constraint (Choice)) then
                  Range_Low
                    := Expr_Value
                         (Low_Bound (Range_Expression (Constraint (Choice))));
                  Range_High
                    := Expr_Value
                         (High_Bound (Range_Expression (Constraint (Choice))));
               else
                  Range_Low
                    := Expr_Value
                         (Type_Low_Bound (Entity (Subtype_Mark (Choice))));
                  Range_High
                    := Expr_Value
                         (Type_High_Bound (Entity (Subtype_Mark (Choice))));
               end if;

               if Range_High - Range_Low > Range_Test_Threshold then
                  Generate_Range_Test (Range_Low, Range_High, Altern_Lbl);
                  Empty_Alt := False;

               elsif Range_High >= Range_Low then
                  Add_Switch_Pair (Range_Low, Range_High, Altern_Lbl);
                  Empty_Alt := False;
               end if;

            elsif Nkind (Choice) = N_Identifier
              and then Is_Type (Entity (Choice))
            then
               Range_Low  := Expr_Value (Type_Low_Bound (Entity (Choice)));
               Range_High := Expr_Value (Type_High_Bound (Entity (Choice)));

               if Range_High - Range_Low > Range_Test_Threshold then
                  Generate_Range_Test (Range_Low, Range_High, Altern_Lbl);
                  Empty_Alt := False;

               elsif Range_High >= Range_Low then
                  Add_Switch_Pair (Range_Low, Range_High, Altern_Lbl);
                  Empty_Alt := False;
               end if;

            --  The choice is a single expression

            else
               Add_Switch_Pair
                 (Expr_Value (Choice), Expr_Value (Choice), Altern_Lbl);
               Empty_Alt := False;
            end if;

            Choice := Next (Choice);
         end loop;

         if not Empty_Alt then
            All_Empty := False;

         --  Cancel the alternative if it consists entirely of null ranges

         elsif Alt_Labels (Alt_Index) /= Default_Lbl then
            Alt_Labels (Alt_Index) := Null_Label;
         end if;

         Next_Non_Pragma (Altern);
      end loop;

         --  If all of the non-others alternatives have empty choice
         --  ranges, then we cancel the switch statement. The JVM doesn't
         --  allow null case tables (they must have at least one value).
         --  Such cases occur in validation tests. Note that this includes
         --  the case of a case statement with only an others choice.

      if All_Empty then
         Gen_Pop;
         Cancel_Switch_Table;
      else
         End_Switch_Table;
      end if;

      --  Now generate code for each of the alternatives

      Altern := First_Non_Pragma (Alterns);
      Alt_Index := 0;

      while Present (Altern) loop
         Alt_Index := Alt_Index + 1;

         --  If all choices of the alternative were null ranges,
         --  then we want to suppress the generation of the code
         --  for that alternative.

         if Alt_Labels (Alt_Index) /= Null_Label then
            Gen_Label (Alt_Labels (Alt_Index));

            Action (Altern, Obj);
         end if;

         Next_Non_Pragma (Altern);

         if Present (Altern) and then Alt_Labels (Alt_Index) /= Null_Label then
            Gen_Goto (Exit_Lbl);
         end if;
      end loop;

      --  If there was not an explicit 'others' alternative, then
      --  generate code to raise Program_Error for the default case.
      --  (But it may be better to always generate a separate exception
      --  default and represent 'others' alternatives as normal alternatives
      --  via Others_Discrete_Choices. ???)

      if Alt_Labels (Alt_Index) /= Default_Lbl
        and then Alt_Labels (Alt_Index) /= Null_Label
      then
         Gen_Goto (Exit_Lbl);
         Gen_Label (Default_Lbl);
         Gen_Default_Object (API_Class (Ada_Program_Error));
         Gen_Exception_Throw;
      end if;

      Gen_Label (Exit_Lbl);
   end Generate_Switch;

end Jx_Swtch;
