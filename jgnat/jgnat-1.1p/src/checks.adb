------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C H E C K S                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.178 $
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Ch2;  use Exp_Ch2;
with Exp_Util; use Exp_Util;
with Expander; use Expander;
with Elists;   use Elists;
with Freeze;   use Freeze;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Urealp;   use Urealp;

package body Checks is

   --  General note: many of these routines are concerned with generating
   --  checking code to make sure that constraint error is raised at runtime.
   --  Clearly this code is only needed if the expander is active, since
   --  otherwise we will not be generating code or going into the runtime
   --  execution anyway.

   --  We therefore disconnect most of these checks if the expander is
   --  inactive. This has the additional benefit that we do not need to
   --  worry about the tree being messed up by previous errors (since errors
   --  turn off expansion anyway).

   --  There are a few exceptions to the above rule. For instance routines
   --  such as Apply_Scalar_Range_Check that do not insert any code can be
   --  safely called even when the Expander is inactive (but Errors_Detected
   --  is 0). The benefit of executing this code when expansion is off, is
   --  the ability to emit constraint error warning for static expressions
   --  even when we are not generating code.

   ----------------------------
   -- Local Subprogram Specs --
   ----------------------------

   procedure Apply_Selected_Length_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Do_Static  : Boolean);
   --  This is the subprogram that does all the work for Apply_Length_Check
   --  and Apply_Static_Length_Check. Expr, Target_Typ and Source_Typ are as
   --  described for the above routines. The Do_Static flag indicates that
   --  only a static check is to be done.

   procedure Apply_Selected_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Do_Static  : Boolean);
   --  This is the subprogram that does all the work for Apply_Range_Check.
   --  Expr, Target_Typ and Source_Typ are as described for the above
   --  routine. The Do_Static flag indicates that only a static check is
   --  to be done.

   function Get_Discriminal (E : Entity_Id; Bound : Node_Id) return Node_Id;
   --  If a discriminal is used in constraining a prival, Return reference
   --  to the discriminal of the protected body (which renames the parameter
   --  of the enclosing protected operation). This clumsy transformation is
   --  needed because privals are created too late and their actual subtypes
   --  are not available when analysing the bodies of the protected operations.
   --  To be cleaned up???

   function Guard_Access
     (Cond    : Node_Id;
      Loc     : Source_Ptr;
      Ck_Node : Node_Id)
      return    Node_Id;
   --  In the access type case, guard the test with a test to ensure
   --  that the access value is non-null, since the checks do not
   --  not apply to null access values.

   procedure Install_Static_Check (R_Cno : Node_Id; Loc : Source_Ptr);
   --  Called by Apply_{Length,Range}_Checks to rewrite the tree with the
   --  Constraint_Error node.

   function Selected_Length_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Warn_Node  : Node_Id)
      return       Check_Result;
   --  Like Apply_Selected_Length_Checks, except it doesn't modify
   --  anything, just returns a list of nodes as described in the spec of
   --  this package for the Range_Check function.

   function Selected_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Warn_Node  : Node_Id)
      return       Check_Result;
   --  Like Apply_Selected_Range_Checks, except it doesn't modify anything,
   --  just returns a list of nodes as described in the spec of this package
   --  for the Range_Check function.

   ------------------------------
   -- Access_Checks_Suppressed --
   ------------------------------

   function Access_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Access_Checks
        or else (Present (E) and then Suppress_Access_Checks (E));
   end Access_Checks_Suppressed;

   -------------------------------------
   -- Accessibility_Checks_Suppressed --
   -------------------------------------

   function Accessibility_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Accessibility_Checks
        or else (Present (E) and then Suppress_Accessibility_Checks (E));
   end Accessibility_Checks_Suppressed;

   -------------------------
   -- Append_Range_Checks --
   -------------------------

   procedure Append_Range_Checks
     (Checks       : Check_Result;
      Stmts        : List_Id;
      Suppress_Typ : Entity_Id;
      Static_Sloc  : Source_Ptr;
      Flag_Node    : Node_Id)
   is
      Internal_Flag_Node   : Node_Id    := Flag_Node;
      Internal_Static_Sloc : Source_Ptr := Static_Sloc;
      Checks_On : constant Boolean :=
                    (not Index_Checks_Suppressed (Suppress_Typ))
                       or else
                    (not Range_Checks_Suppressed (Suppress_Typ));

   begin
      --  For now we just return if Checks_On is false, however this should
      --  be enhanced to check for an always True value in the condition
      --  and to generate a compilation warning???

      if not Checks_On then
         return;
      end if;

      for J in 1 .. 2 loop
         exit when No (Checks (J));

         if Nkind (Checks (J)) = N_Raise_Constraint_Error
           and then Present (Condition (Checks (J)))
         then
            if not Has_Dynamic_Range_Check (Internal_Flag_Node) then
               Append_To (Stmts, Checks (J));
               Set_Has_Dynamic_Range_Check (Internal_Flag_Node);
            end if;

         else
            Append_To
              (Stmts, Make_Raise_Constraint_Error (Internal_Static_Sloc));
         end if;
      end loop;
   end Append_Range_Checks;

   ------------------------
   -- Apply_Access_Check --
   ------------------------

   procedure Apply_Access_Check (N : Node_Id) is
      P : constant Node_Id := Prefix (N);

   begin
      if Inside_A_Generic then
         return;
      end if;

      if Is_Entity_Name (P) then
         Check_Unset_Reference (P);
      end if;

      if Is_Entity_Name (P)
        and then Access_Checks_Suppressed (Entity (P))
      then
         return;

      elsif Access_Checks_Suppressed (Etype (P)) then
         return;

      else
         Set_Do_Access_Check (N, True);
      end if;
   end Apply_Access_Check;

   -------------------------------
   -- Apply_Accessibility_Check --
   -------------------------------

   procedure Apply_Accessibility_Check (N : Node_Id; Typ : Entity_Id) is
      Loc         : constant Source_Ptr := Sloc (N);
      Param_Ent   : constant Entity_Id  := Param_Entity (N);
      Param_Level : Node_Id;
      Type_Level  : Node_Id;

   begin
      if Inside_A_Generic then
         return;

      --  Only apply the run-time check if the access parameter
      --  has an associated extra access level parameter and
      --  when the level of the type is less deep than the level
      --  of the access parameter.

      elsif Present (Param_Ent)
         and then Present (Extra_Accessibility (Param_Ent))
         and then UI_Gt (Object_Access_Level (N),
                         Type_Access_Level (Typ))
         and then not Accessibility_Checks_Suppressed (Param_Ent)
         and then not Accessibility_Checks_Suppressed (Typ)
      then
         Param_Level :=
           New_Occurrence_Of (Extra_Accessibility (Param_Ent), Loc);

         Type_Level :=
           Make_Integer_Literal (Loc, Type_Access_Level (Typ));

         --  Raise Program_Error if the accessibility level of the
         --  the access parameter is deeper than the level of the
         --  target access type.

         Insert_Action (N,
           Make_Raise_Program_Error (Loc,
             Condition =>
               Make_Op_Gt (Loc,
                 Left_Opnd  => Param_Level,
                 Right_Opnd => Type_Level)));

         Analyze_And_Resolve (N);
      end if;
   end Apply_Accessibility_Check;

   -------------------------------------
   -- Apply_Arithmetic_Overflow_Check --
   -------------------------------------

   --  This routine is called only if the type is an integer type, and
   --  a software arithmetic overflow check must be performed for op
   --  (add, subtract, multiply). The check is performed only if
   --  Software_Overflow_Checking is enabled and Do_Overflow_Check
   --  is set. In this case we expand the operation into a more complex
   --  sequence of tests that ensures that overflow is properly caught.

   procedure Apply_Arithmetic_Overflow_Check (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Etype (N);
      Rtyp  : constant Entity_Id  := Root_Type (Typ);
      Siz   : constant Int        := UI_To_Int (Esize (Rtyp));
      Dsiz  : constant Int        := Siz * 2;
      Opnod : Node_Id;
      Ctyp  : Entity_Id;
      Opnd  : Node_Id;
      Cent  : RE_Id;
      Lo    : Uint;
      Hi    : Uint;
      OK    : Boolean;

   begin
      if not Software_Overflow_Checking
        or else not Do_Overflow_Check (N)
        or else not Expander_Active
      then
         return;
      end if;

      --  Nothing to do if the range of the result is known OK

      Determine_Range (N, OK, Lo, Hi);

      --  Note in the test below that we assume that if a bound of the
      --  range is equal to that of the type. That's not quite accurate
      --  but we do this for the following reasons:

      --   a) The way that Determine_Range works, it will typically report
      --      the bounds of the value are the bounds of the type, because
      --      it either can't tell anything more precise, or does not think
      --      it is worth the effort to be more precise.

      --   b) It is very unusual to have a situation in which this would
      --      generate an unnecessary overflow check (an example would be
      --      a subtype with a range 0 .. Integer'Last - 1 to which the
      --      literal value one is added.

      --   c) The alternative is a lot of special casing in this routine
      --      which would partially duplicate the Determine_Range processing.

      if OK
        and then Lo > Expr_Value (Type_Low_Bound  (Typ))
        and then Hi < Expr_Value (Type_High_Bound (Typ))
      then
         return;
      end if;

      --  None of the special case optimizations worked, so there is nothing
      --  for it but to generate the full general case code:

      --    x op y

      --  is expanded into

      --    Typ (Checktyp (x) op Checktyp (y));

      --  where Typ is the type of the original expression, and Checktyp is
      --  an integer type of sufficient length to hold the largest possible
      --  result.

      --  In the case where check type exceeds the size of Long_Long_Integer,
      --  we use a different approach, expanding to:

      --    typ (xxx_With_Ovflo_Check (Integer_64 (x), Integer (y)))

      --  where xxx is Add, Multiply or Subtract as appropriate

      --  Find check type if one exists

      if Dsiz <= Standard_Integer_Size then
         Ctyp := Standard_Integer;

      elsif Dsiz <= Standard_Long_Long_Integer_Size then
         Ctyp := Standard_Long_Long_Integer;

      --  No check type exists, use runtime call

      else
         if Nkind (N) = N_Op_Add then
            Cent := RE_Add_With_Ovflo_Check;
         elsif Nkind (N) = N_Op_Multiply then
            Cent := RE_Multiply_With_Ovflo_Check;
         elsif Nkind (N) = N_Op_Subtract then
            Cent := RE_Subtract_With_Ovflo_Check;
         else
            pragma Assert (False);
            raise Program_Error;
         end if;

         Rewrite (N,
           OK_Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Reference_To (RTE (Cent), Loc),
               Parameter_Associations => New_List (
                 OK_Convert_To (RTE (RE_Integer_64), Left_Opnd  (N)),
                 OK_Convert_To (RTE (RE_Integer_64), Right_Opnd (N))))));

         Analyze_And_Resolve (N, Typ);
         return;
      end if;

      --  If we fall through, we have the case where we do the arithmetic in
      --  the next higher type and get the check by conversion. In these cases
      --  Ctyp is set to the type to be used as the check type.

      Opnod := Relocate_Node (N);

      Opnd := OK_Convert_To (Ctyp, Left_Opnd (Opnod));

      Analyze (Opnd);
      Set_Etype (Opnd, Ctyp);
      Set_Analyzed (Opnd, True);
      Set_Left_Opnd (Opnod, Opnd);

      Opnd := OK_Convert_To (Ctyp, Right_Opnd (Opnod));

      Analyze (Opnd);
      Set_Etype (Opnd, Ctyp);
      Set_Analyzed (Opnd, True);
      Set_Right_Opnd (Opnod, Opnd);

      --  The type of the operation changes to the base type of the check
      --  type, and we reset the overflow check indication, since clearly
      --  no overflow is possible now that we are using a double length
      --  type. We also set the Analyzed flag to avoid a recursive attempt
      --  to expand the node.

      Set_Etype             (Opnod, Base_Type (Ctyp));
      Set_Do_Overflow_Check (Opnod, False);
      Set_Analyzed          (Opnod, True);

      --  Now build the outer conversion

      Opnd := OK_Convert_To (Typ, Opnod);

      Analyze (Opnd);
      Set_Etype (Opnd, Typ);
      Set_Analyzed (Opnd, True);
      Set_Do_Overflow_Check (Opnd, True);

      Rewrite (N, Opnd);
   end Apply_Arithmetic_Overflow_Check;

   ----------------------------
   -- Apply_Array_Size_Check --
   ----------------------------

   --  Note: Really of course this entre check should be in the backend,
   --  and perhaps this is not quite the right value, but it is good
   --  enough to catch the normal cases (and the relevant ACVC tests!)

   procedure Apply_Array_Size_Check (N : Node_Id; Typ : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Ctyp : constant Entity_Id  := Component_Type (Typ);
      Ent  : constant Entity_Id  := Defining_Identifier (N);
      Decl : Node_Id;
      Lo   : Node_Id;
      Hi   : Node_Id;
      Lob  : Uint;
      Hib  : Uint;
      Siz  : Uint;
      Xtyp : Entity_Id;
      Indx : Node_Id;
      Sizx : Node_Id;
      Code : Node_Id;

      Static : Boolean := True;
      --  Set false if any index subtye bound is non-static

      Umark : constant Uintp.Save_Mark := Uintp.Mark;
      --  We can throw away all the Uint computations here, since they are
      --  done only to generate boolean test results.

      Check_Siz : Uint;
      --  Size to check against

      function Is_Address_Or_Import (Decl : Node_Id) return Boolean;
      --  Determines if Decl is an address clause or Import/Interface pragma
      --  that references the defining identifier of the current declaration.

      --------------------------
      -- Is_Address_Or_Import --
      --------------------------

      function Is_Address_Or_Import (Decl : Node_Id) return Boolean is
      begin
         if Nkind (Decl) = N_At_Clause then
            return Chars (Identifier (Decl)) = Chars (Ent);

         elsif Nkind (Decl) = N_Attribute_Definition_Clause then
            return
              Chars (Decl) = Name_Address
                and then
              Nkind (Name (Decl)) = N_Identifier
                and then
              Chars (Name (Decl)) = Chars (Ent);

         elsif Nkind (Decl) = N_Pragma then
            if (Chars (Decl) = Name_Import
                 or else
                Chars (Decl) = Name_Interface)
              and then Present (Pragma_Argument_Associations (Decl))
            then
               declare
                  F : constant Node_Id :=
                        First (Pragma_Argument_Associations (Decl));

               begin
                  return
                    Present (F)
                      and then
                    Present (Next (F))
                      and then
                    Nkind (Expression (Next (F))) = N_Identifier
                      and then
                    Chars (Expression (Next (F))) = Chars (Ent);
               end;

            else
               return False;
            end if;

         else
            return False;
         end if;
      end Is_Address_Or_Import;

   --  Start of processing for Apply_Array_Size_Check

   begin
      if not Expander_Active
        or else Storage_Checks_Suppressed (Typ)
      then
         return;
      end if;

      --  It is pointless to insert this check inside an _init_proc, because
      --  that's too late, we have already built the object to be the right
      --  size, and if it's too large, too bad!

      if Inside_Init_Proc then
         return;
      end if;

      --  Look head for pragma interface/import or address clause applying
      --  to this entity. If found, we suppress the check entirely. For now
      --  we only look ahead 20 declarations to stop this becoming too slow
      --  Note that eventually this whole routine gets moved to gigi.

      Decl := N;
      for Ctr in 1 .. 20 loop
         Next (Decl);
         exit when No (Decl);

         if Is_Address_Or_Import (Decl) then
            return;
         end if;
      end loop;

      --  First step is to calculate the maximum number of elements. For this
      --  calculation, we use the actual size of the subtype if it is static,
      --  and if a bound of a subtype is non-static, we go to the bound of the
      --  base type.

      Siz := Uint_1;
      Indx := First_Index (Typ);
      while Present (Indx) loop
         Xtyp := Etype (Indx);
         Lo := Type_Low_Bound (Xtyp);
         Hi := Type_High_Bound (Xtyp);

         --  If any bound raises constraint error, we will never get this
         --  far, so there is no need to generate any kind of check.

         if Raises_Constraint_Error (Lo)
              or else
            Raises_Constraint_Error (Hi)
         then
            Uintp.Release (Umark);
            return;
         end if;

         --  Otherwise get bounds values

         if Is_Static_Expression (Lo) then
            Lob := Expr_Value (Lo);
         else
            Lob := Expr_Value (Type_Low_Bound (Base_Type (Xtyp)));
            Static := False;
         end if;

         if Is_Static_Expression (Hi) then
            Hib := Expr_Value (Hi);
         else
            Hib := Expr_Value (Type_High_Bound (Base_Type (Xtyp)));
            Static := False;
         end if;

         Siz := Siz *  UI_Max (Hib - Lob + 1, Uint_0);
         Next_Index (Indx);
      end loop;

      --  Compute the limit against which we want to check. For subprograms,
      --  where the array will go on the stack, we use 8*2**24, which (in
      --  bits) is the size of a 16 megabyte array.

      if Is_Subprogram (Scope (Ent)) then
         Check_Siz := Uint_2 ** 27;
      else
         Check_Siz := Uint_2 ** 31;
      end if;

      --  If we have all static bounds and Siz is too large, then we know we
      --  know we have a storage error right now, so generate message

      if Static and then Siz >= Check_Siz then
         Insert_Action (N,
           Make_Raise_Storage_Error (Loc));
         Error_Msg_N ("?Storage_Error will be raised at run-time", N);
         Uintp.Release (Umark);
         return;
      end if;

      --  Case of component size known at compile time. If the array
      --  size is definitely in range, then we do not need a check.

      if Esize_Known_By_Front_End (Ctyp)
        and then Siz * Esize (Ctyp) < Check_Siz
      then
         Uintp.Release (Umark);
         return;
      end if;

      --  Here if a dynamic check is required

      --  What we do is to build an expression for the size of the array,
      --  which is computed as the 'Size of the array component, times
      --  the size of each dimension.

      Uintp.Release (Umark);

      Sizx :=
        Make_Attribute_Reference (Loc,
          Prefix => New_Occurrence_Of (Ctyp, Loc),
          Attribute_Name => Name_Size);

      Indx := First_Index (Typ);

      for J in 1 .. Number_Dimensions (Typ) loop

         if Sloc (Etype (Indx)) = Sloc (N) then
            Ensure_Defined (Etype (Indx), N);
         end if;

         Sizx :=
           Make_Op_Multiply (Loc,
             Left_Opnd  => Sizx,
             Right_Opnd =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Typ, Loc),
                 Attribute_Name => Name_Length,
                 Expressions => New_List (
                   Make_Integer_Literal (Loc, J))));
         Next_Index (Indx);
      end loop;

      Code :=
        Make_Raise_Storage_Error (Loc,
          Condition =>
            Make_Op_Ge (Loc,
              Left_Opnd  => Sizx,
              Right_Opnd =>
                Make_Integer_Literal (Loc, Check_Siz)));

      Set_Size_Check_Code (Defining_Identifier (N), Code);
      Insert_Action (N, Code);

   end Apply_Array_Size_Check;

   ----------------------------
   -- Apply_Constraint_Check --
   ----------------------------

   procedure Apply_Constraint_Check
     (N          : Node_Id;
      Typ        : Entity_Id;
      No_Sliding : Boolean := False)
   is
      Desig_Typ : Entity_Id;

   begin
      if Inside_A_Generic then
         return;

      elsif Is_Scalar_Type (Typ) then
         Apply_Scalar_Range_Check (N, Typ);

      elsif Is_Array_Type (Typ) then

         if Is_Constrained (Typ) then
            Apply_Length_Check (N, Typ);

            if No_Sliding then
               Apply_Range_Check (N, Typ);
            end if;
         else
            Apply_Range_Check (N, Typ);
         end if;

      elsif (Is_Record_Type (Typ)
               or else Is_Private_Type (Typ))
        and then Has_Discriminants (Base_Type (Typ))
        and then Is_Constrained (Typ)
      then
         Apply_Discriminant_Check (N, Typ);

      elsif Is_Access_Type (Typ) then

         Desig_Typ := Designated_Type (Typ);

         --  No checks necessary if expression statically null

         if Nkind (N) = N_Null then
            null;

         --  No sliding possible on access to arrays

         elsif Is_Array_Type (Desig_Typ) then
            if Is_Constrained (Desig_Typ) then
               Apply_Length_Check (N, Typ);
            end if;

            Apply_Range_Check (N, Typ);

         elsif Has_Discriminants (Base_Type (Desig_Typ))
            and then Is_Constrained (Desig_Typ)
         then
            Apply_Discriminant_Check (N, Typ);
         end if;
      end if;
   end Apply_Constraint_Check;

   ------------------------------
   -- Apply_Discriminant_Check --
   ------------------------------

   procedure Apply_Discriminant_Check
     (N   : Node_Id;
      Typ : Entity_Id;
      Lhs : Node_Id := Empty)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Do_Access : constant Boolean    := Is_Access_Type (Typ);
      S_Typ     : Entity_Id  := Etype (N);
      Cond      : Node_Id;
      T_Typ     : Entity_Id;

      function Is_Component_Initialization return Boolean;
      --  It is possible for an aliased component to have a nominal
      --  unconstrained subtype (through instantiation). If this is a
      --  discriminated component assigned in the expansion of an aggregate
      --  in an initialization, the check must be suppressed. This unusual
      --  situation requires a predicate of its own (see 7503-008).

      ---------------------------------
      -- Is_Component_Initialization --
      ---------------------------------

      function Is_Component_Initialization return Boolean is
         Comp : Entity_Id;
         Pref : Node_Id;
         Var  : Entity_Id;

      begin
         if Nkind (Lhs) /= N_Selected_Component then
            return False;
         else
            Comp := Entity (Selector_Name (Lhs));
            Pref := Prefix (Lhs);
         end if;

         if Ekind (Comp) /= E_Component
           or else not Is_Aliased (Comp)
         then
            return False;
         end if;

         if Is_Entity_Name (Pref) then
            Var := Entity (Pref);
         else
            while Nkind (Pref) = N_Selected_Component loop
               Pref := Prefix (Pref);
            end loop;

            if Is_Entity_Name (Pref) then
               Var := Entity (Pref);
            else
               return False;
            end if;
         end if;

         return not Comes_From_Source (Pref);
      end Is_Component_Initialization;

   --  Start of processing for Apply_Discriminant_Check

   begin
      if Do_Access then
         T_Typ := Designated_Type (Typ);
      else
         T_Typ := Typ;
      end if;

      --  Nothing to do if discriminant checks are suppressed or else no code
      --  is to be generated

      if not Expander_Active
        or else Discriminant_Checks_Suppressed (T_Typ)
      then
         return;
      end if;

      --  No discriminant checks necessary for access when expression
      --  is statically Null. This is not only an optimization, this is
      --  fundamental because otherwise discriminant checks may be generated
      --  in init procs for types containing an access to a non-frozen yet
      --  record, causing a deadly forward reference.

      --  Also, if the expression is of an access type whose designated
      --  type is incomplete, then the access value must be null and
      --  we suppress the check.

      if Nkind (N) = N_Null then
         return;

      elsif Is_Access_Type (S_Typ) then
         S_Typ := Designated_Type (S_Typ);

         if Ekind (S_Typ) = E_Incomplete_Type then
            return;
         end if;
      end if;

      --  If an assignment target is present, then we need to generate
      --  the actual subtype if the target is a parameter or aliased
      --  object with an unconstrained nominal subtype.

      if Present (Lhs)
        and then (Present (Param_Entity (Lhs))
                   or else (not Is_Constrained (T_Typ)
                             and then Is_Aliased_View (Lhs)
                             and then not Is_Component_Initialization))
      then
         T_Typ := Get_Actual_Subtype (Lhs);
      end if;

      --  Nothing to do if the type is unconstrained (this is the case
      --  where the actual subtype in the RM sense of N is unconstrained
      --  and no check is required).

      if not Is_Constrained (T_Typ) then
         return;
      end if;

      --  Suppress checks if the subtypes are the same.
      --  the check must be preserved in an assignment to a formal, because
      --  the constraint is given by the actual.

      if Nkind (Original_Node (N)) /= N_Allocator
        and then (No (Lhs)
          or else not Is_Entity_Name (Lhs)
          or else (Ekind (Entity (Lhs)) /=  E_In_Out_Parameter
                    and then Ekind (Entity (Lhs)) /=  E_Out_Parameter))
      then
         if (Etype (N) = Typ
              or else (Do_Access and then Designated_Type (Typ) = S_Typ))
           and then not Is_Aliased_View (Lhs)
         then
            return;
         end if;

      --  We can also eliminate checks on allocators with a subtype mark
      --  that coincides with the context type. The context type may be a
      --  subtype without a constraint (common case, a generic actual).

      elsif Nkind (Original_Node (N)) = N_Allocator
        and then Is_Entity_Name (Expression (Original_Node (N)))
      then
         declare
            Alloc_Typ : Entity_Id := Entity (Expression (Original_Node (N)));

         begin
            if Alloc_Typ = T_Typ
              or else (Nkind (Parent (T_Typ)) = N_Subtype_Declaration
                        and then Is_Entity_Name (
                          Subtype_Indication (Parent (T_Typ)))
                        and then Alloc_Typ = Base_Type (T_Typ))

            then
               return;
            end if;
         end;
      end if;

      --  See if we have a case where the types are both constrained, and
      --  all the constraints are constants. In this case, we can do the
      --  check successfully at compile time.

      --  Note: at the current time, we skip this check for the case where
      --  either of the types is an access type, since there seem to be some
      --  troublesome cases, e.g. DEC test AI00397 (3611-004)

      if Is_Constrained (S_Typ)
        and then not Do_Access
        and then not Is_Access_Type (Etype (N))
      then
         declare
            DconT : Elmt_Id;
            Discr : Entity_Id;
            DconS : Elmt_Id;
            ItemS : Node_Id;
            ItemT : Node_Id;

         begin
            --  S_Typ may not have discriminants in the case where it is a
            --  private type completed by a default discriminated type. In
            --  that case, we need to get the constraints from the
            --  underlying_type. If the underlying type is unconstrained (i.e.
            --  has no default discriminants) no check is needed.

            if Has_Discriminants (S_Typ) then
               Discr := First_Discriminant (S_Typ);
               DconS := First_Elmt (Discriminant_Constraint (S_Typ));

            else
               Discr := First_Discriminant (Underlying_Type (S_Typ));
               DconS :=
                 First_Elmt
                   (Discriminant_Constraint (Underlying_Type (S_Typ)));

               if No (DconS) then
                  return;
               end if;
            end if;

            DconT  := First_Elmt (Discriminant_Constraint (T_Typ));

            while Present (Discr) loop
               ItemS := Node (DconS);
               ItemT := Node (DconT);

               exit when
                 not Is_OK_Static_Expression (ItemS)
                   or else
                 not Is_OK_Static_Expression (ItemT);

               if Expr_Value (ItemS) /= Expr_Value (ItemT) then
                  Apply_Compile_Time_Constraint_Error
                    (N, "incorrect value for discriminant&?", Ent => Discr);
                  return;
               end if;

               Next_Elmt (DconS);
               Next_Elmt (DconT);
               Next_Discriminant (Discr);
            end loop;

            if No (Discr) then
               return;
            end if;
         end;
      end if;

      --  Here we need a discriminant check. First build the expression
      --  for the comparisons of the discriminants:

      --    (n.disc1 /= typ.disc1) or else
      --    (n.disc2 /= typ.disc2) or else
      --     ...
      --    (n.discn /= typ.discn)

      Cond := Build_Discriminant_Checks (N, T_Typ);

      --  If Lhs is set and is a parameter, then the condition is
      --  guarded by: lhs'constrained and then (condition built above)

      if Present (Param_Entity (Lhs)) then
         Cond :=
           Make_And_Then (Loc,
             Left_Opnd =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Param_Entity (Lhs), Loc),
                 Attribute_Name => Name_Constrained),
             Right_Opnd => Cond);
      end if;

      if Do_Access then
         Cond := Guard_Access (Cond, Loc, N);
      end if;

      Insert_Action (N,
        Make_Raise_Constraint_Error (Loc, Condition => Cond));

   end Apply_Discriminant_Check;

   ------------------------
   -- Apply_Divide_Check --
   ------------------------

   procedure Apply_Divide_Check (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Etype (N);
      Left  : constant Node_Id    := Left_Opnd (N);
      Right : constant Node_Id    := Right_Opnd (N);

      LLB : Uint;
      Llo : Uint;
      Lhi : Uint;
      LOK : Boolean;
      Rlo : Uint;
      Rhi : Uint;
      ROK : Boolean;

   begin
      if Expander_Active
        and then Do_Overflow_Check (N)
        and then Software_Overflow_Checking
      then
         Determine_Range (Right, ROK, Rlo, Rhi);

         --  See if division by zero possible, and if so generate test

         if (not ROK) or else (Rlo <= 0 and then 0 <= Rhi) then
            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc,
                Condition =>
                  Make_Op_Eq (Loc,
                    Left_Opnd => Duplicate_Subexpr (Right),
                    Right_Opnd => Make_Integer_Literal (Loc, 0))));
         end if;

         --  Test for extremely annoying case of xxx'First divided by -1

         if Nkind (N) = N_Op_Divide
           and then Is_Signed_Integer_Type (Typ)
         then
            Determine_Range (Left, LOK, Llo, Lhi);
            LLB := Expr_Value (Type_Low_Bound (Base_Type (Typ)));

            if ((not ROK) or else (Rlo <= (-1) and then (-1) <= Rhi))
              and then
               ((not LOK) or else (Llo = LLB))
            then
               Insert_Action (N,
                 Make_Raise_Constraint_Error (Loc,
                   Condition =>
                     Make_And_Then (Loc,

                        Make_Op_Eq (Loc,
                          Left_Opnd  => Duplicate_Subexpr (Left),
                          Right_Opnd => Make_Integer_Literal (Loc, LLB)),

                        Make_Op_Eq (Loc,
                          Left_Opnd => Duplicate_Subexpr (Right),
                          Right_Opnd =>
                            Make_Integer_Literal (Loc, -1)))));
            end if;
         end if;
      end if;
   end Apply_Divide_Check;

   ------------------------
   -- Apply_Length_Check --
   ------------------------

   procedure Apply_Length_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty)
   is
   begin
      Apply_Selected_Length_Checks
        (Ck_Node, Target_Typ, Source_Typ, Do_Static => False);
   end Apply_Length_Check;

   -----------------------
   -- Apply_Range_Check --
   -----------------------

   procedure Apply_Range_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty)
   is
   begin
      Apply_Selected_Range_Checks
        (Ck_Node, Target_Typ, Source_Typ, Do_Static => False);
   end Apply_Range_Check;

   ------------------------------
   -- Apply_Scalar_Range_Check --
   ------------------------------

   --  Note that Apply_Scalar_Range_Check never turns the Do_Range_Check
   --  flag off if it is already set on.

   procedure Apply_Scalar_Range_Check
     (Expr       : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty;
      Fixed_Int  : Boolean   := False)
   is
      Parnt   : constant Node_Id := Parent (Expr);
      S_Typ   : Entity_Id;
      Arr     : Node_Id;
      Arr_Typ : Entity_Id;
      OK      : Boolean;

      Is_Subscr_Ref : Boolean;
      --  Set true if Expr is a subscript

      Is_Unconstrained_Subscr_Ref : Boolean;
      --  Set true if Expr is a subscript of an unconstrained array. In this
      --  case we do not attempt to do an analysis of the value against the
      --  range of the subscript, since we don't know the actual subtype.

      Int_Real : Boolean;
      --  Set to True if Expr should be regarded as a real value
      --  even though the type of Expr might be discrete.

      procedure Bad_Value;
      --  Procedure called if value is determined to be out of range

      procedure Bad_Value is
      begin
         Apply_Compile_Time_Constraint_Error
           (Expr, "value not in range of}?",
            Ent => Target_Typ,
            Typ => Target_Typ);
      end Bad_Value;

   begin
      if Inside_A_Generic then
         return;

      --  Return if check obviously not needed. Note that we do not check
      --  for the expander being inactive, since this routine does not
      --  insert any code, but it does generate useful warnings sometimes,
      --  which we would like even if we are in semantics only mode.

      elsif Target_Typ = Any_Type
        or else not Is_Scalar_Type (Target_Typ)
        or else Raises_Constraint_Error (Expr)
      then
         return;
      end if;

      --  Now, see if checks are suppressed

      Is_Subscr_Ref :=
        Is_List_Member (Expr) and then Nkind (Parnt) = N_Indexed_Component;

      if Is_Subscr_Ref then
         Arr := Prefix (Parnt);
         Arr_Typ := Get_Actual_Subtype_If_Available (Arr);
      end if;

      if not Do_Range_Check (Expr) then

         --  Subscript reference. Check for Index_Checks suppressed

         if Is_Subscr_Ref then

            --  Check array type and its base type

            if Index_Checks_Suppressed (Arr_Typ)
              or else Suppress_Index_Checks (Base_Type (Arr_Typ))
            then
               return;

            --  Check array itself if it is an entity name

            elsif Is_Entity_Name (Arr)
              and then Suppress_Index_Checks (Entity (Arr))
            then
               return;

            --  Check expression itself if it is an entity name

            elsif Is_Entity_Name (Expr)
              and then Suppress_Index_Checks (Entity (Expr))
            then
               return;
            end if;

         --  All other cases, check for Range_Checks suppressed

         else
            --  Check target type and its base type

            if Range_Checks_Suppressed (Target_Typ)
              or else Suppress_Range_Checks (Base_Type (Target_Typ))
            then
               return;

            --  Check expression itself if it is an entity name

            elsif Is_Entity_Name (Expr)
              and then Suppress_Range_Checks (Entity (Expr))
            then
               return;

            --  If Expr is part of an assignment statement, then check
            --  left side of assignment if it is an entity name.

            elsif Nkind (Parnt) = N_Assignment_Statement
              and then Is_Entity_Name (Name (Parnt))
              and then Suppress_Range_Checks (Entity (Name (Parnt)))
            then
               return;
            end if;
         end if;
      end if;

      --  Now see if we need a check

      if No (Source_Typ) then
         S_Typ := Etype (Expr);
      else
         S_Typ := Source_Typ;
      end if;

      if not Is_Scalar_Type (S_Typ) or else S_Typ = Any_Type then
         return;
      end if;

      Is_Unconstrained_Subscr_Ref :=
        Is_Subscr_Ref and then not Is_Constrained (Arr_Typ);

      --  Always do a range check if the source type includes infinities
      --  and the target type does not include infinities.

      if Is_Floating_Point_Type (S_Typ)
        and then Has_Infinities (S_Typ)
        and then not Has_Infinities (Target_Typ)
      then
         Enable_Range_Check (Expr);
         return;
      end if;

      --  Return if we know expression is definitely in the range of
      --  the target type as determined by Determine_Range. Right now
      --  we only do this for discrete types, and not fixed-point or
      --  floating-point types.

      --  The additional less-precise tests below catch these cases.

      --  Note: skip this if we are given a source_typ, since the point
      --  of supplying a Source_Typ is to stop us looking at the expression.
      --  could sharpen this test to be out parameters only ???

      if Is_Discrete_Type (Target_Typ)
        and then Is_Discrete_Type (Etype (Expr))
        and then not Is_Unconstrained_Subscr_Ref
        and then No (Source_Typ)
      then
         declare
            Tlo : constant Node_Id := Type_Low_Bound  (Target_Typ);
            Thi : constant Node_Id := Type_High_Bound (Target_Typ);
            Lo  : Uint;
            Hi  : Uint;

         begin
            if Compile_Time_Known_Value (Tlo)
              and then Compile_Time_Known_Value (Thi)
            then
               Determine_Range (Expr, OK, Lo, Hi);

               if OK then
                  declare
                     Lov : constant Uint := Expr_Value (Tlo);
                     Hiv : constant Uint := Expr_Value (Thi);

                  begin
                     if Lo >= Lov and then Hi <= Hiv then
                        return;

                     elsif Lov > Hi or else Hiv < Lo then
                        Bad_Value;
                        return;
                     end if;
                  end;
               end if;
            end if;
         end;
      end if;

      Int_Real :=
        Is_Floating_Point_Type (S_Typ)
          or else (Is_Fixed_Point_Type (S_Typ) and then not Fixed_Int);

      --  Check if we can determine at compile time whether Expr is in the
      --  range of the target type. Note that if S_Typ is within the
      --  bounds of Target_Typ then this must be the case.

      if not Is_Unconstrained_Subscr_Ref
        and then
          (In_Subrange_Of (S_Typ, Target_Typ, Fixed_Int)
             or else
           Is_In_Range (Expr, Target_Typ, Fixed_Int, Int_Real))
      then
         return;

      elsif Is_Out_Of_Range (Expr, Target_Typ, Fixed_Int, Int_Real) then
         Bad_Value;
         return;

      --  Do not set range checks if they are killed

      elsif Nkind (Expr) = N_Unchecked_Type_Conversion
        and then Kill_Range_Check (Expr)
      then
         return;

      --  ??? We only need a runtime check if the target type is constrained
      --  (the predefined type Float is not for instance).
      --  so the following should really be
      --
      --    elsif Is_Constrained (Target_Typ) then
      --
      --  but it isn't because certain types do not have the Is_Constrained
      --  flag properly set (see 1503-003).

      else
         Enable_Range_Check (Expr);
         return;
      end if;

   end Apply_Scalar_Range_Check;

   ----------------------------------
   -- Apply_Selected_Length_Checks --
   ----------------------------------

   procedure Apply_Selected_Length_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Do_Static  : Boolean)
   is
      Cond     : Node_Id;
      R_Result : Check_Result;
      R_Cno    : Node_Id;

      Loc         : constant Source_Ptr := Sloc (Ck_Node);
      Checks_On   : constant Boolean :=
                      (not Index_Checks_Suppressed (Target_Typ))
                        or else
                      (not Length_Checks_Suppressed (Target_Typ));

   begin
      if not Expander_Active or else not Checks_On then
         return;
      end if;

      R_Result :=
        Selected_Length_Checks (Ck_Node, Target_Typ, Source_Typ, Empty);

      for J in 1 .. 2 loop

         R_Cno := R_Result (J);
         exit when No (R_Cno);

         --  A length check may mention an Itype which is attached to a
         --  subsequent node. At the top level in a package this can cause
         --  an order-of-elaboration problem, so we make sure that the itype
         --  is referenced now.

         if Ekind (Current_Scope) = E_Package
           and then Is_Compilation_Unit (Current_Scope)
         then
            Ensure_Defined (Target_Typ, Ck_Node);

            if Present (Source_Typ) then
               Ensure_Defined (Source_Typ, Ck_Node);
            end if;
         end if;

         --  If the item is a conditional raise of constraint error,
         --  then have a look at what check is being performed and
         --  ???

         if Nkind (R_Cno) = N_Raise_Constraint_Error
           and then Present (Condition (R_Cno))
         then
            Cond := Condition (R_Cno);

            if not Has_Dynamic_Length_Check (Ck_Node) then
               Insert_Action (Ck_Node, R_Cno);

               if not Do_Static then
                  Set_Has_Dynamic_Length_Check (Ck_Node);
               end if;

            end if;

            --  Output a warning if the condition is known to be True

            if Is_Entity_Name (Cond)
              and then Entity (Cond) = Standard_True
            then
               Apply_Compile_Time_Constraint_Error
                 (Ck_Node, "wrong length for array of}?",
                  Ent => Target_Typ,
                  Typ => Target_Typ);

            --  If we were only doing a static check, or if checks are not
            --  on, then we want to delete the check, since it is not needed.
            --  We do this by replacing the if statement by a null statement

            elsif Do_Static or else not Checks_On then
               Rewrite (R_Cno, Make_Null_Statement (Loc));
            end if;

         else
            Install_Static_Check (R_Cno, Loc);
         end if;

      end loop;

   end Apply_Selected_Length_Checks;

   ---------------------------------
   -- Apply_Selected_Range_Checks --
   ---------------------------------

   procedure Apply_Selected_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Do_Static  : Boolean)
   is
      Cond     : Node_Id;
      R_Result : Check_Result;
      R_Cno    : Node_Id;

      Loc       : constant Source_Ptr := Sloc (Ck_Node);
      Checks_On : constant Boolean :=
                    (not Index_Checks_Suppressed (Target_Typ))
                      or else
                    (not Range_Checks_Suppressed (Target_Typ));

   begin
      if not Expander_Active or else not Checks_On then
         return;
      end if;

      R_Result :=
        Selected_Range_Checks (Ck_Node, Target_Typ, Source_Typ, Empty);

      for J in 1 .. 2 loop

         R_Cno := R_Result (J);
         exit when No (R_Cno);

         --  If the item is a conditional raise of constraint error,
         --  then have a look at what check is being performed and
         --  ???

         if Nkind (R_Cno) = N_Raise_Constraint_Error
           and then Present (Condition (R_Cno))
         then
            Cond := Condition (R_Cno);

            if not Has_Dynamic_Range_Check (Ck_Node) then
               Insert_Action (Ck_Node, R_Cno);

               if not Do_Static then
                  Set_Has_Dynamic_Range_Check (Ck_Node);
               end if;
            end if;

            --  Output a warning if the condition is known to be True

            if Is_Entity_Name (Cond)
              and then Entity (Cond) = Standard_True
            then
               --  Since an N_Range is technically not an expression, we
               --  have to set one of the bounds to C_E and then just flag
               --  the N_Range. The warning message will point to the
               --  lower bound and complain about a range, which seems OK.

               if Nkind (Ck_Node) = N_Range then
                  Apply_Compile_Time_Constraint_Error
                    (Low_Bound (Ck_Node), "static range out of bounds of}?",
                     Ent => Target_Typ,
                     Typ => Target_Typ);

                  Set_Raises_Constraint_Error (Ck_Node);

               else
                  Apply_Compile_Time_Constraint_Error
                    (Ck_Node, "static value out of range of}?",
                     Ent => Target_Typ,
                     Typ => Target_Typ);
               end if;

            --  If we were only doing a static check, or if checks are not
            --  on, then we want to delete the check, since it is not needed.
            --  We do this by replacing the if statement by a null statement

            elsif Do_Static or else not Checks_On then
               Rewrite (R_Cno, Make_Null_Statement (Loc));
            end if;

         else
            Install_Static_Check (R_Cno, Loc);
         end if;

      end loop;

   end Apply_Selected_Range_Checks;

   -------------------------------
   -- Apply_Static_Length_Check --
   -------------------------------

   procedure Apply_Static_Length_Check
     (Expr       : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty)
   is
   begin
      Apply_Selected_Length_Checks
        (Expr, Target_Typ, Source_Typ, Do_Static => True);
   end Apply_Static_Length_Check;

   -------------------------------------
   -- Apply_Subscript_Validity_Checks --
   -------------------------------------

   procedure Apply_Subscript_Validity_Checks (Expr : Node_Id) is
      Sub : Node_Id;

   begin
      pragma Assert (Nkind (Expr) = N_Indexed_Component);

      --  Loop through subscripts

      Sub := First (Expressions (Expr));
      while Present (Sub) loop

         --  Check one subscript. Note that we do not worry about
         --  enumeration type with holes, since we will convert the
         --  value to a Pos value for the subscript, and that convert
         --  will do the necessary validity check.

         Ensure_Valid (Sub, Holes_OK => True);

         --  Move to next subscript

         Sub := Next (Sub);
      end loop;
   end Apply_Subscript_Validity_Checks;

   ----------------------------------
   -- Apply_Type_Conversion_Checks --
   ----------------------------------

   procedure Apply_Type_Conversion_Checks (N : Node_Id) is
      Target_Type : constant Entity_Id := Etype (N);
      Target_Base : constant Entity_Id := Base_Type (Target_Type);

      Expr      : constant Node_Id   := Expression (N);
      Expr_Type : constant Entity_Id := Etype (Expr);

   begin
      if Inside_A_Generic then
         return;

      --  Skip these checks if errors detected, there are some nasty
      --  situations of incomplete trees that blow things up.

      elsif Errors_Detected > 0 then
         return;

      --  Scalar type conversions of the form Target_Type (Expr) require
      --  two checks:
      --
      --    - First there is an overflow check to insure that Expr is
      --      in the base type of Target_Typ (4.6 (28)),
      --
      --    - After we know Expr fits into the base type, we must perform a
      --      range check to ensure that Expr meets the constraints of the
      --      Target_Type.

      elsif Is_Scalar_Type (Target_Type) then
         declare
            Conv_OK  : constant Boolean := Conversion_OK (N);
            --  If the Conversion_OK flag on the type conversion is set
            --  and no floating point type is involved in the type conversion
            --  then fixed point values must be read as integral values.

         begin
            --  Overflow check.

            if not Overflow_Checks_Suppressed (Target_Base)
              and then not In_Subrange_Of (Expr_Type, Target_Base, Conv_OK)
            then
               Set_Do_Overflow_Check (N);
            end if;

            if not Range_Checks_Suppressed (Target_Type)
              and then not Range_Checks_Suppressed (Expr_Type)
            then
               Apply_Scalar_Range_Check
                 (Expr, Target_Type, Fixed_Int => Conv_OK);
            end if;
         end;

      elsif Comes_From_Source (N)
        and then Is_Record_Type (Target_Type)
        and then Is_Derived_Type (Target_Type)
        and then not Is_Tagged_Type (Target_Type)
        and then not Is_Constrained (Target_Type)
        and then Present (Girder_Constraint (Target_Type))
      then
         --  A unconstrained derived type may have inherited discriminants.
         --  Build an actual discriminant constraint list using the girder
         --  constraint, to verify that the expression of the parent type
         --  satisfies the constraints imposed by the (unconstrained!)
         --  derived type. This applies to value conversions, not to view
         --  conversions of tagged types.

         declare
            Loc             : constant Source_Ptr := Sloc (N);
            Cond            : Node_Id;
            Constraint      : Elmt_Id;
            Discr_Value     : Node_Id;
            Discr           : Entity_Id;
            New_Constraints : Elist_Id := New_Elmt_List;
            Old_Constraints : Elist_Id := Discriminant_Constraint (Expr_Type);

         begin
            Constraint := First_Elmt (Girder_Constraint (Target_Type));

            while Present (Constraint) loop
               Discr_Value := Node (Constraint);

               if Is_Entity_Name (Discr_Value)
                 and then Ekind (Entity (Discr_Value)) = E_Discriminant
               then
                  Discr := Corresponding_Discriminant (Entity (Discr_Value));

                  if Present (Discr)
                    and then Scope (Discr) = Base_Type (Expr_Type)
                  then
                     --  Parent is constrained by new discriminant. Obtain
                     --  Value of original discriminant in expression. If
                     --  the new discriminant has been used to constrain more
                     --  than one of the girder ones, this will provide the
                     --  required consistency check.

                     Append_Elmt (
                        Make_Selected_Component (Loc,
                          Prefix =>
                            Duplicate_Subexpr (Expr, Name_Req => True),
                          Selector_Name =>
                            Make_Identifier (Loc, Chars (Discr))),
                                New_Constraints);

                  else
                     --  Discriminant of more remote ancestor ???

                     return;
                  end if;

               --  Derived type definition has an explicit value for
               --  this girder discriminant.

               else
                  Append_Elmt
                    (Duplicate_Subexpr (Discr_Value), New_Constraints);
               end if;

               Next_Elmt (Constraint);
            end loop;

            --  Use the unconstrained expression type to retrieve the
            --  discriminants of the parent, and apply momentarily the
            --  discriminant constraint synthesized above.

            Set_Discriminant_Constraint (Expr_Type, New_Constraints);
            Cond := Build_Discriminant_Checks (Expr, Expr_Type);
            Set_Discriminant_Constraint (Expr_Type, Old_Constraints);

            Insert_Action (N,
              Make_Raise_Constraint_Error (Loc, Condition => Cond));
         end;

      --  should there be other checks here for array types ???

      else
         null;
      end if;

   end Apply_Type_Conversion_Checks;

   ----------------------------------------------
   -- Apply_Universal_Integer_Attribute_Checks --
   ----------------------------------------------

   procedure Apply_Universal_Integer_Attribute_Checks (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);

   begin
      if Inside_A_Generic then
         return;

      --  Nothing to do if checks are suppressed

      elsif Range_Checks_Suppressed (Typ)
        and then Overflow_Checks_Suppressed (Typ)
      then
         return;

      --  Nothing to do if the attribute does not come from source. The
      --  internal attributes we generate of this type do not need checks,
      --  and furthermore the attempt to check them causes some circular
      --  elaboration orders when dealing with packed types.

      elsif not Comes_From_Source (N) then
         return;

      --  Otherwise, replace the attribute node with a type conversion
      --  node whose expression is the attribute, retyped to universal
      --  integer, and whose subtype mark is the target type. The call
      --  to analyze this conversion will set range and overflow checks
      --  as required for proper detection of an out of range value.

      else
         Set_Etype    (N, Universal_Integer);
         Set_Analyzed (N, True);

         Rewrite (N,
           Make_Type_Conversion (Loc,
             Subtype_Mark => New_Occurrence_Of (Typ, Loc),
             Expression   => Relocate_Node (N)));

         Analyze_And_Resolve (N, Typ);
         return;
      end if;

   end Apply_Universal_Integer_Attribute_Checks;

   -------------------------------
   -- Build_Discriminant_Checks --
   -------------------------------

   function Build_Discriminant_Checks
     (N     : Node_Id;
      T_Typ : Entity_Id)
      return Node_Id
   is
      Loc      : constant Source_Ptr := Sloc (N);
      Cond     : Node_Id;
      Disc     : Elmt_Id;
      Disc_Ent : Entity_Id;
      Dval     : Node_Id;

   begin
      Cond := Empty;
      Disc := First_Elmt (Discriminant_Constraint (T_Typ));

      --  For a fully private type, use the discriminants of the parent
      --  type.

      if Is_Private_Type (T_Typ)
        and then No (Full_View (T_Typ))
      then
         Disc_Ent := First_Discriminant (Etype (Base_Type (T_Typ)));
      else
         Disc_Ent := First_Discriminant (T_Typ);
      end if;

      while Present (Disc) loop

         Dval := Node (Disc);

         if Nkind (Dval) = N_Identifier
           and then Ekind (Entity (Dval)) = E_Discriminant
         then
            Dval := New_Occurrence_Of (Discriminal (Entity (Dval)), Loc);
         else
            Dval := Duplicate_Subexpr (Dval);
         end if;

         Evolve_Or_Else (Cond,
           Make_Op_Ne (Loc,
             Left_Opnd =>
               Make_Selected_Component (Loc,
                 Prefix =>
                   Duplicate_Subexpr (N, Name_Req => True),
                 Selector_Name =>
                   Make_Identifier (Loc, Chars (Disc_Ent))),
             Right_Opnd => Dval));

         Next_Elmt (Disc);
         Next_Discriminant (Disc_Ent);
      end loop;

      return Cond;
   end Build_Discriminant_Checks;

   -----------------------------------
   -- Check_Valid_Lvalue_Subscripts --
   -----------------------------------

   procedure Check_Valid_Lvalue_Subscripts (Expr : Node_Id) is
   begin
      --  Skip this if range checks are suppressed

      if Range_Checks_Suppressed (Etype (Expr)) then
         return;

      --  Only do this check for expressions that come from source. We
      --  assume that expander generated assignments explicitly include
      --  any necessary checks. Note that this is not just an optimization,
      --  it avoids infinite recursions!

      elsif not Comes_From_Source (Expr) then
         return;

      --  For a selected component, check the prefix

      elsif Nkind (Expr) = N_Selected_Component then
         Check_Valid_Lvalue_Subscripts (Prefix (Expr));
         return;

      --  Case of indexed component

      elsif Nkind (Expr) = N_Indexed_Component then
         Apply_Subscript_Validity_Checks (Expr);

         --  Prefix may itself be or contain an indexed component, and
         --  these subscripts need checking as well

         Check_Valid_Lvalue_Subscripts (Prefix (Expr));
      end if;
   end Check_Valid_Lvalue_Subscripts;

   ---------------------
   -- Determine_Range --
   ---------------------

   Cache_Size : constant := 2 ** 6;
   type Cache_Index is range 0 .. Cache_Size - 1;
   --  Determine size of below cache (power of 2 is more efficient!)

   Determine_Range_Cache_N  : array (Cache_Index) of Node_Id;
   Determine_Range_Cache_Lo : array (Cache_Index) of Uint;
   Determine_Range_Cache_Hi : array (Cache_Index) of Uint;
   --  The above arrays are used to implement a small direct cache
   --  for Determine_Range calls. Because of the way Determine_Range
   --  recursively traces subexpressions, and because overflow checking
   --  calls the routine on the way up the tree, a quadratic behavior
   --  can otherwise be encountered in large expressions. The cache
   --  entry for node N is stored in the (N mod Cache_Size) entry, and
   --  can be validated by checking the actual node value stored there.

   procedure Determine_Range
     (N  : Node_Id;
      OK : out Boolean;
      Lo : out Uint;
      Hi : out Uint)
   is
      Typ  : constant Entity_Id := Etype (N);
      Mtyp : constant Boolean   := Is_Modular_Integer_Type (Typ);

      Lo_Left  : Uint;
      Lo_Right : Uint;
      Hi_Left  : Uint;
      Hi_Right : Uint;
      Bound    : Node_Id;
      Hbound   : Uint;
      Lor      : Uint;
      Hir      : Uint;
      OK1      : Boolean;
      Cindex   : Cache_Index;

      function OK_Operands return Boolean;
      --  Used for binary operators. Determines the ranges of the left and
      --  right operands, and if they are both OK, returns True, and puts
      --  the results in Lo_Right, Hi_Right, Lo_Left, Hi_Left

      function OK_Operands return Boolean is
      begin
         Determine_Range (Left_Opnd  (N), OK1, Lo_Left,  Hi_Left);

         if not OK1 then
            return False;
         end if;

         Determine_Range (Right_Opnd (N), OK1, Lo_Right, Hi_Right);
         return OK1;
      end OK_Operands;

   --  Start of processing for Determine_Range

   begin
      if not Is_Discrete_Type (Typ)
        or else Error_Posted (N)
      then
         OK := False;
         return;
      end if;

      --  For all other cases, we can determine the range

      OK := True;

      --  If value is compile time known, then the possible range is the
      --  one value that we know this expression definitely has!

      if Compile_Time_Known_Value (N) then
         Lo := Expr_Value (N);
         Hi := Lo;
         return;
      end if;

      --  Return if already in the cache

      Cindex := Cache_Index (N mod Cache_Size);

      if Determine_Range_Cache_N (Cindex) = N then
         Lo := Determine_Range_Cache_Lo (Cindex);
         Hi := Determine_Range_Cache_Hi (Cindex);
         return;
      end if;

      --  Otherwise, start by finding the bounds of the type of the
      --  expression, the value cannot be outside this range (if it
      --  is, then we have an overflow situation, which is a separate
      --  check, we are talking here only about the expression value).

      --  We use the actual bound unless it is dynamic, in which case
      --  use the corresponding base type bound (which is always static).

      Bound := Type_Low_Bound (Typ);

      if Compile_Time_Known_Value (Bound) then
         Lo := Expr_Value (Bound);
      else
         Lo := Expr_Value (Type_Low_Bound (Base_Type (Typ)));
      end if;

      Bound := Type_High_Bound (Typ);
      Hbound := Expr_Value (Type_High_Bound (Base_Type (Typ)));

      if Compile_Time_Known_Value (Bound) then
         Hi := Expr_Value (Bound);
      else
         Hi := Hbound;
      end if;

      --  We may be able to refine this value in certain situations. If
      --  refinement is possible, then Lor and Hir are set to possibly
      --  tighter bounds, and OK1 is set to True.

      case Nkind (N) is

         --  For unary plus, result is limited by range of operand

         when N_Op_Plus =>
            Determine_Range (Right_Opnd (N), OK1, Lor, Hir);

         --  For unary minus, determine range of operand, and negate it

         when N_Op_Minus =>
            Determine_Range (Right_Opnd (N), OK1, Lo_Right, Hi_Right);

            if OK1 then
               Lor := -Hi_Right;
               Hir := -Lo_Right;
            end if;

         --  For binary addition, get range of each operand and do the
         --  addition to get the result range.

         when N_Op_Add =>
            if OK_Operands then
               Lor := Lo_Left + Lo_Right;
               Hir := Hi_Left + Hi_Right;
            end if;

         --  Division is tricky. The only case we consider is where the
         --  right operand is a positive constant, and in this case we
         --  simply divide the bounds of the left operand

         when N_Op_Divide =>
            if OK_Operands then
               if Lo_Right = Hi_Right
                 and then Lo_Right > 0
               then
                  Lor := Lo_Left / Lo_Right;
                  Hir := Hi_Left / Lo_Right;

               else
                  OK1 := False;
               end if;
            end if;

         --  For binary subtraction, get range of each operand and do
         --  the worst case subtraction to get the result range.

         when N_Op_Subtract =>
            if OK_Operands then
               Lor := Lo_Left - Hi_Right;
               Hir := Hi_Left - Lo_Right;
            end if;

         --  For MOD, if right operand is a positive constant, then
         --  result must be in the allowable range of mod results.

         when N_Op_Mod =>
            if OK_Operands then
               if Lo_Right = Hi_Right then
                  if Lo_Right > 0 then
                     Lor := Uint_0;
                     Hir := Lo_Right - 1;

                  elsif Lo_Right < 0 then
                     Lor := Lo_Right + 1;
                     Hir := Uint_0;
                  end if;

               else
                  OK1 := False;
               end if;
            end if;

         --  For REM, if right operand is a positive constant, then
         --  result must be in the allowable range of mod results.

         when N_Op_Rem =>
            if OK_Operands then
               if Lo_Right = Hi_Right then
                  declare
                     Dval : constant Uint := (abs Lo_Right) - 1;

                  begin
                     --  The sign of the result depends on the sign of the
                     --  dividend (but not on the sign of the divisor, hence
                     --  the abs operation above).

                     if Lo_Left < 0 then
                        Lor := -Dval;
                     else
                        Lor := Uint_0;
                     end if;

                     if Hi_Left < 0 then
                        Hir := Uint_0;
                     else
                        Hir := Dval;
                     end if;
                  end;

               else
                  OK1 := False;
               end if;
            end if;

         --  Attribute reference cases

         when N_Attribute_Reference =>
            case Attribute_Name (N) is

               --  For Pos/Val attributes, we can refine the range using the
               --  possible range of values of the attribute expression

               when Name_Pos | Name_Val =>
                  Determine_Range (First (Expressions (N)), OK1, Lor, Hir);

               --  For Length attribute, use the bounds of the corresponding
               --  index type to refine the range.

               when Name_Length =>
                  declare
                     Atyp : Entity_Id := Etype (Prefix (N));
                     Inum : Nat;
                     Indx : Node_Id;

                     LL, LU : Uint;
                     UL, UU : Uint;

                  begin
                     if Is_Access_Type (Atyp) then
                        Atyp := Designated_Type (Atyp);
                     end if;

                     if No (Expressions (N)) then
                        Inum := 1;
                     else
                        Inum :=
                          UI_To_Int (Expr_Value (First (Expressions (N))));
                     end if;

                     Indx := First_Index (Atyp);
                     for J in 2 .. Inum loop
                        Indx := Next_Index (Indx);
                     end loop;

                     Determine_Range
                       (Type_Low_Bound (Etype (Indx)), OK1, LL, LU);

                     if OK1 then
                        Determine_Range
                          (Type_High_Bound (Etype (Indx)), OK1, UL, UU);

                        if OK1 then

                           --  The maximum value for Length is the biggest
                           --  possible gap between the values of the bounds.
                           --  But of course, this value cannot be negative.

                           Hir := UI_Max (Uint_0, UU - LL);

                           --  For constrained arrays, the minimum value for
                           --  Length is taken from the actual value of the
                           --  bounds, since the index will be exactly of
                           --  this subtype.

                           if Is_Constrained (Atyp) then
                              Lor := UI_Max (Uint_0, UL - LU);

                           --  For an unconstrained array, the minimum value
                           --  for length is always zero.

                           else
                              Lor := Uint_0;
                           end if;
                        end if;
                     end if;
                  end;

               --  No special handling for other attributes
               --  Probably more opportunities exist here ???

               when others =>
                  OK1 := False;

            end case;

         --  For type conversion from one discrete type to another, we
         --  can refine the range using the converted value.

         when N_Type_Conversion =>
            Determine_Range (Expression (N), OK1, Lor, Hir);

         --  Nothing special to do for all other expression kinds

         when others =>
            OK1 := False;
      end case;

      --  At this stage, if OK1 is true, then we know that the actual
      --  result of the computed expression is in the range Lor .. Hir.
      --  We can use this to restrict the possible range of results.

      if OK1 then

         --  If the refined value of the low bound is greater than the
         --  type high bound, then reset it to the more restrictive
         --  value. However, we do NOT do this for the case of a modular
         --  type where the possible upper bound on the value is above the
         --  base type high bound, because that means the result could wrap.

         if Lor > Lo
           and then not (Mtyp and then Hir > Hbound)
         then
            Lo := Lor;
         end if;

         --  Similarly, if the refined value of the high bound is less
         --  than the value so far, then reset it to the more restrictive
         --  value. Again, we do not do this if the refined low bound is
         --  negative for a modular type, since this would wrap.

         if Hir < Hi
           and then not (Mtyp and then Lor < Uint_0)
         then
            Hi := Hir;
         end if;
      end if;

      --  Set cache entry for future call and we are all done

      Determine_Range_Cache_N  (Cindex) := N;
      Determine_Range_Cache_Lo (Cindex) := Lo;
      Determine_Range_Cache_Hi (Cindex) := Hi;
      return;

   --  If any exception occurs, it means that we have some bug in the compiler
   --  possibly triggered by a previous error, or by some unforseen peculiar
   --  occurrence. However, this is only an optimization attempt, so there is
   --  really no point in crashing the compiler. Instead we just decide, too
   --  bad, we can't figure out a range in this case after all.

   exception
      when others =>

         --  Debug flag K disables this behavior (useful for debugging)

         if Debug_Flag_K then
            raise;
         else
            OK := False;
            return;
         end if;

   end Determine_Range;

   ------------------------------------
   -- Discriminant_Checks_Suppressed --
   ------------------------------------

   function Discriminant_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Discriminant_Checks
        or else (Present (E) and then Suppress_Discriminant_Checks (E));
   end Discriminant_Checks_Suppressed;

   --------------------------------
   -- Division_Checks_Suppressed --
   --------------------------------

   function Division_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Division_Checks
        or else (Present (E) and then Suppress_Division_Checks (E));
   end Division_Checks_Suppressed;

   -----------------------------------
   -- Elaboration_Checks_Suppressed --
   -----------------------------------

   function Elaboration_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Elaboration_Checks
        or else (Present (E) and then Suppress_Elaboration_Checks (E));
   end Elaboration_Checks_Suppressed;

   ------------------------
   -- Enable_Range_Check --
   ------------------------

   procedure Enable_Range_Check (N : Node_Id) is
   begin
      if Nkind (N) = N_Unchecked_Type_Conversion
        and then Kill_Range_Check (N)
      then
         return;
      else
         Set_Do_Range_Check (N, True);
      end if;
   end Enable_Range_Check;

   ------------------
   -- Ensure_Valid --
   ------------------

   procedure Ensure_Valid (Expr : Node_Id; Holes_OK : Boolean := False) is
      Typ : constant Entity_Id  := Etype (Expr);

   begin
      --  Ignore call if we are not doing any validity checking

      if Validity_Checking = None then
         return;

      --  No check required if expression is from the expander, we assume
      --  the expander will generate whatever checks are needed. Note that
      --  this is not just an optimization, it avoids infinite recursions!

      elsif not Comes_From_Source (Expr) then
         return;

      --  No check required if expression is known to have valid value

      elsif Expr_Known_Valid (Expr) then
         return;

      --  No check required if checks off

      elsif Range_Checks_Suppressed (Typ) then
         return;

      --  Validity check required, check special case of enumeration with
      --  holes, where we must worry about the holes.

      elsif Is_Enumeration_Type (Typ)
        and then Has_Non_Standard_Rep (Typ)
      then
         --  Just ignore call is Holes_OK indicates that caller does
         --  not require checking for this particular case.

         if Holes_OK then
            return;

         --  Otherwise we insert a validity check (Expr'Valid) since
         --  a simple range check will clearly not suffice in this case.

         else
            Insert_Valid_Check (Expr);
         end if;

      --  We also insert a real valid check if we are checking an object
      --  whose size is greater than the object_size value of the type.

      elsif Is_Entity_Name (Expr)
        and then Is_Object (Entity (Expr))
        and then Esize (Entity (Expr)) > Esize (Typ)
      then
         Insert_Valid_Check (Expr);

      --  For all other types, the validity check is simply a range check

      else
         Set_Do_Range_Check (Expr, True);
      end if;
   end Ensure_Valid;

   ----------------------
   -- Expr_Known_Valid --
   ----------------------

   function Expr_Known_Valid (Expr : Node_Id) return Boolean is
      Typ : constant Entity_Id := Etype (Expr);

   begin
      --  We consider any non-discrete type value to be valid, because
      --  the issues of erroneous behavior from non-valid values do not
      --  arise for any other kind of type.

      if not Is_Discrete_Type (Typ) then
         return True;

      --  If the expression is the value of an object that is known to
      --  be valid, then clearly the expression value itself is valid.

      elsif Is_Entity_Name (Expr)
        and then Is_Known_Valid (Entity (Expr))
      then
         return True;

      --  If the type is one for which all values are known valid, then
      --  we are sure that the value is valid except in the slightly odd
      --  case where the expression is a reference to a variable whose size
      --  has been explicitly set to a value greater than the object size.

      elsif Is_Known_Valid (Typ) then
         if Is_Entity_Name (Expr)
           and then Ekind (Entity (Expr)) = E_Variable
           and then Esize (Entity (Expr)) > Esize (Typ)
         then
            return False;
         else
            return True;
         end if;

      --  Integer and character literals always have valid values, where
      --  appropriate these will be range checked in any case.

      elsif Nkind (Expr) = N_Integer_Literal
              or else
            Nkind (Expr) = N_Character_Literal
      then
         return True;

      --  If we have a type conversion or a qualification of a known valid
      --  value, then the result will always be valid.

      elsif Nkind (Expr) = N_Type_Conversion
              or else
            Nkind (Expr) = N_Qualified_Expression
      then
         return Expr_Known_Valid (Expression (Expr));

      --  The result of any function call or operator is always considered
      --  valid, since we assume the necessary checks are done by the call.

      elsif Nkind (Expr) in N_Binary_Op
              or else
            Nkind (Expr) in N_Unary_Op
              or else
            Nkind (Expr) = N_Function_Call
      then
         return True;

      --  For all other cases, we do not know the expression is valid

      else
         return False;
      end if;
   end Expr_Known_Valid;

   ---------------------
   -- Get_Discriminal --
   ---------------------

   function Get_Discriminal (E : Entity_Id; Bound : Node_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (E);
      D   : Entity_Id;
      Sc  : Entity_Id;

   begin
      --  The entity E is the type of a private component of the protected
      --  type, or the type of a renaming of that component within a protected
      --  operation of that type.

      Sc := Scope (E);

      if Ekind (Sc) /= E_Protected_Type then
         Sc := Scope (Sc);

         if Ekind (Sc) /= E_Protected_Type then
            return Bound;
         end if;
      end if;

      D := First_Discriminant (Sc);

      while Present (D)
        and then Chars (D) /= Chars (Bound)
      loop
         Next_Discriminant (D);
      end loop;

      return New_Occurrence_Of (Discriminal (D), Loc);
   end Get_Discriminal;

   ------------------
   -- Guard_Access --
   ------------------

   function Guard_Access
     (Cond    : Node_Id;
      Loc     : Source_Ptr;
      Ck_Node : Node_Id)
      return    Node_Id
   is
   begin
      if Nkind (Cond) = N_Or_Else then
         Set_Paren_Count (Cond, 1);
      end if;

      if Nkind (Ck_Node) = N_Allocator then
         return Cond;
      else
         return
           Make_And_Then (Loc,
             Left_Opnd =>
               Make_Op_Ne (Loc,
                 Left_Opnd  => Duplicate_Subexpr (Ck_Node),
                 Right_Opnd => Make_Null (Loc)),
             Right_Opnd => Cond);
      end if;
   end Guard_Access;

   -----------------------------
   -- Index_Checks_Suppressed --
   -----------------------------

   function Index_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Index_Checks
        or else (Present (E) and then Suppress_Index_Checks (E));
   end Index_Checks_Suppressed;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      for J in Determine_Range_Cache_N'Range loop
         Determine_Range_Cache_N (J) := Empty;
      end loop;
   end Initialize;

   -------------------------
   -- Insert_Range_Checks --
   -------------------------

   procedure Insert_Range_Checks
     (Checks       : Check_Result;
      Node         : Node_Id;
      Suppress_Typ : Entity_Id;
      Static_Sloc  : Source_Ptr := No_Location;
      Flag_Node    : Node_Id    := Empty;
      Do_Before    : Boolean    := False)
   is
      Internal_Flag_Node   : Node_Id    := Flag_Node;
      Internal_Static_Sloc : Source_Ptr := Static_Sloc;

      Check_Node : Node_Id;
      Checks_On  : constant Boolean :=
                     (not Index_Checks_Suppressed (Suppress_Typ))
                       or else
                     (not Range_Checks_Suppressed (Suppress_Typ));

   begin
      --  For now we just return if Checks_On is false, however this should
      --  be enhanced to check for an always True value in the condition
      --  and to generate a compilation warning???

      if not Expander_Active or else not Checks_On then
         return;
      end if;

      if Static_Sloc = No_Location then
         Internal_Static_Sloc := Sloc (Node);
      end if;

      if No (Flag_Node) then
         Internal_Flag_Node := Node;
      end if;

      for J in 1 .. 2 loop
         exit when No (Checks (J));

         if Nkind (Checks (J)) = N_Raise_Constraint_Error
           and then Present (Condition (Checks (J)))
         then
            if not Has_Dynamic_Range_Check (Internal_Flag_Node) then
               Check_Node := Checks (J);
               Mark_Rewrite_Insertion (Check_Node);

               if Do_Before then
                  Insert_Before_And_Analyze (Node, Check_Node);
               else
                  Insert_After_And_Analyze (Node, Check_Node);
               end if;

               Set_Has_Dynamic_Range_Check (Internal_Flag_Node);
            end if;

         else
            Check_Node :=
              Make_Raise_Constraint_Error (Internal_Static_Sloc);
            Mark_Rewrite_Insertion (Check_Node);

            if Do_Before then
               Insert_Before_And_Analyze (Node, Check_Node);
            else
               Insert_After_And_Analyze (Node, Check_Node);
            end if;
         end if;
      end loop;
   end Insert_Range_Checks;

   ------------------------
   -- Insert_Valid_Check --
   ------------------------

   procedure Insert_Valid_Check (Expr : Node_Id) is
      Loc : constant Source_Ptr := Sloc (Expr);

   begin
      --  Do not insert if checks off, or if not checking validity

      if Range_Checks_Suppressed (Etype (Expr))
        or else Validity_Checking = None
      then
         null;

      else
         Insert_Action
           (Expr,
            Make_Raise_Constraint_Error (Loc,
              Condition =>
                Make_Op_Not (Loc,
                  Right_Opnd =>
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                        Duplicate_Subexpr (Expr, Name_Req => True),
                      Attribute_Name => Name_Valid))),
            Suppress => All_Checks);
      end if;
   end Insert_Valid_Check;

   --------------------------
   -- Install_Static_Check --
   --------------------------

   procedure Install_Static_Check (R_Cno : Node_Id; Loc : Source_Ptr) is
      Stat : constant Boolean   := Is_Static_Expression (R_Cno);
      Typ  : constant Entity_Id := Etype (R_Cno);

   begin
      Rewrite (R_Cno, Make_Raise_Constraint_Error (Loc));
      Set_Analyzed (R_Cno);
      Set_Etype (R_Cno, Typ);
      Set_Raises_Constraint_Error (R_Cno);
      Set_Is_Static_Expression (R_Cno, Stat);
   end Install_Static_Check;

   ------------------------------
   -- Length_Checks_Suppressed --
   ------------------------------

   function Length_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Length_Checks
        or else (Present (E) and then Suppress_Length_Checks (E));
   end Length_Checks_Suppressed;

   --------------------------------
   -- Overflow_Checks_Suppressed --
   --------------------------------

   function Overflow_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Overflow_Checks
        or else (Present (E) and then Suppress_Overflow_Checks (E));
   end Overflow_Checks_Suppressed;

   -----------------
   -- Range_Check --
   -----------------

   function Range_Check
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id := Empty;
      Warn_Node  : Node_Id   := Empty)
      return       Check_Result
   is
   begin
      return Selected_Range_Checks
        (Ck_Node, Target_Typ, Source_Typ, Warn_Node);
   end Range_Check;

   -----------------------------
   -- Range_Checks_Suppressed --
   -----------------------------

   function Range_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      --  Note: for now we always suppress range checks on Vax float types,
      --  since Gigi does not know how to generate these checks.

      return Scope_Suppress.Range_Checks
        or else (Present (E) and then Suppress_Range_Checks (E))
        or else Vax_Float (E);
   end Range_Checks_Suppressed;

   ----------------------------
   -- Selected_Length_Checks --
   ----------------------------

   function Selected_Length_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Warn_Node  : Node_Id)
      return       Check_Result
   is
      Loc         : constant Source_Ptr := Sloc (Ck_Node);
      S_Typ       : Entity_Id;
      T_Typ       : Entity_Id;
      Expr_Actual : Node_Id;
      Exptyp      : Entity_Id;
      Cond        : Node_Id := Empty;
      Do_Access   : Boolean := False;
      Wnode       : Node_Id := Warn_Node;
      Ret_Result  : Check_Result := (Empty, Empty);
      Num_Checks  : Natural := 0;

      procedure Add_Check (N : Node_Id);
      --  Adds the action given to Ret_Result if N is non-Empty

      function Get_E_Length (E : Entity_Id; Indx : Nat) return Node_Id;
      function Get_N_Length (N : Node_Id; Indx : Nat) return Node_Id;

      function Same_Bounds (L : Node_Id; R : Node_Id) return Boolean;
      --  True for equal literals and for nodes that denote the same constant
      --  entity, even if its value is not a static constant. This removes
      --  some obviously superfluous checks.

      function Length_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat)
         return   Node_Id;
      --  Returns expression to compute:
      --    Typ'Length /= Exptyp'Length

      function Length_N_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id;
         Indx : Nat)
         return Node_Id;
      --  Returns expression to compute:
      --    Typ'Length /= Expr'Length

      ---------------
      -- Add_Check --
      ---------------

      procedure Add_Check (N : Node_Id) is
      begin
         if Present (N) then

            --  For now, ignore attempt to place more than 2 checks ???

            if Num_Checks = 2 then
               return;
            end if;

            pragma Assert (Num_Checks <= 1);
            Num_Checks := Num_Checks + 1;
            Ret_Result (Num_Checks) := N;
         end if;
      end Add_Check;

      ------------------
      -- Get_E_Length --
      ------------------

      function Get_E_Length (E : Entity_Id; Indx : Nat) return Node_Id is
         N  : Node_Id;
         E1 : Entity_Id := E;
         Pt : Entity_Id := Scope (Scope (E));

      begin
         if Ekind (Scope (E)) = E_Record_Type
           and then Has_Discriminants (Scope (E))
         then
            N := Build_Discriminal_Subtype_Of_Component (E);

            if Present (N) then
               Insert_Action (Ck_Node, N);
               E1 := Defining_Identifier (N);
            end if;
         end if;

         if Ekind (E1) = E_String_Literal_Subtype then
            return
              Make_Integer_Literal (Loc,
                Intval => String_Literal_Length (E1));

         elsif Ekind (Pt) = E_Protected_Type
           and then Has_Discriminants (Pt)
           and then Has_Completion (Pt)
           and then not Inside_Init_Proc
         then

            --  If the type whose length is needed is a private component
            --  constrained by a discriminant, we must expand the 'Length
            --  attribute into an explicit computation, using the discriminal
            --  of the current protected operation. This is because the actual
            --  type of the prival is constructed after the protected opera-
            --  tion has been fully expanded.

            declare
               Indx_Type : Node_Id;
               Lo        : Node_Id;
               Hi        : Node_Id;
               Do_Expand : Boolean := False;

            begin
               Indx_Type := First_Index (E);

               for J in 1 .. Indx - 1 loop
                  Next_Index (Indx_Type);
               end loop;

               Get_Index_Bounds  (Indx_Type, Lo, Hi);

               if Nkind (Lo) = N_Identifier
                 and then Ekind (Entity (Lo)) = E_In_Parameter
               then
                  Lo := Get_Discriminal (E, Lo);
                  Do_Expand := True;
               end if;

               if Nkind (Hi) = N_Identifier
                 and then Ekind (Entity (Hi)) = E_In_Parameter
               then
                  Hi := Get_Discriminal (E, Hi);
                  Do_Expand := True;
               end if;

               if Do_Expand then
                  if not Is_Entity_Name (Lo) then
                     Lo := Duplicate_Subexpr (Lo);
                  end if;

                  if not Is_Entity_Name (Hi) then
                     Lo := Duplicate_Subexpr (Hi);
                  end if;

                  N :=
                    Make_Op_Add (Loc,
                      Left_Opnd =>
                        Make_Op_Subtract (Loc,
                          Left_Opnd  => Hi,
                          Right_Opnd => Lo),

                      Right_Opnd => Make_Integer_Literal (Loc, 1));
                  return N;

               else
                  N :=
                    Make_Attribute_Reference (Loc,
                      Attribute_Name => Name_Length,
                      Prefix =>
                        New_Occurrence_Of (E1, Loc));

                  if Indx > 1 then
                     Set_Expressions (N, New_List (
                       Make_Integer_Literal (Loc, Indx)));
                  end if;

                  return N;
               end if;
            end;

         else
            N :=
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Length,
                Prefix =>
                  New_Occurrence_Of (E1, Loc));

            if Indx > 1 then
               Set_Expressions (N, New_List (
                 Make_Integer_Literal (Loc, Indx)));
            end if;

            return N;

         end if;
      end Get_E_Length;

      ------------------
      -- Get_N_Length --
      ------------------

      function Get_N_Length (N : Node_Id; Indx : Nat) return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Length,
             Prefix =>
               Duplicate_Subexpr (N, Name_Req => True),
             Expressions => New_List (
               Make_Integer_Literal (Loc, Indx)));

      end Get_N_Length;

      -------------------
      -- Length_E_Cond --
      -------------------

      function Length_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat)
         return   Node_Id
      is
      begin
         return
           Make_Op_Ne (Loc,
             Left_Opnd  => Get_E_Length (Typ, Indx),
             Right_Opnd => Get_E_Length (Exptyp, Indx));

      end Length_E_Cond;

      -------------------
      -- Length_N_Cond --
      -------------------

      function Length_N_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id;
         Indx : Nat)
         return Node_Id
      is
      begin
         return
           Make_Op_Ne (Loc,
             Left_Opnd  => Get_E_Length (Typ, Indx),
             Right_Opnd => Get_N_Length (Expr, Indx));

      end Length_N_Cond;

      function Same_Bounds (L : Node_Id; R : Node_Id) return Boolean is
      begin
         return
           (Nkind (L) = N_Integer_Literal
             and then Nkind (R) = N_Integer_Literal
             and then Intval (L) = Intval (R))

          or else
            (Is_Entity_Name (L)
              and then Ekind (Entity (L)) = E_Constant
              and then ((Is_Entity_Name (R)
                         and then Entity (L) = Entity (R))
                        or else
                       (Nkind (R) = N_Type_Conversion
                         and then Is_Entity_Name (Expression (R))
                         and then Entity (L) = Entity (Expression (R)))))

          or else
            (Is_Entity_Name (R)
              and then Ekind (Entity (R)) = E_Constant
              and then Nkind (L) = N_Type_Conversion
              and then Is_Entity_Name (Expression (L))
              and then Entity (R) = Entity (Expression (L)));
      end Same_Bounds;

   --  Start of processing for Selected_Length_Checks

   begin
      if not Expander_Active then
         return Ret_Result;
      end if;

      if Target_Typ = Any_Type
        or else Target_Typ = Any_Composite
        or else Raises_Constraint_Error (Ck_Node)
      then
         return Ret_Result;
      end if;

      if No (Wnode) then
         Wnode := Ck_Node;
      end if;

      T_Typ := Target_Typ;

      if No (Source_Typ) then
         S_Typ := Etype (Ck_Node);
      else
         S_Typ := Source_Typ;
      end if;

      if S_Typ = Any_Type or else S_Typ = Any_Composite then
         return Ret_Result;
      end if;

      if Is_Access_Type (T_Typ) and then Is_Access_Type (S_Typ) then
         S_Typ := Designated_Type (S_Typ);
         T_Typ := Designated_Type (T_Typ);
         Do_Access := True;

         --  A simple optimization

         if Nkind (Ck_Node) = N_Null then
            return Ret_Result;
         end if;
      end if;

      if Is_Array_Type (T_Typ) and then Is_Array_Type (S_Typ) then
         if Is_Constrained (T_Typ) then

            --  The checking code to be generated will freeze the
            --  corresponding array type. However, we must freeze the
            --  type now, so that the freeze node does not appear within
            --  the generated condional expression, but ahead of it.

            Freeze_Before (Ck_Node, T_Typ);

            Expr_Actual := Get_Referenced_Object (Ck_Node);
            Exptyp      := Get_Actual_Subtype (Expr_Actual);

            if Is_Access_Type (Exptyp) then
               Exptyp := Designated_Type (Exptyp);
            end if;

            --  String_Literal case. This needs to be handled specially be-
            --  cause no index types are available for string literals. The
            --  condition is simply:

            --    T_Typ'Length = string-literal-length

            if Nkind (Expr_Actual) = N_String_Literal then
               Cond :=
                 Make_Op_Ne (Loc,
                   Left_Opnd  => Get_E_Length (T_Typ, 1),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc,
                       Intval =>
                         String_Literal_Length (Etype (Expr_Actual))));

            --  General array case. Here we have a usable actual subtype for
            --  the expression, and the condition is built from the two types
            --  (Do_Length):

            --     T_Typ'Length     /= Exptyp'Length     or else
            --     T_Typ'Length (2) /= Exptyp'Length (2) or else
            --     T_Typ'Length (3) /= Exptyp'Length (3) or else
            --     ...

            elsif Is_Constrained (Exptyp) then
               declare
                  L_Index : Node_Id;
                  R_Index : Node_Id;
                  Ndims   : Nat := Number_Dimensions (T_Typ);

                  L_Low  : Node_Id;
                  L_High : Node_Id;
                  R_Low  : Node_Id;
                  R_High : Node_Id;

                  L_Length : Uint;
                  R_Length : Uint;

               begin
                  L_Index := First_Index (T_Typ);
                  R_Index := First_Index (Exptyp);

                  for Indx in 1 .. Ndims loop
                     if not (Nkind (L_Index) = N_Raise_Constraint_Error
                       or else Nkind (R_Index) = N_Raise_Constraint_Error)
                     then
                        Get_Index_Bounds (L_Index, L_Low, L_High);
                        Get_Index_Bounds (R_Index, R_Low, R_High);

                        --  Deal with compile time length check. Note that we
                        --  skip this in the access case, because the access
                        --  value may be null, so we cannot know statically.

                        if not Do_Access
                          and then Compile_Time_Known_Value (L_Low)
                          and then Compile_Time_Known_Value (L_High)
                          and then Compile_Time_Known_Value (R_Low)
                          and then Compile_Time_Known_Value (R_High)
                        then
                           if Expr_Value (L_High) >= Expr_Value (L_Low) then
                              L_Length := Expr_Value (L_High) -
                                          Expr_Value (L_Low) + 1;
                           else
                              L_Length := UI_From_Int (0);
                           end if;

                           if Expr_Value (R_High) >= Expr_Value (R_Low) then
                              R_Length := Expr_Value (R_High) -
                                          Expr_Value (R_Low) + 1;
                           else
                              R_Length := UI_From_Int (0);
                           end if;

                           if L_Length > R_Length then
                              Add_Check
                                (Compile_Time_Constraint_Error
                                  (Wnode, "too few elements for}?", T_Typ));

                           elsif  L_Length < R_Length then
                              Add_Check
                                (Compile_Time_Constraint_Error
                                  (Wnode, "too many elements for}?", T_Typ));
                           end if;

                        --  The comparison for an individual index subtype
                        --  is omitted if the corresponding index subtypes
                        --  statically match, since the result is known to
                        --  be true. Note that this test is worth while even
                        --  though we do static evaluation, because non-static
                        --  subtypes can statically match.

                        elsif not
                          Subtypes_Statically_Match
                            (Etype (L_Index), Etype (R_Index))

                          and then not
                            (Same_Bounds (L_Low, R_Low)
                              and then Same_Bounds (L_High, R_High))
                        then
                           Evolve_Or_Else
                             (Cond, Length_E_Cond (Exptyp, T_Typ, Indx));
                        end if;

                        Next (L_Index);
                        Next (R_Index);
                     end if;
                  end loop;
               end;

            --  Handle cases where we do not get a usable actual subtype that
            --  is constrained. This happens for example in the function call
            --  and explicit dereference cases. In these cases, we have to get
            --  the length or range from the expression itself, making sure we
            --  do not evaluate it more than once.

            --  Here Ck_Node is the original expression, or more properly the
            --  result of applying Duplicate_Expr to the original tree,
            --  forcing the result to be a name.

            else
               declare
                  Ndims   : Nat := Number_Dimensions (T_Typ);

               begin
                  --  Build the condition for the explicit dereference case

                  for Indx in 1 .. Ndims loop
                     Evolve_Or_Else
                       (Cond, Length_N_Cond (Ck_Node, T_Typ, Indx));
                  end loop;
               end;
            end if;
         end if;
      end if;

      --  Construct the test and insert into the tree

      if Present (Cond) then
         if Do_Access then
            Cond := Guard_Access (Cond, Loc, Ck_Node);
         end if;

         Add_Check (Make_Raise_Constraint_Error (Loc, Condition => Cond));
      end if;

      return Ret_Result;

   end Selected_Length_Checks;

   ---------------------------
   -- Selected_Range_Checks --
   ---------------------------

   function Selected_Range_Checks
     (Ck_Node    : Node_Id;
      Target_Typ : Entity_Id;
      Source_Typ : Entity_Id;
      Warn_Node  : Node_Id)
      return       Check_Result
   is
      Loc         : constant Source_Ptr := Sloc (Ck_Node);
      S_Typ       : Entity_Id;
      T_Typ       : Entity_Id;
      Expr_Actual : Node_Id;
      Exptyp      : Entity_Id;
      Cond        : Node_Id := Empty;
      Do_Access   : Boolean := False;
      Wnode       : Node_Id  := Warn_Node;
      Ret_Result  : Check_Result := (Empty, Empty);
      Num_Checks  : Integer := 0;

      procedure Add_Check (N : Node_Id);
      --  Adds the action given to Ret_Result if N is non-Empty

      function Discrete_Range_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id)
         return Node_Id;
      --  Returns expression to compute:
      --    Low_Bound (Expr) < Typ'First
      --      or else
      --    High_Bound (Expr) > Typ'Last

      function Discrete_Expr_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id)
         return Node_Id;
      --  Returns expression to compute:
      --    Expr < Typ'First
      --      or else
      --    Expr > Typ'Last

      function Get_E_First_Or_Last
        (E    : Entity_Id;
         Indx : Nat;
         Nam  : Name_Id)
         return Node_Id;
      --  Returns expression to compute:
      --    E'First or E'Last

      function Get_N_First (N : Node_Id; Indx : Nat) return Node_Id;
      function Get_N_Last  (N : Node_Id; Indx : Nat) return Node_Id;
      --  Returns expression to compute:
      --    N'First or N'Last using Duplicate_Subexpr

      function Range_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat)
         return   Node_Id;
      --  Returns expression to compute:
      --    Exptyp'First < Typ'First or else Exptyp'Last > Typ'Last

      function Range_Equal_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat)
         return   Node_Id;
      --  Returns expression to compute:
      --    Exptyp'First /= Typ'First or else Exptyp'Last /= Typ'Last

      function Range_N_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id;
         Indx : Nat)
         return Node_Id;
      --  Return expression to compute:
      --    Expr'First < Typ'First or else Expr'Last > Typ'Last

      ---------------
      -- Add_Check --
      ---------------

      procedure Add_Check (N : Node_Id) is
      begin
         if Present (N) then

            --  For now, ignore attempt to place more than 2 checks ???

            if Num_Checks = 2 then
               return;
            end if;

            pragma Assert (Num_Checks <= 1);
            Num_Checks := Num_Checks + 1;
            Ret_Result (Num_Checks) := N;
         end if;
      end Add_Check;

      -------------------------
      -- Discrete_Range_Cond --
      -------------------------

      function Discrete_Range_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id)
         return Node_Id
      is
         LB : Node_Id := Low_Bound (Expr);
         HB : Node_Id := High_Bound (Expr);

         Left_Opnd  : Node_Id;
         Right_Opnd : Node_Id;

      begin
         if Nkind (LB) = N_Identifier
           and then Ekind (Entity (LB)) = E_Discriminant then
            LB := New_Occurrence_Of (Discriminal (Entity (LB)), Loc);
         end if;

         if Nkind (HB) = N_Identifier
           and then Ekind (Entity (HB)) = E_Discriminant then
            HB := New_Occurrence_Of (Discriminal (Entity (HB)), Loc);
         end if;

         Left_Opnd :=
           Make_Op_Lt (Loc,
             Left_Opnd  =>
               Convert_To
                 (Base_Type (Typ), Duplicate_Subexpr (LB)),

             Right_Opnd =>
               Convert_To
                 (Base_Type (Typ), Get_E_First_Or_Last (Typ, 0, Name_First)));

         if Base_Type (Typ) = Typ then
            return Left_Opnd;

         elsif Compile_Time_Known_Value (High_Bound (Scalar_Range (Typ)))
            and then
               Compile_Time_Known_Value (High_Bound (Scalar_Range
                                                     (Base_Type (Typ))))
         then
            if Is_Floating_Point_Type (Typ) then
               if Expr_Value_R (High_Bound (Scalar_Range (Typ))) =
                  Expr_Value_R (High_Bound (Scalar_Range (Base_Type (Typ))))
               then
                  return Left_Opnd;
               end if;

            else
               if Expr_Value (High_Bound (Scalar_Range (Typ))) =
                  Expr_Value (High_Bound (Scalar_Range (Base_Type (Typ))))
               then
                  return Left_Opnd;
               end if;
            end if;
         end if;

         Right_Opnd :=
           Make_Op_Gt (Loc,
             Left_Opnd  =>
               Convert_To
                 (Base_Type (Typ), Duplicate_Subexpr (HB)),

             Right_Opnd =>
               Convert_To
                 (Base_Type (Typ),
                  Get_E_First_Or_Last (Typ, 0, Name_Last)));

         return Make_Or_Else (Loc, Left_Opnd, Right_Opnd);
      end Discrete_Range_Cond;

      -------------------------
      -- Discrete_Expr_Cond --
      -------------------------

      function Discrete_Expr_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id)
         return Node_Id
      is
      begin
         return
           Make_Or_Else (Loc,
             Left_Opnd =>
               Make_Op_Lt (Loc,
                 Left_Opnd =>
                   Convert_To (Base_Type (Typ), Duplicate_Subexpr (Expr)),
                 Right_Opnd =>
                   Convert_To (Base_Type (Typ),
                               Get_E_First_Or_Last (Typ, 0, Name_First))),

             Right_Opnd =>
               Make_Op_Gt (Loc,
                 Left_Opnd =>
                   Convert_To (Base_Type (Typ), Duplicate_Subexpr (Expr)),
                 Right_Opnd =>
                   Convert_To
                     (Base_Type (Typ),
                      Get_E_First_Or_Last (Typ, 0, Name_Last))));
      end Discrete_Expr_Cond;

      -------------------------
      -- Get_E_First_Or_Last --
      -------------------------

      function Get_E_First_Or_Last
        (E    : Entity_Id;
         Indx : Nat;
         Nam  : Name_Id)
         return Node_Id
      is
         N     : Node_Id;
         LB    : Node_Id;
         HB    : Node_Id;
         Bound : Node_Id;

      begin
         if Is_Array_Type (E) then
            N := First_Index (E);

            for J in 2 .. Indx loop
               Next_Index (N);
            end loop;

         else
            N := Scalar_Range (E);
         end if;

         if Nkind (N) = N_Subtype_Indication then
            LB := Low_Bound (Range_Expression (Constraint (N)));
            HB := High_Bound (Range_Expression (Constraint (N)));

         elsif Is_Entity_Name (N) then
            LB := Type_Low_Bound  (Etype (N));
            HB := Type_High_Bound (Etype (N));

         else
            LB := Low_Bound  (N);
            HB := High_Bound (N);
         end if;

         if Nam = Name_First then
            Bound := LB;
         else
            Bound := HB;
         end if;

         if Nkind (Bound) = N_Identifier
           and then Ekind (Entity (Bound)) = E_Discriminant
         then
            return New_Occurrence_Of (Discriminal (Entity (Bound)), Loc);

         elsif Nkind (Bound) = N_Identifier
           and then Ekind (Entity (Bound)) = E_In_Parameter
           and then not Inside_Init_Proc
         then
            return Get_Discriminal (E, Bound);

         elsif Nkind (Bound) = N_Integer_Literal then
            return  Make_Integer_Literal (Loc, Intval (Bound));

         else
            return Duplicate_Subexpr (Bound);
         end if;
      end Get_E_First_Or_Last;

      -----------------
      -- Get_N_First --
      -----------------

      function Get_N_First (N : Node_Id; Indx : Nat) return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_First,
             Prefix =>
               Duplicate_Subexpr (N, Name_Req => True),
             Expressions => New_List (
               Make_Integer_Literal (Loc, Indx)));

      end Get_N_First;

      ----------------
      -- Get_N_Last --
      ----------------

      function Get_N_Last (N : Node_Id; Indx : Nat) return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Last,
             Prefix =>
               Duplicate_Subexpr (N, Name_Req => True),
             Expressions => New_List (
              Make_Integer_Literal (Loc, Indx)));

      end Get_N_Last;

      ------------------
      -- Range_E_Cond --
      ------------------

      function Range_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat)
         return   Node_Id
      is
      begin
         return
           Make_Or_Else (Loc,
             Left_Opnd =>
               Make_Op_Lt (Loc,
                 Left_Opnd => Get_E_First_Or_Last (Exptyp, Indx, Name_First),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_First)),

             Right_Opnd =>
               Make_Op_Gt (Loc,
                 Left_Opnd => Get_E_First_Or_Last (Exptyp, Indx, Name_Last),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_Last)));

      end Range_E_Cond;

      ------------------------
      -- Range_Equal_E_Cond --
      ------------------------

      function Range_Equal_E_Cond
        (Exptyp : Entity_Id;
         Typ    : Entity_Id;
         Indx   : Nat)
         return   Node_Id
      is
      begin
         return
           Make_Or_Else (Loc,
             Left_Opnd =>
               Make_Op_Ne (Loc,
                 Left_Opnd => Get_E_First_Or_Last (Exptyp, Indx, Name_First),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_First)),
             Right_Opnd =>
               Make_Op_Ne (Loc,
                 Left_Opnd => Get_E_First_Or_Last (Exptyp, Indx, Name_Last),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_Last)));
      end Range_Equal_E_Cond;

      ------------------
      -- Range_N_Cond --
      ------------------

      function Range_N_Cond
        (Expr : Node_Id;
         Typ  : Entity_Id;
         Indx : Nat)
         return Node_Id
      is
      begin
         return
           Make_Or_Else (Loc,
             Left_Opnd =>
               Make_Op_Lt (Loc,
                 Left_Opnd => Get_N_First (Expr, Indx),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_First)),

             Right_Opnd =>
               Make_Op_Gt (Loc,
                 Left_Opnd => Get_N_Last (Expr, Indx),
                 Right_Opnd  => Get_E_First_Or_Last (Typ, Indx, Name_Last)));
      end Range_N_Cond;

   --  Start of processing for Selected_Range_Checks

   begin
      if not Expander_Active then
         return Ret_Result;
      end if;

      if Target_Typ = Any_Type
        or else Target_Typ = Any_Composite
        or else Raises_Constraint_Error (Ck_Node)
      then
         return Ret_Result;
      end if;

      if No (Wnode) then
         Wnode := Ck_Node;
      end if;

      T_Typ := Target_Typ;

      if No (Source_Typ) then
         S_Typ := Etype (Ck_Node);
      else
         S_Typ := Source_Typ;
      end if;

      if S_Typ = Any_Type or else S_Typ = Any_Composite then
         return Ret_Result;
      end if;

      --  The order of evaluating T_Typ before S_Typ seems to be critical
      --  because S_Typ can be derived from Etype (Ck_Node), if it's not passed
      --  in, and since Node can be an N_Range node, it might be invalid.
      --  Should there be an assert check somewhere for taking the Etype of
      --  an N_Range node ???

      if Is_Access_Type (T_Typ) and then Is_Access_Type (S_Typ) then
         S_Typ := Designated_Type (S_Typ);
         T_Typ := Designated_Type (T_Typ);
         Do_Access := True;

         --  A simple optimization

         if Nkind (Ck_Node) = N_Null then
            return Ret_Result;
         end if;
      end if;

      --  For an N_Range Node, check for a null range and then if not
      --  null generate a range check action.

      if Nkind (Ck_Node) = N_Range then

         --  There's no point in checking a range against itself

         if Ck_Node = Scalar_Range (T_Typ) then
            return Ret_Result;
         end if;

         declare
            T_LB       : constant Node_Id := Type_Low_Bound  (T_Typ);
            T_HB       : constant Node_Id := Type_High_Bound (T_Typ);
            LB         : Node_Id := Low_Bound (Ck_Node);
            HB         : Node_Id := High_Bound (Ck_Node);
            Null_Range : Boolean;

            Out_Of_Range_L : Boolean;
            Out_Of_Range_H : Boolean;

         begin
            --  Check for case where everything is static and we can
            --  do the check at compile time. This is skipped if we
            --  have an access type, since the access value may be null.

            --  ??? This code can be improved since you only need to know
            --  that the two respective bounds (LB & T_LB or HB & T_HB)
            --  are known at compile time to emit pertinent messages.

            if Compile_Time_Known_Value (LB)
              and then Compile_Time_Known_Value (HB)
              and then Compile_Time_Known_Value (T_LB)
              and then Compile_Time_Known_Value (T_HB)
              and then not Do_Access
            then
               --  Floating-point case

               if Is_Floating_Point_Type (S_Typ) then
                  Null_Range := Expr_Value_R (HB) < Expr_Value_R (LB);
                  Out_Of_Range_L :=
                    (Expr_Value_R (LB) < Expr_Value_R (T_LB))
                       or else
                    (Expr_Value_R (LB) > Expr_Value_R (T_HB));

                  Out_Of_Range_H :=
                    (Expr_Value_R (HB) > Expr_Value_R (T_HB))
                       or else
                    (Expr_Value_R (HB) < Expr_Value_R (T_LB));

               --  Fixed or discrete type case

               else
                  Null_Range := Expr_Value (HB) < Expr_Value (LB);
                  Out_Of_Range_L :=
                    (Expr_Value (LB) < Expr_Value (T_LB))
                    or else
                    (Expr_Value (LB) > Expr_Value (T_HB));

                  Out_Of_Range_H :=
                    (Expr_Value (HB) > Expr_Value (T_HB))
                    or else
                    (Expr_Value (HB) < Expr_Value (T_LB));
               end if;

               if not Null_Range then
                  if Out_Of_Range_L then
                     if No (Warn_Node) then
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Low_Bound (Ck_Node),
                              "static value out of range of}?", T_Typ));

                     else
                        Add_Check
                          (Compile_Time_Constraint_Error
                            (Wnode,
                             "static range out of bounds of}?", T_Typ));
                     end if;
                  end if;

                  if Out_Of_Range_H then
                     if No (Warn_Node) then
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (High_Bound (Ck_Node),
                              "static value out of range of}?", T_Typ));

                     else
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Wnode,
                              "static range out of bounds of}?", T_Typ));
                     end if;
                  end if;

               end if;

            else
               declare
                  LB : Node_Id := Low_Bound (Ck_Node);
                  HB : Node_Id := High_Bound (Ck_Node);

               begin

                  --  If either bound is a discriminant and we are within
                  --  the record declaration, it is a use of the discriminant
                  --  in a constraint of a component, and nothing can be
                  --  checked here. The check will be emitted within the
                  --  init_proc. Before then, the discriminal has no real
                  --  meaning.

                  if Nkind (LB) = N_Identifier
                    and then Ekind (Entity (LB)) = E_Discriminant
                  then
                     if Current_Scope = Scope (Entity (LB)) then
                        return Ret_Result;
                     else
                        LB :=
                          New_Occurrence_Of (Discriminal (Entity (LB)), Loc);
                     end if;
                  end if;

                  if Nkind (HB) = N_Identifier
                    and then Ekind (Entity (HB)) = E_Discriminant
                  then
                     if Current_Scope = Scope (Entity (HB)) then
                        return Ret_Result;
                     else
                        HB :=
                          New_Occurrence_Of (Discriminal (Entity (HB)), Loc);
                     end if;
                  end if;

                  Cond := Discrete_Range_Cond (Ck_Node, T_Typ);
                  Set_Paren_Count (Cond, 1);

                  Cond :=
                    Make_And_Then (Loc,
                      Left_Opnd =>
                        Make_Op_Ge (Loc,
                          Left_Opnd  => Duplicate_Subexpr (HB),
                          Right_Opnd => Duplicate_Subexpr (LB)),
                      Right_Opnd => Cond);
               end;

            end if;
         end;

      elsif Is_Scalar_Type (S_Typ) then

         --  This somewhat duplicates what Apply_Scalar_Range_Check does,
         --  except the the above simply sets a flag in the node and lets
         --  gigi generate the check base on the Etype of the expression.
         --  Sometimes, however we want to do a dynamic check against an
         --  arbitrary target type, so we do that here.

         if Ekind (Base_Type (S_Typ)) /= Ekind (Base_Type (T_Typ)) then
            Cond := Discrete_Expr_Cond (Ck_Node, T_Typ);

         --  For literals, we can tell if the constraint error will be
         --  raised at compile time, so we never need a dynamic check, but
         --  if the exception will be raised, then post the usual warning,
         --  and replace the literal with a raise constraint error
         --  expression. As usual, skip this for access types

         elsif Compile_Time_Known_Value (Ck_Node)
           and then not Do_Access
         then
            declare
               LB : constant Node_Id := Type_Low_Bound (T_Typ);
               UB : constant Node_Id := Type_High_Bound (T_Typ);

               Out_Of_Range  : Boolean;
               Static_Bounds : constant Boolean :=
                                 Compile_Time_Known_Value (LB)
                                   and Compile_Time_Known_Value (UB);

            begin
               --  Following range tests should use Sem_Eval routine ???

               if Static_Bounds then
                  if Is_Floating_Point_Type (S_Typ) then
                     Out_Of_Range :=
                       (Expr_Value_R (Ck_Node) < Expr_Value_R (LB))
                         or else
                       (Expr_Value_R (Ck_Node) > Expr_Value_R (UB));

                  else -- fixed or discrete type
                     Out_Of_Range :=
                       Expr_Value (Ck_Node) < Expr_Value (LB)
                         or else
                       Expr_Value (Ck_Node) > Expr_Value (UB);
                  end if;

                  --  Bounds of the type are static and the literal is
                  --  out of range so make a warning message.

                  if Out_Of_Range then
                     if No (Warn_Node) then
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Ck_Node,
                              "static value out of range of}?", T_Typ));

                     else
                        Add_Check
                          (Compile_Time_Constraint_Error
                             (Wnode,
                              "static value out of range of}?", T_Typ));
                     end if;
                  end if;

               else
                  Cond := Discrete_Expr_Cond (Ck_Node, T_Typ);
               end if;
            end;

         --  Here for the case of a non-static expression, we need a runtime
         --  check unless the source type range is guaranteed to be in the
         --  range of the target type.

         else
            if not In_Subrange_Of (S_Typ, T_Typ) then
               Cond := Discrete_Expr_Cond (Ck_Node, T_Typ);
            end if;
         end if;
      end if;

      if Is_Array_Type (T_Typ) and then Is_Array_Type (S_Typ) then
         if Is_Constrained (T_Typ) then

            Expr_Actual := Get_Referenced_Object (Ck_Node);
            Exptyp      := Get_Actual_Subtype (Expr_Actual);

            if Is_Access_Type (Exptyp) then
               Exptyp := Designated_Type (Exptyp);
            end if;

            --  String_Literal case. This needs to be handled specially be-
            --  cause no index types are available for string literals. The
            --  condition is simply:

            --    T_Typ'Length = string-literal-length

            if Nkind (Expr_Actual) = N_String_Literal then
               null;

            --  General array case. Here we have a usable actual subtype for
            --  the expression, and the condition is built from the two types

            --     T_Typ'First     < Exptyp'First     or else
            --     T_Typ'Last      > Exptyp'Last      or else
            --     T_Typ'First(1)  < Exptyp'First(1)  or else
            --     T_Typ'Last(1)   > Exptyp'Last(1)   or else
            --     ...

            elsif Is_Constrained (Exptyp) then
               declare
                  L_Index : Node_Id;
                  R_Index : Node_Id;
                  Ndims   : Nat := Number_Dimensions (T_Typ);

                  L_Low  : Node_Id;
                  L_High : Node_Id;
                  R_Low  : Node_Id;
                  R_High : Node_Id;

               begin
                  L_Index := First_Index (T_Typ);
                  R_Index := First_Index (Exptyp);

                  for Indx in 1 .. Ndims loop
                     if not (Nkind (L_Index) = N_Raise_Constraint_Error
                       or else Nkind (R_Index) = N_Raise_Constraint_Error)
                     then
                        Get_Index_Bounds (L_Index, L_Low, L_High);
                        Get_Index_Bounds (R_Index, R_Low, R_High);

                        --  Deal with compile time length check. Note that we
                        --  skip this in the access case, because the access
                        --  value may be null, so we cannot know statically.

                        if not
                          Subtypes_Statically_Match
                            (Etype (L_Index), Etype (R_Index))
                        then
                           --  If the target type is constrained then we
                           --  have to check for exact equality of bounds
                           --  (required for qualified expressions).

                           if Is_Constrained (T_Typ) then
                              Evolve_Or_Else
                                (Cond,
                                 Range_Equal_E_Cond (Exptyp, T_Typ, Indx));

                           else
                              Evolve_Or_Else
                                (Cond, Range_E_Cond (Exptyp, T_Typ, Indx));
                           end if;
                        end if;

                        Next (L_Index);
                        Next (R_Index);

                     end if;
                  end loop;
               end;

            --  Handle cases where we do not get a usable actual subtype that
            --  is constrained. This happens for example in the function call
            --  and explicit dereference cases. In these cases, we have to get
            --  the length or range from the expression itself, making sure we
            --  do not evaluate it more than once.

            --  Here Ck_Node is the original expression, or more properly the
            --  result of applying Duplicate_Expr to the original tree,
            --  forcing the result to be a name.

            else
               declare
                  Ndims   : Nat := Number_Dimensions (T_Typ);

               begin
                  --  Build the condition for the explicit dereference case

                  for Indx in 1 .. Ndims loop
                     Evolve_Or_Else
                       (Cond, Range_N_Cond (Ck_Node, T_Typ, Indx));
                  end loop;
               end;

            end if;

         else
            --  Generate an Action to check that the bounds of the
            --  source value are within the constraints imposed by the
            --  target type for a conversion to an unconstrained type.
            --  Rule is 4.6(38).

            if Nkind (Parent (Ck_Node)) = N_Type_Conversion then
               declare
                  Opnd_Index : Node_Id;
                  Targ_Index : Node_Id;

               begin
                  Opnd_Index
                    := First_Index (Get_Actual_Subtype (Ck_Node));
                  Targ_Index := First_Index (T_Typ);

                  while Opnd_Index /= Empty loop
                     if Nkind (Opnd_Index) = N_Range then
                        if Is_In_Range
                             (Low_Bound (Opnd_Index), Etype (Targ_Index))
                          and then
                            Is_In_Range
                             (High_Bound (Opnd_Index), Etype (Targ_Index))
                        then
                           null;

                        elsif Is_Out_Of_Range
                                (Low_Bound (Opnd_Index), Etype (Targ_Index))
                          or else
                              Is_Out_Of_Range
                                (High_Bound (Opnd_Index), Etype (Targ_Index))
                        then
                           Add_Check
                             (Compile_Time_Constraint_Error
                               (Wnode, "value out of range of}?", T_Typ));

                        else
                           Evolve_Or_Else
                             (Cond,
                              Discrete_Range_Cond
                                (Opnd_Index, Etype (Targ_Index)));
                        end if;
                     end if;

                     Next_Index (Opnd_Index);
                     Next_Index (Targ_Index);
                  end loop;
               end;
            end if;
         end if;
      end if;

      --  Construct the test and insert into the tree

      if Present (Cond) then
         if Do_Access then
            Cond := Guard_Access (Cond, Loc, Ck_Node);
         end if;

         Add_Check (Make_Raise_Constraint_Error (Loc, Condition => Cond));
      end if;

      return Ret_Result;

   end Selected_Range_Checks;

   -------------------------------
   -- Storage_Checks_Suppressed --
   -------------------------------

   function Storage_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Storage_Checks
        or else (Present (E) and then Suppress_Storage_Checks (E));
   end Storage_Checks_Suppressed;

   ---------------------------
   -- Tag_Checks_Suppressed --
   ---------------------------

   function Tag_Checks_Suppressed (E : Entity_Id) return Boolean is
   begin
      return Scope_Suppress.Tag_Checks
        or else (Present (E) and then Suppress_Tag_Checks (E));
   end Tag_Checks_Suppressed;


end Checks;
