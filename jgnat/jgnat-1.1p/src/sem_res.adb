------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ R E S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.680 $
--                                                                          --
--          Copyright (C) 1992-2000, Free Software Foundation, Inc.         --
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
with Checks;   use Checks;
with Debug;    use Debug;
with Debug_A;  use Debug_A;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aggr; use Sem_Aggr;
with Sem_Attr; use Sem_Attr;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch4;  use Sem_Ch4;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Disp; use Sem_Disp;
with Sem_Dist; use Sem_Dist;
with Sem_Elab; use Sem_Elab;
with Sem_Eval; use Sem_Eval;
with Sem_Intr; use Sem_Intr;
with Sem_Util; use Sem_Util;
with Sem_Type; use Sem_Type;
with Sem_Warn; use Sem_Warn;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Sem_Res is

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  Second pass (top-down) type checking and overload resolution procedures
   --  Typ is the type required by context. These procedures propagate the
   --  type information recursively to the descendants of N. If the node
   --  is not overloaded, its Etype is established in the first pass. If
   --  overloaded,  the Resolve routines set the correct type. For arith.
   --  operators, the Etype is the base type of the context.

   --  Note that Resolve_Attribute is separated off in Sem_Attr

   procedure Ambiguous_Character (C : Node_Id);
   --  Give list of candidate interpretations when a character literal cannot
   --  be resolved.

   procedure Check_Discriminant_Use (N : Node_Id);
   --  Enforce the restrictions on the use of discriminants when constraining
   --  a component of a discriminated type (record or concurrent type).

   procedure Check_For_Visible_Operator (N : Node_Id; T : Entity_Id);
   --  Given a node for an operator associated with type T, check that
   --  the operator is visible. Operators all of whose operands are
   --  universal must be checked for visibility during resolution
   --  because their type is not determinable based on their operands.

   function Check_Infinite_Recursion (N : Node_Id) return Boolean;
   --  Given a call node, N, which is known to occur immediately within the
   --  subprogram being called, determines whether it is a detectable case of
   --  an infinite recursion, and if so, outputs appropriate messages. Returns
   --  True if an infinite recursion is detected, and False otherwise.

   procedure Check_Initialization_Call (N : Entity_Id; Nam : Entity_Id);
   --  If the type of the object being initialized uses the secondary stack
   --  directly or indirectly, create a transient scope for the call to the
   --  Init_Proc. This is because we do not create transient scopes for the
   --  initialization of individual components within the init_proc itself.
   --  Could be optimized away perhaps?

   procedure Resolve_Allocator                 (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Arithmetic_Op             (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Call                      (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Character_Literal         (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Comparison_Op             (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Conditional_Expression    (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Equality_Op               (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Explicit_Dereference      (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Entity_Name               (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Indexed_Component         (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Integer_Literal           (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Logical_Op                (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Membership_Op             (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Null                      (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Operator_Symbol           (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Op_Concat                 (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Op_Expon                  (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Op_Not                    (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Qualified_Expression      (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Range                     (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Real_Literal              (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Reference                 (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Selected_Component        (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Shift                     (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Short_Circuit             (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Slice                     (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_String_Literal            (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Subprogram_Info           (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Type_Conversion           (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Unary_Op                  (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Unchecked_Expression      (N : Node_Id; Typ : Entity_Id);
   procedure Resolve_Unchecked_Type_Conversion (N : Node_Id; Typ : Entity_Id);

   function Operator_Kind
     (Op_Name   : Name_Id;
      Is_Binary : Boolean)
      return      Node_Kind;
   --  Utility to map the name of an operator into the corresponding Node. Used
   --  by other node rewriting procedures.

   procedure Resolve_Actuals (N : Node_Id; Nam : Entity_Id);
   --  Resolve actuals of call, and add default expressions for missing ones.

   procedure Resolve_Entry_Call (N : Node_Id; Typ : Entity_Id);
   --  Called from Resolve_Call, when the prefix denotes an entry or element
   --  of entry family. Actuals are resolved as for subprograms, and the node
   --  is rebuilt as an entry call. Also called for protected operations. Typ
   --  is the context type, which is used when the operation is a protected
   --  function with no arguments, and the return value is indexed.

   procedure Resolve_Intrinsic_Operator (N : Node_Id; Typ : Entity_Id);
   --  A call to a user-defined intrinsic operator is rewritten as a call
   --  to the corresponding predefined operator, with suitable conversions.

   procedure Rewrite_Operator_As_Call (N : Node_Id; Nam : Entity_Id);
   --  If an operator node resolves to a call to a user-defined operator,
   --  rewrite the node as a function call.

   procedure Make_Call_Into_Operator
     (N     : Node_Id;
      Typ   : Entity_Id;
      Op_Id : Entity_Id);
   --  Inverse transformation: if an operator is given in functional notation,
   --  then after resolving the node, transform into an operator node, so
   --  that operands are resolved properly. Recall that predefined operators
   --  do not have a full signature and special resolution rules apply.

   procedure Rewrite_Renamed_Operator (N : Node_Id; Op : Entity_Id);
   --  An operator can rename another, e.g. in  an instantiation. In that
   --  case, the proper operator node must be constructed.

   procedure Set_String_Literal_Subtype (N : Node_Id; Typ : Entity_Id);
   --  The String_Literal_Subtype is built for all strings that are not
   --  operands of a static concatenation operation. If the argument is not
   --  a String the function is a no-op.

   procedure Set_Slice_Subtype (N : Node_Id);
   --  Build subtype of array type, with the range specified by the slice.

   function Unique_Fixed_Point_Type (N : Node_Id) return Entity_Id;
   --  A universal_fixed expression in an universal context is unambiguous if
   --  there is only one applicable fixed point type. Determining whether
   --  there is only one requires a search over all visible entities, and
   --  happens only in very pathological cases (see 6115-006).

   function Valid_Conversion
     (N       : Node_Id;
      Target  : Entity_Id;
      Operand : Node_Id)
      return    Boolean;
   --  Verify legality rules given in 4.6 (8-23). Target is the target
   --  type of the conversion, which may be an implicit conversion of
   --  an actual parameter to an anonymous access type (in which case
   --  N denotes the actual parameter and N = Operand).

   -------------------------
   -- Ambiguous_Character --
   -------------------------

   procedure Ambiguous_Character (C : Node_Id) is
      E : Entity_Id;

   begin
      if Nkind (C) = N_Character_Literal then
         Error_Msg_N ("ambiguous character literal", C);
         Error_Msg_N
           ("\possible interpretations: Character, Wide_Character!", C);

         E := Current_Entity (C);

         if Present (E) then

            while Present (E) loop
               Error_Msg_NE ("\possible interpretation:}!", C, Etype (E));
               E := Homonym (E);
            end loop;
         end if;
      end if;
   end Ambiguous_Character;

   -------------------------
   -- Analyze_And_Resolve --
   -------------------------

   procedure Analyze_And_Resolve (N : Node_Id) is
   begin
      Analyze (N);
      Resolve (N, Etype (N));
   end Analyze_And_Resolve;

   procedure Analyze_And_Resolve (N : Node_Id; Typ : Entity_Id) is
   begin
      Analyze (N);
      Resolve (N, Typ);
   end Analyze_And_Resolve;

   --  Version withs check(s) suppressed

   procedure Analyze_And_Resolve
     (N        : Node_Id;
      Typ      : Entity_Id;
      Suppress : Check_Id)
   is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Record := Scope_Suppress;

         begin
            Scope_Suppress := (others => True);
            Analyze_And_Resolve (N, Typ);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Get_Scope_Suppress (Suppress);

         begin
            Set_Scope_Suppress (Suppress, True);
            Analyze_And_Resolve (N, Typ);
            Set_Scope_Suppress (Suppress, Svg);
         end;
      end if;
   end Analyze_And_Resolve;

   procedure Analyze_And_Resolve
     (N        : Node_Id;
      Suppress : Check_Id)
   is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Record := Scope_Suppress;

         begin
            Scope_Suppress := (others => True);
            Analyze_And_Resolve (N);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Get_Scope_Suppress (Suppress);

         begin
            Set_Scope_Suppress (Suppress, True);
            Analyze_And_Resolve (N);
            Set_Scope_Suppress (Suppress, Svg);
         end;
      end if;
   end Analyze_And_Resolve;

   ----------------------------
   -- Check_Discriminant_Use --
   ----------------------------

   procedure Check_Discriminant_Use (N : Node_Id) is
      PN   : constant Node_Id   := Parent (N);
      Disc : constant Entity_Id := Entity (N);
      P    : Node_Id;
      D    : Node_Id;

   begin
      --  Any use in a default expression is legal.

      if In_Default_Expression then
         null;

      elsif Nkind (PN) = N_Range then

         --  Discriminant cannot be used to constrain a scalar type.

         P := Parent (PN);

         if Nkind (P) = N_Range_Constraint
           and then Nkind (Parent (P)) = N_Subtype_Indication
           and then Nkind (Parent (Parent (P))) = N_Component_Declaration
         then
            Error_Msg_N ("discriminant cannot constrain scalar type", N);

         elsif Nkind (P) = N_Index_Or_Discriminant_Constraint then

            --  The following check catches the unusual case where
            --  a discriminant appears within an index constraint
            --  that is part of a larger expression within a constraint
            --  on a component, e.g. "C : Int range 1 .. F (new A(1 .. D))".
            --  For now we only check case of record components, and
            --  note that a similar check should also apply in the
            --  case of discriminant constraints below. ???

            --  Note that the check for N_Subtype_Declaration below is to
            --  detect the valid use of discriminants in the constraints of a
            --  subtype declaration when this subtype declaration appears
            --  inside the scope of a record type (which is syntactically
            --  illegal, but which may be created as part of derived type
            --  processing for records). See Sem_Ch3.Build_Derived_Record_Type
            --  for more info.

            if Ekind (Current_Scope) = E_Record_Type
              and then Scope (Disc) = Current_Scope
              and then not
                (Nkind (Parent (P)) = N_Subtype_Indication
                 and then
                  (Nkind (Parent (Parent (P))) = N_Component_Declaration
                   or else Nkind (Parent (Parent (P))) = N_Subtype_Declaration)
                  and then Paren_Count (N) = 0)
            then
               Error_Msg_N
                 ("discriminant must appear alone in component constraint", N);
               return;
            end if;

            --   Detect a common beginner error:
            --   type R (D : Positive := 100) is record
            --     Name: String (1 .. D);
            --   end record;

            --  The default value causes an object of type R to be
            --  allocated with room for Positive'Last characters.

            declare
               SI : Node_Id;
               T  : Entity_Id;
               TB : Node_Id;
               CB : Entity_Id;

               function Large_Storage_Type (T : Entity_Id) return Boolean;
               --  Return True if type T has a large enough range that
               --  any array whose index type covered the whole range of
               --  the type would likely raise Storage_Error.

               function Large_Storage_Type (T : Entity_Id) return Boolean is
               begin
                  return
                    T = Standard_Integer
                      or else
                    T = Standard_Positive
                      or else
                    T = Standard_Natural;
               end Large_Storage_Type;

            begin
               --  Check that the Disc has a large range

               if not Large_Storage_Type (Etype (Disc)) then
                  goto No_Danger;
               end if;

               --  If the enclosing type is limited, we allocate only the
               --  default value, not the maximum, and there is no need for
               --  a warning.

               if Is_Limited_Type (Scope (Disc)) then
                  goto No_Danger;
               end if;

               --  Check that it is the high bound

               if N /= High_Bound (PN)
                 or else not Present (Discriminant_Default_Value (Disc))
               then
                  goto No_Danger;
               end if;

               --  Check the array allows a large range at this bound.
               --  First find the array

               SI := Parent (P);

               if Nkind (SI) /= N_Subtype_Indication then
                  goto No_Danger;
               end if;

               T := Entity (Subtype_Mark (SI));

               if not Is_Array_Type (T) then
                  goto No_Danger;
               end if;

               --  Next, find the dimension

               TB := First_Index (T);
               CB := First (Constraints (P));
               while True
                 and then Present (TB)
                 and then Present (CB)
                 and then CB /= PN
               loop
                  Next_Index (TB);
                  Next (CB);
               end loop;

               if CB /= PN then
                  goto No_Danger;
               end if;

               --  Now, check the dimension has a large range

               if not Large_Storage_Type (Etype (TB)) then
                  goto No_Danger;
               end if;

               --  Warn about the danger

               Error_Msg_N
                 ("creation of object of this type may raise Storage_Error?",
                  N);

               <<No_Danger>>
                  null;

            end;
         end if;

      --  Legal case is in index or discriminant constraint

      elsif Nkind (PN) = N_Index_Or_Discriminant_Constraint
        or else Nkind (PN) = N_Discriminant_Association
      then
         if Paren_Count (N) > 0 then
            Error_Msg_N
              ("discriminant in constraint must appear alone",  N);
         end if;

         return;

      --  Otherwise, context is an expression. It should not be within
      --  (i.e. a subexpression of) a constraint for a component.

      else
         D := PN;
         P := Parent (PN);

         while Nkind (P) /= N_Component_Declaration
           and then Nkind (P) /= N_Subtype_Indication
           and then Nkind (P) /= N_Entry_Declaration
         loop
            D := P;
            P := Parent (P);
            exit when No (P);
         end loop;

         --  If the discriminant is used in an expression that is a bound
         --  of a scalar type, an Itype is created and the bounds are attached
         --  to its range,  not to the original subtype indication. Such use
         --  is of course a double fault.

         if (Nkind (P) = N_Subtype_Indication
              and then
                (Nkind (Parent (P)) = N_Component_Declaration
                  or else Nkind (Parent (P)) = N_Derived_Type_Definition)
              and then D = Constraint (P))

         --  The constraint itself may be given by a subtype indication,
         --  rather than by a more common discrete range.

           or else (Nkind (P) = N_Subtype_Indication
             and then Nkind (Parent (P)) = N_Index_Or_Discriminant_Constraint)

           or else Nkind (P) = N_Entry_Declaration
           or else Nkind (D) = N_Defining_Identifier
         then
            Error_Msg_N
              ("discriminant in constraint must appear alone",  N);
         end if;
      end if;
   end Check_Discriminant_Use;

   --------------------------------
   -- Check_For_Visible_Operator --
   --------------------------------

   procedure Check_For_Visible_Operator (N : Node_Id; T : Entity_Id) is
      Orig_Node : Node_Id := Original_Node (N);

   begin
      if Comes_From_Source (Orig_Node)
        and then not In_Open_Scopes (Scope (T))
        and then not Is_Potentially_Use_Visible (T)
        and then not In_Use (T)
        and then not In_Use (Scope (T))
        and then (not Present (Entity (N))
                   or else Ekind (Entity (N)) /= E_Function)
        and then (Nkind (Orig_Node) /= N_Function_Call
                   or else Nkind (Name (Orig_Node)) /= N_Expanded_Name
                   or else Entity (Prefix (Name (Orig_Node))) /= Scope (T))
        and then not In_Instance
      then
         Error_Msg_NE
           ("operator for} is not directly visible!", N, First_Subtype (T));
         Error_Msg_N ("use clause would make operation legal!", N);
      end if;
   end Check_For_Visible_Operator;

   ------------------------------
   -- Check_Infinite_Recursion --
   ------------------------------

   function Check_Infinite_Recursion (N : Node_Id) return Boolean is
      P : Node_Id;
      C : Node_Id;

   begin
      --  Loop moving up tree, quitting if something tells us we are
      --  definitely not in an infinite recursion situation.

      C := N;
      loop
         P := Parent (C);
         exit when Nkind (P) = N_Subprogram_Body;

         if Nkind (P) = N_Or_Else        or else
            Nkind (P) = N_And_Then       or else
            Nkind (P) = N_If_Statement   or else
            Nkind (P) = N_Case_Statement
         then
            return False;

         elsif Nkind (P) = N_Handled_Sequence_Of_Statements
           and then C /= First (Statements (P))
         then
            return False;

         else
            C := P;
         end if;
      end loop;

      Warn_On_Instance := True;
      Error_Msg_N ("possible infinite recursion?", N);
      Error_Msg_N ("\Storage_Error may be raised at run time?", N);
      Warn_On_Instance := False;

      return True;
   end Check_Infinite_Recursion;

   ------------------------------
   -- Check_Parameterless_Call --
   ------------------------------

   procedure Check_Parameterless_Call (N : Node_Id) is
      Nam : Node_Id;

   begin
      if Nkind (N) in N_Has_Etype and then Etype (N) = Any_Type then
         return;
      end if;

      --  Rewrite as call if overloadable entity that is (or could be, in
      --  the overloaded case) a function call. If we know for sure that
      --  the entity is an enumeration literal, we do not rewrite it.

      if (Is_Entity_Name (N)
            and then Is_Overloadable (Entity (N))
            and then (Ekind (Entity (N)) /= E_Enumeration_Literal
                        or else Is_Overloaded (N)))

      --  Rewrite as call if it is an explicit deference of an expression of
      --  a subprogram access type, and the suprogram type is not that of a
      --  procedure or entry.

      or else
        (Nkind (N) = N_Explicit_Dereference
          and then Ekind (Etype (N)) = E_Subprogram_Type
          and then Base_Type (Etype (Etype (N))) /= Standard_Void_Type)

      --  Rewrite as call if it is a selected component which is a function,
      --  this is the case of a call to a protected function (which may be
      --  overloaded with other protected operations).

      or else
        (Nkind (N) = N_Selected_Component
          and then (Ekind (Entity (Selector_Name (N))) = E_Function
            or else ((Ekind (Entity (Selector_Name (N))) = E_Entry
                       or else
                      Ekind (Entity (Selector_Name (N))) = E_Procedure)
              and then Is_Overloaded (Selector_Name (N)))))

      --  If one of the above three conditions is met, rewrite as call.
      --  Apply the rewriting only once.

      then
         if Nkind (Parent (N)) /= N_Function_Call
           or else N /= Name (Parent (N))
         then
            Nam := New_Copy (N);

            --  If overloaded, overload set belongs to new copy.

            Save_Interps (N, Nam);

            --  Change node to parameterless function call (note that the
            --  Parameter_Associations associations field is left set to Empty,
            --  its normal default value since there are no parameters)

            Change_Node (N, N_Function_Call);
            Set_Name (N, Nam);
            Set_Sloc (N, Sloc (Nam));
            Analyze_Call (N);
         end if;

      elsif Nkind (N) = N_Parameter_Association then
         Check_Parameterless_Call (Explicit_Actual_Parameter (N));
      end if;
   end Check_Parameterless_Call;

   -----------------------------
   -- Make_Call_Into_Operator --
   -----------------------------

   procedure Make_Call_Into_Operator
     (N     : Node_Id;
      Typ   : Entity_Id;
      Op_Id : Entity_Id)
   is
      Op_Name   : constant Name_Id := Chars (Op_Id);
      Act1      : Node_Id := First_Actual (N);
      Act2      : Node_Id := Next_Actual (Act1);
      Error     : Boolean := False;
      Is_Binary : constant Boolean := Present (Act2);
      Op_Node   : Node_Id;
      Opnd_Type : Entity_Id;
      Orig_Type : Entity_Id := Empty;
      Pack      : Entity_Id;

      type Kind_Test is access function (E : Entity_Id) return Boolean;

      function Type_In_P (Test : Kind_Test) return Entity_Id;
      --  Find a type of the given class in the package Pack that contains
      --  the operator.

      function Is_Definite_Access_Type (E : Entity_Id) return Boolean;
      --  Determine whether E is an acess type declared by an access decla-
      --  ration, and  not an (anonymous) allocator type.

      function Operand_Type_In_Scope (S : Entity_Id) return Boolean;
      --  If the operand is not universal, and the operator is given by a
      --  expanded name,  verify that the operand has an interpretation with
      --  a type defined in the given scope of the operator.


      -----------------------------
      -- Is_Definite_Access_Type --
      -----------------------------

      function Is_Definite_Access_Type (E : Entity_Id) return Boolean is
         Btyp : constant Entity_Id := Base_Type (E);
      begin
         return Ekind (Btyp) = E_Access_Type
           or else (Ekind (Btyp) = E_Access_Subprogram_Type
                     and then Comes_From_Source (Btyp));
      end Is_Definite_Access_Type;

      ---------------
      -- Type_In_P --
      ---------------

      function Type_In_P (Test : Kind_Test) return Entity_Id is
         E : Entity_Id;

         function In_Decl return Boolean;
         --  Verify that node is not part of the type declaration for the
         --  candidate type, which would otherwise be invisible.

         -------------
         -- In_Decl --
         -------------

         function In_Decl return Boolean is
            Decl_Node : constant Node_Id := Parent (E);
            N2        : Node_Id;

         begin
            N2 := N;

            if Etype (E) = Any_Type then
               return True;

            elsif No (Decl_Node) then
               return False;

            else
               while Present (N2)
                 and then Nkind (N2) /= N_Compilation_Unit
               loop
                  if N2 = Decl_Node then
                     return True;
                  else
                     N2 := Parent (N2);
                  end if;
               end loop;

               return False;
            end if;
         end In_Decl;

      --  Start of processing for Type_In_P

      begin
         --  If the context type is declared in the prefix package, this
         --  is the desired base type.

         if Scope (Base_Type (Typ)) = Pack
           and then Test (Typ)
         then
            return Base_Type (Typ);

         else
            E := First_Entity (Pack);

            while Present (E) loop

               if Test (E)
                 and then not In_Decl
               then
                  return E;
               end if;

               Next_Entity (E);
            end loop;

            return Empty;
         end if;
      end Type_In_P;

      ---------------------------
      -- Operand_Type_In_Scope --
      ---------------------------

      function Operand_Type_In_Scope (S : Entity_Id) return Boolean is
         Nod : constant Node_Id := Right_Opnd (Op_Node);
         I   : Interp_Index;
         It  : Interp;

      begin
         if not Is_Overloaded (Nod) then
            return Scope (Base_Type (Etype (Nod))) = S;

         else
            Get_First_Interp (Nod, I, It);

            while Present (It.Typ) loop

               if Scope (Base_Type (It.Typ)) = S then
                  return True;
               end if;

               Get_Next_Interp (I, It);
            end loop;

            return False;
         end if;
      end Operand_Type_In_Scope;

   --  Start of processing for Make_Call_Into_Operator

   begin
      Op_Node := New_Node (Operator_Kind (Op_Name, Is_Binary), Sloc (N));

      --  Binary operator

      if Is_Binary then
         Set_Left_Opnd  (Op_Node, Relocate_Node (Act1));
         Set_Right_Opnd (Op_Node, Relocate_Node (Act2));
         Save_Interps (Act1, Left_Opnd  (Op_Node));
         Save_Interps (Act2, Right_Opnd (Op_Node));
         Act1 := Left_Opnd (Op_Node);
         Act2 := Right_Opnd (Op_Node);

      --  Unary operator

      else
         Set_Right_Opnd (Op_Node, Relocate_Node (Act1));
         Save_Interps (Act1, Right_Opnd (Op_Node));
         Act1 := Right_Opnd (Op_Node);
      end if;

      --  If the operator is denoted by an expanded name, and the prefix is
      --  not Standard, but the operator is a predefined one whose scope is
      --  Standard, then this is an implicit_operator, inserted as an
      --  interpretation by the procedure of the same name. This procedure
      --  overestimates the presence of implicit operators, because it does
      --  not examine the type of the operands. Verify now that the operand
      --  type appears in the given scope. If right operand is universal,
      --  check the other operand. In the case of concatenation, either
      --  argument can be the component type, so check the type of the result.
      --  If both arguments are literals, look for a type of the right kind
      --  defined in the given scope. This elaborate nonsense is brought to
      --  you courtesy of b33302a. The type itself must be frozen, so we must
      --  find the type of the proper class in the given scope.

      --  A final wrinkle is the multiplication operator for fixed point
      --  types, which is defined in Standard only, and not in the scope of
      --  the fixed_point type itself.

      if Nkind (Name (N)) = N_Expanded_Name then
         Pack := Entity (Prefix (Name (N)));

         --  If the entity being called is defined in the given package,
         --  it is a renaming of a predefined operator, and known to be
         --  legal.

         if Scope (Entity (Name (N))) = Pack
            and then Pack /= Standard_Standard
         then
            null;

         elsif (Op_Name =  Name_Op_Multiply
              or else Op_Name = Name_Op_Divide)
           and then Is_Fixed_Point_Type (Etype (Left_Opnd  (Op_Node)))
           and then Is_Fixed_Point_Type (Etype (Right_Opnd (Op_Node)))
         then
            if Pack /= Standard_Standard then
               Error := True;
            end if;

         else
            Opnd_Type := Base_Type (Etype (Right_Opnd (Op_Node)));

            if Op_Name = Name_Op_Concat then
               Opnd_Type := Base_Type (Typ);

            elsif (Scope (Opnd_Type) = Standard_Standard
                     and then Is_Binary)
              or else (Nkind (Right_Opnd (Op_Node)) = N_Attribute_Reference
                        and then Is_Binary
                        and then not Comes_From_Source (Opnd_Type))
            then
               Opnd_Type := Base_Type (Etype (Left_Opnd (Op_Node)));
            end if;

            if Scope (Opnd_Type) = Standard_Standard then

               --  Verify that the scope contains a type that corresponds to
               --  the given literal. Optimize the case where Pack is Standard.

               if Pack /= Standard_Standard then

                  if Opnd_Type = Universal_Integer then
                     Orig_Type :=  Type_In_P (Is_Integer_Type'Access);

                  elsif Opnd_Type = Universal_Real then
                     Orig_Type := Type_In_P (Is_Real_Type'Access);

                  elsif Opnd_Type = Any_String then
                     Orig_Type := Type_In_P (Is_String_Type'Access);

                  elsif Opnd_Type = Any_Access then
                     Orig_Type :=  Type_In_P (Is_Definite_Access_Type'Access);

                  elsif Opnd_Type = Any_Composite then
                     Orig_Type := Type_In_P (Is_Composite_Type'Access);

                     if Present (Orig_Type) then
                        if Has_Private_Component (Orig_Type) then
                           Orig_Type := Empty;
                        else
                           Set_Etype (Act1, Orig_Type);

                           if Is_Binary then
                              Set_Etype (Act2, Orig_Type);
                           end if;
                        end if;
                     end if;

                  else
                     Orig_Type := Empty;
                  end if;

                  Error := No (Orig_Type);
               end if;

            elsif Ekind (Opnd_Type) = E_Allocator_Type
               and then No (Type_In_P (Is_Definite_Access_Type'Access))
            then
               Error := True;

            --  If the type is defined elsewhere, and the operator is not
            --  defined in the given scope (by a renaming declaration, e.g.)
            --  then this is an error as well. If an extension of System is
            --  present, and the type may be defined there, Pack must be
            --  System itself.

            elsif Scope (Opnd_Type) /= Pack
              and then Scope (Op_Id) /= Pack
              and then (No (System_Aux_Id)
                         or else Scope (Opnd_Type) /= System_Aux_Id
                         or else Pack /= Scope (System_Aux_Id))
            then
               Error := True;

            elsif Pack = Standard_Standard
              and then not Operand_Type_In_Scope (Standard_Standard)
            then
               Error := True;
            end if;
         end if;

         if Error then
            Error_Msg_Node_2 := Pack;
            Error_Msg_NE
              ("& not declared in&", N, Selector_Name (Name (N)));
            Set_Etype (N, Any_Type);
            return;
         end if;
      end if;

      Set_Chars  (Op_Node, Op_Name);
      Set_Etype  (Op_Node, Base_Type (Etype (N)));
      Set_Entity (Op_Node, Op_Id);
      Generate_Reference (Op_Id, N, ' ');
      Rewrite (N,  Op_Node);
      Resolve (N, Typ);

      --  For predefined operators on literals, the operation freezes
      --  their type.

      if Present (Orig_Type) then
         Set_Etype (Act1, Orig_Type);
         Freeze_Expression (Act1);
      end if;
   end Make_Call_Into_Operator;

   -------------------
   -- Operator_Kind --
   -------------------

   function Operator_Kind
     (Op_Name   : Name_Id;
      Is_Binary : Boolean)
      return      Node_Kind
   is
      Kind : Node_Kind;

   begin
      if Is_Binary then
         if    Op_Name =  Name_Op_And      then Kind := N_Op_And;
         elsif Op_Name =  Name_Op_Or       then Kind := N_Op_Or;
         elsif Op_Name =  Name_Op_Xor      then Kind := N_Op_Xor;
         elsif Op_Name =  Name_Op_Eq       then Kind := N_Op_Eq;
         elsif Op_Name =  Name_Op_Ne       then Kind := N_Op_Ne;
         elsif Op_Name =  Name_Op_Lt       then Kind := N_Op_Lt;
         elsif Op_Name =  Name_Op_Le       then Kind := N_Op_Le;
         elsif Op_Name =  Name_Op_Gt       then Kind := N_Op_Gt;
         elsif Op_Name =  Name_Op_Ge       then Kind := N_Op_Ge;
         elsif Op_Name =  Name_Op_Add      then Kind := N_Op_Add;
         elsif Op_Name =  Name_Op_Subtract then Kind := N_Op_Subtract;
         elsif Op_Name =  Name_Op_Concat   then Kind := N_Op_Concat;
         elsif Op_Name =  Name_Op_Multiply then Kind := N_Op_Multiply;
         elsif Op_Name =  Name_Op_Divide   then Kind := N_Op_Divide;
         elsif Op_Name =  Name_Op_Mod      then Kind := N_Op_Mod;
         elsif Op_Name =  Name_Op_Rem      then Kind := N_Op_Rem;
         elsif Op_Name =  Name_Op_Expon    then Kind := N_Op_Expon;
         else
            pragma Assert (False);
            raise Program_Error;
         end if;

      --  Unary operators

      else
         if    Op_Name =  Name_Op_Add      then Kind := N_Op_Plus;
         elsif Op_Name =  Name_Op_Subtract then Kind := N_Op_Minus;
         elsif Op_Name =  Name_Op_Abs      then Kind := N_Op_Abs;
         elsif Op_Name =  Name_Op_Not      then Kind := N_Op_Not;
         else
            pragma Assert (False);
            raise Program_Error;
         end if;
      end if;

      return Kind;
   end Operator_Kind;

   -----------------------------
   -- Pre_Analyze_And_Resolve --
   -----------------------------

   procedure Pre_Analyze_And_Resolve (N : Node_Id; T : Entity_Id) is
      Save_Full_Analysis : constant Boolean := Full_Analysis;

   begin
      Full_Analysis := False;
      Expander_Mode_Save_And_Set (False);

      --  We suppress all checks for this analysis, since the checks will
      --  be applied properly, and in the right location, when the default
      --  expression is reanalyzed and reexpanded later on.

      Analyze_And_Resolve (N, T, Suppress => All_Checks);

      Expander_Mode_Restore;
      Full_Analysis := Save_Full_Analysis;
   end Pre_Analyze_And_Resolve;

   --  Version without context type.

   procedure Pre_Analyze_And_Resolve (N : Node_Id) is
      Save_Full_Analysis : constant Boolean := Full_Analysis;

   begin
      Full_Analysis := False;
      Expander_Mode_Save_And_Set (False);

      Analyze (N);
      Resolve (N, Etype (N), Suppress => All_Checks);

      Expander_Mode_Restore;
      Full_Analysis := Save_Full_Analysis;
   end Pre_Analyze_And_Resolve;
   -------------
   -- Resolve --
   -------------

   procedure Resolve (N : Node_Id; Typ : Entity_Id) is
      I         : Interp_Index;
      I1        : Interp_Index;
      It        : Interp;
      It1       : Interp;
      Found     : Boolean := False;
      Seen      : Entity_Id;
      Ctx_Type  : Entity_Id := Typ;
      Expr_Type : Entity_Id;
      Ambiguous : Boolean := False;

      procedure Patch_Up_Value (N : Node_Id; Typ : Entity_Id);
      --  Try and fix up a literal so that it matches its expected type. New
      --  literals are manufactured if necessary to avoid cascaded errors.

      procedure Resolution_Failed;
      --  Called when attempt at resolving current expression fails

      --------------------
      -- Patch_Up_Value --
      --------------------

      procedure Patch_Up_Value (N : Node_Id; Typ : Entity_Id) is
      begin
         if Nkind (N) = N_Integer_Literal
           and then Is_Real_Type (Typ)
         then
            Rewrite (N,
              Make_Real_Literal (Sloc (N),
                Realval => UR_From_Uint (Intval (N))));
            Set_Etype (N, Universal_Real);
            Set_Is_Static_Expression (N);

         elsif Nkind (N) = N_Real_Literal
           and then Is_Integer_Type (Typ)
         then
            Rewrite (N,
              Make_Integer_Literal (Sloc (N),
                Intval => UR_To_Uint (Realval (N))));
            Set_Etype (N, Universal_Integer);
            Set_Is_Static_Expression (N);
         elsif Nkind (N) = N_String_Literal
           and then Is_Character_Type (Typ)
         then
            Set_Character_Literal_Name (Char_Code (Character'Pos ('A')));
            Rewrite (N,
              Make_Character_Literal (Sloc (N),
                Chars => Name_Find,
                Char_Literal_Value => Char_Code (Character'Pos ('A'))));
            Set_Etype (N, Any_Character);
            Set_Is_Static_Expression (N);

         elsif Nkind (N) /= N_String_Literal
           and then Is_String_Type (Typ)
         then
            Rewrite (N,
              Make_String_Literal (Sloc (N),
                Strval => End_String));

         elsif Nkind (N) = N_Range then
            Patch_Up_Value (Low_Bound (N), Typ);
            Patch_Up_Value (High_Bound (N), Typ);
         end if;
      end Patch_Up_Value;

      -----------------------
      -- Resolution_Failed --
      -----------------------

      procedure Resolution_Failed is
      begin
         Patch_Up_Value (N, Typ);
         Set_Etype (N, Typ);
         Debug_A_Exit ("resolving  ", N, " (done, resolution failed)");
         Set_Is_Overloaded (N, False);

         --  The caller will return without calling the expander, so we need
         --  to set the analyzed flag. Note that it is fine to set Analyzed
         --  to True even if we are in the middle of a shallow analysis,
         --  (see the spec of sem for more details) since this is an error
         --  situation anyway, and there is no point in repeating the
         --  analysis later (indeed it won't work to repeat it later, since
         --  we haven't got a clear resolution of which entity is being
         --  referenced.)

         Set_Analyzed (N, True);
         return;
      end Resolution_Failed;

   --  Start of processing for Resolve

   begin
      --  Access attribute on remote subprogram cannot be used for
      --  a non-remote access-to-subprogram type.

      if Nkind (N) = N_Attribute_Reference
        and then (Attribute_Name (N) = Name_Access
          or else Attribute_Name (N) = Name_Unrestricted_Access
          or else Attribute_Name (N) = Name_Unchecked_Access)
        and then Comes_From_Source (N)
        and then Is_Entity_Name (Prefix (N))
        and then Is_Remote_Call_Interface (Entity (Prefix (N)))
        and then not Is_Remote_Access_To_Subprogram_Type (Typ)
      then
         Error_Msg_N
           ("prefix must statically denote a non-remote subprogram", N);
      end if;

      --  If the context is a Remote_Access_To_Subprogram, access attributes
      --  must be resolved with the corresponding fat pointer. There is no need
      --  to check for the attribute name since the return type of an
      --  attribute is never a remote type.

      if Nkind (N) = N_Attribute_Reference
        and then Comes_From_Source (N)
        and then (Is_Remote_Call_Interface (Typ)
                    or else Is_Remote_Types (Typ))
      then
         declare
            Attr      : constant Attribute_Id :=
                          Get_Attribute_Id (Attribute_Name (N));
            Pref      : constant Node_Id      := Prefix (N);
            Decl      : Node_Id;
            Spec      : Node_Id;
            Is_Remote : Boolean := True;

         begin

            --  Check that Typ is a fat pointer with a reference to a RAS as
            --  original access type.

            if
              (Ekind (Typ) = E_Access_Subprogram_Type
                 and then Present (Equivalent_Type (Typ)))
              or else
              (Ekind (Typ) = E_Record_Type
                 and then Present (Corresponding_Remote_Type (Typ)))

            then
               --  Prefix (N) must statically denote a remote subprogram
               --  declared in a package specification.

               if Attr = Attribute_Access then
                  Decl := Unit_Declaration_Node (Entity (Pref));

                  if Nkind (Decl) = N_Subprogram_Body then
                     Spec := Corresponding_Spec (Decl);

                     if not No (Spec) then
                        Decl := Unit_Declaration_Node (Spec);
                     end if;
                  end if;

                  Spec := Parent (Decl);

                  if not Is_Entity_Name (Prefix (N))
                    or else Nkind (Spec) /= N_Package_Specification
                    or else
                      not Is_Remote_Call_Interface (Defining_Entity (Spec))
                  then
                     Is_Remote := False;
                     Error_Msg_N
                       ("prefix must statically denote a remote subprogram ",
                        N);
                  end if;
               end if;

               if Attr = Attribute_Access
                 or else Attr = Attribute_Unchecked_Access
                 or else Attr = Attribute_Unrestricted_Access
               then
                  Check_Subtype_Conformant
                    (New_Id  => Entity (Prefix (N)),
                     Old_Id  => Designated_Type
                       (Corresponding_Remote_Type (Typ)),
                     Err_Loc => N);
                  if Is_Remote then
                     Process_Remote_AST_Attribute (N, Typ);
                  end if;
               end if;
            end if;
         end;
      end if;

      Debug_A_Entry ("resolving  ", N);

      if Is_Fixed_Point_Type (Typ) then
         Check_Restriction (No_Fixed_Point, N);

      elsif Is_Floating_Point_Type (Typ)
        and then Typ /= Universal_Real
        and then Typ /= Any_Real
      then
         Check_Restriction (No_Floating_Point, N);
      end if;

      --  Return if already analyzed

      if Analyzed (N) then
         Debug_A_Exit ("resolving  ", N, "  (done, already analyzed)");
         return;

      --  Return if type = Any_Type (previous error encountered)

      elsif Etype (N) = Any_Type then
         Debug_A_Exit ("resolving  ", N, "  (done, Etype = Any_Type)");
         return;
      end if;

      Check_Parameterless_Call (N);

      --  If not overloaded, then we know the type, and all that needs doing
      --  is to check that this type is compatible with the context.

      if not Is_Overloaded (N) then
         Found := Covers (Typ, Etype (N));
         Expr_Type := Etype (N);

      --  In the overloaded case, we must select the interpretation that
      --  is compatible with the context (i.e. the type passed to Resolve)

      else
         Get_First_Interp (N, I, It);

         --  Loop through possible interpretations

         Interp_Loop : while Present (It.Typ) loop

            --  We are only interested in interpretations that are compatible
            --  with the expected type, any other interpretations are ignored

            if Covers (Typ, It.Typ) then

               --  First matching interpretation

               if not Found then
                  Found := True;
                  I1    := I;
                  Seen  := It.Nam;
                  Expr_Type := It.Typ;

               --  Matching intepretation that is not the first, maybe an
               --  error, but there are some cases where preference rules are
               --  used to choose between the two possibilities. These and
               --  some more obscure cases are handled in Disambiguate.

               else
                  Error_Msg_Sloc := Sloc (Seen);
                  It1 := Disambiguate (N, I1, I, Typ);

                  if It1 = No_Interp then

                     --  Before we issue an ambiguity complaint, check for
                     --  the case of a subprogram call where at least one
                     --  of the arguments is Any_Type, and if so, suppress
                     --  the message, since it is a cascaded error.

                     if Nkind (N) = N_Function_Call
                       or else Nkind (N) = N_Procedure_Call_Statement
                     then
                        declare
                           A : Node_Id := First_Actual (N);
                           E : Node_Id;

                        begin
                           while Present (A) loop
                              E := A;

                              if Nkind (E) = N_Parameter_Association then
                                 E := Explicit_Actual_Parameter (E);
                              end if;

                              if Etype (E) = Any_Type then
                                 if Debug_Flag_V then
                                    Write_Str ("Any_Type in call");
                                    Write_Eol;
                                 end if;

                                 exit Interp_Loop;
                              end if;

                              Next_Actual (A);
                           end loop;
                        end;

                     elsif Nkind (N) in  N_Binary_Op
                       and then (Etype (Left_Opnd (N)) = Any_Type
                                  or else Etype (Right_Opnd (N)) = Any_Type)
                     then
                        exit Interp_Loop;

                     elsif Nkind (N) in  N_Unary_Op
                       and then Etype (Right_Opnd (N)) = Any_Type
                     then
                        exit Interp_Loop;
                     end if;

                     --  Not that special case, so issue message using the
                     --  flag Ambiguous to control printing of the header
                     --  message only at the start of an ambiguous set.

                     if not Ambiguous then
                        Error_Msg_NE
                          ("ambiguous expression (cannot resolve&)!",
                           N, It.Nam);
                        Error_Msg_N
                          ("possible interpretation#!", N);
                        Ambiguous := True;
                     end if;

                     Error_Msg_Sloc := Sloc (It.Nam);
                     Error_Msg_N ("possible interpretation#!", N);

                  --  Disambiguation has succeeded. Skip the remaining
                  --  interpretations.
                  else
                     Seen := It1.Nam;
                     Expr_Type := It1.Typ;

                     while Present (It.Typ) loop
                        Get_Next_Interp (I, It);
                     end loop;
                  end if;
               end if;

               --  We have a matching interpretation, Expr_Type is the
               --  type from this interpretation, and Seen is the entity.

               --  For an operator, just set the entity name. The type will
               --  be set by the specific operator resolution routine.

               if Nkind (N) in N_Op then
                  Set_Entity (N, Seen);
                  Generate_Reference (Seen, N);

               elsif Nkind (N) = N_Character_Literal then
                  Set_Etype (N, Expr_Type);

               --  For an explicit dereference, attribute reference, range,
               --  short-circuit form (which is not an operator node),
               --  or a call with a name that is an explicit dereference,
               --  there is nothing to be done at this point.

               elsif     Nkind (N) = N_Explicit_Dereference
                 or else Nkind (N) = N_Attribute_Reference
                 or else Nkind (N) = N_And_Then
                 or else Nkind (N) = N_Indexed_Component
                 or else Nkind (N) = N_Or_Else
                 or else Nkind (N) = N_Range
                 or else Nkind (N) = N_Selected_Component
                 or else Nkind (N) = N_Slice
                 or else Nkind (Name (N)) = N_Explicit_Dereference
               then
                  null;

               --  For procedure or function calls, set the type of the
               --  name, and also the entity pointer for the prefix

               elsif (Nkind (N) = N_Procedure_Call_Statement
                       or else Nkind (N) = N_Function_Call)
                 and then (Is_Entity_Name (Name (N))
                            or else Nkind (Name (N)) = N_Operator_Symbol)
               then
                  Set_Etype  (Name (N), Expr_Type);
                  Set_Entity (Name (N), Seen);
                  Generate_Reference (Seen, Name (N));

               elsif Nkind (N) = N_Function_Call
                 and then Nkind (Name (N)) = N_Selected_Component
               then
                  Set_Etype (Name (N), Expr_Type);
                  Set_Entity (Selector_Name (Name (N)), Seen);
                  Generate_Reference (Seen, Selector_Name (Name (N)));

               --  For all other cases, just set the type of the Name

               else
                  Set_Etype (Name (N), Expr_Type);
               end if;

            --  Here if interpetation is incompatible with context type

            else
               if Debug_Flag_V then
                  Write_Str ("    intepretation incompatible with context");
                  Write_Eol;
               end if;
            end if;

            --  Move to next interpretation

            if Present (It.Typ) then
               Get_Next_Interp (I, It);
            else
               exit;
            end if;
         end loop Interp_Loop;
      end if;

      --  At this stage Found indicates whether or not an acceptable
      --  interpretation exists. If not, then we have an error, except
      --  that if the context is Any_Type as a result of some other error,
      --  then we suppress the error report.

      if not Found then
         if Typ /= Any_Type then

            --  If type we are looking for is Void, then this is the
            --  procedure call case, and the error is simply that what
            --  we gave is not a procedure name (we think of procedure
            --  calls as expressions with types internally, but the user
            --  doesn't think of them this way!)

            if Typ = Standard_Void_Type then
               Error_Msg_N ("expect procedure name in procedure call", N);
               Found := True;

            --  Otherwise we do have a subexpression with the wrong type

            --  Check for the case of an allocator which uses an access
            --  type instead of the designated type. This is a common
            --  error and we specialize the message, posting an error
            --  on the operand of the allocator, complaining that we
            --  expected the designated type of the allocator.

            elsif Nkind (N) = N_Allocator
              and then Ekind (Typ) in Access_Kind
              and then Ekind (Etype (N)) in Access_Kind
              and then Designated_Type (Etype (N)) = Typ
            then
               Wrong_Type (Expression (N), Designated_Type (Typ));
               Found := True;

            --  Check for an aggregate. Sometimes we can get bogus
            --  aggregates from misuse of parentheses, and we are
            --  about to complain about the aggregate without even
            --  looking inside it.

            --  Instead, if we have an aggregate of type Any_Composite,
            --  then analyze and resolve the component fields, and then
            --  only issue another message if we get no errors doing
            --  this (otherwise assume that the errors in the aggregate
            --  caused the problem).

            elsif Nkind (N) = N_Aggregate
              and then Etype (N) = Any_Composite
            then

               --  Disable expansion in any case. If there is a type mismatch
               --  it may be fatal to try to expand the aggregate. The flag
               --  would otherwise be set to false when the error is posted.

               Expander_Active := False;

               declare
                  procedure Check_Aggr (Aggr : Node_Id);
                  --  Check one aggregate, and set Found to True if we
                  --  have a definite error in any of its elements

                  procedure Check_Elmt (Aelmt : Node_Id);
                  --  Check one element of aggregate and set Found to
                  --  True if we definitely have an error in the element.

                  procedure Check_Aggr (Aggr : Node_Id) is
                     Elmt : Node_Id;

                  begin
                     if Present (Expressions (Aggr)) then
                        Elmt := First (Expressions (Aggr));
                        while Present (Elmt) loop
                           Check_Elmt (Elmt);
                           Next (Elmt);
                        end loop;
                     end if;

                     if Present (Component_Associations (Aggr)) then
                        Elmt := First (Component_Associations (Aggr));
                        while Present (Elmt) loop
                           Check_Elmt (Expression (Elmt));
                           Next (Elmt);
                        end loop;
                     end if;
                  end Check_Aggr;

                  procedure Check_Elmt (Aelmt : Node_Id) is
                  begin
                     --  If we have a nested aggregate, go inside it (to
                     --  attempt a naked analyze-resolve of the aggregate
                     --  can cause undesirable cascaded errors). Do not
                     --  resolve expression if it needs a type from context,
                     --  as for integer * fixed expression.

                     if Nkind (Aelmt) = N_Aggregate then
                        Check_Aggr (Aelmt);

                     else
                        Analyze (Aelmt);

                        if not Is_Overloaded (Aelmt)
                          and then Etype (Aelmt) /= Any_Fixed
                        then
                           Resolve (Aelmt, Etype (Aelmt));
                        end if;

                        if Etype (Aelmt) = Any_Type then
                           Found := True;
                        end if;
                     end if;
                  end Check_Elmt;

               begin
                  Check_Aggr (N);
               end;
            end if;

            --  If an error message was issued already, Found got reset
            --  to True, so if it is still False, issue the standard
            --  Wrong_Type message.

            if not Found then
               Wrong_Type (N, Typ);
            end if;
         end if;

         Resolution_Failed;
         return;

      --  Test if we have more than one interpretation for the context

      elsif Ambiguous then
         Resolution_Failed;
         return;

      --  Here we have an acceptable interpretation for the context

      else
         --  A user-defined operator is tranformed into a function call at
         --  this point, so that further processing knows that operators are
         --  really operators (i.e. are predefined operators). User-defined
         --  operators that are intrinsic are just renamings of the predefined
         --  ones, and need not be turned into calls either, but if they rename
         --  a different operator, we must transform the node accordingly.
         --  Instantiations of Unchecked_Conversion are intrinsic but are
         --  treated as functions, even if given an operator designator.

         if Nkind (N) in N_Op
           and then Present (Entity (N))
           and then Ekind (Entity (N)) /= E_Operator
         then

            if not Is_Intrinsic_Subprogram (Entity (N))
              or else Is_Generic_Instance (Entity (N))
            then
               Rewrite_Operator_As_Call (N, Entity (N));

            elsif Present (Alias (Entity (N))) then

               if Chars (Alias (Entity (N))) in  Any_Operator_Name then
                  Rewrite_Renamed_Operator (N, Alias (Entity (N)));
               else
                  Rewrite_Operator_As_Call (N, Alias (Entity (N)));
               end if;
            end if;
         end if;

         --  Propagate type information and normalize tree for various
         --  predefined operations. If the context only imposes a class of
         --  types, rather than a specific type, propagate the actual type
         --  downward.

         if Typ = Any_Integer
           or else Typ = Any_Boolean
           or else Typ = Any_Modular
           or else Typ = Any_Real
           or else Typ = Any_Discrete
         then
            Ctx_Type := Expr_Type;

            --  Any_Fixed is legal in a real context only if a specific
            --  fixed point type is imposed. If Norman Cohen can be
            --  confused by this, it deserves a separate message.

            if Typ = Any_Real
              and then Expr_Type = Any_Fixed
            then
               Error_Msg_N ("Illegal context for mixed mode operation", N);
               Set_Etype (N, Universal_Real);
               Ctx_Type := Universal_Real;
            end if;
         end if;

         case N_Subexpr'(Nkind (N)) is

            when N_Aggregate => Resolve_Aggregate                (N, Ctx_Type);

            when N_Allocator => Resolve_Allocator                (N, Ctx_Type);

            when N_And_Then | N_Or_Else
                             => Resolve_Short_Circuit            (N, Ctx_Type);

            when N_Attribute_Reference
                             => Resolve_Attribute                (N, Ctx_Type);

            when N_Character_Literal
                             => Resolve_Character_Literal        (N, Ctx_Type);

            when N_Conditional_Expression
                             => Resolve_Conditional_Expression   (N, Ctx_Type);

            when N_Expanded_Name
                             => Resolve_Entity_Name              (N, Ctx_Type);

            when N_Extension_Aggregate
                             => Resolve_Extension_Aggregate      (N, Ctx_Type);

            when N_Explicit_Dereference
                             => Resolve_Explicit_Dereference     (N, Ctx_Type);

            when N_Function_Call
                             => Resolve_Call                     (N, Ctx_Type);

            when N_Identifier
                             => Resolve_Entity_Name              (N, Ctx_Type);

            when N_In | N_Not_In
                             => Resolve_Membership_Op            (N, Ctx_Type);

            when N_Indexed_Component
                             => Resolve_Indexed_Component        (N, Ctx_Type);

            when N_Integer_Literal
                             => Resolve_Integer_Literal          (N, Ctx_Type);

            when N_Null      => Resolve_Null                     (N, Ctx_Type);

            when N_Op_And | N_Op_Or | N_Op_Xor
                             => Resolve_Logical_Op               (N, Ctx_Type);

            when N_Op_Eq | N_Op_Ne
                             => Resolve_Equality_Op              (N, Ctx_Type);

            when N_Op_Lt | N_Op_Le | N_Op_Gt | N_Op_Ge
                             => Resolve_Comparison_Op            (N, Ctx_Type);

            when N_Op_Not    => Resolve_Op_Not                   (N, Ctx_Type);

            when N_Op_Add    | N_Op_Subtract | N_Op_Multiply |
                 N_Op_Divide | N_Op_Mod      | N_Op_Rem

                             => Resolve_Arithmetic_Op            (N, Ctx_Type);

            when N_Op_Concat => Resolve_Op_Concat                (N, Ctx_Type);

            when N_Op_Expon  => Resolve_Op_Expon                 (N, Ctx_Type);

            when N_Op_Plus | N_Op_Minus  | N_Op_Abs
                             => Resolve_Unary_Op                 (N, Ctx_Type);

            when N_Op_Shift  => Resolve_Shift                    (N, Ctx_Type);

            when N_Procedure_Call_Statement
                             => Resolve_Call                     (N, Ctx_Type);

            when N_Operator_Symbol
                             => Resolve_Operator_Symbol          (N, Ctx_Type);

            when N_Qualified_Expression
                             => Resolve_Qualified_Expression     (N, Ctx_Type);

            when N_Raise_xxx_Error
                             => Set_Etype (N, Ctx_Type);

            when N_Range     => Resolve_Range                    (N, Ctx_Type);

            when N_Real_Literal
                             => Resolve_Real_Literal             (N, Ctx_Type);

            when N_Reference => Resolve_Reference                (N, Ctx_Type);

            when N_Selected_Component
                             => Resolve_Selected_Component       (N, Ctx_Type);

            when N_Slice     => Resolve_Slice                    (N, Ctx_Type);

            when N_String_Literal
                             => Resolve_String_Literal           (N, Ctx_Type);

            when N_Subprogram_Info
                             => Resolve_Subprogram_Info          (N, Ctx_Type);

            when N_Type_Conversion
                             => Resolve_Type_Conversion          (N, Ctx_Type);

            when N_Unchecked_Expression =>
               Resolve_Unchecked_Expression                      (N, Ctx_Type);

            when N_Unchecked_Type_Conversion =>
               Resolve_Unchecked_Type_Conversion                 (N, Ctx_Type);

         end case;

         --  If the subexpression was replaced by a non-subexpression, then
         --  all we do is to expand it. The only legitimate case we know of
         --  is converting procedure call statement to entry call statements,
         --  but there may be others, so at least temporarily, we are making
         --  this test general since it was general in previous versions ???

         if Nkind (N) not in N_Subexpr then
            Debug_A_Exit ("resolving  ", N, "  (done)");
            Expand (N);
            return;
         end if;

         --  The expression is definitely NOT overloaded at this point, so
         --  we reset the Is_Overloaded flag to avoid any confusion when
         --  reanalyzing the node.

         Set_Is_Overloaded (N, False);

         --  Freeze expression type, entity if it is a name, and designated
         --  type if it is an allocator (RM 13.14(9,10)).

         --  Now that the resolution of the type of the node is complete,
         --  and we did not detect an error, we can expand this node. We
         --  skip the expand call if we are in a default expression, see
         --  section "Handling of Default Expressions" in Sem spec.

         Debug_A_Exit ("resolving  ", N, "  (done)");

         --  We unconditionally freeze the expression, even if we are in
         --  default expression mode (the Freeze_Expression routine tests
         --  this flag and only freezes static types if it is set).

         Freeze_Expression (N);

         Expand (N);
      end if;

   end Resolve;

   --  Version with check(s) suppressed

   procedure Resolve (N : Node_Id; Typ : Entity_Id; Suppress : Check_Id) is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Record := Scope_Suppress;

         begin
            Scope_Suppress := (others => True);
            Resolve (N, Typ);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Get_Scope_Suppress (Suppress);

         begin
            Set_Scope_Suppress (Suppress, True);
            Resolve (N, Typ);
            Set_Scope_Suppress (Suppress, Svg);
         end;
      end if;
   end Resolve;

   ---------------------
   -- Resolve_Actuals --
   ---------------------

   procedure Resolve_Actuals (N : Node_Id; Nam : Entity_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      A     : Node_Id;
      F     : Entity_Id;
      A_Typ : Entity_Id;
      F_Typ : Entity_Id;
      Prev  : Node_Id := Empty;

      procedure Insert_Default;
      --  If the actual is missing in a call, insert in the actuals list
      --  an instance of the default expression. The insertion is always
      --  a named association.

      --------------------
      -- Insert_Default --
      --------------------

      procedure Insert_Default is
         Actval : Node_Id;
         Assoc  : Node_Id;
         Defun  : constant Entity_Id := Default_Expr_Function (F);

      begin
         --  Note that the default expression is a simple identifier (which is
         --  a function call) or a literal. In either case we can just copy the
         --  node to use as the default expression. If the original expression
         --  was not a literal, a thunk function was created, and the default
         --  expression is simply a call to this function.

         --  If we created a default expression function, then the default
         --  expression that we need is simply a call to this function.

         if Present (Defun) then
            Actval :=
              Make_Function_Call (Loc,
                Name => New_Occurrence_Of (Defun, Loc));

            if Is_Concurrent_Type (Scope (Nam))
              and then Has_Discriminants (Scope (Nam))
            then
               --  Call to protected operation or task entry, where
               --  the enclosing task has discriminants. In this case
               --  the default function has an additional parameter, which
               --  is the target of the call.

               declare
                  Tsk : Node_Id := Empty;

               begin
                  if Nkind (Name (N)) = N_Selected_Component then
                     Tsk := Prefix (Name (N));

                  elsif Nkind (Name (N)) = N_Indexed_Component then
                     Tsk := Prefix (Prefix (Name (N)));
                  end if;

                  if Present (Tsk) then
                     Set_Parameter_Associations (Actval,
                        New_List (New_Copy_Tree (Tsk)));
                  end if;
               end;
            end if;

            --  Analyze and resolve the function call. Note that the type
            --  of the function may be different from that of the formal, if
            --  this is a call to a inherited subprogram. We make the required
            --  conversion explicit, because the call may be renalyzed before
            --  full expansion, e.g. when stack checking is enabled. For
            --  tagged types the default is tag-indeterminate and dispatches.

            if Is_Overloadable (Nam)
              and then Present (Alias (Nam))
              and then Base_Type (Etype (F)) /= Base_Type (Etype (Defun))
              and then not Is_Tagged_Type (Etype (F))
            then
               Actval := Unchecked_Convert_To (Etype (F), Actval);
               Set_Parent (Actval, N);
               Analyze_And_Resolve (Actval, Etype (F));

            else
               Set_Parent (Actval, N);
               Analyze_And_Resolve (Actval, Etype (Defun));
            end if;

            --  If the original default is a function call, it may be
            --  tag-indeterminate, in which case the tag must be propagated
            --  from the current call.

            if Is_Controlling_Formal (F)
              and then Nkind (Default_Value (F)) = N_Function_Call
            then
               Set_Is_Controlling_Actual (Actval);
            end if;

         --  Otherwise, the default expression must be a simple literal
         --  or identifier (referencing a variable or a constant), and the
         --  default expression is simply a copy of this item. The default
         --  function is also absent if we are not generating code, in which
         --  case we also copy the original expression.

         --  Note that we do a full New_Copy_Tree, so that any associated
         --  Itypes are properly copied. This may not be needed any more,
         --  but it does no harm as a safety measure! Note also that we
         --  do not reresolve the default expression since it is already
         --  resolved, and it would be wrong to reresolve it in the derived
         --  subprogram case anyway. However, the defaults of a generic
         --  formal may be out of bounds of the corresponding actual (see
         --  cc1311b) and an additional check may be required.

         --  If the subprogram has no spec and this is a recursive call within,
         --  the default functions have not been created yet. If it is a
         --  compilation unit, there is no place for the definition of the
         --  default functions, and the expressions have not been fully
         --  resolved, so in both these cases we do it now. Neither can be a
         --  primitive operation so this cannot be a call to a derived op.

         elsif Present (Default_Value (F)) then

            Actval := New_Copy_Tree (Default_Value (F),
                                     New_Scope => Current_Scope);

            --  For an indirect call, nothing special to do.

            if not Is_Overloadable (Nam) then
               null;

            elsif Present (Alias (Nam)) then
               if Base_Type (Etype (F)) /= Base_Type (Etype (Actval))
                 and then not Is_Tagged_Type (Etype (F))
               then
                  Actval := Unchecked_Convert_To (Etype (F), Actval);
               end if;

               if Is_Scalar_Type (Etype (F)) then
                  Enable_Range_Check (Actval);
               end if;

            elsif Nkind (Unit_Declaration_Node (Nam)) = N_Subprogram_Body
              or else Nkind (Parent (Unit_Declaration_Node (Nam)))
                = N_Compilation_Unit
            then
               Set_Parent (Actval, N);
               Resolve (Actval, Etype (F));
            end if;

         else
            --  Missing argument in call, nothing to insert.
            return;
         end if;

         --  If the default expression raises constraint error, then just
         --  silently replace it with an N_Raise_Constraint_Error node,
         --  since we already gave the warning on the subprogram spec.

         if Raises_Constraint_Error (Actval) then
            Rewrite (Actval,
              Make_Raise_Constraint_Error (Loc));
            Set_Raises_Constraint_Error (Actval);
            Set_Etype (Actval, Etype (F));
         end if;

         Assoc :=
           Make_Parameter_Association (Loc,
             Explicit_Actual_Parameter => Actval,
             Selector_Name => Make_Identifier (Loc, Chars (F)));

         --  Case of insertion is first named actual

         if No (Prev) or else
            Nkind (Parent (Prev)) /= N_Parameter_Association
         then
            Set_Next_Named_Actual (Assoc, First_Named_Actual (N));
            Set_First_Named_Actual (N, Actval);

            if No (Prev) then
               if not Present (Parameter_Associations (N)) then
                  Set_Parameter_Associations (N, New_List (Assoc));
               else
                  Append (Assoc, Parameter_Associations (N));
               end if;

            else
               Insert_After (Prev, Assoc);
            end if;

         --  Case of insertion is not first named actual

         else
            Set_Next_Named_Actual
              (Assoc, Next_Named_Actual (Parent (Prev)));
            Set_Next_Named_Actual (Parent (Prev), Actval);
            Append (Assoc, Parameter_Associations (N));
         end if;

         Mark_Rewrite_Insertion (Assoc);
         Mark_Rewrite_Insertion (Actval);

         Prev := Actval;
      end Insert_Default;

   --  Start of processing for Resolve_Actuals

   begin
      A := First_Actual (N);
      F := First_Formal (Nam);

      while Present (F) loop

         if Present (A)
           and then (Nkind (Parent (A)) /= N_Parameter_Association
                       or else
                     Chars (Selector_Name (Parent (A))) = Chars (F))
         then
            --  If the formal is Out or In_Out, do not resolve and expand the
            --  conversion, because it is subsequently expanded into explicit
            --  temporaries and assignments. However, the object of the
            --  conversion can be resolved. An exception is the case of
            --  a tagged type conversion with a class-wide actual. In that
            --  case we want the tag check to occur and no temporary will
            --  will be needed (no representation change can occur) and
            --  the parameter is passed by reference, so we go ahead and
            --  resolve the type conversion.

            if (Ekind (F) = E_In_Out_Parameter
                  or else
                Ekind (F) = E_Out_Parameter)
              and then Nkind (A) = N_Type_Conversion
              and then not Is_Class_Wide_Type (Etype (Expression (A)))
            then
               if Conversion_OK (A)
                 or else Valid_Conversion (A, Etype (A), Expression (A))
               then
                  Resolve (Expression (A), Etype (Expression (A)));
               end if;

            else
               Resolve (A, Etype (F));
            end if;

            A_Typ := Etype (A);
            F_Typ := Etype (F);

            if Ekind (F) /= E_In_Parameter
              and then not Is_OK_Variable_For_Out_Formal (A)
            then
               --  Specialize error message for protected procedure call
               --  within function call of the same protected object.

               if Is_Entity_Name (A)
                 and then Chars (Entity (A)) = Name_uObject
                 and then Ekind (Current_Scope) = E_Function
                 and then Convention (Current_Scope) = Convention_Protected
                 and then Ekind (Nam) /= E_Function
               then
                  Error_Msg_N ("within protected function, protected " &
                    "object is constant", A);
                  Error_Msg_N ("\cannot call operation that may modify it", A);
               else
                  Error_Msg_NE ("actual for& must be a variable", A, F);
               end if;
            end if;

            if Ekind (F) /= E_Out_Parameter then
               Check_Unset_Reference (A);

               if Ada_83
                 and then Is_Entity_Name (A)
                 and then Ekind (Entity (A)) = E_Out_Parameter
               then
                  Error_Msg_N ("(Ada 83) illegal reading of out parameter", A);
               end if;
            end if;

            --  Apply appropriate range checks for in, out, and in-out
            --  parameters. Out and in-out parameters also need a separate
            --  check, if there is a type conversion, to make sure the return
            --  value meets the constraints of the variable before the
            --  conversion.

            --  Gigi looks at the check flag and uses the appropriate types.
            --  For now since one flag is used there is an optimization which
            --  might not be done in the In Out case since Gigi does not do
            --  any analysis. More thought required about this ???

            if Ekind (F) = E_In_Parameter
              or else Ekind (F) = E_In_Out_Parameter
            then
               if Is_Scalar_Type (Etype (A)) then
                  Apply_Scalar_Range_Check (A, F_Typ);

               elsif Is_Array_Type (Etype (A)) then
                  Apply_Length_Check (A, F_Typ);

               elsif Is_Record_Type (F_Typ)
                 and then Has_Discriminants (F_Typ)
                 and then Is_Constrained (F_Typ)
                 and then (not Is_Derived_Type (F_Typ)
                             or else Comes_From_Source (Nam))
               then
                  Apply_Discriminant_Check (A, F_Typ);

               elsif Is_Access_Type (F_Typ)
                 and then Is_Array_Type (Designated_Type (F_Typ))
                 and then Is_Constrained (Designated_Type (F_Typ))
               then
                  Apply_Length_Check (A, F_Typ);

               elsif Is_Access_Type (F_Typ)
                 and then Has_Discriminants (Designated_Type (F_Typ))
                 and then Is_Constrained (Designated_Type (F_Typ))
               then
                  Apply_Discriminant_Check (A, F_Typ);

               else
                  Apply_Range_Check (A, F_Typ);
               end if;
            end if;

            if Ekind (F) = E_Out_Parameter
              or else Ekind (F) = E_In_Out_Parameter
            then

               if Nkind (A) = N_Type_Conversion then
                  if Is_Scalar_Type (A_Typ) then
                     Apply_Scalar_Range_Check
                       (Expression (A), Etype (Expression (A)), A_Typ);
                  else
                     Apply_Range_Check
                       (Expression (A), Etype (Expression (A)), A_Typ);
                  end if;

               else
                  if Is_Scalar_Type (F_Typ) then
                     Apply_Scalar_Range_Check (A, A_Typ, F_Typ);

                  elsif Is_Array_Type (F_Typ)
                    and then Ekind (F) = E_Out_Parameter
                  then
                     Apply_Length_Check (A, F_Typ);

                  else
                     Apply_Range_Check (A, A_Typ, F_Typ);
                  end if;
               end if;
            end if;

            --  An actual associated with an access parameter is implicitly
            --  converted to the anonymous access type of the formal and
            --  must satisfy the legality checks for access conversions.

            if Ekind (F_Typ) = E_Anonymous_Access_Type then
               if not Valid_Conversion (A, F_Typ, A) then
                  Error_Msg_N
                    ("invalid implicit conversion for access parameter", A);
               end if;
            end if;

            --  Check bad case of atomic/volatile argument (RM C.6(12))

            if Is_By_Reference_Type (Etype (F))
              and then Comes_From_Source (N)
            then
               if Is_Atomic_Object (A)
                 and then not Is_Atomic (Etype (F))
               then
                  Error_Msg_N
                    ("cannot pass atomic argument to non-atomic formal",
                     N);

               elsif Is_Volatile_Object (A)
                 and then not Is_Volatile (Etype (F))
               then
                  Error_Msg_N
                    ("cannot pass volatile argument to non-volatile formal",
                     N);
               end if;
            end if;

            --  Check that subprograms don't have improper controlling
            --  arguments (RM 3.9.2 (9))

            if Is_Controlling_Formal (F) then
               Set_Is_Controlling_Actual (A);
            elsif Nkind (A) = N_Explicit_Dereference then
               Validate_Remote_Access_To_Class_Wide_Type (A);
            end if;

            if (Is_Class_Wide_Type (A_Typ) or else Is_Dynamically_Tagged (A))
              and then not Is_Class_Wide_Type (F_Typ)
              and then not Is_Controlling_Formal (F)
            then
               Error_Msg_N ("class-wide argument not allowed here!", A);
               if Is_Subprogram (Nam) then
                  Error_Msg_Node_2 := F_Typ;
                  Error_Msg_NE
                    ("& is not a primitive operation of &!", A, Nam);
               end if;

            elsif Is_Access_Type (A_Typ)
              and then Is_Access_Type (F_Typ)
              and then (Is_Class_Wide_Type (Designated_Type (A_Typ))
                or else (Nkind (A) = N_Attribute_Reference
                          and then Is_Class_Wide_Type (Etype (Prefix (A)))))
              and then not Is_Class_Wide_Type (Designated_Type (F_Typ))
              and then not Is_Controlling_Formal (F)
            then
               Error_Msg_N
                 ("access to class-wide argument not allowed here!", A);
               if Is_Subprogram (Nam) then
                  Error_Msg_Node_2 := Designated_Type (F_Typ);
                  Error_Msg_NE
                    ("& is not a primitive operation of &!", A, Nam);
               end if;
            end if;

            Eval_Actual (A);

            --  If it is a named association, treat the selector_name as
            --  a proper identifier, and mark the corresponding entity.

            if Nkind (Parent (A)) = N_Parameter_Association then
               Set_Entity (Selector_Name (Parent (A)), F);
               Generate_Reference (F, Selector_Name (Parent (A)));
               Set_Etype (Selector_Name (Parent (A)), F_Typ);
               Generate_Reference (F_Typ, N, ' ');
            end if;

            Prev := A;
            Next_Actual (A);

         else
            Insert_Default;
         end if;

         Next_Formal (F);
      end loop;

   end Resolve_Actuals;

   -----------------------
   -- Resolve_Allocator --
   -----------------------

   procedure Resolve_Allocator (N : Node_Id; Typ : Entity_Id) is
      E        : constant Node_Id := Expression (N);
      Subtyp   : Entity_Id;
      Discrim  : Entity_Id;
      Constr   : Node_Id;
      Disc_Exp : Node_Id;

   begin
      --  Replace general access with specific type

      if Ekind (Etype (N)) = E_Allocator_Type then
         Set_Etype (N, Base_Type (Typ));
      end if;

      if Is_Abstract (Typ) then
         Error_Msg_N ("type of allocator cannot be abstract",  N);
      end if;

      --  For qualified expression, resolve the expression using the
      --  given subtype (nothing to do for type mark, subtype indication)

      if Nkind (E) = N_Qualified_Expression then
         if Is_Class_Wide_Type (Etype (E))
           and then not Is_Class_Wide_Type (Designated_Type (Typ))
         then
            Error_Msg_N
              ("class-wide allocator not allowed for this access type", N);
         end if;

         Resolve (Expression (E), Etype (E));
         Check_Unset_Reference (Expression (E));

      --  For a subtype mark or subtype indication, freeze the subtype

      else
         Freeze_Expression (E);

         if Is_Access_Constant (Typ) then
            Error_Msg_N
              ("initialization required for access-to-constant allocator", N);
         end if;

         --  A special accessibility check is needed for allocators that
         --  constrain access discriminants. The level of the type of the
         --  expression used to contrain an access discriminant cannot be
         --  deeper than the type of the allocator (in constrast to access
         --  parameters, where the level of the actual can be arbitrary).
         --  We can't use Valid_Conversion to perform this check because
         --  in general the type of the allocator is unrelated to the type
         --  of the access discriminant. Note that specialized checks are
         --  needed for the cases of a constraint expression which is an
         --  access attribute or an access discriminant.

         if Nkind (Original_Node (E)) = N_Subtype_Indication
           and then Ekind (Typ) /= E_Anonymous_Access_Type
         then
            Subtyp := Entity (Subtype_Mark (Original_Node (E)));

            if Has_Discriminants (Subtyp) then
               Discrim := First_Discriminant (Base_Type (Subtyp));
               Constr := First (Constraints (Constraint (Original_Node (E))));

               while Present (Discrim) and then Present (Constr) loop
                  if Ekind (Etype (Discrim)) = E_Anonymous_Access_Type then
                     if Nkind (Constr) = N_Discriminant_Association then
                        Disc_Exp := Original_Node (Expression (Constr));
                     else
                        Disc_Exp := Original_Node (Constr);
                     end if;

                     if Type_Access_Level (Etype (Disc_Exp))
                       > Type_Access_Level (Typ)
                     then
                        Error_Msg_N
                          ("operand type has deeper level than allocator type",
                           Disc_Exp);

                     elsif Nkind (Disc_Exp) = N_Attribute_Reference
                       and then Get_Attribute_Id (Attribute_Name (Disc_Exp))
                                  = Attribute_Access
                       and then Object_Access_Level (Prefix (Disc_Exp))
                                  > Type_Access_Level (Typ)
                     then
                        Error_Msg_N
                          ("prefix of attribute has deeper level than"
                              & " allocator type", Disc_Exp);

                     --  When the operand is an access discriminant the check
                     --  is against the level of the prefix object.

                     elsif Ekind (Etype (Disc_Exp)) = E_Anonymous_Access_Type
                       and then Nkind (Disc_Exp) = N_Selected_Component
                       and then Object_Access_Level (Prefix (Disc_Exp))
                                  > Type_Access_Level (Typ)
                     then
                        Error_Msg_N
                          ("access discriminant has deeper level than"
                              & " allocator type", Disc_Exp);
                     end if;
                  end if;
                  Next_Discriminant (Discrim);
                  Next (Constr);
               end loop;
            end if;
         end if;
      end if;

      --  Check for allocation from an empty storage pool

      if No_Pool_Assigned (Typ) then
         declare
            Loc : constant Source_Ptr := Sloc (N);

         begin
            Error_Msg_N ("?allocation from empty storage pool!", N);
            Error_Msg_N ("?Storage_Error will be raised at run time!", N);
            Insert_Action (N,
              Make_Raise_Storage_Error (Loc));
         end;
      end if;
   end Resolve_Allocator;

   ---------------------------
   -- Resolve_Arithmetic_Op --
   ---------------------------

   --  Used for resolving all arithmetic operators except exponentiation

   procedure Resolve_Arithmetic_Op (N : Node_Id; Typ : Entity_Id) is
      L  : constant Node_Id := Left_Opnd (N);
      R  : constant Node_Id := Right_Opnd (N);
      T  : Entity_Id;
      TL : Entity_Id := Base_Type (Etype (L));
      TR : Entity_Id := Base_Type (Etype (R));

      B_Typ : constant Entity_Id := Base_Type (Typ);
      --  We do the resolution using the base type, because intermediate values
      --  in expressions always are of the base type, not a subtype of it.

      function Is_Integer_Or_Universal (N : Node_Id) return Boolean;
      --  Return True iff given type is Integer or universal real/integer

      procedure Set_Operand_Type (N : Node_Id);
      --  Set operand type to T if universal

      procedure Set_Mixed_Mode_Operand (N : Node_Id; T : Entity_Id);
      --  Choose type of integer literal in fixed-point operation to conform
      --  to available fixed-point type. T is the type of the other operand,
      --  which is needed to determine the expected type of N.

      function Universal_Interpretation (N : Node_Id) return Entity_Id;
      --  Find universal type of operand, if any.

      function Is_Integer_Or_Universal (N : Node_Id) return Boolean is
         T     : Entity_Id;
         Index : Interp_Index;
         It    : Interp;

      begin
         if not Is_Overloaded (N) then
            T := Etype (N);
            return Base_Type (T) = Base_Type (Standard_Integer)
              or else T = Universal_Integer
              or else T = Universal_Real;
         else
            Get_First_Interp (N, Index, It);

            while Present (It.Typ) loop

               if Base_Type (It.Typ) = Base_Type (Standard_Integer)
                 or else It.Typ = Universal_Integer
                 or else It.Typ = Universal_Real
               then
                  return True;
               end if;

               Get_Next_Interp (Index, It);
            end loop;
         end if;

         return False;
      end Is_Integer_Or_Universal;

      function Universal_Interpretation (N : Node_Id) return Entity_Id is
         Index : Interp_Index;
         It    : Interp;

      begin
         if not Is_Overloaded (N) then

            if Etype (N) = Universal_Integer
               or else Etype (N) = Universal_Real
            then
               return Etype (N);
            else
               return Empty;
            end if;

         else
            Get_First_Interp (N, Index, It);

            while Present (It.Typ) loop

               if It.Typ = Universal_Integer
                  or else It.Typ = Universal_Real
               then
                  return It.Typ;
               end if;

               Get_Next_Interp (Index, It);
            end loop;

            return Empty;
         end if;
      end Universal_Interpretation;

      procedure Set_Operand_Type (N : Node_Id) is
      begin
         if Etype (N) = Universal_Integer
           or else Etype (N) = Universal_Real
         then
            Set_Etype (N, T);
         end if;
      end Set_Operand_Type;

      procedure Set_Mixed_Mode_Operand (N : Node_Id; T : Entity_Id) is
         Index : Interp_Index;
         It    : Interp;

      begin
         if Universal_Interpretation (N) = Universal_Integer then

            --  A universal integer literal is resolved as standard integer
            --  except in the case of a fixed-point result, where we leave
            --  it as universal (to be handled by Exp_Fixd later on)

            if Is_Fixed_Point_Type (T) then
               Resolve (N, Universal_Integer);
            else
               Resolve (N, Standard_Integer);
            end if;

         elsif Universal_Interpretation (N) = Universal_Real
           and then (T = Base_Type (Standard_Integer)
                      or else T = Universal_Integer
                      or else T = Universal_Real)
         then
            --  A universal real can appear in a fixed-type context. We resolve
            --  the literal with that context, even though this might raise an
            --  exception prematurely (the other operand may be zero).

            Resolve (N, B_Typ);

         elsif Etype (N) = Base_Type (Standard_Integer)
           and then T = Universal_Real
           and then Is_Overloaded (N)
         then
            --  Integer arg in mixed-mode operation. Resolve with universal
            --  type, in case preference rule must be applied.

            Resolve (N, Universal_Integer);

         elsif Etype (N) = T
           and then B_Typ /= Universal_Fixed
         then
            --  Not a mixed-mode operation. Resolve with context.

            Resolve (N, B_Typ);

         elsif Etype (N) = Any_Fixed then
            --  N may itself be a mixed-mode operation, so use context type.

            Resolve (N, B_Typ);

         elsif Is_Fixed_Point_Type (T)
           and then B_Typ = Universal_Fixed
           and then Is_Overloaded (N)
         then

            --  must be (fixed * fixed) operation, operand must have one
            --  compatible interpretation.

            Resolve (N, Any_Fixed);

         elsif Is_Fixed_Point_Type (B_Typ)
           and then (T = Universal_Real
                      or else Is_Fixed_Point_Type (T))
           and then Is_Overloaded (N)
         then

            --  C * F(X) in a fixed context, where C is a real literal or a
            --  fixed-point expression. F must have either a fixed type
            --  interpretation or an integer interpretation, but not both.

            Get_First_Interp (N, Index, It);

            while Present (It.Typ) loop

               if Base_Type (It.Typ) = Base_Type (Standard_Integer) then

                  if Analyzed (N) then
                     Error_Msg_N ("ambiguous operand in fixed operation", N);
                  else
                     Resolve (N, Standard_Integer);
                  end if;

               elsif Is_Fixed_Point_Type (It.Typ) then

                  if Analyzed (N) then
                     Error_Msg_N ("ambiguous operand in fixed operation", N);
                  else
                     Resolve (N, It.Typ);
                  end if;
               end if;

               Get_Next_Interp (Index, It);
            end loop;

            --  Reanalyze the literal with the fixed type of the context.

            if N = L then
               Set_Analyzed (R, False);
               Resolve (R, B_Typ);
            else
               Set_Analyzed (L, False);
               Resolve (L, B_Typ);
            end if;

         else
            Resolve (N, Etype (N));
         end if;
      end Set_Mixed_Mode_Operand;

   --  Start of processing for Resolve_Arithmetic_Op

   begin
      if Comes_From_Source (N)
        and then Ekind (Entity (N)) = E_Function
        and then Is_Imported (Entity (N))
        and then Present (First_Rep_Item (Entity (N)))
      then
         Resolve_Intrinsic_Operator (N, Typ);
         return;

      --  Special-case for mixed-mode universal expressions or fixed point
      --  type operation: each argument is resolved separately. The same
      --  treatment is required if one of the operands of a fixed point
      --  operation is universal real, since in this case we don't do a
      --  conversion to a specific fixed-point type (instead the expander
      --  takes care of the case).

      elsif (B_Typ = Universal_Integer
           or else B_Typ = Universal_Real)
        and then Present (Universal_Interpretation (L))
        and then Present (Universal_Interpretation (R))
      then
         Resolve (L, Universal_Interpretation (L));
         Resolve (R, Universal_Interpretation (R));
         Set_Etype (N, B_Typ);

      elsif (B_Typ = Universal_Real
           or else Etype (N) = Universal_Fixed
           or else (Etype (N) = Any_Fixed
                     and then Is_Fixed_Point_Type (B_Typ))
           or else (Is_Fixed_Point_Type (B_Typ)
                     and then (Is_Integer_Or_Universal (L)
                                 or else
                               Is_Integer_Or_Universal (R))))
        and then (Nkind (N) = N_Op_Multiply or else
                  Nkind (N) = N_Op_Divide)
      then
         if TL = Universal_Integer or else TR = Universal_Integer then
            Check_For_Visible_Operator (N, B_Typ);
         end if;

         --  If context is a fixed type and one operand is integer, the
         --  other is resolved with the type of the context.

         if Is_Fixed_Point_Type (B_Typ)
           and then (Base_Type (TL) = Base_Type (Standard_Integer)
                      or else TL = Universal_Integer)
         then
            Resolve (R, B_Typ);
            Resolve (L, TL);

         elsif Is_Fixed_Point_Type (B_Typ)
           and then (Base_Type (TR) = Base_Type (Standard_Integer)
                      or else TR = Universal_Integer)
         then
            Resolve (L, B_Typ);
            Resolve (R, TR);

         else
            Set_Mixed_Mode_Operand (L, TR);
            Set_Mixed_Mode_Operand (R, TL);
         end if;

         if Etype (N) = Universal_Fixed
           or else Etype (N) = Any_Fixed
         then
            if B_Typ = Universal_Fixed
              and then Nkind (Parent (N)) /= N_Type_Conversion
              and then Nkind (Parent (N)) /= N_Unchecked_Type_Conversion
            then
               Error_Msg_N
                 ("type cannot be determined from context!", N);
               Error_Msg_N
                 ("\explicit conversion to result type required", N);

               Set_Etype (L, Any_Type);
               Set_Etype (R, Any_Type);

            else
               if Ada_83
                  and then Etype (N) = Universal_Fixed
                  and then Nkind (Parent (N)) /= N_Type_Conversion
                  and then Nkind (Parent (N)) /= N_Unchecked_Type_Conversion
               then
                  Error_Msg_N
                    ("(Ada 83) fixed-point operation " &
                     "needs explicit conversion",
                     N);
               end if;

               Set_Etype (N, B_Typ);
            end if;

         elsif Is_Fixed_Point_Type (B_Typ)
           and then (Is_Integer_Or_Universal (L)
                       or else Nkind (L) = N_Real_Literal
                       or else Nkind (R) = N_Real_Literal
                       or else
                     Is_Integer_Or_Universal (R))
         then
            Set_Etype (N, B_Typ);

         elsif Etype (N) = Any_Fixed then

            --  If no previous errors, this is only possible if one operand
            --  is overloaded and the context is universal. Resolve as such.

            Set_Etype (N, B_Typ);
         end if;

      else
         if (TL = Universal_Integer or else TL = Universal_Real)
           and then (TR = Universal_Integer or else TR = Universal_Real)
         then
            Check_For_Visible_Operator (N, B_Typ);
         end if;

         --  If the context is Universal_Fixed and the operands are also
         --  universal fixed, this is an error, unless there is only one
         --  applicable fixed_point type (usually duration).

         if B_Typ = Universal_Fixed
           and then Etype (L) = Universal_Fixed
         then
            T := Unique_Fixed_Point_Type (N);

            if T  = Any_Type then
               Set_Etype (N, T);
               return;
            else
               Resolve (L, T);
               Resolve (R, T);
            end if;

         else
            Resolve (L, B_Typ);
            Resolve (R, B_Typ);
         end if;

         --  If one of the arguments was resolved to a non-universal type.
         --  label the result of the operation itself with the same type.
         --  Do the same for the universal argument, if any.

         T := Intersect_Types (L, R);
         Set_Etype (N, Base_Type (T));
         Set_Operand_Type (L);
         Set_Operand_Type (R);
      end if;

      Generate_Operator_Reference (N);
      Eval_Arithmetic_Op (N);

      --  Set overflow checking bit. Much cleverer code needed here eventually
      --  and perhaps the Resolve routines should be separated for the various
      --  arithmetic operations, since they will need different processing. ???

      if Nkind (N) in N_Op then
         if not Overflow_Checks_Suppressed (Etype (N)) then
            Set_Do_Overflow_Check (N);
         end if;
      end if;

      Check_Unset_Reference (L);
      Check_Unset_Reference (R);

   end Resolve_Arithmetic_Op;

   ------------------
   -- Resolve_Call --
   ------------------

   procedure Resolve_Call (N : Node_Id; Typ : Entity_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Subp    : constant Node_Id    := Name (N);
      Nam     : Entity_Id;
      I       : Interp_Index;
      It      : Interp;
      Norm_OK : Boolean;
      Scop    : Entity_Id;

      function Is_Predefined_Op (Nam : Entity_Id) return Boolean;
      --  Utility to check whether the name in the call is a predefined
      --  operator, in which case the call is made into an operator node.
      --  An instance of an intrinsic conversion operation may be given
      --  an operator name, but is not treated like an operator.

      function Is_Predefined_Op (Nam : Entity_Id) return Boolean is
      begin
         return Is_Intrinsic_Subprogram (Nam)
           and then not Is_Generic_Instance (Nam)
           and then Chars (Nam) in Any_Operator_Name;
      end Is_Predefined_Op;

   --  Start of processing for Resolve_Call

   begin
      --  The context imposes a unique interpretation with type Typ on
      --  a procedure or function call. Find the entity of the subprogram
      --  that yields the expected type, and propagate the corresponding
      --  formal constraints on the actuals. The caller has established
      --  that an interpretation exists, and emitted an error if not unique.

      --  First deal with the case of a call to an access-to-subprogram,
      --  dereference made explicit in Analyze_Call.

      if Ekind (Etype (Subp)) = E_Subprogram_Type then

         if not Is_Overloaded (Subp) then
            Nam := Etype (Subp);

         else
            --  Find the interpretation whose type (a subprogram type)
            --  has a return type that is compatible with the context.

            Get_First_Interp (Subp,  I, It);
            while Present (It.Typ) loop

               if Covers (Typ, Etype (It.Typ)) then
                  Nam := It.Typ;
                  exit;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end if;

         --  If the prefix is not an entity, then resolve it

         if not Is_Entity_Name (Subp) then
            Resolve (Subp, Nam);
         end if;

      --  If this is a procedure call which is really an entry call, do
      --  the conversion of the procedure call to an entry call. Protected
      --  operations use the same circuitry because the name in the call
      --  can be an arbitrary expression with special resolution rules.

      elsif Nkind (Subp) = N_Selected_Component
        or else Nkind (Subp) = N_Indexed_Component
        or else (Is_Entity_Name (Subp)
                  and then Ekind (Entity (Subp)) = E_Entry)
      then
         Resolve_Entry_Call (N, Typ);
         Check_Elab_Call (N);
         return;

      --  Normal subprogram call with name established in Resolve

      elsif not (Is_Type (Entity (Subp))) then
         Nam := Entity (Subp);
         Set_Entity_With_Style_Check (Subp, Nam);
         Generate_Reference (Nam, Subp);

      --  Otherwise we must have the case of an overloaded call

      else
         pragma Assert (Is_Overloaded (Subp));
         Get_First_Interp (Subp,  I, It);

         while Present (It.Typ) loop
            if Covers (Typ, It.Typ) then
               Nam := It.Nam;
               Set_Entity_With_Style_Check (Subp, Nam);
               Generate_Reference (Nam, Subp);
               exit;
            end if;

            Get_Next_Interp (I, It);
         end loop;
      end if;

      --  Check that a call to Current_Task does not occur in an entry body

      if Is_RTE (Nam, RE_Current_Task) then
         declare
            P : Node_Id;

         begin
            P := N;
            loop
               P := Parent (P);
               exit when No (P);

               if Nkind (P) = N_Entry_Body then
                  Error_Msg_NE
                    ("& should not be used in entry body ('R'M C.7(17))",
                     N, Nam);
                  exit;
               end if;
            end loop;
         end;
      end if;

      --  Check that a procedure call does not occur in the context
      --  of the entry call statement of a conditional or timed
      --  entry call. Note that the case of a call to a subprogram
      --  renaming of an entry will also be rejected. The test
      --  for N not being an N_Entry_Call_Statement is defensive,
      --  covering the possibility that the processing of entry
      --  calls might reach this point due to later modifications
      --  of the code above.

      if Nkind (Parent (N)) = N_Entry_Call_Alternative
        and then Nkind (N) /= N_Entry_Call_Statement
        and then Entry_Call_Statement (Parent (N)) = N
      then
         Error_Msg_N ("entry call required in select statement", N);
      end if;

      --  Freeze the subprogram name if not in default expression. Note
      --  that we freeze procedure calls as well as function calls.
      --  Procedure calls are not frozen according to the rules (RM
      --  13.14(14)) because it is impossible to have a procedure call to
      --  a non-frozen procedure in pure Ada, but in the code that we
      --  generate in the expander, this rule needs extending because we
      --  can generate procedure calls that need freezing.

      if Is_Entity_Name (Subp) and then not In_Default_Expression then
         Freeze_Expression (Subp);
      end if;

      --  For a predefined operator, the type of the result is the type
      --  imposed by context, except for a predefined operation on universal
      --  fixed. Otherwise The type of the call is the type returned by the
      --  subprogram being called.

      if Is_Predefined_Op (Nam) then

         if Etype (N) /= Universal_Fixed then
            Set_Etype (N, Typ);
         end if;

      --  If the subprogram returns an array type, and the context
      --  requires the component type of that array type, the node is
      --  really an indexing of the parameterless call. Resolve as such.

      elsif Needs_No_Actuals (Nam)
        and then
          ((Is_Array_Type (Etype (Nam))
                   and then Covers (Typ, Component_Type (Etype (Nam))))
             or else (Is_Access_Type (Etype (Nam))
                        and then Is_Array_Type (Designated_Type (Etype (Nam)))
                        and then
                          Covers (Typ,
                            Component_Type (Designated_Type (Etype (Nam))))))
      then
         declare
            Index_Node : Node_Id;

         begin
            Check_Elab_Call (N);

            if Component_Type (Etype (Nam)) /= Any_Type then
               Index_Node :=
                 Make_Indexed_Component (Loc,
                   Prefix =>
                     Make_Function_Call (Loc,
                       Name => New_Occurrence_Of (Nam, Loc)),
                   Expressions => Parameter_Associations (N));

               --  Since we are correcting a node classification error made by
               --  the parser, we call Replace rather than Rewrite.

               Replace (N, Index_Node);
               Set_Etype (Prefix (N), Etype (Nam));
               Set_Etype (N, Typ);
               Resolve_Indexed_Component (N, Typ);
            end if;

            return;
         end;

      else
         Set_Etype (N, Etype (Nam));
      end if;

      --  In the case where the call is to an overloaded subprogram, Analyze
      --  calls Normalize_Actuals once per overloaded subprogram. Therefore in
      --  such a case Normalize_Actuals needs to be called once more to order
      --  the actuals correctly. Otherwise the call will have the ordering
      --  given by the last overloaded subprogram whether this is the correct
      --  one being called or not.

      if Is_Overloaded (Subp) then
         Normalize_Actuals (N, Nam, False, Norm_OK);
         pragma Assert (Norm_OK);
      end if;

      --  In any case, call is fully resolved now. Reset Overload flag, to
      --  prevent subsequent overload resolution if node is analyzed again

      Set_Is_Overloaded (Subp, False);
      Set_Is_Overloaded (N, False);

      --  If we are calling the current subprogram from immediately within
      --  its body, then that is the case where we can sometimes detect
      --  cases of infinite recursion statically. Do not try this in case
      --  restriction No_Recursion is in effect anyway.

      Scop := Current_Scope;

      if Nam = Scop
        and then not Restrictions (No_Recursion)
        and then Check_Infinite_Recursion (N)
      then
         --  Here we detected and flagged an infinite recursion, so we do
         --  not need to test the case below for further warnings.

         null;

      --  If call is to immediately containing subprogram, then check for
      --  the case of a possible run-time detectable infinite recursion.

      else
         while Scop /= Standard_Standard loop
            if Nam = Scop then
               --  Although in general recursion is not statically checkable,
               --  the case of calling an immediately containing subprogram
               --  is easy to catch.

               Check_Restriction (No_Recursion, N);

               --  If the recursive call is to a parameterless procedure, then
               --  even if we can't statically detect infinite recursion, this
               --  is pretty suspicious, and we output a warning. Furthermore,
               --  we will try later to detect some cases here at run time by
               --  expanding checking code (see Detect_Infinite_Recursion in
               --  package Exp_Ch6).
               --  If the recursive call is within a handler we do not emit a
               --  warning, because this is a common idiom: loop until input
               --  is correct, catch illegal input in handler and restart.

               if No (First_Formal (Nam))
                 and then Etype (Nam) = Standard_Void_Type
                 and then not Error_Posted (N)
                 and then Nkind (Parent (N)) /= N_Exception_Handler
               then
                  Set_Has_Recursive_Call (Nam);
                  Error_Msg_N ("possible infinite recursion?", N);
                  Error_Msg_N ("Storage_Error may be raised at run time?", N);
               end if;

               exit;
            end if;

            Scop := Scope (Scop);
         end loop;
      end if;

      --  If subprogram name is a predefined operator, it was given in
      --  functional notation. Replace call node with operator node, so
      --  that actuals can be resolved appropriately.

      if Is_Predefined_Op (Nam) or else Ekind (Nam) = E_Operator then
         Make_Call_Into_Operator (N, Typ, Entity (Name (N)));
         return;

      elsif Present (Alias (Nam))
        and then Is_Predefined_Op (Alias (Nam))
      then
         Resolve_Actuals (N, Nam);
         Make_Call_Into_Operator (N, Typ, Alias (Nam));
         return;
      end if;

      --  Create a transient scope if the resulting type requires it.
      --  There are 3 notable exceptions: in init_procs, the transient scope
      --  overhead is not needed and even incorrect due to the actual expansion
      --  of adjust calls; the second case is enumeration literal pseudo calls,
      --  the other case is unchecked_conversion.

      --  If this is an initialization call for a type whose initialization
      --  uses the secondary stack, we also need to create a transient scope
      --  for it, precisely because we will not do it within the init_proc
      --  itself.

      if Expander_Active
        and then Is_Type (Etype (Nam))
        and then Requires_Transient_Scope (Etype (Nam))
        and then Ekind (Nam) /= E_Enumeration_Literal
        and then not Within_Init_Proc
        and then not (Is_Intrinsic_Subprogram (Nam)
                      and then Present (Parent (Nam))
                      and then Present (Generic_Parent (Parent (Nam)))
                      and then Chars (Generic_Parent (Parent (Nam))) =
                          Name_Unchecked_Conversion)
      then
         Establish_Transient_Scope
           (N, Sec_Stack => not Functions_Return_By_DSP_On_Target);

      elsif Chars (Nam) = Name_uInit_Proc
        and then not Within_Init_Proc
      then
         Check_Initialization_Call (N, Nam);
      end if;

      --  A protected function cannot be called within the definition of the
      --  enclosing protected type.

      if Is_Protected_Type (Scope (Nam))
        and then In_Open_Scopes (Scope (Nam))
        and then not Has_Completion (Scope (Nam))
      then
         Error_Msg_NE
           ("& cannot be called before end of protected definition", N, Nam);
      end if;

      --  Propagate interpretation to actuals, and add default expressions
      --  where needed.

      if Present (First_Formal (Nam)) then
         Resolve_Actuals (N, Nam);

         --  Overloaded literals are rewritten as function calls, for
         --  purpose of resolution. After resolution, we can replace
         --  the call with the literal itself.

      elsif Ekind (Nam) = E_Enumeration_Literal then
         Copy_Node (Subp, N);
         Resolve_Entity_Name (N, Typ);

         --  Avoid validation, since it is a static function call.

         return;
      end if;

      --  If the subprogram is a primitive operation, check whether or not
      --  it is a correct dispatching call.

      if Is_Overloadable (Nam)
        and then Is_Dispatching_Operation (Nam)
      then
         Check_Dispatching_Call (N);

            --  If the subprogram is abstract, check that the call has a
            --  controlling argument (i.e. is dispatching) or is disptaching on
            --  result

         if Is_Abstract (Nam)
           and then No (Controlling_Argument (N))
           and then not Is_Class_Wide_Type (Typ)
           and then not Is_Tag_Indeterminate (N)
         then
            Error_Msg_N ("call to abstract subprogram must be dispatching", N);
         end if;

      elsif Is_Abstract (Nam)
        and then not In_Instance
      then
         Error_Msg_NE ("cannot call abstract subprogram &!", N, Nam);
      end if;

      if Is_Intrinsic_Subprogram (Nam) then
         Check_Intrinsic_Call (N);
      end if;

      --  If we fall through we definitely have a non-static call

      Check_Elab_Call (N);

   end Resolve_Call;

   -------------------------------
   -- Check_Initialization_Call --
   -------------------------------

   procedure Check_Initialization_Call (N : Entity_Id; Nam : Entity_Id) is
      Typ : Entity_Id := Etype (First_Formal (Nam));

      function Uses_SS (T : Entity_Id) return Boolean;

      function Uses_SS (T : Entity_Id) return Boolean is
         Comp : Entity_Id;
         Expr : Node_Id;

      begin
         if Is_Controlled (T)
           or else Has_Controlled_Component (T)
           or else Functions_Return_By_DSP_On_Target
         then
            return False;

         elsif Is_Array_Type (T) then
            return Uses_SS (Component_Type (T));

         elsif Is_Record_Type (T) then
            Comp := First_Component (T);

            while Present (Comp) loop

               if Ekind (Comp) = E_Component
                 and then Nkind (Parent (Comp)) = N_Component_Declaration
               then
                  Expr := Expression (Parent (Comp));

                  if Nkind (Expr) = N_Function_Call
                    and then Requires_Transient_Scope (Etype (Expr))
                  then
                     return True;

                  elsif Uses_SS (Etype (Comp)) then
                     return True;
                  end if;
               end if;

               Next_Component (Comp);
            end loop;

            return False;

         else
            return False;
         end if;
      end Uses_SS;

   begin
      if Uses_SS (Typ) then
         Establish_Transient_Scope (First_Actual (N), Sec_Stack => True);
      end if;
   end Check_Initialization_Call;

   -------------------------------
   -- Resolve_Character_Literal --
   -------------------------------

   procedure Resolve_Character_Literal (N : Node_Id; Typ : Entity_Id) is
      B_Typ : constant Entity_Id := Base_Type (Typ);
      C     : Entity_Id;

   begin
      --  Verify that the character does belong to the type of the context

      Set_Etype (N, B_Typ);
      Eval_Character_Literal (N);

      --  Wide_Character literals must always be defined, since the set of
      --  wide character literals is complete, i.e. if a character literal
      --  is accepted by the parser, then it is OK for wide character.

      if Root_Type (B_Typ) = Standard_Wide_Character then
         return;

      --  Always accept character literal for type Any_Character, which
      --  occurs in error situations and in comparisons of literals, both
      --  of which should accept all literals.

      elsif B_Typ = Any_Character then
         return;

      --  For Standard.Character or a type derived from it, check that
      --  the literal is in range

      elsif Root_Type (B_Typ) = Standard_Character then
         if In_Character_Range (Char_Literal_Value (N)) then
            return;
         end if;

      --  If the entity is already set, this has already been resolved in
      --  a generic context, or comes from expansion. Nothing else to do.

      elsif Present (Entity (N)) then
         return;

      --  Otherwise we have a user defined character type, and we can use
      --  the standard visibility mechanisms to locate the referenced entity

      else
         C := Current_Entity (N);

         while Present (C) loop
            if Etype (C) = B_Typ then
               Set_Entity_With_Style_Check (N, C);
               Generate_Reference (C, N);
               return;
            end if;

            C := Homonym (C);
         end loop;
      end if;

      --  If we fall through, then the literal does not match any of the
      --  entries of the enumeration type. This isn't just a constraint
      --  error situation, it is an illegality (see RM 4.2).

      Error_Msg_NE
        ("character not defined for }", N, First_Subtype (B_Typ));

   end Resolve_Character_Literal;

   ---------------------------
   -- Resolve_Comparison_Op --
   ---------------------------

   --  Context requires a boolean type, and plays no role in resolution.
   --  Processing identical to that for equality operators.

   procedure Resolve_Comparison_Op (N : Node_Id; Typ : Entity_Id) is
      L : constant Node_Id := Left_Opnd (N);
      R : constant Node_Id := Right_Opnd (N);
      T : Entity_Id;

   begin
      --  If this is an intrinsic operation which is not predefined, use
      --  the types of its declared arguments to resolve the possibly
      --  overloaded operands. Otherwise the operands are unambiguous and
      --  specify the expected type.

      if Scope (Entity (N)) /= Standard_Standard then
         T := Etype (First_Entity (Entity (N)));
      else
         T := Find_Unique_Type (L, R);

         if T = Any_Fixed then
            T := Unique_Fixed_Point_Type (L);
         end if;
      end if;

      Set_Etype (N, Typ);
      Generate_Reference (T, N, ' ');

      if T /= Any_Type then

         if T = Any_String
           or else T = Any_Composite
           or else T = Any_Character
         then
            if T = Any_Character then
               Ambiguous_Character (L);
            else
               Error_Msg_N ("ambiguous operands for comparison", N);
            end if;

            Set_Etype (N, Any_Type);
            return;

         else
            if Comes_From_Source (N)
              and then Has_Unchecked_Union (T)
            then
               Error_Msg_N
                ("cannot compare Unchecked_Union values", N);
            end if;

            Resolve (L, T);
            Resolve (R, T);
            Check_Unset_Reference (L);
            Check_Unset_Reference (R);
            Generate_Operator_Reference (N);
            Eval_Relational_Op (N);
         end if;
      end if;

   end Resolve_Comparison_Op;

   ------------------------------------
   -- Resolve_Conditional_Expression --
   ------------------------------------

   procedure Resolve_Conditional_Expression (N : Node_Id; Typ : Entity_Id) is
      Condition : constant Node_Id := First (Expressions (N));
      Then_Expr : constant Node_Id := Next (Condition);
      Else_Expr : constant Node_Id := Next (Then_Expr);

   begin
      Resolve (Condition, Standard_Boolean);
      Resolve (Then_Expr, Typ);
      Resolve (Else_Expr, Typ);

      Set_Etype (N, Typ);
      Eval_Conditional_Expression (N);
   end Resolve_Conditional_Expression;

   -----------------------------------------
   -- Resolve_Discrete_Subtype_Indication --
   -----------------------------------------

   procedure Resolve_Discrete_Subtype_Indication
     (N   : Node_Id;
      Typ : Entity_Id)
   is
      R : Node_Id;
      S : Entity_Id;

   begin
      Analyze (Subtype_Mark (N));
      S := Entity (Subtype_Mark (N));

      if Nkind (Constraint (N)) /= N_Range_Constraint then
         Error_Msg_N ("expect range constraint for discrete type", N);
         Set_Etype (N, Any_Type);

      else
         R := Range_Expression (Constraint (N));
         Analyze (R);

         if Base_Type (S) /= Base_Type (Typ) then
            Error_Msg_NE
              ("expect subtype of }", N, First_Subtype (Typ));

            --  Rewrite the constraint as a range of Typ
            --  to allow compilation to proceed further.

            Set_Etype (N, Typ);
            Rewrite (Low_Bound (R),
              Make_Attribute_Reference (Sloc (Low_Bound (R)),
                Prefix =>         New_Occurrence_Of (Typ, Sloc (R)),
                Attribute_Name => Name_First));
            Rewrite (High_Bound (R),
              Make_Attribute_Reference (Sloc (High_Bound (R)),
                Prefix =>         New_Occurrence_Of (Typ, Sloc (R)),
                Attribute_Name => Name_First));

         else
            Resolve (R, Typ);
            Set_Etype (N, Etype (R));

            --  Additionally, we must check that the bounds are compatible
            --  with the given subtype, which might be different from the
            --  type of the context.

            Apply_Range_Check (R, S);

            --  ??? If the above check statically detects a Constraint_Error
            --  it replaces the offending bound(s) of the range R with a
            --  Constraint_Error node. When the itype which uses these bounds
            --  is frozen the resulting call to Duplicate_Subexpr generates
            --  a new temporary for the bounds.

            --  Unfortunately there are other itypes that are also made depend
            --  on these bounds, so when Duplicate_Subexpr is called they get
            --  a forward reference to the newly created temporaries and Gigi
            --  aborts on such forward references. This is probably sign of a
            --  more fundamental problem somewhere else in either the order of
            --  itype freezing or the way certain itypes are constructed.

            --  To get around this problem we call Remove_Side_Effects right
            --  away if either bounds of R are a Constraint_Error.

            declare
               L : Node_Id := Low_Bound (R);
               H : Node_Id := High_Bound (R);

            begin
               if Nkind (L) = N_Raise_Constraint_Error then
                  Remove_Side_Effects (L);
               end if;

               if Nkind (H) = N_Raise_Constraint_Error then
                  Remove_Side_Effects (H);
               end if;
            end;

            Check_Unset_Reference (Low_Bound  (R));
            Check_Unset_Reference (High_Bound (R));
         end if;
      end if;
   end Resolve_Discrete_Subtype_Indication;

   -------------------------
   -- Resolve_Entity_Name --
   -------------------------

   --  Used to resolve identifiers and expanded names

   procedure Resolve_Entity_Name (N : Node_Id; Typ : Entity_Id) is
      E : constant Entity_Id := Entity (N);

   begin
      --  Replace named numbers by corresponding literals. Note that this is
      --  the one case where Resolve_Entity_Name must reset the Etype, since
      --  it is currently marked as universal.

      if Ekind (E) = E_Named_Integer then
         Set_Etype (N, Typ);
         Eval_Named_Integer (N);

      elsif Ekind (E) = E_Named_Real then
         Set_Etype (N, Typ);
         Eval_Named_Real (N);

      --  Allow use of subtype only if it is a concurrent type where we are
      --  currently inside the body. This will eventually be expanded
      --  into a call to Self (for tasks) or _object (for protected
      --  objects). Any other use of a subtype is invalid.

      elsif Is_Type (E) then
         if Is_Concurrent_Type (E)
           and then In_Open_Scopes (E)
         then
            null;
         else
            Error_Msg_N
               ("Invalid use of subtype mark in expression or call", N);
         end if;

      --  Check discriminant use if entity is discriminant in current scope,
      --  i.e. discriminant of record or concurrent type currently being
      --  analyzed. Uses in corresponding body are unrestricted.

      elsif Ekind (E) = E_Discriminant
        and then Scope (E) = Current_Scope
        and then not Has_Completion (Current_Scope)
      then
         Check_Discriminant_Use (N);

      --  A parameterless generic function cannot appear in a context that
      --  requires resolution.

      elsif Ekind (E) = E_Generic_Function then
         Error_Msg_N ("illegal use of generic function", N);

      elsif Ekind (E) = E_Out_Parameter
        and then Ada_83
        and then (Nkind (Parent (N)) in N_Op
                    or else (Nkind (Parent (N)) = N_Assignment_Statement
                              and then N = Expression (Parent (N)))
                    or else Nkind (Parent (N)) = N_Explicit_Dereference)
      then
         Error_Msg_N ("(Ada 83) illegal reading of out parameter", N);

      --  In all other cases, just do the possible static evaluation

      else
         --  A deferred constant that appears in an expression must have
         --  a completion, unless it has been removed by in-place expansion
         --  of an aggregate.

         if Ekind (E) = E_Constant
           and then Comes_From_Source (E)
           and then No (Constant_Value (E))
           and then Is_Frozen (Etype (E))
           and then not In_Default_Expression
           and then not Is_Imported (E)
         then

            if No_Initialization (Parent (E))
              or else (Present (Full_View (E))
                        and then No_Initialization (Parent (Full_View (E))))
            then
               null;
            else
               Error_Msg_N (
                 "deferred constant is frozen before completion", N);
            end if;
         end if;

         Eval_Entity_Name (N);
      end if;
   end Resolve_Entity_Name;


   -------------------
   -- Resolve_Entry --
   -------------------

   procedure Resolve_Entry (Entry_Name : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (Entry_Name);
      Nam    : Entity_Id;
      New_N  : Node_Id;
      S      : Entity_Id;
      Tsk    : Entity_Id;
      E_Name : Node_Id;
      Index  : Node_Id;

      function Actual_Index_Type (E : Entity_Id) return Entity_Id;
      --  If the bounds of the entry family being called depend on task
      --  discriminants, build a new index subtype where a discriminant is
      --  replaced with the the value of the discriminant of the target task.
      --  The target task is the prefix of the entry name in the call.

      -----------------------
      -- Actual_Index_Type --
      -----------------------

      function Actual_Index_Type (E : Entity_Id) return Entity_Id is
         Typ   : Entity_Id := Entry_Index_Type (E);
         Tsk   : Entity_Id := Scope (E);
         Lo    : Node_Id := Type_Low_Bound  (Typ);
         Hi    : Node_Id := Type_High_Bound (Typ);
         New_T : Entity_Id;

         function Actual_Discriminant_Ref (Bound : Node_Id) return Node_Id;
         --  If the bound is given by a discriminant, replace with a reference
         --  to the discriminant of the same name in the target task.
         --  If the entry name is the target of a requeue statement and the
         --  entry is in the current protected object, the bound to be used
         --  is the discriminal of the object (see apply_range_checks for
         --  details of the transformation).

         -----------------------------
         -- Actual_Discriminant_Ref --
         -----------------------------

         function Actual_Discriminant_Ref (Bound : Node_Id) return Node_Id is
            Typ : Entity_Id := Etype (Bound);
            Ref : Node_Id;

         begin
            Remove_Side_Effects (Bound);

            if not Is_Entity_Name (Bound)
              or else Ekind (Entity (Bound)) /= E_Discriminant
            then
               return Bound;

            elsif Is_Protected_Type (Tsk)
              and then In_Open_Scopes (Tsk)
              and then Nkind (Parent (Entry_Name)) = N_Requeue_Statement
            then
               return New_Occurrence_Of (Discriminal (Entity (Bound)), Loc);

            else
               Ref :=
                 Make_Selected_Component (Loc,
                   Prefix => New_Copy_Tree (Prefix (Prefix (Entry_Name))),
                   Selector_Name => New_Occurrence_Of (Entity (Bound), Loc));
               Analyze (Ref);
               Resolve (Ref, Typ);
               return Ref;
            end if;
         end Actual_Discriminant_Ref;

      --  Start of processing for Actual_Index_Type

      begin
         if not Has_Discriminants (Tsk)
           or else (not Is_Entity_Name (Lo)
                     and then not Is_Entity_Name (Hi))
         then
            return Entry_Index_Type (E);

         else
            New_T := Create_Itype (Ekind (Typ), Parent (Entry_Name));
            Set_Etype        (New_T, Base_Type (Typ));
            Set_Size_Info    (New_T, Typ);
            Set_Scalar_Range (New_T,
              Make_Range (Sloc (Entry_Name),
                Low_Bound  => Actual_Discriminant_Ref (Lo),
                High_Bound => Actual_Discriminant_Ref (Hi)));

            return New_T;
         end if;
      end Actual_Index_Type;

   --  Start of processing of Resolve_Entry

   begin
      --  Find name of entry being called, and resolve prefix of name
      --  with its own type. The prefix can be overloaded, and the name
      --  and signature of the entry must be taken into account.

      if Nkind (Entry_Name) = N_Indexed_Component then

         --  Case of dealing with entry family within the current tasks

         E_Name := Prefix (Entry_Name);

      else
         E_Name := Entry_Name;
      end if;

      if Is_Entity_Name (E_Name) then
         --  Entry call to an entry (or entry family) in the current task.
         --  This is legal even though the task will deadlock. Rewrite as
         --  call to current task.

         --  This can also be a call to an entry in  an enclosing task.
         --  If this is a single task, we have to retrieve its name,
         --  because the scope of the entry is the task type, not the
         --  object. If the enclosing task is a task type, the identity
         --  of the task is given by its own self variable.

         --  Finally this can be a requeue on an entry of the same task
         --  or protected object.

         S := Scope (Entity (E_Name));

         for J in reverse 0 .. Scope_Stack.Last loop

            if Is_Task_Type (Scope_Stack.Table (J).Entity)
              and then not Comes_From_Source (S)
            then
               --  S is an enclosing task or protected object. The concurrent
               --  declaration has been converted into a type declaration, and
               --  the object itself has an object declaration that follows
               --  the type in the same declarative part.

               Tsk := Next_Entity (S);

               while Etype (Tsk) /= S loop
                  Next_Entity (Tsk);
               end loop;

               S := Tsk;
               exit;

            elsif S = Scope_Stack.Table (J).Entity then

               --  Call to current task. Will be transformed into call to Self

               exit;

            end if;
         end loop;

         New_N :=
           Make_Selected_Component (Loc,
             Prefix => New_Occurrence_Of (S, Loc),
             Selector_Name =>
               New_Occurrence_Of (Entity (E_Name), Loc));
         Rewrite (E_Name, New_N);
         Analyze (E_Name);

      elsif Nkind (Entry_Name) = N_Selected_Component
        and then Is_Overloaded (Prefix (Entry_Name))
      then
         --  Use the entry name (which must be unique at this point) to
         --  find the prefix that returns the corresponding task type or
         --  protected type.

         declare
            Pref : Node_Id := Prefix (Entry_Name);
            I    : Interp_Index;
            It   : Interp;
            Ent  : Entity_Id :=  Entity (Selector_Name (Entry_Name));

         begin
            Get_First_Interp (Pref, I, It);

            while Present (It.Typ) loop

               if Scope (Ent) = It.Typ then
                  Set_Etype (Pref, It.Typ);
                  exit;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;
      end if;

      if Nkind (Entry_Name) = N_Selected_Component then
         Resolve (Prefix (Entry_Name), Etype (Prefix (Entry_Name)));

      elsif Nkind (Entry_Name) = N_Indexed_Component then
         Nam := Entity (Selector_Name (Prefix (Entry_Name)));
         Resolve (Prefix (Prefix (Entry_Name)),
                   Etype (Prefix (Prefix (Entry_Name))));

         Index :=  First (Expressions (Entry_Name));
         Resolve (Index, Entry_Index_Type (Nam));

         --  Up to this point the expression could have been the actual
         --  in a simple entry call, and be given by a named association.

         if Nkind (Index) = N_Parameter_Association then
            Error_Msg_N ("expect expression for entry index", Index);
         else
            Apply_Range_Check (Index, Actual_Index_Type (Nam));
         end if;

      else
         pragma Assert (False);
         raise Program_Error;
      end if;

   end Resolve_Entry;

   ------------------------
   -- Resolve_Entry_Call --
   ------------------------

   procedure Resolve_Entry_Call (N : Node_Id; Typ : Entity_Id) is
      Entry_Name  : constant Node_Id    := Name (N);
      Loc         : constant Source_Ptr := Sloc (Entry_Name);
      Actuals     : List_Id;
      First_Named : Node_Id;
      Nam         : Entity_Id;
      Norm_OK     : Boolean;
      Obj         : Node_Id;
      Was_Over    : Boolean;

   begin
      --  Processing of the name is similar for entry calls and protected
      --  operation calls. Once the entity is determined, we can complete
      --  the resolution of the actuals.

      --  The selector may be overloaded, in the case of a protected object
      --  with overloaded functions. The type of the context is used for
      --  resolution.

      if Nkind (Entry_Name) = N_Selected_Component
        and then Is_Overloaded (Selector_Name (Entry_Name))
        and then Typ /= Standard_Void_Type
      then
         declare
            I  : Interp_Index;
            It : Interp;

         begin
            Get_First_Interp (Selector_Name (Entry_Name), I, It);

            while Present (It.Typ) loop

               if Covers (Typ, It.Typ) then
                  Set_Entity (Selector_Name (Entry_Name), It.Nam);
                  Set_Etype  (Entry_Name, It.Typ);

                  Generate_Reference (It.Typ, N, ' ');
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;
      end if;

      Resolve_Entry (Entry_Name);

      if Nkind (Entry_Name) = N_Selected_Component then

         --  Simple entry call.

         Nam := Entity (Selector_Name (Entry_Name));
         Obj := Prefix (Entry_Name);
         Was_Over := Is_Overloaded (Selector_Name (Entry_Name));

      elsif Nkind (Entry_Name) = N_Indexed_Component then

         --  Call to member of entry family.

         Nam := Entity (Selector_Name (Prefix (Entry_Name)));
         Obj := Prefix (Prefix (Entry_Name));
         Was_Over := Is_Overloaded (Selector_Name (Prefix (Entry_Name)));
      end if;

      --  Use context type to disambiguate a protected function that can be
      --  called without actuals and that returns an array type, and where
      --  the argument list may be an indexing of the returned value.

      if Ekind (Nam) = E_Function
        and then Needs_No_Actuals (Nam)
        and then Present (Parameter_Associations (N))
        and then
          ((Is_Array_Type (Etype (Nam))
             and then Covers (Typ, Component_Type (Etype (Nam))))

            or else (Is_Access_Type (Etype (Nam))
                      and then Is_Array_Type (Designated_Type (Etype (Nam)))
                      and then Covers (Typ,
                        Component_Type (Designated_Type (Etype (Nam))))))
      then
         declare
            Index_Node : Node_Id;

         begin
            Index_Node :=
              Make_Indexed_Component (Loc,
                Prefix =>
                  Make_Function_Call (Loc,
                    Name => Relocate_Node (Entry_Name)),
                Expressions => Parameter_Associations (N));

            --  Since we are correcting a node classification error made by
            --  the parser, we call Replace rather than Rewrite.

            Replace (N, Index_Node);
            Set_Etype (Prefix (N), Etype (Nam));
            Set_Etype (N, Typ);
            Resolve_Indexed_Component (N, Typ);
            return;
         end;
      end if;

      --  The operation name may have been overloaded. Order the actuals
      --  according to the formals of the resolved entity.

      if Was_Over then
         Normalize_Actuals (N, Nam, False, Norm_OK);
         pragma Assert (Norm_OK);
      end if;

      Resolve_Actuals (N, Nam);
      Generate_Reference (Nam, Entry_Name);

      if Ekind (Nam) = E_Entry
        or else Ekind (Nam) = E_Entry_Family
      then
         Check_Potentially_Blocking_Operation (N);
      end if;

      --  Verify that a procedure call cannot masquerade as an entry
      --  call where an entry call is expected.

      if Ekind (Nam) = E_Procedure then

         if Nkind (Parent (N)) = N_Entry_Call_Alternative
           and then N = Entry_Call_Statement (Parent (N))
         then
            Error_Msg_N ("entry call required in select statement", N);

         elsif Nkind (Parent (N)) = N_Triggering_Alternative
           and then N = Triggering_Statement (Parent (N))
         then
            Error_Msg_N ("triggering statement cannot be procedure call", N);
         end if;
      end if;

      --  After resolution, entry calls and protected procedure calls
      --  are changed into entry calls, for expansion. The structure
      --  of the node does not change, so it can safely be done in place.
      --  Protected function calls must keep their structure because they
      --  are subexpressions.

      if Ekind (Nam) /= E_Function then

         --  A protected operation that is not a function may modify the
         --  corresponding object, and cannot apply to a constant.
         --  If this is an internal call, the prefix is the type itself.

         if Is_Protected_Type (Scope (Nam))
           and then not Is_Variable (Obj)
           and then (not Is_Entity_Name (Obj)
                       or else not Is_Type (Entity (Obj)))
         then
            Error_Msg_N
              ("prefix of protected procedure or entry call must be variable",
               Entry_Name);
         end if;

         Actuals := Parameter_Associations (N);
         First_Named := First_Named_Actual (N);

         Rewrite (N,
           Make_Entry_Call_Statement (Loc,
             Name                   => Entry_Name,
             Parameter_Associations => Actuals));

         Set_First_Named_Actual (N, First_Named);
         Set_Analyzed (N, True);

      --  Protected functions can return on the secondary stack, in which
      --  case we must trigger the transient scope mechanism

      elsif Expander_Active
        and then Requires_Transient_Scope (Etype (Nam))
      then
         Establish_Transient_Scope (N,
           Sec_Stack => not Functions_Return_By_DSP_On_Target);
      end if;

   end Resolve_Entry_Call;

   -------------------------
   -- Resolve_Equality_Op --
   -------------------------

   --  Both arguments must have the same type, and the boolean context
   --  does not participate in the resolution. The first pass verifies
   --  that the interpretation is not ambiguous, and the type of the left
   --  argument is correctly set, or is Any_Type in case of ambiguity.
   --  If both arguments are strings or aggregates, allocators, or Null,
   --  they are ambiguous even though they carry a single (universal) type.
   --  Diagnose this case here.

   procedure Resolve_Equality_Op (N : Node_Id; Typ : Entity_Id) is
      L : constant Node_Id   := Left_Opnd (N);
      R : constant Node_Id   := Right_Opnd (N);
      T : Entity_Id := Find_Unique_Type (L, R);

      function Find_Unique_Access_Type return Entity_Id;
      --  In the case of allocators, make a last-ditch attempt to find a single
      --  access type with the right designated type. This is semantically
      --  dubious, and of no interest to any real code, but c48008a makes it
      --  all worthwhile.

      -----------------------------
      -- Find_Unique_Access_Type --
      -----------------------------

      function Find_Unique_Access_Type return Entity_Id is
         Acc : Entity_Id;
         E   : Entity_Id;
         S   : Entity_Id := Current_Scope;

      begin
         if Ekind (Etype (R)) =  E_Allocator_Type then
            Acc := Designated_Type (Etype (R));

         elsif Ekind (Etype (L)) =  E_Allocator_Type then
            Acc := Designated_Type (Etype (L));

         else
            return Empty;
         end if;

         while S /= Standard_Standard loop
            E := First_Entity (S);

            while Present (E) loop

               if Is_Type (E)
                 and then Is_Access_Type (E)
                 and then Ekind (E) /= E_Allocator_Type
                 and then Designated_Type (E) = Base_Type (Acc)
               then
                  return E;
               end if;

               Next_Entity (E);
            end loop;

            S := Scope (S);
         end loop;

         return Empty;
      end Find_Unique_Access_Type;

   --  Start of processing for Resolve_Equality_Op

   begin
      Set_Etype (N, Base_Type (Typ));
      Generate_Reference (T, N, ' ');

      if T = Any_Fixed then
         T := Unique_Fixed_Point_Type (L);
      end if;

      if T /= Any_Type then

         if T = Any_String
           or else T = Any_Composite
           or else T = Any_Character
         then

            if T = Any_Character then
               Ambiguous_Character (L);
            else
               Error_Msg_N ("ambiguous operands for equality", N);
            end if;

            Set_Etype (N, Any_Type);
            return;

         elsif T = Any_Access
           or else Ekind (T) = E_Allocator_Type
         then
            T := Find_Unique_Access_Type;

            if No (T) then
               Error_Msg_N ("ambiguous operands for equality", N);
               Set_Etype (N, Any_Type);
               return;
            end if;
         end if;

         if Comes_From_Source (N)
           and then Has_Unchecked_Union (T)
         then
            Error_Msg_N
              ("cannot compare Unchecked_Union values", N);
         end if;

         Resolve (L, T);
         Resolve (R, T);
         Check_Unset_Reference (L);
         Check_Unset_Reference (R);
         Generate_Operator_Reference (N);

         --  If this is an inequality, it may be the implicit inequality
         --  created for a user-defined operation, in which case the corres-
         --  ponding equality operation is not intrinsic, and the operation
         --  cannot be constant-folded. Else fold.

         if Nkind (N) = N_Op_Eq
           or else Comes_From_Source (Entity (N))
           or else Ekind (Entity (N)) = E_Operator
           or else Is_Intrinsic_Subprogram
             (Corresponding_Equality (Entity (N)))
         then
            Eval_Relational_Op (N);
         elsif Nkind (N) = N_Op_Ne
           and then Is_Abstract (Entity (N))
         then
            Error_Msg_NE ("cannot call abstract subprogram &!", N, Entity (N));
         end if;
      end if;
   end Resolve_Equality_Op;

   ----------------------------------
   -- Resolve_Explicit_Dereference --
   ----------------------------------

   procedure Resolve_Explicit_Dereference (N : Node_Id; Typ : Entity_Id) is
      P  : constant Node_Id := Prefix (N);
      I  : Interp_Index;
      It : Interp;

   begin
      --  Now that we know the type, check that this is not a
      --  dereference of an uncompleted type. Note that this
      --  is not entirely correct, because dereferences of
      --  private types are legal in default expressions.
      --  This consideration also applies to similar checks
      --  for allocators, qualified expressions, and type
      --  conversions. ???

      Check_Fully_Declared (Typ, N);

      if Is_Overloaded (P) then

         --  Use the context type to select the prefix that has the
         --  correct designated type.

         Get_First_Interp (P, I, It);
         while Present (It.Typ) loop
            exit when Is_Access_Type (It.Typ)
              and then Covers (Typ, Designated_Type (It.Typ));

            Get_Next_Interp (I, It);
         end loop;

         Resolve (P, It.Typ);
         Set_Etype (N, Designated_Type (It.Typ));

      else
         Resolve (P, Etype (P));
      end if;

      if Is_Access_Type (Etype (P)) then
         Apply_Access_Check (N);
      end if;

      --  If the designated type is a packed unconstrained array type,
      --  and the explicit dereference is not in the context of an
      --  attribute reference, then we must compute and set the actual
      --  subtype, since it is needed by Gigi. The reason we exclude
      --  the attribute case is that this is handled fine by Gigi, and
      --  in fact we use such attributes to build the actual subtype.
      --  We also exclude generated code (which builds actual subtypes
      --  directly if they are needed).

      if Is_Array_Type (Etype (N))
        and then Is_Packed (Etype (N))
        and then not Is_Constrained (Etype (N))
        and then Nkind (Parent (N)) /= N_Attribute_Reference
        and then Comes_From_Source (N)
      then
         Set_Etype (N, Get_Actual_Subtype (N));
      end if;

      --  Note: there is no Eval processing required for an explicit
      --  deference, because the type is known to be an allocators, and
      --  allocator expressions can never be static.

   end Resolve_Explicit_Dereference;

   -------------------------------
   -- Resolve_Indexed_Component --
   -------------------------------

   procedure Resolve_Indexed_Component (N : Node_Id; Typ : Entity_Id) is
      Name       : constant Node_Id := Prefix  (N);
      Expr       : Node_Id;
      Array_Type : Entity_Id;
      Index      : Node_Id;

   begin
      if Is_Overloaded (Name) then

         --  Use the context type to select the prefix that yields the
         --  correct component type.

         declare
            I     : Interp_Index;
            It    : Interp;
            I1    : Interp_Index := 0;
            P     : constant Node_Id := Prefix (N);
            Found : Boolean := False;

         begin
            Get_First_Interp (P, I, It);

            while Present (It.Typ) loop

               if (Is_Array_Type (It.Typ)
                     and then Covers (Typ, Component_Type (It.Typ)))
                 or else (Is_Access_Type (It.Typ)
                            and then Is_Array_Type (Designated_Type (It.Typ))
                            and then Covers
                              (Typ, Component_Type (Designated_Type (It.Typ))))
               then
                  if Found then
                     It := Disambiguate (P, I1, I, Any_Type);

                     if It = No_Interp then
                        Error_Msg_N ("ambiguous prefix for indexing",  N);
                        Set_Etype (N, Typ);
                        return;

                     else
                        Found := True;
                        Array_Type := It.Typ;
                        I1 := I;
                     end if;

                  else
                     Found := True;
                     Array_Type := It.Typ;
                     I1 := I;
                  end if;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;

      else
         Array_Type := Etype (Name);
      end if;

      Resolve (Name, Array_Type);
      Array_Type := Get_Actual_Subtype_If_Available (Name);

      --  If prefix is access type, dereference to get real array type.
      --  Note: we do not apply an access check because the expander always
      --  introduces an explicit dereference, and the check will happen there.

      if Is_Access_Type (Array_Type) then
         Array_Type := Designated_Type (Array_Type);
      end if;

      --  If name was overloaded, set component type correctly now.

      Set_Etype (N, Component_Type (Array_Type));

      Index := First_Index (Array_Type);
      Expr  := First (Expressions (N));

      --  The prefix may have resolved to a string literal, in which case
      --  its etype has a special representation. This is only possible
      --  currently if the prefix is a static concatenation, written in
      --  functional notation.

      if Ekind (Array_Type) = E_String_Literal_Subtype then
         Resolve (Expr, Standard_Positive);

      else
         while Present (Index) and Present (Expr) loop
            Resolve (Expr, Etype (Index));
            Check_Unset_Reference (Expr);

            if Is_Scalar_Type (Etype (Expr)) then
               Apply_Scalar_Range_Check (Expr, Etype (Index));
            else
               Apply_Range_Check (Expr, Get_Actual_Subtype (Index));
            end if;

            Next_Index (Index);
            Next (Expr);
         end loop;
      end if;

      Eval_Indexed_Component (N);

   end Resolve_Indexed_Component;

   -----------------------------
   -- Resolve_Integer_Literal --
   -----------------------------

   procedure Resolve_Integer_Literal (N : Node_Id; Typ : Entity_Id) is
   begin
      Set_Etype (N, Typ);
      Eval_Integer_Literal (N);
   end Resolve_Integer_Literal;

   ---------------------------------
   --  Resolve_Intrinsic_Operator --
   ---------------------------------

   procedure Resolve_Intrinsic_Operator  (N : Node_Id; Typ : Entity_Id) is
      Op : Entity_Id;
      Arg1 : Node_Id := Left_Opnd  (N);
      Arg2 : Node_Id := Right_Opnd (N);

   begin
      Op := Entity (N);

      while Scope (Op) /= Standard_Standard loop
         Op := Homonym (Op);
         pragma Assert (Present (Op));
      end loop;

      Set_Entity (N, Op);

      if Typ /= Etype (Arg1) or else Typ = Etype (Arg2) then
         Rewrite (Left_Opnd  (N), Convert_To (Typ, Arg1));
         Rewrite (Right_Opnd (N), Convert_To (Typ, Arg2));

         Analyze (Left_Opnd  (N));
         Analyze (Right_Opnd (N));
      end if;

      Resolve_Arithmetic_Op (N, Typ);
   end Resolve_Intrinsic_Operator;

   ------------------------
   -- Resolve_Logical_Op --
   ------------------------

   procedure Resolve_Logical_Op (N : Node_Id; Typ : Entity_Id) is
      B_Typ : Entity_Id;

   begin
      --  Predefined operations on scalar types yield the base type. On
      --  the other hand, logical operations on arrays yield the type of
      --  the arguments (and the context).

      if Is_Array_Type (Typ) then
         B_Typ := Typ;
      else
         B_Typ := Base_Type (Typ);
      end if;

      --  The following test is required because the operands of the operation
      --  may be literals, in which case the resulting type appears to be
      --  compatible with a signed integer type, when in fact it is compatible
      --  only with modular types. If the context itself is universal, the
      --  operation is illegal.

      if not Valid_Boolean_Arg (Typ) then
         Error_Msg_N ("invalid context for logical operation", N);
         Set_Etype (N, Any_Type);
         return;

      elsif Typ = Any_Modular then
         Error_Msg_N
           ("no modular type available in this context", N);
         Set_Etype (N, Any_Type);
         return;
      end if;

      Resolve (Left_Opnd (N), B_Typ);
      Resolve (Right_Opnd (N), B_Typ);

      Check_Unset_Reference (Left_Opnd  (N));
      Check_Unset_Reference (Right_Opnd (N));

      Set_Etype (N, B_Typ);
      Generate_Operator_Reference (N);
      Eval_Logical_Op (N);
   end Resolve_Logical_Op;

   ---------------------------
   -- Resolve_Membership_Op --
   ---------------------------

   --  The context can only be a boolean type, and does not determine
   --  the arguments. Arguments should be unambiguous, but the preference
   --  rule for universal types applies.

   procedure Resolve_Membership_Op (N : Node_Id; Typ : Entity_Id) is
      L : constant Node_Id   := Left_Opnd (N);
      R : constant Node_Id   := Right_Opnd (N);
      T : Entity_Id;

   begin
      if not Is_Overloaded (R)
        and then
          (Etype (R) = Universal_Integer or else
           Etype (R) = Universal_Real)
        and then Is_Overloaded (L)
      then
         T := Etype (R);
      else
         T := Intersect_Types (L, R);
      end if;

      Resolve (L, T);
      Check_Unset_Reference (L);

      if Nkind (R) = N_Range
        and then not Is_Scalar_Type (T)
      then
         Error_Msg_N ("scalar type required for range", R);
      end if;

      if Is_Entity_Name (R) then
         Freeze_Expression (R);
      else
         Resolve (R, T);
         Check_Unset_Reference (R);
      end if;

      Eval_Membership_Op (N);
   end Resolve_Membership_Op;

   ------------------
   -- Resolve_Null --
   ------------------

   procedure Resolve_Null (N : Node_Id; Typ : Entity_Id) is
   begin

      --  For now allow circumvention of the restriction against
      --  anonymous null access values via a debug switch to allow
      --  for easier trasition (in particular for GNULLI packages
      --  that violate the rule). ???

      if not Debug_Flag_J
        and then Ekind (Typ) = E_Anonymous_Access_Type
        and then Comes_From_Source (N)
      then
         Error_Msg_N ("null cannot be of an anonymous access type", N);
      end if;

      --  In a distributed context, null for a remote access to subprogram
      --  may need to be replaced with a special record aggregate. In this
      --  case, return after having done the transformation.

      if (Ekind (Typ) = E_Record_Type
            or else Is_Remote_Access_To_Subprogram_Type (Typ))
        and then Remote_AST_Null_Value (N, Typ)
      then
         return;
      end if;

      --  The null literal takes its type from the context.

      Set_Etype (N, Typ);
   end Resolve_Null;

   -----------------------------
   -- Resolve_Operator_Symbol --
   -----------------------------

   procedure Resolve_Operator_Symbol (N : Node_Id; Typ : Entity_Id) is
   begin
      null;
   end Resolve_Operator_Symbol;

   -----------------------
   -- Resolve_Op_Concat --
   -----------------------

   procedure Resolve_Op_Concat (N : Node_Id; Typ : Entity_Id) is
      Btyp : constant Entity_Id := Base_Type (Typ);
      Op1  : constant Node_Id := Left_Opnd (N);
      Op2  : constant Node_Id := Right_Opnd (N);

      procedure Resolve_Concatenation_Arg (Arg : Node_Id; Is_Comp : Boolean);
      --  Internal procedure to resolve one operand of concatenation operator.
      --  The operand is either of the array type or of the component type.
      --  If the operand is an aggregate, and the component type is composite,
      --  this is ambiguous if component type has aggregates.

      -------------------------------
      -- Resolve_Concatenation_Arg --
      -------------------------------

      procedure Resolve_Concatenation_Arg (Arg : Node_Id; Is_Comp : Boolean) is
      begin
         if In_Instance then
            if Is_Comp
              or else (not Is_Overloaded (Arg)
               and then Etype (Arg) /= Any_Composite
               and then Covers (Component_Type (Typ), Etype (Arg)))
            then
               Resolve (Arg, Component_Type (Typ));
            else
               Resolve (Arg, Btyp);
            end if;

         elsif Has_Compatible_Type (Arg, Component_Type (Typ)) then

            if Nkind (Arg) = N_Aggregate
              and then Is_Composite_Type (Component_Type (Typ))
            then
               if Is_Private_Type (Component_Type (Typ)) then
                  Resolve (Arg, Btyp);

               else
                  Error_Msg_N ("ambiguous aggregate must be qualified", Arg);
                  Set_Etype (Arg, Any_Type);
               end if;

            else
               if Is_Overloaded (Arg)
                 and then Has_Compatible_Type (Arg, Typ)
                 and then Etype (Arg) /= Any_Type
               then
                  Error_Msg_N ("ambiguous operand for concatenation!", Arg);

                  declare
                     I  : Interp_Index;
                     It : Interp;

                  begin
                     Get_First_Interp (Arg, I, It);

                     while Present (It.Nam) loop

                        if Base_Type (Etype (It.Nam)) = Base_Type (Typ)
                          or else Base_Type (Etype (It.Nam)) =
                            Base_Type (Component_Type (Typ))
                        then
                           Error_Msg_Sloc := Sloc (It.Nam);
                           Error_Msg_N ("\possible interpretation#", Arg);
                        end if;

                        Get_Next_Interp (I, It);
                     end loop;
                  end;
               end if;

               Resolve (Arg, Component_Type (Typ));

               if Arg = Left_Opnd (N) then
                  Set_Is_Component_Left_Opnd (N);
               else
                  Set_Is_Component_Right_Opnd (N);
               end if;
            end if;

         else
            Resolve (Arg, Btyp);
         end if;

         Check_Unset_Reference (Arg);
      end Resolve_Concatenation_Arg;

   --  Start of processing for Resolve_Op_Concat

   begin
      Set_Etype (N, Btyp);

      if Is_Limited_Composite (Btyp) then
         Error_Msg_N ("concatenation not available for limited array", N);
      end if;

      --  If the operands are themselves concatenations, resolve them as
      --  such directly. This removes several layers of recursion and allows
      --  GNAT to handle larger multiple concatenations.

      if Nkind (Op1) = N_Op_Concat
        and then not Is_Array_Type (Component_Type (Typ))
        and then Entity (Op1) = Entity (N)
      then
         Resolve_Op_Concat (Op1, Typ);
      else
         Resolve_Concatenation_Arg
           (Op1,  Is_Component_Left_Opnd  (N));
      end if;

      if Nkind (Op2) = N_Op_Concat
        and then not Is_Array_Type (Component_Type (Typ))
        and then Entity (Op2) = Entity (N)
      then
         Resolve_Op_Concat (Op2, Typ);
      else
         Resolve_Concatenation_Arg
           (Op2, Is_Component_Right_Opnd  (N));
      end if;

      Generate_Operator_Reference (N);

      if Is_String_Type (Typ) then
         Eval_Concatenation (N);
      end if;

      --  If this is not a static concatenation, insure that static
      --  string operands have their subtypes properly constructed.

      if Nkind (N) /= N_String_Literal then
         Set_String_Literal_Subtype (Op1, Typ);
         Set_String_Literal_Subtype (Op2, Typ);
      end if;
   end Resolve_Op_Concat;

   ----------------------
   -- Resolve_Op_Expon --
   ----------------------

   procedure Resolve_Op_Expon (N : Node_Id; Typ : Entity_Id) is
      B_Typ : constant Entity_Id := Base_Type (Typ);

   begin
      --  Catch attempts to do fixed-point exponentation with universal
      --  operands, which is a case where the illegality is not caught
      --  during normal operator analysis.

      if Is_Fixed_Point_Type (Typ) and then Comes_From_Source (N) then
         Error_Msg_N ("exponentiation not available for fixed point", N);
         return;
      end if;

      if Etype (Left_Opnd (N)) = Universal_Integer
        or else Etype (Left_Opnd (N)) = Universal_Real
      then
         Check_For_Visible_Operator (N, B_Typ);
      end if;

      --  We do the resolution using the base type, because intermediate values
      --  in expressions always are of the base type, not a subtype of it.

      Resolve (Left_Opnd (N), B_Typ);
      Resolve (Right_Opnd (N), Standard_Integer);

      Check_Unset_Reference (Left_Opnd  (N));
      Check_Unset_Reference (Right_Opnd (N));

      Set_Etype (N, B_Typ);
      Generate_Operator_Reference (N);
      Eval_Op_Expon (N);

      --  Set overflow checking bit. Much cleverer code needed here eventually
      --  and perhaps the Resolve routines should be separated for the various
      --  arithmetic operations, since they will need different processing. ???

      if Nkind (N) in N_Op then
         if not Overflow_Checks_Suppressed (Etype (N)) then
            Set_Do_Overflow_Check (N, True);
         end if;
      end if;

   end Resolve_Op_Expon;

   --------------------
   -- Resolve_Op_Not --
   --------------------

   procedure Resolve_Op_Not (N : Node_Id; Typ : Entity_Id) is
      B_Typ : Entity_Id;

      function Parent_Is_Boolean return Boolean;
      --  This function determines if the parent node is a boolean operator
      --  or operation (comparison op, membership test, or short circuit form)
      --  and the not in question is the left operand of this operation.
      --  Note that if the not is in parens, then false is returned.

      function Parent_Is_Boolean return Boolean is
      begin
         if Paren_Count (N) /= 0 then
            return False;

         else
            case Nkind (Parent (N)) is
               when N_Op_And   |
                    N_Op_Eq    |
                    N_Op_Ge    |
                    N_Op_Gt    |
                    N_Op_Le    |
                    N_Op_Lt    |
                    N_Op_Ne    |
                    N_Op_Or    |
                    N_Op_Xor   |
                    N_In       |
                    N_Not_In   |
                    N_And_Then |
                    N_Or_Else =>

                  return Left_Opnd (Parent (N)) = N;

               when others =>
                  return False;
            end case;
         end if;
      end Parent_Is_Boolean;

   --  Start of processing for Resolve_Op_Not

   begin
      --  Predefined operations on scalar types yield the base type. On
      --  the other hand, logical operations on arrays yield the type of
      --  the arguments (and the context).

      if Is_Array_Type (Typ) then
         B_Typ := Typ;
      else
         B_Typ := Base_Type (Typ);
      end if;

      if not Valid_Boolean_Arg (Typ) then
         Error_Msg_N ("invalid operand type for operator&", N);
         Set_Etype (N, Any_Type);
         return;

      elsif (Typ = Universal_Integer
        or else Typ = Any_Modular)
      then
         if Parent_Is_Boolean then
            Error_Msg_N
              ("operand of not must be enclosed in parentheses",
               Right_Opnd (N));
         else
            Error_Msg_N
              ("no modular type available in this context", N);
         end if;

         Set_Etype (N, Any_Type);
         return;

      else
         if not Is_Boolean_Type (Typ)
           and then Parent_Is_Boolean
         then
            Error_Msg_N ("?not expression should be parenthesized here", N);
         end if;

         Resolve (Right_Opnd (N), B_Typ);
         Check_Unset_Reference (Right_Opnd (N));
         Set_Etype (N, B_Typ);
         Generate_Operator_Reference (N);
         Eval_Op_Not (N);
      end if;
   end Resolve_Op_Not;

   ----------------------------------
   -- Resolve_Qualified_Expression --
   ----------------------------------

   procedure Resolve_Qualified_Expression (N : Node_Id; Typ : Entity_Id) is
      Target_Typ : constant Entity_Id := Entity (Subtype_Mark (N));
      Expr       : constant Node_Id   := Expression (N);

   begin
      Resolve (Expr, Target_Typ);

      --  A qualified expression requires an exact match of the type,
      --  class-wide matching is not allowed.

      if Is_Class_Wide_Type (Target_Typ)
        and then Base_Type (Etype (Expr)) /= Base_Type (Target_Typ)
      then
         Wrong_Type (Expr, Target_Typ);
      end if;

      --  If the target type is unconstrained, then we reset the type of
      --  the result from the type of the expression. For other cases, the
      --  actual subtype of the expression is the target type.

      if Is_Composite_Type (Target_Typ)
        and then not Is_Constrained (Target_Typ)
      then
         Set_Etype (N, Etype (Expr));
      end if;

      Eval_Qualified_Expression (N);
   end Resolve_Qualified_Expression;

   -------------------
   -- Resolve_Range --
   -------------------

   procedure Resolve_Range (N : Node_Id; Typ : Entity_Id) is
      L : constant Node_Id := Low_Bound (N);
      H : constant Node_Id := High_Bound (N);

   begin
      Set_Etype (N, Typ);
      Resolve (L, Typ);
      Resolve (H, Typ);

      Check_Unset_Reference (L);
      Check_Unset_Reference (H);

      --  We have to check the bounds for being within the base range as
      --  required for a non-static context. Normally this is automatic
      --  and done as part of evaluating expressions, but the N_Range
      --  node is an exception, since in GNAT we consider this node to
      --  be a subexpression, even though in Ada it is not. The circuit
      --  in Sem_Eval could check for this, but that would put the test
      --  on the main evaluation path for expressions.

      Check_Non_Static_Context (L);
      Check_Non_Static_Context (H);

   end Resolve_Range;

   --------------------------
   -- Resolve_Real_Literal --
   --------------------------

   procedure Resolve_Real_Literal (N : Node_Id; Typ : Entity_Id) is
      Actual_Typ : constant Entity_Id := Etype (N);

   begin
      --  Special processing for fixed-point literals to make sure that the
      --  value is an exact multiple of small where this is required. We
      --  skip this for the universal real case, and also for generic types.

      if Is_Fixed_Point_Type (Typ)
        and then Typ /= Universal_Fixed
        and then Typ /= Any_Fixed
        and then not Is_Generic_Type (Typ)
      then
         declare
            Val   : constant Ureal := Realval (N);
            Cintr : constant Ureal := Val / Small_Value (Typ);
            Cint  : constant Uint  := UR_Trunc (Cintr);
            Den   : constant Uint  := Norm_Den (Cintr);
            Stat  : Boolean;

         begin
            --  Case of literal is not an exact multiple of the Small

            if Den /= 1 then

               --  For a source program literal for a decimal fixed-point
               --  type, this is statically illegal (RM 4.9(36)).

               if Is_Decimal_Fixed_Point_Type (Typ)
                 and then Actual_Typ = Universal_Real
                 and then Comes_From_Source (N)
               then
                  Error_Msg_N ("value has extraneous low order digits", N);
               end if;

               --  Replace literal by a value that is the exact representation
               --  of a value of the type, i.e. a multiple of the small value,
               --  by truncation, since Machine_Rounds is false for all GNAT
               --  fixed-point types (RM 4.9(38)).

               Stat := Is_Static_Expression (N);
               Rewrite (N,
                 Make_Real_Literal (Sloc (N),
                   Realval => Small_Value (Typ) * Cint));

               Set_Is_Static_Expression (N, Stat);
            end if;

            --  In all cases, set the corresponding integer field

            Set_Corresponding_Integer_Value (N, Cint);
         end;
      end if;

      --  Now replace the actual type by the expected type as usual

      Set_Etype (N, Typ);
      Eval_Real_Literal (N);
   end Resolve_Real_Literal;

   -----------------------
   -- Resolve_Reference --
   -----------------------

   procedure Resolve_Reference (N : Node_Id; Typ : Entity_Id) is
      P : constant Node_Id := Prefix (N);

   begin
      --  Replace general access with specific type

      if Ekind (Etype (N)) = E_Allocator_Type then
         Set_Etype (N, Base_Type (Typ));
      end if;

      Resolve (P, Designated_Type (Etype (N)));

      --  If we are taking the reference of a volatile entity, then treat
      --  it as a potential modification of this entity. This is much too
      --  conservative, but is neccessary because remove side effects can
      --  result in transformations of normal assignments into reference
      --  sequences that otherwise fail to notice the modification.

      if Is_Entity_Name (P) and then Is_Volatile (Entity (P)) then
         Note_Possible_Modification (P);
      end if;
   end Resolve_Reference;

   --------------------------------
   -- Resolve_Selected_Component --
   --------------------------------

   procedure Resolve_Selected_Component (N : Node_Id; Typ : Entity_Id) is
      Comp  : Entity_Id;
      Comp1 : Entity_Id;
      P     : constant Node_Id   := Prefix  (N);
      S     : constant Node_Id   := Selector_Name (N);
      T     : Entity_Id := Etype (P);
      I     : Interp_Index;
      I1    : Interp_Index;
      It    : Interp;
      It1   : Interp;
      Found : Boolean;

   begin
      if Is_Overloaded (P) then

         --  Use the context type to select the prefix that has a selector
         --  of the correct name and type.

         Found := False;
         Get_First_Interp (P, I, It);

         Search : while Present (It.Typ) loop
            if Is_Access_Type (It.Typ) then
               T := Designated_Type (It.Typ);
            else
               T := It.Typ;
            end if;

            if Is_Record_Type (T) then
               Comp := First_Entity (T);

               while Present (Comp) loop

                  if Chars (Comp) = Chars (S)
                    and then Covers (Etype (Comp), Typ)
                  then
                     if not Found then
                        Found := True;
                        I1  := I;
                        It1 := It;
                        Comp1 := Comp;

                     else
                        It := Disambiguate (P, I1, I, Any_Type);

                        if It = No_Interp then
                           Error_Msg_N
                             ("ambiguous prefix for selected component",  N);
                           Set_Etype (N, Typ);
                           return;

                        else
                           It1 := It;

                           if Scope (Comp1) /= It1.Typ then

                              --  Resolution chooses the new interpretation.
                              --  Find the component with the right name.

                              Comp1 := First_Entity (It1.Typ);

                              while Present (Comp1)
                                and then Chars (Comp1) /= Chars (S)
                              loop
                                 Comp1 := Next_Entity (Comp1);
                              end loop;
                           end if;

                           exit Search;
                        end if;
                     end if;
                  end if;

                  Comp := Next_Entity (Comp);
               end loop;

            end if;

            Get_Next_Interp (I, It);

         end loop Search;

         Resolve (P, It1.Typ);
         Set_Etype (N, Typ);
         Set_Entity (S, Comp1);

      else
         --  Resolve prefix with its type.

         Resolve (P, T);
      end if;

      --  Deal with access type case

      if Is_Access_Type (Etype (P)) then
         Apply_Access_Check (N);
         T := Designated_Type (Etype (P));
      else
         T := Etype (P);
      end if;

      if Has_Discriminants (T)
        and then Present (Original_Record_Component (Entity (S)))
        and then Ekind (Original_Record_Component (Entity (S))) = E_Component
        and then Present (Discriminant_Checking_Func
                           (Original_Record_Component (Entity (S))))
        and then not Discriminant_Checks_Suppressed (T)
      then
         Set_Do_Discriminant_Check (N);
      end if;

      if Ekind (Entity (S)) = E_Void then
         Error_Msg_N ("premature use of component", S);
      end if;

      --  If the prefix is a record conversion, this may be a renamed
      --  discriminant whose bounds differ from those of the original
      --  one, so we must ensure that a range check is performed.

      if Nkind (P) = N_Type_Conversion
        and then Ekind (Entity (S)) = E_Discriminant
      then
         Set_Etype (N, Base_Type (Typ));
      end if;

      --  Note: No Eval processing is required, because the prefix is of a
      --  record type, or protected type, and neither can possibly be static.

   end Resolve_Selected_Component;

   -------------------
   -- Resolve_Shift --
   -------------------

   procedure Resolve_Shift (N : Node_Id; Typ : Entity_Id) is
      B_Typ : constant Entity_Id := Base_Type (Typ);
      L     : constant Node_Id   := Left_Opnd  (N);
      R     : constant Node_Id   := Right_Opnd (N);

   begin
      --  We do the resolution using the base type, because intermediate values
      --  in expressions always are of the base type, not a subtype of it.

      Resolve (L, B_Typ);
      Resolve (R, Standard_Natural);

      Check_Unset_Reference (L);
      Check_Unset_Reference (R);

      Set_Etype (N, B_Typ);
      Generate_Operator_Reference (N);
      Eval_Shift (N);
   end Resolve_Shift;

   ---------------------------
   -- Resolve_Short_Circuit --
   ---------------------------

   procedure Resolve_Short_Circuit (N : Node_Id; Typ : Entity_Id) is
      B_Typ : constant Entity_Id := Base_Type (Typ);
      L     : constant Node_Id   := Left_Opnd  (N);
      R     : constant Node_Id   := Right_Opnd (N);

   begin
      Resolve (L, B_Typ);
      Resolve (R, B_Typ);

      Check_Unset_Reference (L);
      Check_Unset_Reference (R);

      Set_Etype (N, B_Typ);
      Eval_Short_Circuit (N);
   end Resolve_Short_Circuit;

   -------------------
   -- Resolve_Slice --
   -------------------

   procedure Resolve_Slice (N : Node_Id; Typ : Entity_Id) is
      Name       : constant Node_Id := Prefix (N);
      Drange     : constant Node_Id := Discrete_Range (N);
      Array_Type : Entity_Id;
      Index      : Node_Id;

   begin
      if Is_Overloaded (Name) then

         --  Use the context type to select the prefix that yields the
         --  correct array type.

         declare
            I      : Interp_Index;
            I1     : Interp_Index := 0;
            It     : Interp;
            P      : constant Node_Id := Prefix (N);
            Found  : Boolean := False;

         begin
            Get_First_Interp (P, I,  It);

            while Present (It.Typ) loop

               if (Is_Array_Type (It.Typ)
                    and then Covers (Typ,  It.Typ))
                 or else (Is_Access_Type (It.Typ)
                           and then Is_Array_Type (Designated_Type (It.Typ))
                           and then Covers (Typ, Designated_Type (It.Typ)))
               then
                  if Found then
                     It := Disambiguate (P, I1, I, Any_Type);

                     if It = No_Interp then
                        Error_Msg_N ("ambiguous prefix for slicing",  N);
                        Set_Etype (N, Typ);
                        return;
                     else
                        Found := True;
                        Array_Type := It.Typ;
                        I1 := I;
                     end if;
                  else
                     Found := True;
                     Array_Type := It.Typ;
                     I1 := I;
                  end if;
               end if;

               Get_Next_Interp (I, It);
            end loop;
         end;

      else
         Array_Type := Etype (Name);
      end if;

      Resolve (Name, Array_Type);

      if Is_Access_Type (Array_Type) then
         Apply_Access_Check (N);
         Array_Type := Designated_Type (Array_Type);

      elsif Is_Entity_Name (Name)
        or else (Nkind (Name) = N_Function_Call
                  and then not Is_Constrained (Etype (Name)))
      then
         Array_Type := Get_Actual_Subtype (Name);
      end if;

      --  If name was overloaded, set slice type correctly now

      Set_Etype (N, Array_Type);

      --  If the range is specified by a subtype mark, no resolution
      --  is necessary.

      if not Is_Entity_Name (Drange) then
         Index := First_Index (Array_Type);
         Resolve (Drange, Base_Type (Etype (Index)));

         if Nkind (Drange) = N_Range then
            Apply_Range_Check (Drange, Etype (Index));
         end if;
      end if;

      Set_Slice_Subtype (N);
      Eval_Slice (N);

   end Resolve_Slice;

   -----------------------
   -- Set_Slice_Subtype --
   -----------------------

   --  Build an implicit subtype declaration to represent the type delivered
   --  by the slice. This is an abbreviated version of an array subtype. We
   --  define an index subtype for the slice,  using either the subtype name
   --  or the discrete range of the slice. To be consistent with index usage
   --  elsewhere, we create a list header to hold the single index. This list
   --  is not otherwise attached to the syntax tree.

   procedure Set_Slice_Subtype (N : Node_Id) is
      Loc           : constant Source_Ptr := Sloc (N);
      Index         : Node_Id;
      Index_List    : List_Id := New_List;
      Index_Subtype : Entity_Id;
      Index_Type    : Entity_Id;
      Slice_Subtype : Entity_Id;
      Drange        : constant Node_Id := Discrete_Range (N);

   begin
      if Is_Entity_Name (Drange) then
         Index_Subtype := Entity (Drange);

      else
         --  We force the evaluation of a range. This is definitely needed in
         --  the renamed case, and seems safer to do unconditionally. Note in
         --  any case that since we will create and insert an Itype referring
         --  to this range, we must make sure any side effect removal actions
         --  are inserted before the Itype definition.

         if Nkind (Drange) = N_Range then
            Force_Evaluation (Low_Bound (Drange));
            Force_Evaluation (High_Bound (Drange));
         end if;

         Index_Type := Base_Type (Etype (Drange));

         Index_Subtype := Create_Itype (Subtype_Kind (Ekind (Index_Type)), N);

         Set_Scalar_Range (Index_Subtype, Drange);
         Set_Etype        (Index_Subtype, Index_Type);
         Set_Size_Info    (Index_Subtype, Index_Type);
         Set_RM_Size      (Index_Subtype, RM_Size (Index_Type));
      end if;

      Slice_Subtype := Create_Itype (E_Array_Subtype, N);

      Index := New_Occurrence_Of (Index_Subtype, Loc);
      Set_Etype (Index, Index_Subtype);
      Append (Index, Index_List);

      Set_Component_Type (Slice_Subtype, Component_Type (Etype (N)));
      Set_First_Index    (Slice_Subtype, Index);
      Set_Etype          (Slice_Subtype, Base_Type (Etype (N)));
      Set_Is_Constrained (Slice_Subtype, True);
      Init_Esize         (Slice_Subtype);
      Init_Alignment     (Slice_Subtype);

      Check_Compile_Time_Size (Slice_Subtype);

      --  The Etype of the existing Slice node is reset to this slice
      --  subtype. Its bounds are obtained from its first index.

      Set_Etype (N, Slice_Subtype);

      --  In the packed case, this must be immediately frozen

      --  Couldn't we always freeze here??? and if we did, then the above
      --  call to Check_Compile_Time_Size could be eliminated, which would
      --  be nice, because then that routine could be made private to Freeze.

      if Is_Packed (Slice_Subtype) and not In_Default_Expression then
         Freeze_Itype (Slice_Subtype, N);
      end if;

   end Set_Slice_Subtype;

   ----------------------------
   -- Resolve_String_Literal --
   ----------------------------

   procedure Resolve_String_Literal (N : Node_Id; Typ : Entity_Id) is
      C_Typ      : constant Entity_Id  := Component_Type (Typ);
      R_Typ      : constant Entity_Id  := Root_Type (C_Typ);
      Loc        : constant Source_Ptr := Sloc (N);
      Str        : constant String_Id  := Strval (N);
      Strlen     : constant Nat        := String_Length (Str);
      Subtype_Id : Entity_Id;
      Need_Check : Boolean;

   begin
      --  For a string appearing in a concatenation, defer creation of the
      --  string_literal_subtype until the end of the resolution of the
      --  concatenation, because the literal may be constant-folded away.
      --  This is a useful optimization for long concatenation expressions.

      --  If the string is an aggregate built for a single character  (which
      --  happens in a non-static context) or a is null string to which special
      --  checks may apply, we build the subtype. Wide strings must also get
      --  a string subtype if they come from a one character aggregate. Strings
      --  generated by attributes might be static, but it is often hard to
      --  determine whether the enclosing context is static, so we generate
      --  subtypes for them as well, thus losing some rarer optimizations ???
      --  Same for strings that come from a static conversion.

      Need_Check :=
        (Strlen = 0 and then Typ /= Standard_String)
          or else Nkind (Parent (N)) /= N_Op_Concat
          or else (N /= Left_Opnd (Parent (N))
                    and then N /= Right_Opnd (Parent (N)))
          or else (Typ = Standard_Wide_String
                    and then Nkind (Original_Node (N)) /= N_String_Literal);

      --  If the resolving type is itself a string literal subtype, we
      --  can just reuse it, since there is no point in creating another.

      if Ekind (Typ) = E_String_Literal_Subtype then
         Subtype_Id := Typ;

      elsif Nkind (Parent (N)) = N_Op_Concat
        and then not Need_Check
        and then Nkind (Original_Node (N)) /= N_Character_Literal
        and then Nkind (Original_Node (N)) /= N_Attribute_Reference
        and then Nkind (Original_Node (N)) /= N_Qualified_Expression
        and then Nkind (Original_Node (N)) /= N_Type_Conversion
      then
         Subtype_Id := Typ;

      --  Otherwise we must create a string literal subtype. Note that the
      --  whole idea of string literal subtypes is simply to avoid the need
      --  for building a full fledged array subtype for each literal.
      else
         Set_String_Literal_Subtype (N, Typ);
         Subtype_Id := Etype (N);
      end if;

      if Nkind (Parent (N)) /= N_Op_Concat
        or else Need_Check
      then
         Set_Etype (N, Subtype_Id);
         Eval_String_Literal (N);
      end if;

      if Is_Limited_Composite (Typ)
        or else Is_Private_Composite (Typ)
      then
         Error_Msg_N ("string literal not available for private array", N);
         Set_Etype (N, Any_Type);
         return;
      end if;

      --  The validity of a null string has been checked in the
      --  call to  Eval_String_Literal.

      if Strlen = 0 then
         return;

      --  Always accept string literal with component type Any_Character,
      --  which occurs in error situations and in comparisons of literals,
      --  both of which should accept all literals.

      elsif R_Typ = Any_Character then
         return;

      --  If the type is bit-packed, then we always tranform the string
      --  literal into a full fledged aggregate.

      elsif Is_Bit_Packed_Array (Typ) then
         null;

      --  Deal with cases of Wide_String and String

      else
         --  For Standard.Wide_String, or any other type whose component
         --  type is Standard.Wide_Character, we know that all the
         --  characters in the string must be acceptable, since the parser
         --  accepted the characters as valid character literals.

         if R_Typ = Standard_Wide_Character then
            null;

         --  For the case of Standard.String, or any other type whose
         --  component type is Standard.Character, we must make sure that
         --  there are no wide characters in the string, i.e. that it is
         --  entirely composed of characters in range of type String.

         --  If the string literal is the result of a static concatenation,
         --  the test has already been performed on the components, and need
         --  not be repeated.

         elsif R_Typ = Standard_Character
           and then Nkind (Original_Node (N)) /= N_Op_Concat
         then
            for J in 1 .. Strlen loop
               if not In_Character_Range (Get_String_Char (Str, J)) then

                  --  If we are out of range, post error. This is one of the
                  --  very few places that we place the flag in the middle of
                  --  a token, right under the offending wide character.

                  Error_Msg
                    ("literal out of range of type Character",
                     Source_Ptr (Int (Loc) + J));
                  return;
               end if;
            end loop;

         --  If the root type is not a standard character, then we will convert
         --  the string into an aggregate and will let the aggregate code do
         --  the checking.

         else
            null;

         end if;

         --  See if the component type of the array corresponding to the
         --  string has compile time known bounds. If yes we can directly
         --  check whether the evaluation of the string will raise constraint
         --  error. Otherwise we need to transform the string literal into
         --  the corresponding character aggregate and let the aggregate
         --  code do the checking.

         if R_Typ = Standard_Wide_Character
           or else R_Typ = Standard_Character
         then
            --  Check for the case of full range, where we are definitely OK

            if Component_Type (Typ) = Base_Type (Component_Type (Typ)) then
               return;
            end if;

            --  Here the range is not the complete base type range, so check

            declare
               Comp_Typ_Lo : constant Node_Id :=
                               Type_Low_Bound (Component_Type (Typ));
               Comp_Typ_Hi : constant Node_Id :=
                               Type_High_Bound (Component_Type (Typ));

               Char_Val : Uint;

            begin
               if Compile_Time_Known_Value (Comp_Typ_Lo)
                 and then Compile_Time_Known_Value (Comp_Typ_Hi)
               then
                  for J in 1 .. Strlen loop
                     Char_Val := UI_From_Int (Int (Get_String_Char (Str, J)));

                     if Char_Val < Expr_Value (Comp_Typ_Lo)
                       or else Char_Val > Expr_Value (Comp_Typ_Hi)
                     then
                        Apply_Compile_Time_Constraint_Error
                          (N, "character out of range?",
                           Loc => Source_Ptr (Int (Loc) + J));
                     end if;
                  end loop;

                  return;
               end if;
            end;
         end if;
      end if;

      --  If we got here we meed to transform the string literal into the
      --  equivalent qualified positional array aggregate. This is rather
      --  heavy artillery for this situation, but it is hard work to avoid.

      declare
         Lits : List_Id    := New_List;
         P    : Source_Ptr := Loc + 1;
         C    : Char_Code;

      begin
         --  Build the character literals, we give them source locations
         --  that correspond to the string positions, which is a bit tricky
         --  given the possible presence of wide character escape sequences.

         for J in 1 .. Strlen loop
            C := Get_String_Char (Str, J);
            Set_Character_Literal_Name (C);

            Append_To (Lits,
              Make_Character_Literal (P, Name_Find, C));

            if In_Character_Range (C) then
               P := P + 1;
--  ???     else
--             Skip_Wide (P);
            end if;
         end loop;

         Rewrite (N,
           Make_Qualified_Expression (Loc,
             Subtype_Mark => New_Reference_To (Typ, Loc),
             Expression   =>
               Make_Aggregate (Loc, Expressions => Lits)));

         Analyze_And_Resolve (N, Typ);
      end;

   end Resolve_String_Literal;

   --------------------------------
   -- Set_String_Literal_Subtype --
   --------------------------------

   procedure Set_String_Literal_Subtype (N : Node_Id; Typ : Entity_Id) is
      Subtype_Id : Entity_Id;

   begin
      if Nkind (N) /= N_String_Literal then
         return;

      else
         Subtype_Id := Create_Itype (E_String_Literal_Subtype, N);
      end if;

      Set_Component_Type           (Subtype_Id, Component_Type (Typ));
      Set_String_Literal_Length    (Subtype_Id,
        UI_From_Int (String_Length (Strval (N))));
      Set_Etype                    (Subtype_Id, Base_Type (Typ));
      Set_Is_Constrained           (Subtype_Id);

      --  The low bound is set from the low bound of the corresponding
      --  index type. Note that we do not store the high bound in the
      --  string literal subtype, but it can be deduced if necssary
      --  from the length and the low bound.

      Set_String_Literal_Low_Bound
        (Subtype_Id, Type_Low_Bound (Etype (First_Index (Typ))));

      Set_Etype (N, Subtype_Id);
   end Set_String_Literal_Subtype;

   -----------------------------
   -- Resolve_Subprogram_Info --
   -----------------------------

   procedure Resolve_Subprogram_Info (N : Node_Id; Typ : Entity_Id) is
   begin
      Set_Etype (N, Typ);
   end Resolve_Subprogram_Info;

   -----------------------------
   -- Resolve_Type_Conversion --
   -----------------------------

   procedure Resolve_Type_Conversion (N : Node_Id; Typ : Entity_Id) is
      Target_Type : constant Entity_Id := Etype (N);
      Conv_OK     : constant Boolean   := Conversion_OK (N);
      Operand     : Node_Id;
      Opnd_Type   : Entity_Id;
      Rop         : Node_Id;

   begin
      Operand := Expression (N);

      if not Conv_OK
        and then not Valid_Conversion (N, Target_Type, Operand)
      then
         return;
      end if;

      if Etype (Operand) = Any_Fixed then

         --  Mixed-mode operation involving a literal. Context must be a fixed
         --  type which is applied to the literal subsequently.

         if Is_Fixed_Point_Type (Typ) then
            Set_Etype (Operand, Universal_Real);

         elsif Is_Numeric_Type (Typ)
           and then (Nkind (Operand) = N_Op_Multiply
                      or else Nkind (Operand) = N_Op_Divide)
           and then (Etype (Right_Opnd (Operand)) = Universal_Real
                     or else Etype (Left_Opnd (Operand)) = Universal_Real)
         then
            if Unique_Fixed_Point_Type (N) = Any_Type then
               return;    --  expression is ambiguous.
            else
               Set_Etype (Operand, Standard_Duration);
            end if;

            if Etype (Right_Opnd (Operand)) = Universal_Real then
               Rop := New_Copy_Tree (Right_Opnd (Operand));
            else
               Rop := New_Copy_Tree (Left_Opnd (Operand));
            end if;

            Resolve (Rop, Standard_Long_Long_Float);

            if Realval (Rop) /= Ureal_0
              and then abs (Realval (Rop)) < Delta_Value (Standard_Duration)
            then
               Error_Msg_N ("universal real operand can only be interpreted?",
                 Rop);
               Error_Msg_N ("\as Duration, and will lose precision?", Rop);
            end if;

         else
            Error_Msg_N ("invalid context for mixed mode operation", N);
            Set_Etype (Operand, Any_Type);
            return;
         end if;
      end if;

      Opnd_Type := Etype (Operand);
      Resolve (Operand, Opnd_Type);

      --  Note: we do the Eval_Type_Conversion call before applying the
      --  required checks for a subtype conversion. This is important,
      --  since both are prepared under certain circumstances to change
      --  the type conversion to a constraint error node, but in the case
      --  of Eval_Type_Conversion this may reflect an illegality in the
      --  static case, and we would miss the illegality (getting only a
      --  warning message), if we applied the type conversion checks first.

      Eval_Type_Conversion (N);

      --  If after evaluation, we still have a type conversion, then we
      --  may need to apply checks required for a subtype conversion.

      --  Skip these type conversion checks if universal fixed operands
      --  operands involved, since range checks are handled separately for
      --  these cases (in the appropriate Expand routines in unit Exp_Fixd).

      if Nkind (N) = N_Type_Conversion
        and then not Is_Generic_Type (Root_Type (Target_Type))
        and then Target_Type /= Universal_Fixed
        and then Opnd_Type /= Universal_Fixed
      then
         Apply_Type_Conversion_Checks (N);
      end if;

   end Resolve_Type_Conversion;

   ----------------------
   -- Resolve_Unary_Op --
   ----------------------

   procedure Resolve_Unary_Op (N : Node_Id; Typ : Entity_Id) is
      B_Typ : Entity_Id := Base_Type (Typ);
      R     : constant Node_Id := Right_Opnd (N);

   begin
      --  Generate warning for expressions like -5 mod 3

      if Paren_Count (N) = 0
        and then Nkind (N) = N_Op_Minus
        and then Nkind (Right_Opnd (N)) = N_Op_Mod
      then
         Error_Msg_N
           ("?unary minus expression should be parenthesized here", N);
      end if;

      if Etype (R) = Universal_Integer
        or else Etype (R) = Universal_Real
      then
         Check_For_Visible_Operator (N, B_Typ);
      end if;

      Set_Etype (N, B_Typ);
      Resolve (R, B_Typ);
      Check_Unset_Reference (R);
      Generate_Operator_Reference (N);
      Eval_Unary_Op (N);

      --  Set overflow checking bit. Much cleverer code needed here eventually
      --  and perhaps the Resolve routines should be separated for the various
      --  arithmetic operations, since they will need different processing ???

      if Nkind (N) in N_Op then
         if not Overflow_Checks_Suppressed (Etype (N)) then
            Set_Do_Overflow_Check (N, True);
         end if;
      end if;

   end Resolve_Unary_Op;

   ----------------------------------
   -- Resolve_Unchecked_Expression --
   ----------------------------------

   procedure Resolve_Unchecked_Expression
     (N   : Node_Id;
      Typ : Entity_Id)
   is
   begin
      Resolve (Expression (N), Typ, Suppress => All_Checks);
      Set_Etype (N, Typ);
   end Resolve_Unchecked_Expression;

   ---------------------------------------
   -- Resolve_Unchecked_Type_Conversion --
   ---------------------------------------

   procedure Resolve_Unchecked_Type_Conversion
     (N   : Node_Id;
      Typ : Entity_Id)
   is
      Operand   : constant Node_Id   := Expression (N);
      Opnd_Type : constant Entity_Id := Etype (Operand);

   begin
      --  Resolve operand using its own type.

      Resolve (Operand, Opnd_Type);
      Eval_Unchecked_Conversion (N);

   end Resolve_Unchecked_Type_Conversion;

   ------------------------------
   -- Rewrite_Operator_As_Call --
   ------------------------------

   procedure Rewrite_Operator_As_Call (N : Node_Id; Nam : Entity_Id) is
      Loc     :  Source_Ptr := Sloc (N);
      Actuals :  List_Id := New_List;
      New_N   : Node_Id;

   begin
      if Nkind (N) in  N_Binary_Op then
         Append (Left_Opnd (N), Actuals);
      end if;

      Append (Right_Opnd (N), Actuals);

      New_N :=
        Make_Function_Call (Sloc => Loc,
          Name => New_Occurrence_Of (Nam, Loc),
          Parameter_Associations => Actuals);

      Preserve_Comes_From_Source (New_N, N);
      Preserve_Comes_From_Source (Name (New_N), N);
      Rewrite (N, New_N);
      Set_Etype (N, Etype (Nam));
   end Rewrite_Operator_As_Call;

   ------------------------------
   -- Rewrite_Renamed_Operator --
   ------------------------------

   procedure Rewrite_Renamed_Operator (N : Node_Id; Op : Entity_Id) is
      Nam       : constant Name_Id := Chars (Op);
      Is_Binary : constant Boolean := Nkind (N) in N_Binary_Op;
      Op_Node   : Node_Id;

   begin
      if Chars (N) /= Nam then

         --  Rewrite the operator node using the real operator, not its
         --  renaming.

         Op_Node := New_Node (Operator_Kind (Nam, Is_Binary), Sloc (N));
         Set_Chars      (Op_Node, Nam);
         Set_Etype      (Op_Node, Etype (N));
         Set_Entity     (Op_Node, Op);
         Set_Right_Opnd (Op_Node, Right_Opnd (N));

         Generate_Reference (Op, N);

         if Is_Binary then
            Set_Left_Opnd  (Op_Node, Left_Opnd  (N));
         end if;

         Rewrite (N, Op_Node);
      end if;
   end Rewrite_Renamed_Operator;

   -----------------------------
   -- Unique_Fixed_Point_Type --
   -----------------------------

   function Unique_Fixed_Point_Type (N : Node_Id) return Entity_Id is
      T1   : Entity_Id := Empty;
      T2   : Entity_Id;
      Item : Node_Id;
      Scop : Entity_Id;

      procedure Fixed_Point_Error;
      --  If true ambiguity, give details.

      procedure Fixed_Point_Error is
      begin
         Error_Msg_N ("ambiguous universal_fixed_expression", N);
         Error_Msg_NE ("\possible interpretation as}", N, T1);
         Error_Msg_NE ("\possible interpretation as}", N, T2);
      end Fixed_Point_Error;

   begin
      --  In general duration will be directly visible.

      if Current_Entity (Standard_Duration) = Standard_Duration then
         T1 := Standard_Duration;
      end if;

      Scop := Current_Scope;

      --  Look for fixed-point types in enclosing scopes.

      while Scop /= Standard_Standard loop
         T2 := First_Entity (Scop);

         while Present (T2) loop
            if Is_Fixed_Point_Type (T2)
              and then Current_Entity (T2) = T2
              and then Scope (Base_Type (T2)) = Scop
            then
               if Present (T1) then
                  Fixed_Point_Error;
                  return Any_Type;
               else
                  T1 := T2;
               end if;
            end if;

            Next_Entity (T2);
         end loop;

         Scop := Scope (Scop);
      end loop;

      --  Look for visible fixed type declarations in the context.

      Item := First (Context_Items (Cunit (Current_Sem_Unit)));

      while Present (Item) loop

         if Nkind (Item) = N_With_Clause then
            Scop := Entity (Name (Item));
            T2 := First_Entity (Scop);

            while Present (T2) loop
               if Is_Fixed_Point_Type (T2)
                 and then Scope (Base_Type (T2)) = Scop
                 and then (Is_Potentially_Use_Visible (T2)
                             or else In_Use (T2))
               then
                  if Present (T1) then
                     Fixed_Point_Error;
                     return Any_Type;
                  else
                     T1 := T2;
                  end if;
               end if;

               Next_Entity (T2);
            end loop;
         end if;

         Next (Item);
      end loop;

      if Nkind (N) = N_Real_Literal then
         Error_Msg_NE ("real literal interpreted as }?", N, T1);
      else
         Error_Msg_NE ("universal_fixed expression interpreted as }?", N, T1);
      end if;

      return T1;
   end Unique_Fixed_Point_Type;

   ----------------------
   -- Valid_Conversion --
   ----------------------

   function Valid_Conversion
     (N       : Node_Id;
      Target  : Entity_Id;
      Operand : Node_Id)
      return    Boolean
   is
      Target_Type : Entity_Id := Base_Type (Target);
      Opnd_Type   : Entity_Id := Etype (Operand);

      function Conversion_Check
        (Valid : Boolean;
         Msg   : String)
         return  Boolean;
      --  Little routine to post Msg if Valid is False, returns Valid value

      function Valid_Tagged_Conversion
        (Target_Type : Entity_Id;
         Opnd_Type   : Entity_Id)
         return        Boolean;
      --  Specifically test for validity of tagged conversions

      ----------------------
      -- Conversion_Check --
      ----------------------

      function Conversion_Check
        (Valid : Boolean;
         Msg   : String)
         return  Boolean
      is
      begin
         if not Valid then
            Error_Msg_N (Msg, Operand);
         end if;

         return Valid;
      end Conversion_Check;

      -----------------------------
      -- Valid_Tagged_Conversion --
      -----------------------------

      function Valid_Tagged_Conversion
        (Target_Type : Entity_Id;
         Opnd_Type   : Entity_Id)
         return        Boolean
      is
      begin
         --  Upward conversions are allowed (RM 4.6(22)).

         if Covers (Target_Type, Opnd_Type)
           or else Is_Ancestor (Target_Type, Opnd_Type)
         then
            return True;

         --  Downward conversion are allowed if the operand is
         --  is class-wide (RM 4.6(23)).

         elsif Is_Class_Wide_Type (Opnd_Type)
              and then Covers (Opnd_Type, Target_Type)
         then
            return True;

         elsif Covers (Opnd_Type, Target_Type)
           or else Is_Ancestor (Opnd_Type, Target_Type)
         then
            return
              Conversion_Check (False,
                "downward conversion of tagged objects not allowed");
         else
            Error_Msg_NE
              ("invalid tagged conversion, not compatible with}",
               N, First_Subtype (Opnd_Type));
            return False;
         end if;
      end Valid_Tagged_Conversion;

   --  Start of processing for Valid_Conversion

   begin
      Check_Parameterless_Call (Operand);

      if Is_Overloaded (Operand) then
         declare
            I   : Interp_Index;
            I1  : Interp_Index;
            It  : Interp;
            It1 : Interp;
            N1  : Entity_Id;

         begin
            --  Remove procedure calls, which syntactically cannot appear
            --  in this context, but which cannot be removed by type checking,
            --  because the context does not impose a type.

            Get_First_Interp (Operand, I, It);

            while Present (It.Typ) loop

               if It.Typ = Standard_Void_Type then
                  Remove_Interp (I);
               end if;

               Get_Next_Interp (I, It);
            end loop;

            Get_First_Interp (Operand, I, It);
            I1  := I;
            It1 := It;

            if No (It.Typ) then
               Error_Msg_N ("illegal operand in conversion", Operand);
               return False;
            end if;

            Get_Next_Interp (I, It);

            if Present (It.Typ) then
               N1  := It1.Nam;
               It1 :=  Disambiguate (Operand, I1, I, Any_Type);

               if It1 = No_Interp then
                  Error_Msg_N ("ambiguous operand in conversion", Operand);

                  Error_Msg_Sloc := Sloc (It.Nam);
                  Error_Msg_N ("possible interpretation#!", Operand);

                  Error_Msg_Sloc := Sloc (N1);
                  Error_Msg_N ("possible interpretation#!", Operand);

                  return False;
               end if;
            end if;

            Set_Etype (Operand, It1.Typ);
            Opnd_Type := It1.Typ;
         end;
      end if;

      if Chars (Current_Scope) = Name_Unchecked_Conversion then

         --  This check is dubious, what if there were a user defined
         --  scope whose name was Unchecked_Conversion ???

         return True;

      elsif Is_Numeric_Type (Target_Type)  then
         if Opnd_Type = Universal_Fixed then
            return True;
         else
            return Conversion_Check (Is_Numeric_Type (Opnd_Type),
                             "illegal operand for numeric conversion");
         end if;

      elsif Is_Array_Type (Target_Type) then
         if not Is_Array_Type (Opnd_Type)
           or else Opnd_Type = Any_Composite
           or else Opnd_Type = Any_String
         then
            Error_Msg_N
              ("illegal operand for array conversion", Operand);
            return False;

         elsif Number_Dimensions (Target_Type) /=
           Number_Dimensions (Opnd_Type)
         then
            Error_Msg_N
              ("incompatible number of dimensions for conversion", Operand);
            return False;

         else
            declare
               Target_Index      : Node_Id := First_Index (Target_Type);
               Opnd_Index        : Node_Id := First_Index (Opnd_Type);

               Target_Index_Type : Entity_Id;
               Opnd_Index_Type   : Entity_Id;

               Target_Comp_Type  : Entity_Id := Component_Type (Target_Type);
               Opnd_Comp_Type    : Entity_Id := Component_Type (Opnd_Type);

            begin
               while Present (Target_Index) and then Present (Opnd_Index) loop
                  Target_Index_Type := Etype (Target_Index);
                  Opnd_Index_Type   := Etype (Opnd_Index);

                  if not (Is_Integer_Type (Target_Index_Type)
                          and then Is_Integer_Type (Opnd_Index_Type))
                    and then (Root_Type (Target_Index_Type)
                              /= Root_Type (Opnd_Index_Type))
                  then
                     Error_Msg_N
                       ("incompatible index types for array conversion",
                        Operand);
                     return False;
                  end if;

                  Next_Index (Target_Index);
                  Next_Index (Opnd_Index);
               end loop;

               if Base_Type (Target_Comp_Type) /=
                 Base_Type (Opnd_Comp_Type)
               then
                  Error_Msg_N
                    ("incompatible component types for array conversion",
                     Operand);
                  return False;

               elsif
                  Is_Constrained (Target_Comp_Type)
                    /= Is_Constrained (Opnd_Comp_Type)
                  or else not Subtypes_Statically_Match
                                (Target_Comp_Type, Opnd_Comp_Type)
               then
                  Error_Msg_N
                    ("component subtypes must statically match", Operand);
                  return False;

               end if;
            end;
         end if;

         return True;

      elsif (Ekind (Target_Type) = E_General_Access_Type
        or else Ekind (Target_Type) = E_Anonymous_Access_Type)
          and then
            Conversion_Check
              (Is_Access_Type (Opnd_Type)
                 and then Ekind (Opnd_Type) /=
                   E_Access_Subprogram_Type
                 and then Ekind (Opnd_Type) /=
                   E_Access_Protected_Subprogram_Type,
               "must be an access-to-object type")
      then
         if Is_Access_Constant (Opnd_Type)
           and then not Is_Access_Constant (Target_Type)
         then
            Error_Msg_N
              ("access-to-constant operand type not allowed", Operand);
            return False;
         end if;

         --  Check the static accessibility rule of 4.6(17). Note that
         --  the check is not enforced when within an instance body, since
         --  the RM requires such cases to be caught at run time.

         if Ekind (Target_Type) /= E_Anonymous_Access_Type then
            if Type_Access_Level (Opnd_Type)
              > Type_Access_Level (Target_Type)
            then
               --  In an instance, this is a run-time check, but one we
               --  know will fail, so generate an appropriate warning.
               --  The raise will be generated by Expand_N_Type_Conversion.

               if In_Instance_Body then
                  Error_Msg_N
                    ("?cannot convert local pointer to non-local access type",
                     Operand);
                  Error_Msg_N
                    ("?Program_Error will be raised at run time", Operand);

               else
                  Error_Msg_N
                    ("cannot convert local pointer to non-local access type",
                     Operand);
                  return False;
               end if;

            elsif Ekind (Opnd_Type) = E_Anonymous_Access_Type then

               --  When the operand is a selected access discriminant
               --  the check needs to be made against the level of the
               --  object denoted by the prefix of the selected name.
               --  (Object_Access_Level handles checking the prefix
               --  of the operand for this case.)

               if Nkind (Operand) = N_Selected_Component
                 and then Object_Access_Level (Operand)
                   > Type_Access_Level (Target_Type)
               then
                  --  In an instance, this is a run-time check, but one we
                  --  know will fail, so generate an appropriate warning.
                  --  The raise will be generated by Expand_N_Type_Conversion.

                  if In_Instance_Body then
                     Error_Msg_N
                       ("?cannot convert access discriminant to non-local" &
                        " access type", Operand);
                     Error_Msg_N
                       ("?Program_Error will be raised at run time", Operand);

                  else
                     Error_Msg_N
                       ("cannot convert access discriminant to non-local" &
                        " access type", Operand);
                     return False;
                  end if;
               end if;

               --  The case of a reference to an access discriminant
               --  from within a type declaration (which will appear
               --  as a discriminal) is always illegal because the
               --  level of the discriminant is considered to be
               --  deeper than any (namable) access type.

               if Is_Entity_Name (Operand)
                 and then (Ekind (Entity (Operand)) = E_In_Parameter
                            or else Ekind (Entity (Operand)) = E_Constant)
                 and then Present (Discriminal_Link (Entity (Operand)))
               then
                  Error_Msg_N
                    ("discriminant has deeper accessibility level than target",
                     Operand);
                  return False;
               end if;
            end if;
         end if;

         declare
            Target : constant Entity_Id := Designated_Type (Target_Type);
            Opnd   : constant Entity_Id := Designated_Type (Opnd_Type);

         begin
            if Is_Tagged_Type (Target) then
               return Valid_Tagged_Conversion (Target, Opnd);

            else
               if Base_Type (Target) /= Base_Type (Opnd) then
                  Error_Msg_NE
                    ("target designated type not compatible with }",
                     N, Base_Type (Opnd));
                  return False;

               elsif not Subtypes_Statically_Match (Target, Opnd)
                  and then (not Has_Discriminants (Target)
                             or else Is_Constrained (Target))
               then
                  Error_Msg_NE
                    ("target designated subtype not compatible with }",
                     N, Opnd);
                  return False;

               else
                  return True;
               end if;
            end if;
         end;

      elsif Ekind (Target_Type) = E_Access_Subprogram_Type
        and then Conversion_Check
                   (Ekind (Base_Type (Opnd_Type)) = E_Access_Subprogram_Type,
                    "illegal operand for access subprogram conversion")
      then
         --  Check that the designated types are subtype conformant

         if not Subtype_Conformant (Designated_Type (Opnd_Type),
                                    Designated_Type (Target_Type))
         then
            Error_Msg_N
              ("operand type is not subtype conformant with target type",
               Operand);
         end if;

         --  Check the static accessibility rule of 4.6(20)

         if Type_Access_Level (Opnd_Type) >
            Type_Access_Level (Target_Type)
         then
            Error_Msg_N
              ("operand type has deeper accessibility level than target",
               Operand);

         --  Check that if the operand type is declared in a generic body,
         --  then the target type must be declared within that same body
         --  (enforces last sentence of 4.6(20)).

         elsif Present (Enclosing_Generic_Body (Opnd_Type)) then
            declare
               O_Gen : constant Node_Id :=
                         Enclosing_Generic_Body (Opnd_Type);

               T_Gen : Node_Id :=
                         Enclosing_Generic_Body (Target_Type);

            begin
               while Present (T_Gen) and then T_Gen /= O_Gen loop
                  T_Gen := Enclosing_Generic_Body (T_Gen);
               end loop;

               if T_Gen /= O_Gen then
                  Error_Msg_N
                    ("target type must be declared in same generic body"
                     & " as operand type", N);
               end if;
            end;
         end if;

         return True;

      elsif Is_Remote_Access_To_Subprogram_Type (Target_Type)
        and then Is_Remote_Access_To_Subprogram_Type (Opnd_Type)
      then
         --  It is valid to convert from one RAS type to another provided
         --  that their specification statically match.

         Check_Subtype_Conformant
           (New_Id  =>
              Designated_Type (Corresponding_Remote_Type (Target_Type)),
            Old_Id  =>
              Designated_Type (Corresponding_Remote_Type (Opnd_Type)),
            Err_Loc =>
              N);
         return True;

      elsif Is_Tagged_Type (Target_Type) then
         return Valid_Tagged_Conversion (Target_Type, Opnd_Type);

      --  Types derived from the same root type are convertible

      elsif Root_Type (Target_Type) = Root_Type (Opnd_Type) then
         return True;

      --  In an instance, there may be inconsistent views of the same
      --  type, or types derived from the same type.

      elsif In_Instance
        and then Underlying_Type (Target_Type) = Underlying_Type (Opnd_Type)
      then
         return True;

      --  Special check for common access type error case

      elsif Ekind (Target_Type) = E_Access_Type
         and then Is_Access_Type (Opnd_Type)
      then
         Error_Msg_N ("target type must be general access type!", N);
         Error_Msg_NE ("add ALL to }!", N, Target_Type);

         return False;

      else
         Error_Msg_NE ("invalid conversion, not compatible with }",
           N, Opnd_Type);

         return False;
      end if;
   end Valid_Conversion;

end Sem_Res;
