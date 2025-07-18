------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.301 $
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
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch11; use Exp_Ch11;
with Hostparm; use Hostparm;
with Inline;   use Inline;
with Itypes;   use Itypes;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;

package body Exp_Util is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Make_CW_Equivalent_Type
     (T    : Entity_Id;
      E    : Node_Id)
      return Entity_Id;
   --  T is a class-wide type entity, E is the initial expression node that
   --  constrains T in case such as: " X: T := E" or "new T'(E)"
   --  This function returns the entity of the Equivalent type and inserts
   --  on the fly the necessary declaration such as:
   --    type anon is record
   --       _parent : Root_Type (T); constrained with E discriminants (if any)
   --       Extension : String (1 .. expr to match size of E);
   --    end record;
   --
   --  This record is compatible with any object of the class of T thanks
   --  to the first field and has the same size as E thanks to the second.

   function Make_Literal_Range
     (Loc         : Source_Ptr;
      Literal_Typ : Entity_Id;
      Index_Typ   : Entity_Id)
      return        Node_Id;
   --  Produce a Range node whose bounds are:
   --    Index_Typ'first .. Index_Typ'First + Length (Literal_Typ)
   --  this is used for expanding declarations like X : String := "sdfgdfg";

   function New_Class_Wide_Subtype
     (CW_Typ : Entity_Id;
      N      : Node_Id)
      return   Entity_Id;
   --  Create an implicit subtype of CW_Typ attached to node N.

   ----------------------
   -- Adjust_Condition --
   ----------------------

   procedure Adjust_Condition (N : Node_Id) is
   begin
      if No (N) then
         return;
      end if;

      declare
         Loc : constant Source_Ptr := Sloc (N);
         T   : constant Entity_Id  := Etype (N);
         Ti  : Entity_Id;

      begin
         --  For now, we simply ignore a call where the argument has no
         --  type (probably case of unanalyzed condition), or has a type
         --  that is not Boolean. This is because this is a pretty marginal
         --  piece of functionality, and violations of these rules are
         --  likely to be truly marginal (how much code uses Fortran Logical
         --  as the barrier to a protected entry?) and we do not want to
         --  blow up existing programs. We can change this to an assertion
         --  after 3.12a is released ???

         if No (T) or else not Is_Boolean_Type (T) then
            return;
         end if;

         --  Immediate return if standard boolean, the most common case,
         --  where nothing needs to be done.

         if Base_Type (T) = Standard_Boolean then
            return;
         end if;

         --  Case of zero/non-zero semantics or non-standard enumeration
         --  representation. In each case, we rewrite the node as:

         --      ityp!(N) /= False'Enum_Rep

         --  where ityp is an integer type with large enough size to hold
         --  any value of type T.

         if Nonzero_Is_True (T) or else Has_Non_Standard_Rep (T) then
            if Esize (T) <= Esize (Standard_Integer) then
               Ti := Standard_Integer;
            else
               Ti := Standard_Long_Long_Integer;
            end if;

            Rewrite (N,
              Make_Op_Ne (Loc,
                Left_Opnd  => Unchecked_Convert_To (Ti, N),
                Right_Opnd =>
                  Make_Attribute_Reference (Loc,
                    Attribute_Name => Name_Enum_Rep,
                    Prefix         =>
                      New_Occurrence_Of (First_Literal (T), Loc))));
            Analyze_And_Resolve (N, Standard_Boolean);

         else
            Rewrite (N, Convert_To (Standard_Boolean, N));
            Analyze_And_Resolve (N, Standard_Boolean);
         end if;
      end;
   end Adjust_Condition;

   ------------------------
   -- Adjust_Result_Type --
   ------------------------

   procedure Adjust_Result_Type (N : Node_Id; T : Entity_Id) is
   begin
      --  Ignore call if current type is not Standard.Boolean

      if Etype (N) /= Standard_Boolean then
         return;
      end if;

      --  If result is already of correct type, nothing to do. Note that
      --  this will get the most common case where everything has a type
      --  of Standard.Boolean.

      if Base_Type (T) = Standard_Boolean then
         return;

      else
         declare
            KP : constant Node_Kind := Nkind (Parent (N));

         begin
            --  If result is to be used as a Condition in the syntax, no need
            --  to convert it back, since if it was changed to Standard.Boolean
            --  using Adjust_Condition, that is just fine for this usage.

            if KP in N_Raise_xxx_Error or else KP in N_Has_Condition then
               return;

            --  If result is an operand of another logical operation, no need
            --  to reset its type, since Standard.Boolean is just fine, and
            --  such operations always do Adjust_Condition on their operands.

            elsif KP in N_Op_Boolean
              or else KP = N_And_Then
              or else KP = N_Or_Else
              or else KP = N_Op_Not
            then
               return;

            --  Otherwise we perform a conversion from the current type,
            --  which must be Standard.Boolean, to the desired type.

            else
               Set_Analyzed (N);
               Rewrite (N, Convert_To (T, N));
               Analyze_And_Resolve (N, T);
            end if;
         end;
      end if;
   end Adjust_Result_Type;

   --------------------------
   -- Append_Freeze_Action --
   --------------------------

   procedure Append_Freeze_Action (T : Entity_Id; N : Node_Id) is
      Fnode : Node_Id := Freeze_Node (T);

   begin
      Ensure_Freeze_Node (T);
      Fnode := Freeze_Node (T);

      if not Present (Actions (Fnode)) then
         Set_Actions (Fnode, New_List);
      end if;

      Append (N, Actions (Fnode));
   end Append_Freeze_Action;

   ---------------------------
   -- Append_Freeze_Actions --
   ---------------------------

   procedure Append_Freeze_Actions (T : Entity_Id; L : List_Id) is
      Fnode : constant Node_Id := Freeze_Node (T);

   begin
      if No (L) then
         return;

      else
         if No (Actions (Fnode)) then
            Set_Actions (Fnode, L);

         else
            Append_List (L, Actions (Fnode));
         end if;

      end if;
   end Append_Freeze_Actions;

   ------------------------
   -- Build_Runtime_Call --
   ------------------------

   function Build_Runtime_Call (Loc : Source_Ptr; RE : RE_Id) return Node_Id is
   begin
      return
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (RTE (RE), Loc));
   end Build_Runtime_Call;

   --------------------------
   -- Build_Task_Image_Decl--
   --------------------------

   function Build_Task_Image_Decl
     (Loc    : Source_Ptr;
      Id_Ref : Node_Id;
      A_Type : Entity_Id)
      return Node_Id
   is
      T_Id : Entity_Id;
      Decl : Node_Id;
      Expr : Node_Id;
      Indx : Node_Id;
      Val  : Node_Id;

   begin
      --  If Discard_Names is in effect, generate a dummy declaration only.

      if Global_Discard_Names then
         T_Id :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('I'));

         return
           Make_Object_Declaration (Loc,
             Defining_Identifier => T_Id,
             Object_Definition =>
               New_Occurrence_Of (RTE (RE_Task_Image_Type), Loc));

      --  For a simple variable, the image of the task is the name of
      --  the variable.

      elsif Nkind (Id_Ref) = N_Identifier
        or else Nkind (Id_Ref) = N_Defining_Identifier
      then
         T_Id :=
           Make_Defining_Identifier (Loc,
             New_External_Name (Chars (Id_Ref), 'I'));

         Get_Name_String (Chars (Id_Ref));

         Expr :=
           Make_String_Literal (Loc, Strval => String_From_Name_Buffer);

      --  For a selected component, the image of the task is the corresponding
      --  selected component of the enclosing variable, obtained by concate-
      --  nating the variable and selector names.

      elsif Nkind (Id_Ref) = N_Selected_Component then
         T_Id :=
           Make_Defining_Identifier (Loc,
             New_External_Name (Chars (Selector_Name (Id_Ref)), 'I'));

         Get_Name_String (Chars (Selector_Name (Id_Ref)));
         Name_Buffer (2 .. Name_Len + 1) := Name_Buffer (1 .. Name_Len);
         Name_Buffer (1) := '.';
         Name_Len := Name_Len + 1;

         Expr :=
           Make_Op_Concat (Loc,
             Left_Opnd =>
               Make_Explicit_Dereference (Loc,
                 Prefix =>
                   Make_Identifier (Loc, Name_uTask_Id)),
             Right_Opnd =>
               Make_String_Literal (Loc, Strval => String_From_Name_Buffer));

      --  For an indexed component, the image of the task is constructed by
      --  concatenating the images of the index values of the component.

      elsif Nkind (Id_Ref) = N_Indexed_Component then

         T_Id :=
           Make_Defining_Identifier (Loc,
             New_External_Name (Chars (A_Type), 'I'));

         Set_Character_Literal_Name (Char_Code (Character'Pos ('(')));

         Expr :=
           Make_Op_Concat (Loc,
             Left_Opnd =>
               Make_Explicit_Dereference (Loc,
                  Prefix =>
                    Make_Identifier (Loc, Name_uTask_Id)),
               Right_Opnd =>
                 Make_Character_Literal (Loc,
                 Chars => Name_Find,
                 Char_Literal_Value => Char_Code (Character'Pos ('('))));

         Set_Is_Component_Right_Opnd (Expr);
         Indx := First_Index (A_Type);
         Val  := First (Expressions (Id_Ref));

         while Present (Indx) loop
            Expr :=
              Make_Op_Concat (Loc,
                Left_Opnd => Expr,
                Right_Opnd =>
                  Make_Attribute_Reference (Loc,
                    Attribute_Name => Name_Image,
                    Prefix =>
                      New_Occurrence_Of (Etype (Indx), Loc),
                    Expressions => New_List (
                      New_Copy_Tree (Val))));

            Next_Index (Indx);
            Next (Val);

            if Present (Indx) then
               Set_Character_Literal_Name (Char_Code (Character'Pos (',')));

               Expr :=
                 Make_Op_Concat (Loc,
                   Left_Opnd => Expr,
                   Right_Opnd =>
                     Make_Character_Literal (Loc,
                     Chars => Name_Find,
                     Char_Literal_Value => Char_Code (Character'Pos (','))));
            end if;

         end loop;

         Set_Character_Literal_Name (Char_Code (Character'Pos (')')));

         Expr :=
            Make_Op_Concat (Loc,
              Left_Opnd => Expr,
              Right_Opnd =>
                Make_Character_Literal (Loc,
                Chars => Name_Find,
                Char_Literal_Value => Char_Code (Character'Pos (')'))));
         Set_Is_Component_Right_Opnd (Expr);
      end if;

      Decl := Make_Object_Declaration (Loc,
        Defining_Identifier => T_Id,
        Object_Definition =>
          New_Occurrence_Of (RTE (RE_Task_Image_Type), Loc),
        Expression =>
          Make_Allocator (Loc,
            Expression =>
              Make_Qualified_Expression (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of (Standard_String, Loc),
                Expression => Expr)));

      return Decl;
   end Build_Task_Image_Decl;

   -------------------------------
   -- Convert_To_Actual_Subtype --
   -------------------------------

   procedure Convert_To_Actual_Subtype (Exp : Entity_Id) is
      Act_ST : Entity_Id;

   begin
      Act_ST := Get_Actual_Subtype (Exp);

      if Act_ST = Etype (Exp) then
         return;

      else
         Rewrite (Exp,
           Convert_To (Act_ST, Relocate_Node (Exp)));
         Analyze_And_Resolve (Exp, Act_ST);
      end if;
   end Convert_To_Actual_Subtype;

   -----------------------------------
   -- Current_Sem_Unit_Declarations --
   -----------------------------------

   function Current_Sem_Unit_Declarations return List_Id is
      U     : Node_Id := Unit (Cunit (Current_Sem_Unit));
      Decls : List_Id;

   begin
      if Nkind (U) = N_Package_Declaration then
         U := Specification (U);
         Decls := Visible_Declarations (U);

         if No (Decls) then
            Decls := New_List;
            Set_Visible_Declarations (U, Decls);
         end if;
      else
         Decls := Declarations (U);

         if No (Decls) then
            Decls := New_List;
            Set_Declarations (U, Decls);
         end if;
      end if;

      return Decls;
   end Current_Sem_Unit_Declarations;

   -----------------------
   -- Duplicate_Subexpr --
   -----------------------

   function Duplicate_Subexpr
     (Exp      : Node_Id;
      Name_Req : Boolean := False)
      return     Node_Id
   is
   begin
      Remove_Side_Effects (Exp, Name_Req);
      return New_Copy_Tree (Exp);
   end Duplicate_Subexpr;

   --------------------
   -- Ensure_Defined --
   --------------------

   procedure Ensure_Defined (Typ : Entity_Id; N : Node_Id) is
      IR : Node_Id;
      P  : Node_Id;

   begin
      if Is_Itype (Typ) then
         IR := Make_Itype_Reference (Sloc (N));
         Set_Itype (IR, Typ);

         if not In_Open_Scopes (Scope (Typ))
           and then Is_Subprogram (Current_Scope)
           and then Scope (Current_Scope) /= Standard_Standard
         then
            --  Insert node in front of subprogram, to avoid scope anomalies
            --  in gigi.

            P := Parent (N);

            while Present (P)
              and then Nkind (P) /= N_Subprogram_Body
            loop
               P := Parent (P);
            end loop;

            if Present (P) then
               Insert_Action (P, IR);
            else
               Insert_Action (N, IR);
            end if;

         else
            Insert_Action (N, IR);
         end if;
      end if;
   end Ensure_Defined;

   ---------------------
   -- Evolve_And_Then --
   ---------------------

   procedure Evolve_And_Then (Cond : in out Node_Id; Cond1 : Node_Id) is
   begin
      if No (Cond) then
         Cond := Cond1;
      else
         Cond :=
           Make_And_Then (Sloc (Cond1),
             Left_Opnd  => Cond,
             Right_Opnd => Cond1);
      end if;
   end Evolve_And_Then;

   --------------------
   -- Evolve_Or_Else --
   --------------------

   procedure Evolve_Or_Else (Cond : in out Node_Id; Cond1 : Node_Id) is
   begin
      if No (Cond) then
         Cond := Cond1;
      else
         Cond :=
           Make_Or_Else (Sloc (Cond1),
             Left_Opnd  => Cond,
             Right_Opnd => Cond1);
      end if;
   end Evolve_Or_Else;

   ------------------------------
   -- Expand_Subtype_From_Expr --
   ------------------------------

   --  This function is applicable for both static and dynamic allocation of
   --  objects which are constrained by an initial expression. Basically it
   --  transforms an unconstrained subtype indication into a constrained one.
   --  The expression may also be transformed in certain cases in order to
   --  avoid multiple evaulation. In the static allocation case, the general
   --  scheme is :
   --     Val : T := Expr;
   --        is transformed into
   --     Val : Constrained_Subtype_of_T := Maybe_Modified_Expr;
   --
   --  Here are the main cases :
   --
   --  <if Expr is a Slice>
   --    Val : T ([Index_Subtype (Expr)]) := Expr;
   --
   --  <elsif Expr is a String Literal>
   --    Val : T (T'First .. T'First + Length (string literal) - 1) := Expr;
   --
   --  <elsif Expr is Constrained>
   --    subtype T is Type_Of_Expr
   --    Val : T := Expr;
   --
   --  <elsif Expr is an entity_name>
   --    Val : T (contraints taken from Expr) := Expr;
   --
   --  <else>
   --    type Axxx is access all T;
   --    Rval : Axxx := Expr'ref;
   --    Val  : T (contraints taken from Rval) := Rval.all;
   --    ??? note: when the Expression is allocated in the secondary stack
   --              we could use it directly instead of copying it by declaring
   --              Val : T (...) renames Rval.all

   procedure Expand_Subtype_From_Expr
     (N             : Node_Id;
      Unc_Type      : Entity_Id;
      Subtype_Indic : Node_Id;
      Exp           : Node_Id)
   is
      Loc     : constant Source_Ptr := Sloc (N);
      Exp_Typ : constant Entity_Id  := Etype (Exp);
      T       : Entity_Id;

   begin
      --  In general we cannot build the subtype if expansion is disabled,
      --  because internal entities may not have been defined. However, to
      --  avoid some cascaded errors, we try to continue when the expression
      --  is an array (or string), because it is safe to compute the bounds.

      if not Expander_Active
        and then Errors_Detected = 0
      then
         return;

      elsif Errors_Detected > 0
        and then (No (Etype (Exp))
                   or else not Is_Array_Type (Etype (Exp)))
      then
         return;
      end if;

      if Nkind (Exp) = N_Slice then
         declare
            Slice_Type : constant Entity_Id := Etype (First_Index (Exp_Typ));

         begin
            Rewrite (Subtype_Indic,
              Make_Subtype_Indication (Loc,
                Subtype_Mark => New_Reference_To (Unc_Type, Loc),
                Constraint =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List
                      (New_Reference_To (Slice_Type, Loc)))));

            --  This subtype indication may be used later for contraint checks
            --  we better make sure that if a variable was used as a bound of
            --  of the original slice, its value is frozen.

            Force_Evaluation (Low_Bound (Scalar_Range (Slice_Type)));
            Force_Evaluation (High_Bound (Scalar_Range (Slice_Type)));
         end;


      elsif Ekind (Exp_Typ) = E_String_Literal_Subtype then
         Rewrite (Subtype_Indic,
           Make_Subtype_Indication (Loc,
             Subtype_Mark => New_Reference_To (Unc_Type, Loc),
             Constraint =>
               Make_Index_Or_Discriminant_Constraint (Loc,
                 Constraints => New_List (
                   Make_Literal_Range (Loc,
                     Literal_Typ => Exp_Typ,
                     Index_Typ   => Etype (First_Index (Unc_Type)))))));


      elsif Is_Constrained (Exp_Typ)
        and then not Is_Class_Wide_Type (Unc_Type)
      then
         if Is_Itype (Exp_Typ) then

            --  No need to generate a new one.

            T := Exp_Typ;

         else
            T :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('T'));

            Insert_Action (N,
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => T,
                Subtype_Indication  => New_Reference_To (Exp_Typ, Loc)));

            --  This type is marked as an itype even though it has an
            --  explicit declaration because otherwise it can be marked
            --  with Is_Generic_Actual_Type and generate spurious errors.
            --  (see sem_ch8.Analyze_Package_Renaming and sem_type.covers)

            Set_Is_Itype (T);
            Set_Associated_Node_For_Itype (T, Exp);
         end if;

         Rewrite (Subtype_Indic, New_Reference_To (T, Loc));

      --  nothing needs to be done for private types with unknown discriminants
      --  if the underlying type is not an unconstrained composite type.

      elsif Is_Private_Type (Unc_Type)
        and then Has_Unknown_Discriminants (Unc_Type)
        and then (not Is_Composite_Type (Underlying_Type (Unc_Type))
                    or else Is_Constrained (Underlying_Type (Unc_Type)))
      then
         null;

      else
         Remove_Side_Effects (Exp);
         Rewrite (Subtype_Indic,
           Make_Subtype_From_Expr (Exp, Unc_Type));
      end if;
   end Expand_Subtype_From_Expr;

   ------------------
   -- Find_Prim_Op --
   ------------------

   function Find_Prim_Op (T : Entity_Id; Name : Name_Id) return Entity_Id is
      Prim : Elmt_Id;
      Typ  : Entity_Id := T;

   begin
      if Is_Class_Wide_Type (Typ) then
         Typ := Root_Type (Typ);
      end if;

      Typ := Underlying_Type (Typ);

      Prim := First_Elmt (Primitive_Operations (Typ));
      while Chars (Node (Prim)) /= Name loop
         Next_Elmt (Prim);
         pragma Assert (Present (Prim));
      end loop;

      return Node (Prim);
   end Find_Prim_Op;

   ----------------------
   -- Force_Evaluation --
   ----------------------

   procedure Force_Evaluation (Exp : Node_Id; Name_Req : Boolean := False) is
   begin
      Remove_Side_Effects (Exp, Name_Req, Variable_Ref => True);
   end Force_Evaluation;

   ------------------------
   -- Generate_Poll_Call --
   ------------------------

   procedure Generate_Poll_Call (N : Node_Id) is
   begin
      --  No poll call if polling not active

      if not Polling_Required then
         return;

      --  Otherwise generate require poll call

      else
         Insert_Before_And_Analyze (N,
           Make_Procedure_Call_Statement (Sloc (N),
             Name => New_Occurrence_Of (RTE (RE_Poll), Sloc (N))));
      end if;
   end Generate_Poll_Call;

   --------------------
   -- Homonym_Number --
   --------------------

   function Homonym_Number (Subp : Entity_Id) return Nat is
      Count : Nat;
      Hom   : Entity_Id;

   begin
      Count := 1;
      Hom := Homonym (Subp);
      while Present (Hom) loop
         if Scope (Hom) = Scope (Subp) then
            Count := Count + 1;
         end if;

         Hom := Homonym (Hom);
      end loop;

      return Count;
   end Homonym_Number;

   ------------------------------
   -- In_Unconditional_Context --
   ------------------------------

   function In_Unconditional_Context (Node : Node_Id) return Boolean is
      P : Node_Id;

   begin
      P := Node;
      while Present (P) loop
         case Nkind (P) is
            when N_Subprogram_Body =>
               return True;

            when N_If_Statement =>
               return False;

            when N_Loop_Statement =>
               return False;

            when N_Case_Statement =>
               return False;

            when others =>
               P := Parent (P);
         end case;
      end loop;

      return False;
   end In_Unconditional_Context;

   -------------------
   -- Insert_Action --
   -------------------

   procedure Insert_Action (Assoc_Node : Node_Id; Ins_Action : Node_Id) is
   begin
      if Present (Ins_Action) then
         Insert_Actions (Assoc_Node, New_List (Ins_Action));
      end if;
   end Insert_Action;

   --  Version with check(s) suppressed

   procedure Insert_Action
     (Assoc_Node : Node_Id; Ins_Action : Node_Id; Suppress : Check_Id)
   is
   begin
      Insert_Actions (Assoc_Node, New_List (Ins_Action), Suppress);
   end Insert_Action;

   --------------------
   -- Insert_Actions --
   --------------------

   procedure Insert_Actions (Assoc_Node : Node_Id; Ins_Actions : List_Id) is
      N : Node_Id;
      P : Node_Id;

      Wrapped_Node : Node_Id := Empty;

   begin
      if No (Ins_Actions) or else Is_Empty_List (Ins_Actions) then
         return;
      end if;

      --  Ignore insert of actions from inside default expression in the
      --  special preliminary analyze mode. Any insertions at this point
      --  have no relevance, since we are only doing the analyze to freeze
      --  the types of any static expressions. See section "Handling of
      --  Default Expressions" in the spec of package Sem for further details.

      if In_Default_Expression then
         return;
      end if;

      --  If the action derives from stuff inside a record, then the actions
      --  are attached to the current scope, to be inserted and analyzed on
      --  exit from the scope. The reason for this is that we may also
      --  be generating freeze actions at the same time, and they must
      --  eventually be elaborated in the correct order.

      if Is_Record_Type (Current_Scope)
        and then not Is_Frozen (Current_Scope)
      then
         if No (Scope_Stack.Table
           (Scope_Stack.Last).Pending_Freeze_Actions)
         then
            Scope_Stack.Table (Scope_Stack.Last).Pending_Freeze_Actions :=
              Ins_Actions;
         else
            Append_List
              (Ins_Actions,
               Scope_Stack.Table (Scope_Stack.Last).Pending_Freeze_Actions);
         end if;

         return;
      end if;

      --  We now intend to climb up the tree to find the right point to
      --  insert the actions. We start at Assoc_Node, unless this node is
      --  a subexpression in which case we start with its parent. We do this
      --  for two reasons. First it speeds things up. Second, if Assoc_Node
      --  is itself one of the special nodes like N_And_Then, then we assume
      --  that an initial request to insert actions for such a node does not
      --  expect the actions to get deposited in the node for later handling
      --  when the node is expanded, since clearly the node is being dealt
      --  with by the caller. Note that in the subexpression case, N is
      --  always the child we came from.

      --  N_Raise_xxx_Error is an annoying special case, it is a statement
      --  if it has type Standard_Void_Type, and a subexpression otherwise.
      --  otherwise. Procedure attribute references are also statements.

      if Nkind (Assoc_Node) in N_Subexpr
        and then (Nkind (Assoc_Node) in N_Raise_xxx_Error
                   or else Etype (Assoc_Node) /= Standard_Void_Type)
        and then (Nkind (Assoc_Node) /= N_Attribute_Reference
                   or else
                     not Is_Procedure_Attribute_Name
                           (Attribute_Name (Assoc_Node)))
      then
         P := Assoc_Node;             -- ????? does not agree with above!
         N := Parent (Assoc_Node);

      --  Non-subexpression case. Note that N is initially undefined in
      --  this case (N is only guaranteed defined in the subexpr case).

      else
         P := Assoc_Node;
      end if;

      --  Capture root of the transient scope

      if Scope_Is_Transient then
         Wrapped_Node  := Node_To_Be_Wrapped;
      end if;

      loop
         pragma Assert (Present (P));

         case Nkind (P) is

            --  Case of right operand of AND THEN or OR ELSE. Put the actions
            --  in the Actions field of the right operand. They will be moved
            --  out further when the AND THEN or OR ELSE operator is expanded.
            --  Nothing special needs to be done for the left operand since
            --  in that case the actions are executed unconditionally.

            when N_And_Then | N_Or_Else =>
               if N = Right_Opnd (P) then
                  if Present (Actions (P)) then
                     Insert_List_After_And_Analyze
                      (Last (Actions (P)), Ins_Actions);
                  else
                     Set_Actions (P, Ins_Actions);
                     Analyze_List (Actions (P));
                  end if;

                  return;
               end if;

            --  Then or Else operand of conditional expression. Add actions to
            --  Then_Actions or Else_Actions field as appropriate. The actions
            --  will be moved further out when the conditional is expanded.

            when N_Conditional_Expression =>
               declare
                  ThenX : constant Node_Id := Next (First (Expressions (P)));
                  ElseX : constant Node_Id := Next (ThenX);

               begin
                  --  Actions belong to the then expression, temporarily
                  --  place them as Then_Actions of the conditional expr.
                  --  They will be moved to the proper place later when
                  --  the conditional expression is expanded.

                  if N = ThenX then
                     if Present (Then_Actions (P)) then
                        Insert_List_After_And_Analyze
                          (Last (Then_Actions (P)), Ins_Actions);
                     else
                        Set_Then_Actions (P, Ins_Actions);
                        Analyze_List (Then_Actions (P));
                     end if;

                     return;

                  --  Actions belong to the else expression, temporarily
                  --  place them as Else_Actions of the conditional expr.
                  --  They will be moved to the proper place later when
                  --  the conditional expression is expanded.

                  elsif N = ElseX then
                     if Present (Else_Actions (P)) then
                        Insert_List_After_And_Analyze
                          (Last (Else_Actions (P)), Ins_Actions);
                     else
                        Set_Else_Actions (P, Ins_Actions);
                        Analyze_List (Else_Actions (P));
                     end if;

                     return;

                  --  Actions belong to the condition. In this case they are
                  --  unconditionally executed, and so we can continue the
                  --  search for the proper insert point.

                  else
                     null;
                  end if;
               end;

            --  Case of appearing in the condition of a while expression or
            --  elsif. We insert the actions into the Condition_Actions field.
            --  They will be moved further out when the while loop or elsif
            --  is analyzed.

            when N_Iteration_Scheme |
                 N_Elsif_Part
            =>
               if N = Condition (P) then
                  if Present (Condition_Actions (P)) then
                     Insert_List_After_And_Analyze
                       (Last (Condition_Actions (P)), Ins_Actions);
                  else
                     Set_Condition_Actions (P, Ins_Actions);

                     --  Set the parent of the insert actions explicitly.
                     --  This is not a syntactic field, but we need the
                     --  parent field set, in particular so that freeze
                     --  can understand that it is dealing with condition
                     --  actions, and properly insert the freezing actions.

                     Set_Parent (Ins_Actions, P);
                     Analyze_List (Condition_Actions (P));
                  end if;

                  return;
               end if;

            --  Statements, declarations, pragmas, representation clauses.

            when
               --  Statements

               N_Procedure_Call_Statement               |
               N_Statement_Other_Than_Procedure_Call    |

               --  Pragmas

               N_Pragma                                 |

               --  Representation_Clause

               N_At_Clause                              |
               N_Attribute_Definition_Clause            |
               N_Enumeration_Representation_Clause      |
               N_Record_Representation_Clause           |

               --  Declarations

               N_Abstract_Subprogram_Declaration        |
               N_Entry_Body                             |
               N_Exception_Declaration                  |
               N_Exception_Renaming_Declaration         |
               N_Formal_Object_Declaration              |
               N_Formal_Subprogram_Declaration          |
               N_Formal_Type_Declaration                |
               N_Full_Type_Declaration                  |
               N_Function_Instantiation                 |
               N_Generic_Function_Renaming_Declaration  |
               N_Generic_Package_Declaration            |
               N_Generic_Package_Renaming_Declaration   |
               N_Generic_Procedure_Renaming_Declaration |
               N_Generic_Subprogram_Declaration         |
               N_Implicit_Label_Declaration             |
               N_Incomplete_Type_Declaration            |
               N_Number_Declaration                     |
               N_Object_Declaration                     |
               N_Object_Renaming_Declaration            |
               N_Package_Body                           |
               N_Package_Body_Stub                      |
               N_Package_Declaration                    |
               N_Package_Instantiation                  |
               N_Package_Renaming_Declaration           |
               N_Private_Extension_Declaration          |
               N_Private_Type_Declaration               |
               N_Procedure_Instantiation                |
               N_Protected_Body_Stub                    |
               N_Protected_Type_Declaration             |
               N_Single_Task_Declaration                |
               N_Subprogram_Body                        |
               N_Subprogram_Body_Stub                   |
               N_Subprogram_Declaration                 |
               N_Subprogram_Renaming_Declaration        |
               N_Subtype_Declaration                    |
               N_Task_Body                              |
               N_Task_Body_Stub                         |
               N_Task_Type_Declaration                  |

               --  Freeze entity behaves like a declaration or statement

               N_Freeze_Entity
            =>
               --  Do not insert here if the item is not a list member (this
               --  happens for example with a triggering statement, and the
               --  proper approach is to insert before the entire select).

               if not Is_List_Member (P) then
                  null;

               --  Do not insert if parent of P is an N_Component_Association
               --  node (i.e. we are in the context of an N_Aggregate node.
               --  In this case we want to insert before the entire aggregate.

               elsif Nkind (Parent (P)) = N_Component_Association then
                  null;

               --  Do not insert if the parent of P is either an N_Variant
               --  node or an N_Record_Definition node, meaning in either
               --  case that P is a member of a component list, and that
               --  therefore the actions should be inserted outside the
               --  complete record declaration.

               elsif Nkind (Parent (P)) = N_Variant
                 or else Nkind (Parent (P)) = N_Record_Definition
               then
                  null;

               --  Do not insert freeze nodes within the loop generated for
               --  an aggregate, because they may be elaborated too late for
               --  subsequent use in the back end: within a package spec the
               --  loop is part of the elaboration procedure and is only
               --  elaborated during the second pass.
               --  If the loop comes from source, or the entity is local to
               --  the loop itself it must remain within.

               elsif Nkind (Parent (P)) = N_Loop_Statement
                 and then not Comes_From_Source (Parent (P))
                 and then Nkind (First (Ins_Actions)) = N_Freeze_Entity
                 and then
                   Scope (Entity (First (Ins_Actions))) /= Current_Scope
               then
                  null;

               --  Otherwise we can go ahead and do the insertion

               elsif  P = Wrapped_Node then
                  Store_Before_Actions_In_Scope (Ins_Actions);
                  return;

               else
                  Insert_List_Before_And_Analyze (P, Ins_Actions);
                  return;
               end if;

            --  A special case, N_Raise_xxx_Error can act either as a
            --  statement or a subexpression. We tell the difference
            --  by looking at the Etype. It is set to Standard_Void_Type
            --  in the statement case.

            when
               N_Raise_xxx_Error =>
                  if Etype (P) = Standard_Void_Type then
                     if  P = Wrapped_Node then
                        Store_Before_Actions_In_Scope (Ins_Actions);
                     else
                        Insert_List_Before_And_Analyze (P, Ins_Actions);
                     end if;

                     return;

                  --  In the subexpression case, keep climbing

                  else
                     null;
                  end if;

            --  Another special case, an attribute denoting a procedure call

            when
               N_Attribute_Reference =>
                  if Is_Procedure_Attribute_Name (Attribute_Name (P)) then
                     if P = Wrapped_Node then
                        Store_Before_Actions_In_Scope (Ins_Actions);
                     else
                        Insert_List_Before_And_Analyze (P, Ins_Actions);
                     end if;

                     return;

                  --  In the subexpression case, keep climbing

                  else
                     null;
                  end if;

            --  For all other node types, keep climbing tree

            when
               N_Abortable_Part                         |
               N_Accept_Alternative                     |
               N_Access_Definition                      |
               N_Access_Function_Definition             |
               N_Access_Procedure_Definition            |
               N_Access_To_Object_Definition            |
               N_Aggregate                              |
               N_Allocator                              |
               N_Case_Statement_Alternative             |
               N_Character_Literal                      |
               N_Compilation_Unit                       |
               N_Compilation_Unit_Aux                   |
               N_Component_Association                  |
               N_Component_Clause                       |
               N_Component_Declaration                  |
               N_Component_List                         |
               N_Constrained_Array_Definition           |
               N_Decimal_Fixed_Point_Definition         |
               N_Defining_Character_Literal             |
               N_Defining_Identifier                    |
               N_Defining_Operator_Symbol               |
               N_Defining_Program_Unit_Name             |
               N_Delay_Alternative                      |
               N_Delta_Constraint                       |
               N_Derived_Type_Definition                |
               N_Designator                             |
               N_Digits_Constraint                      |
               N_Discriminant_Association               |
               N_Discriminant_Specification             |
               N_Empty                                  |
               N_Entry_Body_Formal_Part                 |
               N_Entry_Call_Alternative                 |
               N_Entry_Declaration                      |
               N_Entry_Index_Specification              |
               N_Enumeration_Type_Definition            |
               N_Error                                  |
               N_Exception_Handler                      |
               N_Expanded_Name                          |
               N_Explicit_Dereference                   |
               N_Extension_Aggregate                    |
               N_Floating_Point_Definition              |
               N_Formal_Decimal_Fixed_Point_Definition  |
               N_Formal_Derived_Type_Definition         |
               N_Formal_Discrete_Type_Definition        |
               N_Formal_Floating_Point_Definition       |
               N_Formal_Modular_Type_Definition         |
               N_Formal_Ordinary_Fixed_Point_Definition |
               N_Formal_Package_Declaration             |
               N_Formal_Private_Type_Definition         |
               N_Formal_Signed_Integer_Type_Definition  |
               N_Function_Call                          |
               N_Function_Specification                 |
               N_Generic_Association                    |
               N_Handled_Sequence_Of_Statements         |
               N_Identifier                             |
               N_In                                     |
               N_Index_Or_Discriminant_Constraint       |
               N_Indexed_Component                      |
               N_Integer_Literal                        |
               N_Itype_Reference                        |
               N_Label                                  |
               N_Loop_Parameter_Specification           |
               N_Mod_Clause                             |
               N_Modular_Type_Definition                |
               N_Not_In                                 |
               N_Null                                   |
               N_Op_Abs                                 |
               N_Op_Add                                 |
               N_Op_And                                 |
               N_Op_Concat                              |
               N_Op_Divide                              |
               N_Op_Eq                                  |
               N_Op_Expon                               |
               N_Op_Ge                                  |
               N_Op_Gt                                  |
               N_Op_Le                                  |
               N_Op_Lt                                  |
               N_Op_Minus                               |
               N_Op_Mod                                 |
               N_Op_Multiply                            |
               N_Op_Ne                                  |
               N_Op_Not                                 |
               N_Op_Or                                  |
               N_Op_Plus                                |
               N_Op_Rem                                 |
               N_Op_Rotate_Left                         |
               N_Op_Rotate_Right                        |
               N_Op_Shift_Left                          |
               N_Op_Shift_Right                         |
               N_Op_Shift_Right_Arithmetic              |
               N_Op_Subtract                            |
               N_Op_Xor                                 |
               N_Operator_Symbol                        |
               N_Ordinary_Fixed_Point_Definition        |
               N_Others_Choice                          |
               N_Package_Specification                  |
               N_Parameter_Association                  |
               N_Parameter_Specification                |
               N_Pragma_Argument_Association            |
               N_Procedure_Specification                |
               N_Protected_Body                         |
               N_Protected_Definition                   |
               N_Qualified_Expression                   |
               N_Range                                  |
               N_Range_Constraint                       |
               N_Real_Literal                           |
               N_Real_Range_Specification               |
               N_Record_Definition                      |
               N_Reference                              |
               N_Selected_Component                     |
               N_Signed_Integer_Type_Definition         |
               N_Single_Protected_Declaration           |
               N_Slice                                  |
               N_String_Literal                         |
               N_Subprogram_Info                        |
               N_Subtype_Indication                     |
               N_Subunit                                |
               N_Task_Definition                        |
               N_Terminate_Alternative                  |
               N_Triggering_Alternative                 |
               N_Type_Conversion                        |
               N_Unchecked_Expression                   |
               N_Unchecked_Type_Conversion              |
               N_Unconstrained_Array_Definition         |
               N_Unused_At_End                          |
               N_Unused_At_Start                        |
               N_Use_Package_Clause                     |
               N_Use_Type_Clause                        |
               N_Variant                                |
               N_Variant_Part                           |
               N_Validate_Unchecked_Conversion          |
               N_With_Clause                            |
               N_With_Type_Clause
            =>
               null;

         end case;

         --  Make sure that inserted actions stay in the transient scope

         if P = Wrapped_Node then
            Store_Before_Actions_In_Scope (Ins_Actions);
            return;
         end if;

         --  If we fall through above tests, keep climbing tree

         N := P;

         if Nkind (Parent (N)) = N_Subunit then

            --  This is the proper body corresponding to a stub. Insertion
            --  must be done at the point of the stub, which is in the decla-
            --  tive part of the parent unit.

            P := Corresponding_Stub (Parent (N));

         else
            P := Parent (N);
         end if;
      end loop;

   end Insert_Actions;

   --  Version with check(s) suppressed

   procedure Insert_Actions
     (Assoc_Node : Node_Id; Ins_Actions : List_Id; Suppress : Check_Id)
   is
   begin
      if Suppress = All_Checks then
         declare
            Svg : constant Suppress_Record := Scope_Suppress;

         begin
            Scope_Suppress := (others => True);
            Insert_Actions (Assoc_Node, Ins_Actions);
            Scope_Suppress := Svg;
         end;

      else
         declare
            Svg : constant Boolean := Get_Scope_Suppress (Suppress);

         begin
            Set_Scope_Suppress (Suppress, True);
            Insert_Actions (Assoc_Node, Ins_Actions);
            Set_Scope_Suppress (Suppress, Svg);
         end;
      end if;
   end Insert_Actions;

   --------------------------
   -- Insert_Actions_After --
   --------------------------

   procedure Insert_Actions_After
     (Assoc_Node  : Node_Id;
      Ins_Actions : List_Id)
   is
   begin
      if Scope_Is_Transient
        and then Assoc_Node = Node_To_Be_Wrapped
      then
         Store_After_Actions_In_Scope (Ins_Actions);
      else
         Insert_List_After_And_Analyze (Assoc_Node, Ins_Actions);
      end if;
   end Insert_Actions_After;

   ---------------------------------
   -- Insert_Library_Level_Action --
   ---------------------------------

   procedure Insert_Library_Level_Action (N : Node_Id) is
      Aux : constant Node_Id := Aux_Decls_Node (Cunit (Main_Unit));

   begin
      New_Scope (Cunit_Entity (Main_Unit));

      if No (Actions (Aux)) then
         Set_Actions (Aux, New_List (N));
      else
         Append (N, Actions (Aux));
      end if;

      Analyze (N);
      Pop_Scope;
   end Insert_Library_Level_Action;

   ----------------------
   -- Inside_Init_Proc --
   ----------------------

   function Inside_Init_Proc return Boolean is
      S : Entity_Id;

   begin
      S := Current_Scope;
      while S /= Standard_Standard loop
         if Chars (S) = Name_uInit_Proc then
            return True;
         else
            S := Scope (S);
         end if;
      end loop;

      return False;
   end Inside_Init_Proc;

   --------------------------------
   -- Is_Ref_To_Bit_Packed_Array --
   --------------------------------

   function Is_Ref_To_Bit_Packed_Array (P : Node_Id) return Boolean is
      Result : Boolean;
      Expr   : Node_Id;

   begin
      if Nkind (P) = N_Indexed_Component
           or else
         Nkind (P) = N_Selected_Component
      then
         if Is_Bit_Packed_Array (Etype (Prefix (P))) then
            Result := True;
         else
            Result := Is_Ref_To_Bit_Packed_Array (Prefix (P));
         end if;

         if Result and then Nkind (P) = N_Indexed_Component then
            Expr := First (Expressions (P));

            while Present (Expr) loop
               Force_Evaluation (Expr);
               Next (Expr);
            end loop;
         end if;

         return Result;

      else
         return False;
      end if;
   end Is_Ref_To_Bit_Packed_Array;

   --------------------------------
   -- Is_Ref_To_Bit_Packed_Slce --
   --------------------------------

   function Is_Ref_To_Bit_Packed_Slice (P : Node_Id) return Boolean is
   begin
      if Nkind (P) = N_Slice
        and then Is_Bit_Packed_Array (Etype (Prefix (P)))
      then
         return True;

      elsif Nkind (P) = N_Indexed_Component
           or else
         Nkind (P) = N_Selected_Component
      then
         return Is_Ref_To_Bit_Packed_Slice (Prefix (P));

      else
         return False;
      end if;
   end Is_Ref_To_Bit_Packed_Slice;

   -----------------------
   -- Is_Renamed_Object --
   -----------------------

   function Is_Renamed_Object (N : Node_Id) return Boolean is
      Pnod : constant Node_Id   := Parent (N);
      Kind : constant Node_Kind := Nkind (Pnod);

   begin
      if Kind = N_Object_Renaming_Declaration then
         return True;

      elsif Kind = N_Indexed_Component
        or else Kind = N_Selected_Component
      then
         return Is_Renamed_Object (Pnod);

      else
         return False;
      end if;
   end Is_Renamed_Object;

   --------------------
   -- Kill_Dead_Code --
   --------------------

   procedure Kill_Dead_Code (N : Node_Id) is
   begin
      if Present (N) then
         Remove_Handler_Entries (N);
         Remove_Warning_Messages (N);

         --  Recurse into block statements to process declarations/statements

         if Nkind (N) = N_Block_Statement then
            Kill_Dead_Code (Declarations (N));
            Kill_Dead_Code (Statements (Handled_Statement_Sequence (N)));

         --  Deal with dead instances caused by deleting instantiations

         elsif Nkind (N) in N_Generic_Instantiation then
            Remove_Dead_Instance (N);
         end if;

         Delete_Tree (N);
      end if;
   end Kill_Dead_Code;

   procedure Kill_Dead_Code (L : List_Id) is
      N : Node_Id;

   begin
      if Is_Non_Empty_List (L) then
         loop
            N := Remove_Head (L);
            exit when No (N);
            Kill_Dead_Code (N);
         end loop;
      end if;
   end Kill_Dead_Code;

   ------------------------
   -- Known_Non_Negative --
   ------------------------

   function Known_Non_Negative (Opnd : Node_Id) return Boolean is
   begin
      if Is_OK_Static_Expression (Opnd)
        and then Expr_Value (Opnd) >= 0
      then
         return True;

      else
         declare
            Lo : constant Node_Id := Type_Low_Bound (Etype (Opnd));

         begin
            return
              Is_OK_Static_Expression (Lo) and then Expr_Value (Lo) >= 0;
         end;
      end if;
   end Known_Non_Negative;

   ----------------------------
   -- Make_Subtype_From_Expr --
   ----------------------------

   --  1. if Expr is an uncontrained array expression, creates
   --    Unc_Type(Expr'first(1)..Expr'Last(1),..., Expr'first(n)..Expr'last(n))

   --  2. if Expr is a unconstrained discriminated type expression, creates
   --    Unc_Type(Expr.Discr1, ... , Expr.Discr_n)

   --  3. if Expr is class-wide, creates an implicit class wide subtype

   function Make_Subtype_From_Expr
     (E       : Node_Id;
      Unc_Typ : Entity_Id)
      return    Node_Id
   is
      Loc         : constant Source_Ptr := Sloc (E);
      List_Constr : List_Id := New_List;
      D           : Entity_Id;

      Full_Subtyp  : Entity_Id;
      Priv_Subtyp  : Entity_Id;
      Utyp         : Entity_Id;
      Full_Exp     : Node_Id;

   begin
      if Is_Private_Type (Unc_Typ)
        and then Has_Unknown_Discriminants (Unc_Typ)
      then

         --  Prepare the subtype completion

         Utyp        := Underlying_Type (Unc_Typ);
         Full_Subtyp := Make_Defining_Identifier (Loc,
                          New_Internal_Name ('C'));
         Full_Exp    := Unchecked_Convert_To (Utyp, Duplicate_Subexpr (E));
         Set_Parent (Full_Exp, Parent (E));

         Priv_Subtyp :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

         Insert_Action (E,
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Full_Subtyp,
             Subtype_Indication  => Make_Subtype_From_Expr (Full_Exp, Utyp)));

         --  Define the dummy private subtype

         Set_Ekind          (Priv_Subtyp, Subtype_Kind (Ekind (Unc_Typ)));
         Set_Etype          (Priv_Subtyp, Unc_Typ);
         Set_Scope          (Priv_Subtyp, Full_Subtyp);
         Set_Is_Constrained (Priv_Subtyp);
         Set_Is_Tagged_Type (Priv_Subtyp, Is_Tagged_Type (Unc_Typ));
         Set_Is_Itype       (Priv_Subtyp);
         Set_Associated_Node_For_Itype (Priv_Subtyp, E);

         if Is_Tagged_Type  (Priv_Subtyp) then
            Set_Class_Wide_Type
              (Base_Type (Priv_Subtyp), Class_Wide_Type (Unc_Typ));
            Set_Primitive_Operations (Priv_Subtyp,
              Primitive_Operations (Unc_Typ));
         end if;

         Set_Full_View (Priv_Subtyp, Full_Subtyp);

         return New_Reference_To (Priv_Subtyp, Loc);

      elsif Is_Array_Type (Unc_Typ) then
         for J in 1 .. Number_Dimensions (Unc_Typ) loop
            Append_To (List_Constr,
              Make_Range (Loc,
                Low_Bound =>
                  Make_Attribute_Reference (Loc,
                    Prefix => Duplicate_Subexpr (E),
                    Attribute_Name => Name_First,
                    Expressions => New_List (
                      Make_Integer_Literal (Loc, J))),
                High_Bound =>
                  Make_Attribute_Reference (Loc,
                    Prefix         => Duplicate_Subexpr (E),
                    Attribute_Name => Name_Last,
                    Expressions    => New_List (
                      Make_Integer_Literal (Loc, J)))));
         end loop;

      elsif Is_Class_Wide_Type (Unc_Typ) then
         declare
            CW_Subtype : Entity_Id;
            EQ_Typ     : Entity_Id := Empty;

         begin
            --  A class-wide equivalent type is not needed when Java_VM
            --  because the JVM back end handles the class-wide object
            --  intialization itself (and doesn't need or want the
            --  additional intermediate type to handle the assignment).

            if Expander_Active and then not Java_VM then
               EQ_Typ := Make_CW_Equivalent_Type (Unc_Typ, E);
            end if;

            CW_Subtype := New_Class_Wide_Subtype (Unc_Typ, E);
            Set_Equivalent_Type (CW_Subtype, EQ_Typ);
            Set_Cloned_Subtype (CW_Subtype, Base_Type (Unc_Typ));

            return New_Occurrence_Of (CW_Subtype, Loc);
         end;

      else
         D := First_Discriminant (Unc_Typ);
         while (Present (D)) loop

            Append_To (List_Constr,
              Make_Selected_Component (Loc,
                Prefix        => Duplicate_Subexpr (E),
                Selector_Name => New_Reference_To (D, Loc)));

            Next_Discriminant (D);
         end loop;
      end if;

      return
        Make_Subtype_Indication (Loc,
          Subtype_Mark => New_Reference_To (Unc_Typ, Loc),
          Constraint   =>
            Make_Index_Or_Discriminant_Constraint (Loc,
              Constraints => List_Constr));
   end Make_Subtype_From_Expr;

   -----------------------------
   -- Make_CW_Equivalent_Type --
   -----------------------------

   --  Create a record type used as an equivalent of any member
   --  of the class which takes its size from exp.

   --  Generate the following code:

   --   type Equiv_T is record
   --     _parent :  T (List of discriminant constaints taken from Exp);
   --     Ext__50 : Storage_Array (1 .. (Exp'size - Typ'size) / Storage_Unit);
   --   end Equiv_T;

   function Make_CW_Equivalent_Type
     (T    : Entity_Id;
      E    : Node_Id)
      return Entity_Id
   is
      Loc         : constant Source_Ptr := Sloc (E);
      Root_Typ    : constant Entity_Id  := Root_Type (T);
      Equiv_Type  : Entity_Id;
      Range_Type  : Entity_Id;
      Str_Type    : Entity_Id;
      List_Def    : List_Id := Empty_List;
      Constr_Root : Entity_Id;
      Sizexpr     : Node_Id;

   begin
      if not Has_Discriminants (Root_Typ) then
         Constr_Root := Root_Typ;
      else
         Constr_Root :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

         --  subtype cstr__n is T (List of discr constraints taken from Exp)

         Append_To (List_Def,
           Make_Subtype_Declaration (Loc,
             Defining_Identifier => Constr_Root,
               Subtype_Indication =>
                 Make_Subtype_From_Expr (E, Root_Typ)));
      end if;

      --  subtype rg__xx is Storage_Offset range
      --                           (Expr'size - typ'size) / Storage_Unit

      Range_Type := Make_Defining_Identifier (Loc, New_Internal_Name ('G'));

      Sizexpr :=
        Make_Op_Subtract (Loc,
          Left_Opnd =>
            Make_Attribute_Reference (Loc,
              Prefix         => OK_Convert_To (T, Duplicate_Subexpr (E)),
              Attribute_Name => Name_Size),
          Right_Opnd =>
            Make_Attribute_Reference (Loc,
              Prefix => New_Reference_To (Constr_Root, Loc),
              Attribute_Name => Name_Size));

      Set_Paren_Count (Sizexpr, 1);

      Append_To (List_Def,
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Range_Type,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Storage_Offset), Loc),
              Constraint => Make_Range_Constraint (Loc,
                Range_Expression =>
                  Make_Range (Loc,
                    Low_Bound => Make_Integer_Literal (Loc, 1),
                    High_Bound =>
                      Make_Op_Divide (Loc,
                        Left_Opnd => Sizexpr,
                        Right_Opnd => Make_Integer_Literal (Loc,
                            Intval => System_Storage_Unit)))))));

      --  subtype str__nn is Storage_Array (rg__x);

      Str_Type := Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      Append_To (List_Def,
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Str_Type,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (RTE (RE_Storage_Array), Loc),
              Constraint =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints =>
                    New_List (New_Reference_To (Range_Type, Loc))))));

      --  type Equiv_T is record
      --    _parent : Tnn;
      --    E : Str_Type;
      --  end Equiv_T;

      Equiv_Type := Make_Defining_Identifier (Loc, New_Internal_Name ('T'));

      --  Avoid the generation of an init procedure

      Set_Is_Frozen (Equiv_Type);

      Set_Ekind (Equiv_Type, E_Record_Type);
      Set_Parent_Subtype (Equiv_Type, Constr_Root);

      Append_To (List_Def,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Equiv_Type,

          Type_Definition =>
            Make_Record_Definition (Loc,
              Component_List => Make_Component_List (Loc,
                Component_Items => New_List (
                  Make_Component_Declaration (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc, Name_uParent),
                    Subtype_Indication => New_Reference_To (Constr_Root, Loc)),

                  Make_Component_Declaration (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc,
                        Chars => New_Internal_Name ('C')),
                    Subtype_Indication => New_Reference_To (Str_Type, Loc))),
                Variant_Part => Empty))));

      Insert_Actions (E, List_Def);
      return Equiv_Type;
   end Make_CW_Equivalent_Type;

   ---------------------------------
   -- Make_Default_Expr_Functions --
   ---------------------------------

   procedure Make_Default_Expr_Functions (N : Node_Id; E : Entity_Id) is
      Loc     : Source_Ptr;
      Formal  : Entity_Id;
      Default : Node_Id;
      Nam     : Name_Id;
      Params  : List_Id;

   begin
      if Default_Expressions_Processed (E)
        or else Is_Eliminated (E)
      then
         return;
      end if;

      --  Default expressions for protected operations are created when
      --  the protected type itself is frozen, so they are defined in the
      --  enclosing scope.

      if Is_Protected_Type (Current_Scope) then
         return;
      end if;

      Formal := First_Formal (E);

      while Present (Formal) loop
         Default := Default_Value (Formal);

         --  If no default expression, then nothing to do

         if No (Default) then
            null;

         --  If default expression is a real literal, integer literal,
         --  character literal, or string literal, then we can copy it
         --  easily, and do not need a function. Same with an identifier
         --  or a Null_Parameter attribute reference.

         --  Special case: Vax float literals are not treated as simply
         --  copied items, since they must be properly expanded.

         --  Another special case, if we are a library level declaration,
         --  then don't create a default expression function, because there
         --  is no place to put it. This is not right  since we really
         --  should analyze this expression in the proper context ???

         elsif (Nkind (Default) = N_Real_Literal
                  and then
                not Vax_Float (Etype (Default)))
           or else Nkind (Default) = N_Integer_Literal
           or else Nkind (Default) = N_Character_Literal
           or else Nkind (Default) = N_String_Literal
           or else Nkind (Default) = N_Null

           or else
               (Nkind (Default) = N_Attribute_Reference
                 and then
                Attribute_Name (Default) = Name_Null_Parameter)

           or else ((Nkind (Default) = N_Identifier
                       or else Nkind (Default) = N_Expanded_Name)
                     and then
                       (Ekind (Entity (Default)) /= E_Discriminant
                          or else not Is_Concurrent_Type
                            (Scope (Entity (Default)))))
           or else
             Nkind (Parent (N)) = N_Compilation_Unit
         then
            null;

         --  If the default function has already been created, nothing to do.
         --  This is the case for imported subprograms which receive a freeze
         --  node after the subprogram declaration has been expanded.

         elsif Present (Default_Expr_Function (Formal)) then
            return;

         --  If the default is a parameterless function call, use the name
         --  of the called function directly. This is efficient, and also
         --  allows dispatching if the default is a tag-indeterminate call.

         elsif Nkind (Default) = N_Function_Call
           and then No (Parameter_Associations (Default))
         then
            Set_Default_Expr_Function
              (Formal, Entity (Name (Default)));

         --  If we have something more complex, construct a function
         --  specification for the function that will be used to evaluate
         --  the default. The body of this function is not constructed
         --  until we encounter the body of the function currently being
         --  processed (since otherwise we would get premature freezing)

         else
            Loc := Sloc (Formal);

            --  We need a name for the function. This will definitely
            --  be referenced externally, and has to be unique in the
            --  presence of overloading etc. We use the name:

            --     subprogDparameter[Dn]

            --  where

            --     subprog is the name of the subprogram

            --     parameter is the name of the formal parameter

            --     n is the number of homonyms in the scope (omit if zero)

            --     If E is an entry or protected operation, the default
            --     function is inserted in the enclosing scope, and it is
            --     there that we look for homonyms.

            Get_Name_String (Chars (Formal));

            declare
               S    : String := Name_Buffer (1 .. Name_Len);
               Ctr  : Nat := 0;
               Ent  : Entity_Id;
               Scop : Entity_Id;

            begin
               Get_Name_String (Chars (E));
               Add_Str_To_Name_Buffer ("D");
               Add_Str_To_Name_Buffer (S);

               if Is_Concurrent_Type (Scope (E)) then
                  Scop := Scope (Scope (E));
               else
                  Scop := Scope (E);
               end if;

               Ent := E;

               loop
                  Ent := Homonym (Ent);
                  exit when No (Ent);

                  if  Scope (Ent) = Scop then
                     Ctr := Ctr + 1;
                  end if;
               end loop;

               if Ctr /= 0 then
                  Add_Str_To_Name_Buffer ("D");
                  Add_Nat_To_Name_Buffer (Ctr);
               end if;

               Nam := Name_Find;
            end;

            Set_Default_Expr_Function
              (Formal, Make_Defining_Identifier (Loc, Nam));

            if Is_Concurrent_Type (Scope (E))
              and then Has_Discriminants (Scope (E))
            then
               Params :=
                 New_List (
                   Make_Parameter_Specification (Loc,
                      Defining_Identifier =>
                         Make_Defining_Identifier (Loc, Chars (Formal)),
                      Parameter_Type      =>
                         New_Occurrence_Of (Scope (E), Loc)));
            else
               Params := No_List;
            end if;

            Insert_Action (N,
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Function_Specification (Loc,
                    Defining_Unit_Name =>
                      Default_Expr_Function (Formal),
                    Parameter_Specifications => Params,
                    Subtype_Mark =>
                      New_Occurrence_Of (Etype (Formal), Loc))));

            Set_Is_Inlined (Default_Expr_Function (Formal));
         end if;

         Next_Formal (Formal);
      end loop;

   end Make_Default_Expr_Functions;

   ------------------------
   -- Make_Literal_Range --
   ------------------------

   function Make_Literal_Range
     (Loc         : Source_Ptr;
      Literal_Typ : Entity_Id;
      Index_Typ   : Entity_Id)
      return        Node_Id
   is
   begin
         return
           Make_Range (Loc,
             Low_Bound =>
               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Index_Typ, Loc),
                 Attribute_Name => Name_First),

             High_Bound =>
               Make_Op_Subtract (Loc,
                  Left_Opnd =>
                    Make_Op_Add (Loc,
                      Left_Opnd =>
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Occurrence_Of (Index_Typ, Loc),
                          Attribute_Name => Name_First),
                      Right_Opnd => Make_Integer_Literal (Loc,
                        String_Literal_Length (Literal_Typ))),
                  Right_Opnd => Make_Integer_Literal (Loc, 1)));
   end Make_Literal_Range;

   -----------------------------
   -- May_Generate_Large_Temp --
   -----------------------------

   --  At the current time, the only types that we return False for (i.e.
   --  where we decide we know they cannot generate large temps) are ones
   --  where we know the size is 128 bits or less at compile time, and we
   --  are still not doing a thorough job on arrays and records ???

   function May_Generate_Large_Temp (Typ : Entity_Id) return Boolean is
   begin
      if not Stack_Checking_Enabled then
         return False;

      elsif not Size_Known_At_Compile_Time (Typ) then
         return False;

      elsif Esize (Typ) /= 0 and then Esize (Typ) <= 256 then
         return False;

      elsif Is_Array_Type (Typ)
        and then Present (Packed_Array_Type (Typ))
      then
         return May_Generate_Large_Temp (Packed_Array_Type (Typ));

      --  We could do more here to find other small types ???

      else
         return True;
      end if;
   end May_Generate_Large_Temp;

   ---------------------
   -- Must_Be_Aligned --
   ---------------------

   function Must_Be_Aligned (Obj : Node_Id) return Boolean is
   begin
      --  If object is strictly aligned, we can quit now

      if Strict_Alignment (Etype (Obj)) then
         return True;

      --  Case of subscripted array reference

      elsif Nkind (Obj) = N_Indexed_Component then

         --  If we have a pointer to an array, then this is definitely
         --  aligned, because pointers always point to aligned versions.

         if Is_Access_Type (Etype (Prefix (Obj))) then
            return True;

         --  Otherwise, go look at the prefix

         else
            return Must_Be_Aligned (Prefix (Obj));
         end if;

      --  Case of record field

      elsif Nkind (Obj) = N_Selected_Component then

         --  What is significant here is whether the record type is packed

         if Is_Record_Type (Etype (Prefix (Obj)))
           and then Is_Packed (Etype (Prefix (Obj)))
         then
            return False;

         --  In all other cases, go look at prefix

         else
            return Must_Be_Aligned (Prefix (Obj));
         end if;

      --  If not selected or indexed component, must be aligned

      else
         return True;
      end if;
   end Must_Be_Aligned;

   ----------------------------
   -- New_Class_Wide_Subtype --
   ----------------------------

   function New_Class_Wide_Subtype
     (CW_Typ : Entity_Id;
      N      : Node_Id)
      return   Entity_Id
   is
      Res      : Entity_Id := Create_Itype (E_Void, N);
      Res_Name : constant Name_Id := Chars (Res);
      Res_Scope : Entity_Id := Scope (Res);

   begin
      Copy_Node (CW_Typ, Res);
      Set_Sloc (Res, Sloc (N));
      Set_Is_Itype (Res);
      Set_Associated_Node_For_Itype (Res, N);
      Set_Public_Status (Res);
      Set_Chars (Res, Res_Name);
      Set_Scope (Res, Res_Scope);
      Set_Ekind (Res, E_Class_Wide_Subtype);
      Set_Next_Entity (Res, Empty);
      Set_Etype (Res, Base_Type (CW_Typ));
      Set_Freeze_Node (Res, Empty);
      return (Res);
   end New_Class_Wide_Subtype;

   -------------------------
   -- Remove_Side_Effects --
   -------------------------

   procedure Remove_Side_Effects
     (Exp          : Node_Id;
      Name_Req     : Boolean := False;
      Variable_Ref : Boolean := False)
   is
      Loc          : constant Source_Ptr := Sloc (Exp);
      Exp_Type     : constant Entity_Id  := Etype (Exp);
      Svg_Suppress : constant Suppress_Record := Scope_Suppress;
      Def_Id       : Entity_Id;
      Ref_Type     : Entity_Id;
      Res          : Node_Id;
      Ptr_Typ_Decl : Node_Id;
      New_Exp      : Node_Id;
      E            : Node_Id;

      function Side_Effect_Free (N : Node_Id) return Boolean;
      --  Determines if the tree N represents an expession that is known
      --  not to have side effects, and for which no processing is required.

      function Side_Effect_Free (L : List_Id) return Boolean;
      --  Determines if all elements of the list L are side effect free

      ----------------------
      -- Side_Effect_Free --
      ----------------------

      function Side_Effect_Free (N : Node_Id) return Boolean is
         K : constant Node_Kind := Nkind (N);

      begin
         --  Note on checks that could raise Constraint_Error. Strictly, if
         --  we take advantage of 11.6, these checks do not count as side
         --  effects. However, we would just as soon consider that they are
         --  side effects, since the backend CSE does not work very well on
         --  expressions which can raise Constraint_Error. On the other
         --  hand, if we do not consider them to be side effect free, then
         --  we get some awkward expansions in -gnato mode, resulting in
         --  code insertions at a point where we do not have a clear model
         --  for performing the insertions. See 4908-002/comment for details.

         --  An attribute reference is side effect free if its expressions
         --  are side effect free and its prefix is (could be a dereference
         --  or an indexed retrieval for example).

         if K = N_Attribute_Reference then
            return Side_Effect_Free (Expressions (N))
              and then (Is_Entity_Name (Prefix (N))
                         or else Side_Effect_Free (Prefix (N)));

         --  An entity is side effect free unless it is a function call, or
         --  a reference to a volatile variable and Name_Req is False. If
         --  Name_Req is True then we can't help returning a name which
         --  effectively allows multiple references in any case.

         elsif Is_Entity_Name (N)
           and then Ekind (Entity (N)) /= E_Function
           and then (not Is_Volatile (Entity (N)) or else Name_Req)
         then
            --  If the entity is a constant, it is definitely side effect
            --  free. Note that the test of Is_Variable (N) below might
            --  be expected to catch this case, but it does not, because
            --  this test goes to the original tree, and we may have
            --  already rewritten a variable node with a constant as
            --  a result of an earlier Force_Evaluation call.

            if Ekind (Entity (N)) = E_Constant then
               return True;

            --  If the Variable_Ref flag is set, any variable reference is
            --  is considered a side-effect

            elsif Variable_Ref then
               return not Is_Variable (N);

            else
               return True;
            end if;

         --  A value known at compile time is always side effect free

         elsif Compile_Time_Known_Value (N) then
            return True;

         --  Literals are always side-effect free

         elsif (K = N_Integer_Literal
                 or else K = N_Real_Literal
                 or else K = N_Character_Literal
                 or else K = N_String_Literal
                 or else K = N_Null)
           and then not Raises_Constraint_Error (N)
         then
            return True;

         --  A type conversion or qualification is side effect free if the
         --  expression to be converted is side effect free.

         elsif K = N_Type_Conversion or else K = N_Qualified_Expression then
            return Side_Effect_Free (Expression (N));

         --  An unchecked type conversion is never side effect free since we
         --  need to check whether it is safe.
         --  effect free if its argument is side effect free.

         elsif K = N_Unchecked_Type_Conversion then
            if Safe_Unchecked_Type_Conversion (N) then
               return Side_Effect_Free (Expression (N));
            else
               return False;
            end if;

         --  A unary operator is side effect free if the operand
         --  is side effect free.

         elsif K in N_Unary_Op then
            return Side_Effect_Free (Right_Opnd (N));

         --  A binary operator is side effect free if and both operands
         --  are side effect free.

         elsif K in N_Binary_Op then
            return Side_Effect_Free (Left_Opnd  (N))
              and then Side_Effect_Free (Right_Opnd (N));

         --  An explicit dereference or selected component is side effect
         --  free if its prefix is side effect free.

         elsif K = N_Explicit_Dereference
           or else K = N_Selected_Component
         then
            return Side_Effect_Free (Prefix (N));

         --  An indexed component can be copied if the prefix is copyable
         --  and all the indexing expressions are copyable and there is
         --  no access check and no range checks.

         elsif K = N_Indexed_Component then
            return Side_Effect_Free (Prefix (N))
              and then Side_Effect_Free (Expressions (N));

         elsif K = N_Unchecked_Expression then
            return Side_Effect_Free (Expression (N));

         --  A call to _rep_to_pos is side effect free, since we generate
         --  this pure function call ourselves. Moreover it is critically
         --  important to make this exception, since otherwise we can
         --  have discriminants in array components which don't look
         --  side effect free in the case of an array whose index type
         --  is an enumeration type with an enumeration rep clause.

         elsif K = N_Function_Call
           and then Nkind (Name (N)) = N_Identifier
           and then Chars (Name (N)) = Name_uRep_To_Pos
         then
            return True;

         --  We consider that anything else has side effects. This is a bit
         --  crude, but we are pretty close for most common cases, and we
         --  are certainly correct (i.e. we never return True when the
         --  answer should be False).

         else
            return False;
         end if;
      end Side_Effect_Free;

      function Side_Effect_Free (L : List_Id) return Boolean is
         N : Node_Id;

      begin
         if L = No_List or else L = Error_List then
            return True;

         else
            N := First (L);

            while Present (N) loop
               if not Side_Effect_Free (N) then
                  return False;
               else
                  Next (N);
               end if;
            end loop;

            return True;
         end if;
      end Side_Effect_Free;

   --  Start of processing for Remove_Side_Effects

   begin
      --  If we are side effect free already or expansion is disabled,
      --  there is nothing to do.

      if Side_Effect_Free (Exp) or else not Expander_Active then
         return;
      end if;

      --  All the must not have any checks

      Scope_Suppress := (others => True);

      --  If the expression has the form v.all then we can just capture
      --  the pointer, and then do an explicit dereference on the result.

      if Nkind (Exp) = N_Explicit_Dereference then
         Def_Id :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
         Res :=
           Make_Explicit_Dereference (Loc, New_Reference_To (Def_Id, Loc));

         Insert_Action (Exp,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   =>
               New_Reference_To (Etype (Prefix (Exp)), Loc),
             Constant_Present    => True,
             Expression          => Relocate_Node (Prefix (Exp))));

      --  If this is a type conversion, leave the type conversion and remove
      --  the side effects in the expression. This is important in several
      --  circumstances: for change of representations, and also when this
      --  is a view conversion to a smaller object, where gigi can end up
      --  its own temporary of the wrong size.
      --  ??? this transformation is inhibited for elementary types that are
      --  not involved in a change of representation because it causes
      --  regressions that are not fully understood yet.

      elsif Nkind (Exp) = N_Type_Conversion
        and then (not Is_Elementary_Type (Underlying_Type (Exp_Type))
                   or else Nkind (Parent (Exp)) = N_Assignment_Statement)
      then
         Remove_Side_Effects (Expression (Exp), Variable_Ref);
         Scope_Suppress := Svg_Suppress;
         return;

      --  For expressions that denote objects, we can use a renaming scheme.
      --  We skip using this if we have a volatile variable and we do not
      --  have Nam_Req set true (see comments above for Side_Effect_Free).
      --  We also skip this scheme for class-wide expressions in order to
      --  avoid recursive expension (see Expand_N_Object_Renaming_Declaration)

      elsif Is_Object_Reference (Exp)
        and then not Variable_Ref
        and then (Name_Req
                   or else not Is_Entity_Name (Exp)
                   or else not Is_Volatile (Entity (Exp)))
        and then not Is_Class_Wide_Type (Exp_Type)
      then
         Def_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

         if Nkind (Exp) = N_Selected_Component
           and then Nkind (Prefix (Exp)) = N_Function_Call
           and then Is_Array_Type (Etype (Exp))
         then
            --  Avoid generating a variable-sized temporary, by generating
            --  the renaming declaration just for the function call. The
            --  transformation could be refined to apply only when the array
            --  component is constrained by a discriminant???

            Res :=
              Make_Selected_Component (Loc,
                Prefix => New_Occurrence_Of (Def_Id, Loc),
                Selector_Name => Selector_Name (Exp));

            Insert_Action (Exp,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Subtype_Mark        =>
                  New_Reference_To (Base_Type (Etype (Prefix (Exp))), Loc),
                Name                => Relocate_Node (Prefix (Exp))));
         else
            Res := New_Reference_To (Def_Id, Loc);

            Insert_Action (Exp,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Def_Id,
                Subtype_Mark        => New_Reference_To (Exp_Type, Loc),
                Name                => Relocate_Node (Exp)));
         end if;

      --  If it is a scalar type, just make a copy. Likewise if this is
      --  an unchecked conversion that Gigi can't handle.

      elsif Is_Elementary_Type (Exp_Type)
        or else (Nkind (Exp) = N_Unchecked_Type_Conversion
                  and then not Safe_Unchecked_Type_Conversion (Exp))
      then
         Def_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
         Set_Etype (Def_Id, Exp_Type);
         Res := New_Reference_To (Def_Id, Loc);

         E :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   => New_Reference_To (Exp_Type, Loc),
             Constant_Present    => True,
             Expression          => Relocate_Node (Exp));

         Set_Assignment_OK (E);
         Insert_Action (Exp, E);

      --  Otherwise we generate a reference to the value

      else
         Ref_Type := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

         Ptr_Typ_Decl :=
           Make_Full_Type_Declaration (Loc,
             Defining_Identifier => Ref_Type,
             Type_Definition =>
               Make_Access_To_Object_Definition (Loc,
                 All_Present => True,
                 Subtype_Indication =>
                   New_Reference_To (Exp_Type, Loc)));

         E := Exp;
         Insert_Action (Exp, Ptr_Typ_Decl);

         Def_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
         Set_Etype (Def_Id, Exp_Type);

         Res :=
           Make_Explicit_Dereference (Loc,
             Prefix => New_Reference_To (Def_Id, Loc));

         if Nkind (E) = N_Explicit_Dereference then
            New_Exp := Relocate_Node (Prefix (E));
         else
            E := Relocate_Node (E);
            New_Exp := Make_Reference (Loc, E);
         end if;

         if Nkind (E) = N_Aggregate and then Expansion_Delayed (E) then
            Set_Expansion_Delayed (E, False);
            Set_Analyzed (E, False);
         end if;

         Insert_Action (Exp,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Def_Id,
             Object_Definition   => New_Reference_To (Ref_Type, Loc),
             Expression          => New_Exp));
      end if;

      --  Preserve the Assignment_OK flag in all copies, since at least
      --  one copy may be used in a context where this flag must be set
      --  (otherwise why would the flag be set in the first place).

      Set_Assignment_OK (Res, Assignment_OK (Exp));

      --  Finally rewrite the original expression and we are done

      Rewrite (Exp, Res);
      Analyze_And_Resolve (Exp, Exp_Type);
      Scope_Suppress := Svg_Suppress;
   end Remove_Side_Effects;

   ------------------------------------
   -- Safe_Unchecked_Type_Conversion --
   ------------------------------------

   --  Note: this function knows quite a bit about the exact requirements
   --  of Gigi with respect to unchecked type conversions, and its code
   --  must be coordinated with any changes in Gigi in this area.

   --  The above requirements should be documented in Sinfo ???

   function Safe_Unchecked_Type_Conversion (Exp : Node_Id) return Boolean is
      Otyp   : Entity_Id;
      Ityp   : Entity_Id;
      Oalign : Uint;
      Ialign : Uint;

   begin
      --  If the expression is the RHS of an assignment or object declaration
      --  or the Prefix of an N_Selected_Component, we are always OK. The
      --  former two cases is because there will always be a target and the
      --  latter because GCC knowns to look inside the conversion.

      if (Nkind (Parent (Exp)) = N_Assignment_Statement
           and then Expression (Parent (Exp)) = Exp)
        or else (Nkind (Parent (Exp)) = N_Selected_Component
                  and then Prefix (Parent (Exp)) = Exp)
        or else Nkind (Parent (Exp)) = N_Object_Declaration
      then
         return True;
      end if;

      --  Set the output type, this comes from Etype if it is set, otherwise
      --  we take it from the subtype mark, which we assume was already
      --  fully analyzed.

      if Present (Etype (Exp)) then
         Otyp := Etype (Exp);
      else
         Otyp := Entity (Subtype_Mark (Exp));
      end if;

      --  The input type always comes from the expression, and we assume
      --  this is indeed always analyzed, so we can simply get the Etype.

      Ityp := Etype (Expression (Exp));

      --  Initialize alignments to unknown so far

      Oalign := No_Uint;
      Ialign := No_Uint;

      --  Replace a concurrent type by its corresponding record type
      --  and each type by its underlying type and do the tests on those.
      --  The original type may be a private type whose completion is a
      --  concurrent type, so find the underlying type first.

      if Present (Underlying_Type (Otyp)) then
         Otyp := Underlying_Type (Otyp);
      end if;

      if Present (Underlying_Type (Ityp)) then
         Ityp := Underlying_Type (Ityp);
      end if;

      if Is_Concurrent_Type (Otyp) then
         Otyp := Corresponding_Record_Type (Otyp);
      end if;

      if Is_Concurrent_Type (Ityp) then
         Ityp := Corresponding_Record_Type (Ityp);
      end if;

      --  If the base types are the same, we know there is no problem since
      --  this conversion will be a noop.

      if Implementation_Base_Type (Otyp) = Implementation_Base_Type (Ityp) then
         return True;

      --  If the size of the input or output type is known at compile time,
      --  there is never a problem.  Note that unconstrained records are
      --  considered to be of known size, but we can't consider them that way
      --  here, because we are talking about the actual size of the object.

      --  We also make sure that in addition to the size being known, we do
      --  not have a case which might generate an embarrassingly large temp
      --  in stack checking mode.

      elsif Size_Known_At_Compile_Time (Otyp)
        and then not May_Generate_Large_Temp (Otyp)
        and then not (Is_Record_Type (Otyp) and then not Is_Constrained (Otyp))
      then
         return True;

      elsif Size_Known_At_Compile_Time (Ityp)
        and then not May_Generate_Large_Temp (Ityp)
        and then not (Is_Record_Type (Ityp) and then not Is_Constrained (Ityp))
      then
         return True;

      --  If either type is tagged, then we know the alignment is OK so
      --  Gigi will be able to use pointer punning.

      elsif Is_Tagged_Type (Otyp) or else Is_Tagged_Type (Ityp) then
         return True;

      --  If either type is a limited record type, we cannot do a copy, so
      --  say safe since there's nothing else we can do.

      elsif Is_Limited_Record (Otyp) or else Is_Limited_Record (Ityp) then
         return True;

      --  Conversions to and from packed array types are always ignored and
      --  hence are safe.

      elsif Is_Packed_Array_Type (Otyp)
        or else Is_Packed_Array_Type (Ityp)
      then
         return True;
      end if;

      --  The only other cases known to be safe is if the input type's
      --  alignment is known to be at least the maximum alignment for the
      --  target or if both alignments are known and the output type's
      --  alignment is no stricter than the input's.  We can use the alignment
      --  of the component type of an array if a type is an unpacked
      --  array type.

      if Present (Alignment_Clause (Otyp)) then
         Oalign := Expr_Value (Expression (Alignment_Clause (Otyp)));

      elsif Is_Array_Type (Otyp)
        and then Present (Alignment_Clause (Component_Type (Otyp)))
      then
         Oalign := Expr_Value (Expression (Alignment_Clause
                                           (Component_Type (Otyp))));
      end if;

      if Present (Alignment_Clause (Ityp)) then
         Ialign := Expr_Value (Expression (Alignment_Clause (Ityp)));

      elsif Is_Array_Type (Ityp)
        and then Present (Alignment_Clause (Component_Type (Ityp)))
      then
         Ialign := Expr_Value (Expression (Alignment_Clause
                                           (Component_Type (Ityp))));
      end if;

      if Ialign /= No_Uint and then Ialign > Maximum_Alignment then
         return True;

      elsif Ialign /= No_Uint and then Oalign /= No_Uint
        and then Ialign <= Oalign
      then
         return True;

      --   Otherwise, Gigi cannot handle this and we must make a temporary.

      else
         return False;
      end if;

   end Safe_Unchecked_Type_Conversion;

   ----------------------------
   -- Is_Untagged_Derivation --
   ----------------------------

   function Is_Untagged_Derivation (T : Entity_Id) return Boolean is
   begin
      return (not Is_Tagged_Type (T) and then Is_Derived_Type (T))
               or else
             (Is_Private_Type (T) and then Present (Full_View (T))
               and then not Is_Tagged_Type (Full_View (T))
               and then Is_Derived_Type (Full_View (T))
               and then Etype (Full_View (T)) /= T);

   end Is_Untagged_Derivation;

   --------------------------
   -- Set_Elaboration_Flag --
   --------------------------

   procedure Set_Elaboration_Flag (N : Node_Id; Spec_Id : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Asn : Node_Id;
      AXN : Node_Id;

   begin
      if Present (Elaboration_Entity (Spec_Id)) then
         Check_Restriction (No_Elaboration_Code, N);
         Asn :=
           Make_Assignment_Statement (Loc,
             Name => New_Occurrence_Of (Elaboration_Entity (Spec_Id), Loc),
             Expression => New_Occurrence_Of (Standard_True, Loc));

         if Nkind (Parent (N)) = N_Compilation_Unit then
            AXN := Aux_Decls_Node (Parent (N));

            if No (Actions (AXN)) then
               Set_Actions (AXN, New_List);
            end if;

            Append (Asn, Actions (AXN));

         elsif Nkind (Parent (N)) = N_Subunit then
            Insert_After (Corresponding_Stub (Parent (N)), Asn);

         else
            Insert_After (N, Asn);
         end if;

         Analyze (Asn);
      end if;
   end Set_Elaboration_Flag;

   ----------------------------
   -- Wrap_Cleanup_Procedure --
   ----------------------------

   procedure Wrap_Cleanup_Procedure (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Stseq : constant Node_Id    := Handled_Statement_Sequence (N);
      Stmts : constant List_Id    := Statements (Stseq);

   begin
      if Abort_Allowed then
         Prepend_To (Stmts, Build_Runtime_Call (Loc, RE_Abort_Defer));
         Append_To  (Stmts, Build_Runtime_Call (Loc, RE_Abort_Undefer));
      end if;
   end Wrap_Cleanup_Procedure;

end Exp_Util;
