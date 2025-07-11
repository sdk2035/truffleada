------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ U T I L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.503 $
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
with Casing;   use Casing;
with Debug;    use Debug;
with Errout;   use Errout;
with Elists;   use Elists;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Output;   use Output;
with Opt;      use Opt;
with Scans;    use Scans;
with Scn;      use Scn;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Style;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;

package body Sem_Util is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Build_Component_Subtype
     (C    : List_Id;
      Loc  : Source_Ptr;
      T    : Entity_Id)
      return Node_Id;
   --  This function builds the subtype for Build_Actual_Subtype_Of_Component
   --  and Build_Discriminal_Subtype_Of_Component. C is a list of constraints,
   --  Loc is the source location, T is the original subtype.

   --------------------------------
   -- Add_Access_Type_To_Process --
   --------------------------------

   procedure Add_Access_Type_To_Process (E : Entity_Id; A : Entity_Id)
   is
      L : Elist_Id;
   begin
      Ensure_Freeze_Node (E);
      L := Access_Types_To_Process (Freeze_Node (E));

      if No (L) then
         L := New_Elmt_List;
         Set_Access_Types_To_Process (Freeze_Node (E), L);
      end if;

      Append_Elmt (A, L);
   end Add_Access_Type_To_Process;

   -----------------------------------------
   -- Apply_Compile_Time_Constraint_Error --
   -----------------------------------------

   procedure Apply_Compile_Time_Constraint_Error
     (N   : Node_Id;
      Msg : String;
      Ent : Entity_Id  := Empty;
      Typ : Entity_Id  := Empty;
      Loc : Source_Ptr := No_Location)
   is
      Stat : constant Boolean := Is_Static_Expression (N);
      Rtyp : Entity_Id;

   begin
      if No (Typ) then
         Rtyp := Etype (N);
      else
         Rtyp := Typ;
      end if;

      if not Present (Compile_Time_Constraint_Error (N, Msg, Ent, Loc)) then
         return;
      end if;

      --  Now we replace the node by an N_Raise_Constraint_Error node
      --  This does not need reanalyzing, so set it as analyzed now.

      Rewrite (N, Make_Raise_Constraint_Error (Sloc (N)));
      Set_Analyzed (N, True);
      Set_Etype (N, Rtyp);
      Set_Raises_Constraint_Error (N);

      --  If the original expression was marked as static, the result is
      --  still marked as static, but the Raises_Constraint_Error flag is
      --  set so that further static evaluation is not attempted.

      if Stat then
         Set_Is_Static_Expression (N);
      end if;

   end Apply_Compile_Time_Constraint_Error;

   -----------------------
   -- Alignment_In_Bits --
   -----------------------

   function Alignment_In_Bits (E : Entity_Id) return Uint is
   begin
      return Alignment (E) * System_Storage_Unit;
   end Alignment_In_Bits;

   --------------------------
   -- Build_Actual_Subtype --
   --------------------------

   function Build_Actual_Subtype
     (T    : Entity_Id;
      N    : Node_Or_Entity_Id)
      return Node_Id
   is
      Obj : Node_Id;

      Loc         : constant Source_Ptr := Sloc (N);
      Constraints : List_Id;
      Decl        : Node_Id;
      Discr       : Entity_Id;
      Hi          : Node_Id;
      Lo          : Node_Id;
      Subt        : Entity_Id;
      Disc_Type   : Entity_Id;

   begin
      if Nkind (N) = N_Defining_Identifier then
         Obj := New_Reference_To (N, Loc);
      else
         Obj := N;
      end if;

      if Is_Array_Type (T) then
         Constraints := New_List;

         for J in 1 .. Number_Dimensions (T) loop

            --  Build an array subtype declaration with the nominal
            --  subtype and the bounds of the actual. Add the declaration
            --  in front of the local declarations for the subprogram,for
            --  analysis before any reference to the formal in the body.

            Lo :=
              Make_Attribute_Reference (Loc,
                Prefix         => Duplicate_Subexpr (Obj, Name_Req => True),
                Attribute_Name => Name_First,
                Expressions    => New_List (
                  Make_Integer_Literal (Loc, J)));

            Hi :=
              Make_Attribute_Reference (Loc,
                Prefix         => Duplicate_Subexpr (Obj, Name_Req => True),
                Attribute_Name => Name_Last,
                Expressions    => New_List (
                  Make_Integer_Literal (Loc, J)));

            Append (Make_Range (Loc, Lo, Hi), Constraints);
         end loop;

      --  If the type has unknown discriminants there is no constrained
      --  subtype to build.

      elsif Has_Unknown_Discriminants (T) then
         return T;

      else
         Constraints := New_List;

         if Is_Private_Type (T) and then No (Full_View (T)) then

            --  Type is a generic derived type. Inherit discriminants from
            --  Parent type.

            Disc_Type := Etype (Base_Type (T));
         else
            Disc_Type := T;
         end if;

         Discr := First_Discriminant (Disc_Type);

         while Present (Discr) loop
            Append_To (Constraints,
              Make_Selected_Component (Loc,
                Prefix => Duplicate_Subexpr (Obj),
                Selector_Name => New_Occurrence_Of (Discr, Loc)));
            Next_Discriminant (Discr);
         end loop;
      end if;

      Subt :=
        Make_Defining_Identifier (Loc,
          Chars => New_Internal_Name ('S'));
      Set_Is_Internal (Subt);

      Decl :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Subt,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (T,  Loc),
              Constraint  =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => Constraints)));

      Mark_Rewrite_Insertion (Decl);
      return Decl;
   end Build_Actual_Subtype;

   ---------------------------------------
   -- Build_Actual_Subtype_Of_Component --
   ---------------------------------------

   function Build_Actual_Subtype_Of_Component
     (T    : Entity_Id;
      N    : Node_Id)
      return Node_Id
   is
      Loc       : constant Source_Ptr := Sloc (N);
      P         : constant Node_Id    := Prefix (N);
      D         : Elmt_Id;
      Id        : Node_Id;
      Indx_Type : Entity_Id;

      Deaccessed_T : Entity_Id;
      --  This is either a copy of T, or if T is an access type, then it is
      --  the directly designated type of this access type.

      function Build_Actual_Array_Constraint return List_Id;
      --  If one or more of the bounds of the component depends on
      --  discriminants, build  actual constraint using the discriminants
      --  of the prefix.

      function Build_Actual_Record_Constraint return List_Id;
      --  Similar to previous one, for discriminated components constrained
      --  by the discriminant of the enclosing object.

      -----------------------------------
      -- Build_Actual_Array_Constraint --
      -----------------------------------

      function Build_Actual_Array_Constraint return List_Id is
         Constraints : List_Id := New_List;
         Indx        : Node_Id;
         Hi          : Node_Id;
         Lo          : Node_Id;
         Old_Hi      : Node_Id;
         Old_Lo      : Node_Id;

      begin
         Indx := First_Index (Deaccessed_T);
         while Present (Indx) loop
            Old_Lo := Type_Low_Bound  (Etype (Indx));
            Old_Hi := Type_High_Bound (Etype (Indx));

            if Denotes_Discriminant (Old_Lo) then
               Lo :=
                 Make_Selected_Component (Loc,
                   Prefix => New_Copy_Tree (P),
                   Selector_Name => New_Occurrence_Of (Entity (Old_Lo), Loc));

            else
               Lo := New_Copy_Tree (Old_Lo);

               --  The new bound will be reanalyzed in the enclosing
               --  declaration. For literal bounds that come from a type
               --  declaration, the type of the context must be imposed, so
               --  insure that analysis will take place. For non-universal
               --  types this is not strictly necessary.

               Set_Analyzed (Lo, False);
            end if;

            if Denotes_Discriminant (Old_Hi) then
               Hi :=
                 Make_Selected_Component (Loc,
                   Prefix => New_Copy_Tree (P),
                   Selector_Name => New_Occurrence_Of (Entity (Old_Hi), Loc));

            else
               Hi := New_Copy_Tree (Old_Hi);
               Set_Analyzed (Hi, False);
            end if;

            Append (Make_Range (Loc, Lo, Hi), Constraints);
            Next_Index (Indx);
         end loop;

         return Constraints;
      end Build_Actual_Array_Constraint;

      ------------------------------------
      -- Build_Actual_Record_Constraint --
      ------------------------------------

      function Build_Actual_Record_Constraint return List_Id is
         Constraints : List_Id := New_List;
         D           : Elmt_Id;
         D_Val       : Node_Id;

      begin
         D := First_Elmt (Discriminant_Constraint (Deaccessed_T));
         while Present (D) loop

            if Denotes_Discriminant (Node (D)) then
               D_Val :=  Make_Selected_Component (Loc,
                 Prefix => New_Copy_Tree (P),
                Selector_Name => New_Occurrence_Of (Entity (Node (D)), Loc));

            else
               D_Val := New_Copy_Tree (Node (D));
            end if;

            Append (D_Val, Constraints);
            Next_Elmt (D);
         end loop;

         return Constraints;
      end Build_Actual_Record_Constraint;

   --  Start of processing for Build_Actual_Subtype_Of_Component

   begin
      if Nkind (N) = N_Explicit_Dereference then
         if Is_Composite_Type (T)
           and then not Is_Constrained (T)
           and then not (Is_Class_Wide_Type (T)
                          and then Is_Constrained (Root_Type (T)))
           and then not Has_Unknown_Discriminants (T)
         then
            Remove_Side_Effects (P);
            return Build_Actual_Subtype (T, N);
         else
            return Empty;
         end if;
      end if;

      if Ekind (T) = E_Access_Subtype then
         Deaccessed_T := Designated_Type (T);
      else
         Deaccessed_T := T;
      end if;

      if Ekind (Deaccessed_T) = E_Array_Subtype then

         Id := First_Index (Deaccessed_T);
         Indx_Type := Underlying_Type (Etype (Id));

         while Present (Id) loop

            if Denotes_Discriminant (Type_Low_Bound  (Indx_Type)) or else
               Denotes_Discriminant (Type_High_Bound (Indx_Type))
            then
               Remove_Side_Effects (P);
               return
                 Build_Component_Subtype (
                   Build_Actual_Array_Constraint, Loc, Base_Type (T));
            end if;

            Next_Index (Id);
         end loop;

      elsif Is_Composite_Type (Deaccessed_T)
        and then Has_Discriminants (Deaccessed_T)
        and then not Has_Unknown_Discriminants (Deaccessed_T)
      then
         D := First_Elmt (Discriminant_Constraint (Deaccessed_T));
         while Present (D) loop

            if Denotes_Discriminant (Node (D)) then
               Remove_Side_Effects (P);
               return
                 Build_Component_Subtype (
                   Build_Actual_Record_Constraint, Loc, Base_Type (T));
            end if;

            Next_Elmt (D);
         end loop;
      end if;

      --  If none of the above, the actual and nominal subtypes are the same.

      return Empty;

   end Build_Actual_Subtype_Of_Component;

   -----------------------------
   -- Build_Component_Subtype --
   -----------------------------

   function Build_Component_Subtype
     (C    : List_Id;
      Loc  : Source_Ptr;
      T    : Entity_Id)
      return Node_Id
   is
      Subt : Entity_Id;
      Decl : Node_Id;

   begin
      Subt :=
        Make_Defining_Identifier (Loc,
          Chars => New_Internal_Name ('S'));
      Set_Is_Internal (Subt);

      Decl :=
        Make_Subtype_Declaration (Loc,
          Defining_Identifier => Subt,
          Subtype_Indication =>
            Make_Subtype_Indication (Loc,
              Subtype_Mark => New_Reference_To (Base_Type (T),  Loc),
              Constraint  =>
                Make_Index_Or_Discriminant_Constraint (Loc,
                  Constraints => C)));

      Mark_Rewrite_Insertion (Decl);
      return Decl;
   end Build_Component_Subtype;

   --------------------------------------------
   -- Build_Discriminal_Subtype_Of_Component --
   --------------------------------------------

   function Build_Discriminal_Subtype_Of_Component
     (T    : Entity_Id)
      return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (T);
      D   : Elmt_Id;
      Id  : Node_Id;

      function Build_Discriminal_Array_Constraint return List_Id;
      --  If one or more of the bounds of the component depends on
      --  discriminants, build  actual constraint using the discriminants
      --  of the prefix.

      function Build_Discriminal_Record_Constraint return List_Id;
      --  Similar to previous one, for discriminated components constrained
      --  by the discriminant of the enclosing object.

      ----------------------------------------
      -- Build_Discriminal_Array_Constraint --
      ----------------------------------------

      function Build_Discriminal_Array_Constraint return List_Id is
         Constraints : List_Id := New_List;
         Indx        : Node_Id;
         Hi          : Node_Id;
         Lo          : Node_Id;
         Old_Hi      : Node_Id;
         Old_Lo      : Node_Id;

      begin
         Indx := First_Index (T);
         while Present (Indx) loop
            Old_Lo := Type_Low_Bound  (Etype (Indx));
            Old_Hi := Type_High_Bound (Etype (Indx));

            if Denotes_Discriminant (Old_Lo) then
               Lo := New_Occurrence_Of (Discriminal (Entity (Old_Lo)), Loc);

            else
               Lo := New_Copy_Tree (Old_Lo);
            end if;

            if Denotes_Discriminant (Old_Hi) then
               Hi := New_Occurrence_Of (Discriminal (Entity (Old_Hi)), Loc);

            else
               Hi := New_Copy_Tree (Old_Hi);
            end if;

            Append (Make_Range (Loc, Lo, Hi), Constraints);
            Next_Index (Indx);
         end loop;

         return Constraints;
      end Build_Discriminal_Array_Constraint;

      -----------------------------------------
      -- Build_Discriminal_Record_Constraint --
      -----------------------------------------

      function Build_Discriminal_Record_Constraint return List_Id is
         Constraints     : List_Id := New_List;
         D     : Elmt_Id;
         D_Val : Node_Id;

      begin
         D := First_Elmt (Discriminant_Constraint (T));
         while Present (D) loop

            if Denotes_Discriminant (Node (D)) then
               D_Val :=
                 New_Occurrence_Of (Discriminal (Entity (Node (D))), Loc);

            else
               D_Val := New_Copy_Tree (Node (D));
            end if;

            Append (D_Val, Constraints);
            Next_Elmt (D);
         end loop;

         return Constraints;
      end Build_Discriminal_Record_Constraint;

   --  Start of processing for Build_Discriminal_Subtype_Of_Component

   begin
      if Ekind (T) = E_Array_Subtype then

         Id := First_Index (T);

         while Present (Id) loop

            if Denotes_Discriminant (Type_Low_Bound  (Etype (Id))) or else
               Denotes_Discriminant (Type_High_Bound (Etype (Id)))
            then
               return Build_Component_Subtype
                 (Build_Discriminal_Array_Constraint, Loc, T);
            end if;

            Next_Index (Id);
         end loop;

      elsif Ekind (T) = E_Record_Subtype
        and then Has_Discriminants (T)
        and then not Has_Unknown_Discriminants (T)
      then
         D := First_Elmt (Discriminant_Constraint (T));
         while Present (D) loop

            if Denotes_Discriminant (Node (D)) then
               return Build_Component_Subtype
                 (Build_Discriminal_Record_Constraint, Loc, T);
            end if;

            Next_Elmt (D);
         end loop;
      end if;

      --  If none of the above, the actual and nominal subtypes are the same.

      return Empty;

   end Build_Discriminal_Subtype_Of_Component;

   --------------------------
   -- Check_Fully_Declared --
   --------------------------

   procedure Check_Fully_Declared (T : Entity_Id; N : Node_Id) is
   begin
      if Ekind (T) = E_Incomplete_Type then
         Error_Msg_NE
           ("premature usage of incomplete}", N, First_Subtype (T));

      elsif Has_Private_Component (T)
        and then not Is_Generic_Type (Root_Type (T))
        and then not In_Default_Expression
      then
         Error_Msg_NE
           ("premature usage of incomplete}", N, First_Subtype (T));
      end if;
   end Check_Fully_Declared;

   ------------------------------------------
   -- Check_Potentially_Blocking_Operation --
   ------------------------------------------

   procedure Check_Potentially_Blocking_Operation (N : Node_Id) is
      S : Entity_Id;

   begin
      --  N is one of the potentially blocking operations listed in
      --  9.5.1 (8). Raise Program_Error before N if the context is
      --  a protected action. Indirect blocking through a subprogram call
      --  cannot be diagnosed statically without interprocedural analysis,
      --  so we do not attempt to do it here.

      S := Scope (Current_Scope);

      while Present (S) and then S /= Standard_Standard loop

         if Is_Protected_Type (S) then
            Error_Msg_N
              ("potentially blocking operation in protected operation?", N);
            return;
         end if;

         S := Scope (S);
      end loop;
   end Check_Potentially_Blocking_Operation;

   ---------------
   -- Check_VMS --
   ---------------

   procedure Check_VMS (Construct : Node_Id) is
   begin
      if not OpenVMS_On_Target and not Debug_Flag_M then
         Error_Msg_N
           ("this construct is allowed only in Open'V'M'S", Construct);
      end if;
   end Check_VMS;

   ----------------------------------
   -- Collect_Primitive_Operations --
   ----------------------------------

   function Collect_Primitive_Operations (T : Entity_Id) return Elist_Id is
      B_Type         : constant Entity_Id := Base_Type (T);
      B_Decl         : constant Node_Id   := Original_Node (Parent (B_Type));
      B_Scope        : Entity_Id          := Scope (B_Type);
      Op_List        : Elist_Id;
      Formal         : Entity_Id;
      Is_Prim        : Boolean;
      Formal_Derived : Boolean := False;
      Id             : Entity_Id;

   begin
      --  For tagged types, the primitive operations are collected as they
      --  are declared, and held in an explicit list which is simply returned.

      if Is_Tagged_Type (B_Type) then
         return Primitive_Operations (B_Type);

      --  An untagged generic type that is a derived type inherits the
      --  primitive operations of its parent type. Other formal types only
      --  have predefined operators, which are not explicitly represented.

      elsif Is_Generic_Type (B_Type) then
         if Nkind (B_Decl) = N_Formal_Type_Declaration
           and then Nkind (Formal_Type_Definition (B_Decl))
             = N_Formal_Derived_Type_Definition
         then
            Formal_Derived := True;
         else
            return New_Elmt_List;
         end if;
      end if;

      Op_List := New_Elmt_List;

      if B_Scope = Standard_Standard then
         if B_Type = Standard_String then
            Append_Elmt (Standard_Op_Concat, Op_List);

         elsif B_Type = Standard_Wide_String then
            Append_Elmt (Standard_Op_Concatw, Op_List);

         else
            null;
         end if;

      elsif (Is_Package (B_Scope)
               and then Nkind (
                 Parent (Declaration_Node (First_Subtype (T))))
                   /=  N_Package_Body)

        or else Is_Derived_Type (B_Type)
      then
         --  The primitive operations appear after the base type, except
         --  if the derivation happens within the private part of B_Scope
         --  and the type is a private type, in which case both the type
         --  and some primitive operations may appear before the base
         --  type, and the list of candidates starts after the type.

         if In_Open_Scopes (B_Scope)
           and then Scope (T) = B_Scope
           and then In_Private_Part (B_Scope)
         then
            Id := Next_Entity (T);
         else
            Id := Next_Entity (B_Type);
         end if;

         while Present (Id) loop

            --  Note that generic formal subprograms are not
            --  considered to be primitive operations and thus
            --  are never inherited.

            if Is_Overloadable (Id)
              and then Nkind (Parent (Parent (Id)))
                         /= N_Formal_Subprogram_Declaration
            then
               Is_Prim := False;

               if Base_Type (Etype (Id)) = B_Type then
                  Is_Prim := True;
               else
                  Formal := First_Formal (Id);
                  while Present (Formal) loop
                     if Base_Type (Etype (Formal)) = B_Type then
                        Is_Prim := True;
                        exit;

                     elsif Ekind (Etype (Formal)) = E_Anonymous_Access_Type
                       and then Base_Type
                         (Designated_Type (Etype (Formal))) = B_Type
                     then
                        Is_Prim := True;
                        exit;
                     end if;

                     Next_Formal (Formal);
                  end loop;
               end if;

               --  For a formal derived type, the only primitives are the
               --  ones inherited from the parent type. Operations appearing
               --  in the package declaration are not primitive for it.

               if Is_Prim
                 and then (not Formal_Derived
                            or else Present (Alias (Id)))
               then
                  Append_Elmt (Id, Op_List);
               end if;
            end if;

            Next_Entity (Id);

            --  For a type declared in System, some of its operations
            --  may appear in  the target-specific extension to System.

            if No (Id)
              and then Chars (B_Scope) = Name_System
              and then Scope (B_Scope) = Standard_Standard
              and then Present_System_Aux
            then
               B_Scope := System_Aux_Id;
               Id := First_Entity (System_Aux_Id);
            end if;

         end loop;

      end if;

      return Op_List;
   end Collect_Primitive_Operations;

   -----------------------------------
   -- Compile_Time_Constraint_Error --
   -----------------------------------

   function Compile_Time_Constraint_Error
     (N    : Node_Id;
      Msg  : String;
      Ent  : Entity_Id  := Empty;
      Loc  : Source_Ptr := No_Location)
      return Node_Id
   is
      Msgc : String (1 .. Msg'Length + 2);
      Msgl : Natural;
      Warn : Boolean;
      P    : Node_Id;
      Msgs : Boolean;

   begin
      --  A static constraint error in an instance body is not a fatal error.
      --  we choose to inhibit the message altogether, because there is no
      --  obvious node (for now) on which to post it. On the other hand the
      --  offending node must be replaced with a constraint_error in any case.

      --  No messages are generated if we already posted an error on this node

      if not Error_Posted (N) then

         --  Make all such messages unconditional

         Msgc (1 .. Msg'Length) := Msg;
         Msgc (Msg'Length + 1) := '!';
         Msgl := Msg'Length + 1;

         --  Message is a warning, even in Ada 95 case

         if Msg (Msg'Length) = '?' then
            Warn := True;

         --  In Ada 83, all messages are warnings. In the private part and
         --  the body of an instance, constraint_checks are only warnings.

         elsif Ada_83 and then Comes_From_Source (N) then

            Msgl := Msgl + 1;
            Msgc (Msgl) := '?';
            Warn := True;

         elsif In_Instance_Not_Visible then

            Msgl := Msgl + 1;
            Msgc (Msgl) := '?';
            Warn := True;
            Warn_On_Instance := True;

         --  Otherwise we have a real error message (Ada 95 static case)

         else
            Warn := False;
         end if;

         --  Should we generate a warning? The answer is not quite yes. The
         --  very annoying exception occurs in the case of a short circuit
         --  operator where the left operand is static and decisive. Climb
         --  parents to see if that is the case we have here.

         Msgs := True;
         P := N;

         loop
            P := Parent (P);
            exit when Nkind (P) not in N_Subexpr;

            if (Nkind (P) = N_And_Then
                and then Compile_Time_Known_Value (Left_Opnd (P))
                and then Is_False (Expr_Value (Left_Opnd (P))))
              or else (Nkind (P) = N_Or_Else
                and then Compile_Time_Known_Value (Left_Opnd (P))
                and then Is_True (Expr_Value (Left_Opnd (P))))
            then
               Msgs := False;
               exit;
            end if;
         end loop;

         if Msgs then
            if Present (Ent) then
               Error_Msg_NE (Msgc (1 .. Msgl), N, Ent);
            else
               Error_Msg_NE (Msgc (1 .. Msgl), N, Etype (N));
            end if;

            if Warn then
               if Inside_Init_Proc then
                  Error_Msg_NE
                    ("\& will be raised for objects of this type!?",
                     N, Standard_Constraint_Error);
               else
                  Error_Msg_NE
                    ("\& will be raised at run time!?",
                     N, Standard_Constraint_Error);
               end if;
            else
               Error_Msg_NE
                 ("\static expression raises&!",
                  N, Standard_Constraint_Error);
            end if;
         end if;
      end if;

      return N;

   end Compile_Time_Constraint_Error;

   -----------------------
   -- Conditional_Delay --
   -----------------------

   procedure Conditional_Delay (New_Ent, Old_Ent : Entity_Id) is
   begin
      if Has_Delayed_Freeze (Old_Ent) and then not Is_Frozen (Old_Ent) then
         Set_Has_Delayed_Freeze (New_Ent);
      end if;
   end Conditional_Delay;

   --------------------
   -- Current_Entity --
   --------------------

   --  The currently visible definition for a given identifier is the
   --  one most chained at the start of the visibility chain, i.e. the
   --  one that is referenced by the Node_Id value of the name of the
   --  given identifier.

   function Current_Entity (N : Node_Id) return Entity_Id is
   begin
      return Get_Name_Entity_Id (Chars (N));
   end Current_Entity;

   -----------------------------
   -- Current_Entity_In_Scope --
   -----------------------------

   function Current_Entity_In_Scope (N : Node_Id) return Entity_Id is
      E  : Entity_Id;
      CS : constant Entity_Id := Current_Scope;

      Transient_Case : constant Boolean := Scope_Is_Transient;

   begin
      E := Get_Name_Entity_Id (Chars (N));

      while Present (E)
        and then Scope (E) /= CS
        and then (not Transient_Case or else Scope (E) /= Scope (CS))
      loop
         E := Homonym (E);
      end loop;

      return E;
   end Current_Entity_In_Scope;

   -------------------
   -- Current_Scope --
   -------------------

   function Current_Scope return Entity_Id is
   begin
      if Scope_Stack.Last = -1 then
         return Standard_Standard;
      else
         declare
            C : constant Entity_Id :=
                  Scope_Stack.Table (Scope_Stack.Last).Entity;
         begin
            if Present (C) then
               return C;
            else
               return Standard_Standard;
            end if;
         end;
      end if;
   end Current_Scope;

   ------------------------
   -- Current_Subprogram --
   ------------------------

   function Current_Subprogram return Entity_Id is
      Scop : constant Entity_Id := Current_Scope;

   begin
      if Ekind (Scop) = E_Function
           or else
         Ekind (Scop) = E_Procedure
           or else
         Ekind (Scop) = E_Generic_Function
           or else
         Ekind (Scop) = E_Generic_Procedure
      then
         return Scop;

      else
         return Enclosing_Subprogram (Scop);
      end if;
   end Current_Subprogram;

   ---------------------
   -- Defining_Entity --
   ---------------------

   function Defining_Entity (N : Node_Id) return Entity_Id is
      K : constant Node_Kind := Nkind (N);

   begin
      case K is
         when
           N_Subprogram_Declaration                 |
           N_Abstract_Subprogram_Declaration        |
           N_Subprogram_Body                        |
           N_Package_Declaration                    |
           N_Subprogram_Renaming_Declaration        |
           N_Subprogram_Body_Stub                   |
           N_Generic_Subprogram_Declaration         |
           N_Generic_Package_Declaration            |
           N_Formal_Subprogram_Declaration
         =>
            return Defining_Entity (Specification (N));

         when
           N_Component_Declaration                  |
           N_Defining_Program_Unit_Name             |
           N_Discriminant_Specification             |
           N_Entry_Body                             |
           N_Entry_Declaration                      |
           N_Entry_Index_Specification              |
           N_Exception_Declaration                  |
           N_Exception_Renaming_Declaration         |
           N_Formal_Object_Declaration              |
           N_Formal_Package_Declaration             |
           N_Formal_Type_Declaration                |
           N_Full_Type_Declaration                  |
           N_Implicit_Label_Declaration             |
           N_Incomplete_Type_Declaration            |
           N_Loop_Parameter_Specification           |
           N_Number_Declaration                     |
           N_Object_Declaration                     |
           N_Object_Renaming_Declaration            |
           N_Package_Body_Stub                      |
           N_Parameter_Specification                |
           N_Private_Extension_Declaration          |
           N_Private_Type_Declaration               |
           N_Protected_Body                         |
           N_Protected_Body_Stub                    |
           N_Protected_Type_Declaration             |
           N_Single_Protected_Declaration           |
           N_Single_Task_Declaration                |
           N_Subtype_Declaration                    |
           N_Task_Body                              |
           N_Task_Body_Stub                         |
           N_Task_Type_Declaration
         =>
            return Defining_Identifier (N);

         when N_Subunit =>
            return Defining_Entity (Proper_Body (N));

         when
           N_Function_Instantiation                 |
           N_Function_Specification                 |
           N_Generic_Function_Renaming_Declaration  |
           N_Generic_Package_Renaming_Declaration   |
           N_Generic_Procedure_Renaming_Declaration |
           N_Package_Body                           |
           N_Package_Instantiation                  |
           N_Package_Renaming_Declaration           |
           N_Package_Specification                  |
           N_Procedure_Instantiation                |
           N_Procedure_Specification
         =>
            declare
               Nam : constant Node_Id := Defining_Unit_Name (N);

            begin
               if Nkind (Nam) in N_Entity then
                  return Nam;
               else
                  return Defining_Identifier (Nam);
               end if;
            end;

         when N_Block_Statement =>
            return Entity (Identifier (N));

         when others =>
            pragma Assert (False);
            raise Program_Error;

      end case;
   end Defining_Entity;

   --------------------------
   -- Denotes_Discriminant --
   --------------------------

   function Denotes_Discriminant (N : Node_Id) return Boolean is
   begin
      return Is_Entity_Name (N)
        and then Present (Entity (N))
        and then Ekind (Entity (N)) = E_Discriminant;
   end Denotes_Discriminant;

   -----------------------------
   -- Depends_On_Discriminant --
   -----------------------------

   function Depends_On_Discriminant (N : Node_Id) return Boolean is
      L : Node_Id;
      H : Node_Id;

   begin
      Get_Index_Bounds (N, L, H);
      return Denotes_Discriminant (L) or else Denotes_Discriminant (H);
   end Depends_On_Discriminant;

   -------------------------
   -- Designate_Same_Unit --
   -------------------------

   function Designate_Same_Unit
     (Name1 : Node_Id;
      Name2 : Node_Id)
      return  Boolean
   is
      K1 : Node_Kind := Nkind (Name1);
      K2 : Node_Kind := Nkind (Name2);

      function Prefix_Node (N : Node_Id) return Node_Id;
      --  Returns the parent unit name node of a defining program unit name
      --  or the prefix if N is a selected component or an expanded name.

      function Select_Node (N : Node_Id) return Node_Id;
      --  Returns the defining identifier node of a defining program unit
      --  name or  the selector node if N is a selected component or an
      --  expanded name.

      function Prefix_Node (N : Node_Id) return Node_Id is
      begin
         if Nkind (N) = N_Defining_Program_Unit_Name then
            return Name (N);

         else
            return Prefix (N);
         end if;
      end Prefix_Node;

      function Select_Node (N : Node_Id) return Node_Id is
      begin
         if Nkind (N) = N_Defining_Program_Unit_Name then
            return Defining_Identifier (N);

         else
            return Selector_Name (N);
         end if;
      end Select_Node;

   --  Start of processing for Designate_Next_Unit

   begin
      if (K1 = N_Identifier or else
          K1 = N_Defining_Identifier)
        and then
         (K2 = N_Identifier or else
          K2 = N_Defining_Identifier)
      then
         return Chars (Name1) = Chars (Name2);

      elsif
         (K1 = N_Expanded_Name      or else
          K1 = N_Selected_Component or else
          K1 = N_Defining_Program_Unit_Name)
        and then
         (K2 = N_Expanded_Name      or else
          K2 = N_Selected_Component or else
          K2 = N_Defining_Program_Unit_Name)
      then
         return
           (Chars (Select_Node (Name1)) = Chars (Select_Node (Name2)))
             and then
               Designate_Same_Unit (Prefix_Node (Name1), Prefix_Node (Name2));

      else
         return False;
      end if;
   end Designate_Same_Unit;

   ----------------------------
   -- Enclosing_Generic_Body --
   ----------------------------

   function Enclosing_Generic_Body
     (E    : Entity_Id)
      return Node_Id
   is
      P    : Node_Id;
      Decl : Node_Id;
      Spec : Node_Id;

   begin
      P := Parent (E);

      while Present (P) loop
         if Nkind (P) = N_Package_Body
           or else Nkind (P) = N_Subprogram_Body
         then
            Spec := Corresponding_Spec (P);

            if Present (Spec) then
               Decl := Unit_Declaration_Node (Spec);

               if Nkind (Decl) = N_Generic_Package_Declaration
                 or else Nkind (Decl) = N_Generic_Subprogram_Declaration
               then
                  return P;
               end if;
            end if;
         end if;

         P := Parent (P);
      end loop;

      return Empty;
   end Enclosing_Generic_Body;

   -------------------------------
   -- Enclosing_Lib_Unit_Entity --
   -------------------------------

   function Enclosing_Lib_Unit_Entity return Entity_Id is
      Unit_Entity : Entity_Id := Current_Scope;

   begin
      --  Look for enclosing library unit entity by following scope links.
      --  Equivalent to, but faster than indexing through the scope stack.

      while (Present (Scope (Unit_Entity))
        and then Scope (Unit_Entity) /= Standard_Standard)
        and not Is_Child_Unit (Unit_Entity)
      loop
         Unit_Entity := Scope (Unit_Entity);
      end loop;

      return Unit_Entity;
   end Enclosing_Lib_Unit_Entity;

   -----------------------------
   -- Enclosing_Lib_Unit_Node --
   -----------------------------

   function Enclosing_Lib_Unit_Node (N : Node_Id) return Node_Id is
      Current_Node : Node_Id := N;

   begin
      while Present (Current_Node)
        and then Nkind (Current_Node) /= N_Compilation_Unit
      loop
         Current_Node := Parent (Current_Node);
      end loop;

      if Nkind (Current_Node) /= N_Compilation_Unit then
         return Empty;
      end if;

      return Current_Node;
   end Enclosing_Lib_Unit_Node;

   --------------------------
   -- Enclosing_Subprogram --
   --------------------------

   function Enclosing_Subprogram (E : Entity_Id) return Entity_Id is
      Dynamic_Scope : constant Entity_Id := Enclosing_Dynamic_Scope (E);

   begin
      if Dynamic_Scope = Standard_Standard then
         return Empty;

      elsif Ekind (Dynamic_Scope) = E_Subprogram_Body then
         return Corresponding_Spec (Parent (Parent (Dynamic_Scope)));

      elsif Ekind (Dynamic_Scope) = E_Block then
         return Enclosing_Subprogram (Dynamic_Scope);

      elsif Ekind (Dynamic_Scope) = E_Task_Type then
         return Task_Body_Procedure (Dynamic_Scope);

      elsif Convention (Dynamic_Scope) = Convention_Protected then
         return Protected_Body_Subprogram (Dynamic_Scope);

      else
         return Dynamic_Scope;
      end if;
   end Enclosing_Subprogram;

   ------------------------
   -- Ensure_Freeze_Node --
   ------------------------

   procedure Ensure_Freeze_Node (E : Entity_Id) is
      FN : Node_Id;

   begin
      if No (Freeze_Node (E)) then
         FN := Make_Freeze_Entity (Sloc (E));
         Set_Has_Delayed_Freeze (E);
         Set_Freeze_Node (E, FN);
         Set_Access_Types_To_Process (FN, No_Elist);
         Set_TSS_Elist (FN, No_Elist);
         Set_Entity (FN, E);
      end if;
   end Ensure_Freeze_Node;

   ----------------
   -- Enter_Name --
   ----------------

   procedure Enter_Name (Def_Id : Node_Id) is
      C : constant Entity_Id := Current_Entity (Def_Id);
      E : constant Entity_Id := Current_Entity_In_Scope (Def_Id);
      S : constant Entity_Id := Current_Scope;

   begin
      Generate_Definition (Def_Id);

      --  Add new name to current scope declarations. Check for duplicate
      --  declaration, which may or may not be a genuine error.

      if Present (E) then

         --  Case of previous entity entered because of a missing declaration
         --  or else a bad subtype indication. Best is to use the new entity,
         --  and make the previous one invisible.

         if Etype (E) = Any_Type then
            Set_Is_Immediately_Visible (E, False);

         --  Case of renaming declaration constructed for package instances.
         --  if there is an explicit declaration with the same identifier,
         --  the renaming is not immediately visible any longer, but remains
         --  visible through selected component notation.

         elsif Nkind (Parent (E)) = N_Package_Renaming_Declaration
           and then not Comes_From_Source (E)
         then
            Set_Is_Immediately_Visible (E, False);

         --  The new entity may be the package renaming, which has the same
         --  same name as a generic formal which has been seen already.

         elsif Nkind (Parent (Def_Id)) = N_Package_Renaming_Declaration
            and then not Comes_From_Source (Def_Id)
         then
            Set_Is_Immediately_Visible (E, False);

         --  For a fat pointer corresponding to a remote access to subprogram,
         --  we use the same identifier as the RAS type, so that the proper
         --  name appears in the stub. This type is only retrieved through
         --  the RAS type and never by visibility, and is not added to the
         --  visibility list (see below).

         elsif Nkind (Parent (Def_Id)) = N_Full_Type_Declaration
           and then Present (Corresponding_Remote_Type (Def_Id))
         then
            null;

         --  A controller component for a type extension overrides the
         --  inherited component.

         elsif Chars (E) = Name_uController then
            null;

         --  Case of an implicit operation or derived literal. The new entity
         --  hides the implicit one,  which is removed from all visibility,
         --  i.e. the entity list of its scope, and homonym chain of its name.

         elsif (Is_Overloadable (E) and then Present (Alias (E)))
           or else Is_Internal (E)
           or else (Ekind (E) = E_Enumeration_Literal
                     and then Is_Derived_Type (Etype (E)))
         then
            declare
               Prev     : Entity_Id;
               Prev_Vis : Entity_Id;

            begin
               --  If E is an implicit declaration, it cannot be the first
               --  entity in the scope.

               Prev := First_Entity (Current_Scope);

               while Next_Entity (Prev) /= E loop
                  Next_Entity (Prev);
               end loop;

               Set_Next_Entity (Prev, Next_Entity (E));

               if No (Next_Entity (Prev)) then
                  Set_Last_Entity (Current_Scope, Prev);
               end if;

               if E = Current_Entity (E) then
                     Prev_Vis := Empty;
               else
                  Prev_Vis := Current_Entity (E);
                  while Homonym (Prev_Vis) /= E loop
                     Prev_Vis := Homonym (Prev_Vis);
                  end loop;
               end if;

               if Present (Prev_Vis)  then

                  --  Skip E in the visibility chain

                  Set_Homonym (Prev_Vis, Homonym (E));

               else
                  Set_Name_Entity_Id (Chars (E), Homonym (E));
               end if;
            end;

         --  This section of code could use a comment ???

         elsif Present (Etype (E))
           and then Is_Concurrent_Type (Etype (E))
           and then E = Def_Id
         then
            return;

         --  In the body or private part of an instance, a type extension
         --  may introduce a component with the same name as that of an
         --  actual. The legality rule is not enforced, but the semantics
         --  of the full type with two components of the same name are not
         --  clear at this point ???

         elsif In_Instance_Not_Visible  then
            null;

         --  When compiling a package body, some child units may have become
         --  visible. They cannot conflict with local entities that hide them.

         elsif Is_Child_Unit (E)
           and then In_Open_Scopes (Scope (E))
           and then not Is_Immediately_Visible (E)
         then
            null;

         --  Case of genuine duplicate declaration

         else
            Error_Msg_Sloc := Sloc (E);

            --  If the previous declaration is an incomplete type declaration
            --  this may be an attempt to complete it with a private type.
            --  The following avoids confusing cascaded errors.

            if Nkind (Parent (E)) = N_Incomplete_Type_Declaration
              and then Nkind (Parent (Def_Id)) = N_Private_Type_Declaration
            then
               Error_Msg_N
                 ("incomplete type cannot be completed" &
                        " with a private declaration",
                    Parent (Def_Id));
               Set_Is_Immediately_Visible (E, False);
               Set_Full_View (E, Def_Id);

            elsif Ekind (E) = E_Discriminant
              and then Present (Scope (Def_Id))
              and then Scope (Def_Id) /= Current_Scope
            then
               --  An inherited component of a record conflicts with
               --  a new discriminant. The discriminant is inserted first
               --  in the scope, but the error should be posted on it, not
               --  on the component.

               Error_Msg_Sloc := Sloc (Def_Id);
               Error_Msg_N ("& conflicts with declaration#", E);
               return;

            else
               Error_Msg_N ("& conflicts with declaration#", Def_Id);

               --  Avoid cascaded messages with duplicate components in
               --  derived types.

               if Ekind (E) = E_Component
                 or else Ekind (E) = E_Discriminant
               then
                  return;
               end if;
            end if;

            if Nkind (Parent (Parent (Def_Id)))
                 = N_Generic_Subprogram_Declaration
              and then Def_Id =
                Defining_Entity (Specification (Parent (Parent (Def_Id))))
            then
               Error_Msg_N ("\generic units cannot be overloaded", Def_Id);
            end if;

            --  If entity is in standard, then we are in trouble, because
            --  it means that we have a library package with a duplicated
            --  name. That's hard to recover from, so abort!

            if S = Standard_Standard then
               raise Unrecoverable_Error;

            --  Otherwise we continue with the declaration. Having two
            --  identical declarations should not cause us too much trouble!

            else
               null;
            end if;
         end if;
      end if;

      --  If we fall through, declaration is OK , or OK enough to continue

      --  If Def_Id is a discriminant or a record component we are in the
      --  midst of inheriting components in a derived record definition.
      --  Preserve their Ekind and Etype.

      if Ekind (Def_Id) = E_Discriminant
        or else Ekind (Def_Id) = E_Component
      then
         null;

      --  If a type is already set, leave it alone (happens whey a type
      --  declaration is reanalyzed following a call to the optimizer)

      elsif Present (Etype (Def_Id)) then
         null;

      --  Otherwise, the kind E_Void insures that premature uses of the entity
      --  will be detected. Any_Type insures that no cascaded errors will occur

      else
         Set_Ekind (Def_Id, E_Void);
         Set_Etype (Def_Id, Any_Type);
      end if;

      --  Inherited discriminants and components in derived record types are
      --  immediately visible. Itypes are not.

      if Ekind (Def_Id) = E_Discriminant
        or else Ekind (Def_Id) = E_Component
        or else (No (Corresponding_Remote_Type (Def_Id))
                 and then not Is_Itype (Def_Id))
      then
         Set_Is_Immediately_Visible (Def_Id);
         Set_Current_Entity         (Def_Id);
      end if;

      Set_Homonym       (Def_Id, C);
      Append_Entity     (Def_Id, S);
      Set_Public_Status (Def_Id);

      --  Warn if new entity hides an old one

      if Warn_On_Hiding
        and then Length_Of_Name (Chars (C)) /= 1
        and then Present (C)
        and then Comes_From_Source (C)
        and then Comes_From_Source (Def_Id)
        and then In_Extended_Main_Source_Unit (Def_Id)
      then
         Error_Msg_Sloc := Sloc (C);
         Error_Msg_N ("declaration hides &#?", Def_Id);
      end if;

   end Enter_Name;

   -------------------------------------
   -- Find_Corresponding_Discriminant --
   -------------------------------------

   function Find_Corresponding_Discriminant
     (Id   : Node_Id;
      Typ  : Entity_Id)
      return Entity_Id
   is
      Par_Disc : Entity_Id;
      Old_Disc : Entity_Id;
      New_Disc : Entity_Id;

   begin
      Par_Disc := Original_Record_Component (Original_Discriminant (Id));
      Old_Disc := First_Discriminant (Scope (Par_Disc));

      if Is_Class_Wide_Type (Typ) then
         New_Disc := First_Discriminant (Root_Type (Typ));
      else
         New_Disc := First_Discriminant (Typ);
      end if;


      while Present (Old_Disc) and then Present (New_Disc) loop
         if Old_Disc = Par_Disc  then
            return New_Disc;
         else
            Next_Discriminant (Old_Disc);
            Next_Discriminant (New_Disc);
         end if;
      end loop;

      --  Should always find it

      pragma Assert (False);
      raise Program_Error;
   end Find_Corresponding_Discriminant;

   ------------------
   -- First_Actual --
   ------------------

   function First_Actual (Node : Node_Id) return Node_Id is
      N : Node_Id;

   begin
      if No (Parameter_Associations (Node)) then
         return Empty;
      end if;

      N := First (Parameter_Associations (Node));

      if Nkind (N) = N_Parameter_Association then
         return First_Named_Actual (Node);
      else
         return N;
      end if;
   end First_Actual;

   -------------------------
   -- Full_Qualified_Name --
   -------------------------

   function Full_Qualified_Name (E : Entity_Id) return String_Id is

      Res : String_Id;

      function Internal_Full_Qualified_Name (E : Entity_Id) return String_Id;
      --  Compute recursively the qualified name without NUL at the end.

      function Internal_Full_Qualified_Name (E : Entity_Id) return String_Id is
         Ent         : Entity_Id := E;
         Parent_Name : String_Id := No_String;

      begin
         --  Deals properly with child units

         if Nkind (Ent) = N_Defining_Program_Unit_Name then
            Ent := Defining_Identifier (Ent);
         end if;

         --  Compute recursively the qualification. Only "Standard" has no
         --  scope.

         if Present (Scope (Scope (Ent))) then
            Parent_Name := Internal_Full_Qualified_Name (Scope (Ent));
         end if;

         --  Every entity should have a name except some expanded blocks
         --  don't bother about those.

         if Chars (Ent) = No_Name then
            return Parent_Name;
         end if;

         --  Add a period between Name and qualification

         if Parent_Name /= No_String then
            Start_String (Parent_Name);
            Store_String_Char (Get_Char_Code ('.'));

         else
            Start_String;
         end if;

         --  Generates the entity name in upper case

         Get_Name_String (Chars (Ent));
         Set_All_Upper_Case;
         Store_String_Chars (Name_Buffer (1 .. Name_Len));
         return End_String;
      end Internal_Full_Qualified_Name;

   begin
      Res := Internal_Full_Qualified_Name (E);
      Store_String_Char (Get_Char_Code (ASCII.nul));
      return End_String;
   end Full_Qualified_Name;

   -----------------------
   -- Gather_Components --
   -----------------------

   procedure Gather_Components
     (Typ           : Entity_Id;
      Comp_List     : Node_Id;
      Governed_By   : List_Id;
      Into          : Elist_Id;
      Report_Errors : out Boolean)
   is
      Assoc           : Node_Id;
      Variant         : Node_Id;
      Discrete_Choice : Node_Id;
      Comp_Item       : Node_Id;

      Discrim       : Entity_Id;
      Discrim_Name  : Node_Id;
      Discrim_Value : Node_Id;

   begin
      Report_Errors := False;

      if No (Comp_List) or else Null_Present (Comp_List) then
         return;

      elsif Present (Component_Items (Comp_List)) then
         Comp_Item := First (Component_Items (Comp_List));

      else
         Comp_Item := Empty;
      end if;

      while Present (Comp_Item) loop

         --  Skip the tag of a tagged record, as well as all items
         --  that are not user components (anonymous types, rep clauses,
         --  Parent field, controller field).

         if Nkind (Comp_Item) = N_Component_Declaration
           and then Chars (Defining_Identifier (Comp_Item)) /= Name_uTag
           and then Chars (Defining_Identifier (Comp_Item)) /= Name_uParent
           and then Chars (Defining_Identifier (Comp_Item)) /= Name_uController
         then
            Append_Elmt (Defining_Identifier (Comp_Item), Into);
         end if;

         Next (Comp_Item);
      end loop;

      if No (Variant_Part (Comp_List)) then
         return;
      else
         Discrim_Name := Name (Variant_Part (Comp_List));
         Variant := First_Non_Pragma (Variants (Variant_Part (Comp_List)));
      end if;

      --  Look for the discriminant that governs this variant part.
      --  The discriminant *must* be in the Governed_By List


      Assoc := First (Governed_By);
      Find_Constraint : loop
         Discrim := First (Choices (Assoc));
         exit when Chars (Discrim_Name) = Chars (Discrim)
           or else (Present (Corresponding_Discriminant (Entity (Discrim)))
                      and then
                    Chars (Corresponding_Discriminant (Entity (Discrim)))
                         = Chars  (Discrim_Name))
           or else Chars (Original_Record_Component (Entity (Discrim)))
                         = Chars (Discrim_Name);

         if No (Next (Assoc)) then
            if not Is_Constrained (Typ)
              and then Is_Derived_Type (Typ)
              and then Present (Girder_Constraint (Typ))
            then

               --  If the type is a tagged type with inherited discriminants,
               --  use the girder constraint on the parent in order to find
               --  the values of discriminants that are otherwise hidden by an
               --  explicit constraint. Renamed discriminants are handled in
               --  the code above.

               declare
                  D : Entity_Id;
                  C : Elmt_Id;

               begin
                  D := First_Discriminant (Etype (Typ));
                  C := First_Elmt (Girder_Constraint (Typ));

                  while Present (D)
                    and then Present (C)
                  loop
                     if Chars (Discrim_Name) = Chars (D) then
                        Assoc :=
                          Make_Component_Association (Sloc (Typ),
                            New_List
                              (New_Occurrence_Of (D, Sloc (Typ))),
                            Duplicate_Subexpr (Node (C)));
                        exit Find_Constraint;
                     end if;

                     D := Next_Discriminant (D);
                     Next_Elmt (C);
                  end loop;
               end;
            end if;
         end if;

         if No (Next (Assoc)) then
            Error_Msg_NE (" missing value for discriminant&",
              First (Governed_By), Discrim_Name);
            Report_Errors := True;
            return;
         end if;

         Next (Assoc);
      end loop Find_Constraint;

      Discrim_Value := Expression (Assoc);

      if not Is_OK_Static_Expression (Discrim_Value) then
         Error_Msg_NE
           ("value for discriminant & must be static", Discrim_Value, Discrim);
         Report_Errors := True;
         return;
      end if;

      Search_For_Discriminant_Value : declare
         Low  : Node_Id;
         High : Node_Id;

         UI_High          : Uint;
         UI_Low           : Uint;
         UI_Discrim_Value : constant Uint := Expr_Value (Discrim_Value);

      begin
         Find_Discrete_Value : while Present (Variant) loop
            Discrete_Choice := First (Discrete_Choices (Variant));
            while Present (Discrete_Choice) loop

               exit Find_Discrete_Value when
                 Nkind (Discrete_Choice) = N_Others_Choice;

               Get_Index_Bounds (Discrete_Choice, Low, High);

               UI_Low  := Expr_Value (Low);
               UI_High := Expr_Value (High);

               exit Find_Discrete_Value when
                 UI_Low <= UI_Discrim_Value
                   and then
                 UI_High >= UI_Discrim_Value;

               Next (Discrete_Choice);
            end loop;

            Next_Non_Pragma (Variant);
         end loop Find_Discrete_Value;
      end Search_For_Discriminant_Value;

      if No (Variant) then
         Error_Msg_NE
           ("value of discriminant & is out of range", Discrim_Value, Discrim);
         Report_Errors := True;
         return;
      end  if;

      --  If we have found the corresponding choice, recursively add its
      --  components to the Into list.

      Gather_Components (Empty,
        Component_List (Variant), Governed_By, Into, Report_Errors);
   end Gather_Components;

   ------------------------
   -- Get_Actual_Subtype --
   ------------------------

   function Get_Actual_Subtype (N : Node_Id) return Entity_Id is
      Typ  : constant Entity_Id := Etype (N);
      Utyp : Entity_Id := Underlying_Type (Typ);
      Decl : Node_Id;
      Atyp : Entity_Id;

   begin
      if not Present (Utyp) then
         Utyp := Typ;
      end if;

      --  If what we have is an identifier that references a subprogram
      --  formal, or a variable or constant object, then we get the actual
      --  subtype from the referenced entity if one has been built.

      if Nkind (N) = N_Identifier
        and then
          (Is_Formal (Entity (N))
            or else Ekind (Entity (N)) = E_Constant
            or else Ekind (Entity (N)) = E_Variable)
        and then Present (Actual_Subtype (Entity (N)))
      then
         return Actual_Subtype (Entity (N));

      --  Actual subtype of unchecked union is always itself. We never need
      --  the "real" actual subtype. If we did, we couldn't get it anyway
      --  because the discriminant is not available. The restrictions on
      --  Unchecked_Union are designed to make sure that this is OK.

      elsif Is_Unchecked_Union (Utyp) then
         return Typ;

      --  Here for the unconstrained case, we must find actual subtype
      --  No actual subtype is available, so we must build it on the fly.

      --  Checking the type, not the underlying type, for constrainedness
      --  seems to be necessary. Maybe all the tests should be on the type???

      elsif (not Is_Constrained (Typ))
           and then (Is_Array_Type (Utyp)
                      or else (Is_Record_Type (Utyp)
                                and then Has_Discriminants (Utyp)))
           and then not Has_Unknown_Discriminants (Utyp)
           and then not (Ekind (Utyp) = E_String_Literal_Subtype)
      then
         --  Nothing to do if in default expression

         if In_Default_Expression then
            return Typ;

         --  Else build the actual subtype

         else
            Decl := Build_Actual_Subtype (Typ, N);
            Atyp := Defining_Identifier (Decl);

            --  If Build_Actual_Subtype generated a new declaration then use it

            if Atyp /= Typ then

               --  The actual subtype is an Itype, so analyze the declaration,
               --  but do not attach it to the tree, to get the type defined.

               Set_Parent (Decl, N);
               Set_Is_Itype (Atyp);
               Analyze (Decl, Suppress => All_Checks);
               Set_Associated_Node_For_Itype (Atyp, N);
               Set_Has_Delayed_Freeze (Atyp, False);

               --  We need to freeze the actual subtype immediately. This is
               --  needed, because otherwise this Itype will not get frozen
               --  at all, and it is always safe to freeze on creation because
               --  any associated types must be frozen at this point.

               Freeze_Itype (Atyp, N);
               return Atyp;

            --  Otherwise we did not build a declaration, so return original

            else
               return Typ;
            end if;
         end if;

      --  For all remaining cases, the actual subtype is the same as
      --  the nominal type.

      else
         return Typ;
      end if;
   end Get_Actual_Subtype;

   -------------------------------------
   -- Get_Actual_Subtype_If_Available --
   -------------------------------------

   function Get_Actual_Subtype_If_Available (N : Node_Id) return Entity_Id is
      Typ  : constant Entity_Id := Etype (N);

   begin
      --  If what we have is an identifier that references a subprogram
      --  formal, or a variable or constant object, then we get the actual
      --  subtype from the referenced entity if one has been built.

      if Nkind (N) = N_Identifier
        and then
          (Is_Formal (Entity (N))
            or else Ekind (Entity (N)) = E_Constant
            or else Ekind (Entity (N)) = E_Variable)
        and then Present (Actual_Subtype (Entity (N)))
      then
         return Actual_Subtype (Entity (N));

      --  Otherwise the Etype of N is returned unchanged

      else
         return Typ;
      end if;
   end Get_Actual_Subtype_If_Available;

   --------------------
   -- Has_Infinities --
   --------------------

   function Has_Infinities (E : Entity_Id) return Boolean is
   begin
      return
        Is_Floating_Point_Type (E)
          and then Nkind (Scalar_Range (E)) = N_Range
          and then Includes_Infinities (Scalar_Range (E));
   end Has_Infinities;

   --------------------------
   -- Unit_Declaration_Node --
   --------------------------

   function Unit_Declaration_Node (Unit_Id : Entity_Id) return Node_Id is
      N : Node_Id := Parent (Unit_Id);

   begin
      --  Predefined operators do not have a full function declaration.

      if Ekind (Unit_Id) = E_Operator then
         return N;
      end if;

      while Nkind (N) /= N_Abstract_Subprogram_Declaration
        and then Nkind (N) /= N_Formal_Package_Declaration
        and then Nkind (N) /= N_Formal_Subprogram_Declaration
        and then Nkind (N) /= N_Function_Instantiation
        and then Nkind (N) /= N_Generic_Package_Declaration
        and then Nkind (N) /= N_Generic_Subprogram_Declaration
        and then Nkind (N) /= N_Package_Declaration
        and then Nkind (N) /= N_Package_Body
        and then Nkind (N) /= N_Package_Instantiation
        and then Nkind (N) /= N_Package_Renaming_Declaration
        and then Nkind (N) /= N_Procedure_Instantiation
        and then Nkind (N) /= N_Subprogram_Declaration
        and then Nkind (N) /= N_Subprogram_Body
        and then Nkind (N) /= N_Subprogram_Body_Stub
        and then Nkind (N) /= N_Subprogram_Renaming_Declaration
        and then Nkind (N) /= N_Task_Body
        and then Nkind (N) /= N_Task_Type_Declaration
        and then Nkind (N) not in N_Generic_Renaming_Declaration
      loop
         N := Parent (N);
         pragma Assert (Present (N));
      end loop;

      return N;
   end Unit_Declaration_Node;

   -------------------------------
   -- Get_Default_External_Name --
   -------------------------------

   function Get_Default_External_Name (E : Node_Or_Entity_Id) return Node_Id is
   begin
      Get_Decoded_Name_String (Chars (E));

      if Opt.External_Name_Imp_Casing = Uppercase then
         Set_Casing (All_Upper_Case);
      else
         Set_Casing (All_Lower_Case);
      end if;

      return
        Make_String_Literal (Sloc (E),
          Strval => String_From_Name_Buffer);

   end Get_Default_External_Name;

   ----------------------
   -- Get_Index_Bounds --
   ----------------------

   procedure Get_Index_Bounds (N : Node_Id; L, H : out Node_Id) is
      Kind : constant Node_Kind := Nkind (N);

   begin
      if Kind = N_Range then
         L := Low_Bound (N);
         H := High_Bound (N);

      elsif Kind = N_Subtype_Indication then
         L := Low_Bound  (Range_Expression (Constraint (N)));
         H := High_Bound (Range_Expression (Constraint (N)));

      elsif Is_Entity_Name (N) and then Is_Type (Entity (N)) then
         if Nkind (Scalar_Range (Entity (N))) = N_Subtype_Indication then
            Get_Index_Bounds (Scalar_Range (Entity (N)), L, H);
         else
            L := Low_Bound  (Scalar_Range (Entity (N)));
            H := High_Bound (Scalar_Range (Entity (N)));
         end if;

      else
         --  N is an expression, indicating a range with one value.

         L := N;
         H := N;
      end if;

   end Get_Index_Bounds;

   ------------------------
   -- Get_Name_Entity_Id --
   ------------------------

   function Get_Name_Entity_Id (Id : Name_Id) return Entity_Id is
   begin
      return Entity_Id (Get_Name_Table_Info (Id));
   end Get_Name_Entity_Id;

   ---------------------------
   -- Get_Referenced_Object --
   ---------------------------

   function Get_Referenced_Object (N : Node_Id) return Node_Id is
      R   : Node_Id := N;

   begin
      while Is_Entity_Name (R)
        and then Present (Renamed_Object (Entity (R)))
      loop
         R := Renamed_Object (Entity (R));
      end loop;

      return R;
   end Get_Referenced_Object;

   -------------------------
   -- Get_Subprogram_Body --
   -------------------------

   function Get_Subprogram_Body (E : Entity_Id) return Node_Id is
      Decl : Node_Id;

   begin
      Decl := Unit_Declaration_Node (E);

      if Nkind (Decl) = N_Subprogram_Body then
         return Decl;

      else           --  Nkind (Decl) = N_Subprogram_Declaration

         if Present (Corresponding_Body (Decl)) then
            return Unit_Declaration_Node (Corresponding_Body (Decl));

         else        --  imported subprogram.
            return Empty;
         end if;
      end if;
   end Get_Subprogram_Body;

   ---------------------------
   -- Has_Private_Component --
   ---------------------------

   function Has_Private_Component (Type_Id : Entity_Id) return Boolean is
      Btype     : Entity_Id := Base_Type (Type_Id);
      Component : Entity_Id;

   begin
      if Error_Posted (Type_Id)
        or else Error_Posted (Btype)
      then
         return False;
      end if;

      if Is_Class_Wide_Type (Btype) then
         Btype := Root_Type (Btype);
      end if;

      if Is_Private_Type (Btype) then
         declare
            UT : constant Entity_Id := Underlying_Type (Btype);
         begin
            if No (UT) then

               if No (Full_View (Btype)) then
                  return not Is_Generic_Type (Btype)
                    and then not Is_Generic_Type (Root_Type (Btype));

               else
                  return not Is_Generic_Type (Root_Type (Full_View (Btype)));
               end if;

            else
               return not Is_Frozen (UT) and then Has_Private_Component (UT);
            end if;
         end;
      elsif Is_Array_Type (Btype) then
         return Has_Private_Component (Component_Type (Btype));

      elsif Is_Record_Type (Btype) then

         Component := First_Component (Btype);
         while Present (Component) loop

            if Has_Private_Component (Etype (Component)) then
               return True;
            end if;

            Next_Component (Component);
         end loop;

         return False;

      elsif Is_Protected_Type (Btype)
        and then Present (Corresponding_Record_Type (Btype))
      then
         return Has_Private_Component (Corresponding_Record_Type (Btype));

      else
         return False;
      end if;
   end Has_Private_Component;

   --------------------------
   -- Has_Tagged_Component --
   --------------------------

   function Has_Tagged_Component (Typ : Entity_Id) return Boolean is
      Comp : Entity_Id;

   begin
      if Is_Private_Type (Typ)
        and then Present (Underlying_Type (Typ))
      then
         return Has_Tagged_Component (Underlying_Type (Typ));

      elsif Is_Array_Type (Typ) then
         return Has_Tagged_Component (Component_Type (Typ));

      elsif Is_Tagged_Type (Typ) then
         return True;

      elsif Is_Record_Type (Typ) then
         Comp := First_Component (Typ);

         while Present (Comp) loop
            if Has_Tagged_Component (Etype (Comp)) then
               return True;
            end if;

            Comp := Next_Component (Typ);
         end loop;

         return False;

      else
         return False;
      end if;
   end Has_Tagged_Component;

   -----------------
   -- In_Instance --
   -----------------

   function In_Instance return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if (Ekind (S) = E_Function
              or else Ekind (S) = E_Package
              or else Ekind (S) = E_Procedure)
           and then Is_Generic_Instance (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance;

   ----------------------
   -- In_Instance_Body --
   ----------------------

   function In_Instance_Body return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if (Ekind (S) = E_Function
              or else Ekind (S) = E_Procedure)
           and then Is_Generic_Instance (S)
         then
            return True;

         elsif Ekind (S) = E_Package
           and then In_Package_Body (S)
           and then Is_Generic_Instance (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance_Body;

   -----------------------------
   -- In_Instance_Not_Visible --
   -----------------------------

   function In_Instance_Not_Visible return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if (Ekind (S) = E_Function
              or else Ekind (S) = E_Procedure)
           and then Is_Generic_Instance (S)
         then
            return True;

         elsif Ekind (S) = E_Package
           and then (In_Package_Body (S) or else In_Private_Part (S))
           and then Is_Generic_Instance (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance_Not_Visible;

   ------------------------------
   -- In_Instance_Visible_Part --
   ------------------------------

   function In_Instance_Visible_Part return Boolean is
      S : Entity_Id := Current_Scope;

   begin
      while Present (S)
        and then S /= Standard_Standard
      loop
         if Ekind (S) = E_Package
           and then Is_Generic_Instance (S)
           and then not In_Package_Body (S)
           and then not In_Private_Part (S)
         then
            return True;
         end if;

         S := Scope (S);
      end loop;

      return False;
   end In_Instance_Visible_Part;

   --------------------------------------
   -- In_Subprogram_Or_Concurrent_Unit --
   --------------------------------------

   function In_Subprogram_Or_Concurrent_Unit return Boolean is
      E : Entity_Id;
      K : Entity_Kind;

   begin
      --  Use scope chain to check successively outer scopes

      E := Current_Scope;
      loop
         K := Ekind (E);

         if K in Subprogram_Kind
           or else K in Concurrent_Kind
           or else K = E_Generic_Procedure
           or else K = E_Generic_Function
         then
            return True;

         elsif E = Standard_Standard then
            return False;
         end if;

         E := Scope (E);
      end loop;

   end In_Subprogram_Or_Concurrent_Unit;

   ---------------------
   -- In_Visible_Part --
   ---------------------

   function In_Visible_Part (Scope_Id : Entity_Id) return Boolean is
   begin
      return
        Is_Package (Scope_Id)
          and then In_Open_Scopes (Scope_Id)
          and then not In_Package_Body (Scope_Id)
          and then not In_Private_Part (Scope_Id);
   end In_Visible_Part;

   -------------------------
   -- Is_Actual_Parameter --
   -------------------------

   function Is_Actual_Parameter (N : Node_Id) return Boolean is
      PK : constant Node_Kind := Nkind (Parent (N));

   begin
      case PK is
         when N_Parameter_Association =>
            return N = Explicit_Actual_Parameter (Parent (N));

         when N_Function_Call | N_Procedure_Call_Statement =>
            return Is_List_Member (N)
              and then
                List_Containing (N) = Parameter_Associations (Parent (N));

         when others =>
            return False;
      end case;
   end Is_Actual_Parameter;

   ---------------------
   -- Is_Aliased_View --
   ---------------------

   function Is_Aliased_View (Obj : Node_Id) return Boolean is
      E : Entity_Id;

   begin
      if Is_Entity_Name (Obj) then

         --  Shouldn't we check that we really have an object here?
         --  If we do, then a-caldel.adb blows up mysteriously ???

         E := Entity (Obj);

         return Is_Aliased (E)
           or else (Present (Renamed_Object (E))
                     and then Is_Aliased_View (Renamed_Object (E)))

           or else ((Is_Formal (E)
                      or else Ekind (E) = E_Generic_In_Out_Parameter
                      or else Ekind (E) = E_Generic_In_Parameter)
                    and then Is_Tagged_Type (Etype (E)))

           or else ((Ekind (E) = E_Task_Type or else
                     Ekind (E) = E_Protected_Type)
                    and then In_Open_Scopes (E))

            --  Current instance of type

           or else (Is_Type (E) and then E = Current_Scope)
           or else (Is_Incomplete_Or_Private_Type (E)
                     and then Full_View (E) = Current_Scope);

      elsif Nkind (Obj) = N_Selected_Component then
         return Is_Aliased (Entity (Selector_Name (Obj)));

      elsif Nkind (Obj) = N_Indexed_Component then
         return Has_Aliased_Components (Etype (Prefix (Obj)))
           or else
             (Is_Access_Type (Etype (Prefix (Obj)))
               and then
              Has_Aliased_Components
                (Designated_Type (Etype (Prefix (Obj)))));

      elsif Nkind (Obj) = N_Unchecked_Type_Conversion
        or else Nkind (Obj) = N_Type_Conversion
      then
         return Is_Tagged_Type (Etype (Obj))
           or else Is_Aliased_View (Expression (Obj));

      elsif Nkind (Obj) = N_Explicit_Dereference then
         return True;

      else
         return False;
      end if;
   end Is_Aliased_View;

   ----------------------
   -- Is_Atomic_Object --
   ----------------------

   function Is_Atomic_Object (N : Node_Id) return Boolean is

      function Object_Has_Atomic_Components (N : Node_Id) return Boolean;
      --  Determines if given object has atomic components

      function Is_Atomic_Prefix (N : Node_Id) return Boolean;
      --  If prefix is an implicit dereference, examine designated type.

      function Is_Atomic_Prefix (N : Node_Id) return Boolean is
      begin
         if Is_Access_Type (Etype (N)) then
            return
              Has_Atomic_Components (Designated_Type (Etype (N)));
         else
            return Object_Has_Atomic_Components (N);
         end if;
      end Is_Atomic_Prefix;

      function Object_Has_Atomic_Components (N : Node_Id) return Boolean is
      begin
         if Has_Atomic_Components (Etype (N))
           or else Is_Atomic (Etype (N))
         then
            return True;

         elsif Is_Entity_Name (N)
           and then (Has_Atomic_Components (Entity (N))
                      or else Is_Atomic (Entity (N)))
         then
            return True;

         elsif Nkind (N) = N_Indexed_Component
           or else Nkind (N) = N_Selected_Component
         then
            return Is_Atomic_Prefix (Prefix (N));

         else
            return False;
         end if;
      end Object_Has_Atomic_Components;

   --  Start of processing for Is_Atomic_Object

   begin
      if Is_Atomic (Etype (N))
        or else (Is_Entity_Name (N) and then Is_Atomic (Entity (N)))
      then
         return True;

      elsif Nkind (N) = N_Indexed_Component
        or else Nkind (N) = N_Selected_Component
      then
         return Is_Atomic_Prefix (Prefix (N));

      else
         return False;
      end if;
   end Is_Atomic_Object;

   ----------------------------------------------
   -- Is_Dependent_Component_Of_Mutable_Object --
   ----------------------------------------------

   function Is_Dependent_Component_Of_Mutable_Object
     (Object : Node_Id)
      return   Boolean
   is
      P           : Node_Id;
      Prefix_Type : Entity_Id;
      P_Aliased   : Boolean := False;
      Comp        : Entity_Id;

      function Is_Declared_Within_Variant (Comp : Entity_Id) return Boolean;
      --  Returns True if and only if Comp is declared within a variant part.

      function Has_Dependent_Constraint (Comp : Entity_Id) return Boolean;
      --  Returns True if and only if Comp has a constrained subtype
      --  that depends on a discriminant.

      function Is_Declared_Within_Variant (Comp : Entity_Id) return Boolean is
         Comp_Decl : constant Node_Id   := Parent (Comp);
         Comp_List : constant Node_Id   := Parent (Comp_Decl);

      begin
         return Nkind (Parent (Comp_List)) = N_Variant;
      end Is_Declared_Within_Variant;

      function Has_Dependent_Constraint (Comp : Entity_Id) return Boolean is
         Comp_Decl  : constant Node_Id   := Parent (Comp);
         Subt_Indic : constant Node_Id   := Subtype_Indication (Comp_Decl);
         Constr     : Node_Id;
         Assn       : Node_Id;

      begin
         if Nkind (Subt_Indic) = N_Subtype_Indication then
            Constr := Constraint (Subt_Indic);

            if Nkind (Constr) = N_Index_Or_Discriminant_Constraint then
               Assn := First (Constraints (Constr));
               while Present (Assn) loop
                  case Nkind (Assn) is
                     when N_Subtype_Indication |
                          N_Range              |
                          N_Identifier
                     =>
                        if Depends_On_Discriminant (Assn) then
                           return True;
                        end if;

                     when N_Discriminant_Association =>
                        if Depends_On_Discriminant (Expression (Assn)) then
                           return True;
                        end if;

                     when others =>
                        null;

                  end case;

                  Next (Assn);
               end loop;
            end if;
         end if;

         return False;
      end Has_Dependent_Constraint;

   --  Start of processing for Is_Dependent_Component_Of_Mutable_Object

   begin
      if Is_Variable (Object) then

         if Nkind (Object) = N_Selected_Component then
            P := Prefix (Object);
            Prefix_Type := Etype (P);

            if Is_Entity_Name (P) then

               if Ekind (Entity (P)) = E_Generic_In_Out_Parameter then
                  Prefix_Type := Base_Type (Prefix_Type);
               end if;

               if Is_Aliased (Entity (P)) then
                  P_Aliased := True;
               end if;

            else
               --  Check for prefix being an aliased component ???
               null;
            end if;

            if Is_Access_Type (Prefix_Type)
              or else Nkind (P) = N_Explicit_Dereference
            then
               return False;
            end if;

            Comp :=
              Original_Record_Component (Entity (Selector_Name (Object)));

            if not Is_Constrained (Prefix_Type)
              and then not Is_Indefinite_Subtype (Prefix_Type)
              and then (Is_Declared_Within_Variant (Comp)
                          or else Has_Dependent_Constraint (Comp))
              and then not P_Aliased
            then
               return True;

            else
               return
                 Is_Dependent_Component_Of_Mutable_Object (Prefix (Object));

            end if;

         elsif Nkind (Object) = N_Indexed_Component
           or else Nkind (Object) = N_Slice
         then
            return Is_Dependent_Component_Of_Mutable_Object (Prefix (Object));
         end if;
      end if;

      return False;
   end Is_Dependent_Component_Of_Mutable_Object;

   --------------------
   -- Is_Entity_Name --
   --------------------

   function Is_Entity_Name (N : Node_Id) return Boolean is
      Kind : constant Node_Kind := Nkind (N);

   begin
      --  Identifiers, operator symbols, expanded names are entity names

      return Kind = N_Identifier
        or else Kind = N_Operator_Symbol
        or else Kind = N_Expanded_Name

      --  Attribute references are entity names if they refer to an entity.
      --  Note that we don't do this by testing for the presence of the
      --  Entity field in the N_Attribute_Reference node, since it may not
      --  have been set yet.

        or else (Kind = N_Attribute_Reference
                  and then Is_Entity_Attribute_Name (Attribute_Name (N)));
   end Is_Entity_Name;

   --------------
   -- Is_False --
   --------------

   function Is_False (U : Uint) return Boolean is
   begin
      return (U = 0);
   end Is_False;

   ---------------------------
   -- Is_Fixed_Model_Number --
   ---------------------------

   function Is_Fixed_Model_Number (U : Ureal; T : Entity_Id) return Boolean is
      S : constant Ureal := Small_Value (T);
      M : Urealp.Save_Mark;
      R : Boolean;

   begin
      M := Urealp.Mark;
      R := (U = UR_Trunc (U / S) * S);
      Urealp.Release (M);
      return R;
   end Is_Fixed_Model_Number;

   -------------------------------
   -- Is_Fully_Initialized_Type --
   -------------------------------

   function Is_Fully_Initialized_Type (Typ : Entity_Id) return Boolean is
   begin
      if Is_Scalar_Type (Typ) then
         return False;

      elsif Is_Access_Type (Typ) then
         return True;

      elsif Is_Array_Type (Typ) then
         if Is_Fully_Initialized_Type (Component_Type (Typ)) then
            return True;
         end if;

         --  An interesting case, if we have a constrained type one of whose
         --  bounds is known to be null, then there are no elements to be
         --  initialized, so all the elements are initialized!

         if Is_Constrained (Typ) then
            declare
               Indx     : Node_Id;
               Indx_Typ : Entity_Id;
               Lbd, Hbd : Node_Id;

            begin
               Indx := First_Index (Typ);
               while Present (Indx) loop

                  if Etype (Indx) = Any_Type then
                     return False;

                  --  If index is a range, use directly.

                  elsif Nkind (Indx) = N_Range then
                     Lbd := Low_Bound  (Indx);
                     Hbd := High_Bound (Indx);

                  else
                     Indx_Typ := Etype (Indx);

                     if Is_Private_Type (Indx_Typ)  then
                        Indx_Typ := Full_View (Indx_Typ);
                     end if;

                     if No (Indx_Typ) then
                        return False;
                     else
                        Lbd := Type_Low_Bound  (Indx_Typ);
                        Hbd := Type_High_Bound (Indx_Typ);
                     end if;
                  end if;

                  if Compile_Time_Known_Value (Lbd)
                    and then Compile_Time_Known_Value (Hbd)
                  then
                     if Expr_Value (Hbd) < Expr_Value (Lbd) then
                        return True;
                     end if;
                  end if;

                  Next_Index (Indx);
               end loop;
            end;
         end if;

         return False;

      elsif Is_Record_Type (Typ) then
         declare
            Ent : Entity_Id;

         begin
            Ent := First_Entity (Typ);

            while Present (Ent) loop
               if Ekind (Ent) = E_Component
                 and then (No (Parent (Ent))
                             or else No (Expression (Parent (Ent))))
                 and then not Is_Fully_Initialized_Type (Etype (Ent))
               then
                  return False;
               end if;

               Next_Entity (Ent);
            end loop;
         end;

         return True;

      elsif Is_Concurrent_Type (Typ) then
         return True;

      elsif Is_Private_Type (Typ) then
         declare
            U : constant Entity_Id := Underlying_Type (Typ);

         begin
            if No (U) then
               return False;
            else
               return Is_Fully_Initialized_Type (U);
            end if;
         end;

      else
         return False;
      end if;
   end Is_Fully_Initialized_Type;

   ----------------------------
   -- Is_Inherited_Operation --
   ----------------------------

   function Is_Inherited_Operation (E : Entity_Id) return Boolean is
      Kind : constant Node_Kind := Nkind (Parent (E));

   begin
      pragma Assert (Is_Overloadable (E));
      return Kind = N_Full_Type_Declaration
        or else Kind = N_Private_Extension_Declaration
        or else Kind = N_Subtype_Declaration;
   end Is_Inherited_Operation;

   -----------------------------
   -- Is_Library_Level_Entity --
   -----------------------------

   function Is_Library_Level_Entity (E : Entity_Id) return Boolean is
   begin
      return Enclosing_Dynamic_Scope (E) = Standard_Standard;
   end Is_Library_Level_Entity;

   ---------------------------------
   -- Is_Local_Variable_Reference --
   ---------------------------------

   function Is_Local_Variable_Reference (Expr : Node_Id) return Boolean is
   begin
      if not Is_Entity_Name (Expr) then
         return False;

      else
         declare
            Ent : constant Entity_Id := Entity (Expr);
            Sub : constant Entity_Id := Enclosing_Subprogram (Ent);

         begin
            if Ekind (Ent) /= E_Variable
                 and then
               Ekind (Ent) /= E_In_Out_Parameter
            then
               return False;

            else
               return Present (Sub) and then Sub = Current_Subprogram;
            end if;
         end;
      end if;
   end Is_Local_Variable_Reference;

   -------------------------
   -- Is_Object_Reference --
   -------------------------

   function Is_Object_Reference (N : Node_Id) return Boolean is
   begin
      if Is_Entity_Name (N) then
         return Is_Object (Entity (N));

      else
         --  Note: one would think that a function call should be a case
         --  that returns True here, but if this is done, then we get a
         --  compilation error in a-strunb, which has not been figured
         --  out, needs investigation ???

         case Nkind (N) is
            when N_Indexed_Component | N_Slice =>
               return True;

            when N_Selected_Component =>
               return Is_Object_Reference (Selector_Name (N));

            when N_Explicit_Dereference =>
               return True;

            --  An unchecked type conversion is considered to be an object if
            --  the operand is an object (this construction arises only as a
            --  result of expansion activities).

            when N_Unchecked_Type_Conversion =>
               return True;

            when others =>
               return False;
         end case;
      end if;
   end Is_Object_Reference;

   -----------------------------------
   -- Is_OK_Variable_For_Out_Formal --
   -----------------------------------

   function Is_OK_Variable_For_Out_Formal (AV : Node_Id) return Boolean is
   begin
      Note_Possible_Modification (AV);

      --  We must reject parenthesized variable names. The check for
      --  Comes_From_Source is present because there are currently
      --  cases where the compiler violates this rule (e.g. passing
      --  a task object to its controlled Initialize routine).

      if Paren_Count (AV) > 0 and then Comes_From_Source (AV) then
         return False;

      --  A variable is always allowed

      elsif Is_Variable (AV) then
         return True;

      --  Unchecked conversions are allowed only if they come from the
      --  generated code, which sometimes uses unchecked conversions for
      --  out parameters in cases where code generation is unaffected.
      --  We tell source unchecked conversions by seeing if they are
      --  rewrites of an original UC function call.

      elsif Nkind (AV) = N_Unchecked_Type_Conversion then
         if Nkind (Original_Node (AV)) = N_Function_Call then
            return False;
         else
            return True;
         end if;

      --  Normal type conversions are allowed if argument is a variable

      elsif Nkind (AV) = N_Type_Conversion then
         if Is_Variable (Expression (AV))
           and then Paren_Count (Expression (AV)) = 0
         then
            Note_Possible_Modification (Expression (AV));
            return True;

         --  We also allow a non-parenthesized expression that raises
         --  constraint error if it rewrites what used to be a variable

         elsif Raises_Constraint_Error (Expression (AV))
            and then Paren_Count (Expression (AV)) = 0
            and then Is_Variable (Original_Node (Expression (AV)))
         then
            return True;

         --  Type conversion of something other than a variable

         else
            return False;
         end if;

      --  If this node is rewritten, then test the original form, if that is
      --  OK, then we consider the rewritten node OK (for example, if the
      --  original node is a conversion, then Is_Variable will not be true
      --  but we still want to allow the conversion if it converts a variable.

      elsif Original_Node (AV) /= AV then
         return Is_OK_Variable_For_Out_Formal (Original_Node (AV));

      --  All other non-variables are rejected

      else
         return False;
      end if;
   end Is_OK_Variable_For_Out_Formal;

   -----------------------------
   -- Is_RCI_Pkg_Spec_Or_Body --
   -----------------------------

   function Is_RCI_Pkg_Spec_Or_Body (Cunit : Node_Id) return Boolean is

      function Is_RCI_Pkg_Decl_Cunit (Cunit : Node_Id) return Boolean;
      --  Return True if the unit of Cunit is an RCI package declaration

      ---------------------------
      -- Is_RCI_Pkg_Decl_Cunit --
      ---------------------------

      function Is_RCI_Pkg_Decl_Cunit (Cunit : Node_Id) return Boolean is
         The_Unit : constant Node_Id := Unit (Cunit);

      begin
         if Nkind (The_Unit) /= N_Package_Declaration then
            return False;
         end if;
         return Is_Remote_Call_Interface (Defining_Entity (The_Unit));
      end Is_RCI_Pkg_Decl_Cunit;

   --  Start of processing for Is_RCI_Pkg_Spec_Or_Body

   begin
      return Is_RCI_Pkg_Decl_Cunit (Cunit)
        or else
         (Nkind (Unit (Cunit)) = N_Package_Body
           and then Is_RCI_Pkg_Decl_Cunit (Library_Unit (Cunit)));
   end Is_RCI_Pkg_Spec_Or_Body;

   -----------------------------------------
   -- Is_Remote_Access_To_Class_Wide_Type --
   -----------------------------------------

   function Is_Remote_Access_To_Class_Wide_Type
     (E    : Entity_Id)
      return Boolean
   is
      D : Entity_Id;

      function Comes_From_Limited_Private_Type_Declaration
        (E    : Entity_Id)
         return Boolean;
      --  Check if the original declaration is a limited private one and
      --  if all the derivations have been using private extensions.

      -------------------------------------------------
      -- Comes_From_Limited_Private_Type_Declaration --
      -------------------------------------------------

      function Comes_From_Limited_Private_Type_Declaration (E : in Entity_Id)
        return Boolean
      is
         N : constant Node_Id := Declaration_Node (E);
      begin
         if Nkind (N) = N_Private_Type_Declaration
           and then Limited_Present (N)
         then
            return True;
         end if;

         if Nkind (N) = N_Private_Extension_Declaration then
            return Comes_From_Limited_Private_Type_Declaration (Etype (E));
         end if;

         return False;
      end Comes_From_Limited_Private_Type_Declaration;

   --  Start of processing for Is_Remote_Access_To_Class_Wide_Type

   begin
      if not (Is_Remote_Call_Interface (E)
               or else Is_Remote_Types (E))
        or else Ekind (E) /= E_General_Access_Type
      then
         return False;
      end if;

      D := Designated_Type (E);

      if Ekind (D) /= E_Class_Wide_Type then
         return False;
      end if;

      return Comes_From_Limited_Private_Type_Declaration
               (Defining_Identifier (Parent (D)));
   end Is_Remote_Access_To_Class_Wide_Type;

   -----------------------------------------
   -- Is_Remote_Access_To_Subprogram_Type --
   -----------------------------------------

   function Is_Remote_Access_To_Subprogram_Type
     (E    : Entity_Id)
      return Boolean
   is
   begin
      return (Ekind (E) = E_Access_Subprogram_Type
                or else (Ekind (E) = E_Record_Type
                           and then Present (Corresponding_Remote_Type (E))))
        and then (Is_Remote_Call_Interface (E)
                   or else Is_Remote_Types (E));
   end Is_Remote_Access_To_Subprogram_Type;

   --------------------
   -- Is_Remote_Call --
   --------------------

   function Is_Remote_Call (N : Node_Id) return Boolean is
   begin
      if Nkind (N) /= N_Procedure_Call_Statement
        and then Nkind (N) /= N_Function_Call
      then
         --  An entry call cannot be remote

         return False;

      elsif Nkind (Name (N)) in N_Has_Entity
        and then Is_Remote_Call_Interface (Entity (Name (N)))
      then
         --  A subprogram declared in the spec of a RCI package is remote

         return True;

      elsif Nkind (Name (N)) = N_Explicit_Dereference
        and then Is_Remote_Access_To_Subprogram_Type
          (Etype (Prefix (Name (N))))
      then
         --  The dereference of a RAS is a remote call

         return True;

      elsif Present (Controlling_Argument (N))
        and then Is_Remote_Access_To_Class_Wide_Type
          (Etype (Controlling_Argument (N)))
      then
         --  Any primitive operation call with a controlling argument of
         --  a RACW type is a remote call.

         return True;
      end if;

      --  All other calls are local calls

      return False;
   end Is_Remote_Call;

   ----------------------
   -- Is_Selector_Name --
   ----------------------

   function Is_Selector_Name (N : Node_Id) return Boolean is

   begin
      if not Is_List_Member (N) then
         declare
            P : constant Node_Id   := Parent (N);
            K : constant Node_Kind := Nkind (P);

         begin
            return
              (K = N_Expanded_Name          or else
               K = N_Generic_Association    or else
               K = N_Parameter_Association  or else
               K = N_Selected_Component)
              and then Selector_Name (P) = N;
         end;

      else
         declare
            L : constant List_Id := List_Containing (N);
            P : constant Node_Id := Parent (L);

         begin
            return (Nkind (P) = N_Discriminant_Association
                     and then Selector_Names (P) = L)
              or else
                   (Nkind (P) = N_Component_Association
                     and then Choices (P) = L);
         end;
      end if;
   end Is_Selector_Name;

   ------------------
   -- Is_Statement --
   ------------------

   function Is_Statement (N : Node_Id) return Boolean is
   begin
      return
        Nkind (N) in N_Statement_Other_Than_Procedure_Call
          or else Nkind (N) = N_Procedure_Call_Statement;
   end Is_Statement;

   -----------------
   -- Is_Transfer --
   -----------------

   function Is_Transfer (N : Node_Id) return Boolean is
      Kind : constant Node_Kind := Nkind (N);

   begin
      if Kind = N_Return_Statement
           or else
         Kind = N_Goto_Statement
           or else
         Kind = N_Raise_Statement
           or else
         Kind = N_Requeue_Statement
      then
         return True;

      elsif (Kind = N_Exit_Statement or else Kind in N_Raise_xxx_Error)
        and then No (Condition (N))
      then
         return True;

      elsif Kind = N_Procedure_Call_Statement
        and then Is_Entity_Name (Name (N))
        and then Present (Entity (Name (N)))
        and then No_Return (Entity (Name (N)))
      then
         return True;

      elsif Nkind (Original_Node (N)) = N_Raise_Statement then
         return True;

      else
         return False;
      end if;
   end Is_Transfer;

   -------------
   -- Is_True --
   -------------

   function Is_True (U : Uint) return Boolean is
   begin
      return (U /= 0);
   end Is_True;

   -----------------
   -- Is_Variable --
   -----------------

   function Is_Variable (N : Node_Id) return Boolean is

      Orig_Node : constant Node_Id := Original_Node (N);
      --  We do the test on the original node, since this is basically a
      --  test of syntactic categories, so it must not be disturbed by
      --  whatever rewriting might have occurred. For example, an aggregate,
      --  which is certainly NOT a variable, could be turned into a variable
      --  by expansion.

      function In_Protected_Function (E : Entity_Id) return Boolean;
      --  Within a protected function, the private components of the
      --  enclosing protected type are constants. A function nested within
      --  a (protected) procedure is not itself protected.

      function Is_Variable_Prefix (P : Node_Id) return Boolean;
      --  Prefixes can involve implicit dereferences, in which case we
      --  must test for the case of a reference of a constant access
      --  type, which can never be a variable.

      function In_Protected_Function (E : Entity_Id) return Boolean is
         Prot : constant Entity_Id := Scope (E);
         S    : Entity_Id;

      begin
         if not Is_Protected_Type (Prot) then
            return False;
         else
            S := Current_Scope;

            while Present (S) and then S /= Prot loop

               if Ekind (S) = E_Function
                 and then Scope (S) = Prot
               then
                  return True;
               end if;

               S := Scope (S);
            end loop;

            return False;
         end if;
      end In_Protected_Function;

      function Is_Variable_Prefix (P : Node_Id) return Boolean is
      begin
         if Is_Access_Type (Etype (P)) then
            return not Is_Access_Constant (Root_Type (Etype (P)));
         else
            return Is_Variable (P);
         end if;
      end Is_Variable_Prefix;

   --  Start of processing for Is_Variable

   begin
      --  Definitely OK if Assignment_OK is set. Since this is something that
      --  only gets set for expanded nodes, the test is on N, not Orig_Node.

      if Nkind (N) in N_Subexpr and then Assignment_OK (N) then
         return True;

      --  Normally we go to the original node, but there is one exception
      --  where we use the rewritten node, namely when it is an explicit
      --  dereference. The generated code may rewrite a prefix which is an
      --  access type with an explicit dereference. The dereference is a
      --  variable, even though the original node may not be (since it could
      --  be a constant of the access type).

      elsif Nkind (N) = N_Explicit_Dereference
        and then Nkind (Orig_Node) /= N_Explicit_Dereference
        and then Is_Access_Type (Etype (Orig_Node))
      then
         return Is_Variable_Prefix (Original_Node (Prefix (N)));

      --  All remaining checks use the original node

      elsif Is_Entity_Name (Orig_Node) then
         declare
            E : constant Entity_Id := Entity (Orig_Node);
            K : constant Entity_Kind := Ekind (E);

         begin
            return (K = E_Variable
                      and then Nkind (Parent (E)) /= N_Exception_Handler)
              or else  (K = E_Component
                          and then not In_Protected_Function (E))
              or else  K = E_Out_Parameter
              or else  K = E_In_Out_Parameter
              or else  K = E_Generic_In_Out_Parameter

               --  Current instance of type:

              or else (Is_Type (E) and then In_Open_Scopes (E))
              or else (Is_Incomplete_Or_Private_Type (E)
                        and then In_Open_Scopes (Full_View (E)));
         end;

      else
         case Nkind (Orig_Node) is
            when N_Indexed_Component | N_Slice =>
               return Is_Variable_Prefix (Prefix (Orig_Node));

            when N_Selected_Component =>
               return Is_Variable_Prefix (Prefix (Orig_Node))
                 and then Is_Variable (Selector_Name (Orig_Node));

            --  For an explicit dereference, we must check whether the type
            --  is ACCESS CONSTANT, since if it is, then it is not a variable.

            when N_Explicit_Dereference =>
               return Is_Access_Type (Etype (Prefix (Orig_Node)))
                 and then not
                   Is_Access_Constant (Root_Type (Etype (Prefix (Orig_Node))));

            --  The type conversion is the case where we do not deal with the
            --  context dependent special case of an actual parameter. Thus
            --  the type conversion is only considered a variable for the
            --  purposes of this routine if the target type is tagged. However,
            --  a type conversion is considered to be a variable if it does not
            --  come from source (this deals for example with the conversions
            --  of expressions to their actual subtypes).

            when N_Type_Conversion =>
               return Is_Variable (Expression (Orig_Node))
                 and then
                   (not Comes_From_Source (Orig_Node)
                      or else
                        (Is_Tagged_Type (Etype (Subtype_Mark (Orig_Node)))
                          and then
                         Is_Tagged_Type (Etype (Expression (Orig_Node)))));

            --  GNAT allows an unchecked type conversion as a variable. This
            --  only affects the generation of internal expanded code, since
            --  calls to instantiations of Unchecked_Conversion are never
            --  considered variables (since they are function calls).
            --  This is also true for expression actions.

            when N_Unchecked_Type_Conversion =>
               return Is_Variable (Expression (Orig_Node));

            when others =>
               return False;
         end case;
      end if;
   end Is_Variable;

   ------------------------
   -- Is_Volatile_Object --
   ------------------------

   function Is_Volatile_Object (N : Node_Id) return Boolean is

      function Object_Has_Volatile_Components (N : Node_Id) return Boolean;
      --  Determines if given object has volatile components

      function Is_Volatile_Prefix (N : Node_Id) return Boolean;
      --  If prefix is an implicit dereference, examine designated type.

      function Is_Volatile_Prefix (N : Node_Id) return Boolean is
      begin
         if Is_Access_Type (Etype (N)) then
            return Has_Volatile_Components (Designated_Type (Etype (N)));
         else
            return Object_Has_Volatile_Components (N);
         end if;
      end Is_Volatile_Prefix;

      function Object_Has_Volatile_Components (N : Node_Id) return Boolean is
      begin
         if Is_Volatile (Etype (N))
           or else Has_Volatile_Components (Etype (N))
         then
            return True;

         elsif Is_Entity_Name (N)
           and then (Has_Volatile_Components (Entity (N))
                      or else Is_Volatile (Entity (N)))
         then
            return True;

         elsif Nkind (N) = N_Indexed_Component
           or else Nkind (N) = N_Selected_Component
         then
            return Is_Volatile_Prefix (Prefix (N));

         else
            return False;
         end if;
      end Object_Has_Volatile_Components;

   --  Start of processing for Is_Volatile_Object

   begin
      if Is_Volatile (Etype (N))
        or else (Is_Entity_Name (N) and then Is_Volatile (Entity (N)))
      then
         return True;

      elsif Nkind (N) = N_Indexed_Component
        or else Nkind (N) = N_Selected_Component
      then
         return Is_Volatile_Prefix (Prefix (N));

      else
         return False;
      end if;
   end Is_Volatile_Object;

   ------------------------
   -- Is_Wrapper_Package --
   ------------------------

   function Is_Wrapper_Package (E : Entity_Id) return Boolean is
   begin
      return (Ekind (E) = E_Package and then Present (Related_Instance (E)));
   end Is_Wrapper_Package;

   --------------------------
   -- Kill_Size_Check_Code --
   --------------------------

   procedure Kill_Size_Check_Code (E : Entity_Id) is
   begin
      if (Ekind (E) = E_Constant or else Ekind (E) = E_Variable)
        and then Present (Size_Check_Code (E))
      then
         Remove (Size_Check_Code (E));
         Set_Size_Check_Code (E, Empty);
      end if;
   end Kill_Size_Check_Code;

   -------------------------
   -- New_External_Entity --
   -------------------------

   function New_External_Entity
     (Kind         : Entity_Kind;
      Scope_Id     : Entity_Id;
      Sloc_Value   : Source_Ptr;
      Related_Id   : Entity_Id;
      Suffix       : Character;
      Suffix_Index : Nat := 0;
      Prefix       : Character := ' ')
      return         Entity_Id
   is
      N : constant Entity_Id :=
            Make_Defining_Identifier (Sloc_Value,
              New_External_Name
                (Chars (Related_Id), Suffix, Suffix_Index, Prefix));

   begin
      Set_Ekind          (N, Kind);
      Set_Is_Internal    (N, True);
      Append_Entity      (N, Scope_Id);
      Set_Public_Status  (N);
      return N;
   end New_External_Entity;

   -------------------------
   -- New_Internal_Entity --
   -------------------------

   function New_Internal_Entity
     (Kind       : Entity_Kind;
      Scope_Id   : Entity_Id;
      Sloc_Value : Source_Ptr;
      Id_Char    : Character)
      return       Entity_Id
   is
      N : constant Entity_Id :=
            Make_Defining_Identifier (Sloc_Value, New_Internal_Name (Id_Char));

   begin
      Set_Ekind          (N, Kind);
      Set_Is_Internal    (N, True);
      Append_Entity      (N, Scope_Id);
      return N;
   end New_Internal_Entity;

   -----------------
   -- Next_Actual --
   -----------------

   function Next_Actual (Actual_Id : Node_Id) return Node_Id is
      N  : Node_Id;

   begin
      --  If we are pointing at a positional parameter, it is a member of
      --  a node list (the list of parameters), and the next parameter
      --  is the next node on the list, unless we hit a parameter
      --  association, in which case we shift to using the chain whose
      --  head is the First_Named_Actual in the parent, and then is
      --  threaded using the Next_Named_Actual of the Parameter_Association.
      --  All this fiddling is because the original node list is in the
      --  textual call order, and what we need is the declaration order.

      if Is_List_Member (Actual_Id) then
         N := Next (Actual_Id);

         if Nkind (N) = N_Parameter_Association then
            return First_Named_Actual (Parent (Actual_Id));
         else
            return N;
         end if;

      else
         return Next_Named_Actual (Parent (Actual_Id));
      end if;
   end Next_Actual;

   procedure Next_Actual (Actual_Id : in out Node_Id) is
   begin
      Actual_Id := Next_Actual (Actual_Id);
   end Next_Actual;

   -----------------------
   -- Normalize_Actuals --
   -----------------------

   --  Chain actuals according to formals of subprogram. If there are
   --  no named associations, the chain is simply the list of Parameter
   --  Associations, since the order is the same as the declaration order.
   --  If there are named associations, then the First_Named_Actual field
   --  in the N_Procedure_Call_Statement node or N_Function_Call node
   --  points to the Parameter_Association node for the parameter that
   --  comes first in declaration order. The remaining named parameters
   --  are then chained in declaration order using Next_Named_Actual.

   --  This routine also verifies that the number of actuals is compatible
   --  with the number and default values of formals, but performs no type
   --  checking (type checking is done by the caller).

   --  If the matching succeeds, Success is set to True, and the caller
   --  proceeds with type-checking. If the match is unsuccessful, then
   --  Success is set to False, and the caller attempts a different
   --  interpretation, if there is one.

   --  If the flag Report is on, the call is not overloaded, and a failure
   --  to match can be reported here, rather than in the caller.

   procedure Normalize_Actuals
     (N       : Node_Id;
      S       : Entity_Id;
      Report  : Boolean;
      Success : out Boolean)
   is
      Actuals     : constant List_Id := Parameter_Associations (N);
      Actual      : Node_Id   := Empty;
      Formal      : Entity_Id;
      Last        : Node_Id := Empty;
      First_Named : Node_Id := Empty;
      Found       : Boolean;

      Formals_To_Match : Integer := 0;
      Actuals_To_Match : Integer := 0;

      procedure Chain (A : Node_Id);
      --  Add named actual at the proper place in the list, using the
      --  Next_Named_Actual link.

      function Reporting return Boolean;
      --  Determines if an error is to be reported. To report an error, we
      --  need Report to be True, and also we do not report errors caused
      --  by calls to Init_Proc's that occur within other Init_Proc's. Such
      --  errors must always be cascaded errors, since if all the types are
      --  declared correctly, the compiler will certainly build decent calls!

      procedure Chain (A : Node_Id) is
      begin
         if No (Last) then

            --  Call node points to first actual in list.

            Set_First_Named_Actual (N, Explicit_Actual_Parameter (A));

         else
            Set_Next_Named_Actual (Last, Explicit_Actual_Parameter (A));
         end if;

         Last := A;
         Set_Next_Named_Actual (Last, Empty);
      end Chain;

      function Reporting return Boolean is
      begin
         if not Report then
            return False;

         elsif not Within_Init_Proc then
            return True;

         elsif Chars (Entity (Name (N))) = Name_uInit_Proc then
            return False;

         else
            return True;
         end if;
      end Reporting;

   --  Start of processing for Normalize_Actuals

   begin
      if Is_Access_Type (S) then

         --  The name in the call is a function call that returns an access
         --  to subprogram. The designated type has the list of formals.

         Formal := First_Formal (Designated_Type (S));
      else
         Formal := First_Formal (S);
      end if;

      while Present (Formal) loop
         Formals_To_Match := Formals_To_Match + 1;
         Next_Formal (Formal);
      end loop;

      --  Find if there is a named association, and verify that no positional
      --  associations appear after named ones.

      if Present (Actuals) then
         Actual := First (Actuals);
      end if;

      while Present (Actual)
        and then Nkind (Actual) /= N_Parameter_Association
      loop
         Actuals_To_Match := Actuals_To_Match + 1;
         Next (Actual);
      end loop;

      if No (Actual) and Actuals_To_Match = Formals_To_Match then

         --  Most common case: positional notation, no defaults

         Success := True;
         return;

      elsif Actuals_To_Match > Formals_To_Match then

         --  Too many actuals: will not work.

         if Reporting then
            Error_Msg_N ("too many arguments in call", N);
         end if;

         Success := False;
         return;
      end if;

      First_Named := Actual;

      while Present (Actual) loop
         if Nkind (Actual) /= N_Parameter_Association then
            Error_Msg_N
              ("positional parameters not allowed after named ones", Actual);
            Success := False;
            return;

         else
            Actuals_To_Match := Actuals_To_Match + 1;
         end if;

         Next (Actual);
      end loop;

      if Present (Actuals) then
         Actual := First (Actuals);
      end if;

      Formal := First_Formal (S);

      while Present (Formal) loop

         --  Match the formals in order. If the corresponding actual
         --  is positional,  nothing to do. Else scan the list of named
         --  actuals to find the one with the right name.

         if Present (Actual)
           and then Nkind (Actual) /= N_Parameter_Association
         then
            Next (Actual);
            Actuals_To_Match := Actuals_To_Match - 1;
            Formals_To_Match := Formals_To_Match - 1;

         else
            --  For named parameters, search the list of actuals to find
            --  one that matches the next formal name.

            Actual := First_Named;
            Found  := False;

            while Present (Actual) loop
               if Chars (Selector_Name (Actual)) = Chars (Formal) then
                  Found := True;
                  Chain (Actual);
                  Actuals_To_Match := Actuals_To_Match - 1;
                  Formals_To_Match := Formals_To_Match - 1;
                  exit;
               end if;

               Next (Actual);
            end loop;

            if not Found then
               if Ekind (Formal) /= E_In_Parameter
                 or else No (Default_Value (Formal))
               then
                  if Reporting then
                     if Comes_From_Source (S)
                       and then Is_Overloadable (S)
                     then
                        Error_Msg_Name_1 := Chars (S);
                        Error_Msg_Sloc := Sloc (S);
                        Error_Msg_NE
                          ("missing argument for parameter & " &
                             "in call to % declared #", N, Formal);
                     else
                        Error_Msg_NE
                          ("missing argument for parameter &", N, Formal);
                     end if;
                  end if;

                  Success := False;
                  return;

               else
                  Formals_To_Match := Formals_To_Match - 1;
               end if;
            end if;
         end if;

         Next_Formal (Formal);
      end loop;

      if  Formals_To_Match = 0 and then Actuals_To_Match = 0 then
         Success := True;
         return;

      else
         if Reporting then

            --  Find some superfluous named actual that did not get
            --  attached to the list of associations.

            Actual := First (Actuals);

            while Present (Actual) loop

               if Nkind (Actual) = N_Parameter_Association
                 and then Actual /= Last
                 and then No (Next_Named_Actual (Actual))
               then
                  Error_Msg_N ("Unmatched actual in call",  Actual);
                  exit;
               end if;

               Next (Actual);
            end loop;
         end if;

         Success := False;
         return;
      end if;
   end Normalize_Actuals;

   --------------------------------
   -- Note_Possible_Modification --
   --------------------------------

   procedure Note_Possible_Modification (N : Node_Id) is
      Ent : Entity_Id;
      Exp : Node_Id;

      procedure Set_Ref (E : Entity_Id);
      --  Internal routine to note modification on entity E

      procedure Set_Ref (E : Entity_Id) is
      begin
         Set_Not_Source_Assigned (E, False);
         Set_Is_True_Constant (E, False);
         Generate_Reference (E, N, 'm');
      end Set_Ref;

   --  Start of processing for Note_Possible_Modification

   begin
      --  Loop to find referenced entity, if there is one

      Exp := N;
      loop
         --  Test for node rewritten as dereference (e.g. accept parameter)

         if Nkind (Exp) = N_Explicit_Dereference
           and then Is_Entity_Name (Original_Node (Exp))
         then
            Set_Ref (Entity (Original_Node (Exp)));
            return;

         elsif Is_Entity_Name (Exp) then
            Ent := Entity (Exp);

            if (Ekind (Ent) = E_Variable or else Ekind (Ent) = E_Constant)
              and then Present (Renamed_Object (Ent))
            then
               Exp := Renamed_Object (Ent);

            else
               Set_Ref (Ent);
               return;
            end if;

         elsif     Nkind (Exp) = N_Type_Conversion
           or else Nkind (Exp) = N_Unchecked_Type_Conversion
         then
            Exp := Expression (Exp);

         elsif     Nkind (Exp) = N_Slice
           or else Nkind (Exp) = N_Indexed_Component
           or else Nkind (Exp) = N_Selected_Component
         then
            Exp := Prefix (Exp);

         else
            return;
         end if;
      end loop;
   end Note_Possible_Modification;

   -------------------------
   -- Object_Access_Level --
   -------------------------

   function Object_Access_Level (Obj : Node_Id) return Uint is
      E : Entity_Id;

   --  Returns the static accessibility level of the view denoted
   --  by Obj.  Note that the value returned is the result of a
   --  call to Scope_Depth.  Only scope depths associated with
   --  dynamic scopes can actually be returned.  Since only
   --  relative levels matter for accessibility checking, the fact
   --  that the distance between successive levels of accessibility
   --  is not always one is immaterial (invariant: if level(E2) is
   --  deeper than level(E1), then Scope_Depth(E1) < Scope_Depth(E2)).

   begin
      if Is_Entity_Name (Obj) then
         E := Entity (Obj);

         --  If E is a type then it denotes a current instance.
         --  For this case we add one to the normal accessibility
         --  level of the type to ensure that current instances
         --  are treated as always being deeper than than the level
         --  of any visible named access type (see 3.10.2(21)).

         if Is_Type (E) then
            return Type_Access_Level (E) +  1;

         elsif Present (Renamed_Object (E)) then
            return Object_Access_Level (Renamed_Object (E));

         --  Similarly, if E is a component of the current instance of a
         --  protected type, any instance of it is assumed to be at a deeper
         --  level than the type. For a protected object (whose type is an
         --  anonymous protected type) its components are at the same level
         --  as the type itself.

         elsif not Is_Overloadable (E)
           and then Ekind (Scope (E)) = E_Protected_Type
           and then Comes_From_Source (Scope (E))
         then
            return Type_Access_Level (Scope (E)) + 1;

         else
            return Scope_Depth (Enclosing_Dynamic_Scope (E));
         end if;

      elsif Nkind (Obj) = N_Selected_Component then
         if Is_Access_Type (Etype (Prefix (Obj))) then
            return Type_Access_Level (Etype (Prefix (Obj)));
         else
            return Object_Access_Level (Prefix (Obj));
         end if;

      elsif Nkind (Obj) = N_Indexed_Component then
         if Is_Access_Type (Etype (Prefix (Obj))) then
            return Type_Access_Level (Etype (Prefix (Obj)));
         else
            return Object_Access_Level (Prefix (Obj));
         end if;

      elsif Nkind (Obj) = N_Explicit_Dereference then

         --  If the prefix is a selected access discriminant then
         --  we make a recursive call on the prefix, which will
         --  in turn check the level of the prefix object of
         --  the selected discriminant.

         if Nkind (Prefix (Obj)) = N_Selected_Component
           and then Ekind (Etype (Prefix (Obj))) = E_Anonymous_Access_Type
           and then
             Ekind (Entity (Selector_Name (Prefix (Obj)))) = E_Discriminant
         then
            return Object_Access_Level (Prefix (Obj));
         else
            return Type_Access_Level (Etype (Prefix (Obj)));
         end if;

      elsif Nkind (Obj) = N_Type_Conversion then
         return Object_Access_Level (Expression (Obj));

      --  Function results are objects, so we get either the access level
      --  of the function or, in the case of an indirect call, the level of
      --  of the access-to-subprogram type.

      elsif Nkind (Obj) = N_Function_Call then
         if Is_Entity_Name (Name (Obj)) then
            return Subprogram_Access_Level (Entity (Name (Obj)));
         else
            return Type_Access_Level (Etype (Prefix (Name (Obj))));
         end if;

      --  For convenience we handle qualified expressions, even though
      --  they aren't technically object names.

      elsif Nkind (Obj) = N_Qualified_Expression then
         return Object_Access_Level (Expression (Obj));

      --  Otherwise return the scope level of Standard.
      --  (If there are cases that fall through
      --  to this point they will be treated as
      --  having global accessibility for now. ???)

      else
         return Scope_Depth (Standard_Standard);
      end if;
   end Object_Access_Level;

   -----------------------
   -- Private_Component --
   -----------------------

   function Private_Component (Type_Id : Entity_Id) return Entity_Id is
      Ancestor  : constant Entity_Id := Base_Type (Type_Id);

      function Trace_Components
        (T     : Entity_Id;
         Check : Boolean)
         return  Entity_Id;
      --  Recursive function that does the work, and checks against circular
      --  definition for each subcomponent type.

      ----------------------
      -- Trace_Components --
      ----------------------

      function Trace_Components
         (T     : Entity_Id;
          Check : Boolean) return Entity_Id
       is
         Btype     : constant Entity_Id := Base_Type (T);
         Component : Entity_Id;
         P         : Entity_Id;
         Candidate : Entity_Id := Empty;

      begin
         if Check and then Btype = Ancestor then
            Error_Msg_N ("circular type definition", Type_Id);
            return Any_Type;
         end if;

         if Is_Private_Type (Btype)
           and then not Is_Generic_Type (Btype)
         then
            return Btype;

         elsif Is_Array_Type (Btype) then
            return Trace_Components (Component_Type (Btype), True);

         elsif Is_Record_Type (Btype) then
            Component := First_Entity (Btype);
            while Present (Component) loop
               if not Is_Type (Component) then
                  P := Trace_Components (Etype (Component), True);
               end if;

               if Present (P) then
                  if P = Any_Type then
                     return P;
                  else
                     Candidate := P;
                  end if;
               end if;

               Next_Entity (Component);
            end loop;

            return Candidate;

         else
            return Empty;
         end if;
      end Trace_Components;

   --  Start of processing for Private_Component

   begin
      return Trace_Components (Type_Id, False);
   end Private_Component;

   ------------------
   -- Real_Convert --
   ------------------

   --  We do the conversion to get the value of the real string by using
   --  the scanner, see Sinput for details on use of the internal source
   --  buffer for scanning internal strings.

   function Real_Convert (S : String) return Node_Id is
      Save_Src : constant Source_Buffer_Ptr := Source;
      Negative : Boolean;

   begin
      Source := Internal_Source_Ptr;
      Scan_Ptr := 1;

      for J in S'Range loop
         Source (Source_Ptr (J)) := S (J);
      end loop;

      Source (S'Length + 1) := EOF;

      if Source (Scan_Ptr) = '-' then
         Negative := True;
         Scan_Ptr := Scan_Ptr + 1;
      else
         Negative := False;
      end if;

      Scan;

      if Negative then
         Set_Realval (Token_Node, UR_Negate (Realval (Token_Node)));
      end if;

      Source := Save_Src;
      return Token_Node;
   end Real_Convert;

   --------------------------
   -- Reset_Analyzed_Flags --
   --------------------------

   procedure Reset_Analyzed_Flags (N : Node_Id) is

      function Clear_Analyzed
        (N    : Node_Id)
         return Traverse_Result;
      --  Function used to reset Analyzed flags in tree. Note that we do
      --  not reset Analyzed flags in entities, since there is no need to
      --  renalalyze entities, and indeed, it is wrong to do so, since it
      --  can result in generating auxiliary stuff more than once.

      function Clear_Analyzed
        (N    : Node_Id)
         return Traverse_Result
      is
      begin
         if not Has_Extension (N) then
            Set_Analyzed (N, False);
         end if;

         return OK;
      end Clear_Analyzed;

      function Reset_Analyzed is
        new Traverse_Func (Clear_Analyzed);

      Discard : Traverse_Result;

   --  Start of processing for Reset_Analyzed_Flags

   begin
      Discard := Reset_Analyzed (N);
   end Reset_Analyzed_Flags;

   ---------------
   -- Same_Name --
   ---------------

   function Same_Name (N1, N2 : Node_Id) return Boolean is
      K1 : constant Node_Kind := Nkind (N1);
      K2 : constant Node_Kind := Nkind (N2);

   begin
      if (K1 = N_Identifier or else K1 = N_Defining_Identifier)
        and then (K2 = N_Identifier or else K2 = N_Defining_Identifier)
      then
         return Chars (N1) = Chars (N2);

      elsif (K1 = N_Selected_Component or else K1 = N_Expanded_Name)
        and then (K2 = N_Selected_Component or else K2 = N_Expanded_Name)
      then
         return Same_Name (Selector_Name (N1), Selector_Name (N2))
           and then Same_Name (Prefix (N1), Prefix (N2));

      else
         return False;
      end if;
   end Same_Name;

   ---------------
   -- Same_Type --
   ---------------

   function Same_Type (T1, T2 : Entity_Id) return Boolean is
   begin
      if T1 = T2 then
         return True;

      elsif not Is_Constrained (T1)
        and then not Is_Constrained (T2)
        and then Base_Type (T1) = Base_Type (T2)
      then
         return True;

      --  For now don't bother with case of identical constraints, to be
      --  fiddled with later on perhaps (this is only used for optimization
      --  purposes, so it is not critical to do a best possible job)

      else
         return False;
      end if;
   end Same_Type;

   ------------------------
   -- Scope_Is_Transient --
   ------------------------

   function Scope_Is_Transient  return Boolean is
   begin
      return Scope_Stack.Table (Scope_Stack.Last).Is_Transient;
   end Scope_Is_Transient;

   ------------------
   -- Scope_Within --
   ------------------

   function Scope_Within (Scope1, Scope2 : Entity_Id) return Boolean is
      Scop : Entity_Id;

   begin
      Scop := Scope1;
      while Scop /= Standard_Standard loop
         Scop := Scope (Scop);

         if Scop = Scope2 then
            return True;
         end if;
      end loop;

      return False;
   end Scope_Within;

   --------------------------
   -- Scope_Within_Or_Same --
   --------------------------

   function Scope_Within_Or_Same (Scope1, Scope2 : Entity_Id) return Boolean is
      Scop : Entity_Id;

   begin
      Scop := Scope1;
      while Scop /= Standard_Standard loop
         if Scop = Scope2 then
            return True;
         else
            Scop := Scope (Scop);
         end if;
      end loop;

      return False;
   end Scope_Within_Or_Same;

   ------------------------
   -- Set_Current_Entity --
   ------------------------

   --  The given entity is to be set as the currently visible definition
   --  of its associated name (i.e. the Node_Id associated with its name).
   --  All we have to do is to get the name from the identifier, and
   --  then set the associated Node_Id to point to the given entity.

   procedure Set_Current_Entity (E : Entity_Id) is
   begin
      Set_Name_Entity_Id (Chars (E), E);
   end Set_Current_Entity;

   ---------------------------------
   -- Set_Entity_With_Style_Check --
   ---------------------------------

   procedure Set_Entity_With_Style_Check (N : Node_Id; Val : Entity_Id) is
      Val_Actual : Entity_Id;
      Nod        : Node_Id;

   begin
      Set_Entity (N, Val);

      if Style_Check and then not Suppress_Style_Checks (Val) then
         if Nkind (N) = N_Identifier then
            Nod := N;

         elsif Nkind (N) = N_Expanded_Name then
            Nod := Selector_Name (N);

         else
            return;
         end if;

         Val_Actual := Val;

         --  A special situation arises for derived operations, where we want
         --  to do the check against the parent (since the Sloc of the derived
         --  operation points to the derived type declaration itself).

         while not Comes_From_Source (Val_Actual)
           and then Nkind (Val_Actual) in N_Entity
           and then (Ekind (Val_Actual) = E_Enumeration_Literal
                      or else Ekind (Val_Actual) = E_Function
                      or else Ekind (Val_Actual) = E_Generic_Function
                      or else Ekind (Val_Actual) = E_Procedure
                      or else Ekind (Val_Actual) = E_Generic_Procedure)
           and then Present (Alias (Val_Actual))
         loop
            Val_Actual := Alias (Val_Actual);
         end loop;

         --  Renaming declarations for generic actuals do not come from source,
         --  and have a different name from that of the entity they rename, so
         --  there is no style check to perform here.

         if Chars (Nod) = Chars (Val_Actual) then
            Style.Check_Identifier (Nod, Val_Actual);
         end if;

      end if;

      Set_Entity (N, Val);
   end Set_Entity_With_Style_Check;

   ------------------------
   -- Set_Name_Entity_Id --
   ------------------------

   procedure Set_Name_Entity_Id (Id : Name_Id; Val : Entity_Id) is
   begin
      Set_Name_Table_Info (Id, Int (Val));
   end Set_Name_Entity_Id;

   ---------------------
   -- Set_Next_Actual --
   ---------------------

   procedure Set_Next_Actual (Ass1_Id : Node_Id; Ass2_Id : Node_Id) is
   begin
      if Nkind (Parent (Ass1_Id)) = N_Parameter_Association then
         Set_First_Named_Actual (Parent (Ass1_Id), Ass2_Id);
      end if;
   end Set_Next_Actual;

   -----------------------
   -- Set_Public_Status --
   -----------------------

   procedure Set_Public_Status (Id : Entity_Id) is
      S : constant Entity_Id := Current_Scope;

   begin
      if S = Standard_Standard
        or else (Is_Public (S)
                  and then (Ekind (S) = E_Package
                             or else Is_Record_Type (S)
                             or else Ekind (S) = E_Void))
      then
         Set_Is_Public (Id);

      --  The bounds of an entry family declaration can generate object
      --  declarations that are visible to the back-end, e.g. in the
      --  the declaration of a composite type that contains tasks.

      elsif Is_Public (S)
        and then Is_Concurrent_Type (S)
        and then not Has_Completion (S)
        and then Nkind (Parent (Id)) = N_Object_Declaration
      then
         Set_Is_Public (Id);
      end if;
   end Set_Public_Status;

   ----------------------------
   -- Set_Scope_Is_Transient --
   ----------------------------

   procedure Set_Scope_Is_Transient (V : Boolean := True) is
   begin
      Scope_Stack.Table (Scope_Stack.Last).Is_Transient := V;
   end Set_Scope_Is_Transient;

   -------------------
   -- Set_Size_Info --
   -------------------

   procedure Set_Size_Info (T1, T2 : Entity_Id) is
   begin
      Set_Esize                     (T1, Esize                     (T2));
      Set_Has_Biased_Representation (T1, Has_Biased_Representation (T2));

      if Is_Discrete_Or_Fixed_Point_Type (T1)
           and then
         Is_Discrete_Or_Fixed_Point_Type (T2)
      then
         Set_Is_Unsigned_Type       (T1, Is_Unsigned_Type          (T2));
      end if;

      Set_Alignment                 (T1, Alignment                 (T2));
   end Set_Size_Info;

   --------------------
   -- Static_Integer --
   --------------------

   function Static_Integer (N : Node_Id) return Uint is
   begin
      Analyze_And_Resolve (N, Any_Integer);

      if N = Error
        or else Error_Posted (N)
        or else Etype (N) = Any_Type
      then
         return No_Uint;
      end if;

      if Is_Static_Expression (N) then
         if not Raises_Constraint_Error (N) then
            return Expr_Value (N);
         else
            return No_Uint;
         end if;

      elsif Etype (N) = Any_Type then
         return No_Uint;

      else
         Error_Msg_N ("static integer expression required here", N);
         return No_Uint;
      end if;
   end Static_Integer;

   --------------------------
   -- Statically_Different --
   --------------------------

   function Statically_Different (E1, E2 : Node_Id) return Boolean is
      R1 : constant Node_Id := Get_Referenced_Object (E1);
      R2 : constant Node_Id := Get_Referenced_Object (E2);

   begin
      return     Is_Entity_Name (R1)
        and then Is_Entity_Name (R2)
        and then Entity (R1) /= Entity (R2)
        and then not Is_Formal (Entity (R1))
        and then not Is_Formal (Entity (R2));
   end Statically_Different;

   -----------------------------
   -- Subprogram_Access_Level --
   -----------------------------

   function Subprogram_Access_Level (Subp : Entity_Id) return Uint is
   begin
      if Present (Alias (Subp)) then
         return Subprogram_Access_Level (Alias (Subp));
      else
         return Scope_Depth (Enclosing_Dynamic_Scope (Subp));
      end if;
   end Subprogram_Access_Level;

   -----------------
   -- Trace_Scope --
   -----------------

   procedure Trace_Scope (N : Node_Id; E : Entity_Id; Msg : String) is
   begin
      if Debug_Flag_W then
         for J in 0 .. Scope_Stack.Last loop
            Write_Str ("  ");
         end loop;

         Write_Str (Msg);
         Write_Name (Chars (E));
         Write_Str ("   line ");
         Write_Int (Int (Get_Line_Number (Sloc (N))));
         Write_Eol;
      end if;
   end Trace_Scope;

   -----------------------
   -- Transfer_Entities --
   -----------------------

   procedure Transfer_Entities (From : Entity_Id; To : Entity_Id) is
      Ent      : Entity_Id := First_Entity (From);

   begin
      if No (Ent) then
         return;
      end if;

      if (Last_Entity (To)) = Empty then
         Set_First_Entity (To, Ent);
      else
         Set_Next_Entity (Last_Entity (To), Ent);
      end if;

      Set_Last_Entity (To, Last_Entity (From));

      while Present (Ent) loop
         Set_Scope (Ent, To);

         if not Is_Public (Ent) then
            Set_Public_Status (Ent);

            if Is_Public (Ent)
              and then Ekind (Ent) = E_Record_Subtype

            then
               --  The components of the propagated Itype must be public
               --  as well.

               declare
                  Comp : Entity_Id;

               begin
                  Comp := First_Entity (Ent);

                  while Present (Comp) loop
                     Set_Is_Public (Comp);
                     Next_Entity (Comp);
                  end loop;
               end;
            end if;
         end if;

         Next_Entity (Ent);
      end loop;

      Set_First_Entity (From, Empty);
      Set_Last_Entity (From, Empty);
   end Transfer_Entities;

   -----------------------
   -- Type_Access_Level --
   -----------------------

   function Type_Access_Level (Typ : Entity_Id) return Uint is
      Btyp : Entity_Id := Base_Type (Typ);

   begin
      --  If the type is an anonymous access type we treat it as being
      --  declared at the library level to ensure that names such as
      --  X.all'access don't fail static accessibility checks.

      if Ekind (Btyp) in Access_Kind then
         if Ekind (Btyp) = E_Anonymous_Access_Type then
            return Scope_Depth (Standard_Standard);
         end if;

         Btyp := Root_Type (Btyp);
      end if;

      return Scope_Depth (Enclosing_Dynamic_Scope (Btyp));
   end Type_Access_Level;

   -------------------
   -- Unimplemented --
   -------------------

   procedure Unimplemented (N : Node_Id; Feature : String) is
      Msg1 : constant String := " not implemented yet";
      Msg2 : String (Feature'First .. Feature'Last + Msg1'Length);

   begin
      --  Note that we don't want to use dynamic concatenation in the compiler
      --  (to limit the number of run-time routines, and hence the possibility
      --  of bootstrap path problems is reduced).

      Msg2 (Feature'Range) := Feature;
      Msg2 (Feature'Last + 1 .. Msg2'Last) := Msg1;
      Error_Msg_N (Msg2, N);
   end Unimplemented;

   ----------------------
   -- Within_Init_Proc --
   ----------------------

   function Within_Init_Proc return Boolean is
      S : Entity_Id;

   begin
      S := Current_Scope;
      while not Is_Overloadable (S) loop
         if S = Standard_Standard then
            return False;
         else
            S := Scope (S);
         end if;
      end loop;

      return Chars (S) = Name_uInit_Proc;
   end Within_Init_Proc;

   ----------------
   -- Wrong_Type --
   ----------------

   procedure Wrong_Type (Expr : Node_Id; Expected_Type : Entity_Id) is
      Found_Type : constant Entity_Id := First_Subtype (Etype (Expr));
      Expec_Type : constant Entity_Id := First_Subtype (Expected_Type);

      function Has_One_Matching_Field return Boolean;
      --  Determines whether Expec_Type is a record type with a single
      --  component or discriminant whose type matches the found type or
      --  is a one dimensional array whose component type matches the
      --  found type.

      function Has_One_Matching_Field return Boolean is
         E : Entity_Id;

      begin
         if Is_Array_Type (Expec_Type)
           and then Number_Dimensions (Expec_Type) = 1
           and then
             Covers (Etype (Component_Type (Expec_Type)), Found_Type)
         then
            return True;

         elsif not Is_Record_Type (Expec_Type) then
            return False;

         else
            E := First_Entity (Expec_Type);

            loop
               if No (E) then
                  return False;

               elsif (Ekind (E) /= E_Discriminant
                       and then Ekind (E) /= E_Component)
                 or else (Chars (E) = Name_uTag
                           or else Chars (E) = Name_uParent)
               then
                  Next_Entity (E);

               else
                  exit;
               end if;
            end loop;

            if not Covers (Etype (E), Found_Type) then
               return False;

            elsif Present (Next_Entity (E)) then
               return False;

            else
               return True;
            end if;
         end if;
      end Has_One_Matching_Field;

   --  Start of processing for Wrong_Type

   begin
      --  Don't output message if either type is Any_Type, or if a message
      --  has already been posted for this node. We need to do the latter
      --  check explicitly (it is ordinarily done in Errout), because we
      --  are using ! to force the output of the error messages.

      if Expec_Type = Any_Type
        or else Found_Type = Any_Type
        or else Error_Posted (Expr)
      then
         return;

      --  In  an instance, there is an ongoing problem with completion of
      --  type derived from private types. Their structure is what Gigi
      --  expects, but the  Etype is the parent type rather than the
      --  derived private type itself. Do not flag error in this case. The
      --  private completion is an entity without a parent, like an Itype.
      --  Similarly, full and partial views may be incorrect in the instance.
      --  There is no simple way to insure that it is consistent ???

      elsif In_Instance then

         if Etype (Etype (Expr)) = Etype (Expected_Type)
           and then No (Parent (Expected_Type))
         then
            return;
         end if;
      end if;

      --  An interesting special check. If the expression is parenthesized
      --  and its type corresponds to the type of the sole component of the
      --  expected record type, or to the component type of the expected one
      --  dimensional array type, then assume we have a bad aggregate attempt.

      if Nkind (Expr) in N_Subexpr
        and then Paren_Count (Expr) /= 0
        and then Has_One_Matching_Field
      then
         Error_Msg_N ("positional aggregate cannot have one component", Expr);

      --  Another special check, if we are looking for a pool-specific access
      --  type and we found an E_Access_Attribute_Type, then we have the case
      --  of an Access attribute being used in a context which needs a pool-
      --  specific type, which is never allowed. The one extra check we make
      --  is that the expected designated type covers the Found_Type.

      elsif Is_Access_Type (Expec_Type)
        and then Ekind (Found_Type) = E_Access_Attribute_Type
        and then Ekind (Base_Type (Expec_Type)) /= E_General_Access_Type
        and then Ekind (Base_Type (Expec_Type)) /= E_Anonymous_Access_Type
        and then Covers
          (Designated_Type (Expec_Type), Designated_Type (Found_Type))
      then
         Error_Msg_N ("result must be general access type!", Expr);
         Error_Msg_NE ("add ALL to }!", Expr, Expec_Type);

      --  Normal case of one type found, some other type expected

      else
         --  If the names of the two types are the same, see if some
         --  number of levels of qualification will help. Don't try
         --  more than three levels, and if we get to standard, it's
         --  no use (and probably represents an error in the compiler)
         --  Also do not bother with internal scope names.

         declare
            Expec_Scope : Entity_Id;
            Found_Scope : Entity_Id;

         begin
            Expec_Scope := Expec_Type;
            Found_Scope := Found_Type;

            for Levels in Int range 0 .. 3 loop
               if Chars (Expec_Scope) /= Chars (Found_Scope) then
                  Error_Msg_Qual_Level := Levels;
                  exit;
               end if;

               Expec_Scope := Scope (Expec_Scope);
               Found_Scope := Scope (Found_Scope);

               exit when Expec_Scope = Standard_Standard
                           or else
                         Found_Scope = Standard_Standard
                           or else
                         not Comes_From_Source (Expec_Scope)
                           or else
                         not Comes_From_Source (Found_Scope);
            end loop;
         end;

         Error_Msg_NE ("expected}!", Expr, Expec_Type);

         if Is_Entity_Name (Expr)
           and then Is_Package (Entity (Expr))
         then
            Error_Msg_N ("found package name!", Expr);

         elsif Is_Entity_Name (Expr)
           and then
             (Ekind (Entity (Expr)) = E_Procedure
                or else
              Ekind (Entity (Expr)) = E_Generic_Procedure)
         then
            Error_Msg_N ("found procedure name instead of function!", Expr);

         --  catch common error: a prefix or infix operator which is not
         --  directly visible because the type isn't.

         elsif Nkind (Expr) in N_Op
            and then Is_Overloaded (Expr)
            and then not Is_Immediately_Visible (Expec_Type)
            and then not Is_Potentially_Use_Visible (Expec_Type)
            and then not In_Use (Expec_Type)
            and then Has_Compatible_Type (Right_Opnd (Expr), Expec_Type)
         then
            Error_Msg_N (
              "operator of the type is not directly visible!", Expr);

         else
            Error_Msg_NE ("found}!", Expr, Found_Type);
         end if;

         Error_Msg_Qual_Level := 0;
      end if;
   end Wrong_Type;

end Sem_Util;
