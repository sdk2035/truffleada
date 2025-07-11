------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ T Y P E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.186 $
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
with Lib;      use Lib;
with Opt;      use Opt;
with Output;   use Output;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Uintp;    use Uintp;

package body Sem_Type is

   -------------------------------------
   -- Handling of Overload Resolution --
   -------------------------------------

   --  Overload resolution uses two passes over the syntax tree of a complete
   --  context. In the first, bottom-up pass, the types of actuals in calls
   --  are used to resolve possibly overloaded subprogram and operator names.
   --  In the second top-down pass, the type of the context (for example the
   --  condition in a while statement) is used to resolve a possibly ambiguous
   --  call, and the unique subprogram name in turn imposes a specific context
   --  on each of its actuals.

   --  Most expressions are in fact unambiguous, and the bottom-up pass is
   --  sufficient  to resolve most everything. To simplify the common case,
   --  names and expressions carry a flag Is_Overloaded to indicate whether
   --  they have more than one interpretation. If the flag is off, then each
   --  name has already a unique meaning and type, and the bottom-up pass is
   --  sufficient (and much simpler).

   --------------------------
   -- Operator Overloading --
   --------------------------

   --  The visibility of operators is handled differently from that of
   --  other entities. We do not introduce explicit versions of primitive
   --  operators for each type definition. As a result, there is only one
   --  entity corresponding to predefined addition on all numeric types, etc.
   --  The back-end resolves predefined operators according to their type.
   --  The visibility of primitive operations then reduces to the visibility
   --  of the resulting type:  (a + b) is a legal interpretation of some
   --  primitive operator + if the type of the result (which must also be
   --  the type of a and b) is directly visible (i.e. either immediately
   --  visible or use-visible.)

   --  User-defined operators are treated like other functions, but the
   --  visibility of these user-defined operations must be special-cased
   --  to determine whether they hide or are hidden by predefined operators.
   --  The form P."+" (x, y) requires additional handling.
   --
   --  Concatenation is treated more conventionally: for every one-dimensional
   --  array type we introduce a explicit concatenation operator. This is
   --  necessary to handle the case of (element & element => array) which
   --  cannot be handled conveniently if there is no explicit instance of
   --  resulting type of the operation.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure All_Overloads;
   pragma Warnings (Off, All_Overloads);
   --  Debugging procedure: list full contents of Overloads table.

   function Universal_Interpretation (Opnd : Node_Id) return Entity_Id;
   --  Yields universal_Integer or Universal_Real if this is a candidate.

   function Specific_Type (T1, T2 : Entity_Id) return Entity_Id;
   --  If T1 and T2 are compatible, return  the one that is not
   --  universal or is not a "class" type (any_character,  etc).

   ------------------------
   -- Init_Interp_Tables --
   ------------------------

   procedure Init_Interp_Tables is
   begin
      All_Interp.Init;
      Interp_Map.Init;
   end Init_Interp_Tables;

   --------------------
   -- Add_One_Interp --
   --------------------

   procedure Add_One_Interp (N : Node_Id; E : Entity_Id; T : Entity_Id) is

      Vis_Type : Entity_Id;

      procedure Add_Entry (Name :  Entity_Id; Typ : Entity_Id);
      --  Add one interpretation to node. Node is already known to be
      --  overloaded. Add new interpretation if not hidden by previous
      --  one, and remove previous one if hidden by new one.

      function Is_Universal_Operation (Op : Entity_Id) return Boolean;
      --  True if the entity is a predefined operator and the operands have
      --  a universal Interpretation.

      ---------------
      -- Add_Entry --
      ---------------

      procedure Add_Entry (Name :  Entity_Id; Typ : Entity_Id) is
         Index : Interp_Index;
         It    : Interp;

      begin
         Get_First_Interp (N, Index, It);

         while Present (It.Nam) loop

            --  A user-defined subprogram hides another declared at an outer
            --  level, or one that is use-visible. So return if previous
            --  definition hides new one (which is either in an outer
            --  scope, or use-visible). Note that for functions use-visible
            --  is the same as potentially use-visible. If new one hides
            --  previous one, replace entry in table of interpretations.
            --  If this is a universal operation, retain the operator in case
            --  preference rule applies.

            if (((Ekind (Name) = E_Function or else Ekind (Name) = E_Procedure)
                 and then Ekind (Name) = Ekind (It.Nam))
                or else (Ekind (Name) = E_Operator
              and then Ekind (It.Nam) = E_Function))

              and then Is_Immediately_Visible (It.Nam)
              and then Type_Conformant (Name, It.Nam)
              and then Base_Type (It.Typ) = Base_Type (T)
            then
               if Is_Universal_Operation (Name) then
                  exit;

               --  If node is an operator symbol, we have no actuals with
               --  which to check hiding, and this is done in full in the
               --  caller (Analyze_Subprogram_Renaming) so we include the
               --  predefined operator in any case.

               elsif Nkind (N) = N_Operator_Symbol
                 or else (Nkind (N) = N_Expanded_Name
                            and then
                          Nkind (Selector_Name (N)) = N_Operator_Symbol)
               then
                  exit;

               elsif not In_Open_Scopes (Scope (Name))
                 or else Scope_Depth (Scope (Name))
                   <= Scope_Depth (Scope (It.Nam))
               then
                  --  If ambiguity within instance, and entity is not an
                  --  implicit operation, save for later disambiguation.

                  if Scope (Name) = Scope (It.Nam)
                    and then not Is_Inherited_Operation (Name)
                    and then In_Instance
                  then
                     exit;
                  else
                     return;
                  end if;

               else
                  All_Interp.Table (Index).Nam := Name;
                  return;
               end if;

            --  Avoid making duplicate entries in overloads

            elsif Name = It.Nam
              and then Base_Type (It.Typ) = Base_Type (T)
            then
               return;

            --  Otherwise keep going

            else
               Get_Next_Interp (Index, It);
            end if;

         end loop;

         --  On exit, enter new interpretation. The context, or a preference
         --  rule, will resolve the ambiguity on the second pass.

         All_Interp.Table (All_Interp.Last) := (Name, Typ);
         All_Interp.Increment_Last;
         All_Interp.Table (All_Interp.Last) := No_Interp;

      end Add_Entry;

      ----------------------------
      -- Is_Universal_Operation --
      ----------------------------

      function Is_Universal_Operation (Op : Entity_Id) return Boolean is
         Arg : Node_Id;

      begin
         if Ekind (Op) /= E_Operator then
            return False;

         elsif Nkind (N) in N_Binary_Op then
            return Present (Universal_Interpretation (Left_Opnd (N)))
              and then Present (Universal_Interpretation (Right_Opnd (N)));

         elsif Nkind (N) in N_Unary_Op then
            return Present (Universal_Interpretation (Right_Opnd (N)));

         elsif Nkind (N) = N_Function_Call then
            Arg := First_Actual (N);

            while Present (Arg) loop

               if No (Universal_Interpretation (Arg)) then
                  return False;
               end if;

               Next_Actual (Arg);
            end loop;

            return True;

         else
            return False;
         end if;
      end Is_Universal_Operation;

   --  Start of processing for Add_One_Interp

   begin
      --  If the interpretation is a predefined operator, verify that the
      --  result type is visible, or that the entity has already been
      --  resolved (case of an instantiation node that refers to a predefined
      --  operation, or an internally generated operator node, or an operator
      --  given as an expanded name). If the operator is a comparison or
      --  equality, it is the type of the operand that matters to determine
      --  whether the operator is visible. In an instance, the check is not
      --  performed, given that the operator was visible in the generic.

      if Ekind (E) = E_Operator then

         Vis_Type := Base_Type (T);

         if Nkind (N) = N_Op_Eq
           or else Nkind (N) = N_Op_Ge
           or else Nkind (N) = N_Op_Gt
           or else Nkind (N) = N_Op_Le
           or else Nkind (N) = N_Op_Lt
           or else Nkind (N) = N_Op_Ne
         then
            Vis_Type := Base_Type (Etype (Left_Opnd (N)));
         end if;

         if In_Open_Scopes (Scope (Vis_Type))
           or else Is_Potentially_Use_Visible (Vis_Type)
           or else In_Use (Vis_Type)
           or else (In_Use (Scope (Vis_Type))
                     and then not Is_Hidden (Vis_Type))
           or else Nkind (N) = N_Expanded_Name
           or else (Nkind (N) in N_Op and then E = Entity (N))
           or else In_Instance
         then
            null;

         --  If the node is given in functional notation and the prefix
         --  is an expanded name, then the operator is visible if the
         --  prefix is the scope of the result type as well.

         elsif Nkind (N) = N_Function_Call
           and then Nkind (Name (N)) = N_Expanded_Name
           and then Entity (Prefix (Name (N))) = Scope (Base_Type (T))
         then
            null;

         --  Save type for subsequent error message, in case no other
         --  interpretation is found.

         else
            Candidate_Type := Vis_Type;
            return;
         end if;

      --  In an instance, an abstract non-dispatching operation cannot
      --  be a candidate interpretation, because it could not have been
      --  one in the generic (it may be a spurious overloading in the
      --  instance).

      elsif In_Instance
        and then Is_Abstract (E)
        and then not Is_Dispatching_Operation (E)
      then
         return;
      end if;

      --  If this is the first interpretation of N, N has type Any_Type.
      --  In that case place the new type on the node. If one interpretation
      --  already exists, indicate that the node is overloaded, and store
      --  both the previous and the new interpretation in All_Interp. If
      --  this is a later interpretation, just add it to the set.

      if Etype (N) = Any_Type then
         if Is_Type (E) then
            Set_Etype (N, T);

         else
            --  Record both the operator or subprogram name, and its type.

            if Nkind (N) in N_Op or else Is_Entity_Name (N) then
               Set_Entity (N, E);
            end if;

            Set_Etype (N, T);
         end if;

      --  Either there is no current interpretation in the table for any
      --  node or the interpretation that is present is for a different
      --  node. In both cases add a new interpretation to the table.

      elsif Interp_Map.Last < 0
        or else Interp_Map.Table (Interp_Map.Last).Node /= N
      then
         New_Interps (N);

         if (Nkind (N) in N_Op or else Is_Entity_Name (N))
           and then Present (Entity (N))
         then
            Add_Entry (Entity (N), Etype (N));

         elsif (Nkind (N) = N_Function_Call
                 or else Nkind (N) = N_Procedure_Call_Statement)
           and then (Nkind (Name (N)) = N_Operator_Symbol
                      or else Is_Entity_Name (Name (N)))
         then
            Add_Entry (Entity (Name (N)), Etype (N));

         else
            --  Overloaded prefix in indexed or selected component,
            --  or call whose name is an expresion or another call.

            Add_Entry (Etype (N), Etype (N));
         end if;

         Add_Entry (E, T);

      else
         Add_Entry (E, T);
      end if;
   end Add_One_Interp;

   ---------------------
   -- Collect_Interps --
   ---------------------

   procedure Collect_Interps (N : Node_Id) is
      Ent          : constant Entity_Id := Entity (N);
      H            : Entity_Id;
      First_Interp : Interp_Index;

   begin
      New_Interps (N);

      --  Unconditionally add the entity that was initially matched

      First_Interp := All_Interp.Last;
      Add_One_Interp (N, Ent, Etype (N));

      --  For expanded name, pick up all additional entities from the
      --  same scope, since these are obviously also visible. Note that
      --  these are not necessarily contiguous on the homonym chain.

      if Nkind (N) = N_Expanded_Name then
         H := Homonym (Ent);
         while Present (H) loop
            if Scope (H) = Scope (Entity (N)) then
               Add_One_Interp (N, H, Etype (H));
            end if;

            H := Homonym (H);
         end loop;

      --  Case of direct name

      else
         --  First, search the homonym chain for directly visible entities

         H := Current_Entity (Ent);
         while Present (H) loop
            exit when (not Is_Overloadable (H))
              and then Is_Immediately_Visible (H);

            if Is_Immediately_Visible (H)
              and then H /= Ent
            then
               --  Only add interpretation if not hidden by an inner
               --  immediately visible one.

               for J in First_Interp .. All_Interp.Last - 1 loop

                  --  Current homograph is not hidden. Add to overloads.

                  if not Is_Immediately_Visible (All_Interp.Table (J).Nam) then
                     exit;

                  --  Homograph is hidden, unless it is a predefined operator.

                  elsif Type_Conformant (H, All_Interp.Table (J).Nam) then

                     --  A homograph in the same scope can occur within an
                     --  instantiation, the resulting ambiguity has to be
                     --  resolved later.

                     if Scope (H) = Scope (Ent)
                        and then In_Instance
                        and then not Is_Inherited_Operation (H)
                     then
                        All_Interp.Table (All_Interp.Last) := (H, Etype (H));
                        All_Interp.Increment_Last;
                        All_Interp.Table (All_Interp.Last) := No_Interp;
                        goto Next_Homograph;

                     elsif Scope (H) /= Standard_Standard then
                        goto Next_Homograph;
                     end if;
                  end if;
               end loop;

               --  On exit, we know that current homograph is not hidden.

               Add_One_Interp (N, H, Etype (H));

               if Debug_Flag_E then
                  Write_Str ("Add overloaded Interpretation ");
                  Write_Int (Int (H));
                  Write_Eol;
               end if;
            end if;

            <<Next_Homograph>>
               H := Homonym (H);
         end loop;

         --  Scan list of homographs for use-visible entities only.

         H := Current_Entity (Ent);

         while Present (H) loop
            if Is_Potentially_Use_Visible (H)
              and then H /= Ent
              and then Is_Overloadable (H)
            then
               for J in First_Interp .. All_Interp.Last - 1 loop

                  if not Is_Immediately_Visible (All_Interp.Table (J).Nam) then
                     exit;

                  elsif Type_Conformant (H, All_Interp.Table (J).Nam) then
                     goto Next_Use_Homograph;
                  end if;
               end loop;

               Add_One_Interp (N, H, Etype (H));
            end if;

            <<Next_Use_Homograph>>
               H := Homonym (H);
         end loop;
      end if;

      if All_Interp.Last = First_Interp + 1 then

         --  The original interpretation is in fact not overloaded.

         Set_Is_Overloaded (N, False);
      end if;
   end Collect_Interps;

   ------------
   -- Covers --
   ------------

   function Covers (T1, T2 : Entity_Id) return Boolean is
      Typ1 : Entity_Id := T1;
      Typ2 : Entity_Id := T2;

   begin
      pragma Assert (Present (T1) and Present (T2));

      --  Simplest case: same types are compatible, and types that have the
      --  same base type and are not generic actuals are compatible. Generic
      --  actuals  belong to their class but are not compatible with other
      --  types of their class, and in particular with other generic actuals.
      --  They are however compatible with their own subtypes, and itypes
      --  with the same base are compatible as well. Similary, constrained
      --  subtypes obtained from expressions of an unconstrained nominal type
      --  are compatible with the base type (may lead to spurious ambiguities
      --  in obscure cases ???)

      --  Generic actuals require special treatment to avoid spurious ambi-
      --  guities in an instance, when two formal types are instantiated with
      --  the same actual, so that different subprograms end up with the same
      --  signature in the instance.

      if Typ1 = Typ2 then
         return True;

      elsif Base_Type (Typ1) = Base_Type (Typ2) then
         if not Is_Generic_Actual_Type (Typ1) then
            return True;
         else
            return (not Is_Generic_Actual_Type (Typ2)
                     or else Is_Itype (Typ1)
                     or else Is_Itype (Typ2)
                     or else Is_Constr_Subt_For_U_Nominal (Typ1)
                     or else Is_Constr_Subt_For_U_Nominal (Typ2)
                     or else Scope (Typ1) /= Scope (Typ2));
         end if;

      --  Literals are compatible with types in  a given "class"

      elsif (Typ2 = Universal_Integer and then Is_Integer_Type (Typ1))
        or else (Typ2 = Universal_Real    and then Is_Real_Type (Typ1))
        or else (Typ2 = Universal_Fixed   and then Is_Fixed_Point_Type (Typ1))
        or else (Typ2 = Any_Fixed         and then Is_Fixed_Point_Type (Typ1))
        or else (Typ2 = Any_String        and then Is_String_Type (Typ1))
        or else (Typ2 = Any_Character     and then Is_Character_Type (Typ1))
        or else (Typ2 = Any_Access        and then Is_Access_Type (Typ1))
      then
         return True;

      --  The context may be class wide.

      elsif Is_Class_Wide_Type (Typ1)
        and then Is_Ancestor (Root_Type (Typ1), Typ2)
      then
         return True;

      elsif Is_Class_Wide_Type (Typ1)
        and then Is_Class_Wide_Type (Typ2)
        and then Base_Type (Etype (Typ1)) = Base_Type (Etype (Typ2))
      then
         return True;

      --  In a dispatching call the actual may be class-wide

      elsif Is_Class_Wide_Type (Typ2)
        and then Base_Type (Root_Type (Typ2)) = Base_Type (Typ1)
      then
         return True;

      --  Some contexts require a class of types rather than a specific type

      elsif (Typ1 = Any_Integer and then Is_Integer_Type (Typ2))
        or else (Typ1 = Any_Boolean and then Is_Boolean_Type (Typ2))
        or else (Typ1 = Any_Real and then Is_Real_Type (Typ2))
        or else (Typ1 = Any_Fixed and then Is_Fixed_Point_Type (Typ2))
        or else (Typ1 = Any_Discrete and then Is_Discrete_Type (Typ2))
      then
         return True;

      --  An aggregate is compatible with an array or record type

      elsif Typ2 = Any_Composite
        and then Ekind (Typ1) in E_Array_Type .. E_Record_Subtype
      then
         return True;

      --  If the expected type is an anonymous access, the designated
      --  type must cover that of the expression.

      elsif Ekind (Typ1) = E_Anonymous_Access_Type
        and then Is_Access_Type (Typ2)
        and then Covers (Designated_Type (Typ1), Designated_Type (Typ2))
      then
         return True;

      --  An Access_To_Subprogram is compatible with itself, or with an
      --  anonymous type created for an attribute reference Access.

      elsif (Ekind (Base_Type (Typ1)) = E_Access_Subprogram_Type
               or else
             Ekind (Base_Type (Typ1)) = E_Access_Protected_Subprogram_Type)
        and then Is_Access_Type (Typ2)
        and then (not Comes_From_Source (Typ1)
                   or else not Comes_From_Source (Typ2))
        and then (Is_Overloadable (Designated_Type (Typ2))
                    or else
                  Ekind (Designated_Type (Typ2)) = E_Subprogram_Type)
        and then
          Type_Conformant (Designated_Type (Typ1), Designated_Type (Typ2))
        and then
          Mode_Conformant (Designated_Type (Typ1), Designated_Type (Typ2))
      then
         return True;

      elsif Is_Record_Type (Typ1)
        and then (Is_Remote_Call_Interface (Typ1)
                   or else Is_Remote_Types (Typ1))
        and then Present (Corresponding_Remote_Type (Typ1))
      then
         return Covers (Corresponding_Remote_Type (Typ1), Typ2);

      elsif Ekind (Base_Type (Typ1)) = E_General_Access_Type
        and then Ekind (Typ2) = E_Access_Attribute_Type
        and then Covers (Designated_Type (Typ1), Designated_Type (Typ2))
      then
         --  If the target type is a RACW type while the source is an access
         --  attribute type, we are building a RACW that may be exported.

         if Is_Remote_Access_To_Class_Wide_Type (Base_Type (Typ1)) then
            Set_Has_RACW (Current_Sem_Unit);
         end if;

         return True;

      elsif Ekind (Typ2) = E_Allocator_Type
        and then Is_Access_Type (Typ1)
        and then Covers (Designated_Type (Typ1), Designated_Type (Typ2))
      then
         return True;

      --  A boolean operation on integer literals is compatible with a
      --  modular context.

      elsif Typ2 = Any_Modular
        and then Is_Modular_Integer_Type (Typ1)
      then
         return True;

      --  The actual type may be the result of a previous error

      elsif Base_Type (Typ2) = Any_Type then
         return True;

      --  A packed array type covers its corresponding non-packed type.
      --  This is not legitimate Ada, but allows the omission of a number
      --  of otherwise useless unchecked conversions, and since this can
      --  only arise in (known correct) expanded code, no harm is done

      elsif Is_Array_Type (T2)
        and then Is_Packed (T2)
        and then T1 = Packed_Array_Type (T2)
      then
         return True;

      --  Similarly an array type covers its corresponding packed array type

      elsif Is_Array_Type (T1)
        and then Is_Packed (T1)
        and then T2 = Packed_Array_Type (T1)
      then
         return True;

      --  In an instance the proper view may not always be correct for
      --  private types, but private and full view are compatible. This
      --  removes spurious errors from nested instantiations that involve,
      --  among other things, types derived from privated types.

      elsif In_Instance
        and then Is_Private_Type (T1)
        and then ((Present (Full_View (T1))
                    and then Covers (Full_View (T1), T2))
          or else Base_Type (T1) = T2
          or else Base_Type (T2) = T1)
      then
         return True;

      --  Otherwise it doesn't cover!

      else
         return False;
      end if;
   end Covers;

   ------------------
   -- Disambiguate --
   ------------------

   function Disambiguate
     (N      : Node_Id;
      I1, I2 : Interp_Index;
      Typ    : Entity_Id)
      return   Interp
   is
      I           : Interp_Index;
      It          : Interp;
      It1, It2    : Interp;
      Nam1, Nam2  : Entity_Id;
      Predef_Subp : Entity_Id;
      User_Subp   : Entity_Id;

      function Matches (Actual, Formal : Node_Id) return Boolean;
      --  Look for exact type match in an instance, to remove spurious
      --  ambiguities when two formal types have the same actual.

      function Standard_Operator return Boolean;

      function Remove_Conversions return Interp;
      --  Last chance for pathological cases involving comparisons on
      --  literals, and user overloadings of the same operator. Such
      --  pathologies have been removed from the ACVC, but still appear in
      --  two DEC tests, with the following notable quote from Ben Brosgol:
      --
      --  [Note: I disclaim all credit/responsibility/blame for coming up with
      --  this example;  Robert Dewar brought it to our attention, since it
      --  is apparently found in the ACVC 1.5. I did not attempt to find
      --  the reason in the Reference Manual that makes the example legal,
      --  since I was too nauseated by it to want to pursue it further.]
      --
      --  Accordingly, this is not a fully recursive solution, but it handles
      --  DEC tests c460vsa, c460vsb. It also handles ai00136a, which pushes
      --  pathology in the other direction with calls whose multiple overloaded
      --  actuals make them truly unresolvable.

      -------------
      -- Matches --
      -------------

      function Matches (Actual, Formal : Node_Id) return Boolean is
         T1 : constant Entity_Id := Etype (Actual);
         T2 : constant Entity_Id := Etype (Formal);

      begin
         return T1 = T2
           or else
             (Is_Numeric_Type (T2)
               and then
             (T1 = Universal_Real or else T1 = Universal_Integer));
      end Matches;

      -----------------------
      -- Standard_Operator --
      -----------------------

      function Standard_Operator return Boolean is
         Nam : Node_Id;

      begin
         if Nkind (N) in N_Op then
            return True;

         elsif Nkind (N) = N_Function_Call then
            Nam := Name (N);

            if Nkind (Nam) /= N_Expanded_Name then
               return True;
            else
               return Entity (Prefix (Nam)) = Standard_Standard;
            end if;
         else
            return False;
         end if;
      end Standard_Operator;

      ------------------------
      -- Remove_Conversions --
      ------------------------

      function Remove_Conversions return Interp is
         I    : Interp_Index;
         It   : Interp;
         It1  : Interp;
         F1   : Entity_Id;
         Act1 : Node_Id;
         Act2 : Node_Id;

      begin
         It1   := No_Interp;
         Get_First_Interp (N, I, It);

         while Present (It.Typ) loop

            if not Is_Overloadable (It.Nam) then
               return No_Interp;
            end if;

            F1 := First_Formal (It.Nam);

            if No (F1) then
               return It1;

            else
               if Nkind (N) = N_Function_Call
                 or else Nkind (N) = N_Procedure_Call_Statement
               then
                  Act1 := First_Actual (N);

                  if Present (Act1) then
                     Act2 := Next_Actual (Act1);
                  else
                     Act2 := Empty;
                  end if;

               elsif Nkind (N) in N_Unary_Op then
                  Act1 := Right_Opnd (N);
                  Act2 := Empty;

               elsif Nkind (N) in N_Binary_Op then
                  Act1 := Left_Opnd (N);
                  Act2 := Right_Opnd (N);

               else
                  return It1;
               end if;

               if Nkind (Act1) in N_Op
                 and then Is_Overloaded (Act1)
                 and then (Nkind (Right_Opnd (Act1)) = N_Integer_Literal
                            or else Nkind (Right_Opnd (Act1)) = N_Real_Literal)
                 and then Has_Compatible_Type (Act1, Standard_Boolean)
                 and then Etype (F1) = Standard_Boolean
               then

                  if It1 /= No_Interp then
                     return No_Interp;

                  elsif Present (Act2)
                    and then Nkind (Act2) in N_Op
                    and then Is_Overloaded (Act2)
                    and then (Nkind (Right_Opnd (Act1)) = N_Integer_Literal
                                or else
                              Nkind (Right_Opnd (Act1)) = N_Real_Literal)
                    and then Has_Compatible_Type (Act2, Standard_Boolean)
                  then
                     --  The preference rule on the first actual is not
                     --  sufficient to disambiguate.

                     goto Next_Interp;

                  else
                     It1 := It;
                  end if;
               end if;
            end if;

            <<Next_Interp>>
               Get_Next_Interp (I, It);
         end loop;

         if Errors_Detected > 0 then

            --  After some error, a formal may have Any_Type and yield
            --  a spurious match. To avoid cascaded errors if possible,
            --  check for such a formal in either candidate.

            declare
               Formal : Entity_Id;

            begin
               Formal := First_Formal (Nam1);
               while Present (Formal) loop
                  if Etype (Formal) = Any_Type then
                     return Disambiguate.It2;
                  end if;

                  Next_Formal (Formal);
               end loop;

               Formal := First_Formal (Nam2);
               while Present (Formal) loop
                  if Etype (Formal) = Any_Type then
                     return Disambiguate.It1;
                  end if;

                  Next_Formal (Formal);
               end loop;
            end;
         end if;

         return It1;
      end Remove_Conversions;

   --  Start of processing for Disambiguate

   begin
      --  Recover the two legal interpretations.

      Get_First_Interp (N, I, It);

      while I /= I1 loop
         Get_Next_Interp (I, It);
      end loop;

      It1  := It;
      Nam1 := It.Nam;

      while I /= I2 loop
         Get_Next_Interp (I, It);
      end loop;

      It2  := It;
      Nam2 := It.Nam;

      --  If the context is universal, the predefined operator is preferred.
      --  This includes bounds in numeric type declarations, and expressions
      --  in type conversions. If no interpretation yields a universal type,
      --  then we must check whether the user-defined entity hides the prede-
      --  fined one.

      if Chars (Nam1) in  Any_Operator_Name
        and then Standard_Operator
      then
         if        Typ = Universal_Integer
           or else Typ = Universal_Real
           or else Typ = Any_Integer
           or else Typ = Any_Discrete
           or else Typ = Any_Real
           or else Typ = Any_Type
         then
            --  Find an interpretation that yields the universal type, or else
            --  a predefined operator that yields a predefined numeric type.

            declare
               Candidate : Interp := No_Interp;
            begin
               Get_First_Interp (N, I, It);

               while Present (It.Typ) loop
                  if (Covers (Typ, It.Typ)
                       or else Typ = Any_Type)
                    and then
                     (It.Typ = Universal_Integer
                       or else It.Typ = Universal_Real)
                  then
                     return It;

                  elsif Covers (Typ, It.Typ)
                    and then Scope (It.Typ) = Standard_Standard
                    and then Scope (It.Nam) = Standard_Standard
                    and then Is_Numeric_Type (It.Typ)
                  then
                     Candidate := It;
                  end if;

                  Get_Next_Interp (I, It);
               end loop;

               if Candidate /= No_Interp then
                  return Candidate;
               end if;
            end;

         elsif Chars (Nam1) /= Name_Op_Not
           and then (Typ = Standard_Boolean
             or else Typ = Any_Boolean)
         then
            --  Equality or comparison operation. Choose predefined operator
            --  if arguments are universal. The node may be an operator, a
            --  name, or a function call, so unpack arguments accordingly.

            declare
               Arg1, Arg2 : Node_Id;

            begin
               if Nkind (N) in N_Op then
                  Arg1 := Left_Opnd  (N);
                  Arg2 := Right_Opnd (N);

               elsif Is_Entity_Name (N)
                 or else Nkind (N) = N_Operator_Symbol
               then
                  Arg1 := First_Entity (Entity (N));
                  Arg2 := Next_Entity (Arg1);

               else
                  Arg1 := First_Actual (N);
                  Arg2 := Next_Actual (Arg1);
               end if;

               if Present (Arg2)
                 and then Present (Universal_Interpretation (Arg1))
                 and then Universal_Interpretation (Arg2) =
                   Universal_Interpretation (Arg1)
               then
                  Get_First_Interp (N, I, It);

                  while Scope (It.Nam) /= Standard_Standard loop
                     Get_Next_Interp (I, It);
                  end loop;

                  return It;
               end if;
            end;
         end if;
      end if;

      --  If no universal interpretation, check whether user-defined operator
      --   hides predefined one, as well as other special cases.
      --  If the node is a range, then one or both bounds are ambiguous. Each
      --  will have to be disambiguated w.r.t. the context type. The type of
      --  the range itself is imposed by the context, so we can return either
      --  legal interpretation.

      if Ekind (Nam1) = E_Operator then
         Predef_Subp := Nam1;
         User_Subp   := Nam2;

      elsif Ekind (Nam2) = E_Operator then
         Predef_Subp := Nam2;
         User_Subp   := Nam1;

      elsif Nkind (N) = N_Range then
         return It1;

      --  If two user defined-subprograms are visible, it is a true ambiguity,
      --  unless one of them is an entry and the context is a conditional or
      --  timed entry call, or unless we are within an instance and this is
      --  results from two formals types with the same actual.

      else

         if Nkind (N) = N_Procedure_Call_Statement
           and then Nkind (Parent (N)) = N_Entry_Call_Alternative
           and then N = Entry_Call_Statement (Parent (N))
         then
            if Ekind (Nam2) = E_Entry then
               return It2;
            elsif Ekind (Nam1) = E_Entry then
               return It1;
            else
               return No_Interp;
            end if;

         --  If the ambiguity occurs within an instance, it is due to several
         --  formal types with the same actual. Look for an exact match
         --  between the types of the formals of the overloadable entities,
         --  and the actuals in the call, to recover the unambiguous match
         --  in the original generic.

         elsif In_Instance then
            if (Nkind (N) = N_Function_Call
              or else Nkind (N) = N_Procedure_Call_Statement)
            then
               declare
                  Actual : Node_Id;
                  Formal : Entity_Id;

               begin
                  Actual := First_Actual (N);
                  Formal := First_Formal (Nam1);

                  while Present (Actual) loop
                     if Etype (Actual) /= Etype (Formal) then
                        return It2;
                     end if;

                     Next_Actual (Actual);
                     Next_Formal (Formal);
                  end loop;

                  return It1;
               end;

            elsif Nkind (N) in N_Binary_Op then

               if Matches (Left_Opnd (N), First_Formal (Nam1))
                 and then
                   Matches (Right_Opnd (N), Next_Formal (First_Formal (Nam1)))
               then
                  return It1;
               else
                  return It2;
               end if;

            elsif Nkind (N) in  N_Unary_Op then

               if Etype (Right_Opnd (N)) = Etype (First_Formal (Nam1)) then
                  return It1;
               else
                  return It2;
               end if;

            else
               return Remove_Conversions;
            end if;
         else
            return Remove_Conversions;
         end if;
      end if;

      --  an implicit concatenation operator on a string type cannot be
      --  disambiguated from the predefined concatenation. This can only
      --  happen with concatenation of string literals.

      if Chars (User_Subp) = Name_Op_Concat
        and then Ekind (User_Subp) = E_Operator
        and then Is_String_Type (Etype (First_Formal (User_Subp)))
      then
         return No_Interp;

      --  If the user-defined operator is in  an open scope, or in the scope
      --  of the resulting type, or given by an expanded name that names its
      --  scope, it hides the predefined operator for the type. Exponentiation
      --  has to be special-cased because the implicit operator does not have
      --  a symmetric signature, and may not be hidden by the explicit one.

      elsif (Nkind (N) = N_Function_Call
              and then Nkind (Name (N)) = N_Expanded_Name
              and then (Chars (Predef_Subp) /= Name_Op_Expon
                          or else Hides_Op (User_Subp, Predef_Subp))
              and then Scope (User_Subp) = Entity (Prefix (Name (N))))
        or else Hides_Op (User_Subp, Predef_Subp)
      then
         if It1.Nam = User_Subp then
            return It1;
         else
            return It2;
         end if;

      --  Otherwise, the predefined operator has precedence, or if the
      --  user-defined operation is directly visible we have a true ambiguity.
      --  If this is a fixed-point multiplication and division in Ada83 mode,
      --  exclude the universal_fixed operator, which often causes ambiguities
      --  in legacy code.

      else
         if (In_Open_Scopes (Scope (User_Subp))
           or else Is_Potentially_Use_Visible (User_Subp))
           and then not In_Instance
         then
            if Is_Fixed_Point_Type (Typ)
              and then (Chars (Nam1) = Name_Op_Multiply
                         or else Chars (Nam1) = Name_Op_Divide)
              and then Ada_83
            then
               if It2.Nam = Predef_Subp then
                  return It1;

               else
                  return It2;
               end if;
            else
               return No_Interp;
            end if;

         elsif It1.Nam = Predef_Subp then
            return It1;

         else
            return It2;
         end if;
      end if;

   end Disambiguate;

   ---------------------
   -- End_Interp_List --
   ---------------------

   procedure End_Interp_List is
   begin
      All_Interp.Table (All_Interp.Last) := No_Interp;
      All_Interp.Increment_Last;
   end End_Interp_List;

   -------------------------
   -- Entity_Matches_Spec --
   -------------------------

   function Entity_Matches_Spec (Old_S, New_S : Entity_Id) return Boolean is
   begin
      --  Simple case: same entity kinds, type conformance is required.
      --  A parameterless function can also rename a literal.

      if Ekind (Old_S) = Ekind (New_S)
        or else (Ekind (New_S) = E_Function
                  and then Ekind (Old_S) = E_Enumeration_Literal)
      then
         return Type_Conformant (New_S, Old_S);

      elsif Ekind (New_S) = E_Function
        and then Ekind (Old_S) = E_Operator
      then
         return Operator_Matches_Spec (Old_S, New_S);

      elsif Ekind (New_S) = E_Procedure
        and then Is_Entry (Old_S)
      then
         return Type_Conformant (New_S, Old_S);

      else
         return False;
      end if;
   end Entity_Matches_Spec;

   ----------------------
   -- Find_Unique_Type --
   ----------------------

   function Find_Unique_Type (L : Node_Id; R : Node_Id) return Entity_Id is
      I  : Interp_Index;
      It : Interp;
      T  : Entity_Id := Etype (L);
      TR : Entity_Id := Any_Type;

   begin
      if Is_Overloaded (R) then
         Get_First_Interp (R, I, It);

         while Present (It.Typ) loop
            if Covers (T, It.Typ) or else Covers (It.Typ, T) then

               --  If several interpretations are possible and L is universal,
               --  apply preference rule.

               if TR /= Any_Type then

                  if (T = Universal_Integer or else T = Universal_Real)
                    and then It.Typ = T
                  then
                     TR := It.Typ;
                  end if;

               else
                  TR := It.Typ;
               end if;
            end if;

            Get_Next_Interp (I, It);
         end loop;

         Set_Etype (R, TR);

      --  In the non-overloaded case, the Etype of R is already set
      --  correctly.

      else
         null;
      end if;

      --  If one of the operands is Universal_Fixed, the type of the
      --  other operand provides the context.

      if Etype (R) = Universal_Fixed then
         return T;

      elsif T = Universal_Fixed then
         return Etype (R);

      else
         return Specific_Type (T, Etype (R));
      end if;

   end Find_Unique_Type;

   ----------------------
   -- Get_First_Interp --
   ----------------------

   procedure Get_First_Interp
     (N  : Node_Id;
      I  : out Interp_Index;
      It : out Interp)
   is
      Int_Ind : Interp_Index;
      O_N     : Node_Id;

   begin
      --  If a selected component is overloaded because the selector has
      --  multiple interpretations, the node is a call to a protected
      --  operation or an indirect call. Retrieve the interpretation from
      --  the selector name. The selected component may be overloaded as well
      --  if the prefix is overloaded. That case is unchanged.

      if Nkind (N) = N_Selected_Component
        and then Is_Overloaded (Selector_Name (N))
      then
         O_N := Selector_Name (N);
      else
         O_N := N;
      end if;

      for Index in 0 .. Interp_Map.Last loop
         if Interp_Map.Table (Index).Node = O_N then
            Int_Ind := Interp_Map.Table (Index).Index;
            It := All_Interp.Table (Int_Ind);
            I := Int_Ind;
            return;
         end if;
      end loop;

      --  Procedure should never be called if the node has no interpretations

      pragma Assert (False);
      raise Program_Error;
   end Get_First_Interp;

   ----------------------
   --  Get_Next_Interp --
   ----------------------

   procedure Get_Next_Interp (I : in out Interp_Index; It : out Interp) is
   begin
      I  := I + 1;
      It := All_Interp.Table (I);
   end Get_Next_Interp;

   -------------------------
   -- Has_Compatible_Type --
   -------------------------

   function Has_Compatible_Type
     (N    : Node_Id;
      Typ  : Entity_Id)
      return Boolean
   is
      I  : Interp_Index;
      It : Interp;

   begin
      if Nkind (N) = N_Subtype_Indication or else not Is_Overloaded (N) then
         return Covers (Typ, Etype (N))
          or else (not Is_Tagged_Type (Typ)
                    and then Ekind (Typ) /= E_Anonymous_Access_Type
                    and then Covers (Etype (N), Typ));

      else
         Get_First_Interp (N, I, It);

         while Present (It.Typ) loop
            if Covers (Typ, It.Typ)
              or else (not Is_Tagged_Type (Typ)
                        and then Ekind (Typ) /= E_Anonymous_Access_Type
                        and then Covers (It.Typ, Typ))
            then
               return True;
            end if;

            Get_Next_Interp (I, It);
         end loop;

         return False;
      end if;
   end Has_Compatible_Type;

   --------------
   -- Hides_Op --
   --------------

   function Hides_Op (F : Entity_Id; Op : Entity_Id) return Boolean is
      Btyp : constant Entity_Id := Base_Type (Etype (First_Formal (F)));

   begin
      return Operator_Matches_Spec (Op, F)
        and then (In_Open_Scopes (Scope (F))
                    or else Scope (F) = Scope (Btyp)
                    or else (not In_Open_Scopes (Scope (Btyp))
                              and then not In_Use (Btyp)
                              and then not In_Use (Scope (Btyp))));
   end Hides_Op;

   -------------------
   -- Specific_Type --
   -------------------

   function Specific_Type (T1, T2 : Entity_Id) return Entity_Id is
      B1 : constant Entity_Id := Base_Type (T1);
      B2 : constant Entity_Id := Base_Type (T2);

      function Is_Remote_Access (T : Entity_Id) return Boolean;
      --  Check whether T is the equivalent type of a remote access type.
      --  If distribution is enabled, T is a legal context for Null.

      function Is_Remote_Access (T : Entity_Id) return Boolean is
      begin
         return Is_Record_Type (T)
           and then (Is_Remote_Call_Interface (T)
                      or else Is_Remote_Types (T))
           and then Present (Corresponding_Remote_Type (T))
           and then Is_Access_Type (Corresponding_Remote_Type (T));
      end Is_Remote_Access;

   --  Start of processing for Specific_Type

   begin
      if (T1 = Any_Type or else T2 = Any_Type) then
         return Any_Type;
      end if;

      if B1 = B2 then
         return B1;

      elsif (T1 = Universal_Integer  and then Is_Integer_Type (T2))
        or else (T1 = Universal_Real and then Is_Real_Type (T2))
        or else (T1 = Any_Fixed      and then Is_Fixed_Point_Type (T2))
      then
         return B2;

      elsif (T2 = Universal_Integer  and then Is_Integer_Type (T1))
        or else (T2 = Universal_Real and then Is_Real_Type (T1))
        or else (T2 = Any_Fixed      and then Is_Fixed_Point_Type (T1))
      then
         return B1;

      elsif (T2 = Any_String and then Is_String_Type (T1)) then
         return B1;

      elsif (T1 = Any_String and then Is_String_Type (T2)) then
         return B2;

      elsif (T2 = Any_Character and then Is_Character_Type (T1)) then
         return B1;

      elsif (T1 = Any_Character and then Is_Character_Type (T2)) then
         return B2;

      elsif (T1 = Any_Access
        and then (Is_Access_Type (T2) or else Is_Remote_Access (T2)))
      then
         return T2;

      elsif (T2 = Any_Access
        and then (Is_Access_Type (T1) or else Is_Remote_Access (T1)))
      then
         return T1;

      elsif (T2 = Any_Composite
         and then Ekind (T1) in E_Array_Type .. E_Record_Subtype)
      then
         return T1;

      elsif (T1 = Any_Composite
         and then Ekind (T2) in E_Array_Type .. E_Record_Subtype)
      then
         return T2;

      elsif (T1 = Any_Modular and then Is_Modular_Integer_Type (T2)) then
         return T2;

      elsif (T2 = Any_Modular and then Is_Modular_Integer_Type (T1)) then
         return T1;

      --  Special cases for equality operators (all other predefined
      --  operators can never apply to tagged types)

      elsif Is_Class_Wide_Type (T1)
        and then Is_Ancestor (Root_Type (T1), T2)
      then
         return T1;

      elsif Is_Class_Wide_Type (T2)
        and then Is_Ancestor (Root_Type (T2), T1)
      then
         return T2;

      elsif (Ekind (B1) = E_Access_Subprogram_Type
               or else
             Ekind (B1) = E_Access_Protected_Subprogram_Type)
        and then Ekind (Designated_Type (B1)) /= E_Subprogram_Type
        and then Is_Access_Type (T2)
      then
         return T2;

      elsif (Ekind (B2) = E_Access_Subprogram_Type
               or else
             Ekind (B2) = E_Access_Protected_Subprogram_Type)
        and then Ekind (Designated_Type (B2)) /= E_Subprogram_Type
        and then Is_Access_Type (T1)
      then
         return T1;

      elsif (Ekind (T1) = E_Allocator_Type
              or else Ekind (T1) = E_Access_Attribute_Type
              or else Ekind (T1) = E_Anonymous_Access_Type)
        and then Is_Access_Type (T2)
      then
         return T2;

      elsif (Ekind (T2) = E_Allocator_Type
              or else Ekind (T2) = E_Access_Attribute_Type
              or else Ekind (T2) = E_Anonymous_Access_Type)
        and then Is_Access_Type (T1)
      then
         return T1;

      --  If none of the above cases applies, types are not compatible.

      else
         return Any_Type;
      end if;
   end Specific_Type;

   ------------------------------
   -- Universal_Interpretation --
   ------------------------------

   function Universal_Interpretation (Opnd : Node_Id) return Entity_Id is
      Index : Interp_Index;
      It    : Interp;

   begin
      --  The argument may be a formal parameter of an operator or subprogram
      --  with multiple interpretations, or else an expression for an actual.

      if Nkind (Opnd) = N_Defining_Identifier
        or else not Is_Overloaded (Opnd)
      then
         if Etype (Opnd) = Universal_Integer
           or else Etype (Opnd) = Universal_Real
         then
            return Etype (Opnd);
         else
            return Empty;
         end if;

      else
         Get_First_Interp (Opnd, Index, It);

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

   ---------------------
   -- Intersect_Types --
   ---------------------

   function Intersect_Types (L, R : Node_Id) return Entity_Id is
      Index : Interp_Index;
      It    : Interp;
      Typ   : Entity_Id;

      function Check_Right_Argument (T : Entity_Id) return Entity_Id;
      --  Find interpretation of right arg that has type compatible with T

      function Check_Right_Argument (T : Entity_Id) return Entity_Id is
         Index : Interp_Index;
         It    : Interp;
         T2    : Entity_Id;

      begin
         if not Is_Overloaded (R) then
            return Specific_Type (T, Etype (R));

         else
            Get_First_Interp (R, Index, It);

            loop
               T2 := Specific_Type (T, It.Typ);

               if T2 /= Any_Type then
                  return T2;
               end if;

               Get_Next_Interp (Index, It);
               exit when No (It.Typ);
            end loop;

            return Any_Type;
         end if;
      end Check_Right_Argument;

   --  Start processing for Intersect_Types

   begin
      if Etype (L) = Any_Type or else Etype (R) = Any_Type then
         return Any_Type;
      end if;

      if not Is_Overloaded (L) then
         Typ := Check_Right_Argument (Etype (L));

      else
         Typ := Any_Type;
         Get_First_Interp (L, Index, It);

         while Present (It.Typ) loop
            Typ := Check_Right_Argument (It.Typ);
            exit when Typ /= Any_Type;
            Get_Next_Interp (Index, It);
         end loop;

      end if;

      --  If Typ is Any_Type, it means no compatible pair of types was found

      if Typ = Any_Type then

         if Nkind (Parent (L)) in N_Op then
            Error_Msg_N ("incompatible types for operator", Parent (L));

         elsif Nkind (Parent (L)) = N_Range then
            Error_Msg_N ("incompatible types given in constraint", Parent (L));

         else
            Error_Msg_N ("incompatible types", Parent (L));
         end if;
      end if;

      return Typ;
   end Intersect_Types;

   -----------------
   -- Is_Ancestor --
   -----------------

   function Is_Ancestor (T1, T2 : Entity_Id) return Boolean is
      Par : Entity_Id;

   begin
      if T1 = T2 then
         return True;

      elsif Is_Private_Type (T1)
        and then Present (Full_View (T1))
        and then Base_Type (T2) = Base_Type (Full_View (T1))
      then
         return True;

      else
         Par := Etype (T2);

         loop
            if T1 = Par
              or else (Is_Private_Type (T1)
                        and then Par = Full_View (T1))
            then
               return True;

            elsif Etype (Par) /= Par then
               Par := Etype (Par);
            else
               return False;
            end if;
         end loop;
      end if;
   end Is_Ancestor;

   -------------------
   -- Is_Subtype_Of --
   -------------------

   function Is_Subtype_Of (T1 : Entity_Id; T2 : Entity_Id) return Boolean is
      S : Entity_Id;

   begin
      S := Ancestor_Subtype (T1);
      while Present (S) loop
         if S = T2 then
            return True;
         else
            S := Ancestor_Subtype (S);
         end if;
      end loop;

      return False;
   end Is_Subtype_Of;

   -----------------
   -- New_Interps --
   -----------------

   procedure New_Interps (N : Node_Id)  is
   begin
      Interp_Map.Increment_Last;
      All_Interp.Increment_Last;
      Interp_Map.Table (Interp_Map.Last) := (N, All_Interp.Last);
      All_Interp.Table (All_Interp.Last) := No_Interp;
      Set_Is_Overloaded (N, True);
   end New_Interps;

   ---------------------------
   -- Operator_Matches_Spec --
   ---------------------------

   function Operator_Matches_Spec (Op, New_S : Entity_Id) return Boolean is
      Op_Name : constant Name_Id   := Chars (Op);
      T       : constant Entity_Id := Etype (New_S);
      New_F   : Entity_Id;
      Old_F   : Entity_Id;
      Num     : Int;
      T1      : Entity_Id;
      T2      : Entity_Id;

   begin
      --  To verify that a predefined operator matches a given signature,
      --  do a case analysis of the operator classes. Function can have one
      --  or two formals and must have the proper result type.

      New_F := First_Formal (New_S);
      Old_F := First_Formal (Op);
      Num := 0;

      while Present (New_F) and then Present (Old_F) loop
         Num := Num + 1;
         Next_Formal (New_F);
         Next_Formal (Old_F);
      end loop;

      --  Definite mismatch if different number of parameters

      if Present (Old_F) or else Present (New_F) then
         return False;

      --  Unary operators

      elsif Num = 1 then
         T1 := Etype (First_Formal (New_S));

         if Op_Name = Name_Op_Subtract
           or else Op_Name = Name_Op_Add
           or else Op_Name = Name_Op_Abs
         then
            return Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T);

         elsif Op_Name = Name_Op_Not then
            return Base_Type (T1) = Base_Type (T)
              and then Valid_Boolean_Arg (Base_Type (T));

         else
            return False;
         end if;

      --  Binary operators

      else
         T1 := Etype (First_Formal (New_S));
         T2 := Etype (Next_Formal (First_Formal (New_S)));

         if Op_Name =  Name_Op_And or else Op_Name = Name_Op_Or
           or else Op_Name = Name_Op_Xor
         then
            return Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Valid_Boolean_Arg (Base_Type (T));

         elsif Op_Name = Name_Op_Eq or else Op_Name = Name_Op_Ne then
            return Base_Type (T1) = Base_Type (T2)
              and then not Is_Limited_Type (T1)
              and then Is_Boolean_Type (T);

         elsif Op_Name = Name_Op_Lt or else Op_Name = Name_Op_Le
           or else Op_Name = Name_Op_Gt or else Op_Name = Name_Op_Ge
         then
            return Base_Type (T1) = Base_Type (T2)
              and then Valid_Comparison_Arg (T1)
              and then Is_Boolean_Type (T);

         elsif Op_Name = Name_Op_Add or else Op_Name = Name_Op_Subtract then
            return Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T);

         --  for division and multiplication, a user-defined function does
         --  not match the predefined universal_fixed operation, except in
         --  Ada83 mode.

         elsif Op_Name = Name_Op_Divide then
            return (Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T)
              and then (not Is_Fixed_Point_Type (T)
                         or else Ada_83))

            --  Mixed_Mode operations on fixed-point types.

              or else (Base_Type (T1) = Base_Type (T)
                and then Base_Type (T2) = Base_Type (Standard_Integer)
                and then Is_Fixed_Point_Type (T))

            --  A user defined operator can also match (and hide) a mixed
            --  operation on universal literals.

              or else (Is_Integer_Type (T2)
                 and then Is_Floating_Point_Type (T1)
                 and then Base_Type (T1) = Base_Type (T));

         elsif Op_Name = Name_Op_Multiply then
            return (Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T)
              and then (not Is_Fixed_Point_Type (T)
                         or else Ada_83))

            --  Mixed_Mode operations on fixed-point types.

              or else (Base_Type (T1) = Base_Type (T)
                and then Base_Type (T2) = Base_Type (Standard_Integer)
                and then Is_Fixed_Point_Type (T))

              or else (Base_Type (T2) = Base_Type (T)
                and then Base_Type (T1) = Base_Type (Standard_Integer)
                and then Is_Fixed_Point_Type (T))

              or else (Is_Integer_Type (T2)
                 and then Is_Floating_Point_Type (T1)
                 and then Base_Type (T1) = Base_Type (T))

              or else (Is_Integer_Type (T1)
                 and then Is_Floating_Point_Type (T2)
                 and then Base_Type (T2) = Base_Type (T));

         elsif Op_Name = Name_Op_Mod or else Op_Name = Name_Op_Rem then
            return Base_Type (T1) = Base_Type (T2)
              and then Base_Type (T1) = Base_Type (T)
              and then Is_Integer_Type (T);

         elsif Op_Name = Name_Op_Expon then
            return Base_Type (T1) = Base_Type (T)
              and then Is_Numeric_Type (T)
              and then Base_Type (T2) = Base_Type (Standard_Integer);

         elsif Op_Name = Name_Op_Concat then
            return Is_Array_Type (T)
              and then (Base_Type (T) = Base_Type (Etype (Op)))
              and then (Base_Type (T1) = Base_Type (T)
               or else (Base_Type (T1) = Base_Type (Component_Type (T))))
              and then (Base_Type (T2) = Base_Type (T)
               or else (Base_Type (T2) = Base_Type (Component_Type (T))));

         else
            return False;
         end if;
      end if;
   end Operator_Matches_Spec;

   -------------------
   -- Remove_Interp --
   -------------------

   procedure Remove_Interp (I : in out Interp_Index) is
      II : Interp_Index;

   begin
      --  Find end of Interp list and copy downward to erase the discarded one

      II := I + 1;

      while Present (All_Interp.Table (II).Typ) loop
         II := II + 1;
      end loop;

      for J in I + 1 .. II loop
         All_Interp.Table (J - 1) := All_Interp.Table (J);
      end loop;

      --  Back up interp. index to insure that iterator will pick up next
      --  available interpretation.

      I := I - 1;
   end Remove_Interp;

   ------------------
   -- Save_Interps --
   ------------------

   procedure Save_Interps (Old_N : Node_Id; New_N : Node_Id) is
   begin
      if Is_Overloaded (Old_N) then
         for Index in 0 .. Interp_Map.Last loop
            if Interp_Map.Table (Index).Node = Old_N then
               Interp_Map.Table (Index).Node := New_N;
               exit;
            end if;
         end loop;
      end if;
   end Save_Interps;

   -----------------------
   -- Valid_Boolean_Arg --
   -----------------------

   function Valid_Boolean_Arg (T : Entity_Id) return Boolean is

      --  In addition to booleans and arrays of booleans,  we must include
      --  aggregates as valid boolean arguments, because in the first pass
      --  of resolution their components are not examined. If it turns out not
      --  to be an aggregate of booleans, this will be diagnosed in Resolve.
      --  Any_Composite must be checked for prior to the array type checks
      --  because Any_Composite does not have any associated indexes.

   begin
      return Is_Boolean_Type (T)
        or else T = Any_Composite
        or else (Is_Array_Type (T)
                  and then T /= Any_String
                  and then Number_Dimensions (T) = 1
                  and then Is_Boolean_Type (Component_Type (T))
                  and then not Is_Private_Composite (T)
                  and then not Is_Limited_Composite (T))
        or else Is_Modular_Integer_Type (T)
        or else T = Universal_Integer;
   end Valid_Boolean_Arg;

   ---------------------------
   --  Valid_Comparison_Arg --
   ---------------------------

   function Valid_Comparison_Arg (T : Entity_Id) return Boolean is
   begin
      return Is_Discrete_Type (T)
        or else Is_Real_Type (T)
        or else (Is_Array_Type (T) and then Number_Dimensions (T) = 1
                  and then Is_Discrete_Type (Component_Type (T))
                  and then not Is_Private_Composite (T)
                  and then not Is_Limited_Composite (T))
        or else Is_String_Type (T);
   end Valid_Comparison_Arg;

   ---------------------
   -- Write_Overloads --
   ---------------------

   procedure Write_Overloads (N : Node_Id) is
      I   : Interp_Index;
      It  : Interp;
      Nam : Entity_Id;

   begin
      if not Is_Overloaded (N) then
         Write_Str ("Non-overloaded entity ");
         Write_Eol;
         Write_Entity_Info (Entity (N), " ");

      else
         Get_First_Interp (N, I, It);
         Write_Str ("Overloaded entity ");
         Write_Eol;
         Nam := It.Nam;

         while Present (Nam) loop
            Write_Entity_Info (Nam,  "      ");
            Write_Str ("=================");
            Write_Eol;
            Get_Next_Interp (I, It);
            Nam := It.Nam;
         end loop;
      end if;
   end Write_Overloads;

   -------------------
   -- All_Overloads --
   -------------------

   procedure All_Overloads is
   begin
      for J in All_Interp.First .. All_Interp.Last loop

         if Present (All_Interp.Table (J).Nam) then
            Write_Entity_Info (All_Interp.Table (J). Nam, " ");
         else
            Write_Str ("No Interp");
         end if;

         Write_Str ("=================");
         Write_Eol;
      end loop;
   end All_Overloads;

end Sem_Type;
