------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ D I S P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.108 $
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
with Elists;   use Elists;
with Einfo;    use Einfo;
with Exp_Disp; use Exp_Disp;
with Errout;   use Errout;
with Hostparm; use Hostparm;
with Nlists;   use Nlists;
with Output;   use Output;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Uintp;    use Uintp;

package body Sem_Disp is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Override_Dispatching_Operation
     (Tagged_Type : Entity_Id;
      Prev_Op     : Entity_Id;
      New_Op      : Entity_Id);
   --  Replace an implicit dispatching operation with an  explicit one.
   --  Prev_Op is an inherited primitive operation which is overridden
   --  by the explicit declaration of New_Op.

   procedure Add_Dispatching_Operation
     (Tagged_Type : Entity_Id;
      New_Op      : Entity_Id);
   --  Add New_Op in the list of primitive operations of Tagged_Type

   function Check_Controlling_Type
     (T    : Entity_Id;
      Subp : Entity_Id)
      return Entity_Id;
      --  T is the type of a formal parameter of subp. Returns the tagged
      --  if the parameter can be a controlling argument, empty otherwise

   ----------------------------
   -- Check_Controlling_Type --
   ----------------------------

   function Check_Controlling_Type
     (T    : Entity_Id;
      Subp : Entity_Id)
      return Entity_Id
   is
      Tagged_Type : Entity_Id := Empty;

   begin
      if Is_Tagged_Type (T) then
         if Is_First_Subtype (T) then
            Tagged_Type := T;
         else
            Tagged_Type := Base_Type (T);
         end if;

      elsif Ekind (T) = E_Anonymous_Access_Type
        and then Is_Tagged_Type (Designated_Type (T))
        and then Ekind (Designated_Type (T)) /= E_Incomplete_Type
      then
         if Is_First_Subtype (Designated_Type (T)) then
            Tagged_Type := Designated_Type (T);
         else
            Tagged_Type := Base_Type (Designated_Type (T));
         end if;
      end if;

      if No (Tagged_Type)
        or else Is_Class_Wide_Type (Tagged_Type)
      then
         return Empty;

      --  The dispatching type and the primitive operation must be defined
      --  in the same scope except for internal operations.

      elsif (Scope (Subp) = Scope (Tagged_Type)
              or else Is_Internal (Subp))
        and then
            (not Is_Generic_Type (Tagged_Type)
              or else not Comes_From_Source (Subp))
      then
         return Tagged_Type;

      else
         return Empty;
      end if;
   end Check_Controlling_Type;

   ---------------------------
   -- Find_Dispatching_Type --
   ---------------------------

   function Find_Dispatching_Type (Subp : Entity_Id) return Entity_Id is
      Formal    : Entity_Id;
      Ctrl_Type : Entity_Id;

   begin
      if Present (DTC_Entity (Subp)) then
         return Scope (DTC_Entity (Subp));

      else
         Formal := First_Formal (Subp);
         while Present (Formal) loop
            Ctrl_Type := Check_Controlling_Type (Etype (Formal), Subp);

            if Present (Ctrl_Type) then
               return Ctrl_Type;
            end if;

            Next_Formal (Formal);
         end loop;

      --  The subprogram may also be dispatching on result

         if Present (Etype (Subp)) then
            Ctrl_Type := Check_Controlling_Type (Etype (Subp), Subp);

            if Present (Ctrl_Type) then
               return Ctrl_Type;
            end if;
         end if;
      end if;

      return Empty;
   end Find_Dispatching_Type;

   -------------------------------
   -- Check_Controlling_Formals --
   -------------------------------

   procedure Check_Controlling_Formals
     (Typ  : Entity_Id;
      Subp : Entity_Id)
   is
      Formal    : Entity_Id;
      Ctrl_Type : Entity_Id;

   begin
      Formal := First_Formal (Subp);
      while Present (Formal) loop
         Ctrl_Type := Check_Controlling_Type (Etype (Formal), Subp);

         if Present (Ctrl_Type) then
            if Ctrl_Type = Typ then
               Set_Is_Controlling_Formal (Formal);

               --  Check that the parameter's nominal subtype statically
               --  matches the first subtype.

               if Ekind (Etype (Formal)) = E_Anonymous_Access_Type then
                  if not Subtypes_Statically_Match
                           (Typ, Designated_Type (Etype (Formal)))
                  then
                     Error_Msg_N
                       ("parameter subtype does not match controlling type",
                        Formal);
                  end if;

               elsif not Subtypes_Statically_Match (Typ, Etype (Formal)) then
                  Error_Msg_N
                    ("parameter subtype does not match controlling type",
                     Formal);
               end if;

               if Present (Default_Value (Formal)) then
                  if Ekind (Etype (Formal)) = E_Anonymous_Access_Type then
                     Error_Msg_N
                       ("default not allowed for controlling access parameter",
                        Default_Value (Formal));

                  elsif not Is_Tag_Indeterminate (Default_Value (Formal)) then
                     Error_Msg_N
                       ("default expression must be a tag indeterminate" &
                        " function call", Default_Value (Formal));
                  end if;
               end if;

            elsif Comes_From_Source (Subp) then
               Error_Msg_N
                 ("operation can be dispatching in only one type", Subp);
            end if;
         end if;

         Next_Formal (Formal);
      end loop;

      if Present (Etype (Subp)) then
         Ctrl_Type := Check_Controlling_Type (Etype (Subp), Subp);

         if Present (Ctrl_Type) then
            if Ctrl_Type = Typ then
               Set_Has_Controlling_Result (Subp);

               --  Check that the result subtype statically matches
               --  the first subtype.

               if not Subtypes_Statically_Match (Typ, Etype (Subp)) then
                  Error_Msg_N
                    ("result subtype does not match controlling type", Subp);
               end if;

            elsif Comes_From_Source (Subp) then
               Error_Msg_N
                 ("operation can be dispatching in only one type", Subp);
            end if;
         end if;
      end if;
   end Check_Controlling_Formals;

   ---------------------------------
   -- Check_Dispatching_Operation --
   ---------------------------------

   procedure Check_Dispatching_Operation (Subp, Old_Subp : Entity_Id) is
      Tagged_Seen            : Entity_Id;
      Has_Dispatching_Parent : Boolean := False;
      Body_Is_Last_Primitive : Boolean := False;

   begin
      if Ekind (Subp) /= E_Procedure and then Ekind (Subp) /= E_Function then
         return;
      end if;

      Set_Is_Dispatching_Operation (Subp, False);
      Tagged_Seen := Find_Dispatching_Type (Subp);

      --  If Subp is derived from a dispatching operation then it should
      --  always be treated as dispatching. In this case various checks
      --  below will be bypassed. Makes sure that late declarations for
      --  inherited private subprograms are treated as dispatching, even
      --  if the associated tagged type is already frozen.

      Has_Dispatching_Parent := Present (Alias (Subp))
        and then Is_Dispatching_Operation (Alias (Subp));

      if No (Tagged_Seen) then
         return;

      --  The subprograms build internally after the freezing point (such as
      --  the Init procedure) are not primitives

      elsif Is_Frozen (Tagged_Seen)
        and then not Comes_From_Source (Subp)
        and then not Has_Dispatching_Parent
      then
         return;

      --  The operation may be a child unit, whose scope is the defining
      --  package, but which is not a primitive operation of the type.

      elsif Is_Child_Unit (Subp) then
         return;

      --  If the subprogram is not defined in a package spec, the only case
      --  where it can be a dispatching op is when it overrides an operation
      --  before the freezing point of the type.

      elsif ((not Is_Package (Scope (Subp)))
              or else In_Package_Body (Scope (Subp)))
        and then not Has_Dispatching_Parent
      then
         if not Comes_From_Source (Subp)
           or else (Present (Old_Subp) and then not Is_Frozen (Tagged_Seen))
         then
            null;

         --  If the type is already frozen, the overriding is not allowed
         --  except when Old_Subp is not a dispatching operation (which
         --  can occur when Old_Subp was inherited by an untagged type).
         --  However, a body with no previous spec freezes the type "after"
         --  its declaration, and therefore is a legal overriding (unless
         --  the type has already been frozen). Only the first such body
         --  is legal.

         elsif Present (Old_Subp)
           and then Is_Dispatching_Operation (Old_Subp)
         then
            if Nkind (Unit_Declaration_Node (Subp)) = N_Subprogram_Body
              and then Comes_From_Source (Subp)
            then
               declare
                  Subp_Body : constant Node_Id := Unit_Declaration_Node (Subp);
                  Decl_Item : Node_Id := Next (Parent (Tagged_Seen));

               begin
                  --  ??? The checks here for whether the type has been
                  --  frozen prior to the new body are not complete. It's
                  --  not simple to check frozenness at this point since
                  --  the body has already caused the type to be prematurely
                  --  frozen in Analyze_Declarations, but we're forced to
                  --  recheck this here because of the odd rule interpretation
                  --  that allows the overriding if the type wasn't frozen
                  --  prior to the body. The freezing action should probably
                  --  be delayed until after the spec is seen, but that's
                  --  a tricky change to the delicate freezing code.

                  --  Look at each declaration following the type up
                  --  until the new subprogram body. If any of the
                  --  declarations is a body then the type has been
                  --  frozen already so the overriding primitive is
                  --  illegal.

                  while Present (Decl_Item)
                    and then (Decl_Item /= Subp_Body)
                  loop
                     if Comes_From_Source (Decl_Item)
                       and then (Nkind (Decl_Item) in N_Proper_Body
                                  or else Nkind (Decl_Item) in N_Body_Stub)
                     then
                        Error_Msg_N ("overriding of& is too late!", Subp);
                        Error_Msg_N
                          ("\spec should appear immediately after the type!",
                           Subp);
                        exit;
                     end if;

                     Next (Decl_Item);
                  end loop;

                  --  If the subprogram doesn't follow in the list of
                  --  declarations including the type then the type
                  --  has definitely been frozen already and the body
                  --  is illegal.

                  if not Present (Decl_Item) then
                     Error_Msg_N ("overriding of& is too late!", Subp);
                     Error_Msg_N
                       ("\spec should appear immediately after the type!",
                        Subp);

                  elsif Is_Frozen (Subp) then

                     --  the subprogram body declares a primitive operation.
                     --  if the subprogram is already frozen, we must update
                     --  its dispatching information explicitly here. The
                     --  information is taken from the overridden subprogram.

                     Body_Is_Last_Primitive := True;

                     if Present (DTC_Entity (Old_Subp)) then
                        Set_DTC_Entity (Subp, DTC_Entity (Old_Subp));
                        Set_DT_Position (Subp, DT_Position (Old_Subp));
                        Insert_After (
                          Subp_Body, Fill_DT_Entry (Sloc (Subp_Body), Subp));
                     end if;
                  end if;
               end;

            else
               Error_Msg_N ("overriding of& is too late!", Subp);
               Error_Msg_N
                 ("\subprogram spec should appear immediately after the type!",
                  Subp);
            end if;

         --  If the type is not frozen yet and we are not in the overridding
         --  case it looks suspiciously like an attempt to define a primitive
         --  operation.

         elsif not Is_Frozen (Tagged_Seen) then
            Error_Msg_N
              ("?not dispatching (must be defined in a package spec)", Subp);
            return;

         --  When the type is frozen, it is legitimate to define a new
         --  non-primitive operation.

         else
            return;
         end if;

      --  Now, we are sure that the scope is a package spec. If the subprogram
      --  is declared after the freezing point ot the type that's an error

      elsif Is_Frozen (Tagged_Seen) and then not Has_Dispatching_Parent then
         Error_Msg_N ("this primitive operation is declared too late", Subp);
         Error_Msg_NE
           ("?no primitive operations for& after this line",
            Freeze_Node (Tagged_Seen),
            Tagged_Seen);
         return;
      end if;

      Check_Controlling_Formals (Tagged_Seen, Subp);

      --  Now it should be a correct primitive operation, put it in the list

      if Present (Old_Subp) then
         Check_Subtype_Conformant (Subp, Old_Subp);
         Override_Dispatching_Operation (Tagged_Seen, Old_Subp, Subp);

      else
         Add_Dispatching_Operation (Tagged_Seen, Subp);
      end if;

      Set_Is_Dispatching_Operation (Subp, True);

      if not Body_Is_Last_Primitive then
         Set_DT_Position (Subp, No_Uint);
      end if;

   end Check_Dispatching_Operation;

   --------------------------------
   --  Add_Dispatching_Operation --
   --------------------------------

   procedure Add_Dispatching_Operation
     (Tagged_Type : Entity_Id;
      New_Op      : Entity_Id)
   is
      List : constant Elist_Id := Primitive_Operations (Tagged_Type);

   begin
      Append_Elmt (New_Op, List);
   end Add_Dispatching_Operation;

   -------------------------------------------
   --  Check_Operation_From_Incomplete_Type --
   -------------------------------------------

   procedure Check_Operation_From_Incomplete_Type
     (Subp : Entity_Id;
      Typ  : Entity_Id)
   is
      Full       : constant Entity_Id := Full_View (Typ);
      Parent_Typ : constant Entity_Id := Etype (Full);
      Old_Prim   : constant Elist_Id  := Primitive_Operations (Parent_Typ);
      New_Prim   : constant Elist_Id  := Primitive_Operations (Full);
      Op1, Op2   : Elmt_Id;
      Prev       : Elmt_Id := No_Elmt;

      function Derives_From (Proc : Entity_Id) return Boolean;
      --  Check that Subp has the signature of an operation derived from Proc.
      --  Subp has an access parameter that designates Typ.

      function Derives_From (Proc : Entity_Id) return Boolean is
         F1, F2 : Entity_Id;

      begin

         if Chars (Proc) /= Chars (Subp) then
            return False;
         end if;

         F1 := First_Formal (Proc);
         F2 := First_Formal (Subp);

         while Present (F1) and then Present (F2) loop

            if Ekind (Etype (F1)) = E_Anonymous_Access_Type then

               if Ekind (Etype (F2)) /= E_Anonymous_Access_Type then
                  return False;

               elsif Designated_Type (Etype (F1)) = Parent_Typ
                 and then Designated_Type (Etype (F2)) /= Full
               then
                  return False;
               end if;

            elsif Ekind (Etype (F2)) = E_Anonymous_Access_Type then
               return False;

            elsif Etype (F1) /= Etype (F2) then
               return False;
            end if;

            Next_Formal (F1);
            Next_Formal (F2);
         end loop;

         return No (F1) and then No (F2);
      end Derives_From;

   begin

      --  The operation may override an inherited one, or may be a new one
      --  altogether. The inherited operation will have been hidden by the
      --  current one at the point of the type derivation, so it does not
      --  appear in the list of primitive operations of the type. We have to
      --  find the proper place of insertion in the list of primitive opera-
      --  tions by iterating over the list for the parent type.

      Op1 := First_Elmt (Old_Prim);
      Op2 := First_Elmt (New_Prim);

      while Present (Op1) and then Present (Op2) loop

         if Derives_From (Node (Op1)) then

            if No (Prev) then
               Prepend_Elmt (Subp, New_Prim);
            else
               Insert_Elmt_After (Subp, Prev);
            end if;

            return;
         end if;

         Prev := Op2;
         Next_Elmt (Op1);
         Next_Elmt (Op2);
      end loop;

      --  Operation is a new primitive.

      Append_Elmt (Subp, New_Prim);

   end Check_Operation_From_Incomplete_Type;

   ---------------------------------------
   -- Check_Operation_From_Private_View --
   ---------------------------------------

   procedure Check_Operation_From_Private_View (Subp, Old_Subp : Entity_Id) is
      Tagged_Type : Entity_Id;

   begin
      if Is_Dispatching_Operation (Alias (Subp)) then
         Set_Scope (Subp, Current_Scope);
         Tagged_Type := Find_Dispatching_Type (Subp);

         if Present (Tagged_Type) and then Is_Tagged_Type (Tagged_Type) then
            Append_Elmt (Old_Subp, Primitive_Operations (Tagged_Type));

            --  If Old_Subp isn't already marked as dispatching then
            --  this is the case of an operation of an untagged private
            --  type fulfilled by a tagged type that overrides an
            --  inherited dispatching operation, so we set the necessary
            --  dispatching attributes here.

            if not Is_Dispatching_Operation (Old_Subp) then
               Check_Controlling_Formals (Tagged_Type, Old_Subp);
               Set_Is_Dispatching_Operation (Old_Subp, True);
               Set_DT_Position (Old_Subp, No_Uint);
            end if;

            --  If the old subprogram is an explicit renaming of some other
            --  entity, it is not overridden by the inherited subprogram.
            --  Otherwise, update its alias and other attributes.

            if Present (Alias (Old_Subp))
              and then Nkind (Unit_Declaration_Node (Old_Subp))
                /= N_Subprogram_Renaming_Declaration
            then
               Set_Alias (Old_Subp, Alias (Subp));

               --  The derived subprogram should inherit the abstractness
               --  of the parent subprogram (except in the case of a function
               --  returning the type). This sets the abstractness properly
               --  for cases where a private extension may have inherited
               --  an abstract operation, but the full type is derived from
               --  a descendant type and inherits a nonabstract version.

               if Etype (Subp) /= Tagged_Type then
                  Set_Is_Abstract (Old_Subp, Is_Abstract (Alias (Subp)));
               end if;
            end if;
         end if;
      end if;
   end Check_Operation_From_Private_View;

   ----------------------------
   -- Check_Dispatching_Call --
   ----------------------------

   procedure Check_Dispatching_Call (N : Node_Id) is
      Actual  : Node_Id;
      Control : Node_Id := Empty;
      Func    : Entity_Id;

      procedure Check_Dispatching_Context;
      --  If the call is tag-indeterminate and the entity being called is
      --  abstract, verify that the context is a call that will eventually
      --  provide a tag for dispatching, or has provided one already.

      -------------------------------
      -- Check_Dispatching_Context --
      -------------------------------

      procedure Check_Dispatching_Context is
         Func : constant Entity_Id := Entity (Name (N));
         Par  : Node_Id;

      begin
         if Is_Abstract (Func)
           and then No (Controlling_Argument (N))
         then
            Par := Parent (N);

            while Present (Par) loop

               if Nkind (Par) = N_Function_Call            or else
                  Nkind (Par) = N_Procedure_Call_Statement or else
                  Nkind (Par) = N_Assignment_Statement     or else
                  Nkind (Par) = N_Op_Eq                    or else
                  Nkind (Par) = N_Op_Ne
               then
                  return;

               elsif Nkind (Par) = N_Qualified_Expression
                 or else Nkind (Par) = N_Unchecked_Type_Conversion
               then
                  Par := Parent (Par);

               else
                  Error_Msg_N
                    ("call to abstract function must be dispatching", N);
                  return;
               end if;
            end loop;
         end if;
      end Check_Dispatching_Context;

   --  Start of processing for Check_Dispatching_Call

   begin
      --  Find a controlling argument, if any

      if Present (Parameter_Associations (N)) then
         Actual := First_Actual (N);

         while Present (Actual) loop
            Control := Find_Controlling_Arg (Actual);
            exit when Present (Control);
            Next_Actual (Actual);
         end loop;

         if Present (Control) then

            --  Verify that no controlling arguments are statically tagged

            if Debug_Flag_E then
               Write_Str ("Found Dispatching call");
               Write_Int (Int (N));
               Write_Eol;
            end if;

            Actual := First_Actual (N);

            while Present (Actual) loop
               if Actual /= Control then

                  if not Is_Controlling_Actual (Actual) then
                     null; -- can be anything

                  elsif (Is_Dynamically_Tagged (Actual)) then
                     null; --  valid parameter

                  elsif Is_Tag_Indeterminate (Actual) then

                     --  The tag is inherited from the enclosing call (the
                     --  node we are currently analyzing). Explicitly expand
                     --  the actual, since the previous call to Expand
                     --  (from Resolve_Call) had no way of knowing about
                     --  the required dispatching.

                     Propagate_Tag (Control, Actual);

                  else
                     Error_Msg_N
                       ("controlling argument is not dynamically tagged",
                        Actual);
                     return;
                  end if;
               end if;

               Next_Actual (Actual);
            end loop;

            --  Mark call as a dispatching call

            Set_Controlling_Argument (N, Control);

         else
            --  The call is not dispatching, check that there isn't any
            --  tag indeterminate abstract call left

            Actual := First_Actual (N);

            while Present (Actual) loop
               if Is_Tag_Indeterminate (Actual) then

                  --  Function call case

                  if Nkind (Original_Node (Actual)) = N_Function_Call then
                     Func := Entity (Name (Original_Node (Actual)));

                  --  Only other possibility is a qualified expression whose
                  --  consituent expression is itself a call.

                  else
                     Func :=
                       Entity (Name
                         (Original_Node
                           (Expression (Original_Node (Actual)))));
                  end if;

                  if Is_Abstract (Func) then
                     Error_Msg_N (
                       "call to abstract function must be dispatching", N);
                  end if;
               end if;

               Next_Actual (Actual);
            end loop;

            Check_Dispatching_Context;
         end if;

      else
         --  If dispatching on result, the enclosing call, if any, will
         --  determine the controlling argument. Otherwise this is the
         --  primitive operation of the root type.

         Check_Dispatching_Context;
      end if;
   end Check_Dispatching_Call;

   ---------------------------
   -- Is_Dynamically_Tagged --
   ---------------------------

   function Is_Dynamically_Tagged (N : Node_Id) return Boolean is
   begin
      return Find_Controlling_Arg (N) /= Empty;
   end Is_Dynamically_Tagged;

   --------------------------
   -- Find_Controlling_Arg --
   --------------------------

   function Find_Controlling_Arg (N : Node_Id) return Node_Id is
      Orig_Node : constant Node_Id := Original_Node (N);
      Typ       : Entity_Id;

   begin
      if Nkind (Orig_Node) = N_Qualified_Expression then
         return Find_Controlling_Arg (Expression (Orig_Node));
      end if;

      --  Dispatching on result case

      if Nkind (Orig_Node) = N_Function_Call
        and then Present (Controlling_Argument (Orig_Node))
        and then Has_Controlling_Result (Entity (Name (Orig_Node)))
      then
         return Controlling_Argument (Orig_Node);

      --  Normal case

      elsif Is_Controlling_Actual (N) then
         Typ := Etype (N);

         if Is_Access_Type (Typ) then
            --  In the case of an Access attribute, use the type of
            --  the prefix, since in the case of an actual for an
            --  access parameter, the attribute's type may be of a
            --  specific designated type, even though the prefix
            --  type is class-wide.

            if Nkind (N) = N_Attribute_Reference then
               Typ := Etype (Prefix (N));
            else
               Typ := Designated_Type (Typ);
            end if;
         end if;

         if Is_Class_Wide_Type (Typ) then
            return N;
         end if;
      end if;

      return Empty;
   end Find_Controlling_Arg;

   --------------------------
   -- Is_Tag_Indeterminate --
   --------------------------

   function Is_Tag_Indeterminate (N : Node_Id) return Boolean is
      Nam       : Entity_Id;
      Actual    : Node_Id;
      Orig_Node : constant Node_Id := Original_Node (N);

   begin
      if Nkind (Orig_Node) = N_Function_Call
        and then Is_Entity_Name (Name (Orig_Node))
      then
         Nam := Entity (Name (Orig_Node));

         if not Has_Controlling_Result (Nam) then
            return False;

         --  If there are no actuals, the call is tag-indeterminate

         elsif No (Parameter_Associations (Orig_Node)) then
            return True;

         else
            Actual := First_Actual (Orig_Node);

            while Present (Actual) loop
               if Is_Controlling_Actual (Actual)
                 and then not Is_Tag_Indeterminate (Actual)
               then
                  return False; -- one operand is dispatching
               end if;

               Next_Actual (Actual);
            end loop;

            return True;

         end if;

      elsif Nkind (Orig_Node) = N_Qualified_Expression then
         return Is_Tag_Indeterminate (Expression (Orig_Node));

      else
         return False;
      end if;
   end Is_Tag_Indeterminate;

   ------------------------------------
   -- Override_Dispatching_Operation --
   ------------------------------------

   procedure Override_Dispatching_Operation
     (Tagged_Type : Entity_Id;
      Prev_Op     : Entity_Id;
      New_Op      : Entity_Id)
   is
      Op_Elmt   : Elmt_Id := First_Elmt (Primitive_Operations (Tagged_Type));

   begin
      --  Patch the primitive operation list

      while Present (Op_Elmt)
        and then Node (Op_Elmt) /= Prev_Op
      loop
         Next_Elmt (Op_Elmt);
      end loop;

      --  If there is no previous operation to override, the type declaration
      --  was malformed, and an error must have been emitted already.

      if No (Op_Elmt) then
         return;
      end if;

      Replace_Elmt (Op_Elmt, New_Op);

      if (not Is_Package (Current_Scope))
        or else not In_Private_Part (Current_Scope)
      then
         --  Not a private primitive

         null;

      elsif Is_Inherited_Operation (Prev_Op) then

         --  Make the overriding operation into an alias of the implicit one.
         --  In this fashion a call from outside ends up calling the new
         --  body even if non-dispatching, and a call from inside calls the
         --  overriding operation because it hides the implicit one.
         --  To indicate that the body of Prev_Op is never called, set its
         --  dispatch table entity to Empty.

         Set_Alias (Prev_Op, New_Op);
         Set_DTC_Entity (Prev_Op, Empty);
         return;

      else
         null;
         pragma Assert (False);
      end if;
   end Override_Dispatching_Operation;

   -------------------
   -- Propagate_Tag --
   -------------------

   procedure Propagate_Tag (Control : Node_Id; Actual : Node_Id) is
      Call_Node : Node_Id;
      Arg       : Node_Id;

   begin
      if Nkind (Actual) = N_Function_Call then
         Call_Node := Actual;

      --  Only other possibility is parenthesized or qualified expression

      else
         Call_Node := Expression (Actual);
      end if;

      --  Do not set the Controlling_Argument if already set. This happens
      --  in the special case of _Input (see Exp_Attr, case Input).

      if No (Controlling_Argument (Call_Node)) then
         Set_Controlling_Argument (Call_Node, Control);
      end if;

      Arg := First_Actual (Call_Node);

      while Present (Arg) loop
         if Is_Tag_Indeterminate (Arg) then
            Propagate_Tag (Control,  Arg);
         end if;

         Next_Actual (Arg);
      end loop;

      --  Expansion of dispatching calls is suppressed when Java_VM, because
      --  the JVM back end directly handles the generation of dispatching
      --  calls and would have to undo any expansion to an indirect call.

      if not Java_VM then
         Expand_Dispatch_Call (Call_Node);
      end if;
   end Propagate_Tag;

end Sem_Disp;
