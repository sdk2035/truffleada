------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 8                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.23 $
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
with Einfo;    use Einfo;
with Exp_Dbug; use Exp_Dbug;
with Exp_Util; use Exp_Util;
with Nlists;   use Nlists;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;

package body Exp_Ch8 is

   ---------------------------------------------
   -- Expand_N_Exception_Renaming_Declaration --
   ---------------------------------------------

   procedure Expand_N_Exception_Renaming_Declaration (N : Node_Id) is
      Decl : constant Node_Id := Debug_Renaming_Declaration (N);

   begin
      if Present (Decl) then
         Insert_Action (N, Decl);
      end if;
   end Expand_N_Exception_Renaming_Declaration;

   ------------------------------------------
   -- Expand_N_Object_Renaming_Declaration --
   ------------------------------------------

   --  Most object renaming cases can be done by just capturing the address
   --  of the renamed object. The cases in which this is not true are when
   --  this address is not computable, since it involves extraction of a
   --  packed array element, or of a record component to which a component
   --  clause applies (that can specify an arbitrary bit boundary).

   --  In these two cases, we pre-evaluate the renaming expression, by
   --  extracting and freezing the values of any subscripts, and then we
   --  set the flag Is_Renaming_Of_Object which means that any reference
   --  to the object will be handled by macro substitution in the front
   --  end, and the back end will know to ignore the renaming declaration.

   --  The other special processing required is for the case of renaming
   --  of an object of a class wide type, where it is necessary to build
   --  the appropriate subtype for the renamed object.
   --  More comments needed for this para ???

   procedure Expand_N_Object_Renaming_Declaration (N : Node_Id) is
      Nam  : Node_Id := Name (N);
      T    : Entity_Id;
      Decl : Node_Id;

      procedure Evaluate_Name (Fname : Node_Id);
      --  A recursive procedure used to freeze a name in the sense described
      --  above, i.e. any variable references or function calls are removed.
      --  Of course the outer level variable reference must not be removed.
      --  For example in A(J,F(K)), A is left as is, but J and F(K) are
      --  evaluated and removed.

      function Evaluation_Required (Nam : Node_Id) return Boolean;
      --  Determines whether it is necessary to do static name evaluation
      --  for renaming of Nam. It is considered necessary if evaluating the
      --  name involves indexing a packed array, or extracting a component
      --  of a record to which a component clause applies. Note that we are
      --  only interested in these operations if they occur as part of the
      --  name itself, subscripts are just values that are computed as part
      --  of the evaluation, so their form is unimportant.

      -------------------
      -- Evaluate_Name --
      -------------------

      procedure Evaluate_Name (Fname : Node_Id) is
         K : constant Node_Kind := Nkind (Fname);
         E : Node_Id;

      begin
         --  For an explicit dereference, we simply force the evaluation
         --  of the name expression. The dereference provides a value that
         --  is the address for the renamed object, and it is precisely
         --  this value that we want to preserve.

         if K = N_Explicit_Dereference then
            Force_Evaluation (Prefix (Fname));

         --  For a selected component, we simply evaluate the prefix

         elsif K = N_Selected_Component then
            Evaluate_Name (Prefix (Fname));

         --  For an indexed component, or an attribute reference, we evaluate
         --  the prefix, which is itself a name, recursively, and then force
         --  the evaluation of all the subscripts (or attribute expressions).

         elsif K = N_Indexed_Component
           or else K = N_Attribute_Reference
         then
            Evaluate_Name (Prefix (Fname));

            E := First (Expressions (Fname));
            while Present (E) loop
               Force_Evaluation (E);
               Next (E);
            end loop;

         --  For a slice, we evaluate the prefix, as for the indexed component
         --  case and then, if there is a range present, either directly or
         --  as the constraint of a discrete subtype indication, we evaluate
         --  the two bounds of this range.

         elsif K = N_Slice then
            Evaluate_Name (Prefix (Fname));

            declare
               DR     : constant Node_Id := Discrete_Range (Fname);
               Constr : Node_Id;
               Rexpr  : Node_Id;

            begin
               if Nkind (DR) = N_Range then
                  Force_Evaluation (Low_Bound (DR));
                  Force_Evaluation (High_Bound (DR));

               elsif Nkind (DR) = N_Subtype_Indication then
                  Constr := Constraint (DR);

                  if Nkind (Constr) = N_Range_Constraint then
                     Rexpr := Range_Expression (Constr);

                     Force_Evaluation (Low_Bound (Rexpr));
                     Force_Evaluation (High_Bound (Rexpr));
                  end if;
               end if;
            end;

         --  For a type conversion, the expression of the conversion must be
         --  the name of an object, and we simply need to evaluate this name.

         elsif K = N_Type_Conversion then
            Evaluate_Name (Expression (Fname));

         --  For a function call, we evaluate the call.

         elsif K = N_Function_Call then
            Force_Evaluation (Fname);

         --  The remaining cases are direct name, operator symbol and
         --  character literal. In all these cases, we do nothing, since
         --  we want to reevaluate each time the renamed object is used.

         else
            return;
         end if;
      end Evaluate_Name;

      -------------------------
      -- Evaluation_Required --
      -------------------------

      function Evaluation_Required (Nam : Node_Id) return Boolean is
      begin
         if Nkind (Nam) = N_Indexed_Component
           or else Nkind (Nam) = N_Slice
         then
            if Is_Packed (Etype (Prefix (Nam))) then
               return True;
            else
               return Evaluation_Required (Prefix (Nam));
            end if;

         elsif Nkind (Nam) = N_Selected_Component then
            if Present (Component_Clause (Entity (Selector_Name (Nam)))) then
               return True;
            else
               return Evaluation_Required (Prefix (Nam));
            end if;

         else
            return False;
         end if;
      end Evaluation_Required;

   --  Start of processing for Expand_N_Object_Renaming_Declaration

   begin
      --  Perform name evaluation if required

      if Evaluation_Required (Nam) then
         Evaluate_Name (Nam);
         Set_Is_Renaming_Of_Object (Defining_Identifier (N));
      end if;

      --  Deal with construction of subtype in class-wide case

      T := Etype (Defining_Identifier (N));

      if Is_Class_Wide_Type (T) then
         Expand_Subtype_From_Expr (N, T, Subtype_Mark (N), Name (N));
         Find_Type (Subtype_Mark (N));
         Set_Etype (Defining_Identifier (N), Entity (Subtype_Mark (N)));
      end if;

      --  Create renaming entry for debug information

      Decl := Debug_Renaming_Declaration (N);

      if Present (Decl) then
         Insert_Action (N, Decl);
      end if;
   end Expand_N_Object_Renaming_Declaration;

   -------------------------------------------
   -- Expand_N_Package_Renaming_Declaration --
   -------------------------------------------

   procedure Expand_N_Package_Renaming_Declaration (N : Node_Id) is
      Decl : constant Node_Id := Debug_Renaming_Declaration (N);

   begin
      if Present (Decl) then

         --  If we are in a compilation unit, then this is an outer
         --  level declaration, and must have a scope of Standard

         if Nkind (Parent (N)) = N_Compilation_Unit then
            declare
               Aux : constant Node_Id := Aux_Decls_Node (Parent (N));

            begin
               New_Scope (Standard_Standard);

               if No (Actions (Aux)) then
                  Set_Actions (Aux, New_List (Decl));
               else
                  Append (Decl, Actions (Aux));
               end if;

               Analyze (Decl);
               Pop_Scope;
            end;

         --  Otherwise, just insert after the package declaration

         else
            Insert_Action (N, Decl);
         end if;
      end if;
   end Expand_N_Package_Renaming_Declaration;

   ----------------------------------------------
   -- Expand_N_Subprogram_Renaming_Declaration --
   ----------------------------------------------

   --  Same processing as for N_Subprogram_Declaration

   procedure Expand_N_Subprogram_Renaming_Declaration (N : Node_Id) is
      Subp : Entity_Id := Defining_Entity (N);

   begin
      --  Generate Default expr functions only if it is not possible to
      --  generate them at the freezing point, where they really belong
      --  otherwise they will be inserted too soon and will cause all sorts
      --  of trouble (e.g. becoming primitive operations of a tagged type etc).

      if No (Corresponding_Spec (N))
        and then not Has_Delayed_Freeze (Subp)
      then
         Make_Default_Expr_Functions (N, Subp);
      end if;

   end Expand_N_Subprogram_Renaming_Declaration;

end Exp_Ch8;
