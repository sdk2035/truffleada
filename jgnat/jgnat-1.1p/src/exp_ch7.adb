------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 7                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.232 $
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

--  This package contains virtually all expansion mechanisms related to
--    - controlled types
--    - transient scopes

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Ch11; use Exp_Ch11;
with Exp_Dbug; use Exp_Dbug;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Freeze;   use Freeze;
with Hostparm; use Hostparm;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rtsfind;  use Rtsfind;
with Targparm; use Targparm;
with Sinfo;    use Sinfo;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Ch7 is

   ---------------------------
   -- Expand_N_Package_Body --
   ---------------------------

   --  Add call to Activate_Tasks if body is an activator (actual
   --  processing is in chapter 9).

   --  Generate subprogram descriptor for elaboration routine

   --  ENcode entity names in package body

   procedure Expand_N_Package_Body (N : Node_Id) is
      Ent : Entity_Id := Corresponding_Spec (N);

   begin
      --  This is done only for non-generic packages

      if Ekind (Ent) = E_Package then
         New_Scope (Corresponding_Spec (N));
         Build_Task_Activation_Call (N);
         Pop_Scope;
      end if;

      Set_Elaboration_Flag (N, Corresponding_Spec (N));

      --  Generate a subprogram descriptor for the elaboration routine of
      --  a package body if the package body has no pending instantiations
      --  and it has generated at least one exception handler

      if Present (Handler_Records (Body_Entity (Ent)))
        and then Is_Compilation_Unit (Ent)
        and then not Delay_Subprogram_Descriptors (Body_Entity (Ent))
      then
         Generate_Subprogram_Descriptor_For_Package
           (N, Body_Entity (Ent));
      end if;

      Set_In_Package_Body (Ent, False);

      --  Set to encode entity names in package body before gigi is called

      Qualify_Entity_Names (N);
   end Expand_N_Package_Body;

   ----------------------------------
   -- Expand_N_Package_Declaration --
   ----------------------------------

   --  Add call to Activate_Tasks if there are tasks declared and the
   --  package has no body. Note that in Ada83,  this may result in
   --  premature activation of some tasks, given that we cannot tell
   --  whether a body will eventually appear.

   procedure Expand_N_Package_Declaration (N : Node_Id) is
   begin
      if Nkind (Parent (N)) = N_Compilation_Unit
        and then not Body_Required (Parent (N))
        and then Present (Activation_Chain_Entity (N))
      then
         New_Scope (Defining_Entity (N));
         Build_Task_Activation_Call (N);
         Pop_Scope;
      end if;

      --  Note: it is not necessary to worry about generating a subprogram
      --  descriptor, since the only way to get exception handlers into a
      --  package spec is to include instantiations, and that would cause
      --  generation of subprogram descriptors to be delayed in any case.

      --  Set to encode entity names in package spec before gigi is called

      Qualify_Entity_Names (N);
   end Expand_N_Package_Declaration;

   --------------------------------------------------
   -- Transient Blocks and Finalization Management --
   --------------------------------------------------

   function Find_Node_To_Be_Wrapped (N : Node_Id) return Node_Id;
   --  N is a node wich may generate a transient scope. Loop over the
   --  parent pointers of N until it find the appropriate node to
   --  wrap. It it returns Empty, it means that no transient scope is
   --  needed in this context.

   function Make_Clean
     (N                          : Node_Id;
      Clean                      : Entity_Id;
      Mark                       : Entity_Id;
      Flist                      : Entity_Id;
      Is_Task                    : Boolean;
      Is_Master                  : Boolean;
      Is_Protected_Subprogram    : Boolean;
      Is_Task_Allocation_Block   : Boolean;
      Is_Asynchronous_Call_Block : Boolean)
      return      Node_Id;
   --  Expand a the clean-up procedure for controlled and/or transient
   --  block, and/or task master or task body, or blocks used to
   --  implement task allocation or asynchronous entry calls, or
   --  procedures used to implement protected procedures. Clean is the
   --  entity for such a procedure. Mark is the entity for the secondary
   --  stack mark, if empty only controlled block clean-up will be
   --  performed. Flist is the entity for the local final list, if empty
   --  only transient scope clean-up will be performed. The flags
   --  Is_Task and Is_Master control the calls to the corresponding
   --  finalization actions for a task body or for an entity that is a
   --  task master.

   procedure Set_Node_To_Be_Wrapped (N : Node_Id);
   --  Set the field Node_To_Be_Wrapped of the current scope

   procedure Insert_Actions_In_Scope_Around (N : Node_Id);
   --  Insert the before-actions kept in the scope stack before N, and the
   --  after after-actions, after N which must be a member of a list.

   function Make_Transient_Block
     (Loc    : Source_Ptr;
      Action : Node_Id)
      return   Node_Id;
   --  Create a transient block whose name is Scope, which is also a
   --  controlled block if Flist is not empty and whose only code is
   --  Action (either a single statement or single declaration).

   type Final_Primitives is (Initialize_Case, Adjust_Case, Finalize_Case);
   --  This enumeration type is defined in order to ease sharing code for
   --  building finalization procedures for composite types.

   Name_Of      : constant array (Final_Primitives) of Name_Id :=
                    (Initialize_Case => Name_Initialize,
                     Adjust_Case     => Name_Adjust,
                     Finalize_Case   => Name_Finalize);

   Deep_Name_Of : constant array (Final_Primitives) of Name_Id :=
                    (Initialize_Case => Name_uDeep_Initialize,
                     Adjust_Case     => Name_uDeep_Adjust,
                     Finalize_Case   => Name_uDeep_Finalize);

   procedure Build_Record_Deep_Procs (Typ : Entity_Id);
   --  Build the deep Initialize/Adjust/Finalize for a record Typ with
   --  Has_Component_Component set and store them using the TSS mechanism.

   procedure Build_Array_Deep_Procs (Typ : Entity_Id);
   --  Build the deep Initialize/Adjust/Finalize for a record Typ with
   --  Has_Controlled_Component set and store them using the TSS mechanism.

   function Make_Deep_Proc
     (Prim  : Final_Primitives;
      Typ   : Entity_Id;
      Stmts : List_Id)
      return  Node_Id;
   --  This function generates the tree for Deep_Initialize, Deep_Adjust
   --  or Deep_Finalize procedures according to the first parameter,
   --  these procedures operate on the type Typ. The Stmts parameter
   --  gives the body of the procedure.

   function Make_Deep_Array_Body
     (Prim : Final_Primitives;
      Typ  : Entity_Id)
      return List_Id;
   --  This function generates the list of statements for implementing
   --  Deep_Initialize, Deep_Adjust or Deep_Finalize procedures
   --  according to the first parameter, these procedures operate on the
   --  array type Typ.

   function Make_Deep_Record_Body
     (Prim : Final_Primitives;
      Typ  : Entity_Id)
      return List_Id;
   --  This function generates the list of statements for implementing
   --  Deep_Initialize, Deep_Adjust or Deep_Finalize procedures
   --  according to the first parameter, these procedures operate on the
   --  record type Typ.

   function Convert_View
     (Proc : Entity_Id;
      Arg  : Node_Id;
      Ind  : Pos := 1)
      return Node_Id;
   --  Proc is one of the Initialize/Adjust/Finalize operations, and
   --  Arg is the argument being passed to it. Ind indicates which
   --  formal of procedure Proc we are trying to match. This function
   --  will, if necessary, generate an conversion between the partial
   --  and full view of Arg to match the type of the formal of Proc,
   --  or force a conversion to the class-wide type in the case where
   --  the operation is abstract.

   -----------------------------
   -- Finalization Management --
   -----------------------------

   --  This part describe how Initialization/Adjusment/Finalization procedures
   --  are generated and called. Two cases must be considered, types that are
   --  Controlled (Is_Controlled flag set) and composite types that contain
   --  controlled components (Has_Controlled_Component flag set). In the first
   --  case the procedures to call are the user-defined primitive operations
   --  Initialize/Adjust/Finalize. In the second case, GNAT generates
   --  Deep_Initialize, Deep_Adjust and Deep_Finalize that are in charge of
   --  calling the former procedures on the controlled components.

   --  For records with Has_Controlled_Component set, a hidden "controller"
   --  component is inserted. This controller component contains its own
   --  finalization list on which all controlled components are attached
   --  creating an indirection on the upper-level Finalization list. This
   --  technique facilitates the management of objects whose number of
   --  controlled components changes during execution. This controller
   --  component is itself controlled and is attached to the upper-level
   --  finalization chain. Its adjust primitive is in charge of calling
   --  adjust on the components and adusting the finalization pointer to
   --  match their new location (see a-finali.adb)

   --  It is not possible to use a similar technique for arrays that have
   --  Has_Controlled_Component set. In this case, deep procedures are
   --  generated that call initialize/adjust/finalize + attachment or
   --  detachment on the finalization list for all component.

   --  Initialize calls: they are generated for declarations or dynamic
   --  allocations of Controlled objects with no initial value. They are
   --  always followed by an attachment to the current Finalization
   --  Chain. For the dynamic allocation case this the chain attached to
   --  the scope of the access type definition otherwise, this is the chain
   --  of the current scope.

   --  Adjust Calls: They are generated on 2 occasions: (1) for
   --  declarations or dynamic allocations of Controlled objects with an
   --  initial value. (2) after an assignment. In the first case they are
   --  followed by an attachment to the final chain, in the second case
   --  they are not.

   --  Finalization Calls: They are generated on (1) scope exit, (2)
   --  assignments, (3) unchecked deallocations. In case (3) they have to
   --  be detached from the final chain, in case (2) they must not and in
   --  case (1) this is not important since we are exiting the scope
   --  anyway.

   --  Here is a simple example of the expansion of a controlled block :

   --    declare
   --       X : Controlled ;
   --       Y : Controlled := Init;
   --
   --       type R is record
   --          C : Controlled;
   --       end record;
   --       W : R;
   --       Z : R := (C => X);
   --    begin
   --       X := Y;
   --       W := Z;
   --    end;
   --
   --  is expanded into
   --
   --    declare
   --       _L : System.FI.Finalizable_Ptr;

   --       procedure _Clean is
   --       begin
   --          Abort_Defer;
   --          System.FI.Finalize_List (_L);
   --          Abort_Undefer;
   --       end _Clean;

   --       X : Controlled;
   --       Initialize (X);
   --       Attach_To_Final_List (_L, Finalizable (X), 1);
   --       Y : Controlled := Init;
   --       Adjust (Y);
   --       Attach_To_Final_List (_L, Finalizable (Y), 1);
   --
   --       type R is record
   --         _C : Record_Controller;
   --          C : Controlled;
   --       end record;
   --       W : R;
   --       Deep_Initialize (W, _L, 1);
   --       Z : R := (C => X);
   --       Deep_Adjust (Z, _L, 1);

   --    begin
   --       Finalize (X);
   --       X := Y;
   --       Adjust (X);

   --       Deep_Finalize (W, False);
   --       W := Z;
   --       Deep_Adjust (W, _L, 0);
   --    at end
   --       _Clean;
   --    end;

   function Global_Flist_Ref (Flist_Ref : Node_Id) return Boolean;
   --  Return True if Flist_Ref refers to a global final list, either
   --  the object GLobal_Final_List which is used to attach standalone
   --  objects, or any of the list controllers associated with library
   --  level access to controlled objects

   ----------------------------
   -- Build_Array_Deep_Procs --
   ----------------------------

   procedure Build_Array_Deep_Procs (Typ : Entity_Id) is
   begin
      Set_TSS (Typ,
        Make_Deep_Proc (
          Prim  => Initialize_Case,
          Typ   => Typ,
          Stmts => Make_Deep_Array_Body (Initialize_Case, Typ)));

      if not Is_Return_By_Reference_Type (Typ) then
         Set_TSS (Typ,
           Make_Deep_Proc (
             Prim  => Adjust_Case,
             Typ   => Typ,
             Stmts => Make_Deep_Array_Body (Adjust_Case, Typ)));
      end if;

      Set_TSS (Typ,
        Make_Deep_Proc (
          Prim  => Finalize_Case,
          Typ   => Typ,
          Stmts => Make_Deep_Array_Body (Finalize_Case, Typ)));
   end Build_Array_Deep_Procs;

   ----------------------
   -- Build_Final_List --
   ----------------------

   procedure Build_Final_List (N : Node_Id; Typ : Entity_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      Set_Associated_Final_Chain (Typ,
        Make_Defining_Identifier (Loc,
          New_External_Name (Chars (Typ), 'L')));

      Insert_Action (N,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
             Associated_Final_Chain (Typ),
          Object_Definition   =>
            New_Reference_To
              (RTE (RE_List_Controller), Loc)));
   end Build_Final_List;

   -----------------------------
   -- Build_Controlling_Procs --
   -----------------------------

   procedure Build_Controlling_Procs (Typ : Entity_Id) is
   begin
      if Is_Array_Type (Typ) then
         Build_Array_Deep_Procs (Typ);

      elsif Is_Record_Type (Typ) then
         Build_Record_Deep_Procs (Typ);

      else
         pragma Assert (False);
         raise Program_Error;
      end if;
   end Build_Controlling_Procs;

   -----------------------------
   -- Build_Record_Deep_Procs --
   -----------------------------

   procedure Build_Record_Deep_Procs (Typ : Entity_Id) is
   begin
      Set_TSS (Typ,
        Make_Deep_Proc (
          Prim  => Initialize_Case,
          Typ   => Typ,
          Stmts => Make_Deep_Record_Body (Initialize_Case, Typ)));

      if not Is_Return_By_Reference_Type (Typ) then
         Set_TSS (Typ,
           Make_Deep_Proc (
             Prim  => Adjust_Case,
             Typ   => Typ,
             Stmts => Make_Deep_Record_Body (Adjust_Case, Typ)));
      end if;

      Set_TSS (Typ,
        Make_Deep_Proc (
          Prim  => Finalize_Case,
          Typ   => Typ,
          Stmts => Make_Deep_Record_Body (Finalize_Case, Typ)));
   end Build_Record_Deep_Procs;

   ---------------------
   -- Controlled_Type --
   ---------------------

   function Controlled_Type (T : Entity_Id) return Boolean is
   begin
      --  Class-wide types are considered controlled because they may contain
      --  an extension that has controlled components

      return (Is_Class_Wide_Type (T)
                and then not In_Finalization_Root (T))
        or else Is_Controlled (T)
        or else Has_Controlled_Component (T)
        or else (Is_Concurrent_Type (T)
          and then Present (Corresponding_Record_Type (T))
          and then Controlled_Type (Corresponding_Record_Type (T)));
   end Controlled_Type;

   ----------------------------------
   -- Has_New_Controlled_Component --
   ----------------------------------

   function Has_New_Controlled_Component (E : Entity_Id) return Boolean is
      Comp : Entity_Id;

   begin
      if not Is_Tagged_Type (E) then
         return Has_Controlled_Component (E);
      elsif not Is_Derived_Type (E) then
         return Has_Controlled_Component (E);
      end if;

      Comp := First_Component (E);
      while Present (Comp) loop

         if Chars (Comp) = Name_uParent then
            null;

         elsif Scope (Original_Record_Component (Comp)) = E
           and then Controlled_Type (Etype (Comp))
         then
            return True;
         end if;

         Next_Component (Comp);
      end loop;

      return False;
   end Has_New_Controlled_Component;

   --------------------------
   -- Controller_Component --
   --------------------------

   function Controller_Component (Typ : Entity_Id) return Entity_Id is
      T         : Entity_Id := Base_Type (Typ);
      Comp      : Entity_Id;
      Comp_Scop : Entity_Id;
      Res       : Entity_Id := Empty;
      Res_Scop  : Entity_Id := Empty;

   begin
      if Is_Class_Wide_Type (T) then
         T := Root_Type (T);
      end if;

      if Is_Private_Type (T) then
         T := Underlying_Type (T);
      end if;

      --  Fetch the outermost controller

      Comp := First_Entity (T);
      while Present (Comp) loop
         if Chars (Comp) = Name_uController then
            Comp_Scop := Scope (Original_Record_Component (Comp));

            --  If this controller is at the outermost level, no need to
            --  look for another one

            if Comp_Scop = T then
               return Comp;

            --  Otherwise record the outermost one and continue looking

            elsif Res = Empty or else Is_Ancestor (Res_Scop, Comp_Scop) then
               Res      := Comp;
               Res_Scop := Comp_Scop;
            end if;
         end if;

         Next_Entity (Comp);
      end loop;

      --  If we fall through the loop, there is no controller component

      return Res;
   end Controller_Component;

   ------------------
   -- Convert_View --
   ------------------

   function Convert_View
     (Proc : Entity_Id;
      Arg  : Node_Id;
      Ind  : Pos := 1)
      return Node_Id
   is
      Fent : Entity_Id := First_Entity (Proc);
      Ftyp : Entity_Id;
      Atyp : Entity_Id;

   begin
      for J in 2 .. Ind loop
         Next_Entity (Fent);
      end loop;

      Ftyp := Etype (Fent);

      if Nkind (Arg) = N_Type_Conversion
        or else Nkind (Arg) = N_Unchecked_Type_Conversion
      then
         Atyp := Entity (Subtype_Mark (Arg));
      else
         Atyp := Etype (Arg);
      end if;

      if Is_Abstract (Proc) and then Is_Tagged_Type (Ftyp) then
         return Unchecked_Convert_To (Class_Wide_Type (Ftyp), Arg);

      elsif Ftyp /= Atyp
        and then Present (Atyp)
        and then
          (Is_Private_Type (Ftyp) or else Is_Private_Type (Atyp))
        and then Underlying_Type (Atyp) = Underlying_Type (Ftyp)
      then
         return Unchecked_Convert_To (Ftyp, Arg);

      --  If the argument is already a conversion, as generated by
      --  Make_Init_Call, set the target type to the type of the formal
      --  directly, to avoid spurious typing problems.

      elsif (Nkind (Arg) = N_Unchecked_Type_Conversion
              or else Nkind (Arg) = N_Type_Conversion)
        and then not Is_Class_Wide_Type (Atyp)
      then
         Set_Subtype_Mark (Arg, New_Occurrence_Of (Ftyp, Sloc (Arg)));
         Set_Etype (Arg, Ftyp);
         return Arg;

      else
         return Arg;
      end if;
   end Convert_View;

   -------------------------------
   -- Expand_Ctrl_Function_Call --
   -------------------------------

   procedure Expand_Ctrl_Function_Call (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Rtype  : constant Entity_Id  := Etype (N);
      Utype  : constant Entity_Id  := Underlying_Type (Rtype);
      Ref    : Node_Id;
      Action : Node_Id;

      Attach_Level : Uint    := Uint_1;
      Len_Ref      : Node_Id := Empty;

      function Last_Array_Component
        (Ref :  Node_Id;
         Typ :  Entity_Id)
         return Node_Id;
      --  Creates a reference to the last component of the array object
      --  designated by Ref whose type is Typ.

      function Last_Array_Component
        (Ref :  Node_Id;
         Typ :  Entity_Id)
         return Node_Id
      is
         N          : Int;
         Index_List : List_Id := New_List;

      begin
         N := 1;
         while N <= Number_Dimensions (Typ) loop
            Append_To (Index_List,
              Make_Attribute_Reference (Loc,
                Prefix         => Duplicate_Subexpr (Ref),
                Attribute_Name => Name_Last,
                Expressions    => New_List (
                  Make_Integer_Literal (Loc, N))));

            N := N + 1;
         end loop;

         return
           Make_Indexed_Component (Loc,
             Prefix      => Duplicate_Subexpr (Ref),
             Expressions => Index_List);
      end Last_Array_Component;

   --  Start of processing for Expand_Ctrl_Function_Call

   begin
      --  Optimization, if the returned value (which is on the sec-stack)
      --  is returned again, no need to copy/readjust/finalize, we can just
      --  pass the value thru (see Expand_N_Return_Statement), and thus no
      --  attachment is needed

      if Nkind (Parent (N)) = N_Return_Statement then
         return;

      --  This is a class-wide type (potentially controlled). We cannot
      --  attach him since it may not have a Final pointer ??? for now do
      --  nothing. The proper fix is to pass the final chain to the called
      --  function as an implicit parameter

      elsif not (Has_Controlled_Component (Rtype)
                  or else Is_Controlled (Rtype))
      then
         return;
      end if;

      --  Resolution is now finished, make sure we don't start analysis again
      --  because of the duplication

      Set_Analyzed (N);
      Ref := Duplicate_Subexpr (N);

      --  Now we can generate the Attach Call, note that this value is
      --  always in the (secondary) stack and thus is attached to a singly
      --  linked final list:
      --
      --    Resx := F (X)'reference;
      --    Attach_To_Final_List (_Lx, Resx.all, 1);
      --  or when there are controlled components
      --    Attach_To_Final_List (_Lx, Resx._controller, 1);
      --  or if it is an array with is_controlled components
      --    Attach_To_Final_List (_Lx, Resx (Resx'last), 3);
      --    An attach level of 3 means that a whole array is to be
      --    attached to the finalization list
      --  or if it is an array with has_controlled components
      --    Attach_To_Final_List (_Lx, Resx (Resx'last)._controller, 3);

      if Has_Controlled_Component (Rtype) then
         declare
            T1 : Entity_Id := Rtype;
            T2 : Entity_Id := Utype;

         begin
            if Is_Array_Type (T2) then
               Len_Ref :=
                 Make_Attribute_Reference (Loc,
                 Prefix => Duplicate_Subexpr (Unchecked_Convert_To (T2, Ref)),
                 Attribute_Name => Name_Length);
            end if;

            while Is_Array_Type (T2) loop
               if T1 /= T2 then
                  Ref := Unchecked_Convert_To (T2, Ref);
               end if;
               Ref := Last_Array_Component (Ref, T2);
               Attach_Level := Uint_3;
               T1 := Component_Type (T2);
               T2 := Underlying_Type (T1);
            end loop;

            if Has_Controlled_Component (T2) then
               if T1 /= T2 then
                  Ref := Unchecked_Convert_To (T2, Ref);
               end if;
               Ref :=
                 Make_Selected_Component (Loc,
                   Prefix        => Ref,
                   Selector_Name => Make_Identifier (Loc, Name_uController));
            end if;
         end;

         --  Here we know that 'Ref' has a controller so we may as well
         --  attach it directly

         Action :=
           Make_Attach_Call (
             Obj_Ref      => Ref,
             Flist_Ref    => Find_Final_List (Current_Scope),
             With_Attach  => Make_Integer_Literal (Loc, Attach_Level));

      else
         --  Here, we have a controlled type that does not seem to have
         --  controlled components but it could be a class wide type whose
         --  further derivations have controlled components. So we don't know
         --  if the object itself needs to be attached or if it
         --  has a record controller. We need to call a runtime function
         --  (Deep_Tag_Attach) which knows what to do thanks to the
         --  RC_Offset in the dispatch table.

         Action :=
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Deep_Tag_Attach), Loc),
             Parameter_Associations => New_List (
               Find_Final_List (Current_Scope),

               Make_Attribute_Reference (Loc,
                   Prefix => Ref,
                   Attribute_Name => Name_Address),

               Make_Integer_Literal (Loc, Attach_Level)));
      end if;

      if Present (Len_Ref) then
         Action :=
           Make_Implicit_If_Statement (N,
             Condition => Make_Op_Gt (Loc,
               Left_Opnd  => Len_Ref,
               Right_Opnd => Make_Integer_Literal (Loc, 0)),
             Then_Statements => New_List (Action));
      end if;

      Insert_Action (N, Action);
   end Expand_Ctrl_Function_Call;

   ---------------------
   -- Find_Final_List --
   ---------------------

   function Find_Final_List
     (E    : Entity_Id;
      Ref  : Node_Id := Empty)
      return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Ref);
      S   : Entity_Id;
      Id  : Entity_Id;
      R   : Node_Id;

   begin
      --  Case of an internal component. The Final list is the record
      --  controller of the enclosing record

      if Present (Ref) then
         R := Ref;
         loop
            case Nkind (R) is
               when N_Unchecked_Type_Conversion | N_Type_Conversion =>
                  R := Expression (R);

               when N_Indexed_Component | N_Explicit_Dereference =>
                  R := Prefix (R);

               when  N_Selected_Component =>
                  R := Prefix (R);
                  exit;

               when  N_Identifier =>
                  exit;

               when others =>
                  pragma Assert (False);
                  raise Program_Error;
            end case;
         end loop;

         return
           Make_Selected_Component (Loc,
             Prefix =>
               Make_Selected_Component (Loc,
                 Prefix        => R,
                 Selector_Name => Make_Identifier (Loc, Name_uController)),
             Selector_Name => Make_Identifier (Loc, Name_F));

      --  Case of a dynamically allocated object. The final list is the
      --  corresponding list controller (The next entity in the scope of
      --  the access type with the right type)

      elsif Is_Access_Type (E) then
         return
           Make_Selected_Component (Loc,
             Prefix        =>
               New_Reference_To (Associated_Final_Chain (Base_Type (E)), Loc),
             Selector_Name => Make_Identifier (Loc, Name_F));

      else
         if Is_Dynamic_Scope (E) then
            S := E;
         else
            S := Enclosing_Dynamic_Scope (E);
         end if;

         --  when the finalization chain entity is 'Error', it means that
         --  there should not be any chain at that level and that the
         --  enclosing one should be used

         while Finalization_Chain_Entity (S) = Error loop
            S := Enclosing_Dynamic_Scope (S);
         end loop;

         if S = Standard_Standard then
            return New_Reference_To (RTE (RE_Global_Final_List), Sloc (E));
         else
            if No (Finalization_Chain_Entity (S)) then

               Id := Make_Defining_Identifier (Sloc (S),
                 New_Internal_Name ('F'));
               Set_Finalization_Chain_Entity (S, Id);

               --  Set momentarily some semantics attributes to allow normal
               --  analysis of expansions containing references to this chain.
               --  Will be fully decorated during the expansion of the scope
               --  itself

               Set_Ekind (Id, E_Variable);
               Set_Etype (Id, RTE (RE_Finalizable_Ptr));
            end if;

            return New_Reference_To (Finalization_Chain_Entity (S), Sloc (E));
         end if;
      end if;
   end Find_Final_List;

   ----------------------
   -- Global_Flist_Ref --
   ----------------------

   function Global_Flist_Ref  (Flist_Ref : Node_Id) return Boolean is
      Flist : Entity_Id;

   begin
      --  Look for the Global_Final_List

      if Is_Entity_Name (Flist_Ref) then
         Flist := Entity (Flist_Ref);

      --  Look for the final list associated with an access to controlled

      elsif  Nkind (Flist_Ref) = N_Selected_Component
        and then Is_Entity_Name (Prefix (Flist_Ref))
      then
         Flist :=  Entity (Prefix (Flist_Ref));
      else
         return False;
      end if;

      return Present (Flist)
        and then Present (Scope (Flist))
        and then Enclosing_Dynamic_Scope (Flist) = Standard_Standard;
   end Global_Flist_Ref;

   --------------------------
   -- In_Finalization_Root --
   --------------------------

   --  It would seem simpler to test Scope (RTE (RE_Root_Controlled)) but
   --  the purpose of this function is to avoid a circular call to Rtsfind
   --  which would been caused by such a test.

   function In_Finalization_Root (E : Entity_Id) return Boolean is
      S : constant Entity_Id := Scope (E);

   begin
      return Chars (Scope (S))     = Name_System
        and then Chars (S)         = Name_Finalization_Root
        and then Scope (Scope (S)) = Standard_Standard;
   end  In_Finalization_Root;

   -----------------------
   -- Make_Adjust_Call --
   -----------------------

   function Make_Adjust_Call
     (Ref          : Node_Id;
      Typ          : Entity_Id;
      Flist_Ref    : Node_Id;
      With_Attach  : Node_Id)
      return         List_Id
   is
      Loc    : constant Source_Ptr := Sloc (Ref);
      Res    : constant List_Id    := New_List;
      Utyp   : Entity_Id;
      Proc   : Entity_Id;
      Cref   : Node_Id := Ref;
      Cref2  : Node_Id;
      Attach : Node_Id := With_Attach;

   begin
      if Is_Class_Wide_Type (Typ) then
         Utyp := Underlying_Type (Base_Type (Root_Type (Typ)));
      else
         Utyp := Underlying_Type (Base_Type (Typ));
      end if;

      Set_Assignment_OK (Cref);

      --  Deal with non-tagged derivation of private views

      if Is_Untagged_Derivation (Typ) then
         Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));
         Cref := Unchecked_Convert_To (Utyp, Cref);
         Set_Assignment_OK (Cref);
         --  To prevent problems with UC see 1.156 RH ???
      end if;

      --  If the underlying_type is a subtype, we are dealing with
      --  the completion of a private type. We need to access
      --  the base type and generate a conversion to it.

      if Utyp /= Base_Type (Utyp) then
         pragma Assert (Is_Private_Type (Typ));
         Utyp := Base_Type (Utyp);
         Cref := Unchecked_Convert_To (Utyp, Cref);
      end if;

      --  We do not need to attach to one of the Global Final Lists
      --  the objects whose type is Finalize_Storage_Only

      if Finalize_Storage_Only (Typ)
        and then (Global_Flist_Ref (Flist_Ref)
          or else Entity (Constant_Value (RTE (RE_Garbage_Collected)))
                  = Standard_True)
      then
         Attach := Make_Integer_Literal (Loc, 0);
      end if;

      --  Generate:
      --    Deep_Adjust (Flist_Ref, Ref, With_Attach);

      if Has_Controlled_Component (Utyp)
        or else Is_Class_Wide_Type (Typ)
      then
         if Is_Tagged_Type (Utyp) then
            Proc := Find_Prim_Op (Utyp, Deep_Name_Of (Adjust_Case));

         else
            Proc := TSS (Utyp, Deep_Name_Of (Adjust_Case));
         end if;

         Cref := Convert_View (Proc, Cref, 2);

         Append_To (Res,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (Proc, Loc),
             Parameter_Associations =>
               New_List (Flist_Ref, Cref, Attach)));

      --  Generate:
      --    if With_Attach then
      --       Attach_To_Final_List (Ref, Flist_Ref);
      --    end if;
      --    Adjust (Ref);

      else -- Is_Controlled (Utyp)

         Proc  := Find_Prim_Op (Utyp, Name_Of (Adjust_Case));
         Cref  := Convert_View (Proc, Cref);
         Cref2 := New_Copy_Tree (Cref);

         Append_To (Res,
           Make_Procedure_Call_Statement (Loc,
           Name => New_Reference_To (Proc, Loc),
           Parameter_Associations => New_List (Cref2)));

         Append_To (Res, Make_Attach_Call (Cref, Flist_Ref, Attach));

         --  Treat this as a reference to Adjust if the Adjust routine
         --  comes from source. The call is not explicit, but it is near
         --  enough, and we won't typically get explicit adjust calls.

         if Comes_From_Source (Proc) then
            Generate_Reference (Proc, Ref);
         end if;
      end if;

      return Res;
   end Make_Adjust_Call;

   ----------------------
   -- Make_Attach_Call --
   ----------------------

   --  Generate:
   --    System.FI.Attach_To_Final_List (Flist, Ref, Nb_Link)

   function Make_Attach_Call
     (Obj_Ref      : Node_Id;
      Flist_Ref    : Node_Id;
      With_Attach  : Node_Id)
      return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Obj_Ref);

   begin
      --  Optimization: If the number of links is statically '0', don't
      --  call the attach_proc.

      if Nkind (With_Attach) = N_Integer_Literal
        and then Intval (With_Attach) = Uint_0
      then
         return Make_Null_Statement (Loc);
      end if;

      return
        Make_Procedure_Call_Statement (Loc,
          Name => New_Reference_To (RTE (RE_Attach_To_Final_List), Loc),
          Parameter_Associations => New_List (
            Flist_Ref,
            OK_Convert_To (RTE (RE_Finalizable), Obj_Ref),
            With_Attach));
   end Make_Attach_Call;

   --------------------------
   -- Make_Deep_Array_Body --
   --------------------------

   --  Array components are initialized and adjusted in the normal order
   --  and finalized in the reverse order. Exceptions are handled and
   --  Program_Error is re-raise in the Adjust and Finalize case
   --  (RM 7.6.1(12)). Generate the following code :
   --
   --  procedure Deep_<P>   --  with <P> being Initialize or Adjust or Finalize
   --   (L : in out Finalizable_Ptr;
   --    V : in out Typ)
   --  is
   --  begin
   --     for J1 in             Typ'First (1) .. Typ'Last (1) loop
   --               ^ reverse ^  --  in the finalization case
   --        ...
   --           for J2 in Typ'First (n) .. Typ'Last (n) loop
   --                 Make_<P>_Call (Typ, V (J1, .. , Jn), L, V);
   --           end loop;
   --        ...
   --     end loop;
   --  exception                                --  not in the
   --     when others => raise Program_Error;   --     Initialize case
   --  end Deep_<P>;

   function Make_Deep_Array_Body
     (Prim : Final_Primitives;
      Typ  : Entity_Id)
      return List_Id
   is
      Loc : constant Source_Ptr := Sloc (Typ);

      Index_List : constant List_Id := New_List;
      --  Stores the list of references to the indexes (one per dimension)

      function One_Component return List_Id;
      --  Create one statement to initialize/adjust/finalize one array
      --  component, designated by a full set of indices.

      function One_Dimension (N : Int) return List_Id;
      --  Create loop to deal with one dimension of the array. The single
      --  statement in the body of the loop initializes the inner dimensions if
      --  any, or else a single component.

      -------------------
      -- One_Component --
      -------------------

      function One_Component return List_Id is
         Comp_Typ : constant Entity_Id := Component_Type (Typ);
         Comp_Ref : constant Node_Id :=
                      Make_Indexed_Component (Loc,
                        Prefix      => Make_Identifier (Loc, Name_V),
                        Expressions => Index_List);

      begin
         case Prim is
            when Initialize_Case =>
               return Make_Init_Call (Comp_Ref, Comp_Typ,
                        Make_Identifier (Loc, Name_L),
                        Make_Identifier (Loc, Name_B));

            when Adjust_Case =>
               return Make_Adjust_Call (Comp_Ref, Comp_Typ,
                        Make_Identifier (Loc, Name_L),
                        Make_Identifier (Loc, Name_B));

            when Finalize_Case =>
               return Make_Final_Call (Comp_Ref, Comp_Typ,
                        Make_Identifier (Loc, Name_B));
         end case;
      end One_Component;

      -------------------
      -- One_Dimension --
      -------------------

      function One_Dimension (N : Int) return List_Id is
         Index : Entity_Id;

      begin
         if N > Number_Dimensions (Typ) then
            return One_Component;

         else
            Index :=
              Make_Defining_Identifier (Loc, New_External_Name ('J', N));

            Append_To (Index_List, New_Reference_To (Index, Loc));

            return New_List (
              Make_Implicit_Loop_Statement (Typ,
                Identifier => Empty,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Loop_Parameter_Specification =>
                      Make_Loop_Parameter_Specification (Loc,
                        Defining_Identifier => Index,
                        Discrete_Subtype_Definition =>
                          Make_Attribute_Reference (Loc,
                            Prefix => Make_Identifier (Loc, Name_V),
                            Attribute_Name  => Name_Range,
                            Expressions => New_List (
                              Make_Integer_Literal (Loc, N))),
                        Reverse_Present => Prim = Finalize_Case)),
                Statements => One_Dimension (N + 1)));
         end if;
      end One_Dimension;

   --  Start of processing for Make_Deep_Array_Body

   begin
      return One_Dimension (1);
   end Make_Deep_Array_Body;

   --------------------
   -- Make_Deep_Proc --
   --------------------

   --  Generate:
   --    procedure DEEP_<prim>
   --      (L : IN OUT Finalizable_Ptr;    -- not for Finalize
   --       V : IN OUT <typ>;
   --       B : IN Short_Short_Integer) is
   --    begin
   --       <stmts>;
   --    exception                   --  Finalize and Adjust Cases only
   --       raise Program_Error;     --  idem
   --    end DEEP_<prim>;

   function Make_Deep_Proc
     (Prim  : Final_Primitives;
      Typ   : Entity_Id;
      Stmts : List_Id)
      return Entity_Id
   is
      Loc       : constant Source_Ptr := Sloc (Typ);
      Formals   : List_Id;
      Proc_Name : Entity_Id;
      Handler   : List_Id := No_List;
      Subp_Body : Node_Id;
      Type_B    : Entity_Id;

   begin
      if Prim = Finalize_Case then
         Formals := New_List;
         Type_B := Standard_Boolean;

      else
         Formals := New_List (
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_L),
             In_Present          => True,
             Out_Present         => True,
             Parameter_Type      =>
               New_Reference_To (RTE (RE_Finalizable_Ptr), Loc)));
         Type_B := Standard_Short_Short_Integer;
      end if;

      Append_To (Formals,
        Make_Parameter_Specification (Loc,
           Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
          In_Present          => True,
          Out_Present         => True,
          Parameter_Type      => New_Reference_To (Typ, Loc)));

      Append_To (Formals,
        Make_Parameter_Specification (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_B),
          Parameter_Type      => New_Reference_To (Type_B, Loc)));

      if Prim = Finalize_Case or else Prim = Adjust_Case then
         Handler := New_List (
           Make_Exception_Handler (Loc,
             Exception_Choices => New_List (Make_Others_Choice (Loc)),
             Statements        => New_List (
               Make_Raise_Program_Error (Loc))));
      end if;

      Proc_Name := Make_Defining_Identifier (Loc, Deep_Name_Of (Prim));

      Subp_Body :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name       => Proc_Name,
              Parameter_Specifications => Formals),

          Declarations =>  Empty_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements         => Stmts,
              Exception_Handlers => Handler));

      return Proc_Name;
   end Make_Deep_Proc;

   ---------------------------
   -- Make_Deep_Record_Body --
   ---------------------------

   --  The Deep procedures call the appropriate Controlling proc on the
   --  the controller component. In the init case, it also attach the
   --  controller to the current finalization list.

   function Make_Deep_Record_Body
     (Prim : Final_Primitives;
      Typ  : Entity_Id)
      return List_Id
   is
      Loc            : constant Source_Ptr := Sloc (Typ);
      Controller_Typ : Entity_Id;
      Obj_Ref        : constant Node_Id := Make_Identifier (Loc, Name_V);
      Controller_Ref : constant Node_Id :=
                         Make_Selected_Component (Loc,
                           Prefix        => Obj_Ref,
                           Selector_Name =>
                             Make_Identifier (Loc, Name_uController));

   begin
      if Is_Return_By_Reference_Type (Typ) then
         Controller_Typ := RTE (RE_Limited_Record_Controller);
      else
         Controller_Typ := RTE (RE_Record_Controller);
      end if;

      case Prim is
         when Initialize_Case =>
            declare
               Res  : constant List_Id := New_List;

            begin
               Append_List_To (Res,
                 Make_Init_Call (
                   Ref          => Controller_Ref,
                   Typ          => Controller_Typ,
                   Flist_Ref    => Make_Identifier (Loc, Name_L),
                   With_Attach  => Make_Identifier (Loc, Name_B)));

               --  When the type is also a controlled type by itself,
               --  Initialize it and attach it at the end of the internal
               --  finalization chain

               if Is_Controlled (Typ) then
                  Append_To (Res,
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Reference_To (
                        Find_Prim_Op (Typ, Name_Of (Prim)), Loc),

                      Parameter_Associations =>
                        New_List (New_Copy_Tree (Obj_Ref))));

                  Append_To (Res, Make_Attach_Call (
                    Obj_Ref      => New_Copy_Tree (Obj_Ref),
                    Flist_Ref    =>
                      Make_Selected_Component (Loc,
                        Prefix        => New_Copy_Tree (Controller_Ref),
                        Selector_Name => Make_Identifier (Loc, Name_F)),
                    With_Attach => Make_Integer_Literal (Loc, 1)));
               end if;

               return Res;
            end;

         when Adjust_Case =>
            return
              Make_Adjust_Call (Controller_Ref, Controller_Typ,
                Make_Identifier (Loc, Name_L),
                Make_Identifier (Loc, Name_B));

         when Finalize_Case =>
            return
              Make_Final_Call (Controller_Ref, Controller_Typ,
                Make_Identifier (Loc, Name_B));
      end case;
   end Make_Deep_Record_Body;

   ----------------------
   -- Make_Final_Call --
   ----------------------

   function Make_Final_Call
     (Ref         : Node_Id;
      Typ         : Entity_Id;
      With_Detach : Node_Id)
      return        List_Id
   is
      Loc   : constant Source_Ptr := Sloc (Ref);
      Res   : constant List_Id    := New_List;
      Cref  : Node_Id;
      Cref2 : Node_Id;
      Proc  : Entity_Id;
      Utyp  : Entity_Id;

   begin
      if Is_Class_Wide_Type (Typ) then
         Utyp := Root_Type (Typ);
         Cref := Ref;

      elsif Is_Concurrent_Type (Typ) then
         Utyp := Corresponding_Record_Type (Typ);
         Cref := Convert_Concurrent (Ref, Typ);

      elsif Is_Private_Type (Typ)
        and then Present (Full_View (Typ))
        and then Is_Concurrent_Type (Full_View (Typ))
      then
         Utyp := Corresponding_Record_Type (Full_View (Typ));
         Cref := Convert_Concurrent (Ref, Full_View (Typ));
      else
         Utyp := Typ;
         Cref := Ref;
      end if;

      Utyp := Underlying_Type (Base_Type (Utyp));
      Set_Assignment_OK (Cref);

      --  Deal with non-tagged derivation of private views

      if Is_Untagged_Derivation (Typ) then
         Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));
         Cref := Unchecked_Convert_To (Utyp, Cref);
         Set_Assignment_OK (Cref);
         --  To prevent problems with UC see 1.156 RH ???
      end if;

      --  If the underlying_type is a subtype, we are dealing with
      --  the completion of a private type. We need to access
      --  the base type and generate a conversion to it.

      if Utyp /= Base_Type (Utyp) then
         pragma Assert (Is_Private_Type (Typ));
         Utyp := Base_Type (Utyp);
         Cref := Unchecked_Convert_To (Utyp, Cref);
      end if;

      --  Generate:
      --    Deep_Finalize (Ref, With_Detach);

      if Has_Controlled_Component (Utyp)
        or else Is_Class_Wide_Type (Typ)
      then
         if Is_Tagged_Type (Utyp) then
            Proc := Find_Prim_Op (Utyp, Deep_Name_Of (Finalize_Case));
         else
            Proc := TSS (Utyp, Deep_Name_Of (Finalize_Case));
         end if;

         Cref := Convert_View (Proc, Cref);

         Append_To (Res,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (Proc, Loc),
             Parameter_Associations =>
               New_List (Cref, With_Detach)));

      --  Generate:
      --    if With_Detach then
      --       Finalize_One (Ref);
      --    else
      --       Finalize (Ref);
      --    end if;

      else
         Proc := Find_Prim_Op (Utyp, Name_Of (Finalize_Case));

         if Chars (With_Detach) = Chars (Standard_True) then
            Append_To (Res,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Finalize_One), Loc),
                Parameter_Associations => New_List (
                  OK_Convert_To (RTE (RE_Finalizable), Cref))));

         elsif Chars (With_Detach) = Chars (Standard_False) then
            Append_To (Res,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (Proc, Loc),
                Parameter_Associations =>
                  New_List (Convert_View (Proc, Cref))));

         else
            Cref2 := New_Copy_Tree (Cref);
            Append_To (Res,
              Make_Implicit_If_Statement (Ref,
                Condition => With_Detach,
                Then_Statements => New_List (
                  Make_Procedure_Call_Statement (Loc,
                    Name => New_Reference_To (RTE (RE_Finalize_One), Loc),
                    Parameter_Associations => New_List (
                      OK_Convert_To (RTE (RE_Finalizable), Cref)))),

                Else_Statements => New_List (
                  Make_Procedure_Call_Statement (Loc,
                    Name => New_Reference_To (Proc, Loc),
                    Parameter_Associations =>
                      New_List (Convert_View (Proc, Cref2))))));
         end if;
      end if;

         --  Treat this as a reference to Finalize if the Finalize routine
         --  comes from source. The call is not explicit, but it is near
         --  enough, and we won't typically get explicit adjust calls.

         if Comes_From_Source (Proc) then
            Generate_Reference (Proc, Ref);
         end if;
      return Res;
   end Make_Final_Call;

   --------------------
   -- Make_Init_Call --
   --------------------

   function Make_Init_Call
     (Ref          : Node_Id;
      Typ          : Entity_Id;
      Flist_Ref    : Node_Id;
      With_Attach  : Node_Id)
      return         List_Id
   is
      Loc     : constant Source_Ptr := Sloc (Ref);
      Is_Conc : Boolean;
      Res     : constant List_Id := New_List;
      Proc    : Entity_Id;
      Utyp    : Entity_Id;
      Cref    : Node_Id;
      Cref2   : Node_Id;
      Attach  : Node_Id := With_Attach;

   begin
      if Is_Concurrent_Type (Typ) then
         Is_Conc := True;
         Utyp    := Corresponding_Record_Type (Typ);
         Cref    := Convert_Concurrent (Ref, Typ);

      elsif Is_Private_Type (Typ)
        and then Present (Full_View (Typ))
        and then Is_Concurrent_Type (Underlying_Type (Typ))
      then
         Is_Conc := True;
         Utyp    := Corresponding_Record_Type (Underlying_Type (Typ));
         Cref    := Convert_Concurrent (Ref, Underlying_Type (Typ));

      else
         Is_Conc := False;
         Utyp    := Typ;
         Cref    := Ref;
      end if;

      Utyp := Underlying_Type (Base_Type (Utyp));

      Set_Assignment_OK (Cref);

      --  Deal with non-tagged derivation of private views

      if Is_Untagged_Derivation (Typ)
        and then not Is_Conc
      then
         Utyp := Underlying_Type (Root_Type (Base_Type (Typ)));
         Cref := Unchecked_Convert_To (Utyp, Cref);
         Set_Assignment_OK (Cref);
         --  To prevent problems with UC see 1.156 RH ???
      end if;

      --  If the underlying_type is a subtype, we are dealing with
      --  the completion of a private type. We need to access
      --  the base type and generate a conversion to it.

      if Utyp /= Base_Type (Utyp) then
         pragma Assert (Is_Private_Type (Typ));
         Utyp := Base_Type (Utyp);
         Cref := Unchecked_Convert_To (Utyp, Cref);
      end if;

      --  We do not need to attach to one of the Global Final Lists
      --  the objects whose type is Finalize_Storage_Only

      if Finalize_Storage_Only (Typ)
        and then (Global_Flist_Ref (Flist_Ref)
          or else Entity (Constant_Value (RTE (RE_Garbage_Collected)))
                  = Standard_True)
      then
         Attach := Make_Integer_Literal (Loc, 0);
      end if;

      --  Generate:
      --    Deep_Initialize (Ref, Flist_Ref);

      if Has_Controlled_Component (Utyp) then
         Proc := TSS (Utyp, Deep_Name_Of (Initialize_Case));

         Cref := Convert_View (Proc, Cref, 2);

         Append_To (Res,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (Proc, Loc),
             Parameter_Associations => New_List (
               Node1 => Flist_Ref,
               Node2 => Cref,
               Node3 => Attach)));

      --  Generate:
      --    Attach_To_Final_List (Ref, Flist_Ref);
      --    Initialize (Ref);

      else -- Is_Controlled (Utyp)
         Proc  := Find_Prim_Op (Utyp, Name_Of (Initialize_Case));
         Cref  := Convert_View (Proc, Cref);
         Cref2 := New_Copy_Tree (Cref);

         Append_To (Res,
           Make_Procedure_Call_Statement (Loc,
           Name => New_Reference_To (Proc, Loc),
           Parameter_Associations => New_List (Cref2)));

         Append_To (Res,
           Make_Attach_Call (Cref, Flist_Ref, Attach));

         --  Treat this as a reference to Initialize if Initialize routine
         --  comes from source. The call is not explicit, but it is near
         --  enough, and we won't typically get explicit adjust calls.

         if Comes_From_Source (Proc) then
            Generate_Reference (Proc, Ref);
         end if;
      end if;

      return Res;
   end Make_Init_Call;

   --------------------------------
   -- Transient Scope Management --
   --------------------------------

   --  A transient scope is created when temporary objects are created by the
   --  compiler. These temporary objects are allocated on the secondary stack
   --  and the transient scope is responsible for finalizing the object when
   --  appropriate and reclaiming the memory at the right time. The temporary
   --  objects are generally the objects allocated to store the result of a
   --  function returning an unconstrained or a tagged value. Expressions
   --  needing to be wrapped in a transient scope (functions calls returning
   --  unconstrained or tagged values) may appear in 3 different contexts which
   --  lead to 3 different kinds of transient scope expansion:

   --   1. In a simple statement (procedure call, assignment, ...). In
   --      this case the instruction is wrapped into a transient block.
   --      (See Wrap_Transient_Statement for details)

   --   2. In an expression of a control structure (test in a IF statement,
   --      expression in a CASE statement, ...).
   --      (See Wrap_Transient_Expression for details)

   --   3. In a expression of an object_declaration. No wrapping is possible
   --      here, so the finalization actions, if any are done right after the
   --      declaration and the secondary stack deallocation is done in the
   --      proper enclosing scope (see Wrap_Transient_Declaration for details)

   --  Note about function returning tagged types: It has been decided to
   --  always allocate their result in the secondary stack while it is not
   --  absolutely mandatory when the tagged type is constrained because the
   --  caller knows the size of the returned object and thus could allocate the
   --  result in the primary stack. But, allocating them always in the
   --  secondary stack simplifies many implementation hassles:

   --    - If it is dispatching function call, the computation of the size of
   --      the result is possible but complex from the outside.

   --    - If the returned type is controlled, the assignment of the returned
   --      value to the anonymous object involves an Adjust, and we have no
   --      easy way to access the anonymous object created by the back-end

   --    - If the returned type is class-wide, this is an unconstrained type
   --      anyway

   --  Furthermore, the little loss in efficiency which is the result of this
   --  decision is not such a big deal because function returning tagged types
   --  are not very much used in real life as opposed to functions returning
   --  access to a tagged type

   -------------------------------
   -- Establish_Transient_Scope --
   -------------------------------

   --  This procedure is called each time a transient block has to be inserted
   --  that is to say for each call to a function with unconstrained ot tagged
   --  result. It creates a new scope on the stack scope in order to enclose
   --  all transient variables generated

   procedure Establish_Transient_Scope (N : Node_Id; Sec_Stack : Boolean) is
      Loc       : constant Source_Ptr := Sloc (N);
      Wrap_Node : Node_Id;

      Sec_Stk : constant Boolean :=
                  Sec_Stack and not Functions_Return_By_DSP_On_Target;
      --  We never need a secondary stack if functions return by DSP

   begin
      --  Do not create a transient scope if we are already inside one

      for S in reverse Scope_Stack.First .. Scope_Stack.Last loop

         if Scope_Stack.Table (S).Is_Transient then
            if Sec_Stk then
               Set_Uses_Sec_Stack (Scope_Stack.Table (S).Entity);
            end if;

            return;

         --  If we have encountered Standard there are no enclosing
         --  transient scopes.

         elsif Scope_Stack.Table (S).Entity = Standard_Standard then
            exit;

         end if;
      end loop;

      Wrap_Node := Find_Node_To_Be_Wrapped (N);

      --  Case of no wrap node, false alert, no transient scope needed

      if No (Wrap_Node) then
         null;

      --  Transient scope is required

      else
         New_Scope (New_Internal_Entity (E_Block, Current_Scope, Loc, 'B'));
         Set_Scope_Is_Transient;

         if Sec_Stk then
            Set_Uses_Sec_Stack (Current_Scope);
            Disallow_In_No_Run_Time_Mode (N);
         end if;

         Set_Etype (Current_Scope, Standard_Void_Type);
         Set_Node_To_Be_Wrapped (Wrap_Node);

         if Debug_Flag_W then
            Write_Str ("    <Transient>");
            Write_Eol;
         end if;
      end if;
   end Establish_Transient_Scope;

   ----------------------------
   -- Expand_Cleanup_Actions --
   ----------------------------

   procedure Expand_Cleanup_Actions (N : Node_Id) is
      Loc                  :  Source_Ptr;
      S                    : constant Entity_Id  :=
                               Current_Scope;
      Flist                : constant Entity_Id  :=
                               Finalization_Chain_Entity (S);
      Is_Task              : constant Boolean    :=
                               (Nkind (Original_Node (N)) = N_Task_Body);
      Is_Master            : constant Boolean    :=
                               Nkind (N) /= N_Entry_Body
                                 and then Is_Task_Master (N);
      Is_Protected         : constant Boolean    :=
                               Nkind (N) = N_Subprogram_Body
                                 and then Is_Protected_Subprogram_Body (N);
      Is_Task_Allocation   : constant Boolean    :=
                               Nkind (N) = N_Block_Statement
                                 and then Is_Task_Allocation_Block (N);
      Is_Asynchronous_Call : constant Boolean    :=
                               Nkind (N) = N_Block_Statement
                                 and then Is_Asynchronous_Call_Block (N);

      Clean     : Entity_Id;
      Mark      : Entity_Id := Empty;
      New_Decls : List_Id   := New_List;
      Blok      : Node_Id;
      Wrapped   : Boolean   := False;
      Chain     : Entity_Id := Empty;
      Decl      : Node_Id;
      Old_Poll  : Boolean;

   begin

      --  Compute a location that is not directly in the user code in
      --  order to avoid to generate confusing debug info. A good
      --  approximation is the name of the outer user-defined scope

      declare
         S1 : Entity_Id := S;

      begin
         while not Comes_From_Source (S1) and then S1 /= Standard_Standard loop
            S1 := Scope (S1);
         end loop;

         Loc := Sloc (S1);
      end;

      --  There are cleanup actions only if the secondary stack needs
      --  releasing or some finalizations are needed or in the context
      --  of tasking

      if Uses_Sec_Stack  (Current_Scope)
        and then not Sec_Stack_Needed_For_Return (Current_Scope)
      then
         null;
      elsif No (Flist)
        and then not Is_Master
        and then not Is_Task
        and then not Is_Protected
        and then not Is_Task_Allocation
        and then not Is_Asynchronous_Call
      then
         return;
      end if;

      --  Set polling off, since we don't need to poll during cleanup
      --  actions, and indeed for the cleanup routine, which is executed
      --  with aborts deferred, we don't want polling.

      Old_Poll := Polling_Required;
      Polling_Required := False;

      --  Make sure we have a declaration list, since we will add to it

      if No (Declarations (N)) then
         Set_Declarations (N, New_List);
      end if;

      --  The task activation call has already been built for task
      --  allocation blocks.

      if not Is_Task_Allocation then
         Build_Task_Activation_Call (N);
      end if;

      if Is_Master then
         Establish_Task_Master (N);
      end if;

      --  If secondary stack is in use, expand:
      --    _Mxx : constant Mark_Id := SS_Mark;

      --  Suppress calls to SS_Mark and SS_Release if Java_VM,
      --  since we never use the secondary stack on the JVM.

      if Uses_Sec_Stack (Current_Scope)
        and then not Sec_Stack_Needed_For_Return (Current_Scope)
        and then not Java_VM
      then
         Mark := Make_Defining_Identifier (Loc, New_Internal_Name ('M'));
         Append_To (New_Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Mark,
             Object_Definition   => New_Reference_To (RTE (RE_Mark_Id), Loc),
             Expression =>
               Make_Function_Call (Loc,
                 Name => New_Reference_To (RTE (RE_SS_Mark), Loc))));

         Set_Uses_Sec_Stack (Current_Scope, False);
      end if;

      --  If finalization list is present then expand:
      --   Local_Final_List : System.FI.Finalizable_Ptr;

      if Present (Flist) then
         Append_To (New_Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Flist,
             Object_Definition   =>
               New_Reference_To (RTE (RE_Finalizable_Ptr), Loc)));
      end if;

      --  Clean-up procedure definition

      Clean := Make_Defining_Identifier (Loc, Name_uClean);
      Set_Suppress_Elaboration_Warnings (Clean);
      Append_To (New_Decls,
        Make_Clean (N, Clean, Mark, Flist,
          Is_Task,
          Is_Master,
          Is_Protected,
          Is_Task_Allocation,
          Is_Asynchronous_Call));

      --  If exception handlers are present, wrap the Sequence of
      --  statements in a block because it is not possible to get
      --  exception handlers and an AT END call in the same scope.

      if Present (Exception_Handlers (Handled_Statement_Sequence (N))) then
         Blok :=
           Make_Block_Statement (Loc,
             Handled_Statement_Sequence => Handled_Statement_Sequence (N));
         Set_Handled_Statement_Sequence (N,
           Make_Handled_Sequence_Of_Statements (Loc, New_List (Blok)));
         Wrapped := True;
      end if;

      --  Don't move the _chain Activation_Chain declaration in task
      --  allocation blocks. Task allocation blocks use this object
      --  in their cleanup handlers, and gigi complains if it is declared
      --  in the sequence of statements of the scope that declares the
      --  handler.

      if Is_Task_Allocation then
         Chain := Activation_Chain_Entity (N);
         Decl := First (Declarations (N));

         while Nkind (Decl) /= N_Object_Declaration
           or else Defining_Identifier (Decl) /= Chain
         loop
            Next (Decl);
            pragma Assert (Present (Decl));
         end loop;

         Remove (Decl);
         Prepend_To (New_Decls, Decl);
      end if;

      --  Now we move the declarations into the Sequence of statements
      --  in order to get them protected by the AT END call. It may seem
      --  weird to put declarations in the sequence of statement but in
      --  fact nothing forbids that at the tree level. We also set the
      --  First_Real_Statement field so that we remember where the real
      --  statements (i.e. original statements) begin. Note that if we
      --  wrapped the statements, the first real statement is inside the
      --  inner block. If the First_Real_Statement is already set (as is
      --  the case for subprogram bodies that are expansions of task bodies)
      --  then do not reset it, because its declarative part would migrate
      --  to the statement part.

      if not Wrapped then
         if No (First_Real_Statement (Handled_Statement_Sequence (N))) then
            Set_First_Real_Statement (Handled_Statement_Sequence (N),
              First (Statements (Handled_Statement_Sequence (N))));
         end if;

      else
         Set_First_Real_Statement (Handled_Statement_Sequence (N), Blok);
      end if;

      Append_List_To (Declarations (N),
        Statements (Handled_Statement_Sequence (N)));
      Set_Statements (Handled_Statement_Sequence (N), Declarations (N));

      --  We need to reset the Sloc of the handled statement sequence to
      --  properly reflect the new initial "statement" in the sequence.

      Set_Sloc
        (Handled_Statement_Sequence (N), Sloc (First (Declarations (N))));

      --  The declarations of the _Clean procedure and finalization chain
      --  replace the old declarations that have been moved inward

      Set_Declarations (N, New_Decls);
      Analyze_Declarations (New_Decls);

      --  The At_End call is attached to the sequence of statements.

      declare
         HSS : Node_Id;

      begin
         --  If the construct is a protected subprogram, then the call to
         --  the corresponding unprotected program appears in a block which
         --  is the last statement in the body, and it is this block that
         --  must be covered by the At_End handler.

         if Is_Protected then
            HSS := Handled_Statement_Sequence
              (Last (Statements (Handled_Statement_Sequence (N))));
         else
            HSS := Handled_Statement_Sequence (N);
         end if;

         Set_At_End_Proc (HSS, New_Occurrence_Of (Clean, Loc));
         Expand_At_End_Handler (HSS, Empty);
      end;

      --  Restore saved polling mode

      Polling_Required := Old_Poll;
   end Expand_Cleanup_Actions;

   -----------------------------
   -- Find_Node_To_Be_Wrapped --
   -----------------------------

   function Find_Node_To_Be_Wrapped (N : Node_Id) return Node_Id is
      P          : Node_Id;
      The_Parent : Node_Id;

   begin
      The_Parent := N;
      loop
         P := The_Parent;
         pragma Assert (P /= Empty);
         The_Parent := Parent (P);

         case Nkind (The_Parent) is

            --  Simple statements are ideal nodes to be wrapped

            when N_Assignment_Statement |
                 N_Pragma               =>
               return The_Parent;

            --  An entry call statement is a special case if it occurs in
            --  the context of a Timed_Entry_Call. In this case we wrap
            --  the entire timed entry call.

            when N_Entry_Call_Statement     |
                 N_Procedure_Call_Statement =>
               if Nkind (Parent (The_Parent)) = N_Entry_Call_Alternative
                 and then
                   Nkind (Parent (Parent (The_Parent))) = N_Timed_Entry_Call
               then
                  return Parent (Parent (The_Parent));
               else
                  return The_Parent;
               end if;

            --  Object declarations are also a boundary for the transient scope
            --  even if they are not really wrapped
            --  (see Wrap_Transient_Declaration)

            when N_Object_Declaration          |
                 N_Object_Renaming_Declaration |
                 N_Subtype_Declaration         =>
               return The_Parent;

            --  The expression itself is to be wrapped if its parent is a
            --  compound statement or any other statement where the expression
            --  is known to be scalar

            when N_Accept_Alternative               |
                 N_Attribute_Definition_Clause      |
                 N_Case_Statement                   |
                 N_Code_Statement                   |
                 N_Delay_Alternative                |
                 N_Delay_Until_Statement            |
                 N_Delay_Relative_Statement         |
                 N_Discriminant_Association         |
                 N_Elsif_Part                       |
                 N_Entry_Body_Formal_Part           |
                 N_Exit_Statement                   |
                 N_If_Statement                     |
                 N_Iteration_Scheme                 |
                 N_Terminate_Alternative            =>
               return P;

            when N_Attribute_Reference              =>

               if Is_Procedure_Attribute_Name
                    (Attribute_Name (The_Parent))
               then
                  return The_Parent;
               end if;

            --  ??? No scheme yet for "for I in Expression'Range loop"
            --  ??? the current scheme for Expression wrapping doesn't apply
            --  ??? because a RANGE is NOT an expression. Tricky problem...
            --  ??? while this problem is not solved we have a potential for
            --  ??? leak and unfinalized intermediate objects here.

            when N_Loop_Parameter_Specification =>
               return Empty;

            --  The following nodes contains "dummy calls" which don't
            --  need to be wrapped.

            when N_Parameter_Specification     |
                 N_Discriminant_Specification  |
                 N_Component_Declaration       =>
               return Empty;

            --  The return statement is not to be wrapped when the function
            --  itself needs wrapping at the outer-level

            when N_Return_Statement            =>
               if Requires_Transient_Scope (Return_Type (The_Parent)) then
                  return Empty;
               else
                  return The_Parent;
               end if;

            --  If we leave a scope without having been able to find a node to
            --  wrap, something is going wrong but this can happen in error
            --  situation that are not detected yet (such as a dynamic string
            --  in a pragma export)

            when N_Subprogram_Body     |
                 N_Package_Declaration |
                 N_Package_Body        |
                 N_Block_Statement     =>
               return Empty;

            --  otherwise continue the search

            when others =>
               null;
         end case;
      end loop;
   end Find_Node_To_Be_Wrapped;

   ------------------------------------
   -- Insert_Actions_In_Scope_Around --
   ------------------------------------

   procedure Insert_Actions_In_Scope_Around (N : Node_Id) is
      SE : Scope_Stack_Entry renames Scope_Stack.Table (Scope_Stack.Last);

   begin
      if Present (SE.Actions_To_Be_Wrapped_Before) then
         Insert_List_Before (N, SE.Actions_To_Be_Wrapped_Before);
         SE.Actions_To_Be_Wrapped_Before := No_List;
      end if;

      if Present (SE.Actions_To_Be_Wrapped_After) then
         Insert_List_After (N, SE.Actions_To_Be_Wrapped_After);
         SE.Actions_To_Be_Wrapped_After := No_List;
      end if;
   end Insert_Actions_In_Scope_Around;

   ----------------
   -- Make_Clean --
   ----------------

   function Make_Clean
     (N                          : Node_Id;
      Clean                      : Entity_Id;
      Mark                       : Entity_Id;
      Flist                      : Entity_Id;
      Is_Task                    : Boolean;
      Is_Master                  : Boolean;
      Is_Protected_Subprogram    : Boolean;
      Is_Task_Allocation_Block   : Boolean;
      Is_Asynchronous_Call_Block : Boolean)
      return      Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Clean);

      Stmt         : List_Id := New_List;
      Sbody        : Node_Id;
      Spec         : Node_Id;
      Name         : Node_Id;
      Param        : Node_Id;
      Unlock       : Node_Id;
      Param_Type   : Entity_Id;
      Pid          : Entity_Id := Empty;
      Cancel_Param : Entity_Id;

   begin
      if Is_Task then
         if Restricted_Profile then
            Append_To
              (Stmt, Build_Runtime_Call (Loc, RE_Complete_Restricted_Task));
         else
            Append_To (Stmt, Build_Runtime_Call (Loc, RE_Complete_Task));
         end if;

      elsif Is_Master then
         if Restrictions (No_Task_Hierarchy) = False then
            Append_To (Stmt, Build_Runtime_Call (Loc, RE_Complete_Master));
         end if;

      elsif Is_Protected_Subprogram then

         --  Add statements to the cleanup handler of the (ordinary)
         --  subprogram expanded to implement a protected subprogram,
         --  unlocking the protected object parameter and undeferring abortion.
         --  If this is a protected procedure, and the object contains
         --  entries, this also calls the entry service routine.

         --  NOTE: This cleanup handler references _object, a parameter
         --        to the procedure.

         --  Find the _object parameter representing the protected object.

         Spec := Parent (Corresponding_Spec (N));

         Param := First (Parameter_Specifications (Spec));
         loop
            Param_Type := Etype (Parameter_Type (Param));

            if Ekind (Param_Type) = E_Record_Type then
               Pid := Corresponding_Concurrent_Type (Param_Type);
            end if;

            exit when not Present (Param) or else Present (Pid);
            Next (Param);
         end loop;

         pragma Assert (Present (Param));

         --  If the associated protected object declares entries,
         --  a protected procedure has to service entry queues.
         --  In this case, add

         --  Service_Entries (_object._object'Access);

         --  _object is the record used to implement the protected object.
         --  It is a parameter to the protected subprogram.

         if Nkind (Specification (N)) = N_Procedure_Specification
           and then Has_Entries (Pid)
         then
            if Abort_Allowed or else Restrictions (No_Entry_Queue) = False
              or else Number_Entries (Pid) > 1
            then
               Name := New_Reference_To (RTE (RE_Service_Entries), Loc);
            else
               Name := New_Reference_To (RTE (RE_Service_Entry), Loc);
            end if;

            Append_To (Stmt,
              Make_Procedure_Call_Statement (Loc,
                Name => Name,
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      Make_Selected_Component (Loc,
                        Prefix => New_Reference_To (
                          Defining_Identifier (Param), Loc),
                        Selector_Name =>
                          Make_Identifier (Loc, Name_uObject)),
                    Attribute_Name => Name_Unchecked_Access))));
         end if;

         --  Unlock (_object._object'Access);

         --  _object is the record used to implement the protected object.
         --  It is a parameter to the protected subprogram.

         --  If the protected object is controlled (i.e it has entries or
         --  needs finalization for interrupt handling), call Unlock_Entries,
         --  except if the protected object follows the ravenscar profile, in
         --  which case call Unlock_Entry, otherwise call the simplified
         --  version, Unlock.

         if Has_Entries (Pid)
           or else Has_Interrupt_Handler (Pid)
           or else Has_Attach_Handler (Pid)
         then
            if Abort_Allowed or else Restrictions (No_Entry_Queue) = False
              or else Number_Entries (Pid) > 1
            then
               Unlock := New_Reference_To (RTE (RE_Unlock_Entries), Loc);
            else
               Unlock := New_Reference_To (RTE (RE_Unlock_Entry), Loc);
            end if;

         else
            Unlock := New_Reference_To (RTE (RE_Unlock), Loc);
         end if;

         Append_To (Stmt,
           Make_Procedure_Call_Statement (Loc,
             Name => Unlock,
             Parameter_Associations => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   Make_Selected_Component (Loc,
                     Prefix =>
                       New_Reference_To (Defining_Identifier (Param), Loc),
                     Selector_Name =>
                       Make_Identifier (Loc, Name_uObject)),
                 Attribute_Name => Name_Unchecked_Access))));

         if Abort_Allowed then
            --  Abort_Undefer;

            Append_To (Stmt,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Reference_To (
                    RTE (RE_Abort_Undefer), Loc),
                Parameter_Associations => Empty_List));
         end if;

      elsif Is_Task_Allocation_Block then

         --  Add a call to Expunge_Unactivated_Tasks to the cleanup
         --  handler of a block created for the dynamic allocation of
         --  tasks:

         --  Expunge_Unactivated_Tasks (_chain);

         --  where _chain is the list of tasks created by the allocator
         --  but not yet activated. This list will be empty unless
         --  the block completes abnormally.

         --  This only applies to dynamically allocated tasks;
         --  other unactivated tasks are completed by Complete_Task or
         --  Complete_Master.

         --  NOTE: This cleanup handler references _chain, a local
         --        object.

         Append_To (Stmt,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Reference_To (
                 RTE (RE_Expunge_Unactivated_Tasks), Loc),
             Parameter_Associations => New_List (
               New_Reference_To (Activation_Chain_Entity (N), Loc))));

      elsif Is_Asynchronous_Call_Block then

         --  Add a call to attempt to cancel the asynchronous entry call
         --  whenever the block containing the abortable part is exited.

         --  NOTE: This cleanup handler references C, a local object

         --  Get the argument to the Cancel procedure
         Cancel_Param := Entry_Cancel_Parameter (Entity (Identifier (N)));

         --  If it is of type Communication_Block, this must be a
         --  protected entry call.

         if Is_RTE (Etype (Cancel_Param), RE_Communication_Block) then

            Append_To (Stmt,

            --  if Enqueued (Cancel_Parameter) then

              Make_Implicit_If_Statement (Clean,
                Condition => Make_Function_Call (Loc,
                  Name => New_Reference_To (
                    RTE (RE_Enqueued), Loc),
                  Parameter_Associations => New_List (
                    New_Reference_To (Cancel_Param, Loc))),
                Then_Statements => New_List (

            --  Cancel_Protected_Entry_Call (Cancel_Param);

                  Make_Procedure_Call_Statement (Loc,
                    Name => New_Reference_To (
                      RTE (RE_Cancel_Protected_Entry_Call), Loc),
                    Parameter_Associations => New_List (
                      New_Reference_To (Cancel_Param, Loc))))));

         --  Asynchronous delay

         elsif Is_RTE (Etype (Cancel_Param), RE_Delay_Block) then
            Append_To (Stmt,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (RTE (RE_Cancel_Async_Delay), Loc),
                Parameter_Associations => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Reference_To (Cancel_Param, Loc),
                    Attribute_Name => Name_Unchecked_Access))));

         --  Task entry call

         else
            --  Append call to Cancel_Task_Entry_Call (C);

            Append_To (Stmt,
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To (
                  RTE (RE_Cancel_Task_Entry_Call),
                  Loc),
                Parameter_Associations => New_List (
                  New_Reference_To (Cancel_Param, Loc))));

         end if;
      end if;

      if Present (Flist) then
         Append_To (Stmt,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Finalize_List), Loc),
             Parameter_Associations => New_List (
                    New_Reference_To (Flist, Loc))));
      end if;

      if Present (Mark) then
         Append_To (Stmt,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_SS_Release), Loc),
             Parameter_Associations => New_List (
                    New_Reference_To (Mark, Loc))));
      end if;

      Sbody :=
        Make_Subprogram_Body (Loc,
          Specification =>
            Make_Procedure_Specification (Loc,
              Defining_Unit_Name => Clean),

          Declarations  => New_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmt));

      if Present (Flist) or else Is_Task or else Is_Master then
         Wrap_Cleanup_Procedure (Sbody);
      end if;

      return Sbody;
   end Make_Clean;

   --------------------------
   -- Make_Transient_Block --
   --------------------------

   --  If finalization is involved, this function just wraps the instruction
   --  into a block whose name is the transient block entity, and then
   --  Expand_Cleanup_Actions (called on the expansion of the handled
   --  sequence of statements will do the necessary expansions for
   --  cleanups).

   function Make_Transient_Block
     (Loc    : Source_Ptr;
      Action : Node_Id)
      return   Node_Id
   is
      Flist  : constant Entity_Id := Finalization_Chain_Entity (Current_Scope);
      Decls  : constant List_Id   := New_List;
      Instrs : constant List_Id   := New_List (Action);
      Blk    : Node_Id;

   begin
      --  Case where only secondary stack use is involved

      if Uses_Sec_Stack (Current_Scope)
        and then No (Flist)
        and then Nkind (Action) /= N_Return_Statement
      then

         declare
            S  : Entity_Id;
            K  : Entity_Kind;
         begin
            S := Scope (Current_Scope);
            loop
               K := Ekind (S);

               --  At the outer level, no need to release the sec stack

               if S = Standard_Standard then
                  Set_Uses_Sec_Stack (Current_Scope, False);
                  exit;

               --  In a function, only release the sec stack if the
               --  function does not return on the sec stack otherwise
               --  the result may be lost. The caller is responsible for
               --  releasing.

               elsif K = E_Function then
                  Set_Uses_Sec_Stack (Current_Scope, False);

                  if not Requires_Transient_Scope (Etype (S)) then
                     if not Functions_Return_By_DSP_On_Target then
                        Set_Uses_Sec_Stack (S, True);
                        Disallow_In_No_Run_Time_Mode (Action);
                     end if;
                  end if;

                  exit;

               --  In a loop or entry we should install a block encompassing
               --  all the construct. For now just release right away.

               elsif K = E_Loop or else K = E_Entry then
                  exit;

               --  In a procedure or a block, we release on exit of the
               --  procedure or block. ??? memory leak can be created by
               --  recursive calls.

               elsif K = E_Procedure
                 or else K = E_Block
               then
                  if not Functions_Return_By_DSP_On_Target then
                     Set_Uses_Sec_Stack (S, True);
                     Disallow_In_No_Run_Time_Mode (Action);
                  end if;

                  Set_Uses_Sec_Stack (Current_Scope, False);
                  exit;

               else
                  S := Scope (S);
               end if;
            end loop;
         end;
      end if;

      --  Insert actions stuck in the transient scopes as well as all
      --  freezing nodes needed by those actions

      Insert_Actions_In_Scope_Around (Action);

      declare
         Last_Inserted : Node_Id := Prev (Action);

      begin
         if Present (Last_Inserted) then
            Freeze_All (First_Entity (Current_Scope), Last_Inserted);
         end if;
      end;

      Blk :=
        Make_Block_Statement (Loc,
          Identifier => New_Reference_To (Current_Scope, Loc),
          Declarations => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Statements => Instrs),
          Has_Created_Identifier => True);

      --  When the transient scope was established, we pushed the entry for
      --  the transient scope onto the scope stack, so that the scope was
      --  active for the installation of finalizable entities etc. Now we
      --  must remove this entry, since we have constructed a proper block.

      Pop_Scope;

      return Blk;
   end Make_Transient_Block;

   ------------------------
   -- Node_To_Be_Wrapped --
   ------------------------

   function Node_To_Be_Wrapped return Node_Id is
   begin
      return Scope_Stack.Table (Scope_Stack.Last).Node_To_Be_Wrapped;
   end Node_To_Be_Wrapped;

   ----------------------------
   -- Set_Node_To_Be_Wrapped --
   ----------------------------

   procedure Set_Node_To_Be_Wrapped (N : Node_Id) is
   begin
      Scope_Stack.Table (Scope_Stack.Last).Node_To_Be_Wrapped := N;
   end Set_Node_To_Be_Wrapped;

   -----------------------------------
   -- Store_Before_Actions_In_Scope --
   -----------------------------------

   procedure Store_Before_Actions_In_Scope (L : List_Id) is
      SE : Scope_Stack_Entry renames Scope_Stack.Table (Scope_Stack.Last);

   begin
      if Present (SE.Actions_To_Be_Wrapped_Before) then
         Insert_List_After_And_Analyze (
           Last (SE.Actions_To_Be_Wrapped_Before), L);

      else
         SE.Actions_To_Be_Wrapped_Before := L;

         if Is_List_Member (SE.Node_To_Be_Wrapped) then
            Set_Parent (L, Parent (SE.Node_To_Be_Wrapped));
         else
            Set_Parent (L, SE.Node_To_Be_Wrapped);
         end if;

         Analyze_List (L);
      end if;
   end Store_Before_Actions_In_Scope;

   ----------------------------------
   -- Store_After_Actions_In_Scope --
   ----------------------------------

   procedure Store_After_Actions_In_Scope (L : List_Id) is
      SE : Scope_Stack_Entry renames Scope_Stack.Table (Scope_Stack.Last);

   begin
      if Present (SE.Actions_To_Be_Wrapped_After) then
         Insert_List_Before_And_Analyze (
          First (SE.Actions_To_Be_Wrapped_After), L);

      else
         SE.Actions_To_Be_Wrapped_After := L;

         if Is_List_Member (SE.Node_To_Be_Wrapped) then
            Set_Parent (L, Parent (SE.Node_To_Be_Wrapped));
         else
            Set_Parent (L, SE.Node_To_Be_Wrapped);
         end if;

         Analyze_List (L);
      end if;
   end Store_After_Actions_In_Scope;

   --------------------------------
   -- Wrap_Transient_Declaration --
   --------------------------------

   --  If a transient scope has been established during the processing of the
   --  Expression of an Object_Declaration, it is not possible to wrap the
   --  declaration into a transient block as usual case, otherwise the object
   --  would be itself declared in the wrong scope. Therefore, all entities (if
   --  any) defined in the transient block are moved to the proper enclosing
   --  scope, furthermore, if they are controlled variables they are finalized
   --  right after the declaration. The finalization list of the transient
   --  scope is defined as a renaming of the enclosing one so during their
   --  initialization they will be attached to the proper finalization
   --  list. For instance, the following declaration :

   --        X : Typ := F (G (A), G (B));

   --  (where G(A) and G(B) return controlled values, expanded as _v1 and _v2)
   --  is expanded into :

   --    _local_final_list_1 : Finalizable_Ptr;
   --    X : Typ := [ complex Expression-Action ];
   --    Finalize_One(_v1);
   --    Finalize_One (_v2);

   procedure Wrap_Transient_Declaration (N : Node_Id) is
      S           : Entity_Id;
      LC          : Entity_Id := Empty;
      Nodes       : List_Id;
      Loc         : constant Source_Ptr := Sloc (N);
      Enclosing_S : Entity_Id;
      Uses_SS     : Boolean;
      Next_N      : constant Node_Id := Next (N);

   begin
      S := Current_Scope;
      Enclosing_S := Scope (S);

      --  Insert Actions kept in the Scope stack

      Insert_Actions_In_Scope_Around (N);

      --  If the declaration is consuming some secondary stack, mark the
      --  Enclosing scope appropriately.

      Uses_SS := Uses_Sec_Stack (S);
      Pop_Scope;

      --  Create a List controller and rename the final list to be its
      --  internal final pointer:
      --       Lxxx : Simple_List_Controller;
      --       Fxxx : Finalizable_Ptr renames Lxxx.F;

      if Present (Finalization_Chain_Entity (S)) then
         LC := Make_Defining_Identifier (Loc, New_Internal_Name ('L'));

         Nodes := New_List (
           Make_Object_Declaration (Loc,
             Defining_Identifier => LC,
             Object_Definition   =>
               New_Reference_To (RTE (RE_Simple_List_Controller), Loc)),

           Make_Object_Renaming_Declaration (Loc,
             Defining_Identifier => Finalization_Chain_Entity (S),
             Subtype_Mark => New_Reference_To (RTE (RE_Finalizable_Ptr), Loc),
             Name =>
               Make_Selected_Component (Loc,
                 Prefix        => New_Reference_To (LC, Loc),
                 Selector_Name => Make_Identifier (Loc, Name_F))));

         --  Put the declaration at the beginning of the declaration part
         --  to make sure it will be before all other actions that have been
         --  inserted before N.

         Insert_List_Before_And_Analyze (First (List_Containing (N)), Nodes);

         --  Generate the Finalization calls by finalizing the list
         --  controller right away. It will be re-finalized on scope exit
         --  but it doesn't matter.

         Nodes :=
           Make_Final_Call (
                Ref         => New_Reference_To (LC, Loc),
                Typ         => Etype (LC),
                With_Detach => New_Reference_To (Standard_False, Loc));
         if Present (Next_N) then
            Insert_List_Before_And_Analyze (Next_N, Nodes);
         else
            Append_List_To (List_Containing (N), Nodes);
         end if;
      end if;

      --  Put the local entities back in the enclosing scope, and set the
      --  Is_Public flag appropriately.

      Transfer_Entities (S, Enclosing_S);

      --  Mark the enclosing dynamic scope so that the sec stack will be
      --  released upon its exit unless this is a function that returns on
      --  the sec stack in which case this will be done by the caller.

      if Uses_SS then
         S := Enclosing_Dynamic_Scope (S);

         if Ekind (S) = E_Function
           and then Requires_Transient_Scope (Etype (S))
         then
            null;
         else
            Set_Uses_Sec_Stack (S);
            Disallow_In_No_Run_Time_Mode (N);
         end if;
      end if;
   end Wrap_Transient_Declaration;

   -------------------------------
   -- Wrap_Transient_Expression --
   -------------------------------

   --  Insert actions before <Expression>:

   --  (lines marked with <CTRL> are expanded only in presence of Controlled
   --   objects needing finalization)

   --     _E : Etyp;
   --     declare
   --        _M : constant Mark_Id := SS_Mark;
   --        Local_Final_List : System.FI.Finalizable_Ptr;    <CTRL>

   --        procedure _Clean is
   --        begin
   --           Abort_Defer;
   --           System.FI.Finalize_List (Local_Final_List);   <CTRL>
   --           SS_Release (M);
   --           Abort_Undefer;
   --        end _Clean;

   --     begin
   --        _E := <Expression>;
   --     at end
   --        _Clean;
   --     end;

   --    then expression is replaced by _E

   procedure Wrap_Transient_Expression (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      E    : constant Entity_Id :=
               Make_Defining_Identifier (Loc, New_Internal_Name ('E'));
      Etyp : Entity_Id := Etype (N);

   begin
      Insert_Actions (N, New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier => E,
          Object_Definition   => New_Reference_To (Etyp, Loc)),

        Make_Transient_Block (Loc,
          Action =>
            Make_Assignment_Statement (Loc,
              Name       => New_Reference_To (E, Loc),
              Expression => Relocate_Node (N)))));

      Rewrite (N, New_Reference_To (E, Loc));
      Analyze_And_Resolve (N, Etyp);
   end Wrap_Transient_Expression;

   ------------------------------
   -- Wrap_Transient_Statement --
   ------------------------------

   --  Transform <Instruction> into

   --  (lines marked with <CTRL> are expanded only in presence of Controlled
   --   objects needing finalization)

   --    declare
   --       _M : Mark_Id := SS_Mark;
   --       Local_Final_List : System.FI.Finalizable_Ptr ;    <CTRL>

   --       procedure _Clean is
   --       begin
   --          Abort_Defer;
   --          System.FI.Finalize_List (Local_Final_List);    <CTRL>
   --          SS_Release (_M);
   --          Abort_Undefer;
   --       end _Clean;

   --    begin
   --       <Instr uction>;
   --    at end
   --       _Clean;
   --    end;

   procedure Wrap_Transient_Statement (N : Node_Id) is
      Loc           : constant Source_Ptr := Sloc (N);
      New_Statement : constant Node_Id := Relocate_Node (N);

   begin
      Rewrite (N, Make_Transient_Block (Loc, New_Statement));

      --  With the scope stack back to normal, we can call analyze on the
      --  resulting block. At this point, the transient scope is being
      --  treated like a perfectly normal scope, so there is nothing
      --  special about it.

      --  Note: Wrap_Transient_Statement is called with the node already
      --  analyzed (i.e. Analyzed (N) is True). This is important, since
      --  otherwise we would get a recursive processing of the node when
      --  we do this Analyze call.

      Analyze (N);
   end Wrap_Transient_Statement;

end Exp_Ch7;
