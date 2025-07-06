------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 3                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.86 $
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

with Ada.Exceptions;    use Ada.Exceptions;
with Atree;             use Atree;
with Debug;             use Debug;
with Einfo;             use Einfo;
with Elists;            use Elists;
with Errout;            use Errout;
with Exp_Tss;           use Exp_Tss;
with GNAT.Case_Util;    use GNAT.Case_Util;
with JVM;               use JVM;
with JVM.API;           use JVM.API;
with JVM.Dbg;           use JVM.Dbg;
with JVM.Map;           use JVM.Map;
with J_String;          use J_String;
with J_Types;           use J_Types;
with Jx_Ch4;            use Jx_Ch4;
with Jx_Decl;           use Jx_Decl;
with Jx_Drive;          use Jx_Drive;
with Jx_Swtch;          use Jx_Swtch;
with Jx_Uplev;          use Jx_Uplev;
with Output;            use Output;
with Osint;
with Sem_Eval;          use Sem_Eval;
with Sem_Util;          use Sem_Util;
with Sinfo;             use Sinfo;
with Sinput;            use Sinput;
with Snames;            use Snames;
with Stand;             use Stand;
with System.Soft_Links; use System.Soft_Links;
with Types;             use Types;
with Nlists;            use Nlists;
with Uintp;             use Uintp;
with Urealp;            use Urealp;

package body Jx_Ch3 is

   procedure Generate_Discrete_Type (Discr_Type : Entity_Id);
   --  Generates the literal name table for enumeration types

   procedure Generate_Record_Type (R_Type : Entity_Id);
   --  Creates a new class for a record type and starts generation
   --  of its class file.

   procedure Generate_Array_Type (A_Type : Entity_Id);
   --  Creates a deep copy operation for an array type for the cases
   --  of multidimensional arrays and arrays with composite or aliased
   --  components. Otherwise does nothing.

   procedure Generate_Integer_Subtype (Subt : Entity_Id);
   --  Processes an integer subtype

   procedure Generate_Array_Subtype (Subt : Entity_Id);
   --  Processes an array subtype

   procedure Generate_Record_Subtype (Subt : Entity_Id);
   --  Processes a record subtype

   procedure Generate_Object_Access_Type (Acc_Type : Entity_Id);
   --  If needed, generates the class for an access-to-unconstrained-array
   --  type.

   procedure Generate_Subprogram_Access_Type (Subp_Acc_Type : Entity_Id);
   --  Generates the class and method for an access-to-subprogram type

   procedure Generate_Concurrent_Type (Task_Or_Prot_Type : Entity_Id);
   --  Translates the visible and private declarations of a concurrent type

   procedure Generate_Deep_Copy (R : Entity_Id);
   --  Generates the deep copy operation for the class associated
   --  with the record type R.

   procedure Generate_Deep_Clone (R : Entity_Id);
   --  Generates the deep clone operation for the class associated
   --  with the record type R. This is a dispatching operation needed
   --  for implementing class-wide object declarations and allocators.

   procedure Allocate_Component (Comp : Entity_Id; Obj : Local_Var_Id);
   --  If Component has a composite type, then generate code to
   --  allocate the component and initialize its corresponding
   --  reference field in the object denoted by Object.

   procedure Allocate_Component_List (Variant : Node_Id; Obj : Local_Var_Id);
   --  Recursively calls Allocate_Record_Components on the component list
   --  associated with Variant. This procedure is passed into Generate_Switch
   --  from Allocate_Record_Components. Obj denotes the object whose components
   --  are to be allocated.

   procedure Allocate_Record_Components
     (Comp_List : Node_Id;
      Obj       : Local_Var_Id);
   --  Traverses a component list, calling Allocate_Component for each
   --  component and recursively processing component lists of any
   --  variant parts. The parameter Obj denotes the object for which
   --  component allocation is to be performed.

   ----------------------------
   -- Translate_Declarations --
   ----------------------------

   procedure Translate_Declarations (Declarations : List_Id) is
      Decl_Node : Node_Id := First (Declarations);

   begin
      --  Invoke the main Translate routine (see Jx_Drive.adb) for
      --  each declaration in the list.

      while Present (Decl_Node) loop
         Set_Current_Source_Loc (Sloc (Decl_Node));

         begin
            Print_Source_Line (Decl_Node);

            Translate (Decl_Node);

         --  If an exception occurs during declaration translation,
         --  then report the error and continue processing at the
         --  next declaration. The stack is reset to avoid cascading
         --  stack errors.

         exception
            when others =>
               if Debug_Flag_JJ then
                  Reset_Stack;
                  Write_Str ("*** Unsupported feature at ");
                  Write_Location (Sloc (Decl_Node));
                  Write_Eol;
                  Write_Str (">>> Exception raised at ");
                  Write_Str (Exception_Message (Get_Current_Excep.all.all));
                  Write_Eol;

               else
                  raise;
               end if;
         end;

         Decl_Node := Next (Decl_Node);
      end loop;
   end Translate_Declarations;

   ----------------------------------
   -- Translate_Object_Declaration --
   ----------------------------------

   procedure Translate_Object_Declaration (Obj : Node_Id) is
      Obj_Entity : constant Entity_Id := Defining_Entity (Obj);
      Init_Expr  : constant Node_Id   := Expression (Obj);
      Obj_Field  : Field_Id;
      Local_Var  : Local_Var_Id;
      Is_Global  : Boolean   := Is_Global_Entity (Obj_Entity);
      Repn_Subt  : Entity_Id := Full_Subtype (Obj_Entity);
      Repn_Type  : Entity_Id := Full_Type (Repn_Subt);
      Wrap_Class : Class_Id;
      Defr_Const : constant Boolean
        := Constant_Present (Obj)
             and then not Present (Expression (Obj))
             and then not No_Initialization (Obj);

   begin
      --  If this is a static scalar constant that is not aliased, then no
      --  field or variable need be allocated, so simply return. We also
      --  check whether the static value of the constant would need a constant
      --  pool reference, and if a pool reference would be required to load
      --  it then we will allocate storage anyway, to allow more efficient
      --  access.

      if Constant_Present (Obj)
        and then Present (Init_Expr)
        and then Ekind (Repn_Type) in Scalar_Kind
        and then not Is_Aliased (Obj_Entity)
        and then Compile_Time_Known_Value (Init_Expr)
      then
         if Ekind (Repn_Type) in Einfo.Float_Kind then
            if not Literal_Needs_Pool_Ref
                     (JVM_Type (Repn_Type), Expr_Value_R (Init_Expr))
            then
               return;
            end if;

         elsif not Literal_Needs_Pool_Ref
                     (JVM_Type (Repn_Type), Expr_Value (Init_Expr))
         then
            return;
         end if;
      end if;

      --  If the object declaration includes an array type definition
      --  then we need to process the type here (e.g., the type may
      --  require a deep copy method).

      if Nkind (Object_Definition (Obj)) = N_Constrained_Array_Definition
        or else
          Nkind (Object_Definition (Obj)) = N_Unconstrained_Array_Definition
      then
         Translate_Type (Base_Type (Etype (Obj_Entity)));
      end if;

      --  For now we disallow the declaration of tagged objects whose
      --  type has convention Java. This prevents problems with types
      --  which do not have no-arg constructors. Perhaps we should
      --  relax this restriction at some point, checking for the
      --  presence of a no-arg constructor for the type. Would it
      --  be better to simply change this to a warning for now and
      --  leave it up to the user to determine the safety rather
      --  than being overly restrictive ???

      if Is_Tagged_Type (Repn_Type)
        and then Convention (Repn_Type) = Convention_Java
      then
         Error_Msg_N
           ("stand-alone object of Java-convention tagged type not allowed",
            Obj);
         Error_Msg_N
           ("must use access values to reference Java objects", Obj);
      end if;

      --  Global objects are declared as static fields of the enclosing
      --  package class and local objects are declared as local variables
      --  of the current method.

      if Is_Global then
         --  For a deferred constant, declare the JVM entity for the full
         --  constant and also associate it with the deferred constant.
         --  If the deferred constant is imported, then we go ahead with
         --  the normal processing and let Declare_Field take care of
         --  handling the import.

         if Defr_Const and then not Is_Imported (Obj_Entity) then
            Declare_Field
              (Associated_Class (Obj_Entity), Full_View (Obj_Entity));
            Obj_Field := JVM_Field (Full_View (Obj_Entity));
            Set_Map (Obj_Entity, Obj_Field);

         --  If Obj_Entity already has an associated field, then the
         --  object is the full declaration for a deferred constant,
         --  so the full constant has already been declared and we
         --  simply retrieve its established field.

         elsif JVM_Entity (Obj_Entity) /= Null_Field then
            Obj_Field := JVM_Field (Obj_Entity);

         --  Otherwise we declare the field now (the usual case)

         else
            Declare_Field (Associated_Class (Obj_Entity), Obj_Entity);
            Obj_Field := JVM_Field (Obj_Entity);
         end if;

      else

         --  If the object is imported from C or has an explicitly
         --  specified interface name then declare it as a field
         --  rather than a local variable and set Is_Global.

         if Is_Imported (Obj_Entity)
           and then (Convention (Obj_Entity) = Convention_C
                      or else Present (Interface_Name (Obj_Entity)))
         then
            Declare_Field (Associated_Class (Obj_Entity), Obj_Entity);
            Obj_Field := JVM_Field (Obj_Entity);
            Is_Global := True;

         --  For a deferred constant, declare the JVM entity for the full
         --  constant and also associate it with the deferred constant.

         elsif Defr_Const then
            Declare_Local_Variable (Full_View (Obj_Entity));
            Local_Var := JVM_Local_Var (Full_View (Obj_Entity));
            Set_Map (Obj_Entity, Local_Var);

         --  If Obj_Entity already has an associated local variable, then
         --  the object is the full declaration for a deferred constant,
         --  so the full constant has already been declared and we simply
         --  retrieve its established local variable.

         elsif JVM_Entity (Obj_Entity) /= Null_Local_Var then
            Local_Var := JVM_Local_Var (Obj_Entity);

         --  Otherwise we declare the local variable now (the usual case)

         else
            Declare_Local_Variable (Obj_Entity);
            Local_Var := JVM_Local_Var (Obj_Entity);
         end if;
      end if;

      --  No further processing if the object is a deferred constant.
      --  The processing of the full declaration will take care of the
      --  constant's allocation and initialization.

      if Defr_Const then
         return;
      end if;

      --  We support address clauses for composite objects.
      --  The corresponding field or local variable is simply
      --  intialized by the address given in the address clause.
      --  It's up to the user to ensure that the address denotes
      --  an object of a compatible type.

      if Present (Address_Clause (Obj_Entity)) then

         --  For now only allow composite objects. Should we
         --  extend this to aliased elementary objects and
         --  issue an error message in all other cases ???

         pragma Assert (Is_Composite_Type (Etype (Obj_Entity)));

         Evaluate_Expr (Expression (Address_Clause (Obj_Entity)));
         Gen_Check_Cast (JVM_Type (Obj_Entity));

         if Is_Global then
            Gen_Put_Static_Field (Obj_Field);
         else
            Gen_Store_Local (Local_Var);
         end if;

      elsif not Is_Imported (Obj_Entity) then

         --  Generate code to allocate the object if its type is composite
         --  or if the object has elementary type and is aliased.

         case Ekind (Repn_Type) is
            when E_Record_Type | E_Task_Type | E_Protected_Type =>

               --  If the object is explicitly initialized then the result
               --  of the initialization itself will become the object.

               if not Present (Init_Expr) or else No_Initialization (Obj) then
                  Gen_New_Object (Class_Of_Type (JVM_Type (Obj_Entity)));

                  --  Invoke the default <init> constructor of the type.
                  --  If the type has an associated _init_proc routine then
                  --  that will be called subsequently and will handle
                  --  allocation of any composite components (unless the
                  --  object has an explicit initialization, in which case
                  --  the component allocation will happen via the deep copy).

                  Gen_Duplicate;
                  Gen_Invoke_Init (Repn_Type);

                  if Is_Global then
                     Gen_Put_Static_Field (Obj_Field);
                  else
                     Gen_Store_Local (Local_Var);
                  end if;

                  --  If the declaration has an explicit initialization,
                  --  but it was performed in-line by the front end or in
                  --  any case if No_Initialization is true, then in addition
                  --  to the object allocation itself we have to ensure that
                  --  any composite components are allocated. If the type has
                  --  no _init_proc, then the constructor will have taken
                  --  care of the component allocations. But if there is
                  --  an _init_proc, then the component allocations would
                  --  be performed by the _init_proc, which unfortunately
                  --  won't be called in this situation (it might have
                  --  side effects, which shouldn't be performed in the
                  --  presence of explicit initialization). So for now
                  --  we force the generation of the allocation in-line
                  --  It seems that what is really needed is either a
                  --  separate routine that gets called for component
                  --  allocation, or else a parameterized constructor
                  --  that conditionally performs component allocation. ???

                  if No_Initialization (Obj)
                    and then Present (Base_Init_Proc (Repn_Type))
                  then
                     --  If the object is global, then allocate a local
                     --  temporary to hold its reference, for use in the
                     --  allocation of the object's composite components.

                     if Is_Global then
                        Local_Var
                          := New_Local_Var ("_aggr_tmp", JVM_Type (Repn_Type));

                        Gen_Get_Static_Field (Obj_Field);
                        Gen_Store_Local (Local_Var);
                     end if;

                     --  We have to initialize any discriminants of the new
                     --  object before allocating its discriminant-dependent
                     --  components, otherwise Allocate_Composite_Components
                     --  has no way of determining the size of discriminant-
                     --  dependent arrays.

                     if Has_Discriminants (Repn_Type) then
                        --  If there's a discriminant constraint, then we
                        --  evaluate and store the values given in the
                        --  constraint into the new object.

                        if Present (Discriminant_Constraint (Repn_Subt))
                          and then Is_Constrained (Repn_Subt)
                        then
                           declare
                              Discr : Entity_Id
                                := First_Discriminant (Repn_Type);
                              Assn  : Elmt_Id
                                := First_Elmt
                                     (Discriminant_Constraint (Repn_Subt));
                           begin
                              while Present (Assn) loop
                                 Gen_Load_Local (Local_Var);
                                 Evaluate_Expr (Node (Assn));
                                 Gen_Put_Field (JVM_Field (Discr));

                                 Next_Discriminant (Discr);
                                 Assn := Next_Elmt (Assn);
                              end loop;
                           end;

                        --  In the absence of a constraint, then the only
                        --  place to get the discriminants is from the
                        --  initialization itself, which we require to
                        --  be an aggregate.

                        elsif Present (Init_Expr) then
                           declare
                              Aggr : Node_Id := Init_Expr;
                              Comp : Entity_Id;
                              Assn : Node_Id;

                           begin
                              if Nkind (Aggr) = N_Qualified_Expression then
                                 Aggr := Expression (Aggr);
                              end if;

                              pragma Assert (Nkind (Aggr) = N_Aggregate);

                              Comp := First_Discriminant (Repn_Type);
                              Assn := First (Expressions (Aggr));

                              --  Process the aggregate's associations and
                              --  evaluate the expressions associated with
                              --  the discriminants of the type, storing the
                              --  values into the fields of the new object.

                              --  The front end currently always normalizes
                              --  record aggregates to have named associations,
                              --  for benefit of Gigi, but the specification
                              --  allows for the more general form, so we
                              --  handle positional associations here even
                              --  though they can't currently occur.

                              while Present (Assn) and then Present (Comp) loop
                                 Gen_Load_Local (Local_Var);
                                 Evaluate_Expr (Assn);
                                 Gen_Put_Field (JVM_Field (Comp));

                                 Next_Discriminant (Comp);
                                 Next (Assn);
                              end loop;

                              --  Traverse the named component associations

                              Assn := First (Component_Associations (Aggr));
                              while Present (Assn) loop
                                 Comp := Entity (First (Choices (Assn)));
                                 if Ekind (Comp) = E_Discriminant then
                                    Gen_Load_Local (Local_Var);
                                    Evaluate_Expr (Expression (Assn));
                                    Gen_Put_Field (JVM_Field (Comp));
                                 end if;

                                 Next (Assn);
                              end loop;
                           end;

                        --  The object's subtype is unconstrained, but there's
                        --  no initialization from which to extract the values
                        --  of the discriminants. This should never occur, or
                        --  if it does then we don't have anyplace where we
                        --  can get the discriminants.

                        else
                           pragma Assert (False);
                           raise Program_Error;
                        end if;
                     end if;

                     Allocate_Composite_Components (Repn_Type, Local_Var);
                  end if;
               end if;

            when E_Array_Type | E_String_Type =>

               --  If the object is explicitly initialized then the result
               --  of the initialization itself will become the object.

               if not Present (Init_Expr) or else No_Initialization (Obj) then
                  declare
                     Index  : Node_Id := First_Index (Repn_Subt);
                     Arr_LV : Local_Var_Id;

                  begin
                     if Number_Dimensions (Repn_Subt) = 1 then
                        Load_Index_Length (Index);
                        Gen_New_Array (JVM_Type (Obj_Entity));

                     --  Multidimensional array case

                     else
                        while Present (Index) loop
                           Load_Index_Length (Index);
                           Next_Index (Index);
                        end loop;

                        Gen_New_Multiarray (JVM_Type (Obj_Entity));
                     end if;

                     --  An array with composite or aliased components
                     --  requires a traversal of the array and allocation
                     --  of objects for all its components. For now this
                     --  is generated inline, but eventually we should
                     --  encapsulate it in a method associated with the
                     --  array type.

                     if Ekind (Full_Type (Component_Type (Repn_Subt)))
                         in Composite_Kind
                       or else Has_Aliased_Components (Repn_Subt)
                     then
                        Arr_LV
                          := New_Local_Var ("_arr_tmp", JVM_Type (Repn_Type));

                        Gen_Store_Local (Arr_LV);

                        Allocate_Array_Components (Repn_Subt, Arr_LV);

                        Gen_Load_Local (Arr_LV);
                     end if;
                  end;

                  if Is_Global then
                     Gen_Put_Static_Field (Obj_Field);
                  else
                     Gen_Store_Local (Local_Var);
                  end if;

                  --  Note: Eventually we want to allocate locals/fields
                  --  with names having "__first" and "__last" suffixes
                  --  suffixes that will hold the bounds of the array
                  --  object, so as to allow easy access of bounds
                  --  information from JVM debuggers (this will be
                  --  done even when the bounds are static). ???
               end if;

            when Wrappable_Kind =>

               --  If this is an aliased scalar or access object, then allocate
               --  and initialize its associated wrapper object.

               if Is_Aliased (Obj_Entity) then
                  Wrap_Class := Class_Of_Type (Wrapper_Type (Obj_Entity));

                  Gen_Default_Object (Wrap_Class);

                  if Is_Global then
                     Gen_Put_Static_Field (Obj_Field);
                  else
                     Gen_Store_Local (Local_Var);
                  end if;

               --  All scalar local variables without an explicit
               --  initialization expression are initialized to
               --  zero in order to satisfy the verifier (which
               --  performs a flow analysis and will reject
               --  code that evaluates scalar locals that are
               --  potentially uninitialized).

               elsif not Is_Global and then not Present (Init_Expr) then
                  case JVM.Type_Kind (JVM_Type (Repn_Type)) is
                     when Boolean_Kind .. Int_Kind =>
                        Gen_Push_Int (Uint_0);
                     when Long_Kind =>
                        Gen_Push_Long (Uint_0);
                     when JVM.Float_Kind =>
                        Gen_Push_Float (Ureal_0);
                     when Double_Kind =>
                        Gen_Push_Double (Ureal_0);
                     when Class_Kind =>
                        Gen_Push_Null;
                     when others =>
                        pragma Assert (False);
                        raise Program_Error;
                  end case;

                  Gen_Store_Local (Local_Var);
               end if;

            when others =>
               null;
         end case;
      end if;

      --  If needed, perform the explicit initialization of the object

      if Present (Init_Expr) and then not No_Initialization (Obj) then
         case Ekind (Repn_Type) is
            when Elementary_Kind =>

               --  If the object is aliased and has an associated
               --  wrapper type, then the ".all" field of the wrapper
               --  object must be initialized.

               if Has_Wrapper (Obj_Entity) then
                  if Is_Global then
                     Gen_Get_Static_Field (Obj_Field);
                  else
                     Gen_Load_Local (Local_Var);
                  end if;

                  Evaluate_Expr (Init_Expr, Check_Subtype => Repn_Subt);

                  Gen_Put_Object_Field (Wrapper_Field (Obj_Entity));

               else
                  Evaluate_Expr (Init_Expr, Check_Subtype => Repn_Subt);

                  if Is_Global then
                     Gen_Put_Static_Field (Obj_Field);
                  else
                     Gen_Store_Local (Local_Var);
                  end if;
               end if;

            when Record_Kind | Einfo.Array_Kind =>
               --  The result of calling Evaluate_With_Copy can be used
               --  directly as the object itself (no additional allocation
               --  and copy is needed).

               Evaluate_With_Copy (Init_Expr);

               if Is_Global then
                  Gen_Put_Static_Field (Obj_Field);
               else
                  Gen_Store_Local (Local_Var);
               end if;

            when others =>
               pragma Assert (False);
               raise Program_Error;
         end case;
      end if;
   end Translate_Object_Declaration;

   --------------------
   -- Translate_Type --
   --------------------

   procedure Translate_Type (T : Entity_Id) is
      Base_T : constant Entity_Id := Base_Type (T);

   begin
      --  If the type already has an associated JVM Type_Id,
      --  then it's the full type for a private type and has
      --  already been declared (see Declare_Type).

      if JVM_Entity (Base_T) = Null_Type then
         Declare_Type (Base_T);
      end if;

      --  If the type is an untagged derived type then it has been associated
      --  with its parent type's JVM type and no further actions are needed.
      --  In the case of derived enumeration type's we continue, allowing
      --  the call to Generate_Discrete_Type since that may need to generate
      --  the type's literal table.

      if Is_Derived_Type (Base_T)
        and then not Is_Tagged_Type (Base_T)
        and then Ekind (Base_T) /= E_Enumeration_Type
      then
         return;
      end if;

      case Ekind (Base_T) is
         when Discrete_Kind =>
            Generate_Discrete_Type (Base_T);

         when Einfo.Float_Kind =>
            null;

         when Fixed_Point_Kind =>
            null;

         when E_Record_Type =>
            Generate_Record_Type (Base_T);

         when E_Class_Wide_Type =>
            null;

         when E_Array_Type | E_String_Type | E_Enum_Table_Type =>
            Generate_Array_Type (Base_T);

         when E_Access_Type |
              E_General_Access_Type |
              E_Anonymous_Access_Type =>

            Generate_Object_Access_Type (Base_T);

         when E_Access_Subprogram_Type =>
            Generate_Subprogram_Access_Type (Base_T);

         when E_Private_Type |
              E_Limited_Private_Type |
              E_Record_Type_With_Private =>
            null;

         when E_Task_Type | E_Protected_Type =>
            Generate_Concurrent_Type (Base_T);

         when others =>
            if Debug_Flag_JJ then
               Osint.Fail
                 ("*** Unsupported type: " & Entity_Kind'Image (Ekind (T)));
            else
               pragma Assert (False);
               raise Program_Error;
            end if;
      end case;
   end Translate_Type;

   -----------------------
   -- Translate_Subtype --
   -----------------------

   procedure Translate_Subtype (S : Entity_Id) is
   begin
      case Ekind (S) is
         when E_Array_Subtype | E_String_Subtype =>
            Generate_Array_Subtype (S);
         when E_Enumeration_Subtype =>
            null;
         when E_Floating_Point_Subtype =>
            null;
         when E_Ordinary_Fixed_Point_Subtype =>
            null;
         when E_Decimal_Fixed_Point_Subtype =>
            null;
         when E_Record_Subtype =>
            Generate_Record_Subtype (S);
         when E_Signed_Integer_Subtype | E_Modular_Integer_Subtype =>
            Generate_Integer_Subtype (S);
         when E_Access_Subtype =>
            null;
         when E_Private_Subtype | E_Limited_Private_Subtype =>
            null;
         when E_Record_Subtype_With_Private =>
            null;
         when E_Class_Wide_Subtype =>
            null;
         when E_Task_Subtype =>
            null;
         when E_Protected_Subtype =>
            null;
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Translate_Subtype;

   ------------------------
   -- Allocate_Component --
   ------------------------

   procedure Allocate_Component (Comp : Entity_Id; Obj : Local_Var_Id) is
      Comp_Subt : constant Entity_Id := Full_Subtype (Comp);
      Comp_Type : constant Entity_Id := Full_Type (Comp_Subt);

   begin
      --  Ignore any _parent field

      if Chars (Comp) = Name_uParent then
         return;
      end if;

      --  Generate code to allocate the component if its type is composite

      case Ekind (Comp_Type) is
         when E_Record_Type | E_Task_Type | E_Protected_Type =>
            Gen_Load_Local (Obj);
            Gen_New_Object (Class_Of_Type (JVM_Type (Comp_Subt)));

            --  Invoke the default <init> constructor of the type.
            --  If the type has an associated _init_proc routine then
            --  that will be perform any composite component allocation
            --  (unless the component has an explicit initialization, in
            --  Which case the subcomponent allocation will happen via
            --  the deep copy on initialization).

            Gen_Duplicate;
            Gen_Invoke_Init (Comp_Type);

            Gen_Put_Field (JVM_Field (Comp));

         when E_Array_Type | E_String_Type =>
            declare
               Index     : Node_Id := First_Index (Comp_Subt);
               Elt_Type  : Entity_Id
                 := Full_Type (Component_Type (Comp_Type));
               Array_LV  : Local_Var_Id;

            begin
               Gen_Load_Local (Obj);

               if Number_Dimensions (Comp_Subt) = 1 then
                  Load_Index_Length (Index, Obj);
                  Gen_New_Array (JVM_Type (Comp_Type));

               --  Multidimensional array case

               else
                  while Present (Index) loop
                     Load_Index_Length (Index, Obj);
                     Next_Index (Index);
                  end loop;

                  Gen_New_Multiarray (JVM_Type (Comp_Type));
               end if;

               --  If the array component has composite or aliased components,
               --  then we call Allocate_Array_Components to allocate them.
               --  The record object address is popped and the array address
               --  is saved in a temporary to avoid problems with stack checks
               --  when generating the loop in Allocate_Array_Components.
               --  It would be better to create an allocation routine for
               --  each array type with allocatable components and simply
               --  call that rather than generating the allocation in line.
               --  ???

               if Ekind (Elt_Type) in Composite_Kind
                 or else Has_Aliased_Components (Comp_Type)
               then
                  Array_LV
                    := New_Local_Var ("_arry_cmp", JVM_Type (Comp_Type));

                  Gen_Store_Local (Array_LV);
                  Gen_Pop;
                  Allocate_Array_Components (Comp_Type, Array_LV);
                  Gen_Load_Local (Obj);
                  Gen_Load_Local (Array_LV);
               end if;

               Gen_Put_Field (JVM_Field (Comp));
            end;

         when Wrappable_Kind =>

            --  If this is an aliased component with a wrapper type, then
            --  allocate its wrapper object.

            if Is_Aliased (Comp) then
               Gen_Load_Local (Obj);
               Gen_Default_Object (Class_Of_Type (Wrapper_Type (Comp)));
               Gen_Put_Field (JVM_Field (Comp));
            end if;

         when others =>
            null;
      end case;
   end Allocate_Component;

   -----------------------------
   -- Allocate_Component_List --
   -----------------------------

   procedure Allocate_Component_List (Variant : Node_Id; Obj : Local_Var_Id) is
   begin
      Allocate_Record_Components (Component_List (Variant), Obj);
   end Allocate_Component_List;

   --------------------------------
   -- Allocate_Record_Components --
   --------------------------------

   procedure Allocate_Record_Components
     (Comp_List : Node_Id;
      Obj       : Local_Var_Id)
   is
      Comp_Decl   : Node_Id;
      Variant_Prt : Node_Id;
      Var_Discrim : Node_Id;

   begin
      if Present (Comp_List) then
         Comp_Decl   := First_Non_Pragma (Component_Items (Comp_List));
         Variant_Prt := Variant_Part (Comp_List);

         while Present (Comp_Decl) loop
            Allocate_Component (Defining_Identifier (Comp_Decl), Obj);
            Next_Non_Pragma (Comp_Decl);
         end loop;

         if Present (Variant_Prt) then
            Var_Discrim := Name (Variant_Prt);

            --  First load the discriminant that controls the variant part

            --  If we're inside an init_proc, then the discriminant
            --  is loaded by evaluating its associated discriminal.
            --  We check the Base_Init_Proc of the discriminant's
            --  parent record type rather than just checking the
            --  name of the current method is Name_uInit_Proc since
            --  the name of the method may have been expanded to
            --  include the enclosing scope as a prefix.

            if JVM_Method (Base_Init_Proc (Scope (Entity (Var_Discrim))))
              = Current_Method
            then
               Evaluate_Expr (Var_Discrim);

            --  Otherwise, we have to get the discriminant from the
            --  record object itself.

            else
               Gen_Load_Local (Obj);
               Gen_Get_Field (JVM_Field (Entity (Var_Discrim)));
            end if;

            --  Now generate a switch statement that will cover the
            --  variants and perform allocation for each variant's
            --  associated set of composite components.

            Generate_Switch
              (Variants (Variant_Prt), Allocate_Component_List'Access, Obj);
         end if;
      end if;
   end Allocate_Record_Components;

   -------------------------------
   -- Allocate_Array_Components --
   -------------------------------

   procedure Allocate_Array_Components
     (Arr_Type  : Entity_Id;
      Obj       : Local_Var_Id)
   is
      Comp_Type   : constant Entity_Id
        := Full_Subtype (Component_Type (Arr_Type));
      Check_State : Boolean;

      procedure Traverse_Subarrays (Dimensions : Pos_8);
      --  Generates loops to traverse a multidimensional array
      --  and perform composite component allocation in the
      --  innermost loop.

      procedure Allocate_Array_Component (Comp_Type : Entity_Id);
      --  Generates code to allocate the object for a composite
      --  or aliased scalar component. The reference to the new
      --  object is left on top of the stack.

      procedure Allocate_Array_Component (Comp_Type : Entity_Id) is
      begin
         case Ekind (Full_Type (Comp_Type)) is
            when E_Record_Type | E_Task_Type | E_Protected_Type =>

               --  Allocate and construct the array component

               Gen_Default_Object (JVM_Class (Full_Type (Comp_Type)));

            when E_Array_Type | E_String_Type =>
               declare
                  Index       : Node_Id := First_Index (Comp_Type);
                  Arr_LV      : Local_Var_Id;
                  Check_State : Boolean;

               begin
                  Load_Index_Length (Index);
                  Gen_New_Array (JVM_Type (Comp_Type));

                  --  If the array component's component type is composite
                  --  or aliased, then allocate the array's subcomponents.

                  if Ekind (Full_Type (Component_Type (Comp_Type)))
                      in Composite_Kind
                    or else Has_Aliased_Components (Comp_Type)
                  then
                     Arr_LV
                       := New_Local_Var ("_arry_cmp", JVM_Type (Comp_Type));
                     Gen_Store_Local (Arr_LV);

                     --  Note that stack checking must be suppressed around
                     --  this allocation, since it will involve branch and
                     --  label generation, and the stack is currently loaded
                     --  with the outer array component reference and index.
                     --  This is covered by the suppression that occurs at
                     --  the outer level of Allocate_Array_Components, but
                     --  we leave this redundant suppression in place in
                     --  case that were ever to change.

                     Suppress_Stack_Checking (Check_State);
                     Allocate_Array_Components (Comp_Type, Arr_LV);
                     Restore_Stack_Checking (Check_State);

                     Gen_Load_Local (Arr_LV);
                  end if;
               end;

            when Wrappable_Kind =>

               --  If this is an aliased component with a wrapper type, then
               --  allocate a wrapper object.

               if Has_Aliased_Components (Arr_Type) then
                  Gen_Default_Object
                    (Class_Of_Type (Wrapper_Type (Comp_Type)));
               end if;

            when others =>
               null;
         end case;
      end Allocate_Array_Component;

      procedure Traverse_Subarrays (Dimensions : Pos_8) is
         Arr_Temp  : constant Local_Var_Id
           := New_Local_Var ("_arr_tmp", JVM_Type (Arr_Type));
         Index     : constant Local_Var_Id
           := New_Local_Var ("_loop_index", Int_Type);
         Length    : constant Local_Var_Id
           := New_Local_Var ("_index_max", Int_Type);
         Loop_Head : constant Label_Id := New_Label;
         Loop_Exit : constant Label_Id := New_Label;

      begin
         --  Generate a loop to load references to each of the current
         --  dimension's subarrays (or to allocate components when the
         --  deepest subarray level has been reached).

         Gen_Store_Local (Arr_Temp);

         --  Generate a loop to traverse the subarrays

         --  Initialize loop index to zero

         Gen_Push_Int (Uint_0);
         Gen_Store_Local (Index);

         Gen_Load_Local (Arr_Temp);
         Gen_Array_Length;
         Gen_Store_Local (Length);

         Gen_Label (Loop_Head);

         --  Check for end of loop (Index = Length)

         Gen_Load_Local (Index);
         Gen_Load_Local (Length);
         Gen_Compare_Branch_Equal (Loop_Exit);

         --  When Dimensions = 1 we are at the deepest dimension of
         --  the arrays so we allocate the individual component objects
         --  store the references into the array.

         if Dimensions = 1 then
            --  Load the array parameter and the current index in
            --  preparation for storing the reference to the new
            --  component back into the array after the allocation.

            Gen_Load_Local (Arr_Temp);
            Gen_Load_Local (Index);

            Allocate_Array_Component (Comp_Type);

            Gen_Store_Array_Element;

         --  If Dimensions > 1, then index the current subarray and load
         --  a reference to the next subarray level, and recurse to generate
         --  a traversal of the subarrays of the next dimension.

         else
            Gen_Load_Local (Arr_Temp);
            Gen_Load_Local (Index);
            Gen_Load_Subarray_Reference;

            Traverse_Subarrays (Dimensions - 1);
         end if;

         --  Increment loop index and interate

         Gen_Incr_Local (Index, Uint_1);
         Gen_Goto (Loop_Head);

         Gen_Label (Loop_Exit);
      end Traverse_Subarrays;

   --  Start processing for Allocate_Array_Components

   begin
      --  Suppress stack checking around the allocation
      --  since it will involve branch and label generation,
      --  and in general this procedure can be called from
      --  contexts where the stack is not empty (in particular,
      --  this can occur in the case of allocators). It's
      --  better to just do this here rather than leave it
      --  up to callers to worry about whether to suppress.

      Suppress_Stack_Checking (Check_State);

      Gen_Load_Local (Obj);
      Traverse_Subarrays (Pos_8 (Number_Dimensions (Arr_Type)));

      Restore_Stack_Checking (Check_State);
   end Allocate_Array_Components;

   -----------------------------------
   -- Allocate_Composite_Components --
   -----------------------------------

   procedure Allocate_Composite_Components
     (Ada_Type : Entity_Id;
      Obj      : Local_Var_Id)
   is
      Rep_Type  : constant Entity_Id := Full_Type (Ada_Type);
      Type_Def  : Node_Id;

   begin
      case Ekind (Rep_Type) is
         when E_Record_Type =>
            Type_Def := Type_Definition (Parent (Rep_Type));

            if Nkind (Type_Def) = N_Record_Definition then
               Allocate_Record_Components (Component_List (Type_Def), Obj);

            --  Type_Def must be N_Derived_Type_Definition

            elsif Present (Record_Extension_Part (Type_Def)) then
               --  Allocate the components of the parent type, but only in the
               --  case where we're not inside the type's init_proc, because
               --  in the init_proc the parent components will be allocated by
               --  expander-generated calls to the parent's init_proc (tagged
               --  types always have an init_proc).

               if JVM_Method (Base_Init_Proc (Rep_Type)) /= Current_Method then
                  Allocate_Composite_Components
                    (Full_Type (Etype (Rep_Type)), Obj);
               end if;

               --  Allocate the components of the extension part
               Allocate_Record_Components
                 (Component_List (Record_Extension_Part (Type_Def)), Obj);
            end if;

         when E_Array_Type =>
            if Ekind (Full_Type (Component_Type (Rep_Type)))
                 in Composite_Kind
              or else Has_Aliased_Components (Rep_Type)
            then
               Allocate_Array_Components (Rep_Type, Obj);
            end if;

         when others =>
            null;
      end case;
   end Allocate_Composite_Components;

   -------------------------------
   -- Generate_Invoke_Deep_Copy --
   -------------------------------

   procedure Gen_Invoke_Deep_Copy (Typ : Entity_Id) is
   begin
      Gen_Invoke_Static
        (Method (Deep_Copy_Class (Typ), Deep_Copy_Method_Name (Typ)));
   end Gen_Invoke_Deep_Copy;

   --------------------------------
   -- Generate_Invoke_Deep_Clone --
   --------------------------------

   procedure Gen_Invoke_Deep_Clone (Typ : Entity_Id) is
   begin
      Gen_Invoke_Virtual
        (Method (Deep_Clone_Class (Typ), Deep_Clone_Method_Name (Typ)));

      --  A type cast is necessary because the result type of the
      --  deep clone function will be an ultimate ancestor type, which
      --  may be different than the expected type of the call context.

      Gen_Check_Cast (Class_Of_Type (JVM_Type (Typ)));
   end Gen_Invoke_Deep_Clone;

   --------------------------
   -- Generate_Invoke_Init --
   --------------------------

   procedure Gen_Invoke_Init (Typ : Entity_Id) is
   begin
      --  If the type is declared within a subprogram and has
      --  no init proc, then the type has an <init> method with
      --  a static link that must be called to ensure allocation
      --  of any composite_components.

      if Present (Enclosing_Subprogram (Typ))
        and then not Present (Base_Init_Proc (Typ))
      then
         Load_Static_Link (Enclosing_Method (Typ));
         Gen_Invoke_Special
           (Method (JVM_Class (Typ), "<init>",
            Result  => Void_Type,
            Param_0 => JVM_Type (Typ),
            Param_1 => Top_Type));

      --  Otherwise just call the type's no-arg constructor

      else
         Gen_Invoke_Special
           (Default_Constructor (JVM_Class (Typ)));
      end if;
   end Gen_Invoke_Init;

   ------------------------
   -- Generate_Deep_Copy --
   ------------------------

   procedure Generate_Deep_Copy (R : Entity_Id) is
      R_Type     : constant Type_Id      := JVM_Type (R);
      R_Class    : constant Class_Id     := Class_Of_Type (R_Type);
      Deep_Copy  : constant Method_Id    := Method (R_Class, "_deep_copy");
      Target     : Local_Var_Id          := First_Local_Var (Deep_Copy);
      Source     : constant Local_Var_Id := Next_Local_Var (Target);
      Result     : Local_Var_Id;
      Temp_Lbl   : Label_Id;
      Component  : Entity_Id;
      Comp_Type  : Entity_Id;
      Comp_Field : Field_Id;

   begin
      --  Start generation of the _deep_copy method. The profile for
      --  this operation is:
      --
      --    public static R_Type _deep_copy (R_Type <target>, R_Type <source>)

      Open_Method (Deep_Copy);
      Set_Current_Method (Deep_Copy);
      Method_Stack.Push (Deep_Copy);

      --  The source record value will be copied into the Result object

      Result := New_Local_Var ("_result", R_Type);

      --  If the type is abstract then the following special code for handling
      --  variant-dependent components within mutable records is not needed,
      --  because components of an enclosing record can never have an abstract
      --  type. However, abstract types can still require a deep copy method
      --  because assignments to the "abstract" portion of a nonabstract object
      --  are still possible (e.g., via parameters of an abstract type's
      --  primitive operations or through conversions).

      if not Is_Abstract (R) then
         --  If Source = null, then simply return null as the result.
         --  This case can occur when there is a component of the
         --  record type in an unused variant of an enclosing source
         --  record, in which case the null reference must be copied.

         Temp_Lbl := New_Label;
         Gen_Load_Local (Source);
         Gen_Push_Null;
         Gen_Compare_Branch_Not_Equal (Temp_Lbl);
         Gen_Push_Null;
         Gen_Method_Return;
         Gen_Label (Temp_Lbl);

         --  Initialize Result from Target. Then, if Target = null, allocate
         --  a new record object, invoke its default constructor, and
         --  initialize Result to refer to the new object. This case can
         --  occur when assigning an enclosing variant record to a target
         --  for which the record component is null (due to an inactive
         --  variant in the target). It can also occur when initializing
         --  an object declaration whose default constructor does not perform
         --  component allocation (in fact, this method of using _deep_copy
         --  to allocate the target object could be used for all initialized
         --  composite object creations, though we don't currently take
         --  advantage of that ???).

         Temp_Lbl := New_Label;
         Gen_Load_Local (Target);
         Gen_Duplicate;
         Gen_Store_Local (Result);
         Gen_Push_Null;
         Gen_Compare_Branch_Not_Equal (Temp_Lbl);
         Gen_Default_Object (R_Class);
         Gen_Store_Local (Result);
         Gen_Label (Temp_Lbl);

      --  In the abstract case we still need to initialize Result from Target.

      else
         Gen_Load_Local (Target);
         Gen_Store_Local (Result);
      end if;

      Target := Result;  -- Let Target be same as Result (for clarity below)

      --  For each component of the record type, assign the value of
      --  the component in Source to the corresponding component of
      --  the Result object. In the case of composite components, this
      --  involves invoking the _deep_copy method of the component's
      --  type.

      Component := First_Entity (R);

      while Present (Component) loop
         if (Ekind (Component) = E_Component
           or else Ekind (Component) = E_Discriminant)
             and then Chars (Component) /= Name_uTag
         then
            Comp_Type := Full_Type (Component);
            Comp_Field := JVM_Field (Component);

            case Ekind (Comp_Type) is

               --  For components which themselves are records,
               --  invoke the component type's deep copy operation.

               when E_Record_Type =>

                  --  There are cases where a deep copy routine is
                  --  generated for a type derived from a Java API
                  --  type (e.g., java.lang.Object), even though
                  --  the type is limited. Currently we don't reliably
                  --  know whether the containing record type really
                  --  needs a deep copy (see comments at the end
                  --  of Generate_Record_Type), so this defensive
                  --  check is performed here to avoid trying to
                  --  call deep copy for a Java type. ???

                  if not Is_Imported (Scope (Comp_Type)) then

                     --  If the component is _parent, then simply call
                     --  the deep copy of the parent type and pop the
                     --  result (no component selection is needed).

                     if Chars (Component) = Name_uParent then
                        Gen_Load_Local (Target);
                        Gen_Load_Local (Source);
                        Gen_Invoke_Deep_Copy (Comp_Type);
                        Gen_Pop;

                     else
                        --  Load the reference to the target object,
                        --  duplicating it in preparation for storing
                        --  the result of the call to the deep copy.

                        Gen_Load_Local (Target);
                        Gen_Duplicate;

                        --  Load the target field

                        Gen_Get_Field (Comp_Field);

                        --  Load the source field

                        Gen_Load_Local (Source);
                        Gen_Get_Field (Comp_Field);

                        --  Call deep copy for the component's type and
                        --  store the result in the target component. (The
                        --  store back into Target.Component is needed to
                        --  handle cases involving variant records, where
                        --  target component may be null, but the source
                        --  component is non-null.)

                        Gen_Invoke_Deep_Copy (Comp_Type);
                        Gen_Put_Field (Comp_Field);
                     end if;

                  end if;

               --  For scalar arrays we simply copy the array using
               --  java.lang.System.arraycopy. For the case of composite
               --  components we invoke the deep copy method of the
               --  array's component type.

               when E_Array_Type | E_String_Type =>

                  declare
                     A_Comp_Type    : constant Entity_Id
                       := Full_Type (Component_Type (Comp_Type));

                     Test_Lbl       : Label_Id;
                     Copy_Lbl       : Label_Id;
                     Done_Lbl       : Label_Id;
                     Alloc_Targ_Lbl : Label_Id;

                  begin
                     --  If the component type is a limited record or
                     --  a concurrent type, then the array type does
                     --  not need to be copied (and no deep copy
                     --  method will have been generated for it).
                     --  It should not be possible to reach this code
                     --  in that case, but currently we don't reliably
                     --  know whether the containing record type really
                     --  needs a deep copy (see comments at the end
                     --  of Generate_Record_Type), so this defensive
                     --  check is performed here. ???

                     if not Is_Limited_Record (A_Comp_Type)
                       and then not Is_Concurrent_Type (A_Comp_Type)
                     then
                        Test_Lbl       := New_Label;
                        Copy_Lbl       := New_Label;
                        Done_Lbl       := New_Label;
                        Alloc_Targ_Lbl := New_Label;

                        --  If Source.Array_Comp /= null then jump to
                        --  the test for Target.Array_Comp

                        Gen_Load_Local (Source);
                        Gen_Get_Field (Comp_Field);
                        Gen_Push_Null;
                        Gen_Compare_Branch_Not_Equal (Test_Lbl);

                        --  Source.Array_Comp = null, so set Target.Array_Comp
                        --  to null as well and branch past the array copy.

                        Gen_Load_Local (Target);
                        Gen_Push_Null;
                        Gen_Put_Field (Comp_Field);
                        Gen_Goto (Done_Lbl);

                        --  If Target.Array_Comp = null then jump to code that
                        --  will allocate a new target array.

                        Gen_Label (Test_Lbl);
                        Gen_Load_Local (Target);
                        Gen_Get_Field (Comp_Field);
                        Gen_Push_Null;
                        Gen_Compare_Branch_Equal (Alloc_Targ_Lbl);

                        --  If the lengths of the target and source components
                        --  match then proceed to the code for copying the
                        --  array.

                        Gen_Load_Local (Target);
                        Gen_Get_Field (Comp_Field);
                        Gen_Array_Length;
                        Gen_Load_Local (Source);
                        Gen_Get_Field (Comp_Field);
                        Gen_Array_Length;
                        Gen_Compare_Branch_Equal (Copy_Lbl);

                        --  Otherwise, either Target.Array_Comp = null or
                        --  the source and target have different lengths
                        --  (can only occur if the target is dependent on
                        --  a discriminant), so allocate a new array whose
                        --  length is that of Source.Array_Comp and initialize
                        --  Target.Array_Comp to the new array.

                        Gen_Label (Alloc_Targ_Lbl);

                        Gen_Load_Local (Result);
                        Gen_Load_Local (Source);
                        Gen_Get_Field (Comp_Field);

                        if Number_Dimensions (Comp_Type) = 1 then
                           Gen_Array_Length;
                           Gen_New_Array (JVM_Type (Comp_Type));

                        --  Handle multidimensional allocation case. This
                        --  requires traversal of each dimension of the source
                        --  to load the length of each dimension. This isn't
                        --  quite right yet for the case of null arrays,
                        --  so we may need to add checks for zero subarray
                        --  lengths, which is a real pain! ???

                        else
                           for D in 2 .. Number_Dimensions (Comp_Type) loop
                              Gen_Duplicate;
                              Gen_Array_Length;
                              Gen_Swap;
                              Gen_Push_Int (Uint_0);
                              Gen_Load_Subarray_Reference;
                           end loop;

                           Gen_Array_Length;

                           Gen_New_Multiarray (JVM_Type (Comp_Type));
                        end if;

                        Gen_Put_Field (Comp_Field);

                        Gen_Label (Copy_Lbl);

                        --  When the array component is one-dimensional and has
                        --  elementary components, then we generate a call to
                        --  java.lang.System.arraycopy, passing the following
                        --  parameters:
                        --
                        --    Source.Array_Comp, 0,
                        --    Target.Array_Comp, 0, Target.Array_Comp'Length

                        if Ekind (A_Comp_Type) in Elementary_Kind
                          and then Number_Dimensions (Comp_Type) = 1
                        then
                           Gen_Load_Local (Source);
                           Gen_Get_Field (Comp_Field);
                           Gen_Push_Int (Uint_0);
                           Gen_Load_Local (Target);
                           Gen_Get_Field (Comp_Field);
                           Gen_Duplicate;
                           Gen_Array_Length;
                           Gen_Push_Int (Uint_0);
                           Gen_Swap;
                           Gen_Invoke_API_Method (System_arraycopy);

                        --  For arrays of composite components, we invoke the
                        --  array type's deep copy method.

                        else
                           --  Load the target and source references as well
                           --  as the source length and zeros for the indexes.

                           Gen_Load_Local (Target);
                           Gen_Get_Field (Comp_Field);
                           Gen_Push_Int (Uint_0);
                           Gen_Load_Local (Source);
                           Gen_Get_Field (Comp_Field);
                           Gen_Duplicate;
                           Gen_Array_Length;
                           Gen_Push_Int (Uint_0);

                           --  Call the array type's deep copy method and
                           --  pop the result (since target will never be
                           --  null due to the earlier allocation code).

                           Gen_Invoke_Deep_Copy (Comp_Type);
                           Gen_Pop;
                        end if;

                        Gen_Label (Done_Lbl);
                     end if;
                  end;

               when others =>
                  --  If the component is aliased and has a wrapper,
                  --  then we have to load and store from the 'all'
                  --  fields of the source and target wrapper objects.

                  if Has_Wrapper (Component) then
                     Gen_Load_Local (Result);
                     Gen_Get_Field  (Comp_Field);
                     Gen_Load_Local (Source);
                     Gen_Get_Field  (Comp_Field);
                     Gen_Get_Field  (Wrapper_Field (Component));
                     Gen_Put_Field  (Wrapper_Field (Component));

                  --  In all other cases we simply copy the field across

                  else
                     Gen_Load_Local (Result);
                     Gen_Load_Local (Source);
                     Gen_Get_Field  (Comp_Field);
                     Gen_Put_Field  (Comp_Field);
                  end if;
            end case;
         end if;

         Component := Next_Entity (Component);
      end loop;

      --  Return Result, which denotes the fully updated target object

      Gen_Load_Local (Result);
      Gen_Method_Return;

      --  Complete the deep copy method

      Method_Stack.Pop;
      Close_Method (Deep_Copy);

      if not Method_Stack.Empty then
         Set_Current_Method (Method_Stack.Top);
      end if;
   end Generate_Deep_Copy;

   -------------------------
   -- Generate_Deep_Clone --
   -------------------------

   procedure Generate_Deep_Clone (R : Entity_Id) is
      R_Type     : constant Type_Id      := JVM_Type (R);
      R_Class    : constant Class_Id     := Class_Of_Type (R_Type);
      Deep_Clone : constant Method_Id    := Method (R_Class, "_deep_clone");
      Source     : constant Local_Var_Id := This_Local (Deep_Clone);

   begin
      --  Start generation of the _deep_clone method. The profile for
      --  this virtual method is:
      --
      --    public R_Type _deep_clone ()

      Open_Method (Deep_Clone);
      Set_Current_Method (Deep_Clone);
      Method_Stack.Push (Deep_Clone);

      --  Call the deep copy operation for type R with a null target
      --  and the source object that was passed in.

      Gen_Push_Null;
      Gen_Load_Local (Source);
      Gen_Invoke_Deep_Copy (R);

      --  Return the result of the deep copy, which denotes a new result object
      --  copied from the source parameter.

      Gen_Method_Return;

      --  Complete the deep copy method

      Method_Stack.Pop;
      Close_Method (Deep_Clone);

      if not Method_Stack.Empty then
         Set_Current_Method (Method_Stack.Top);
      end if;
   end Generate_Deep_Clone;

   ----------------------------
   -- Generate_Discrete_Type --
   ----------------------------

   procedure Generate_Discrete_Type (Discr_Type : Entity_Id) is
   begin
      case Ekind (Discr_Type) is
         when E_Enumeration_Type =>

            --  Create the literal name table for the enumeration type

            if Present (Lit_Name_Table (Discr_Type)) then
               declare
                  Name_Table : Entity_Id := Lit_Name_Table (Discr_Type);
                  Is_Global  : Boolean := Is_Global_Entity (Name_Table);
                  Obj_Field  : Field_Id;
                  Local_Var  : Local_Var_Id;
                  Literal    : Entity_Id;
                  Lit_Index  : Uint := Uint_0;
                  Alloc_LV   : Local_Var_Id;
                  Ref_Class  : constant Class_Id
                    := Class_Of_Type
                         (JVM_Type (Component_Type (Etype (Name_Table))));

               begin
                  if Is_Global then
                     Declare_Field (Current_Compilation_Class, Name_Table);
                     Obj_Field := JVM_Field (Name_Table);
                  else
                     Declare_Local_Variable (Name_Table);
                     Local_Var := JVM_Local_Var (Name_Table);
                  end if;

                  --  Load the size of the table and create the table object
                  --  (the size is computed as pos(enum_type'last) + 1)

                  Gen_Push_Int
                    (Intval (Table_High_Bound (Etype (Name_Table))) + 1);
                  Gen_New_Array (JVM_Type (Name_Table));

                  if Is_Global then
                     Gen_Put_Static_Field (Obj_Field);
                  else
                     Gen_Store_Local (Local_Var);
                  end if;

                  --  Now initialize the table by allocating string literals
                  --  based on the names of the enumeration literals.

                  Literal := First_Literal (Discr_Type);

                  Alloc_LV
                    := New_Local_Var ("_alloc_tmp", Type_Of (Ref_Class));

                  while Present (Literal) loop
                     declare
                        Lit_String : String := Name_String (Chars (Literal));

                     begin
                        To_Upper (Lit_String);

                        Gen_Default_Object (Ref_Class);
                        Gen_Store_Local (Alloc_LV);

                        --  Generate the string literal and save its reference
                        --  in the 'all' field of the string access value.

                        Gen_Load_Local (Alloc_LV);
                        Evaluate_String_Literal
                          (Str_Id (Lit_String), Standard_String);
                        Gen_Put_Field (Field (Ref_Class, "all"));

                        --  Push the low bound (1) of the literal and save
                        --  in the 'first' field of the string access value.

                        Gen_Load_Local (Alloc_LV);
                        Gen_Push_Int (Uint_1);
                        Gen_Put_Field (Field (Ref_Class, "first"));

                        --  Push the high bound of the literal (its length) and
                        --  save in 'last' field of the string access value.

                        Gen_Load_Local (Alloc_LV);
                        Gen_Push_Int (UI_From_Int (Lit_String'Length));
                        Gen_Put_Field (Field (Ref_Class, "last"));

                        --  Now save the reference to the string access value
                        --  in the current name table entry.

                        if Is_Global then
                           Gen_Get_Static_Field (Obj_Field);
                        else
                           Gen_Load_Local (Local_Var);
                        end if;
                        Gen_Push_Int (Lit_Index);
                        Gen_Load_Local (Alloc_LV);
                        Gen_Store_Array_Element;

                        Lit_Index := Lit_Index + 1;
                        Literal := Next_Literal (Literal);
                     end;
                  end loop;
               end;
            end if;

         when others =>
            null;

      end case;
   end Generate_Discrete_Type;

   --------------------------
   -- Generate_Record_Type --
   --------------------------

   procedure Generate_Record_Type (R_Type : Entity_Id) is
      Record_Class : Class_Id;
      Component    : Entity_Id := First_Entity (R_Type);
      Desig_Type   : Entity_Id;
      Discrim      : Entity_Id;
      Intface_Prim : Elmt_Id;
      Impl_Prim    : Elmt_Id;
      Overridden   : Boolean;

   begin
      if JVM_Entity (R_Type) = Null_Type then
         Declare_Record_Class (R_Type);
      end if;
      Record_Class := Class_Of_Type (JVM_Entity (R_Type));

      --  We don't yet have proper support for Adjust and Finalize of
      --  objects with controlled components, which requires developing
      --  a revised version of System.Finalization_Implementation that
      --  does not rely on low-level knowledge of the links within
      --  controlled composite objects (uses of 'Address and unchecked
      --  conversion not supported on the JVM). For now we issue a
      --  warning for declaring types with controlled components.
      --  We relax the warning in the case of protected types with
      --  entries, though those may not be completely safe, but it
      --  avoids lots of superfluous warnings. ???

      if Has_Controlled_Component (R_Type)
        and then (not Present (Corresponding_Concurrent_Type (R_Type))
          or else not Has_Entries
                        (Full_Type (Corresponding_Concurrent_Type (R_Type))))
      then
         Error_Msg_N
           ("types with controlled components not fully supported?", R_Type);
      end if;

      --  Following is a rather complicated check to ensure that a type
      --  that implements any Java interfaces overrides each of the
      --  primitives inherited from the interface. This is an expensive
      --  check, but only occurs on the type declaration of discriminated
      --  types with convention Java.

      if Convention (R_Type) = Convention_Java
        and then Has_Discriminants (R_Type)
      then
         --  Skip any 'self' discriminant in case this is an interface
         --  that implements other interfaces.

         Discrim := First_Discriminant (R_Type);
         if Name_String (Chars (Discrim)) = "self" then
            Next_Discriminant (Discrim);
         end if;

         while Present (Discrim) loop
            if Ekind (Etype (Discrim)) in Access_Kind then
               Desig_Type := Directly_Designated_Type (Etype (Discrim));

               --  The designated type of the discriminant is a Java
               --  interface, so check that all of its primitives
               --  have been overridden.

               if Is_Tagged_Type (Desig_Type)
                 and then Is_Interface (JVM_Class (Desig_Type))
               then
                  if Is_Class_Wide_Type (Desig_Type) then
                     Desig_Type := Root_Type (Desig_Type);
                  end if;

                  Desig_Type := Full_Type (Desig_Type);

                  if Has_Primitive_Operations (Desig_Type) then
                     Intface_Prim
                       := First_Elmt (Primitive_Operations (Desig_Type));

                     --  Traverse the set of interface primitives

                     while Present (Intface_Prim) loop
                        if Comes_From_Source (Node (Intface_Prim)) then
                           Overridden := False;

                           --  Traverse the set of potentially overriding
                           --  primitives.

                           if Has_Primitive_Operations (R_Type) then
                              Impl_Prim
                                := First_Elmt (Primitive_Operations (R_Type));
                           end if;

                           while Present (Impl_Prim) loop
                              if Overrides_Interface_Op
                                   (Node (Impl_Prim), Node (Intface_Prim))
                              then
                                 Overridden := True;
                                 exit;
                              end if;

                              Next_Elmt (Impl_Prim);
                           end loop;

                           --  If the interface primitive was not overridden
                           --  then issue an error indicating the nonoverridden
                           --  primitive.

                           if not Overridden then
                              Error_Msg_Sloc := Sloc (Node (Intface_Prim));
                              Error_Msg_NE
                                ("must override interface operation &#",
                                 R_Type, Node (Intface_Prim));
                           end if;
                        end if;

                        Next_Elmt (Intface_Prim);
                     end loop;
                  end if;
               end if;
            end if;

            Next_Discriminant (Discrim);
         end loop;
      end if;

      --  Declare the fields of the record class

      while Present (Component) loop
         if (Ekind (Component) = E_Component
           or else Ekind (Component) = E_Discriminant)
             and then Chars (Component) /= Name_uParent
             and then Chars (Component) /= Name_uTag
         then
            --  We disallow composite components within types with Java
            --  convention, since such components require specialized
            --  treatment to allocate and initialize them, which is
            --  tricky to support in the presence of user-defined
            --  constructors (the allocation and initialization must
            --  occur before any user code can reference the component,
            --  but a constructor must be executed immediately after
            --  allocation of the containing object). This could
            --  potentially be supported properly by generating the
            --  code to allocate and initialize the components inside
            --  any user-defined defined constructors, but that's tricky,
            --  so for now we simply disallow such types. We also disallow
            --  default initialization of elementary components for
            --  the same reason. Note that the prohibition against
            --  composite components includes disallowing controlled,
            --  task, and protected components. ???

            if Convention (R_Type) = Convention_Java then
               if Is_Composite_Type (Full_Type (Component))
                 and then Comes_From_Source (Component)
               then
                  Error_Msg_N
                    ("composite component not allowed in Java-convention type",
                     Component);

               elsif Nkind (Parent (Component)) = N_Component_Declaration
                 and then Present (Expression (Parent (Component)))
               then
                  Error_Msg_N
                    ("initialization not allowed in Java-convention type",
                     Expression (Parent (Component)));
               end if;
            end if;

            --  For now we disallow the declaration of tagged components whose
            --  type has convention Java. This prevents problems with types
            --  which do not have no-arg constructors. Perhaps we should
            --  relax this restriction at some point, checking for the
            --  presence of a no-arg constructor for the type. Would it
            --  be better to simply change this to a warning for now and
            --  leave it up to the user to determine the safety rather
            --  than being overly restrictive ???

            if Is_Tagged_Type (Full_Type (Component))
              and then Convention (Full_Type (Component)) = Convention_Java
            then
               Error_Msg_N
                 ("component of Java-convention tagged type not allowed",
                  Component);
               Error_Msg_N
                 ("must use access values to reference Java objects",
                  Component);
            end if;

            --  Access discriminants that denote a Java interface type
            --  are treated specially and do not represent real fields,
            --  so they are not declared, but rather serve to indicate
            --  that the type implements the interface.

            if Ekind (Component) = E_Discriminant
              and then Ekind (Etype (Component)) in Access_Kind
            then
               Desig_Type := Directly_Designated_Type (Etype (Component));

               if Convention (R_Type) = Convention_Java
                 and then Is_Tagged_Type (Desig_Type)
                 and then Is_Interface (JVM_Class (Desig_Type))
               then
                  Associate_Interface (Record_Class, JVM_Class (Desig_Type));

               --  If the record type itself is associated with a Java
               --  interface, then we also don't declare the discriminant,
               --  since it must be the special "self" discriminant that
               --  is used denote a java.lang.Object of the interface type.

               elsif not Is_Interface (Record_Class) then
                  Declare_Field (Record_Class, Component);
               end if;

            else
               Declare_Field (Record_Class, Component);
            end if;
         end if;
         Component := Next_Entity (Component);
      end loop;

      --  Note that the class file for the record type is started but
      --  not completed here. Processing of later declarations may add
      --  methods and fields to the class, which will eventually be
      --  closed after completing the class for the containing scope.

      Class_Stack.Push (Record_Class);
      Begin_Class_File (Record_Class);

      --  For now we generate a trivial version of the class's <clinit> method

      Generate_Class_Init_Method (Record_Class);

      --  Generate the class's no-arg <init> method

      declare
         Init_Method      : Method_Id := Default_Constructor (Record_Class);
         Parent_Subp      : Entity_Id := Enclosing_Subprogram (R_Type);
         AR_Param         : Local_Var_Id;
         Prim_Op          : Entity_Id;
         Formal_1         : Entity_Id;
         Formal_Desig_Typ : Entity_Id;
         Gen_Def_Constr   : Boolean   := True;

      begin
         --  Suppress generation of the <init> method for a type mapped
         --  to a Java interface.

         if Is_Interface (Record_Class) then
            Gen_Def_Constr := False;
         end if;

         --  If the type has a user-defined constructor with a single
         --  formal (must be named 'this'), then we will not generate
         --  the default 'no-arg' constructor.

         if Gen_Def_Constr and then Is_Tagged_Type (R_Type) then
            Prim_Op := First_Entity (Scope (R_Type));

            while Present (Prim_Op) loop
               if Ekind (Prim_Op) = E_Function
                 and then Is_Constructor (Prim_Op)
               then
                  Formal_1 := First_Formal (Prim_Op);
                  if Present (Formal_1)
                    and then Name_String (Chars (Formal_1)) = "this"
                    and then not Present (Next_Formal (Formal_1))
                    and then Ekind (Etype (Formal_1)) in Access_Kind
                  then
                     Formal_Desig_Typ
                       := Full_Type (Designated_Type (Etype (Formal_1)));

                     --  We allow the designated type of the formal
                     --  to be either the specific tagged type or
                     --  its class-wide type.

                     if Formal_Desig_Typ = R_Type
                       or else Formal_Desig_Typ = Class_Wide_Type (R_Type)
                     then
                        Gen_Def_Constr := False;
                        exit;
                     end if;
                  end if;
               end if;

               Prim_Op := Next_Entity (Prim_Op);
            end loop;
         end if;

         if Gen_Def_Constr then
            --  Generate a method that calls the default constructor
            --  for the record class's superclass and generate code to
            --  allocate any composite components of the class.

            Open_Method (Init_Method);
            Set_Current_Method (Init_Method);
            Method_Stack.Push (Init_Method);
            Gen_Load_Local (This_Local (Init_Method));
            Gen_Invoke_Special
              (Default_Constructor (Superclass (Record_Class)));

            --  Allocate the composite components of the type within the
            --  <init> method unless the type has an init_proc, in which
            --  case the init_proc will perform the allocation. This
            --  special treatment for init_procs is necessary in cases
            --  such as discriminant-dependent arrays, which need to
            --  be allocated using a size determined from init_proc
            --  parameters.

            if not Present (Base_Init_Proc (R_Type)) then
               --  If the type is nested within a subprogram, then we need
               --  an activation record class for the containing subprogram
               --  since the allocation of composite component may require
               --  access to up-level data. But this means that the <init>
               --  method may require a static link parameter, so we can't use
               --  the default <init> method for the allocation, but have to
               --  create a new <init> method. This is handled by closing out
               --  the default <init> method and starting the new <init> here.

               if Present (Parent_Subp) then
                  --  Close the default <init> method

                  Gen_Method_Return;
                  Method_Stack.Pop;
                  Close_Method (Init_Method);
                  if not Method_Stack.Empty then
                     Set_Current_Method (Method_Stack.Top);
                  end if;

                  --  If not already present, make the activation record in
                  --  the parent subprogram

                  if AR_Stack.Empty
                    or else AR_Stack.Top.Method /= JVM_Method (Parent_Subp)
                  then
                     Make_Activation_Record (Parent_Subp);
                  end if;

                  --  Create and open a new <init> method which has a static
                  --  link parameter.

                  Init_Method
                    := New_Method (Record_Class,
                                   J_String.Name ("<init>"),
                                   Void_Type, False,
                                   Parent => Current_Method);
                  AR_Param
                    := New_Method_Parameter
                         (Init_Method, Name ("__AR_SL"),
                          Type_Of (AR_Stack.Top.AR_Class));

                  Open_Method (Init_Method);
                  Set_Current_Method (Init_Method);
                  Method_Stack.Push (Init_Method);
                  Gen_Load_Local (This_Local (Init_Method));
                  Gen_Invoke_Special
                    (Default_Constructor (Superclass (Record_Class)));
               end if;

               Allocate_Composite_Components
                 (R_Type, This_Local (Init_Method));
            end if;

            Gen_Method_Return;
            Method_Stack.Pop;
            Close_Method (Init_Method);

            if not Method_Stack.Empty then
               Set_Current_Method (Method_Stack.Top);
            end if;
         end if;
      end;

      --  Create the deep copy method for the record type if needed.
      --  (Currently we unconditionally generate the method, but it
      --  would be nice to suppress it for limited records. Unfortunately
      --  it seems necessary to check all the components of the type to
      --  determine this. The existing attributes Is_Limited_Record and
      --  Is_Limited aren't quite right to make this determination.
      --  The Is_Limited attribute can be true, but the type can still
      --  require deep copy for function returns, and Is_Limited_Record
      --  can cause problems for types that aren't marked Is_Limited_Record
      --  but contain components that are so marked.) ???

      if Convention (R_Type) /= Convention_Java
        and then Convention (Scope (R_Type)) /= Convention_Java
      then
         Generate_Deep_Copy (R_Type);
         Generate_Deep_Clone (R_Type);
      end if;
   end Generate_Record_Type;

   -------------------------
   -- Generate_Array_Type --
   -------------------------

   procedure Generate_Array_Type (A_Type : Entity_Id) is
      J_Type       : constant Type_Id   := JVM_Type (A_Type);
      Comp_Type    : constant Entity_Id := Full_Type (Component_Type (A_Type));

      --  The deep copy method and parameters

      Deep_Copy    : Method_Id;
      Target       : Local_Var_Id;
      Target_Start : Local_Var_Id;
      Source       : Local_Var_Id;
      Source_Count : Local_Var_Id;
      Source_Start : Local_Var_Id;

      Result       : Local_Var_Id;
      Loop_Index   : Local_Var_Id;
      Target_Index : Local_Var_Id;
      Source_Index : Local_Var_Id;
      Temp_Lbl     : Label_Id;
      Repeat_Label : Label_Id;
      Exit_Label   : Label_Id;

      procedure Prepare_For_Component_Deep_Copy
        (Target_Array  : Local_Var_Id;
         Source_Array  : Local_Var_Id;
         Array_Index   : Local_Var_Id;
         Is_Array_Copy : Boolean);
      --  Load the target and source array addresses and index them by
      --  Array_Index to obtain the references to the array components
      --  that will be copied by a subsequent deep copy call. The target
      --  array reference and loop index are also left on the stack in
      --  preparation for storing the result of the component deep copy.

      procedure Copy_Array_Component
        (Target_Array : Local_Var_Id;
         Source_Array : Local_Var_Id;
         Array_Index  : Local_Var_Id);
      --  Copy an individual composite component from one array (or subarray)
      --  to another, indexing the arrays by Loop_Index.

      procedure Assign_Subarrays (Dimensions : Pos_8);
      --  Generates code to traverse down the subarrays of the remaining
      --  Dimensions number of dimensions of two array objects and assign
      --  the contents of the deepest subarrays from the source to the
      --  target. Requires that references to two subarrays are on the
      --  stack with the Target subarray address on the top of stack.

      procedure Prepare_For_Component_Deep_Copy
        (Target_Array  : Local_Var_Id;
         Source_Array  : Local_Var_Id;
         Array_Index   : Local_Var_Id;
         Is_Array_Copy : Boolean)
      is
      begin
         --  The result of the following sequence is (from bottom to top of
         --  stack):
         --
         --    <trg_array> <index> <trg_array_comp_ref> <src_array_Comp_ref>

         --  Load the reference to the target array, duplicating it in
         --  preparation for storing the result of a call to _deep_copy
         --  for a component of the array (the target index is also loaded
         --  and swapped to put it into proper position for the later store).

         Gen_Load_Local (Target_Array);
         Gen_Duplicate;
         Gen_Load_Local (Array_Index);
         Gen_Swap;

         --  Index and load the target array field

         Gen_Load_Local (Array_Index);
         Gen_Load_Array_Element;

         --  If the copy is for an array component, then we must also pass
         --  the index to the target array (zero in this case since it can't
         --  be a slice copy).

         if Is_Array_Copy then
            Gen_Push_Int (Uint_0);
         end if;

         --  Index and load the source array field

         Gen_Load_Local (Source_Array);
         Gen_Load_Local (Array_Index);
         Gen_Load_Array_Element;

         --  If the copy is for an array component, then we must also pass
         --  the length (count of elements to copy) and the index into the
         --  source array (zero in this case since it can't be a slice copy).

         if Is_Array_Copy then
            Gen_Duplicate;
            Gen_Array_Length;
            Gen_Push_Int (Uint_0);
         end if;
      end Prepare_For_Component_Deep_Copy;

      procedure Copy_Array_Component
        (Target_Array : Local_Var_Id;
         Source_Array : Local_Var_Id;
         Array_Index  : Local_Var_Id)
      is
      begin
         case Ekind (Comp_Type) is

            --  For record components invoke the component type's
            --  deep copy method.

            when E_Record_Type =>

               --  Call the deep copy for the component's type and
               --  store the result in the target component. (The
               --  store back into Target.Component is needed to
               --  handle cases involving variant records, where
               --  target component may be null, but the source
               --  component is non-null.)

               Prepare_For_Component_Deep_Copy
                 (Target_Array,
                  Source_Array,
                  Array_Index,
                  Is_Array_Copy => False);
               Gen_Invoke_Deep_Copy (Comp_Type);
               Gen_Store_Array_Element;

            --  For scalar arrays, we copy the array by calling
            --  java.lang.System.arraycopy. For the case of composite
            --  components we invoke the deep copy method of the
            --  array's component type.

            when E_Array_Type | E_String_Type =>
               declare
                  A_Comp_Type : constant Entity_Id
                    := Full_Type (Component_Type (Comp_Type));

               begin
                  if Is_Limited_Record (A_Comp_Type)
                    or else Is_Concurrent_Type (A_Comp_Type)
                  then
                     null;

                  elsif Ekind (A_Comp_Type) not in Elementary_Kind
                    or else Has_Aliased_Components (Comp_Type)
                    or else Number_Dimensions (Comp_Type) > 1
                  then
                     Prepare_For_Component_Deep_Copy
                       (Target_Array,
                        Source_Array,
                        Array_Index,
                        Is_Array_Copy => True);
                     Gen_Invoke_Deep_Copy (Comp_Type);
                     Gen_Store_Array_Element;

                  else
                     --  When the array component has elementary components,
                     --  we generate a call to java.lang.System.arraycopy,
                     --  passing the following parameters:
                     --
                     --    Source.Array_Comp, 0,
                     --    Target.Array_Comp, 0, Target.Array_Comp'Length

                     Gen_Load_Local (Source_Array);
                     Gen_Load_Local (Array_Index);
                     Gen_Load_Array_Element;
                     Gen_Push_Int (Uint_0);

                     Gen_Load_Local (Target_Array);
                     Gen_Load_Local (Array_Index);
                     Gen_Load_Array_Element;
                     Gen_Duplicate;
                     Gen_Array_Length;
                     Gen_Push_Int (Uint_0);
                     Gen_Swap;

                     Gen_Invoke_API_Method (System_arraycopy);
                  end if;
               end;

            when others =>
               --  If the components are aliased, then we have to load
               --  and store from the 'all' fields of the source and
               --  target wrapper objects.

               if Has_Aliased_Components (A_Type) then
                  Gen_Load_Local (Target_Array);
                  Gen_Load_Local (Array_Index);
                  Gen_Load_Array_Element;

                  Gen_Load_Local (Source_Array);
                  Gen_Load_Local (Array_Index);
                  Gen_Load_Array_Element;

                  Gen_Get_Field  (Wrapper_Field (Wrapper_Type (Comp_Type)));
                  Gen_Put_Field  (Wrapper_Field (Wrapper_Type (Comp_Type)));

               --  No other cases should occur

               else
                  pragma Assert (False);
                  raise Program_Error;
               end if;
         end case;
      end Copy_Array_Component;

      procedure Assign_Subarrays (Dimensions : Pos_8) is
      begin
         --  When Dimensions = 1 we are at the deepest dimension of
         --  the arrays so we now assign the subarrays.

         if Dimensions = 1
           and then Ekind (Comp_Type) in Elementary_Kind
           and then not Has_Aliased_Components (Comp_Type)
         then
            --  The target subarray address is on the top of stack and
            --  the source subarray address is next to top of stack,
            --  we push a 0 for the source offset (srcOffset) and
            --  swap to bring the target array to top of stack.

            Gen_Push_Int (Uint_0);
            Gen_Swap;

            --  Generate the number of elements to copy

            Gen_Duplicate;
            Gen_Array_Length;

            --  Push the offset for the target array (dstOffset)
            --  and swap it with the array length

            Gen_Push_Int (Uint_0);
            Gen_Swap;

            --  Finally, invoke java.lang.System.arraycopy (takes
            --  parameters: src object, srcOffset, dest object,
            --  dstOffset, length).

            Gen_Invoke_API_Method (System_arraycopy);

         --  If Dimensions > 1, then we generate a loop to load references
         --  to each of the current dimension's subarrays and recurse to
         --  generate a traversal of the subarrays of the next dimension.

         else
            declare
               Trg_Temp  : constant Local_Var_Id
                 := New_Local_Var ("_trg_tmp", J_Type);
               Src_Temp  : constant Local_Var_Id
                 := New_Local_Var ("_src_tmp", J_Type);
               Index     : constant Local_Var_Id
                 := New_Local_Var ("_loop_index", Int_Type);
               Length    : constant Local_Var_Id
                 := New_Local_Var ("_index_max", Int_Type);
               Loop_Head : constant Label_Id := New_Label;
               Loop_Exit : constant Label_Id := New_Label;

            begin
               Gen_Store_Local (Trg_Temp);
               Gen_Store_Local (Src_Temp);

               --  Generate a loop to traverse the subarrays

               --  Initialize loop index to zero

               Gen_Push_Int (Uint_0);
               Gen_Store_Local (Index);

               Gen_Load_Local (Trg_Temp);
               Gen_Array_Length;
               Gen_Store_Local (Length);

               Gen_Label (Loop_Head);

               --  Check for end of loop (Index = Length)

               Gen_Load_Local (Index);
               Gen_Load_Local (Length);
               Gen_Compare_Branch_Equal (Loop_Exit);

               --  If we arrive here with a one-dimensional subarray,
               --  then the components are objects and need to be
               --  assigned via a deep copy operation.

               if Dimensions = 1 then
                  Copy_Array_Component (Trg_Temp, Src_Temp, Index);

               else
                  --  Load source subarray reference

                  Gen_Load_Local (Src_Temp);
                  Gen_Load_Local (Index);
                  Gen_Load_Subarray_Reference;

                  --  Load target subarray reference

                  Gen_Load_Local (Trg_Temp);
                  Gen_Load_Local (Index);
                  Gen_Load_Subarray_Reference;

                  Assign_Subarrays (Dimensions - 1);
               end if;

               --  Increment loop index and interate

               Gen_Incr_Local (Index, Uint_1);
               Gen_Goto (Loop_Head);

               Gen_Label (Loop_Exit);
            end;
         end if;
      end Assign_Subarrays;

   --  Start of processing for Generate_Array_Type

   begin
      --  For now we disallow the declaration of tagged components whose
      --  type has convention Java. This prevents problems with types
      --  which do not have no-arg constructors. Perhaps we should
      --  relax this restriction at some point, checking for the
      --  presence of a no-arg constructor for the type. Would it
      --  be better to simply change this to a warning for now and
      --  leave it up to the user to determine the safety rather
      --  than being overly restrictive ???

      if Is_Tagged_Type (Comp_Type)
        and then Convention (Comp_Type) = Convention_Java
      then
         Error_Msg_N
           ("array with components of Java-convention tagged type not allowed",
            A_Type);
         Error_Msg_N
           ("must use access values to reference Java objects",
            A_Type);
      end if;

      if Is_Limited_Record (Comp_Type)
        or else Is_Concurrent_Type (Comp_Type)
      then
         return;
      end if;

      --  It may be better to always generate an array deep copy routine,
      --  even for arrays of nonaliased elementary components. This would
      --  help to handle certain currently problematic cases involving
      --  object declarations initialized by aggregates that are transformed
      --  to individual component assignments by the front end (e.g., the
      --  deep copy would take care of cases where the target array reference
      --  is null the component has not been allocated). One special case
      --  though would be the deep copy needed for the types String and
      --  Wide_String since Standard does not have an associated class.
      --  ???

      if Ekind (Full_Type (Component_Type (A_Type))) not in Elementary_Kind
        or else Has_Aliased_Components (A_Type)
        or else Number_Dimensions (A_Type) > 1
      then
         --  Retrieve the deep copy method and its parameters

         Deep_Copy := Method (Deep_Copy_Class (A_Type),
                              Deep_Copy_Method_Name (A_Type));

         --  If the type's deep copy method was already generated,
         --  then simply return (can happen in cases such as calls
         --  to this procedure via an N_Itype_Reference).

         if Is_Completed (Deep_Copy) then
            return;
         end if;

         Target       := First_Local_Var (Deep_Copy);
         Target_Start := Next_Local_Var (Target);
         Source       := Next_Local_Var (Target_Start);
         Source_Count := Next_Local_Var (Source);
         Source_Start := Next_Local_Var (Source_Count);

         --  Start generation of the array deep copy method. The profile for
         --  this operation is:
         --
         --    public static R_Type <array_type_name>_deep_copy
         --      (A_Type <target>, Int <trg_index>,
         --       A_Type <source>, Int <src_cnt>, Int <src_index>)
         --
         --  (The *_index and src_cnt parameters are needed to handle the
         --  case of slice assignments.)

         Open_Method (Deep_Copy);
         Set_Current_Method (Deep_Copy);
         Method_Stack.Push (Deep_Copy);

         --  The source record value will be copied into the Result object

         Result := New_Local_Var ("_result", J_Type);

         --  Declare various temporaries and labels

         Loop_Index   := New_Local_Var ("_loop_index", Int_Type);
         Target_Index := New_Local_Var ("_trgindex", Int_Type);
         Source_Index := New_Local_Var ("_srcindex", Int_Type);
         Repeat_Label := New_Label;
         Exit_Label   := New_Label;
         Temp_Lbl     := New_Label;

         --  If Source = null, then simply return null as the result.
         --  This case can occur when the array is a component in an
         --  unused variant of an enclosing record, in which case the
         --  null reference must be copied.

         Gen_Load_Local (Source);
         Gen_Push_Null;
         Gen_Compare_Branch_Not_Equal (Temp_Lbl);
         Gen_Push_Null;
         Gen_Method_Return;
         Gen_Label (Temp_Lbl);

         --  Initialize Result from Target. Then, if Target = null, allocate
         --  a new array and initialize Result to refer to the new array.
         --  This case can occur when assigning an enclosing variant record
         --  to a target for which the array component is null (due to an
         --  inactive variant in the target). It can also potentially occur
         --  when initializing an object declaration.

         Temp_Lbl := New_Label;
         Gen_Load_Local (Target);
         Gen_Duplicate;
         Gen_Store_Local (Result);
         Gen_Push_Null;
         Gen_Compare_Branch_Not_Equal (Temp_Lbl);

         --  Target is null, so allocate a new array with the same size as
         --  the source array.

         Gen_Load_Local (Source);

         if Number_Dimensions (A_Type) = 1 then
            --  If this is an array of arrays where the component type of
            --  the subarrays is elementary, then load the length of both
            --  array levels and treat this like a multidimensional array
            --  allocation. This case requires special treatment because
            --  elementary one-dimensional array types don't have an
            --  associated deep copy method. ???

            if Ekind (Comp_Type) in Einfo.Array_Kind
              and then Number_Dimensions (Comp_Type) = 1
              and then
                Is_Elementary_Type (Full_Type (Component_Type (Comp_Type)))
            then
               Gen_Duplicate;
               Gen_Array_Length;
               Gen_Swap;
               Gen_Push_Int (Uint_0);
               Gen_Load_Subarray_Reference;
               Gen_Array_Length;
               Gen_New_Multiarray (J_Type);

            --  Normal one-dimensional array case

            else
               Gen_Array_Length;
               Gen_New_Array (J_Type);
            end if;

         --  Handle multidimensional allocation case. This requires
         --  traversal of each dimension of the source to load the
         --  length of each dimension. This isn't quite right yet
         --  for the case of null arrays, so we may need to add checks
         --  for zero subarray lengths, which is a real pain! ???

         else
            for D in 2 .. Number_Dimensions (A_Type) loop
               Gen_Duplicate;
               Gen_Array_Length;
               Gen_Swap;
               Gen_Push_Int (Uint_0);
               Gen_Load_Subarray_Reference;
            end loop;

            Gen_Array_Length;

            Gen_New_Multiarray (J_Type);
         end if;

         Gen_Store_Local (Result);

         Target := Result;  -- Let Target be same as Result (for clarity below)
         Gen_Label (Temp_Lbl);

         --  For multidimensional array copies, the arrays must be
         --  traversed to copy the elements at the deepest dimension.

         if Number_Dimensions (A_Type) > 1 then
            Gen_Load_Local (Source);
            Gen_Load_Local (Target);
            Assign_Subarrays (Pos_8 (Number_Dimensions (A_Type)));

         else
            --  Use the "source count" parameter as the initial value of
            --  an index count-down variable for the loop (number of elments
            --  to copy), and initialize the source and target indexes.

            Gen_Load_Local (Source_Count);
            Gen_Store_Local (Loop_Index);

            Gen_Load_Local (Source_Start);
            Gen_Store_Local (Source_Index);

            Gen_Load_Local (Target_Start);
            Gen_Store_Local (Target_Index);

            --  This is the loop head, which decrements the loop counter
            --  by one, and branches out when it becomes negative.

            Gen_Label (Repeat_Label);
            Gen_Incr_Local (Loop_Index, Uint_Minus_1);
            Gen_Load_Local (Loop_Index);
            Gen_Branch_Less (Exit_Label);

            Copy_Array_Component (Target, Source, Loop_Index);

            --  Increment the target and source index variables

            Gen_Incr_Local (Target_Index, Uint_1);
            Gen_Incr_Local (Source_Index, Uint_1);

            --  Branch back to the loop header

            Gen_Goto (Repeat_Label);

            Gen_Label (Exit_Label);

         end if;

         --  Return Result, which denotes the fully updated target object

         Gen_Load_Local (Result);
         Gen_Method_Return;

         --  Complete the deep copy method

         Method_Stack.Pop;
         Close_Method (Deep_Copy);

         if not Method_Stack.Empty then
            Set_Current_Method (Method_Stack.Top);
         end if;
      end if;
   end Generate_Array_Type;

   ---------------------------------
   -- Generate_Object_Access_Type --
   ---------------------------------

   procedure Generate_Object_Access_Type (Acc_Type : Entity_Id) is
   begin
      --  If the designated type is an unconstrained array type, then
      --  the type has an associated class and we now generate the class
      --  file and its <clinit> and <init> methods (unless the access
      --  does not have an associated class, in which case it must
      --  be an access type designating a Java array type).

      if Ekind (Designated_Type (Acc_Type)) in Einfo.Array_Kind
        and then not Is_Constrained (Designated_Type (Acc_Type))
        and then JVM.Type_Kind (JVM_Type (Acc_Type)) = Class_Kind
      then
         declare
            Access_Class : Class_Id := Class_Of_Type (JVM_Type (Acc_Type));

         begin
            --  Generate the class unless the type is sharing the
            --  predefined standard$access_string class.

            if Access_Class /= Null_Class
              and then Access_Class /= API_Class (Ada_Access_String)
            then
               Begin_Class_File (Access_Class);
               Generate_Class_Init_Method (Access_Class);
               Generate_Default_Constructor (Access_Class);
               End_Class_File (Access_Class);
            end if;
         end;
      end if;
   end Generate_Object_Access_Type;

   -------------------------------------
   -- Generate_Subprogram_Access_Type --
   -------------------------------------

   procedure Generate_Subprogram_Access_Type (Subp_Acc_Type : Entity_Id) is
      Subp_Acc_Class : Class_Id  := Class_Of_Type (JVM_Type (Subp_Acc_Type));

   begin
      Class_Stack.Push (Subp_Acc_Class);
      Begin_Class_File (Subp_Acc_Class);

      --  Generate trivial methods for <clinit> and <init>

      Generate_Class_Init_Method   (Subp_Acc_Class);
      Generate_Default_Constructor (Subp_Acc_Class);

      End_Class_File (Subp_Acc_Class);
      Class_Stack.Pop;
   end Generate_Subprogram_Access_Type;

   ------------------------------
   -- Generate_Concurrent_Type --
   ------------------------------

   procedure Generate_Concurrent_Type (Task_Or_Prot_Type : Entity_Id) is
      Parent_Node : constant Node_Id := Parent (Task_Or_Prot_Type);

   begin
      case Nkind (Parent_Node) is
         when N_Task_Type_Declaration | N_Single_Task_Declaration =>
            if Present (Task_Definition (Parent_Node)) then
               Translate_Declarations
                 (Visible_Declarations (Task_Definition (Parent_Node)));
               Translate_Declarations
                 (Private_Declarations (Task_Definition (Parent_Node)));
            end if;
         when N_Protected_Type_Declaration | N_Single_Protected_Declaration =>
            if Present (Protected_Definition (Parent_Node)) then
               Translate_Declarations
                 (Visible_Declarations (Protected_Definition (Parent_Node)));
               Translate_Declarations
                 (Private_Declarations (Protected_Definition (Parent_Node)));
            end if;
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Generate_Concurrent_Type;

   ------------------------------
   -- Generate_Integer_Subtype --
   ------------------------------

   procedure Generate_Integer_Subtype (Subt : Entity_Id) is
   begin
      --  For now we do nothing...
      null;
   end Generate_Integer_Subtype;

   ----------------------------
   -- Generate_Array_Subtype --
   ----------------------------

   procedure Generate_Array_Subtype (Subt : Entity_Id) is
      Index : Node_Id := First_Index (Subt);

   begin
      while Present (Index) loop
         if Is_Itype (Etype (Index)) then
            Translate_Subtype (Etype (Index));
         end if;

         Index := Next (Index);
      end loop;
   end Generate_Array_Subtype;

   -----------------------------
   -- Generate_Record_Subtype --
   -----------------------------

   procedure Generate_Record_Subtype (Subt : Entity_Id) is
   begin
      --  For now we do nothing...
      null;
   end Generate_Record_Subtype;

end Jx_Ch3;
