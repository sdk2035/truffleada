------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 4                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.115 $
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
with Debug;    use Debug;
with Debug_A;  use Debug_A;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Tss;  use Exp_Tss;
with JVM;      use JVM;
with JVM.API;  use JVM.API;
with J_String; use J_String;
with J_Types;  use J_Types;
with Jx_Ch3;   use Jx_Ch3;
with Jx_Ch6;   use Jx_Ch6;
with Jx_Ch11;  use Jx_Ch11;
with Jx_Decl;  use Jx_Decl;
with Jx_Uplev; use Jx_Uplev;
with Nlists;   use Nlists;
with Osint;
with Sem_Disp; use Sem_Disp;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Types;    use Types;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Jx_Ch4 is

   procedure Evaluate_Attribute (Attr : Node_Id);
   --  Generates code to evaluate an Ada attribute

   procedure Evaluate_Array_Aggregate (Aggr : Node_Id);
   --  Generates code to evaluate an array aggregate

   procedure Evaluate_Record_Aggregate
     (Aggr : Node_Id; Aggr_LV : Local_Var_Id := Null_Local_Var);
   --  Generates code to evaluate a record aggregate. If the Aggr_LV parameter
   --  is null, then the aggregate associations will be evaluated into the
   --  the object designated by Aggr_LV and it is the responsibility of the
   --  caller to load the result of the aggregate evaluation on the stack.
   --  This is currently used to support the evaluation of parent aggregates
   --  in the case of a tagged type extension (see the body of this procedure).


   procedure Evaluate_Operator
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean);
   --  Generates code to evaluate an Ada operator. Label is used to
   --  provide a target for the evaluation of a conditional expression
   --  (Label = Null_Label if no target available). If Label /= Null_Label,
   --  then True_Branch indicates whether the branch should occur when
   --  the condition is True or when it's False.

   procedure Evaluate_Array_Comparison
     (Op        : Node_Kind;
      Left_Arr  : Node_Id;
      Right_Arr : Node_Id;
      Jtype     : Type_Id);
   --  Generates code to evaluate an array comparison for array type Jtype.
   --  Requires that loads of references to the two arrays have been generated
   --  immediately prior to calling.

   procedure Evaluate_And_Then
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean);
   --  Generates code to evaluates an "and then" short-circuit control form.
   --  Label is used to provide a target for the evaluation. If Label /=
   --  Null_Label, then True_Branch indicates whether the branch should
   --  occur when the condition is True or when it's False. If Label is
   --  Null_Label, then the operation will simply produce a Boolean result.

   procedure Evaluate_Or_Else
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean);
   --  Generates code to for an "or else" short-circuit control form.
   --  Label is used to provide a target for the evaluation. If Label
   --  /= Null_Label, then True_Branch indicates whether the branch
   --  should occur when the condition is True or when it's False.
   --  If Label is Null_Label, then the operation will simply produce
   --  a Boolean result.

   procedure Evaluate_Membership_Test
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean);
   --  Generates code for a membership test. Label is used to provide
   --  a target for the evaluation. If Label /= Null_Label, then True_Branch
   --  indicates whether the branch should occur when the condition is True
   --  or when it's False. If Label is Null_Label, then the operation will
   --  simply produce a Boolean result.

   procedure Evaluate_Aggregate (Aggr : Node_Id);
   --  Generates code to evaluate an Ada aggregate, producing an initialized
   --  object and pushing a reference to the object onto the stack.

   procedure Evaluate_Allocator (Allocator : Node_Id);
   --  Generates code to evaluate an Ada allocator.

   procedure Evaluate_Integer_Literal (Literal : Node_Id; Typ : Entity_Id);
   --  Generates code to push the value of an integer literal of type Typ

   procedure Evaluate_Real_Literal (Literal : Node_Id; Typ : Entity_Id);
   --  Generates code to push the value of a real literal of type Typ

   procedure Evaluate_Subprogram_Access
     (Subp_Name     : Node_Id;
      Subp_Acc_Type : Entity_Id);
   --  Generates code to evaluate an access to a subprogram by creating a
   --  subclass of the class of the attribute's access-to-subprogram type
   --  with a method that will invoke the subprogram. An object of the
   --  subclass is created and its reference is pushed on the stack.

   procedure Evaluate_Unconstrained_Array_Ref
     (Arr_Expr : Node_Id;
      Acc_Type : Entity_Id);
   --  Generates code to construct a compound reference object that denotes
   --  an array value needed in the context of an unconstrained access type
   --  Acc_Type. Used when evaluating N_References and access attributes.

   procedure Check_For_Overflow (N : Node_Id);
   --  Generates code to check that the integer value on the top of stack
   --  is in the base range of the type of N, but only if Do_Overflow_Check
   --  is set on N. Currently requires that N denote an N_Type_Conversion.

   -------------------
   -- JVM_Expr_Type --
   -------------------

   function JVM_Expr_Type (Expr : Node_Id) return Type_Id is
   begin
      return JVM_Type (Etype (Expr));
   end JVM_Expr_Type;

   ------------------------
   -- Check_For_Overflow --
   ------------------------

   procedure Check_For_Overflow (N : Node_Id) is
      Check_Type  : Entity_Id;
      TOS_Type    : Entity_Id;
      Raise_Label : Label_Id;
      OK_Label    : Label_Id;
      Check_State : Boolean;

   begin
      pragma Assert (Nkind (N) = N_Type_Conversion);

      if Do_Overflow_Check (N) then
         Check_Type := Full_Type (N);
         TOS_Type := Full_Type (Expression (N));

         --  For now we don't do overflow checks for floating point
         --  conversions, and in any case we don't perform them
         --  if the JVM types are the same (will this miss some
         --  fixed point cases that need to be checked ???).

         if not Is_Floating_Point_Type (Check_Type)
           and then not Is_Floating_Point_Type (TOS_Type)
           and then JVM_Type (Check_Type) /= JVM_Type (TOS_Type)
         then
            Raise_Label := New_Label;
            OK_Label    := New_Label;

            Suppress_Stack_Checking (Check_State);

            Gen_Duplicate;

            --  Compare the top-of-stack value against the bounds of the
            --  target type's base range and raise Constraint_Error if
            --  out of bounds.

            Evaluate_Integer_Literal (Type_Low_Bound (Check_Type), TOS_Type);
            Gen_Compare_Branch_Less (Raise_Label);

            Gen_Duplicate;
            Evaluate_Integer_Literal (Type_High_Bound (Check_Type), TOS_Type);
            Gen_Compare_Branch_Less_Equal (OK_Label);

            --  Generate a raise of Constraint_Error

            Gen_Label (Raise_Label);
            Generate_Exception_And_Throw
              (API_Class (Ada_Constraint_Error), Sloc (N));

            Gen_Label (OK_Label);

            Restore_Stack_Checking (Check_State);
         end if;
      end if;
   end Check_For_Overflow;

   -------------------
   -- Evaluate_Addr --
   -------------------

   function Evaluate_Addr (Obj_Name : Node_Id) return Address_Descriptor is
      Addr      : Address_Descriptor;

   begin
      Debug_A_Entry ("(Ada-to-JVM) ", Obj_Name);

      case Nkind (Obj_Name) is
         when N_Identifier | N_Expanded_Name =>
            declare
               Ada_Obj        : constant Entity_Id := Entity (Obj_Name);
               Ada_Type       : Entity_Id          := Full_Type (Ada_Obj);
               Formal_LV      : Local_Var_Id;
               Obj_Field      : Field_Id;
               Up_Level_Field : Field_Id;

            begin
               case Ekind (Ada_Obj) is
                  when E_Variable | E_Constant =>
                     if Present (Renamed_Object (Ada_Obj))
                       and then Ekind (Ada_Type) in Elementary_Kind
                     then
                        --  If this is a renaming an elementary entity,
                        --  then simply return the address evaluation of
                        --  the renamed entity.

                        if Is_Entity_Name (Renamed_Object (Ada_Obj)) then
                           return Evaluate_Addr (Renamed_Object (Ada_Obj));

                        --  If the renamed object is a selected component,
                        --  change the type to indicate that we want to load
                        --  the reference to the record object containing the
                        --  the component. The address of the elementary
                        --  component field itself will be established
                        --  further below, after the containing object has
                        --  been loaded.

                        elsif Nkind (Renamed_Object (Ada_Obj))
                          = N_Selected_Component
                        then
                           Ada_Type
                             := Full_Type (Prefix (Renamed_Object (Ada_Obj)));
                           if Is_Access_Type (Ada_Type) then
                              Ada_Type
                                := Full_Type
                                     (Directly_Designated_Type (Ada_Type));
                           end if;

                        --  Blow up in other cases (e.g., indexed components),
                        --  which aren't supported yet. ???

                        else
                           pragma Assert (False);
                           raise Program_Error;
                        end if;
                     end if;

                     if Is_Global_Entity (Ada_Obj)
                       or else Is_Imported (Ada_Obj)
                     then
                        Obj_Field := JVM_Field (Ada_Obj);

                        --  If the Ada object denotes a JVM object, then we
                        --  must load the object reference since the object
                        --  must be addressed indirectly.

                        if Ekind (Ada_Type) not in Wrappable_Kind then
                           Gen_Get_Static_Field (Obj_Field);

                        --  A wrapped scalar or access object requires
                        --  loading the reference to the wrapper.

                        elsif Has_Wrapper (Ada_Obj) then
                           Gen_Get_Static_Field (Obj_Field);
                           Obj_Field := Wrapper_Field (Ada_Obj);
                        end if;

                        Addr := (Field_Address, Obj_Field);

                     elsif Enclosing_Method (Ada_Obj) = Current_Method then

                        --  If the local variable has been allocated to
                        --  a field in the current method's AR object,
                        --  then form an address denoting that field if
                        --  its type is scalar or access (otherwise we
                        --  can get the value from the local variable
                        --  since it's effectively a constant).

                        if Access_From_Current_AR (Ada_Obj) then
                           Gen_Load_Local (AR_Stack.Top.AR_Obj);

                           Up_Level_Field := AR_Field (AR_Stack.Top, Ada_Obj);

                           --  If the Ada object denotes a JVM object, then we
                           --  must load the object reference since the object
                           --  must be addressed indirectly.

                           if Ekind (Ada_Type) not in Wrappable_Kind then
                              Gen_Get_Object_Field (Up_Level_Field);

                           --  A wrapped scalar or access object requires
                           --  loading the reference to the wrapper.

                           elsif Has_Wrapper (Ada_Obj) then
                              Gen_Get_Object_Field (Up_Level_Field);
                              Up_Level_Field := Wrapper_Field (Ada_Obj);
                           end if;

                           Addr := (Field_Address, Up_Level_Field);

                        else
                           --  If the Ada object denotes a JVM object, then we
                           --  must load the object reference since the object
                           --  must be addressed indirectly.

                           if Ekind (Ada_Type) not in Wrappable_Kind then
                              Gen_Load_Local (JVM_Local_Var (Ada_Obj));
                              Addr := (Local_Address, JVM_Local_Var (Ada_Obj));

                           --  A wrapped scalar or access object requires
                           --  loading the reference to the wrapper.

                           elsif Has_Wrapper (Ada_Obj) then
                              Gen_Load_Local (JVM_Local_Var (Ada_Obj));
                              Addr := (Field_Address, Wrapper_Field (Ada_Obj));

                           --  The object must be an unwrapped scalar/access,
                           --  so its address is the local variable itself.

                           else
                              Addr := (Local_Address, JVM_Local_Var (Ada_Obj));
                           end if;
                        end if;

                     else
                        --  We have to address the local variable
                        --  in an up-level activation frame object.

                        Up_Level_Field := Access_AR_Field (Ada_Obj);

                        --  If the Ada object denotes a JVM object, then we
                        --  must load the object reference since the object
                        --  must be addressed indirectly.

                        if Ekind (Ada_Type) not in Wrappable_Kind then
                           Gen_Get_Object_Field (Up_Level_Field);

                        --  A wrapped scalar or access object requires
                        --  loading the reference to the wrapper.

                        elsif Has_Wrapper (Ada_Obj) then
                           Gen_Get_Object_Field (Up_Level_Field);
                           Up_Level_Field := Wrapper_Field (Ada_Obj);
                        end if;

                        Addr := (Field_Address, Up_Level_Field);
                     end if;

                     --  If the object is a renaming of an elementary object,
                     --  then at this point it must be the renaming of an
                     --  object denoted by a selected component. In this
                     --  case the reference to the component's containing
                     --  object has been loaded, and we now change the
                     --  address to denote the component itself. Support
                     --  for other cases, such as renamings of indexed
                     --  components, will be added later. ???

                     if Present (Renamed_Object (Ada_Obj))
                       and then Ekind (Full_Type (Ada_Obj)) in Elementary_Kind
                     then
                        pragma Assert (Nkind (Renamed_Object (Ada_Obj))
                                        = N_Selected_Component);

                        declare
                           Selector : constant Entity_Id
                             := Entity
                                  (Selector_Name (Renamed_Object (Ada_Obj)));
                           J_Field  : Field_Id := JVM_Field (Selector);

                        begin
                           if Has_Wrapper (Selector) then
                              Gen_Get_Object_Field (J_Field);
                              J_Field := Wrapper_Field (Selector);
                           end if;

                           Addr := (Field_Address, J_Field);
                        end;
                     end if;

                  when Formal_Kind =>

                     if JVM_Method (Enclosing_Subprogram (Ada_Obj))
                       = Current_Method
                     then
                        Formal_LV := JVM_Local_Var (Ada_Obj);

                        Gen_Load_Local (Formal_LV);

                        if Has_Wrapper (Ada_Obj) then
                           Addr := (Field_Address, Wrapper_Field (Ada_Obj));
                        else
                           Addr := (Local_Address, JVM_Local_Var (Ada_Obj));
                        end if;

                     else
                        --  We have to address the local variable in
                        --  an up-level activation frame object.

                        Up_Level_Field := Access_AR_Field (Ada_Obj);
                        Gen_Get_Object_Field (Up_Level_Field);

                        if Ekind (Ada_Obj) /= E_In_Parameter
                          and then Ekind (Ada_Type) in Elementary_Kind
                        then
                           Addr := (Field_Address, Wrapper_Field (Ada_Obj));
                        else
                           Addr := (Field_Address, Up_Level_Field);
                        end if;
                     end if;

                     --  If the formal is a controlling parameter of a
                     --  dispatching operation, then its JVM type may have
                     --  been changed to some parent type (in the case
                     --  of an overriding operation). In that case we
                     --  have to cast the parameter value to the JVM
                     --  type associated with the formal's Ada type.

                     if Is_Controlling_Formal (Ada_Obj)
                       and then
                         JVM_Type (Find_Dispatching_Type (Scope (Ada_Obj)))
                           /= Type_Of (JVM_Local_Var (Ada_Obj))
                     then
                        Gen_Check_Cast
                          (Class_Of_Type (JVM_Type (Etype (Ada_Obj))));
                     end if;

                  when E_Exception =>
                     Gen_Default_Object (JVM_Class (Ada_Obj));

                  when E_Procedure | E_Function =>
                     pragma Assert
                       (Nkind (Parent (Obj_Name)) = N_Attribute_Reference);

                     --  Normally an Address attribute applied to a subprogram
                     --  is not defined for the JVM, but the GNAT front end
                     --  sometimes expands such attributes as the argument
                     --  to an unchecked conversion, so we support that case.

                     declare
                        Attr_Name : constant Name_Id
                          := Attribute_Name (Parent (Obj_Name));
                        Attr_Id   : constant Attribute_Id
                          := Get_Attribute_Id (Attr_Name);

                     begin
                        if Attr_Id = Attribute_Address
                          or else Attr_Id = Attribute_Code_Address
                        then
                           if Nkind (Parent (Parent (Obj_Name)))
                             = N_Unchecked_Type_Conversion
                           then
                              Evaluate_Subprogram_Access
                                (Obj_Name,
                                 Full_Type (Parent (Parent (Obj_Name))));

                           --  For an Address attribute applied to a subprogram
                           --  without any access-to-subprogram type context,
                           --  simply push a null, since this operation isn't
                           --  sensibly supported for the JVM.

                           else
                              Gen_Push_Null;
                           end if;

                        --  Otherwise assume that the attribute is one of the
                        --  flavors of access attributes, in which case we get
                        --  the subprogram access type from the attribute's
                        --  Etype.

                        else
                           Evaluate_Subprogram_Access
                             (Obj_Name, Full_Type (Parent (Obj_Name)));
                        end if;
                     end;

                  when others =>
                     pragma Assert (False);
                     raise Program_Error;
               end case;
            end;

         when N_Selected_Component =>
            declare
               Selector   : Entity_Id := Entity (Selector_Name (Obj_Name));
               J_Field    : Field_Id;
               Desig_Type : Entity_Id;

            begin
               --  References to the _tag field should never occur
               --  in an address context. For the case of _tag in
               --  a value context see Evaluate_Expr.

               pragma Assert (Chars (Selector) /= Name_uTag);

               --  If the selector is a _parent field, then we elide the
               --  field selection and simply evaluate the prefix. This
               --  usage should only occur in contexts where passing the
               --  containing object is appropriate in any case (the
               --  selection of the _parent field is equivalent to a
               --  conversion to the parent type).

               if Chars (Selector) = Name_uParent then
                  Addr := Evaluate_Addr (Obj_Name);

               else
                  Evaluate_Expr (Prefix (Obj_Name));

                  J_Field := JVM_Field (Selector);

                  --  If the selected component denotes a JVM object, then we
                  --  always load the object reference since the object must be
                  --  addressed indirectly in any case.

                  if Ekind (Full_Type (Obj_Name)) not in Wrappable_Kind then
                     Gen_Get_Object_Field (J_Field);

                     --  If the selector is an access discriminant linked to
                     --  a parent discriminant, then we force the type (via
                     --  a checkcast) to be the selector's designated type
                     --  in the case where it has a different type than the
                     --  discriminant that it constrains. This ensures
                     --  compatibility with the type required by the context.

                     if Ekind (Selector) = E_Discriminant
                       and then Is_Access_Type (Etype (Selector))
                       and then Present (Corresponding_Discriminant (Selector))
                       and then
                         Etype (Selector)
                           /= Etype (Corresponding_Discriminant (Selector))
                     then
                        Desig_Type
                          := Directly_Designated_Type (Etype (Selector));
                        Gen_Check_Cast (Class_Of_Type (JVM_Type (Desig_Type)));
                     end if;

                  --  A wrapped scalar or access component requires loading
                  --  the reference to the wrapper.

                  elsif Has_Wrapper (Selector) then
                     Gen_Get_Object_Field (J_Field);
                     J_Field := Wrapper_Field (Selector);
                  end if;

                  Addr := (Field_Address, J_Field);
               end if;
            end;

         when N_Indexed_Component =>
            Evaluate_Expr (Prefix (Obj_Name));

            Gen_Array_Subscript
              (Prefix (Obj_Name), First (Expressions (Obj_Name)));

            --  If the indexed component denotes a JVM object, then we
            --  always load the object reference since the object must be
            --  addressed indirectly in any case.

            if Ekind (Full_Type (Obj_Name)) not in Wrappable_Kind then
               Gen_Load_Array_Element;
               Addr := (Indexed_Address, JVM_Type (Etype (Obj_Name)));

            --  A wrapped scalar or access component requires loading
            --  the reference to the wrapper and forming a field address.

            elsif Has_Aliased_Components (Full_Type (Prefix (Obj_Name))) then
               Gen_Load_Array_Element;
               Addr := (Field_Address, Wrapper_Field (Full_Type (Obj_Name)));

            else
               Addr := (Indexed_Address, JVM_Type (Etype (Obj_Name)));
            end if;

         when N_Slice =>
            Evaluate_Expr (Prefix (Obj_Name));

            Addr := (Array_Address, Is_Slice => True,
                     Descriptor_Class => Null_Class);

         when N_Explicit_Dereference =>
            declare
               Designated_Atype : constant Entity_Id
                 := Underlying_Type
                      (Designated_Type (Full_Type (Prefix (Obj_Name))));
               Designated_Jtype : constant Type_Id
                 := JVM_Type (Designated_Atype);

            begin
               Evaluate_Expr (Prefix (Obj_Name));

               if JVM.Type_Kind (Designated_Jtype) = JVM.Array_Kind then
                  if Is_Constrained (Designated_Atype)
                    or else
                      Convention (Scope (Full_Type (Prefix (Obj_Name))))
                        = Convention_Java
                  then
                     Addr := (Array_Address, Is_Slice => False,
                              Descriptor_Class => Null_Class);

                  else
                     Addr :=
                       (Array_Address,
                        Is_Slice => False,
                        Descriptor_Class =>
                         Class_Of_Type (JVM_Type (Etype (Prefix (Obj_Name)))));
                  end if;

               elsif Ekind (Designated_Atype) in Wrappable_Kind then
                  Addr := (Field_Address, Wrapper_Field (Designated_Atype));

               elsif JVM.Type_Kind (Designated_Jtype) = JVM.Class_Kind then
                  Addr := (Addr_Kind => Object_Address);

               else
                  pragma Assert (False);
                  raise Program_Error;
               end if;
            end;

         when N_Type_Conversion =>
            Addr := Evaluate_Addr (Expression (Obj_Name));
            Check_For_Overflow (Obj_Name);
            Gen_Conversion (JVM_Expr_Type (Obj_Name));

         when N_Unchecked_Type_Conversion =>
            Addr := Evaluate_Addr (Expression (Obj_Name));

            --  If the target is a class or array type, then we may have
            --  to apply a cast on downward conversions, even though the
            --  conversion is unchecked, in order to satisfy the Java
            --  verifier.

            if JVM.Type_Kind (JVM_Expr_Type (Obj_Name)) = Class_Kind
              or else JVM.Type_Kind (JVM_Expr_Type (Obj_Name)) = JVM.Array_Kind
            then
               Gen_Conversion (JVM_Expr_Type (Obj_Name));

            else
               --  We have to explicitly change the top-of-stack type to
               --  match the target of the unchecked conversion.

               Pop_Type;
               Push_Type (JVM_Expr_Type (Obj_Name));
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      Debug_A_Exit ("(Ada-to-JVM) ", Obj_Name, " (done)");
      return Addr;
   end Evaluate_Addr;

   --------------------------------
   -- Evaluate_Subprogram_Access --
   --------------------------------

   procedure Evaluate_Subprogram_Access
     (Subp_Name     : Node_Id;
      Subp_Acc_Type : Entity_Id)
   is
      Subp_Entity    : constant Entity_Id := Entity (Subp_Name);
      Subp_Acc_Class : constant Class_Id  := JVM_Class (Subp_Acc_Type);
      Subp_Profile   : constant Entity_Id
        := Directly_Designated_Type (Subp_Acc_Type);
      Formal         : Entity_Id := First_Formal (Subp_Profile);
      Subp_Formal    : Entity_Id;
      Ctrl_Formal    : Entity_Id;
      Acc_Formal     : Local_Var_Id;
      Call_Formal    : Local_Var_Id;
      AR_Field       : Field_Id;
      Class_Name     : constant String
        := Name_String (Name (Subp_Acc_Class))
             & "$" & JVM_Expanded_Name (Subp_Entity);
      Acc_Attr_Class : constant Class_Id
        := New_Class (Name      => Name (Class_Name),
                      Pkg_Name  => Package_Name (Subp_Name),
                      Src_Name  => Source_Name (Sloc (Subp_Name)),
                      Super     => Subp_Acc_Class);
      Acc_Method     : constant Method_Id
        := New_Method (Acc_Attr_Class,
                       J_String.Name ("Invoke"),
                       JVM_Type (Etype (Subp_Profile)),
                       Static  => False);

   begin
      --  Create the formal parameters for the abstract invocation method
      --  associated with the access-to-subprogram type's class.

      while Present (Formal) loop
         Acc_Formal := New_Method_Parameter
                         (Acc_Method, Chars (Formal), JVM_Type (Formal));
         Formal := Next_Formal_With_Extras (Formal);
      end loop;

      --  Generate the access attribute's class (a subclass of the class
      --  associated with attribute's access-to-subprogram type).

      Class_Stack.Push (Acc_Attr_Class);
      Begin_Class_File (Acc_Attr_Class);

      --  Generate trivial methods for <clinit> and <init>

      Generate_Class_Init_Method   (Acc_Attr_Class);
      Generate_Default_Constructor (Acc_Attr_Class);

      --  For now generate a trivial code body for the method
      --  (which should really be abstract). ???

      Open_Method (Acc_Method);
      Set_Current_Method (Acc_Method);
      Method_Stack.Push (Acc_Method);

      --  Retrieve the first parameter to pass to the accessed method's
      --  corresponding parameter as well as the first Ada formal of
      --  the subprogram.

      Acc_Formal  := Next_Local_Var (This_Local (Acc_Method));
      Subp_Formal := First_Formal (Subp_Entity);
      Call_Formal := First_Local_Var (JVM_Method (Subp_Entity));

      --  For dispatching operations, retrieve the first controlling
      --  formal, if any, with the method's 'this' argument.

      if Is_Dispatching_Operation (Subp_Entity) then
         while Present (Subp_Formal)
           and then not Is_Controlling_Formal (Subp_Formal)
         loop
            Acc_Formal  := Next_Local_Var (Acc_Formal);
            Next_Formal (Subp_Formal);
         end loop;

         --  If the method has any controlling formals then the first of
         --  these is the method's 'this' argument. Load the corresponding
         --  formal parameter of the enclosing Invoke method to pass to
         --  as the 'this' argument of the subprogram to call indirectly.

         if Present (Subp_Formal) then
            Ctrl_Formal := Subp_Formal;
            Gen_Load_Local (Acc_Formal);

         --  If there is no controlling formal available, then this must
         --  be a function with a controlling result. Create an object of
         --  the specific tagged type for which the subprogram is a
         --  primitive and pass the new object as the call's 'this'
         --  argument to produce nondispatching semantics.

         else
            pragma Assert (Base_Type (Etype (Subp_Entity))
                            = Find_Dispatching_Type (Subp_Entity));

            Gen_Default_Object
              (JVM_Class (Find_Dispatching_Type (Subp_Entity)));
            Call_Formal := Next_Local_Var (Call_Formal);
         end if;

         --  Reset to the first formal parameters of both the wrapper
         --  and the subprogram to call.

         Acc_Formal  := Next_Local_Var (This_Local (Acc_Method));
         Subp_Formal := First_Formal (Subp_Entity);
      end if;

      --  Load all (non-this) formals of the containing method as actual
      --  parameters to pass to the invoked method.

      while Acc_Formal /= Null_Local_Var loop
         --  Skip the load of the wrapper's formal if it corresponds to
         --  the controlling formal of the subprogram to call, since it
         --  was already loaded as the 'this' parameter above.

         if not Present (Ctrl_Formal) or else Subp_Formal /= Ctrl_Formal then
            Gen_Load_Local (Acc_Formal);

            --  In certain cases involving front-end generated unchecked
            --  conversions of Subp'Address (e.g., for tasking), the formals
            --  of the subprogram access type may not match the types of
            --  accessed subprogram. In that case a downcast will be required
            --  in order to satisfy the verifier (note that the call to
            --  Gen_Conversion will only emit a checkcast if it's actually
            --  needed).

            if JVM.Type_Kind (Type_Of (Acc_Formal)) = Class_Kind then
               Gen_Conversion (Type_Of (JVM_Local_Var (Subp_Formal)));
            end if;
         end if;

         Next_Formal_With_Extras (Subp_Formal);
         Acc_Formal  := Next_Local_Var (Acc_Formal);
         Call_Formal := Next_Local_Var (Call_Formal);
      end loop;

      --  If this is a call to a nested method, then create an object field
      --  to hold the static link to the subprogram's parent and pass the
      --  static link field saved in the access-to-subprogram object.

      if Present (Enclosing_Subprogram (Subp_Entity)) then
         pragma Assert (Call_Formal /= Null_Local_Var);

         --  Use the type of the static link formal (denoted
         --  by the last formal of the subprogram) in creating
         --  the static link field.

         AR_Field
           := New_Field
                (Acc_Attr_Class, Name ("_static_link"),
                 Type_Of (Call_Formal), Static => False);

         Gen_Load_Local (This_Local (Acc_Method));
         Gen_Get_Field (AR_Field);
      end if;

      --  Generate a call to the method to which the access attribute applies

      Gen_Invoke_Method (JVM_Method (Subp_Entity));

      Gen_Method_Return;

      Method_Stack.Pop;
      Close_Method (Acc_Method);
      if not Method_Stack.Empty then
         Set_Current_Method (Method_Stack.Top);
      end if;

      End_Class_File (Acc_Attr_Class);
      Class_Stack.Pop;

      --  Finally, generate an object of the new subclass and leave its
      --  reference on the stack as the result of the access attribute.

      Gen_Default_Object (Acc_Attr_Class);

      --  If this is an access to a nested method, then save the reference to
      --  the activation record associated with the method's static parent
      --  in the static link field of the access-to-subprogram object.

      if Present (Enclosing_Subprogram (Subp_Entity)) then
         Gen_Duplicate;
         Load_Static_Link (Enclosing_Method (Subp_Entity));
         Gen_Conversion (Type_Of (AR_Field));
         Gen_Put_Field (AR_Field);
      end if;
   end Evaluate_Subprogram_Access;

   --------------------------------------
   -- Evaluate_Unconstrained_Array_Ref --
   --------------------------------------

   procedure Evaluate_Unconstrained_Array_Ref
     (Arr_Expr : Node_Id;
      Acc_Type : Entity_Id)
   is
      Arr_Ref_Class : constant Class_Id
        := Class_Of_Type (JVM_Expr_Type (Acc_Type));
      Alloc_LV      : constant Local_Var_Id
        := New_Local_Var ("_alloc_tmp", Type_Of (Arr_Ref_Class));

      Arr_Typ       : Entity_Id := Designated_Type (Acc_Type);
      Dimensions    : Pos_8     := Pos_8 (Number_Dimensions (Arr_Typ));

   begin
      --  Allocate the unconstrained array pointer object

      Gen_Default_Object (Arr_Ref_Class);
      Gen_Store_Local (Alloc_LV);

      --  Evaluate the prefix and load its bounds

      if Nkind (Arr_Expr) /= N_Function_Call then
         Evaluate_Expr (Arr_Expr);
         Load_Array_Bounds (Arr_Expr);

      --  The result of the function is a bare array reference and we construct
      --  the bounds wrapper here. We call Translate_Subprogram_Call here to
      --  avoid recursion. Eventually this code will only be used for calls
      --  to imported Java functions returning arrays, but Ada functions
      --  with unconstrained result subtypes do not currently return bounds
      --  information, so we artificially create them here. This is not
      --  correct in general of course, but works for most practical
      --  purposes. ???

      else
         Translate_Subprogram_Call (Arr_Expr);

         --  Would be nice to call Load_Array_Bounds here as well, but
         --  it's easier to just handle the bounds here because it avoids
         --  adding another special case check in Load_Array_Bounds.
         --  (But given the additional special-case code now present here
         --  for nonzero lower bounds, maybe that should be revisited. ???)

         declare
            Index     : Node_Id   := First_Index (Arr_Typ);
            Lower_Bnd : Node_Id   := Array_Index_First (Index);
            Arr_Temp  : Local_Var_Id;

         begin
            --  For the multidimensional case we save the array reference
            --  in a temporary and use it to traverse successive subarrays
            --  to get the length of each dimension.

            if Dimensions > 1 then
               Arr_Temp
                 := New_Local_Var ("_multiarr_tmp", JVM_Type (Arr_Typ));
               Gen_Duplicate;
               Gen_Store_Local (Arr_Temp);
            end if;

            for Current_Dimension in 1 .. Dimensions loop
               if Current_Dimension = 1 then
                  Gen_Duplicate;
                  Gen_Array_Length;

               --  In the case of higher dimensions, we traverse the
               --  the first element of the current subarray temporary
               --  to get the dimension length. This is not correct for
               --  null subarrays however ???

               else
                  Next_Index (Index);
                  Lower_Bnd := Array_Index_First (Index);

                  Gen_Load_Local (Arr_Temp);
                  Gen_Push_Int (Uint_0);
                  Gen_Load_Subarray_Reference;
                  Gen_Duplicate;
                  Gen_Store_Local (Arr_Temp);
                  Gen_Array_Length;
               end if;

               if Nkind (Lower_Bnd) = N_Integer_Literal
                 and then Expr_Value (Lower_Bnd) <= Uint_1
               then
                  --  When the array type's lower bound is one, simply
                  --  use one and the length for the bounds.

                  if Expr_Value (Lower_Bnd) = Uint_1 then
                     Gen_Push_Int (Uint_1);
                     Gen_Swap;

                  --  If the lower bound is less than one, assume a low bound
                  --  of zero and decrement length by one to get the upper
                  --  bound. (It seems reasonable to assume zero as the
                  --  lower bound for cases such as "Integer range <>".)

                  else
                     Gen_Push_Int (Uint_0);
                     Gen_Swap;
                     Gen_Push_Int (Uint_1);
                     Gen_Sub;
                  end if;

               --  In cases where the array type's lower bound is nonstatic
               --  or greater than one, we use the lower bound as is, and add
               --  the length to the lower bound and decrement to get the upper
               --  bound.

               else
                  Evaluate_Expr (Lower_Bnd);
                  Gen_Add;
                  Evaluate_Expr (Lower_Bnd);
                  Gen_Swap;
                  Gen_Push_Int (Uint_1);
                  Gen_Sub;
               end if;
            end loop;
         end;
      end if;

      --  Save the upper bounds, lower bounds, and array reference
      --  in reverse order into the unconstrained array pointer.

      while Dimensions > 1 loop
         Gen_Load_Local (Alloc_LV);
         Gen_Swap;
         Gen_Put_Field (Field (Arr_Ref_Class, "last_" & Image (Dimensions)));

         Gen_Load_Local (Alloc_LV);
         Gen_Swap;
         Gen_Put_Field (Field (Arr_Ref_Class, "first_" & Image (Dimensions)));

         Dimensions := Dimensions - 1;
      end loop;

      Gen_Load_Local (Alloc_LV);
      Gen_Swap;
      Gen_Put_Field (Field (Arr_Ref_Class, "last"));

      Gen_Load_Local (Alloc_LV);
      Gen_Swap;
      Gen_Put_Field (Field (Arr_Ref_Class, "first"));

      Gen_Load_Local (Alloc_LV);
      Gen_Swap;
      Gen_Put_Field (Field (Arr_Ref_Class, "all"));

      --  Leave a reference to the unconstrained array pointer
      --  on the stack

      Gen_Load_Local (Alloc_LV);
   end Evaluate_Unconstrained_Array_Ref;

   ------------------------
   -- Evaluate_Attribute --
   ------------------------

   procedure Evaluate_Attribute (Attr : Node_Id) is
      Attr_Kind     : constant Attribute_Id
        := Get_Attribute_Id (Attribute_Name (Attr));
      Attr_Prefix   : Node_Id   := Prefix (Attr);
      Prefix_Type   : Entity_Id := Underlying_Type (Etype (Attr_Prefix));
      Prefix_Obj    : Entity_Id;
      Formal_LV     : Local_Var_Id;
      Formal_First  : Local_Var_Id;
      Formal_Last   : Local_Var_Id;
      Parent_Method : Method_Id;
      Addr          : Address_Descriptor;
      Index_Range   : Node_Id;
      Index_Bound   : Node_Id;
      Dimension     : Pos_8;
      Acc_Type      : Entity_Id;
      Desig_Type    : Entity_Id;

      function Dimension_Value (Attr : Node_Id) return Pos_8;
      --  Returns the static dimension specified in the attribute,
      --  or one if none was specified.

      function Dimension_Value (Attr : Node_Id) return Pos_8 is
         Dimension_Exp : Node_Id;
         Dimension     : Pos_8 := 1;

      begin
         if Present (Expressions (Attr)) then
            Dimension_Exp := First (Expressions (Attr));
            Dimension := Pos_8 (UI_To_Int (Expr_Value (Dimension_Exp)));
         end if;

         return Dimension;
      end Dimension_Value;

   begin
      case Attr_Kind is
         when Attribute_First | Attribute_Last =>
            Dimension := Dimension_Value (Attr);

            if Is_Access_Type (Prefix_Type)
              or else Nkind (Attr_Prefix) = N_Explicit_Dereference
            then
               if Is_Access_Type (Prefix_Type) then
                  Acc_Type := Prefix_Type;
               else
                  Acc_Type := Underlying_Type (Etype (Prefix (Attr_Prefix)));
               end if;
               Desig_Type := Full_Subtype (Designated_Type (Acc_Type));

               if Is_Constrained (Desig_Type) then
                  Index_Range := First_Index (Desig_Type);

                  --  Retrieve the index corresponding to the
                  --  specified dimension.

                  while Dimension > 1 loop
                     Next_Index (Index_Range);
                     Dimension := Dimension - 1;
                  end loop;

                  if Attr_Kind = Attribute_First then
                     Evaluate_Expr (Array_Index_First (Index_Range));
                  else
                     Evaluate_Expr (Array_Index_Last (Index_Range));
                  end if;

               --  The designated type is unconstrained, so load the bound
               --  from the access value's referenced array descriptor.

               else
                  if Is_Access_Type (Prefix_Type) then
                     Evaluate_Expr (Attr_Prefix);

                  --  If the prefix is an N_Explicit_Dereference, then
                  --  evaluate the prefix of the dereference.

                  else
                     Evaluate_Expr (Prefix (Attr_Prefix));
                  end if;

                  --  In the case where the access-to-unconstrained type
                  --  comes from a Java-convention package, the access
                  --  type is represented as a simple reference, so we
                  --  always return zero for First and (length - 1) for
                  --  Last (following subarrays if needed).

                  if Convention (Scope (Acc_Type)) = Convention_Java then
                     if Attr_Kind = Attribute_First then
                        --  Pop prefix evaluation and push zero
                        Gen_Pop;
                        Gen_Push_Int (Uint_0);

                     else
                        Dimension := Dimension_Value (Attr);
                        if Dimension = 1 then
                           Gen_Array_Length;

                        else
                           while Dimension > 1 loop
                              Gen_Push_Int (Uint_0);
                              Gen_Load_Subarray_Reference;
                              Dimension := Dimension - 1;
                           end loop;

                           Gen_Array_Length;
                        end if;

                        Gen_Push_Int (Uint_1);
                        Gen_Sub;
                     end if;

                  --  Normal Ada access-to-unconstrained case

                  else
                     if Dimension = 1 then
                        if Attr_Kind = Attribute_First then
                           Gen_Get_Field
                             (Field (Class_Of_Type
                                       (JVM_Type (Acc_Type)), "first"));
                        else
                           Gen_Get_Field
                             (Field (Class_Of_Type
                                       (JVM_Type (Acc_Type)), "last"));
                        end if;

                     else
                        if Attr_Kind = Attribute_First then
                           Gen_Get_Field
                             (Field (Class_Of_Type (JVM_Type (Acc_Type)),
                                     "first_" & Image (Dimension)));
                        else
                           Gen_Get_Field
                             (Field (Class_Of_Type (JVM_Type (Acc_Type)),
                                     "last_" & Image (Dimension)));
                        end if;
                     end if;
                  end if;
               end if;

            elsif Is_Scalar_Type (Prefix_Type) then
               if Attr_Kind = Attribute_First then
                  Evaluate_Expr (Type_Low_Bound (Prefix_Type));
               else
                  Evaluate_Expr (Type_High_Bound (Prefix_Type));
               end if;

            elsif Ekind (Prefix_Type) in Einfo.Array_Kind then

               --  If the prefix denotes a renamed object, then use
               --  the denoted object directly (traversing multiple
               --  levels of renamings if necessary), in order to
               --  pick up its bounds properly.

               while Is_Entity_Name (Attr_Prefix)
                 and then Present (Renamed_Object (Entity (Attr_Prefix)))
               loop
                  Attr_Prefix := Renamed_Object (Entity (Attr_Prefix));
               end loop;

               if Nkind (Attr_Prefix) = N_Type_Conversion
                 or else Nkind (Attr_Prefix) = N_Unchecked_Type_Conversion
               then
                  Attr_Prefix := Expression (Attr_Prefix);
                  Prefix_Type := Underlying_Type (Etype (Attr_Prefix));
               end if;

               if Is_Constrained (Prefix_Type) then
                  --  If the prefix is a function call then evaluate
                  --  it simply for its side effects and pop the result.
                  --  Note that we still don't handle the general case
                  --  of prefixes involving function calls. ???

                  if Nkind (Attr_Prefix) = N_Function_Call then
                     Evaluate_Expr (Attr_Prefix);
                     Gen_Pop;

                  elsif Nkind (Attr_Prefix) = N_Selected_Component
                    and then Nkind (Prefix (Attr_Prefix)) = N_Function_Call
                  then
                     Evaluate_Expr (Prefix (Attr_Prefix));
                     Gen_Pop;
                  end if;

                  Index_Range := First_Index (Prefix_Type);

                  --  Retrieve the index corresponding to the
                  --  specified dimension

                  while Dimension > 1 loop
                     Next_Index (Index_Range);
                     Dimension := Dimension - 1;
                  end loop;

                  if Attr_Kind = Attribute_First then
                     Index_Bound := Array_Index_First (Index_Range);
                  else
                     Index_Bound := Array_Index_Last (Index_Range);
                  end if;

                  --  If the bound is a bare discriminant, then evaluate
                  --  the prefix of the attribute's selected component
                  --  prefix and load the discriminant.

                  if Nkind (Attr_Prefix) = N_Selected_Component
                    and then Is_Entity_Name (Index_Bound)
                    and then
                      Ekind (Entity (Index_Bound)) = E_Discriminant
                  then
                     Evaluate_Expr (Prefix (Attr_Prefix));
                     Gen_Get_Object_Field
                       (JVM_Field (Entity (Index_Bound)));

                  --  Otherwise, evaluate the bound expression

                  else
                     Evaluate_Expr (Index_Bound);
                  end if;

               --  Case of function with unconstrained result subtype

               elsif Nkind (Attr_Prefix) = N_Function_Call then

                  --  Evaluate the function call for any side effects

                  Evaluate_Expr (Attr_Prefix);

                  --  For now we cheat on the array bounds because we
                  --  don't yet support proper returning of bounds
                  --  from unconstrained array function calls. For
                  --  'First we always use a value of one and for 'Last
                  --  we use the length of the specified dimension. ???

                  if Attr_Kind = Attribute_First then
                     Gen_Pop;
                     Gen_Push_Int (Uint_1);

                  else
                     --  For a multidimensional array function result, traverse
                     --  the array, indexing each dimension by 0 to get the
                     --  reference to a subarray at the desired level and use
                     --  the length of the subarray as the value of 'Last (but
                     --  that's not really correct as noted above, and it won't
                     --  work properly for the case of null arrays). ???

                     for D in 2 .. Dimension loop
                        Gen_Push_Int (Uint_0);
                        Gen_Load_Subarray_Reference;
                     end loop;

                     Gen_Array_Length;
                  end if;

               --  Case of unconstrained formal parameter

               else
                  Prefix_Obj := Entity (Attr_Prefix);

                  if Ekind (Prefix_Obj) not in Formal_Kind then
                     pragma Assert (False);
                     raise Program_Error;

                  else
                     Parent_Method := Enclosing_Method (Prefix_Obj);
                     Formal_LV := JVM_Local_Var (Prefix_Obj);
                     Formal_First := Next_Local_Var (Formal_LV);

                     --  Retrieve the formal parameter corresponding
                     --  to the lower bound of the specified dimension.
                     --  Note that it's necessary to step over the
                     --  upper bound parameter of each dimension to
                     --  get to the next lower bound parameter.

                     while Dimension > 1 loop
                        Formal_First
                          := Next_Local_Var
                               (Next_Local_Var (Formal_First));

                        Dimension := Dimension - 1;
                     end loop;

                     if Attr_Kind = Attribute_First then
                        if Parent_Method = Current_Method then
                           Gen_Load_Local (Formal_First);
                        else
                           Register_Up_Level_Reference
                             (Parent_Method, Formal_First);
                           Load_Up_Level_Field
                             (Parent_Method, Name (Formal_First));
                        end if;
                     else
                        Formal_Last
                          := Next_Local_Var (Formal_First);
                        if Parent_Method = Current_Method then
                           Gen_Load_Local (Formal_Last);
                        else
                           Register_Up_Level_Reference
                             (Parent_Method, Formal_Last);
                           Load_Up_Level_Field
                             (Parent_Method, Name (Formal_Last));
                        end if;
                     end if;
                  end if;
               end if;
            end if;

         when Attribute_Length =>
            Dimension := Dimension_Value (Attr);

            if Is_Entity_Name (Attr_Prefix)
              and then Is_Type (Entity (Attr_Prefix))
            then
               Index_Range := First_Index (Prefix_Type);

               --  Retrieve the index corresponding to the
               --  specified dimension

               while Dimension > 1 loop
                  Next_Index (Index_Range);
                  Dimension := Dimension - 1;
               end loop;

               Load_Index_Length (Index_Range);

            else
               if Is_Access_Type (Prefix_Type)
                 and then Convention (Scope (Prefix_Type)) /= Convention_Java
               then
                  Evaluate_Expr (Attr_Prefix);
                  if not Is_Constrained
                           (Designated_Type (Full_Type (Prefix_Type)))
                  then
                     Gen_Get_Field
                       (Field (Class_Of_Type (JVM_Type (Prefix_Type)), "all"));
                  end if;
               else
                  Evaluate_Array_Address (Attr_Prefix);
               end if;

               --  Multidimensional array lengths require following
               --  levels of indirection. However, this method won't
               --  work properly in general for arrays with dimensions
               --  of length zero, which is very annoying! ???

               while Dimension > 1 loop
                  Gen_Push_Int (Uint_0);
                  Gen_Load_Subarray_Reference;
                  Dimension := Dimension - 1;
               end loop;

               --  Now retrieve the length of an array directly from
               --  the JVM.

               Gen_Array_Length;
            end if;

            --  If the type is Universal_Integer then for now we always
            --  convert to long, though this is sometimes pessimistic,
            --  but Universal_Integer expressions can require the largest
            --  range in some cases. Normally this will only be in the
            --  context of relational expressions. Eventually we should
            --  try to recognize cases where the expression only requires
            --  int range. ???

            if Etype (Attr) = Universal_Integer then
               Gen_Conversion (Long_Type);
            end if;

         when Attribute_Max | Attribute_Min =>
            declare
               Compare_Label : constant Label_Id := New_Label;
               Check_State   : Boolean;

            begin
               Evaluate_Expr (First (Expressions (Attr)));
               Evaluate_Expr (Last  (Expressions (Attr)));
               Gen_Double_Duplicate;

               Suppress_Stack_Checking (Check_State);

               if Attr_Kind = Attribute_Max then
                  Gen_Compare_Branch_Greater_Equal (Compare_Label);
               else
                  Gen_Compare_Branch_Less_Equal (Compare_Label);
               end if;

               Gen_Swap;
               Gen_Label (Compare_Label);
               Gen_Pop;

               Restore_Stack_Checking (Check_State);
            end;

         when Attribute_Pos =>
            Evaluate_Expr (First (Expressions (Attr)));

            --  If the Pos attribute appears within a context
            --  requiring a universal or 64-bit value, then
            --  ensure that its type is long by conversion if
            --  necessary, since the argument of the attribute
            --  can be a 32-bit int and the enclosing operation
            --  may require the larger range.

            if JVM_Type (Etype (Attr)) = Long_Type
              and then Top_Type /= Long_Type
            then
               Gen_Conversion (Long_Type);
            end if;

         when Attribute_Pred =>
            Evaluate_Expr (First (Expressions (Attr)));
            Gen_Push_Int (Uint_1);
            Gen_Sub;

         when Attribute_Range_Length =>

            Load_Index_Length (Full_Subtype (Prefix (Attr)));

            --  If the type is Universal_Integer then for now we always
            --  convert to long, though this is sometimes pessimistic,
            --  but Universal_Integer expressions can require the largest
            --  range in some cases. Normally this will only be in the
            --  context of relational expressions. Eventually we should
            --  try to recognize cases where the expression only requires
            --  int range. ???

            if Etype (Attr) = Universal_Integer then
               Gen_Conversion (Long_Type);
            end if;

         when Attribute_Succ =>
            Evaluate_Expr (First (Expressions (Attr)));
            Gen_Push_Int (Uint_1);
            Gen_Add;

         when Attribute_Size | Attribute_Max_Size_In_Storage_Elements =>

            --  The Size attribute is only partially supported,
            --  in particular when the size is statically known.
            --  Other cases will require specialized type handling
            --  return zero for now. ???

            if Is_Entity_Name (Attr_Prefix)
              and then Is_Type (Entity (Attr_Prefix))
            then
               if JVM_Type (Etype (Attr)) = Int_Type then
                  Gen_Push_Int (Get_RM_Size (Full_Subtype (Attr)));
               else
                  Gen_Push_Long (Get_RM_Size (Full_Subtype (Attr)));
               end if;

            elsif JVM_Type (Etype (Attr)) = Int_Type then
               Gen_Push_Int (Esize (Full_Subtype (Attr_Prefix)));

            else
               Gen_Push_Long (Esize (Full_Subtype (Attr_Prefix)));
            end if;

            --  It's not clear what we should use for the JVM's
            --  storage element size. Arrays of bytes and characters
            --  are supported, but normal scalar objects require
            --  a minimum of 32 bits. For now we use Character'Size.

            if Attr_Kind = Attribute_Max_Size_In_Storage_Elements then
               if JVM_Type (Etype (Attr)) = Int_Type then
                  Gen_Push_Int (Get_RM_Size (Standard_Character));
               else
                  Gen_Push_Long (Get_RM_Size (Standard_Character));
               end if;
               Gen_Div;
            end if;

         when Attribute_Val =>
            Evaluate_Expr (First (Expressions (Attr)));

            --  The result of the attribute argument's evaluation can be
            --  universal integer, in which case a type conversion may
            --  be needed.

            if Top_Type /= JVM_Type (Etype (Attr)) then
               Gen_Conversion (JVM_Type (Etype (Attr)));
            end if;

         when Attribute_Access |
              Attribute_Unchecked_Access |
              Attribute_Unrestricted_Access |
              Attribute_Address |
              Attribute_Code_Address =>

            --  If this is an unconstrained array access attribute, then we
            --  have to construct a compound pointer object that contains
            --  both the array reference and its bounds.

            if Attr_Kind /= Attribute_Address
              and then Attr_Kind /= Attribute_Code_Address
              and then
                Ekind (Designated_Type (Full_Type (Attr))) in Einfo.Array_Kind
              and then not Is_Constrained (Designated_Type (Full_Type (Attr)))
              and then Convention (Scope (Full_Type (Attr))) /= Convention_Java
            then
               Evaluate_Unconstrained_Array_Ref
                 (Attr_Prefix, Full_Type (Attr));

            --  In all other cases simply evaluate the prefix

            else
               if Attr_Kind /= Attribute_Address then
                  Addr := Evaluate_Addr (Attr_Prefix);

               else
                  --  In the case of Address applied to an array we call
                  --  Evaluate_Array_Address in order to step past the
                  --  the ".all" component of access-to-unconstrained
                  --  objects so that we get a true array reference.

                  if Ekind (Prefix_Type) in Einfo.Array_Kind then
                     Evaluate_Array_Address (Attr_Prefix);

                  --  If this is an unwrapped elementary object, then
                  --  we can't take its address, so we just push null.
                  --  Perhaps this should be rejected with an error
                  --  or warning (note that it occurs in Ada validation
                  --  tests that are simply checking for the presence of
                  --  the attribute, without actually using the result). ???

                  elsif Ekind (Prefix_Type) in Elementary_Kind
                    and then not Is_Aliased_View (Attr_Prefix)
                  then
                     Gen_Push_Null;

                  --  Otherwise it's safe to simply get the address

                  else
                     Addr := Evaluate_Addr (Attr_Prefix);
                  end if;
               end if;

               --  Type System.Address is associated with java.lang.Object,
               --  so we change the type of the prefix result to match.

               if Attr_Kind = Attribute_Address
                 or else Attr_Kind = Attribute_Code_Address
               then
                  Pop_Type;
                  Push_Type (Type_Of (Java_Lang_Object));
               end if;
            end if;

         when others =>
            Error_Msg_Name_1 := Attribute_Name (Attr);
            Error_Msg_N ("unimplemented attribute: %", Attr);

      end case;
   end Evaluate_Attribute;

   -------------------------------
   -- Evaluate_Array_Comparison --
   -------------------------------

   procedure Evaluate_Array_Comparison
     (Op        : Node_Kind;
      Left_Arr  : Node_Id;
      Right_Arr : Node_Id;
      Jtype     : Type_Id)
   is
      Lft_Is_Slice : Boolean;
      Lft_Prefix   : Node_Id;
      Lft_Subt     : Entity_Id;
      Rgt_Is_Slice : Boolean;
      Rgt_Prefix   : Node_Id;
      Rgt_Subt     : Entity_Id;

      Lbl_F        : constant Label_Id := New_Label;
      Lbl_T        : constant Label_Id := New_Label;
      Lbl_End      : constant Label_Id := New_Label;
      Check_State  : Boolean;

      procedure Compare_Subarrays (Dimensions : Pos_8);
      --  Generates code to compare the subarrays of the remaining
      --  Dimensions number of dimensions of two array objects.
      --  Requires that references to two arrays are on the stack.

      procedure Compare_Subarrays (Dimensions : Pos_8) is
         Left_Temp   : constant Local_Var_Id
           := New_Local_Var ("_l_eqtmp", Jtype);
         Right_Temp  : constant Local_Var_Id
           := New_Local_Var ("_r_eqtmp", Jtype);
         Length      : constant Local_Var_Id
           := New_Local_Var ("_index_max", Int_Type);
         Counter     : constant Local_Var_Id
           := New_Local_Var ("_loop_cnt", Int_Type);
         Left_Index  : Local_Var_Id
           := Counter;
         Right_Index : Local_Var_Id
           := Counter;
         Loop_Head   : constant Label_Id := New_Label;

      begin
         --  If the right operand is a slice then initialize its index temp
         --  before saving the array reference (this is necessary because
         --  Gen_Array_Subscript requires the array reference to be on the
         --  stack in certain cases).

         if Rgt_Is_Slice then
            Right_Index := New_Local_Var ("_r_index", Int_Type);
            Gen_Array_Subscript (Rgt_Prefix, Index_First (Rgt_Subt));
            Gen_Store_Local (Right_Index);
         end if;

         --  Now save the reference to the right array operand

         Gen_Store_Local (Right_Temp);

         --  If the left operand is a slice then initialize its index temp
         --  before saving the array reference (this is necessary because
         --  Gen_Array_Subscript requires the array reference to be on the
         --  stack in certain cases).

         if Lft_Is_Slice then
            Left_Index := New_Local_Var ("_l_index", Int_Type);
            Gen_Array_Subscript (Lft_Prefix, Index_First (Lft_Subt));
            Gen_Store_Local (Left_Index);
         end if;

         --  Now save the reference to the left array operand

         Gen_Store_Local (Left_Temp);

         --  First compare the array lengths and skip the
         --  array comparison if the lengths are unequal.

         if Lft_Is_Slice then
            Load_Index_Length (First_Index (Lft_Subt));
         else
            Gen_Load_Local (Left_Temp);
            Gen_Array_Length;
         end if;
         Gen_Duplicate;
         Gen_Store_Local (Length);

         if Rgt_Is_Slice then
            Load_Index_Length (First_Index (Rgt_Subt));
         else
            Gen_Load_Local (Right_Temp);
            Gen_Array_Length;
         end if;

         if Op = N_Op_Eq then
            Gen_Compare_Branch_Not_Equal (Lbl_F);

         elsif Op = N_Op_Ne then
            Gen_Compare_Branch_Not_Equal (Lbl_T);

         else
            pragma Assert (False);
            raise Program_Error;
         end if;

         --  Generate a loop to compare subarrays or array elements

         --  Initialize loop counter to zero

         Gen_Push_Int (Uint_0);
         Gen_Store_Local (Counter);

         Gen_Label (Loop_Head);

         --  Check for end of loop (Counter = Length)

         Gen_Load_Local (Counter);
         Gen_Load_Local (Length);

         if Op = N_Op_Eq then
            Gen_Compare_Branch_Equal (Lbl_T);

         elsif Op = N_Op_Ne then
            Gen_Compare_Branch_Equal (Lbl_F);

         else
            pragma Assert (False);
            raise Program_Error;
         end if;

         --  When Dimensions = 1 we are at the bottommost dimension of
         --  the arrays so we now compare actual array elements.

         if Dimensions = 1 then
            --  Load left array element

            Gen_Load_Local (Left_Temp);
            Gen_Load_Local (Left_Index);
            Gen_Load_Array_Element;

            --  Load right array element

            Gen_Load_Local (Right_Temp);
            Gen_Load_Local (Right_Index);
            Gen_Load_Array_Element;

            --  Compare and exit if L(I) /= R(I)

            if Op = N_Op_Eq then
               Gen_Compare_Branch_Not_Equal (Lbl_F);

            elsif Op = N_Op_Ne then
               Gen_Compare_Branch_Not_Equal (Lbl_T);

            else
               pragma Assert (False);
               raise Program_Error;
            end if;

         --  If Dimensions > 1, then we load references to the subarrays
         --  and recurse to generate a comparison of the subarrays of the
         --  next dimension.

         else
            --  Load left subarray reference

            Gen_Load_Local (Left_Temp);
            Gen_Load_Local (Counter);
            Gen_Load_Subarray_Reference;

            --  Load right subarray reference

            Gen_Load_Local (Right_Temp);
            Gen_Load_Local (Counter);
            Gen_Load_Subarray_Reference;

            Compare_Subarrays (Dimensions - 1);
         end if;

         --  Increment loop counter and indexes, then iterate

         Gen_Incr_Local (Counter, Uint_1);

         if Lft_Is_Slice then
            Gen_Incr_Local (Left_Index, Uint_1);
         end if;

         if Rgt_Is_Slice then
            Gen_Incr_Local (Right_Index, Uint_1);
         end if;

         Gen_Goto (Loop_Head);
      end Compare_Subarrays;

   --  Start of processing for Evaluate_Array_Comparison

   begin
      Suppress_Stack_Checking (Check_State);

      Test_For_Slice (Left_Arr,  Lft_Is_Slice, Lft_Prefix, Lft_Subt);
      Test_For_Slice (Right_Arr, Rgt_Is_Slice, Rgt_Prefix, Rgt_Subt);

      Compare_Subarrays (Dimensionality (Jtype));

      --  Generate a push of True and False values for the result of
      --  the comparison. The comparisons generated by Compare_Subarrays
      --  will branch to the code following the goto to Lbl_End as soon
      --  as a result is known (or else the code will fall through to the
      --  code preceding the Lbl_End goto to push the result).

      if Op = N_Op_Eq then
         Gen_Label (Lbl_T);
         Gen_Push_Int (Uint_1);

      elsif Op = N_Op_Ne then
         Gen_Label (Lbl_F);
         Gen_Push_Int (Uint_0);

      else
         pragma Assert (False);
         raise Program_Error;
      end if;

      Gen_Goto (Lbl_End);

      Mark_Stack;
      if Op = N_Op_Eq then
         Gen_Label (Lbl_F);
         Gen_Push_Int (Uint_0);

      elsif Op = N_Op_Ne then
         Gen_Label (Lbl_T);
         Gen_Push_Int (Uint_1);

      else
         pragma Assert (False);
         raise Program_Error;
      end if;
      Release_Stack;

      Gen_Label (Lbl_End);

      Restore_Stack_Checking (Check_State);
   end Evaluate_Array_Comparison;

   -----------------------
   -- Evaluate_Operator --
   -----------------------

   procedure Evaluate_Operator
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean)
   is
      Exp_Jtype   : Type_Id;
      Exp_Kind    : Node_Kind := Nkind (Expr);
      Opnd_Kind   : constant Entity_Kind
        := Ekind (Full_Type (Right_Opnd (Expr)));
      Check_State : Boolean;

      procedure Check_For_Unsigned_Comparison;
      --  Checks if the operands are full-width modular values and
      --  if so then generates a test and swaps the operands if they
      --  differ in sign (which requires a reversal of the comparison).

      function Invalid_Address_Comparison return Boolean;
      --  Checks if this is an operator applied to System.Address
      --  operands, and in that case it emits a warning and pushes
      --  a False result on the stack after popping the operands.
      --  Only intended to be called in the case of relational
      --  operators that are not supported for object references
      --  on the JVM (>, <, >=, <=). Returns True if the comparison
      --  is invalid, otherwise returns False.

      procedure Check_For_Unsigned_Comparison is
         No_Swap_Label : constant Label_Id := New_Label;
         Check_State   : Boolean;

      begin
         if Opnd_Kind in Modular_Integer_Kind
           and then Modulus (Full_Type (Right_Opnd (Expr))) >= 2 ** Uint_32
         then
            Suppress_Stack_Checking (Check_State);

            Gen_Double_Duplicate;
            Gen_Xor;
            Gen_Branch_Greater_Equal (No_Swap_Label);
            Gen_Swap;
            Gen_Label (No_Swap_Label);

            Restore_Stack_Checking (Check_State);
         end if;
      end Check_For_Unsigned_Comparison;

      function Invalid_Address_Comparison return Boolean is
      begin
         --  Address comparisons other than "=" and "/=" do not make
         --  sense on the JVM, so issue a warning, and generate a
         --  false result anyway to allow compilation to continue.

         if JVM_Type (Left_Opnd (Expr)) = Type_Of (Java_Lang_Object) then
            Error_Msg_N ("unsupported form of address comparison?", Expr);

            Gen_Pop (2);
            Gen_Push_Int (Uint_0);

            return True;

         else
            return False;
         end if;
      end Invalid_Address_Comparison;

   --  Start of processing for Evaluate_Operator

   begin
      if Exp_Kind in N_Binary_Op then
         if Opnd_Kind in Einfo.Array_Kind then
            Evaluate_Array_Address (Left_Opnd (Expr));
         else
            Evaluate_Expr (Left_Opnd (Expr));
         end if;
      end if;

      --  We handle "not" specially, see below.

      if Exp_Kind /= N_Op_Not then
         if Opnd_Kind in Einfo.Array_Kind then
            Evaluate_Array_Address (Right_Opnd (Expr));
         else
            Evaluate_Expr (Right_Opnd (Expr));
         end if;
      end if;

      --  If this is a "/=" operator expanded to 'not "="',
      --  then we handle this specially, treating it directly
      --  as if it were N_Op_Ne.

      if Exp_Kind = N_Op_Eq
        and then Nkind (Parent (Expr)) = N_Op_Not
      then
         Exp_Kind := N_Op_Ne;
      end if;

      case Exp_Kind is
         when N_Op_Abs =>
            declare
               Non_Neg_Lbl : constant Label_Id := New_Label;

            begin
               Gen_Duplicate;
               Suppress_Stack_Checking (Check_State);
               Gen_Branch_Greater_Equal (Non_Neg_Lbl);
               Gen_Neg;
               Gen_Label (Non_Neg_Lbl);
               Restore_Stack_Checking (Check_State);
            end;

         when N_Op_Add =>
            Gen_Add;

            --  If this is a modular integer addition, then normalize
            --  by computing the sum modulo the type's modulus.

            if Opnd_Kind in Modular_Integer_Kind
              and then Modulus (Full_Type (Expr)) < 2 ** Uint_32
            then
               Gen_Push_Int (Modulus (Full_Type (Expr)));
               Gen_Rem;
            end if;

         when N_Op_And =>
            Gen_And;

         when N_Op_Divide =>
            Gen_Div;

         when N_Op_Eq =>
            if Opnd_Kind in Einfo.Array_Kind then
               Exp_Jtype := JVM_Type (Full_Type (Right_Opnd (Expr)));
               Evaluate_Array_Comparison
                 (N_Op_Eq, Left_Opnd (Expr), Right_Opnd (Expr), Exp_Jtype);

            else
               if Label /= Null_Label then
                  if True_Branch then
                     Gen_Compare_Branch_Equal (Label);
                  else
                     Gen_Compare_Branch_Not_Equal (Label);
                  end if;
                  Label := Null_Label;
               else
                  declare
                     Lbl_T : constant Label_Id := New_Label;
                     Lbl_F : constant Label_Id := New_Label;

                  begin
                     Suppress_Stack_Checking (Check_State);

                     Gen_Compare_Branch_Equal (Lbl_T);
                     Gen_Push_Int (Uint_0);
                     Gen_Goto (Lbl_F);

                     Gen_Label (Lbl_T);
                     Mark_Stack;
                     Gen_Push_Int (Uint_1);
                     Release_Stack;

                     Gen_Label (Lbl_F);

                     Restore_Stack_Checking (Check_State);
                  end;
               end if;
            end if;

         when N_Op_Ge =>
            if Invalid_Address_Comparison then
               return;
            end if;

            Check_For_Unsigned_Comparison;

            if Label /= Null_Label then
               if True_Branch then
                  Gen_Compare_Branch_Greater_Equal (Label);
               else
                  Gen_Compare_Branch_Less (Label);
               end if;
               Label := Null_Label;
            else
               declare
                  Lbl_T : constant Label_Id := New_Label;
                  Lbl_F : constant Label_Id := New_Label;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Compare_Branch_Greater_Equal (Lbl_T);
                  Gen_Push_Int (Uint_0);
                  Gen_Goto (Lbl_F);

                  Gen_Label (Lbl_T);
                  Mark_Stack;
                  Gen_Push_Int (Uint_1);
                  Release_Stack;

                  Gen_Label (Lbl_F);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when N_Op_Gt =>
            if Invalid_Address_Comparison then
               return;
            end if;

            Check_For_Unsigned_Comparison;

            if Label /= Null_Label then
               if True_Branch then
                  Gen_Compare_Branch_Greater (Label);
               else
                  Gen_Compare_Branch_Less_Equal (Label);
               end if;
               Label := Null_Label;
            else
               declare
                  Lbl_T : constant Label_Id := New_Label;
                  Lbl_F : constant Label_Id := New_Label;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Compare_Branch_Greater (Lbl_T);
                  Gen_Push_Int (Uint_0);
                  Gen_Goto (Lbl_F);

                  Gen_Label (Lbl_T);
                  Mark_Stack;
                  Gen_Push_Int (Uint_1);
                  Release_Stack;

                  Gen_Label (Lbl_F);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when N_Op_Le =>
            if Invalid_Address_Comparison then
               return;
            end if;

            Check_For_Unsigned_Comparison;

            if Label /= Null_Label then
               if True_Branch then
                  Gen_Compare_Branch_Less_Equal (Label);
               else
                  Gen_Compare_Branch_Greater (Label);
               end if;
               Label := Null_Label;
            else
               declare
                  Lbl_T : constant Label_Id := New_Label;
                  Lbl_F : constant Label_Id := New_Label;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Compare_Branch_Less_Equal (Lbl_T);
                  Gen_Push_Int (Uint_0);
                  Gen_Goto (Lbl_F);

                  Gen_Label (Lbl_T);
                  Mark_Stack;
                  Gen_Push_Int (Uint_1);
                  Release_Stack;

                  Gen_Label (Lbl_F);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when N_Op_Lt =>
            if Invalid_Address_Comparison then
               return;
            end if;

            Check_For_Unsigned_Comparison;

            if Label /= Null_Label then
               if True_Branch then
                  Gen_Compare_Branch_Less (Label);
               else
                  Gen_Compare_Branch_Greater_Equal (Label);
               end if;
               Label := Null_Label;
            else
               declare
                  Lbl_T : constant Label_Id := New_Label;
                  Lbl_F : constant Label_Id := New_Label;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Compare_Branch_Less (Lbl_T);
                  Gen_Push_Int (Uint_0);
                  Gen_Goto (Lbl_F);

                  Gen_Label (Lbl_T);
                  Mark_Stack;
                  Gen_Push_Int (Uint_1);
                  Release_Stack;

                  Gen_Label (Lbl_F);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;

         when N_Op_Ne =>
            if Opnd_Kind in Einfo.Array_Kind then
               Exp_Jtype := JVM_Type (Full_Type (Right_Opnd (Expr)));
               Evaluate_Array_Comparison
                 (N_Op_Ne, Left_Opnd (Expr), Right_Opnd (Expr), Exp_Jtype);

            else
               if Label /= Null_Label then
                  if True_Branch then
                     Gen_Compare_Branch_Not_Equal (Label);
                  else
                     Gen_Compare_Branch_Equal (Label);
                  end if;
                  Label := Null_Label;
               else
                  declare
                     Lbl_T : constant Label_Id := New_Label;
                     Lbl_F : constant Label_Id := New_Label;

                  begin
                     Suppress_Stack_Checking (Check_State);

                     Gen_Compare_Branch_Not_Equal (Lbl_T);
                     Gen_Push_Int (Uint_0);
                     Gen_Goto (Lbl_F);

                     Gen_Label (Lbl_T);
                     Mark_Stack;
                     Gen_Push_Int (Uint_1);
                     Release_Stack;

                     Gen_Label (Lbl_F);

                     Restore_Stack_Checking (Check_State);
                  end;
               end if;
            end if;

         when N_Op_Mod =>

            declare
               Mod_Jtype   : constant Type_Id
                 := JVM_Type (Full_Type (Right_Opnd (Expr)));
               Dividend_LV : constant Local_Var_Id
                 := New_Local_Var ("_mod_l", Mod_Jtype);
               Divisor_LV  : constant Local_Var_Id
                 := New_Local_Var ("_mod_r", Mod_Jtype);
               Normal_Lbl  : constant Label_Id := New_Label;
               Exit_Lbl    : constant Label_Id := New_Label;
               Check_State : Boolean;

            begin
               Suppress_Stack_Checking (Check_State);

               Gen_Duplicate;
               Gen_Store_Local (Divisor_LV);
               Gen_Swap;
               Gen_Duplicate;
               Gen_Store_Local (Dividend_LV);
               Gen_Xor;
               Gen_Branch_Greater_Equal (Normal_Lbl);

               Gen_Load_Local (Dividend_LV);
               Gen_Load_Local (Divisor_LV);
               Gen_Rem;
               Gen_Duplicate;
               Gen_Branch_Equal (Exit_Lbl);

               Gen_Load_Local (Divisor_LV);
               Gen_Add;

               Gen_Goto (Exit_Lbl);

               Gen_Label (Normal_Lbl);

               Mark_Stack;
               Gen_Load_Local (Dividend_LV);
               Gen_Load_Local (Divisor_LV);
               Gen_Rem;
               Release_Stack;

               Gen_Label (Exit_Lbl);

               Restore_Stack_Checking (Check_State);
            end;

         when N_Op_Multiply =>
            Gen_Mul;

            --  If this is a modular integer multiplication, then normalize
            --  by computing the product modulo the type's modulus.

            if Opnd_Kind in Modular_Integer_Kind
              and then Modulus (Full_Type (Expr)) < 2 ** Uint_32
            then
               Gen_Push_Int (Modulus (Full_Type (Expr)));
               Gen_Rem;
            end if;

         when N_Op_Or =>
            Gen_Or;

         when N_Op_Rem =>
            Gen_Rem;

         when N_Op_Subtract =>
            Gen_Sub;

            --  If this is a modular integer subtraction, then normalize
            --  by first adding the modulus (to compensate for a possible
            --  negative difference) and then taking the result modulo the
            --  type's modulus.

            if Opnd_Kind in Modular_Integer_Kind
              and then Modulus (Full_Type (Expr)) < 2 ** Uint_32
            then
               Gen_Push_Int (Modulus (Full_Type (Expr)));
               Gen_Add;
               Gen_Push_Int (Modulus (Full_Type (Expr)));
               Gen_Rem;
            end if;

         when N_Op_Xor =>
            Gen_Xor;

         when N_Op_Minus =>
            --  If this is a modular integer negation, then subtract the
            --  operand from the modulus, except that -0 equals 0.

            if Opnd_Kind in Modular_Integer_Kind
              and then Modulus (Full_Type (Expr)) < 2 ** Uint_32
            then
               declare
                  Zero_Label  : constant Label_Id := New_Label;
                  Check_State : Boolean;

               begin
                  Suppress_Stack_Checking (Check_State);

                  Gen_Duplicate;
                  Gen_Branch_Equal (Zero_Label);
                  Gen_Push_Int (Modulus (Full_Type (Expr)));
                  Gen_Swap;
                  Gen_Sub;
                  Gen_Label (Zero_Label);

                  Restore_Stack_Checking (Check_State);
               end;

            --  Otherwise simply negate the operand

            else
               Gen_Neg;
            end if;

         when N_Op_Not =>
            --  If this is a rewritten "/=" operator then we pass
            --  along any label and let the evaluation of the "="
            --  detect that the parent is "not", so as to generate
            --  more efficient code.

            if Nkind (Right_Opnd (Expr)) = N_Op_Eq then
               Evaluate_Expr (Right_Opnd (Expr), Label, True_Branch);

            --  If the type is modular, then simply subtract from the upper
            --  bound of the base range, unless this is a full-length type,
            --  in which case we can simply apply a full-width complement.

            elsif Opnd_Kind in Modular_Integer_Kind then
               Evaluate_Expr (Right_Opnd (Expr));

               if Modulus (Full_Type (Expr)) >= 2 ** Uint_32 then
                  Gen_Not;

               else
                  Gen_Push_Int (Modulus (Base_Type (Full_Type (Expr))) - 1);
                  Gen_Swap;
                  Gen_Sub;
               end if;

            else
               --  Note: We can't call Gen_Not here, because that will
               --  perform a full-word bit complement, which is not
               --  correct for results of type Standard.Boolean. Perhaps
               --  there should be an operation called Gen_Boolean_Not
               --  in JVM which would generate the XOR with 1. ???

               Evaluate_Expr (Right_Opnd (Expr));
               Gen_Push_Int (Uint_1);
               Gen_Xor;
            end if;

         when N_Op_Plus =>
            null;

         when N_Op_Shift_Left =>
            Gen_Shift_Left (RM_Size (Etype (Left_Opnd (Expr))));

         when N_Op_Shift_Right_Arithmetic =>
            Gen_Shift_Right_Arithmetic (RM_Size (Etype (Left_Opnd (Expr))));

         when N_Op_Shift_Right =>
            Gen_Shift_Right_Logical;

         when N_Op_Rotate_Left =>
            Gen_Rotate_Left (RM_Size (Etype (Left_Opnd (Expr))));

         when N_Op_Rotate_Right =>
            Gen_Rotate_Right (RM_Size (Etype (Left_Opnd (Expr))));

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Evaluate_Operator;

   -----------------------
   -- Evaluate_And_Then --
   -----------------------

   procedure Evaluate_And_Then
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean)
   is
   begin
      if Label /= Null_Label then
         if True_Branch then
            declare
               False_Label       : constant Label_Id := New_Label;
               Save_False_Label  : Label_Id := False_Label;
               Save_True_Label   : Label_Id := Label;

            begin
               Evaluate_Expr (Left_Opnd (Expr), Save_False_Label, False);
               if Save_False_Label /= Null_Label then
                  Gen_Branch_Equal (False_Label);
               end if;
               Evaluate_Expr (Right_Opnd (Expr), Save_True_Label, True);
               if Save_True_Label /= Null_Label then
                  Gen_Branch_Not_Equal (Label);
               end if;
               Gen_Label (False_Label);
               Label := Null_Label;
            end;
         else
            declare
               Save_Label : Label_Id := Label;

            begin
               Evaluate_Expr (Left_Opnd (Expr), Save_Label, False);
               if Save_Label /= Null_Label then
                  Gen_Branch_Equal (Label);
               end if;
               Save_Label := Label;
               Evaluate_Expr (Right_Opnd (Expr), Save_Label, False);
               if Save_Label /= Null_Label then
                  Gen_Branch_Equal (Label);
               end if;
               Label := Null_Label;
            end;
         end if;

      --  If no branch label is available, we have to produce a Boolean
      --  result (e.g., for a statement like B0 := B1 and then B2).

      else
         declare
            False_Label      : constant Label_Id := New_Label;
            True_Label       : constant Label_Id := New_Label;
            Save_False_Label : Label_Id := False_Label;
            Check_State      : Boolean;

         begin
            Suppress_Stack_Checking (Check_State);

            Evaluate_Expr (Left_Opnd (Expr), Save_False_Label, False);
            if Save_False_Label /= Null_Label then
               Gen_Branch_Equal (False_Label);
            end if;

            Save_False_Label := False_Label;
            Evaluate_Expr (Right_Opnd (Expr), Save_False_Label, False);
            if Save_False_Label /= Null_Label then
               Gen_Branch_Equal (False_Label);
            end if;

            Gen_Push_Int (Uint_1);
            Gen_Goto (True_Label);

            Gen_Label (False_Label);
            Mark_Stack;
            Gen_Push_Int (Uint_0);
            Release_Stack;

            Gen_Label (True_Label);

            Restore_Stack_Checking (Check_State);
         end;
      end if;
   end Evaluate_And_Then;

   ----------------------
   -- Evaluate_Or_Else --
   ----------------------

   procedure Evaluate_Or_Else
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean)
   is
   begin
      if Label /= Null_Label then
         if True_Branch then
            declare
               Save_Label : Label_Id := Label;

            begin
               Evaluate_Expr (Left_Opnd (Expr), Save_Label, True);
               if Save_Label /= Null_Label then
                  Gen_Branch_Not_Equal (Label);
               end if;
               Save_Label := Label;
               Evaluate_Expr (Right_Opnd (Expr), Save_Label, True);
               if Save_Label /= Null_Label then
                  Gen_Branch_Not_Equal (Label);
               end if;
               Label := Null_Label;
            end;
         else
            declare
               True_Label       : constant Label_Id := New_Label;
               Save_True_Label  : Label_Id := True_Label;
               Save_False_Label : Label_Id := Label;

            begin
               Evaluate_Expr (Left_Opnd (Expr), Save_True_Label, True);
               if Save_True_Label /= Null_Label then
                  Gen_Branch_Not_Equal (True_Label);
               end if;
               Evaluate_Expr
                 (Right_Opnd (Expr), Save_False_Label, False);
               if Save_False_Label /= Null_Label then
                  Gen_Branch_Equal (Label);
               end if;
               Gen_Label (True_Label);
               Label := Null_Label;
            end;
         end if;

      --  If no branch label is available, we have to produce a Boolean
      --  result (e.g., for a statement like B0 := B1 or else B2).

      else
         declare
            True_Label       : constant Label_Id := New_Label;
            False_Label      : constant Label_Id := New_Label;
            Save_True_Label  : Label_Id := True_Label;
            Check_State      : Boolean;

         begin
            Suppress_Stack_Checking (Check_State);

            Evaluate_Expr (Left_Opnd (Expr), Save_True_Label, True);
            if Save_True_Label /= Null_Label then
               Gen_Branch_Not_Equal (True_Label);
            end if;

            Save_True_Label := True_Label;
            Evaluate_Expr (Right_Opnd (Expr), Save_True_Label, True);
            if Save_True_Label /= Null_Label then
               Gen_Branch_Not_Equal (True_Label);
            end if;

            Gen_Push_Int (Uint_0);
            Gen_Goto (False_Label);

            Gen_Label (True_Label);
            Mark_Stack;
            Gen_Push_Int (Uint_1);
            Release_Stack;

            Gen_Label (False_Label);

            Restore_Stack_Checking (Check_State);
         end;
      end if;
   end Evaluate_Or_Else;

   ------------------------------
   -- Evaluate_Membership_Test --
   ------------------------------

   procedure Evaluate_Membership_Test
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean)
   is
      Test_Expr        : constant Node_Id   := Left_Opnd (Expr);
      Test_Type        : constant Entity_Id := Full_Type (Test_Expr);
      Member_Type      : constant Entity_Id := Full_Type (Right_Opnd (Expr));
      Test_Range       : Node_Id;

      Test_LV          : constant Local_Var_Id
        := New_Local_Var ("_test_tmp", JVM_Type (Test_Type));
      False_Label      : Label_Id;
      True_Label       : Label_Id;
      Low              : Node_Id;
      High             : Node_Id;
      Check_State      : Boolean;

   begin
      if Ekind (Test_Type) in Scalar_Kind then
         Test_Range := Right_Opnd (Expr);

         case Nkind (Right_Opnd (Expr)) is
            when N_Range =>
               Low  := Low_Bound (Test_Range);
               High := High_Bound (Test_Range);

            when N_Identifier | N_Expanded_Name =>
               Low  := Low_Bound (Scalar_Range (Entity (Test_Range)));
               High := High_Bound (Scalar_Range (Entity (Test_Range)));

            when N_Subtype_Indication =>
               Low  := Low_Bound (Scalar_Range (Full_Type (Test_Range)));
               High := High_Bound (Scalar_Range (Full_Type (Test_Range)));

            when others =>
               pragma Assert (False);
               raise Program_Error;
         end case;

         --  Evaluate and save away the test expression (but duplicate it
         --  so it's on the stack).

         Evaluate_Expr (Test_Expr);
         Gen_Duplicate;
         Gen_Store_Local (Test_LV);

         Suppress_Stack_Checking (Check_State);

         if Label /= Null_Label then

            --  Evaluate the low bound of the range and compare against
            --  the test expression.

            Evaluate_Expr (Low);
            if True_Branch then
               if Nkind (Expr) = N_In then
                  False_Label := New_Label;
                  Gen_Compare_Branch_Less (False_Label);
               else  --  Nkind (Expr) = N_Not_In
                  Gen_Compare_Branch_Less (Label);
               end if;
            else
               if Nkind (Expr) = N_In then
                  Gen_Compare_Branch_Less (Label);
               else  --  Nkind (Expr) = N_Not_In
                  True_Label := New_Label;
                  Gen_Compare_Branch_Less (True_Label);
               end if;
            end if;

            --  Evaluate the high bound of the range and compare against
            --  the test expression.

            Gen_Load_Local (Test_LV);
            Evaluate_Expr (High);
            if True_Branch then
               if Nkind (Expr) = N_In then
                  Gen_Compare_Branch_Less_Equal (Label);
                  Gen_Label (False_Label);
               else  --  Nkind (Expr) = N_Not_In
                  Gen_Compare_Branch_Greater (Label);
               end if;
            else
               if Nkind (Expr) = N_In then
                  Gen_Compare_Branch_Greater (Label);
               else  --  Nkind (Expr) = N_Not_In
                  Gen_Compare_Branch_Less_Equal (Label);
                  Gen_Label (True_Label);
               end if;
            end if;

            Label := Null_Label;

         --  Label = Null_Label, so compute a Boolean result on the stack

         else
            False_Label := New_Label;
            True_Label  := New_Label;

            --  Compare Test_Expr vs. Low

            Evaluate_Expr (Low);
            Gen_Compare_Branch_Less (False_Label);

            --  Compare Test_Expr vs. High

            Gen_Load_Local (Test_LV);
            Evaluate_Expr (High);
            Gen_Compare_Branch_Greater (False_Label);

            if Nkind (Expr) = N_In then
               Gen_Push_Int (Uint_1);
            else  --  Nkind (Expr) = N_Not_In
               Gen_Push_Int (Uint_0);
            end if;
            Gen_Goto (True_Label);

            Gen_Label (False_Label);
            Mark_Stack;
            if Nkind (Expr) = N_In then
               Gen_Push_Int (Uint_0);
            else  --  Nkind (Expr) = N_Not_In
               Gen_Push_Int (Uint_1);
            end if;
            Release_Stack;

            Gen_Label (True_Label);
         end if;

         Restore_Stack_Checking (Check_State);

      elsif Is_Tagged_Type (Test_Type) then
         if Is_Class_Wide_Type (Member_Type) then
            Evaluate_Expr (Test_Expr);
            Gen_Instance_Of (JVM_Class (Member_Type));

            if Label /= Null_Label then
               if True_Branch then
                  Gen_Branch_Not_Equal (Label);
               else
                  Gen_Branch_Equal (Label);
               end if;
            end if;

            Label := Null_Label;

         --  Generate a comparison of the class of the object against
         --  the class of the membership type.

         else
            --  First get the reference for the class associated with Test_Expr
            --  (but if the Test_Expr is statically tagged it's not always
            --  correct to call getClass, e.g., this won't work for the case
            --  of formal parameters ???).

            Evaluate_Expr (Test_Expr);
            Gen_Invoke_API_Method (Object_getClass);

            --  Then push the reference for the class of the membership type

            Gen_Push_String_Const
              (Name_String (Name (JVM_Class (Member_Type))));
            Gen_Invoke_API_Method (Class_forName);

            --  Now check if the class references are equal

            if Label /= Null_Label then
               if True_Branch then
                  Gen_Compare_Branch_Equal (Label);
               else
                  Gen_Compare_Branch_Not_Equal (Label);
               end if;

               Label := Null_Label;

            else
               False_Label := New_Label;
               True_Label  := New_Label;

               Suppress_Stack_Checking (Check_State);

               Gen_Compare_Branch_Equal (True_Label);
               Gen_Push_Int (Uint_0);
               Gen_Goto (False_Label);

               Gen_Label (True_Label);
               Mark_Stack;
               Gen_Push_Int (Uint_1);
               Release_Stack;

               Gen_Label (False_Label);

               Restore_Stack_Checking (Check_State);
            end if;
         end if;

      --  All other cases should be handled by the front end (statically
      --  or by expansion).

      else
         pragma Assert (False);
         raise Program_Error;
      end if;
   end Evaluate_Membership_Test;

   -----------------------------
   -- Evaluate_String_Literal --
   -----------------------------

   procedure Evaluate_String_Literal
     (Str_Lit  : String_Id;
      Str_Type : Entity_Id)
   is
      J_Str_Type : constant Type_Id := JVM_Type (Str_Type);
      String_LV  : constant Local_Var_Id
        := New_Local_Var ("_str_literal", J_Str_Type);
      Lit_Length : constant Uint
        := UI_From_Int (String_Length (Str_Lit));

   begin
      --  Allocate a temporary array object of the appropriate type

      Gen_Push_Int (UI_From_Int (String_Length (Str_Lit)));
      Gen_New_Array (J_Str_Type);
      Gen_Store_Local (String_LV);

      if Lit_Length > 0 then
         --  Generate a constant pool entry for the string and load
         --  its address (of type java.lang.String). The JVM String
         --  is then passed to java.lang.String.getBytes (or getChars)
         --  along with with the address of the Ada string temporary.

         Gen_Push_String_Const (New_String_Constant (Str_Lit));
         Gen_Push_Int (Uint_0);
         Gen_Push_Int (Lit_Length);
         Gen_Load_Local (String_LV);
         Gen_Push_Int (Uint_0);

         --  Call the appropriate API function in java.lang.String to
         --  transfer the characters from the string pool item into
         --  the array temporary.

         if Root_Type (Component_Type (Str_Type)) = Standard_Character then
            Gen_Invoke_API_Method (String_getBytes);

         elsif
           Root_Type (Component_Type (Str_Type)) = Standard_Wide_Character
         then
            Gen_Invoke_API_Method (String_getChars);

         else
            pragma Assert (False);
            raise Program_Error;
         end if;
      end if;

      --  Finally, load the reference to the string temporary

      Gen_Load_Local (String_LV);
   end Evaluate_String_Literal;

   ------------------------
   -- Evaluate_With_Copy --
   ------------------------

   procedure Evaluate_With_Copy (Expr : Node_Id) is
      Exp_Type     : constant Entity_Id := Underlying_Type (Etype (Expr));
      Sub_Expr     : Node_Id := Expr;
      Sub_Exp_Type : Entity_Id := Exp_Type;
      Is_Slice     : Boolean;
      Arr_Name     : Node_Id;
      Arr_Subt     : Entity_Id;

   begin
      if Nkind (Expr) = N_Qualified_Expression then
         Sub_Expr := Expression (Expr);
         Sub_Exp_Type := Full_Type (Sub_Expr);
      end if;

      case Ekind (Exp_Type) is
         when Elementary_Kind =>
            Evaluate_Expr (Expr);

         when Einfo.Record_Kind =>
            --  Extract the innermost subexpression and its type from
            --  within any containing qualifications or conversions.
            --  This is important for the case of applying a static
            --  deep copy for a tagged type (we want to invoke the
            --  deep copy associated with the type of the innermost
            --  object to preserve its tag in the case of initializing
            --  a class-wide object). This is still not right for the
            --  case of a formal parameter whose underlying object
            --  has a different tag (seems to require either knowing
            --  that the context is a class-wide object initalization
            --  or else always doing a clone, even when a dispatch
            --  is not needed). ???

            while Nkind (Sub_Expr) = N_Qualified_Expression
              or else Nkind (Sub_Expr) = N_Type_Conversion
            loop
               Sub_Expr := Expression (Sub_Expr);
               Sub_Exp_Type := Full_Type (Sub_Expr);
            end loop;

            --  If the expression is an aggregate or a function result,
            --  then simply evaluate without making a copy (which would
            --  be redundant).

            if Nkind (Sub_Expr) = N_Aggregate
              or else Nkind (Sub_Expr) = N_Function_Call
            then
               Evaluate_Expr (Expr);

            --  Otherwise the result of the expression needs to be copied.

            --  In the non-class-wide case we push a null target value,
            --  evaluate the expression, and generate a call to the type's
            --  deep copy operation.

            elsif not Is_Class_Wide_Type (Sub_Exp_Type) then
               Gen_Push_Null;
               Evaluate_Expr (Expr);
               Gen_Invoke_Deep_Copy (Sub_Exp_Type);

            --  If the result of the expression is class-wide, then
            --  evaluate the expression and generate a call to the type's
            --  deep clone operation. This is a dispatching call, which
            --  is necessary to handle cases involving class-wide object
            --  creation and initialization.

            else
               Evaluate_Expr (Expr);
               Gen_Invoke_Deep_Clone (Sub_Exp_Type);
            end if;

         when Einfo.Array_Kind =>
            if Nkind (Sub_Expr) = N_Aggregate
              or else Nkind (Sub_Expr) = N_Function_Call
            then
               Evaluate_Expr (Expr);

            --  If the value is of a one-dimensional scalar array type,
            --  then allocate a new array object for the result and
            --  copy in the value by calling java.lang.System.arrayCopy.

            elsif Number_Dimensions (Exp_Type) = 1
               and then Ekind (Full_Type (Component_Type (Exp_Type)))
                          in Elementary_Kind
               and then not
                 Has_Aliased_Components (Full_Type (Component_Type (Exp_Type)))
            then
               declare
                  Is_Slice : Boolean;
                  Arr_Name : Node_Id;
                  Arr_Subt : Entity_Id;
                  Arr_LV   : constant Local_Var_Id
                    := New_Local_Var ("_arr_tmp", JVM_Type (Exp_Type));

               begin
                  Evaluate_Array_Address (Expr);

                  Test_For_Slice (Expr, Is_Slice, Arr_Name, Arr_Subt);

                  --  Load the starting index of the evaluated array
                  --  followed by the array length

                  if Is_Slice then
                     Gen_Array_Subscript (Arr_Name, Index_First (Arr_Subt));
                     Load_Index_Length (First_Index (Arr_Subt));
                  else
                     Gen_Duplicate;
                     Gen_Array_Length;
                     Gen_Push_Int (Uint_0);
                     Gen_Swap;
                  end if;

                  --  Allocate the new object and save its reference
                  --  in a temporary.

                  Gen_New_Array (JVM_Type (Arr_Subt));
                  Gen_Duplicate;
                  Gen_Store_Local (Arr_LV);

                  --  Load the offset of the target array object (always 0)

                  Gen_Push_Int (Uint_0);

                  --  Generate the array length again and call the copy routine

                  Gen_Load_Local (Arr_LV);
                  Gen_Array_Length;
                  Gen_Invoke_API_Method (System_arraycopy);

                  --  Leave a reference to the new array object on the stack

                  Gen_Load_Local (Arr_LV);
               end;

            --  If the result of the expression needs to be copied, then
            --  push a null target value, evaluate the expression, and
            --  generate a call to the type's deep copy operation.

            else
               Gen_Push_Null;
               Gen_Push_Int (Uint_0);

               Evaluate_Array_Address (Expr);
               Test_For_Slice (Expr, Is_Slice, Arr_Name, Arr_Subt);

               --  Load the array length followed by the starting index
               --  of the evaluated array.

               if Is_Slice then
                  Gen_Array_Subscript (Arr_Name, Index_First (Arr_Subt));
                  Load_Index_Length (First_Index (Arr_Subt));
                  Gen_Swap;
               else
                  Gen_Duplicate;
                  Gen_Array_Length;
                  Gen_Push_Int (Uint_0);
               end if;

               Gen_Invoke_Deep_Copy (Full_Type (Exp_Type));
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Evaluate_With_Copy;

   ------------------------------
   -- Evaluate_Array_Aggregate --
   ------------------------------

   procedure Evaluate_Array_Aggregate (Aggr : Node_Id) is
      Aggr_Type   : constant Entity_Id    := Underlying_Type (Etype (Aggr));
      J_Type      : constant Type_Id      := JVM_Type (Aggr);
      Array_Index : Node_Id := First_Index (Aggr_Type);
      Dimensions  : Pos_8 := Dimensionality (J_Type);

      procedure Evaluate_Subaggregates (Aggr : Node_Id; Dimensions : Pos_8);
      --  Traverses the subaggregates of the remaining Dimensions number
      --  of dimensions of an array aggregate, generating code to compute
      --  the indexes of each element of the aggregate object and finally
      --  initializing each element from the expressions at the deepest level.
      --  Requires that a reference to an aggregate (or subaggregate) is
      --  on the top of stack.

      procedure Evaluate_Subaggregates (Aggr : Node_Id; Dimensions : Pos_8) is
         Aggr_Temp : constant Local_Var_Id := New_Local_Var ("_aggr", J_Type);
         Comp_Expr : Node_Id := First (Expressions (Aggr));
         Index     : Uint    := Uint_0;

      begin
         --  Save away the currently computed subarray reference

         Gen_Store_Local (Aggr_Temp);

         --  Loop through the list of subaggregates or element expressions

         while Present (Comp_Expr) loop

            --  If this is the deepest dimension of the aggregate, then
            --  generate the final index into the array and store the
            --  the result of evaluating the element expression.

            if Dimensions = 1 then
               Gen_Load_Local (Aggr_Temp);
               Gen_Push_Int (Index);

               Evaluate_With_Copy (Comp_Expr);

               Gen_Store_Array_Element;

            --  If Dimensions > 1, then we generate the index of a subarray
            --  and recurse.

            else
               Gen_Load_Local (Aggr_Temp);
               Gen_Push_Int (Index);
               Gen_Load_Subarray_Reference;

               Evaluate_Subaggregates (Comp_Expr, Dimensions - 1);
            end if;

            Index := Index + 1;

            Comp_Expr := Next (Comp_Expr);
         end loop;
      end Evaluate_Subaggregates;

   begin
      --  For now we don't support array aggregates with composite components,
      --  which requires traversing the array to allocate each component. ???

      if Dimensions = 1 then
         Load_Index_Length (Aggregate_Bounds (Aggr));
         Gen_New_Array (J_Type);
         Gen_Duplicate;

      else
         while Present (Array_Index) loop
            Load_Index_Length (Array_Index);
            Next_Index (Array_Index);
         end loop;

         Gen_New_Multiarray (J_Type);
         Gen_Duplicate;
      end if;

      Evaluate_Subaggregates (Aggr, Dimensions);
   end Evaluate_Array_Aggregate;

   -------------------------------
   -- Evaluate_Record_Aggregate --
   -------------------------------

   procedure Evaluate_Record_Aggregate
     (Aggr : Node_Id; Aggr_LV : Local_Var_Id := Null_Local_Var)
   is
      Comp_Assn  : Node_Id;
      Selector   : Entity_Id;
      Aggr_Local : Local_Var_Id     := Aggr_LV;
      J_Type     : constant Type_Id := JVM_Type (Aggr);

   begin
      if Aggr_Local = Null_Local_Var then
         Aggr_Local := New_Local_Var ("_aggr", J_Type);

         Gen_Default_Object (Class_Of_Type (J_Type));
         Gen_Store_Local (Aggr_Local);
      end if;

      Comp_Assn := First (Component_Associations (Aggr));

      while Present (Comp_Assn) loop
         Selector := Entity (First (Choices (Comp_Assn)));

         --  If the association is for the parent part of a tagged aggregate,
         --  then recursively evaluate the subaggregate into the same aggregate
         --  object.

         if Chars (Selector) = Name_uParent then
            Evaluate_Record_Aggregate (Expression (Comp_Assn), Aggr_Local);

         else
            --  Load the reference to the aggregate followed by the value of
            --  the component expression.

            Gen_Load_Local (Aggr_Local);

            --  If the component is aliased, then we have to create
            --  a wrapper object and initialize it from the expression,
            --  leaving a reference to the wrapper on the stack.

            if Has_Wrapper (Selector) then
               Gen_Default_Object (Class_Of_Type (Wrapper_Type (Selector)));
               Gen_Duplicate;
               Evaluate_Expr (Expression (Comp_Assn));
               Gen_Put_Object_Field (Wrapper_Field (Selector));

            else
               Evaluate_With_Copy (Expression (Comp_Assn));
            end if;

            --  Store the result of the component association into
            --  the associated field of the aggregate object.

            Gen_Put_Field (JVM_Field (Selector));
         end if;

         Comp_Assn := Next (Comp_Assn);
      end loop;

      --  If Aggr_LV is non-null then the caller must take care of loading
      --  the aggregate reference (e.g., for the case of the evaluation of
      --  a tagged parent aggregate, the evaluation of the enclosing aggregate
      --  will take care of it).

      if Aggr_LV = Null_Local_Var then
         Gen_Load_Local (Aggr_Local);
      end if;
   end Evaluate_Record_Aggregate;

   ------------------------
   -- Evaluate_Aggregate --
   ------------------------

   procedure Evaluate_Aggregate (Aggr : Node_Id) is
      Aggr_Type  : constant Entity_Id    := Underlying_Type (Etype (Aggr));

   begin
      if Ekind (Aggr_Type) in Einfo.Array_Kind then
         Evaluate_Array_Aggregate (Aggr);

      elsif Ekind (Aggr_Type) in Record_Kind then
         Evaluate_Record_Aggregate (Aggr);

      else
         pragma Assert (False);
         raise Program_Error;
      end if;
   end Evaluate_Aggregate;

   ------------------------
   -- Evaluate_Allocator --
   ------------------------

   procedure Evaluate_Allocator (Allocator : Node_Id) is
      Expr       : Node_Id   := Expression (Allocator);
      Acc_Type   : Entity_Id := Full_Type (Allocator);
      Desig_Subt : Entity_Id := Underlying_Type (Designated_Type (Acc_Type));
      Desig_Type : Entity_Id := Full_Type (Desig_Subt);
      Alloc_Subt : Entity_Id;
      Alloc_Type : Entity_Id;

   begin
      if Nkind (Expr) = N_Qualified_Expression then
         Alloc_Subt := Full_Subtype (Entity (Subtype_Mark (Expr)));

      elsif Nkind (Expr) = N_Subtype_Indication then
         Alloc_Subt := Full_Subtype (Entity (Subtype_Mark (Expr)));

      else
         Alloc_Subt := Full_Subtype (Entity (Expr));
      end if;
      Alloc_Type := Full_Type (Alloc_Subt);

      --  For now we disallow the allocation of tagged objects whose
      --  type has convention Java. This prevents problems with types
      --  which do not have no-arg constructors. Perhaps we should
      --  relax this restriction at some point, checking for the
      --  presence of a no-arg constructor for the type. Would it
      --  be better to simply change this to a warning for now and
      --  leave it up to the user to determine the safety rather
      --  than being overly restrictive ???

      if Is_Tagged_Type (Alloc_Type)
        and then Convention (Alloc_Type) = Convention_Java
      then
         Error_Msg_N
           ("allocator-created object of Java-convention tagged type" &
            " not allowed", Expr);
         Error_Msg_N
           ("must create object via call to Java constructor", Expr);
      end if;

      case Ekind (Desig_Type) is
         when E_Record_Type | E_Class_Wide_Type
            | E_Task_Type   | E_Protected_Type =>
            --  First case is an allocator with a type mark or subtype
            --  indication.

            if Nkind (Expr) /= N_Qualified_Expression then
               Gen_New_Object (JVM_Class (Alloc_Type));
               Gen_Duplicate;
               Gen_Invoke_Init (Alloc_Type);

            --  If the allocator is initialized with an aggregate, then
            --  just use the result of evaluating the aggregate directly
            --  as the allocator result.

            elsif Nkind (Expression (Expr)) = N_Aggregate then
               Evaluate_Expr (Expression (Expr));

            --  The allocator has a qualified expression and requires
            --  a full copy of the result of the expression. In the
            --  non-class-wide case we push a null target value, evaluate
            --  the expression, and generate a call to the type's deep copy
            --  operation.

            elsif not Is_Class_Wide_Type (Desig_Type) then
               Gen_Push_Null;
               Evaluate_Expr (Expr);
               Gen_Invoke_Deep_Copy (Desig_Type);

            --  Otherwise, for the class-wide case, we evaluate and pass
            --  the source expression to the record type's deep clone
            --  operation to create and initialize the new object. We
            --  use deep clone instead of deep copy here in order to
            --  handle the case of allocators for class-wide types,
            --  which necessitate a dispatch on the source object.

            else
               Evaluate_Expr (Expression (Expr));
               Gen_Invoke_Deep_Clone (Desig_Type);
            end if;

         when E_Array_Type | E_String_Type =>
            if Is_Constrained (Desig_Subt)
              or else Convention (Scope (Acc_Type)) = Convention_Java
            then
               if Nkind (Expr) = N_Qualified_Expression then
                  Evaluate_With_Copy (Expression (Expr));

               else
                  declare
                     Index     : Node_Id   := First_Index (Alloc_Subt);
                     Comp_Type : Entity_Id := Component_Type (Desig_Type);
                     Array_LV  : Local_Var_Id;

                  begin
                     if Number_Dimensions (Alloc_Type) = 1 then
                        Load_Index_Length (Index);
                        Gen_New_Array (JVM_Type (Alloc_Type));

                     --  Multidimensional array case

                     else
                        while Present (Index) loop
                           Load_Index_Length (Index);
                           Next_Index (Index);
                        end loop;

                        Gen_New_Multiarray (JVM_Type (Alloc_Type));
                     end if;

                     --  An array with composite or aliased components
                     --  requires a traversal of the array and allocation
                     --  of objects for all its components. For now this
                     --  is generated inline, but eventually we should
                     --  encapsulate it in a method associated with the
                     --  array type.

                     if Ekind (Full_Type (Comp_Type)) in Composite_Kind
                       or else Has_Aliased_Components (Desig_Type)
                     then
                        Array_LV
                          := New_Local_Var ("_arr_tmp", JVM_Type (Desig_Type));
                        Gen_Store_Local (Array_LV);
                        Allocate_Array_Components (Desig_Type, Array_LV);
                        Gen_Load_Local (Array_LV);
                     end if;
                  end;
               end if;

            --  Designated type is unconstrained so must allocate
            --  an array descriptor to contain the bounds.

            else
               declare
                  Arr_Ref_Class : constant Class_Id
                    := Class_Of_Type (JVM_Type (Acc_Type));
                  Alloc_LV      : constant Local_Var_Id
                    := New_Local_Var
                         ("_alloc_tmp", Type_Of (Arr_Ref_Class));
                  Dimensions    : Pos_8
                    := Pos_8 (Number_Dimensions (Alloc_Type));

               begin
                  Gen_Default_Object (Arr_Ref_Class);
                  Gen_Store_Local (Alloc_LV);

                  --  Allocate the array object (by evaluation in the case
                  --  of an qualified expression with an aggregate) and
                  --  save its reference and bounds in the appropriate
                  --  fields of the array descriptor.

                  Gen_Load_Local (Alloc_LV);

                  if Nkind (Expr) = N_Qualified_Expression then
                     --  Evaluate the array expression followed by loading
                     --  its bounds. This is needed for the case of an
                     --  expression that is a dereferenced access-to-
                     --  unconstrained array, because Load_Array_Bounds
                     --  requires the (undereferenced) access value on
                     --  the stack. It would be nice to clean this up
                     --  at some point, since it requires a pop and a
                     --  reevaluation of the array further below
                     --  (which can be incorrect, since there may
                     --  be side effects). ???

                     Evaluate_Expr (Expression (Expr));
                     Load_Array_Bounds (Expression (Expr));

                     --  Save the bounds in the new compound array
                     --  reference object. In the multidimensional
                     --  case save each pair of bounds, counting
                     --  down the dimensions in reverse.

                     for D in reverse 2 .. Dimensions loop
                        Gen_Load_Local (Alloc_LV);
                        Gen_Swap;
                        Gen_Put_Field
                          (Field (Arr_Ref_Class, "last_" & Image (D)));

                        Gen_Load_Local (Alloc_LV);
                        Gen_Swap;
                        Gen_Put_Field
                          (Field (Arr_Ref_Class, "first_" & Image (D)));
                     end loop;

                     Gen_Load_Local (Alloc_LV);
                     Gen_Swap;
                     Gen_Put_Field (Field (Arr_Ref_Class, "last"));

                     Gen_Load_Local (Alloc_LV);
                     Gen_Swap;
                     Gen_Put_Field (Field (Arr_Ref_Class, "first"));

                     --  Pop the array reference off the stack.

                     Gen_Pop;

                     --  Now reevaluate the array to make the new copy

                     Evaluate_With_Copy (Expression (Expr));

                     --  Save the new array reference in the 'all' field
                     --  of the new compound reference object.

                     Gen_Put_Field (Field (Arr_Ref_Class, "all"));

                  --  Case of allocator with an index constraint

                  else
                     declare
                        Index     : Node_Id   := First_Index (Alloc_Subt);
                        Comp_Type : Entity_Id := Component_Type (Desig_Type);
                        Array_LV  : Local_Var_Id;

                     begin
                        if Number_Dimensions (Alloc_Type) = 1 then
                           Load_Index_Length (Index);
                           Gen_New_Array (JVM_Type (Alloc_Type));

                        --  Multidimensional array case

                        else
                           while Present (Index) loop
                              Load_Index_Length (Index);
                              Next_Index (Index);
                           end loop;

                           Gen_New_Multiarray (JVM_Type (Alloc_Type));
                        end if;

                        Gen_Put_Field (Field (Arr_Ref_Class, "all"));

                        --  Save the bounds for each dimension in the
                        --  new compound array reference object.

                        Index := First_Index (Alloc_Subt);

                        Gen_Load_Local (Alloc_LV);
                        Evaluate_Expr (Array_Index_First (Index));
                        Gen_Put_Field (Field (Arr_Ref_Class, "first"));

                        Gen_Load_Local (Alloc_LV);
                        Evaluate_Expr (Array_Index_Last (Index));
                        Gen_Put_Field (Field (Arr_Ref_Class, "last"));

                        for D in 2 .. Dimensions loop
                           Next_Index (Index);

                           Gen_Load_Local (Alloc_LV);
                           Evaluate_Expr (Array_Index_First (Index));
                           Gen_Put_Field
                             (Field (Arr_Ref_Class, "first_" & Image (D)));

                           Gen_Load_Local (Alloc_LV);
                           Evaluate_Expr (Array_Index_Last (Index));
                           Gen_Put_Field
                             (Field (Arr_Ref_Class, "last_" & Image (D)));
                        end loop;

                        --  An array with composite or aliased components
                        --  requires a traversal of the array and allocation
                        --  of objects for all its components. For now this
                        --  is generated inline, but eventually we should
                        --  encapsulate it in a method associated with the
                        --  array type.

                        if Ekind (Full_Type (Comp_Type)) in Composite_Kind
                          or else Has_Aliased_Components (Desig_Type)
                        then
                           Array_LV
                             := New_Local_Var
                                  ("_arr_tmp", JVM_Type (Desig_Type));
                           Gen_Load_Local (Alloc_LV);
                           Gen_Get_Field (Field (Arr_Ref_Class, "all"));
                           Gen_Store_Local (Array_LV);
                           Allocate_Array_Components (Desig_Type, Array_LV);
                        end if;
                     end;
                  end if;

                  Gen_Load_Local (Alloc_LV);
               end;
            end if;

         --  If the designated type is a scalar or (non-subprogram) access
         --  type then a wrapper object must be allocated.

         when Wrappable_Kind =>

            Gen_Default_Object (Class_Of_Type (Wrapper_Type (Desig_Type)));

            if Nkind (Expr) = N_Qualified_Expression then
               Gen_Duplicate;
               Evaluate_Expr (Expression (Expr));
               Gen_Put_Field (Wrapper_Field (Desig_Type));
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Evaluate_Allocator;

   ------------------------------
   -- Evaluate_Integer_Literal --
   ------------------------------

   procedure Evaluate_Integer_Literal (Literal : Node_Id; Typ : Entity_Id) is
      J_Type : constant Type_Id := JVM_Type (Typ);

   begin
      if JVM.Type_Kind (J_Type) in Byte_Kind .. Int_Kind then

         --  If the integer value is greater than Integer'Last, then
         --  the literal must be of a modular type, so normalize the
         --  value to the equivalent signed (negative) value.

         if Intval (Literal) >= 2 ** (Uint_32 - 1) then
            pragma Assert (Ekind (Typ) in Modular_Integer_Kind);

            Gen_Push_Int (Intval (Literal) - (2 ** Uint_32));

         else
            Gen_Push_Int (Intval (Literal));
         end if;

      elsif J_Type = Long_Type then

         --  If the integer value is greater than Long_Integer'Last,
         --  then the literal must be of a modular type, so normalize
         --  the value to the equivalent signed (negative) value.

         if Intval (Literal) >= 2 ** (Uint_64 - 1) then
            pragma Assert (Ekind (Typ) in Modular_Integer_Kind);

            Gen_Push_Long (Intval (Literal) - (2 ** Uint_64));

         else
            Gen_Push_Long (Intval (Literal));
         end if;

      else
         pragma Assert (False);
         raise Program_Error;
      end if;
   end Evaluate_Integer_Literal;

   ---------------------------
   -- Evaluate_Real_Literal --
   ---------------------------

   procedure Evaluate_Real_Literal (Literal : Node_Id; Typ : Entity_Id) is
      J_Type : constant Type_Id := JVM_Type (Typ);

   begin
      if J_Type = Float_Type then
         Gen_Push_Float (Realval (Literal));

      elsif J_Type = Double_Type then
         Gen_Push_Double (Realval (Literal));

      --  The real literal is associated with a fixed-point type, in
      --  which case we push the literal's associated integer value.

      elsif J_Type = Int_Type then
         Gen_Push_Int (Corresponding_Integer_Value (Literal));

      elsif J_Type = Long_Type then
         Gen_Push_Long (Corresponding_Integer_Value (Literal));

      else
         pragma Assert (False);
         raise Program_Error;
      end if;
   end Evaluate_Real_Literal;

   -------------------
   -- Evaluate_Expr --
   -------------------

   procedure Evaluate_Expr (Expr : Node_Id) is
      No_Label : Label_Id := Null_Label;

   begin
      Evaluate_Expr (Expr, No_Label, False);
   end Evaluate_Expr;

   -------------------
   -- Evaluate_Expr --
   -------------------

   procedure Evaluate_Expr (Expr : Node_Id; Check_Subtype : Entity_Id) is
      No_Label : Label_Id := Null_Label;

   begin
      Evaluate_Expr (Expr, No_Label, False);

      if Do_Range_Check (Expr) then
         Gen_Scalar_Subtype_Check (Check_Subtype);
      end if;
   end Evaluate_Expr;

   -------------------
   -- Evaluate_Expr --
   -------------------

   procedure Evaluate_Expr
     (Expr        : Node_Id;
      Label       : in out Label_Id;
      True_Branch : Boolean)
   is
      Exp_Kind    : Node_Kind := Nkind (Expr);
      A_Type      : constant Entity_Id := Full_Type (Expr);
      J_Type      : constant Type_Id   := JVM_Expr_Type (Expr);
      Ada_Entity  : Entity_Id;
      Formal_LV   : Local_Var_Id;
      Formal_Type : Type_Id;
      AR_Fld      : Field_Id;

   begin
      Debug_A_Entry ("(Ada-to-JVM) ", Expr);

      case Exp_Kind is
         when N_Integer_Literal =>
            Evaluate_Integer_Literal (Expr, A_Type);

         when N_Real_Literal =>
            Evaluate_Real_Literal (Expr, A_Type);

         when N_Null =>

            Gen_Push_Null;

         when N_Identifier | N_Character_Literal | N_Expanded_Name =>
            Ada_Entity := Entity (Expr);

            if Exp_Kind = N_Character_Literal
              and then not Present (Ada_Entity)
            then
               Gen_Push_Int (UI_From_Int (Int (Char_Literal_Value (Expr))));

            --  If the entity denotes a static scalar constant whose value
            --  does not require a constant pool reference to load, then
            --  push the constant's corresponding literal value directly.
            --  Otherwise we load it from the field or local variable
            --  associated with the constant.

            elsif Compile_Time_Known_Value (Expr)
              and then Ekind (A_Type) in Scalar_Kind
              and then
                ((Ekind (A_Type) not in Einfo.Float_Kind
                   and then (J_Type = Type_Of (Java_Lang_Object)
                     or else
                       not Literal_Needs_Pool_Ref (J_Type, Expr_Value (Expr))))
                 or else (Ekind (A_Type) in Einfo.Float_Kind
                   and then
                     not Literal_Needs_Pool_Ref (J_Type, Expr_Value_R (Expr))))
            then
               --  The special case of System.Null_Address is caught
               --  here, and we load a null instead of the value zero.

               if J_Type = Type_Of (Java_Lang_Object) then
                  Gen_Push_Null;

               else
                  case JVM.Type_Kind (J_Type) is
                     when Boolean_Kind .. Int_Kind =>
                        Gen_Push_Int (Expr_Rep_Value (Expr));
                     when Long_Kind =>
                        Gen_Push_Long (Expr_Rep_Value (Expr));
                     when JVM.Float_Kind =>
                        Gen_Push_Float (Expr_Value_R (Expr));
                     when Double_Kind =>
                        Gen_Push_Double (Expr_Value_R (Expr));
                     when others =>
                        pragma Assert (False);
                        raise Program_Error;
                  end case;
               end if;

            else
               case Ekind (Ada_Entity) is
                  when E_Variable | E_Constant =>
                     if Present (Renamed_Object (Ada_Entity))
                       and then Ekind (A_Type) in Elementary_Kind
                     then
                        --  If this is a renaming of an elementary entity,
                        --  simply evaluate the renamed object and return.

                        if Is_Entity_Name (Renamed_Object (Ada_Entity)) then
                           Evaluate_Expr (Renamed_Object (Ada_Entity));
                           return;

                        --  If the renamed object is a selected component,
                        --  then continue, and let the following code take
                        --  care of loading the reference to the containing
                        --  object, which is the JVM entity associated with
                        --  the renaming declaration entity (see jx_ch8).
                        --  The loading of the elementary component field
                        --  itself will happen further below, after loading
                        --  the containing object's reference.

                        elsif Nkind (Renamed_Object (Ada_Entity))
                          = N_Selected_Component
                        then
                           null;

                        --  If this is a reference to a renamed function
                        --  call, then the renaming is simply an elementary
                        --  entity that contains the result, so we let the
                        --  processing continue as for a normal evaluation.

                        elsif Nkind (Renamed_Object (Ada_Entity))
                          = N_Function_Call
                        then
                           null;

                        --  Blow up in other cases (e.g., indexed components),
                        --  which aren't supported yet. ???

                        else
                           pragma Assert (False);
                           raise Program_Error;
                        end if;
                     end if;

                     if Is_Global_Entity (Ada_Entity)
                       or else Is_Imported (Ada_Entity)
                     then
                        Gen_Get_Static_Field (JVM_Field (Ada_Entity));

                     else
                        if Enclosing_Method (Ada_Entity) = Current_Method then
                           --  If the local variable has been allocated to
                           --  a field in the current method's AR object,
                           --  then load its value from that field if its
                           --  type is scalar or access (otherwise we can
                           --  get the value from the local variable since
                           --  it's effectively a constant).

                           if Access_From_Current_AR (Ada_Entity) then
                              Gen_Load_Local (AR_Stack.Top.AR_Obj);
                              Gen_Get_Field
                                (AR_Field (AR_Stack.Top, Ada_Entity));
                           else
                              Gen_Load_Local (JVM_Local_Var (Ada_Entity));
                           end if;

                        else
                           --  We have to load the local variable from
                           --  an up-level activation frame object.

                           Gen_Get_Field (Access_AR_Field (Ada_Entity));
                        end if;
                     end if;

                     --  If the object is a renaming of an elementary object,
                     --  then at this point it must be the renaming of a
                     --  a selected component (or function call, see above).
                     --  In this case the reference to the component's
                     --  containing object has been loaded, and so we now
                     --  load the component itself. Support for other cases,
                     --  such as renamings of indexed components, will be
                     --  added later. ???

                     if Present (Renamed_Object (Ada_Entity))
                       and then Ekind (A_Type) in Elementary_Kind
                       and then Nkind (Renamed_Object (Ada_Entity))
                                  /= N_Function_Call
                     then
                        pragma Assert (Nkind (Renamed_Object (Ada_Entity))
                                        = N_Selected_Component);

                        declare
                           Selector : constant Entity_Id
                             := Entity
                                 (Selector_Name (Renamed_Object (Ada_Entity)));
                           J_Field  : constant Field_Id
                             := JVM_Field (Selector);
                        begin
                           Gen_Get_Object_Field (J_Field);

                           --  Change the Ada_Entity to the selected component
                           --  to allow handling the case where it's wrapped.

                           Ada_Entity := Selector;
                        end;
                     end if;

                     --  If this is an aliased object with a wrapper type,
                     --  then load its 'all' field.

                     if Has_Wrapper (Ada_Entity) then
                        Gen_Get_Object_Field (Wrapper_Field (Ada_Entity));

                        --  The 'all' component for an access type wrapper
                        --  is of class Object, so a downcast to the formal
                        --  type is needed to satisfy consistency checking
                        --  of the operand stack.

                        if Ekind (Full_Type (Ada_Entity)) in Access_Kind then
                           Gen_Check_Cast
                             (Class_Of_Type
                               (JVM_Type (Full_Type (Ada_Entity))));
                        end if;
                     end if;

                  when Formal_Kind =>
                     if Enclosing_Method (Ada_Entity) = Current_Method then
                        Formal_LV   := JVM_Local_Var (Ada_Entity);
                        Formal_Type := Type_Of (Formal_LV);
                        Gen_Load_Local (Formal_LV);

                     else
                        --  We have to load the local variable from
                        --  an up-level activation frame object.

                        AR_Fld      := Access_AR_Field (Ada_Entity);
                        Formal_Type := Type_Of (AR_Fld);
                        Gen_Get_Field (AR_Fld);
                     end if;

                     if Has_Wrapper (Ada_Entity) then
                        Gen_Get_Field (Wrapper_Field (Ada_Entity));

                        --  The 'all' component for an access type wrapper
                        --  is of class Object, so a downcast to the formal
                        --  type is needed to satisfy consistency checking
                        --  of the operand stack.

                        if Ekind (Full_Type (Ada_Entity)) in Access_Kind then
                           Gen_Check_Cast
                             (Class_Of_Type
                               (JVM_Type (Full_Type (Ada_Entity))));
                        end if;

                     --  If the formal is a controlling parameter of a
                     --  dispatching operation, then its JVM type may have
                     --  been changed to some parent type (in the case
                     --  of an overriding operation). In that case we
                     --  have to cast the parameter value to the JVM
                     --  type associated with the formal's Ada type.

                     elsif Is_Controlling_Formal (Ada_Entity)
                       and then
                         JVM_Type (Find_Dispatching_Type (Scope (Ada_Entity)))
                           /= Type_Of (JVM_Local_Var (Ada_Entity))
                     then
                        Gen_Check_Cast
                          (Class_Of_Type (JVM_Type (Full_Type (Ada_Entity))));
                     end if;

                  --  For now always treat loop parameters as local variables,
                  --  although this is not fully general, since potentially
                  --  a loop parameter might be referenced as an up-level
                  --  variable from a subprogram declared inside the loop. ???

                  when E_Loop_Parameter =>
                     Gen_Load_Local (JVM_Local_Var (Ada_Entity));

                  when E_Discriminant =>
                     Gen_Load_Local (JVM_Local_Var (Discriminal (Ada_Entity)));

                  when E_Enumeration_Literal =>
                     Gen_Push_Int (Enumeration_Rep (Ada_Entity));

                  --  When evaluating an exception we want to push its
                  --  associated class, not an instance of the exception,
                  --  so we generate an instance and call getClass.
                  --  This should only occur for constructs such as
                  --  exc'Identity (which gets mapped to exc'Reference
                  --  by the front end). Is there an easier way to
                  --  generate the class that doesn't require creating
                  --  an exception instance ???

                  when E_Exception =>
                     Gen_Default_Object (JVM_Class (Ada_Entity));
                     Gen_Invoke_API_Method (Object_getClass);

                  when others =>
                     pragma Assert (False);
                     raise Program_Error;
               end case;
            end if;

         when N_Selected_Component =>
            declare
               Selector   : Entity_Id := Entity (Selector_Name (Expr));
               J_Field    : Field_Id;
               Desig_Type : Entity_Id;

            begin
               --  If the selector is an access discriminant designating
               --  a Java interface type, then treat the selection specially
               --  by simply evaluating the prefix and changing the type
               --  of the result to the interface type. This kind of selection
               --  is treated as a conversion of the prefix object to the
               --  Java interface type.

               if Ekind (Etype (Selector)) in Access_Kind then
                  Desig_Type := Directly_Designated_Type (Etype (Selector));

                  if Is_Tagged_Type (Desig_Type)
                    and then Is_Interface (JVM_Class (Full_Type (Desig_Type)))
                  then
                     Evaluate_Expr (Prefix (Expr));
                     Pop_Type;
                     Push_Type (JVM_Type (Desig_Type));

                     return;
                  end if;
               end if;

               Evaluate_Expr (Prefix (Expr));

               --  References to the _tag component of a tagged object are
               --  converted to calls to the API Object.getClass method,
               --  which returns a reference to the java.lang.Class object
               --  associated with the prefix object's JVM class.

               if Chars (Selector) = Name_uTag then
                  Gen_Invoke_API_Method (Object_getClass);

               --  If the selector is a _parent field, then we elide the
               --  field selection and only evaluate the prefix. This
               --  usage should only occur in contexts where passing the
               --  containing object is appropriate in any case (the
               --  selection of the _parent field is equivalent to a
               --  conversion to the parent type).

               elsif Chars (Selector) /= Name_uParent then
                  J_Field := JVM_Field (Selector);

                  Gen_Get_Object_Field (J_Field);

                  if Has_Wrapper (Selector) then
                     Gen_Get_Object_Field (Wrapper_Field (Selector));

                     --  The 'all' component for an access type wrapper is of
                     --  class Object, so a downcast to the class of the access
                     --  type is needed to satisfy consistency checking of the
                     --  operand stack.

                     if Ekind (A_Type) in Access_Kind then
                        Gen_Check_Cast (Class_Of_Type (JVM_Type (A_Type)));
                     end if;

                  --  If the selector is an access discriminant linked to
                  --  a parent discriminant, then we force the type (via
                  --  a checkcast) to be the selector's designated type
                  --  in the case where it has a different type than the
                  --  discriminant that it constrains. This ensures
                  --  compatibility with the type required by the context.

                  elsif Ekind (Selector) = E_Discriminant
                    and then Is_Access_Type (Etype (Selector))
                    and then Present (Corresponding_Discriminant (Selector))
                    and then Etype (Selector)
                              /= Etype (Corresponding_Discriminant (Selector))
                  then
                     Gen_Check_Cast (Class_Of_Type (JVM_Type (Desig_Type)));
                  end if;
               end if;
            end;

         when N_Indexed_Component =>
            Evaluate_Expr (Prefix (Expr));
            Gen_Array_Subscript (Prefix (Expr), First (Expressions (Expr)));
            Gen_Load_Array_Element;

            --  A wrapped scalar or access component requires loading
            --  the 'all' field from the wrapper object.

            if Has_Aliased_Components (Full_Type (Prefix (Expr)))
              and then Ekind (Full_Type (Expr)) in Wrappable_Kind
            then
               Gen_Get_Object_Field (Wrapper_Field (Full_Type (Expr)));

               --  The 'all' component for an access type wrapper is of class
               --  Object, so a downcast to the class of the access type is
               --  needed to satisfy consistency checking of the operand stack.

               if Ekind (A_Type) in Access_Kind then
                  Gen_Check_Cast (Class_Of_Type (JVM_Type (A_Type)));
               end if;
            end if;

         when N_Slice =>
            Evaluate_Expr (Prefix (Expr));

         when N_Explicit_Dereference =>
            Evaluate_Expr (Prefix (Expr));

            --  If this a dereference of an access value designating a
            --  scalar or access value then load the 'all' field from
            --  the wrapper object.

            if Ekind (A_Type) in Wrappable_Kind then
               Gen_Get_Field (Wrapper_Field (A_Type));

               --  The 'all' component for an access type wrapper is of class
               --  Object, so a downcast to the class of the access type is
               --  needed to satisfy consistency checking of the operand stack.

               if Ekind (A_Type) in Access_Kind then
                  Gen_Check_Cast (Class_Of_Type (JVM_Type (A_Type)));
               end if;
            end if;

         when N_Function_Call =>

            --  For a call to a Java-imported function returning an
            --  access-to-unconstrained array result, we need to
            --  construct a wrapper object for the array and its
            --  bounds (unless the access type itself is declared
            --  in a package with convention Java).

            if Is_Entity_Name (Name (Expr))
              and then Convention (Entity (Name (Expr))) = Convention_Java
              and then Ekind (Full_Type (Expr)) in Access_Kind
              and then Ekind (Designated_Type (Full_Type (Expr)))
                         in Einfo.Array_Kind
              and then not Is_Constrained (Designated_Type (Full_Type (Expr)))
              and then Convention (Scope (Full_Type (Expr))) /= Convention_Java
            then
               Evaluate_Unconstrained_Array_Ref (Expr, Etype (Expr));

            else
               Translate_Subprogram_Call (Expr);
            end if;

         when N_Procedure_Call_Statement =>
            Translate_Subprogram_Call (Expr);

         when N_Attribute_Reference =>
            Evaluate_Attribute (Expr);

         when N_Binary_Op | N_Unary_Op =>
            Evaluate_Operator (Expr, Label, True_Branch);

         when N_And_Then =>
            Evaluate_And_Then (Expr, Label, True_Branch);

         when N_Or_Else =>
            Evaluate_Or_Else (Expr, Label, True_Branch);

         when N_In | N_Not_In =>
            Evaluate_Membership_Test (Expr, Label, True_Branch);

         when N_Conditional_Expression =>
            declare
               Condition   : constant Node_Id  := First (Expressions (Expr));
               Then_Expr   : constant Node_Id  := Next (Condition);
               Else_Expr   : constant Node_Id  := Next (Then_Expr);
               False_Label : constant Label_Id := New_Label;
               Save_Label  : Label_Id          := False_Label;
               Exit_Label  : constant Label_Id := New_Label;
               Check_State : Boolean;

            begin
               Suppress_Stack_Checking (Check_State);

               Evaluate_Expr (Condition, Save_Label, False);

               --  If Save_Label was unused during the condition evaluation,
               --  then generate a branch to it now based on the Boolean
               --  top-of-stack value.

               if Save_Label /= Null_Label then
                  Gen_Branch_Equal (False_Label);
               end if;

               Evaluate_Expr (Then_Expr);
               Gen_Goto (Exit_Label);

               Gen_Label (False_Label);
               Mark_Stack;
               Evaluate_Expr (Else_Expr);
               Release_Stack;

               Gen_Label (Exit_Label);

               Restore_Stack_Checking (Check_State);
            end;

         when N_Qualified_Expression =>
            --  NOTE: Eventually need to perform subtype checks ???
            Evaluate_Expr (Expression (Expr));

         when N_Reference =>

            --  If this is an unconstrained array reference, then we
            --  have to construct a compound pointer object that contains
            --  both the array reference and its bounds (unless the
            --  access type comes from a Java-convention package).

            if Ekind (Designated_Type (Etype (Expr))) in Einfo.Array_Kind
              and then not Is_Constrained (Designated_Type (Etype (Expr)))
              and then Convention (Scope (Etype (Expr))) /= Convention_Java
            then
               Evaluate_Unconstrained_Array_Ref (Prefix (Expr), Etype (Expr));

            --  In all other cases simply evaluate the prefix

            else
               Evaluate_Expr (Prefix (Expr));
            end if;

         when N_String_Literal =>
            Evaluate_String_Literal (Strval (Expr), A_Type);

         when N_Type_Conversion =>
            if Ekind (A_Type) in Einfo.Float_Kind
              or else Ekind (A_Type) in Discrete_Kind
            then
               Evaluate_Expr (Expression (Expr));
               Check_For_Overflow (Expr);
               Gen_Conversion (JVM_Expr_Type (Expr));

               --  The range check must be performed after the conversion
               --  instead of being done by the expression evaluation because
               --  the conversion may involve a representation change and
               --  the value must be checked against the bounds of the
               --  target type.

               if Do_Range_Check (Expression (Expr)) then
                  Gen_Scalar_Subtype_Check (Etype (Expr));
               end if;

               if Ekind (A_Type) in Modular_Integer_Kind
                 and then Modulus (A_Type) < 2 ** Uint_32
               then
                  declare
                     Exc_Label    : constant Label_Id := New_Label;
                     No_Exc_Label : constant Label_Id := New_Label;
                     Check_State  : Boolean;

                  begin
                     Suppress_Stack_Checking (Check_State);

                     --  If the result of the conversion is outside the base
                     --  range of the modular type then raise Constraint_Error.

                     Gen_Duplicate;
                     Gen_Branch_Less (Exc_Label);
                     Gen_Duplicate;
                     Gen_Push_Int (Modulus (A_Type));
                     Gen_Compare_Branch_Less (No_Exc_Label);
                     Gen_Label (Exc_Label);
                     Gen_Default_Object (API_Class (Ada_Constraint_Error));
                     Gen_Exception_Throw;
                     Gen_Label (No_Exc_Label);

                     Restore_Stack_Checking (Check_State);
                  end;
               end if;

            else
               Evaluate_Expr (Expression (Expr));

               --  Casts should never be needed when the result is an array
               --  type, so skip the conversion, which avoids problems when
               --  the conversion argument is a dereference of an access-to-
               --  unconstrained array (since the stack result in that case
               --  is an access to a wrapped pointer and should not be
               --  converted since it will need to be dereferenced at a
               --  later point).

               if not Is_Array_Type (A_Type) then
                  Gen_Conversion (JVM_Expr_Type (Expr));
               end if;

               --  Are there any cases where additional actions are required,
               --  or does front end expansion take care of those ???
            end if;

         when N_Unchecked_Type_Conversion =>
            Evaluate_Expr (Expression (Expr));

            --  If the target is a class or array type, then we may have
            --  to apply a cast on downward conversions, even though the
            --  conversion is unchecked, in order to satisfy the Java verifier.

            if JVM.Type_Kind (JVM_Expr_Type (Expr)) = Class_Kind
              or else JVM.Type_Kind (JVM_Expr_Type (Expr)) = JVM.Array_Kind
            then
               Gen_Conversion (JVM_Expr_Type (Expr));

            --  Unchecked conversions from Float to Integer are generated
            --  by the front end in certain cases (comparisons with Integer
            --  zero from some reason, e.g., in calls to the intrinsic
            --  Is_Negative in s-imgrea.adb). Since the JVM does not support
            --  such conversions, for now we generate a normal numeric
            --  conversion. Not clear why these conversions are generated
            --  for such cases (comparison with zero). ???

            elsif JVM.Type_Kind (JVM_Expr_Type (Expr)) = Int_Kind
              and then JVM.Type_Kind (JVM_Expr_Type (Expression (Expr)))
                         = JVM.Float_Kind
            then
               Gen_Conversion (JVM_Expr_Type (Expr));

            else
               --  We have to explicitly change the top-of-stack type to
               --  match the target of the unchecked conversion.

               Pop_Type;
               Push_Type (JVM_Expr_Type (Expr));
            end if;

         when N_Aggregate | N_Extension_Aggregate =>
            Evaluate_Aggregate (Expr);

         when N_Allocator =>
            Evaluate_Allocator (Expr);

         when N_Raise_xxx_Error =>

            Translate_Predefined_Raise (Expr);

            --  Since this is an expression evaluation, the context will
            --  require a result on the stack, so we must push a dummy
            --  value compatible with the expected type to ensure stack
            --  consistency. This will only occur in cases of programs
            --  which have unconditional errors.

            case JVM.Type_Kind (JVM_Type (Expr)) is
               when Boolean_Kind | Byte_Kind | Char_Kind
                  | Short_Kind | Int_Kind =>
                  Gen_Push_Int (Uint_0);
               when Long_Kind =>
                  Gen_Push_Long (Uint_0);
               when JVM.Float_Kind =>
                  Gen_Push_Float (Ureal_0);
               when Double_Kind =>
                  Gen_Push_Double (Ureal_0);
               when JVM.Array_Kind | Class_Kind =>
                  Gen_Push_Null;
               when others =>
                  pragma Assert (False);
                  raise Program_Error;
            end case;

         when others =>
            if Debug_Flag_JJ then
               Osint.Fail
                 ("*** Unsupported expression node: ",
                  Node_Kind'Image (Nkind (Expr)));
            else
               pragma Assert (False);
               raise Program_Error;
            end if;
      end case;

      Debug_A_Exit ("(Ada-to-JVM) ", Expr, " (done)");
   end Evaluate_Expr;

   ----------------------------
   -- Evaluate_Array_Address --
   ----------------------------

   procedure Evaluate_Array_Address (Arr : Node_Id) is
      Arr_Subtype : constant Entity_Id := Underlying_Type (Etype (Arr));

   begin
      pragma Assert (Ekind (Arr_Subtype) in Einfo.Array_Kind);

      if Nkind (Arr) = N_Type_Conversion
        or else Nkind (Arr) = N_Qualified_Expression
      then
         Evaluate_Array_Address (Expression (Arr));

      else
         Evaluate_Expr (Arr);

         --  If this is a dereference of an access-to-unconstrained array
         --  value then dereference the descriptor object pointed to by
         --  the access value and load its 'all' field (unless the access
         --  type comes from a Java-Convention package).

         if Nkind (Arr) = N_Explicit_Dereference
           and then
             not Is_Constrained
                   (Underlying_Type
                     (Designated_Type (Full_Type (Prefix (Arr)))))
           and then
             Convention (Scope (Full_Type (Prefix (Arr)))) /= Convention_Java
         then
            Gen_Get_Field
              (Field (Class_Of_Type (JVM_Type (Etype (Prefix (Arr)))), "all"));
         end if;
      end if;
   end Evaluate_Array_Address;

   ----------------------------
   -- Store_Elementary_Value --
   ----------------------------

   procedure Store_Elementary_Value (Target : Address_Descriptor) is
   begin
      case Target.Addr_Kind is
         when No_Address =>
            pragma Assert (False);
            raise Program_Error;
         when Local_Address =>
            Gen_Store_Local (Target.Local_Var);
         when Field_Address =>
            Gen_Put_Field (Target.Field);
         when Indexed_Address =>
            Gen_Store_Array_Element;
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Store_Elementary_Value;

   -----------------------
   -- Load_Index_Length --
   -----------------------

   procedure Load_Index_Length
     (Index  : Node_Id;
      Obj_LV : Local_Var_Id := Null_Local_Var)
   is
      Low    : Node_Id;
      High   : Node_Id;
      Length : Uint;

      procedure Evaluate_Bound
        (Bound  : Node_Id;
         Obj_LV : Local_Var_Id := Null_Local_Var);
      --  Evaluates a bound expression. If the bound is a discriminant
      --  and Obj_LV is present, then load it will be loaded from the
      --  discriminant field of the denoted record object.

      procedure Evaluate_Bound
        (Bound  : Node_Id;
         Obj_LV : Local_Var_Id := Null_Local_Var)
      is
      begin
         --  If the bound is a discriminant and there is an object
         --  available, then load the discriminant from the object,
         --  unless this is within an init_proc, in which case we
         --  simply evaluate the discriminant directly (which will
         --  result in loading the value from the corresponding
         --  discriminal parameter). Note that the test for being
         --  within an init_proc is more complex than simply testing
         --  that the current method name is Name_uInit_Proc, since
         --  the name of the method may have been expanded to include
         --  the enclosing scope as a prefix.

         if Obj_LV /= Null_Local_Var
           and then Is_Entity_Name (Bound)
           and then Ekind (Entity (Bound)) = E_Discriminant
           and then
             JVM_Method (Base_Init_Proc (Scope (Entity (Bound))))
               /= Current_Method
         then
            Gen_Load_Local (Obj_LV);
            Gen_Get_Field (JVM_Field (Entity (Bound)));

         else
            Evaluate_Expr (Bound);
         end if;
      end Evaluate_Bound;

   --  Start of processing for Load_Index_Length

   begin
      case Nkind (Index) is
         when N_Defining_Identifier =>
            Load_Index_Length (Scalar_Range (Index), Obj_LV);

         when N_Identifier | N_Expanded_Name =>
            Load_Index_Length (Scalar_Range (Entity (Index)), Obj_LV);

         when N_Subtype_Indication =>
            Load_Index_Length (Scalar_Range (Etype (Index)), Obj_LV);

         when N_Range =>
            Low  := Low_Bound (Index);
            High := High_Bound (Index);

            --  When both bounds are static we compute the length
            --  at compile time and push the value.

            if Compile_Time_Known_Value (High)
              and then Compile_Time_Known_Value (Low)
            then
               Length := Expr_Value (High) - Expr_Value (Low) + 1;

               if Length < Uint_0 then
                  Length := Uint_0;
               end if;

               Gen_Push_Int (Length);

            --  Nonstatic bounds case...

            else
               declare
                  Lbl            : constant Label_Id := New_Label;
                  Check_State    : Boolean;

               begin
                  Evaluate_Bound (High, Obj_LV);

                  --  Compute the length of the array dimension.
                  --  Note that for one-based arrays we simply
                  --  use the high bound as is.

                  if Compile_Time_Known_Value (Low) then
                     if Expr_Value (Low) = Uint_0 then
                        Gen_Push_Int (Uint_1);
                        Gen_Add;

                     elsif Expr_Value (Low) /= Uint_1 then
                        Gen_Push_Int (Expr_Value (Low) - 1);
                        Gen_Sub;
                     end if;

                  else
                     Evaluate_Bound (Low, Obj_LV);
                     Gen_Sub;
                     Gen_Push_Int (Uint_1);
                     Gen_Add;
                  end if;

                  --  We have to account for the possibility of
                  --  array indexes that are "super-null" ranges
                  --  ('Last - 'First + 1 is negative), in which
                  --  case we still want to produce a zero length
                  --  result. For that case, the following generates
                  --  a conditional that will replace the computed
                  --  value with zero.

                  Suppress_Stack_Checking (Check_State);

                  Gen_Duplicate;
                  Gen_Branch_Greater_Equal (Lbl);
                  Gen_Pop;
                  Gen_Push_Int (Uint_0);
                  Gen_Label (Lbl);

                  Restore_Stack_Checking (Check_State);
               end;
            end if;
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Load_Index_Length;

   -----------------------
   -- Array_Index_First --
   -----------------------

   function Array_Index_First (Index : Node_Id) return Node_Id is
   begin
      case Nkind (Index) is
         when N_Range =>
            return Low_Bound (Index);
         when N_Identifier | N_Expanded_Name =>
            if Nkind (Scalar_Range (Entity (Index)))
              = N_Subtype_Indication
            then
               return Array_Index_First (Scalar_Range (Entity (Index)));
            else
               return Low_Bound (Scalar_Range (Entity (Index)));
            end if;
         when N_Subtype_Indication =>
            return Low_Bound (Scalar_Range (Etype (Index)));
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Array_Index_First;

   ----------------------
   -- Array_Index_Last --
   ----------------------

   function Array_Index_Last (Index : Node_Id) return Node_Id is
   begin
      case Nkind (Index) is
         when N_Range =>
            return High_Bound (Index);
         when N_Identifier | N_Expanded_Name =>
            if Nkind (Scalar_Range (Entity (Index)))
              = N_Subtype_Indication
            then
               return Array_Index_Last (Scalar_Range (Entity (Index)));
            else
               return High_Bound (Scalar_Range (Entity (Index)));
            end if;
         when N_Subtype_Indication =>
            return High_Bound (Scalar_Range (Etype (Index)));
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Array_Index_Last;

   -----------------
   -- Index_First --
   -----------------

   function Index_First (Array_Subtype : Entity_Id) return Node_Id is
      Index : constant Node_Id
        := First_Index (Underlying_Type (Array_Subtype));

   begin
      return Array_Index_First (Index);
   end Index_First;

   ----------------
   -- Index_Last --
   ----------------

   function Index_Last (Array_Subtype : Entity_Id) return Node_Id is
      Index : constant Node_Id
        := First_Index (Underlying_Type (Array_Subtype));

   begin
      return Array_Index_Last (Index);
   end Index_Last;

   -------------------------
   -- Gen_Array_Subscript --
   -------------------------

   procedure Gen_Array_Subscript
     (Prefix          : Node_Id;
      First_Subscript : Node_Id)
   is
      Arr_Subtype     : Entity_Id := Underlying_Type (Etype (Prefix));
      Subscript       : Node_Id := First_Subscript;
      Index_Range     : Node_Id;
      Low_Bound       : Node_Id;
      Parent_Method   : Method_Id;
      Formal_LV       : Local_Var_Id;
      Formal_First    : Local_Var_Id;
      Descriptor_Type : Type_Id;
      Arr_Descriptor  : Local_Var_Id;
      Acc_Prefix      : Boolean := False;
      Acc_Unconstr    : Boolean := False;
      Unconstrained   : Boolean := False;
      Unconstr_Init   : Boolean := False;
      Dimension       : Pos_8   := 1;
      Desig_Subtype   : Entity_Id;

   begin
      if Ekind (Arr_Subtype) in Access_Kind then
         Arr_Subtype
           := Full_Subtype (Designated_Type (Full_Type (Arr_Subtype)));
         Desig_Subtype := Arr_Subtype;
         Acc_Prefix  := True;

      elsif Nkind (Prefix) = N_Explicit_Dereference then
         Desig_Subtype :=
           Full_Subtype (Designated_Type (Full_Type (Sinfo.Prefix (Prefix))));
      end if;

      Index_Range := First_Index (Arr_Subtype);

      --  If this is a dereference of an access-to-unconstrained array type
      --  then save a copy of the descriptor address and select its 'all'
      --  field to get the actual array address.

      if (Acc_Prefix or else Nkind (Prefix) = N_Explicit_Dereference)
        and then not Is_Constrained (Desig_Subtype)
        and then Convention (Scope (Desig_Subtype)) /= Convention_Java
      then
         Acc_Unconstr := True;
         if Acc_Prefix then
            Descriptor_Type := JVM_Type (Prefix);
         else
            Descriptor_Type := JVM_Type (Etype (Sinfo.Prefix (Prefix)));
         end if;
         Arr_Descriptor  := New_Local_Var ("_arr_desc", Descriptor_Type);
         Gen_Duplicate;
         Gen_Store_Local (Arr_Descriptor);
         Gen_Get_Field   (Field (Class_Of_Type (Descriptor_Type), "all"));

      elsif not Acc_Prefix
        and then Is_Entity_Name (Prefix)
        and then Ekind (Entity (Prefix)) in Formal_Kind
        and then not Is_Constrained (Etype (Entity (Prefix)))
      then
         Unconstrained := True;

         if Chars (Entity (Prefix)) = Name_uInit then
            Unconstr_Init := True;

            --  If the formal parameter has the name "_init", then the
            --  Front End will not have created an actual subtype for it,
            --  so we have to special case the indexing by explicitly
            --  grabbing the 'First formals via successors of the "_init"
            --  parameter.

            Formal_LV := JVM_Local_Var (Entity (Prefix));
            Formal_First := Next_Local_Var (Formal_LV);
         else
            Index_Range
              := First_Index
                   (Underlying_Type (Actual_Subtype (Entity (Prefix))));
         end if;
      end if;

      while Present (Subscript) loop
         --  For an unconstrained _init parameter, the normalization of the
         --  subscript by the lower bound is performed by explicitly loading
         --  and subtracting the _init formal's lower bound parameter.

         if Unconstr_Init then
            Evaluate_Expr (Subscript);

            Parent_Method := Enclosing_Method (Entity (Prefix));
            if Parent_Method = Current_Method then
               Gen_Load_Local (Formal_First);
            else
               Register_Up_Level_Reference (Parent_Method, Formal_First);
               Load_Up_Level_Field (Parent_Method, Name (Formal_First));
            end if;

            Gen_Sub;

         elsif Acc_Unconstr then
            Evaluate_Expr (Subscript);

            --  Load the appropriate lower bound from the array descriptor
            --  and subtract from the subscript.

            Gen_Load_Local (Arr_Descriptor);
            if Dimension = 1 then
               Gen_Get_Field
                 (Field (Class_Of_Type (Descriptor_Type), "first"));
            else
               Gen_Get_Field
                 (Field (Class_Of_Type (Descriptor_Type),
                  "first_" & Image (Dimension)));
            end if;
            Gen_Sub;

         --  Normal indexing case; we recognize cases where the lower
         --  bound adjustment can be calculated statically

         else
            Low_Bound := Array_Index_First (Index_Range);

            if not Compile_Time_Known_Value (Low_Bound)
              or else Expr_Value (Low_Bound) /= Uint_0
            then
               if Compile_Time_Known_Value (Subscript)
                 and then Compile_Time_Known_Value (Low_Bound)
               then
                  Gen_Push_Int
                    (Expr_Value (Subscript) - Expr_Value (Low_Bound));

               --  We have to normalize the subscript by subtracting
               --  the lower bound at run time.

               else
                  Evaluate_Expr (Subscript);

                  --  If the lower bound is a bare discriminant, then
                  --  we have to load it from the object containing
                  --  the array. Unfortunately the reference to that
                  --  object is no longer around (due to selecting
                  --  the array from it), so we have to reevaluate
                  --  the record name prefix and select the discriminant.
                  --  This is not correct though in the case where
                  --  the record name evaluation has side effects.
                  --  Not clear how to cleanly handle this (perhaps
                  --  need to have caller pass in a local variable
                  --  containing the prefix, but that's very awkward!). ???

                  if Is_Entity_Name (Low_Bound)
                    and then Ekind (Entity (Low_Bound)) = E_Discriminant
                    and then Nkind (Prefix) = N_Selected_Component
                    and then
                      JVM_Method (Base_Init_Proc (Scope (Entity (Low_Bound))))
                        /= Current_Method
                  then
                     Evaluate_Expr (Sinfo.Prefix (Prefix));
                     Gen_Get_Field (JVM_Field (Entity (Low_Bound)));

                  else
                     Evaluate_Expr (Low_Bound);
                  end if;

                  Gen_Sub;
               end if;

            else
               Evaluate_Expr (Subscript);
            end if;

            Next_Index (Index_Range);
         end if;

         --  If the array is indexed by subscripts that are long values,
         --  then we have to convert to int, since the JVM only allows
         --  arrays to be indexed by ints. If the normalized subscript
         --  has a value outside of int then an exception will be raised.
         --  This is currently a capacity limitation of the JVM implementation.
         --  ???

         if JVM.Type_Kind (JVM_Type (Subscript)) = Long_Kind then
            Gen_Conversion (Int_Type);
         end if;

         --  If this is a slice indexing then the subscript isn't part
         --  of an expression list, so simply exit.

         if Is_List_Member (Subscript) then
            Subscript := Next (Subscript);
         else
            return;
         end if;

         --  If Subscript is not empty, then the preceding subscript is a
         --  subscript (not yet the last) for a multidimensional array and
         --  the reference to the next dimension's subarray must be loaded.

         if Present (Subscript) then
            Gen_Load_Subarray_Reference;

            --  For a multidimensional _init parameter we have to step to
            --  the next implicit bounds parameter (while first stepping
            --  past the upper bound parameter of the preceding dimension).

            if Unconstr_Init then
               Formal_First := Next_Local_Var (Next_Local_Var (Formal_First));
            end if;
         end if;

         Dimension := Dimension + 1;
      end loop;
   end Gen_Array_Subscript;

   ------------------------------
   -- Gen_Scalar_Subtype_Check --
   ------------------------------

   procedure Gen_Scalar_Subtype_Check (Scalar_Subt : Entity_Id) is
      Low_Bnd     : Node_Id;
      High_Bnd    : Node_Id;
      Raise_Label : Label_Id;
      OK_Label    : Label_Id;
      Check_State : Boolean;

   begin
      if Present (Scalar_Range (Scalar_Subt))
        and then Is_Constrained (Scalar_Subt)
      then
         Low_Bnd  := Low_Bound (Scalar_Range (Scalar_Subt));
         High_Bnd := High_Bound (Scalar_Range (Scalar_Subt));

         Raise_Label := New_Label;
         OK_Label    := New_Label;

         --  Duplicate the expression value in order to leave the expression
         --  on the stack after the check (as well as providing the value
         --  for the upper bound check).

         Gen_Duplicate;

         Suppress_Stack_Checking (Check_State);

         --  Evaluate the low bound and test against the expression. If the
         --  bound is a literal, then its type may be universal (e.g., if
         --  it occurs in the range of an integer_type_definition), in which
         --  case we have to force it to be evaluated with the proper type.

         if Nkind (Low_Bnd) = N_Integer_Literal then
            Evaluate_Integer_Literal (Low_Bnd, Scalar_Subt);
         elsif Nkind (Low_Bnd) = N_Real_Literal then
            Evaluate_Real_Literal (Low_Bnd, Scalar_Subt);
         else
            Evaluate_Expr (Low_Bnd);
         end if;

         Gen_Compare_Branch_Less (Raise_Label);

         --  If the subtype to check against is modular with
         --  an upper bound that is greater than 2**31 - 1 then
         --  we can't do a signed comparison (because it will
         --  compare against a negative signed value). It's
         --  not clear how to cleanly handle this case ???
         --  This suppression also isn't right for the 64-bit
         --  modular case, since the comparison should work
         --  fine for upper bounds up to 2**63 - 1. ???

         if Ekind (Scalar_Subt) in Modular_Integer_Kind
           and then Nkind (High_Bnd) = N_Integer_Literal
           and then Intval (High_Bnd) >= 2 ** (Uint_32 - 1)
         then
            Gen_Goto (OK_Label);

         --  Evaluate the high bound and test against the expression. If the
         --  bound is a literal, then its type may be universal (e.g., if
         --  it occurs in the range of an integer_type_definition), in which
         --  case we have to force it to be evaluated with the proper type.

         else
            Gen_Duplicate;

            if Nkind (High_Bnd) = N_Integer_Literal then
               Evaluate_Integer_Literal (High_Bnd, Scalar_Subt);
            elsif Nkind (High_Bnd) = N_Real_Literal then
               Evaluate_Real_Literal (High_Bnd, Scalar_Subt);
            else
               Evaluate_Expr (High_Bnd);
            end if;

            Gen_Compare_Branch_Less_Equal (OK_Label);
         end if;

         --  Generate a raise of Constraint_Error


         Gen_Label (Raise_Label);
         Gen_Default_Object (API_Class (Ada_Constraint_Error));
         Gen_Exception_Throw;

         Gen_Label (OK_Label);

         Restore_Stack_Checking (Check_State);
      end if;
   end Gen_Scalar_Subtype_Check;

   --------------------
   -- Test_For_Slice --
   --------------------

   procedure Test_For_Slice
     (N     : Node_Id;
      Slice : out Boolean;
      Prfix : out Node_Id;
      Subt  : out Entity_Id)
   is
   begin
      if Nkind (N) = N_Slice then
         Slice := True;
         Prfix := Prefix (N);
         Subt  := Etype (N);

      elsif Nkind (N) = N_Type_Conversion
        or else Nkind (N) = N_Unchecked_Type_Conversion
        or else Nkind (N) = N_Qualified_Expression
      then
         Test_For_Slice (Expression (N), Slice, Prfix, Subt);

      else
         Slice := False;
         Prfix := N;
         Subt  := Etype (N);
      end if;
   end Test_For_Slice;

   -----------------------
   -- Load_Array_Bounds --
   -----------------------

   procedure Load_Array_Bounds (Arr_Expr : Node_Id) is
      Descr_Class  : Class_Id;
      Index_Range  : Node_Id;
      Array_Temp   : Local_Var_Id;
      Dimensions   : Pos_8;
      Arr_Entity   : Entity_Id;

   begin
      --  If the expression denotes an array descriptor, then
      --  load the array's reference and bounds from the
      --  descriptor.

      if Nkind (Arr_Expr) = N_Explicit_Dereference
        and then not Is_Constrained (Full_Subtype (Arr_Expr))
      then
         --  If this a dereference of an access-to-unconstrained
         --  array value for a type from a Java-Convention package,
         --  then use zero for the lower bound and (length - 1)
         --  for the upper bound (no wrapper object is available
         --  in this case).

         if Convention (Scope (Full_Type (Prefix (Arr_Expr))))
           = Convention_Java
         then
            pragma Assert (Number_Dimensions (Full_Type (Arr_Expr)) = 1);

            Gen_Duplicate;
            Gen_Array_Length;

            --  Treat access-to-string as a special case and use
            --  bounds of 1..array'length. Perhaps we should handle
            --  the general case of arbitrary lower bounds (taken
            --  from the lower bound of the array index type), but
            --  seems like overkill for array types declared in
            --  Java-imported packages (especially since at present
            --  jvm2ada always uses Natural for the index type).

            if Full_Type (Arr_Expr) = Standard_String then
               Gen_Push_Int (Uint_1);

            else
               Gen_Push_Int (Uint_1);
               Gen_Sub;
               Gen_Push_Int (Uint_0);
            end if;

            Gen_Swap;

            return;
         end if;

         Descr_Class
           := Class_Of_Type (JVM_Type (Full_Type (Prefix (Arr_Expr))));
         Array_Temp
           := New_Local_Var ("_arr_tmp", Type_Of (Descr_Class));

         Gen_Duplicate;
         Gen_Store_Local (Array_Temp);

         Gen_Get_Field (Field (Descr_Class, "all"));

         Gen_Load_Local (Array_Temp);
         Gen_Get_Field (Field (Descr_Class, "first"));

         Gen_Load_Local (Array_Temp);
         Gen_Get_Field (Field (Descr_Class, "last"));

         --  Load the bounds for each dimension of a multidimensional
         --  array.

         Dimensions := Pos_8 (Number_Dimensions (Full_Type (Arr_Expr)));
         for D in 2 .. Dimensions loop
            Gen_Load_Local (Array_Temp);
            Gen_Get_Field (Field (Descr_Class, "first_" & Image (D)));
            Gen_Load_Local (Array_Temp);
            Gen_Get_Field (Field (Descr_Class, "last_" & Image (D)));
         end loop;

      elsif Is_Entity_Name (Arr_Expr)
        and then not Is_Constrained (Full_Subtype (Entity (Arr_Expr)))
      then
         Arr_Entity := Entity (Arr_Expr);

         --  In the case of a renamed entity, recursively load
         --  the bounds of the renamed entity

         if Present (Renamed_Object (Arr_Entity))
           and then Is_Entity_Name (Renamed_Object (Arr_Entity))
         then
            Load_Array_Bounds (Renamed_Object (Arr_Entity));

         else
            --  The bounds of an array that is an unconstrained
            --  formal are obtained from the actual subtype.

            if Ekind (Arr_Entity) in Formal_Kind then
               Index_Range := First_Index (Actual_Subtype (Arr_Entity));
            else
               Index_Range := First_Index (Full_Subtype (Arr_Entity));
            end if;

            --  Load the low and high bound for each dimension

            while Present (Index_Range) loop
               Evaluate_Expr (Array_Index_First (Index_Range));
               Evaluate_Expr (Array_Index_Last  (Index_Range));
               Next_Index (Index_Range);
            end loop;
         end if;

      elsif Ekind (Full_Subtype (Arr_Expr)) = E_String_Literal_Subtype then
         declare
            String_Low : constant Node_Id
              := String_Literal_Low_Bound (Full_Subtype (Arr_Expr));
         begin
            Evaluate_Expr (String_Low);
            if Compile_Time_Known_Value (String_Low) then
               Gen_Push_Int (Expr_Value (String_Low)
                 + String_Literal_Length (Full_Subtype (Arr_Expr)) - 1);
            else
               Gen_Duplicate;
               Gen_Push_Int
                 (String_Literal_Length (Full_Subtype (Arr_Expr)) - 1);
               Gen_Add;
            end if;
         end;

      --  If the actual is a call to a function with an unconstrained
      --  array result, then for now we just assume a lower bound of
      --  of one and an upper bound given by the length of the result.
      --  This will handle unconstrained string function results well
      --  enough for the moment. (What we plan to do is to pass the
      --  bounds as out parameters.) ???

      elsif Nkind (Arr_Expr) = N_Function_Call
        and then not Is_Constrained (Full_Subtype (Entity (Name (Arr_Expr))))
      then
         if Number_Dimensions (Full_Type (Arr_Expr)) = 1 then
            Gen_Duplicate;
            Gen_Array_Length;
            Gen_Push_Int (Uint_1);
            Gen_Swap;

         --  For a multidimensional array function result, we traverse
         --  the array, indexing each dimension by 0,  and load the bounds
         --  according to the length of the subarrays (but that's not
         --  really correct as noted above, plus it won't work properly
         --  for the case of null arrays). ???

         else
            Array_Temp
              := New_Local_Var ("_arr_tmp", JVM_Type (Full_Type (Arr_Expr)));
            Gen_Duplicate;
            Gen_Store_Local (Array_Temp);

            for D in reverse 1 .. Number_Dimensions (Full_Type (Arr_Expr)) loop
               Gen_Push_Int (Uint_1);
               Gen_Load_Local (Array_Temp);
               Gen_Array_Length;
               if D > 1 then
                  Gen_Load_Local (Array_Temp);
                  Gen_Push_Int (Uint_0);
                  Gen_Load_Subarray_Reference;
                  Gen_Store_Local (Array_Temp);
               end if;
            end loop;
         end if;

      --  If the expression is a conversion with an unconstrained target
      --  type, then we need to load the bounds of the conversion argument.

      elsif Nkind (Arr_Expr) = N_Type_Conversion
        and then not Is_Constrained (Full_Subtype (Arr_Expr))
      then
         Load_Array_Bounds (Expression (Arr_Expr));

      else
         Index_Range := First_Index (Full_Subtype (Arr_Expr));

         --  Load the low and high bound for each dimension

         while Present (Index_Range) loop
            Evaluate_Expr (Array_Index_First (Index_Range));
            Evaluate_Expr (Array_Index_Last  (Index_Range));
            Next_Index (Index_Range);
         end loop;
      end if;
   end Load_Array_Bounds;

end Jx_Ch4;
