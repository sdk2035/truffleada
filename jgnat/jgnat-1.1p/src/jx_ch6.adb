------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 6                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.48 $
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

with Atree;     use Atree;
with Einfo;     use Einfo;
with Errout;    use Errout;
with JVM;       use JVM;
with JVM.API;   use JVM.API;
with JVM.Map;   use JVM.Map;
with Jx_Ch3;    use Jx_Ch3;
with Jx_Ch4;    use Jx_Ch4;
with Jx_Ch5;    use Jx_Ch5;
with Jx_Ch11;   use Jx_Ch11;
with J_String;  use J_String;
with Jx_Decl;   use Jx_Decl;
with Jx_Uplev;  use Jx_Uplev;
with Namet;     use Namet;
with Nlists;    use Nlists;
with Opt;       use Opt;
with Sem_Disp;  use Sem_Disp;
with Sem_Util;  use Sem_Util;
with Sinfo;     use Sinfo;
with Snames;    use Snames;
with Stand;     use Stand;
with Types;     use Types;
with Uintp;     use Uintp;

package body Jx_Ch6 is

   -------------------------------
   -- Generate_Subprogram_Class --
   -------------------------------

   procedure Generate_Subprogram_Class (Comp_Unit : Node_Id) is
      Comp_Spec    : Node_Id   := Library_Unit (Comp_Unit);
      Subp         : Node_Id   := Unit (Comp_Unit);
      Subp_Entity  : Entity_Id := Defining_Entity (Subp);
      Subp_Class   : Class_Id;
      Binder_Class : Class_Id;
      Elab_Method  : Method_Id;
      Main_Method  : Method_Id;
      Main_Args    : Local_Var_Id;
      Args_Type    : Type_Id;
      Other_Class  : Class_Id;

   begin
      if Nkind (Subp) = N_Subprogram_Body then
         if Acts_As_Spec (Subp) then
            Subp_Entity := Defining_Entity (Specification (Subp));
         else
            Subp_Entity := Corresponding_Spec (Subp);
         end if;
      end if;

      --  The call to Associated_Class will have the side effect
      --  of creating the subprogram's JVM class entity.

      Subp_Class := Associated_Class (Subp_Entity);

      Class_Stack.Push (Subp_Class);
      Begin_Class_File (Subp_Class);
      Current_Compilation_Class := Current_Class;

      Generate_Class_Init_Method (Subp_Class);
      Generate_Default_Constructor (Subp_Class);

      --  Generate the elaboration method for the subprogram spec if needed

      if Present (Comp_Spec) then
         Elab_Method
           := New_Method (Subp_Class, Name ("_elabs"), Void_Type, True);
         Open_Method (Elab_Method);
         Set_Current_Method (Elab_Method);
         Method_Stack.Push (Elab_Method);

         if Present (Aux_Decls_Node (Comp_Spec)) then
            Translate_Declarations (Declarations (Aux_Decls_Node (Comp_Spec)));
            Translate_Statements   (Actions (Aux_Decls_Node (Comp_Spec)));
         end if;

         Gen_Method_Return;
         Method_Stack.Pop;
         Close_Method (Elab_Method);
      end if;

      --  Generate the elaboration method for the subprogram body

      Elab_Method
        := New_Method (Subp_Class, Name ("_elabb"), Void_Type, True);
      Open_Method (Elab_Method);
      Set_Current_Method (Elab_Method);
      Method_Stack.Push (Elab_Method);

      if Present (Aux_Decls_Node (Comp_Unit)) then
         Translate_Declarations (Declarations (Aux_Decls_Node (Comp_Unit)));
         Translate_Statements   (Actions (Aux_Decls_Node (Comp_Unit)));
      end if;

      Gen_Method_Return;
      Method_Stack.Pop;
      Close_Method (Elab_Method);

      --  Declare and generate code for the library subprogram

      Declare_Method (Subp_Class, Subp_Entity);
      Generate_Method (Subp);

      --  If the subprogram is a parameterless procedure, then create
      --  a main method with the appropriate args parameter that contains
      --  calls to adainit, the Ada main subprogram, and adafinal.

      if Ekind (Subp_Entity) = E_Procedure
        and then not Present (First_Formal (Subp_Entity))
      then
         Args_Type := New_Array_Type (Type_Of (API_Class (Lang_String)));
         Main_Method
           := New_Method
                (Subp_Class, Name ("main"), Void_Type, Static => True);
         Main_Args
           := New_Method_Parameter (Main_Method, Name ("args"), Args_Type);

         Open_Method (Main_Method);
         Set_Current_Method (Main_Method);
         Method_Stack.Push (Main_Method);

         --  Save the command line arguments reference in GNAT_libc.gnat_argv

         Gen_Load_Local (Main_Args);
         Gen_Put_Static_Field (API_Field (Gnat_Argv));

         --  Save the main program name in GNAT_libc.command_name

         Gen_Push_String_Const
           (New_String_Constant (Str_Id (Name_String (Chars (Subp_Entity)))));
         Gen_Put_Static_Field (API_Field (Command_Name));

         --  Generate call to adainit

         Get_Name_String (Chars (Subp_Entity));
         Binder_Class
           := New_Class (Name ("ada_" & Name_Buffer (1 .. Name_Len)));
         Gen_Invoke_Static
           (New_Method (Binder_Class, Name ("adainit"),
            Void_Type, Static => True));

         --  Generate call to the main subprogram

         Gen_Invoke_Static (JVM_Method (Subp_Entity));

         Gen_Invoke_Static
           (New_Method (Binder_Class, Name ("adafinal"),
            Void_Type, Static => True));
         Gen_Method_Return;

         Method_Stack.Pop;
         Close_Method (Main_Method);
      end if;

      pragma Assert (Method_Stack.Empty);

      --  Close any pending classes that may have been generated

      while Class_Stack.Top /= Subp_Class loop
         Other_Class := Class_Stack.Top;
         End_Class_File (Other_Class);
         Class_Stack.Pop;
      end loop;

      pragma Assert (Class_Stack.Top = Subp_Class);

      End_Class_File (Subp_Class);
      Class_Stack.Pop;
   end Generate_Subprogram_Class;

   ---------------------
   -- Generate_Method --
   ---------------------

   procedure Generate_Method (Subp_Body : Node_Id) is
      Subp_Entity      : Entity_Id;
      Subp_Method      : Method_Id;
      First_Param      : Entity_Id;
      This_LV          : Local_Var_Id;
      Formal_LV        : Local_Var_Id;
      Has_ND_Method    : Boolean := False;
      Empty_Init_Proc  : Boolean := False;
      Disp_Method      : Method_Id := Null_Method;

   begin
      if Acts_As_Spec (Subp_Body) then
         Subp_Entity := Defining_Entity (Specification (Subp_Body));
      else
         Subp_Entity := Corresponding_Spec (Subp_Body);
      end if;

      if Ekind (Subp_Entity) in Generic_Unit_Kind then
         return;
      end if;

      --  We should never generate a method body for a subprogram
      --  associated with a dispatching operation of a type declared
      --  as a Java interface. Note that nonabstract subprograms of
      --  such types can occur for cases such as the _Size operation
      --  (created by the front end for 'Size). Perhaps such operations
      --  should be reassociated with the containing package's class ???

      if Is_Dispatching_Operation (Subp_Entity)
        and then Is_Interface (JVM_Class (Find_Dispatching_Type (Subp_Entity)))
      then
         return;
      end if;

      Subp_Method := JVM_Entity (Subp_Entity);

      --  If the subprogram is dispatching and nonabstract, then we need
      --  to generate the body for its associated nondispatching method.
      --  This method is suppressed, however, in the case where the tagged
      --  type's scope has convention Java since the corresponding Java class
      --  has no such operation.

      if Has_Nondispatching_Method (Subp_Entity) then
         Has_ND_Method := True;
         Disp_Method := Next_Method (Subp_Method);
      end if;

      Open_Method (Subp_Method);
      Set_Current_Method (Subp_Method);
      Method_Stack.Push (Subp_Method);

      --  If this is an _init_proc, then perform allocation of any composite
      --  components. Note that at present we don't handle array _init_procs
      --  here, though in general we do need to perform allocations of
      --  composite components for arrays. ???

      if Chars (Subp_Entity) = Name_uInit_Proc then
         First_Param := First_Formal (Subp_Entity);
         This_LV := JVM_Entity (First_Param);

         --  If this is an init_proc for a type with convention Java,
         --  then we want to skip all code generation inside the body of
         --  of the init_proc. This avoids the problem of translating
         --  code that tries to call an init_proc for a parent type that
         --  is a Java API, as well as preventing initialization of any
         --  fictional discriminants of the type (no field is actually
         --  declared for interface 'self' discriminants or discriminants
         --  of types implementing Java interfaces). For now at least
         --  we want to simply disallow any stand-alone objects of
         --  types mapped to Java, and users should always initialize
         --  objects of such types by calling constructor functions.
         --  This is admittedly a kludge, and it would be nice to find
         --  a cleaner way to handle this or suppress the init_proc. ???

         if Is_Tagged_Type (Full_Type (First_Param))
           and then Convention (Full_Type (First_Param)) = Convention_Java
         then
            Empty_Init_Proc := True;

         else
            Allocate_Composite_Components (Etype (First_Param), This_LV);
         end if;
      end if;

      --  Generate a return in the case of an empty init_proc, to
      --  prevent JVM from generating a raise of Program_Error.
      --  (If we decide to allow stand-alone Java-convention tagged
      --  objects then we don't want an exception raised when the
      --  init_proc is called.)

      if Empty_Init_Proc then
         Gen_Method_Return;

      else
         Translate_Declarations (Declarations (Subp_Body));
         Translate_Handled_Statements (Handled_Statement_Sequence (Subp_Body));

         if not AR_Stack.Empty
           and then AR_Stack.Top.Method = Subp_Method
         then
            End_Activation_Record (Subp_Method);
         end if;
      end if;

      pragma Assert (Method_Stack.Top = Subp_Method);
      Method_Stack.Pop;
      Close_Method (Subp_Method);

      --  If the subprogram is a nonabstract dispatching operation, then
      --  generate the associated dispatching version of the method.
      --  This method has perforce already been declared and is obtained
      --  as the successor of the nondispatching method. The nondispatching
      --  method body contains all of the subprogram's code and is invoked
      --  by the dispatching method. (Note that we used to generate the
      --  code in the dispatching method and have the nondispatching method
      --  call the dispatching method via an Invokespecial, but that turned
      --  out not to work on certain JVMs because of an ambiguity in the
      --  definition of Invokespecial semantics, though it worked on the
      --  Sun JVM.)

      --  If the subprogram has an associated nondispatching method,
      --  then we simply generate a call to the nondispatching method,
      --  passing along all of the parameters.

      if Has_ND_Method then
         Open_Method (Disp_Method);
         Set_Current_Method (Disp_Method);
         Method_Stack.Push (Disp_Method);

         Formal_LV := First_Local_Var (Disp_Method);

         while Formal_LV /= Null_Local_Var loop
            Gen_Load_Local (Formal_LV);
            Formal_LV := Next_Local_Var (Formal_LV);
         end loop;

         Gen_Invoke_Static (Subp_Method);
         Gen_Method_Return;

         pragma Assert (Method_Stack.Top = Disp_Method);
         Method_Stack.Pop;
         Close_Method (Disp_Method);
      end if;

      if not Method_Stack.Empty then
         Set_Current_Method (Method_Stack.Top);
      end if;
   end Generate_Method;

   -------------------------------
   -- Translate_Subprogram_Call --
   -------------------------------

   procedure Translate_Subprogram_Call (Call : Node_Id) is
      Subp_Name    : constant Node_Id := Name (Call);
      Formal       : Entity_Id;
      Formal_Type  : Entity_Id;
      Actual       : Node_Id;
      Subp         : Entity_Id;
      Subp_Method  : Method_Id;
      Is_Slice     : Boolean;
      Slice_Prefix : Node_Id;
      Slice_Subt   : Entity_Id;
      Actual_Addr  : Address_Descriptor;
      Jtype        : Type_Id;
      Control_Arg  : Node_Id;
      Control_Type : Entity_Id;
      Cntrl_Formal : Entity_Id := Empty;
      Constr_Class : Class_Id;
      Access_Call  : Boolean   := False;
      Temp_Count   : Integer   := 0;
      Copy_Temps   : array (1 .. 10) of Local_Var_Id;
      Addr_Kinds   : array (1 .. 10) of Address_Kind;
      Ref_Temps    : array (1 .. 10) of Local_Var_Id;
      Field_Ids    : array (1 .. 10) of Field_Id;
      Index_Temps  : array (1 .. 10) of Local_Var_Id;

   begin
      case Nkind (Subp_Name) is
         when N_Identifier | N_Expanded_Name | N_Operator_Symbol =>
            Subp   := Entity (Subp_Name);
            Subp_Method := JVM_Method (Subp);

         when N_Explicit_Dereference =>
            --  if not Comes_From_Source (Call) then
            --     return;  --  ??? Ignore indirect run-time calls for now ???
            --  end if;

            Access_Call := True;
            Evaluate_Expr (Prefix (Subp_Name));
            Subp := Directly_Designated_Type (Etype (Prefix (Subp_Name)));
            Subp_Method
              := Method (JVM_Class (Etype (Prefix (Subp_Name))), "Invoke");

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      --  Calls to intrinsic assembly operations require specialized treatment

      if Convention (Subp) = Convention_Assembler
        or else Convention (Subp) = Convention_Intrinsic
      then
         --  Various calls such as to addressing operations (e.g., To_Pointer,
         --  address arithmetic) are not supported in general and will be
         --  caught by this warning check.

         if not Is_Imported (Subp)
           or else not Present (Interface_Name (Subp))
         then
            Error_Msg_N
              ("intrinsic subprogram call not supported?", Call);

         else
            declare
               Java_Name : constant String
                 := Str (Strval (Interface_Name (Subp)));
            begin
               --  The code for supporting the special "+" conversion
               --  operations on Java Strings is implemented here but
               --  not used yet, due to some front end issues that need
               --  to be resolved that currently prevent the operations
               --  from being declared as intrinsic. ???

               if Java_Name = "+" then

                  --  This is the special "+" operation for converting
                  --  from an Ada String to a java.lang.String. Generate
                  --  a call to the String constructor that takes a byte
                  --  array and 'hibyte' parameter (zero).

                  if Etype (First_Formal (Subp)) = Standard_String
                    and then Ekind (Etype (Subp)) in Access_Kind
                  then
                     --  This doesn't currently handle slices properly,
                     --  since slices need to be copied first ???

                     Gen_New_Object (API_Class (Lang_String));
                     Gen_Duplicate;
                     Evaluate_Expr (First_Actual (Call));
                     Gen_Push_Int (Uint_0);
                     Gen_Invoke_API_Method (String_Ascii_Init);

                  --  This is the special "+" operation for converting
                  --  from a java.lang.String to an Ada String. Generate
                  --  a call to the getBytes method to retrieve the
                  --  Ada String value from a Java String.

                  elsif Etype (Subp) = Standard_String
                    and then Ekind (Etype (First_Formal (Subp))) in Access_Kind
                  then
                     declare
                        String_Tmp : Local_Var_Id
                          := New_Local_Var
                               ("_str_tmp", JVM_Type (Standard_String));
                     begin
                        Evaluate_Expr (First_Actual (Call));
                        Gen_Duplicate;
                        Gen_Invoke_API_Method (String_length);
                        Gen_New_Array (JVM_Type (Standard_String));
                        Gen_Store_Local (String_Tmp);

                        Gen_Push_Int (Uint_0);
                        Gen_Load_Local (String_Tmp);
                        Gen_Array_Length;
                        Gen_Load_Local (String_Tmp);
                        Gen_Push_Int (Uint_0);
                        Gen_Invoke_API_Method (String_getBytes);
                     end;

                  else
                     pragma Assert (False);
                     raise Program_Error;
                  end if;

               elsif Java_Name = "current_target_exception" then
                  Gen_Load_Current_Exception;

               elsif Java_Name = "monitorenter" then
                  Evaluate_Expr (First_Actual (Call));
                  Gen_Monitor_Enter;

               elsif Java_Name = "monitorexit" then
                  Evaluate_Expr (First_Actual (Call));
                  Gen_Monitor_Exit;

               elsif Java_Name = "throw" then
                  Evaluate_Expr (First_Actual (Call));
                  Gen_Exception_Throw;

               else
                  pragma Assert (False);
                  raise Program_Error;
               end if;

               --  Processing is complete

               return;
            end;
         end if;
      end if;

      --  If the subprogram is marked as a constructor then generate a new
      --  object of the constructor's class and push it on the stack.

      if Is_Constructor (Subp) then
         --  For now we only support the case of function constructors

         pragma Assert (Ekind (Subp) = E_Function);

         Constr_Class
           := Class_Of_Type
                (JVM_Type (Directly_Designated_Type (Etype (Subp))));

         --  Search for a formal named 'this'

         Cntrl_Formal := First_Formal (Subp);
         Actual := First_Actual (Call);

         while Present (Cntrl_Formal)
           and then Name_String (Chars (Cntrl_Formal)) /= "this"
         loop
            Actual := Next_Actual (Actual);
            Next_Formal (Cntrl_Formal);
         end loop;

         if Present (Cntrl_Formal) then

            --  If the 'this' formal's corresponding actual is a null literal,
            --  then a new object is created and duplicated so that a reference
            --  to it will remain on the stack after the constructor call.

            if Nkind (Actual) = N_Null then
               Gen_New_Object (Constr_Class);
               Gen_Duplicate;

            --  If the actual for the 'this' formal is not a null literal,
            --  then evaluate the actual and duplicate it. This occurs for
            --  cases of calls to a constructor's parent type constructor.

            else
               Evaluate_Expr (Actual);
               Gen_Duplicate;
            end if;

         --  If the function has no formal named 'this', then we generate
         --  and duplicate the new object.

         else
            Gen_New_Object (Constr_Class);
            Gen_Duplicate;
         end if;

      --  If this is a call to a dispatching operation, then an
      --  appropriate value of the dispatching type must be pushed
      --  to control the dispatch (this value will correspond to
      --  the method's 'this' argument).  If the call is dispatching,
      --  then the first argument must be the value of the first
      --  controlling argument of the call (the reevaluation of
      --  that actual is suppressed when encountering it below
      --  within the main loop over actuals).

      elsif Is_Dispatching_Operation (Subp) then
         Control_Type := Full_Type (Find_Dispatching_Type (Subp));

         Cntrl_Formal := First_Formal (Subp);
         Actual := First_Actual (Call);

         while Present (Cntrl_Formal)
           and then not Is_Controlling_Formal (Cntrl_Formal)
         loop
            Actual := Next_Actual (Actual);
            Next_Formal (Cntrl_Formal);
         end loop;

         if Present (Cntrl_Formal) then
            Evaluate_Expr (Actual);

         --  If the subprogram has no controlling formal, then this must
         --  be a call to a function with controlling result, in which
         --  case we still have to pass a controlling actual for the
         --  'this' argument, obtained from the Controlling_Argument
         --  of the call. If the function call has no controlling
         --  argument, then it must be a nondispatching call, and
         --  in that case we have to construct a dummy object of the
         --  type to pass as an actual (the type is guaranteed to be
         --  nonabstract, so this will always work).

         else
            Control_Arg  := Controlling_Argument (Call);
            if Present (Control_Arg) then
               Evaluate_Expr (Control_Arg);
            else
               Gen_Default_Object (JVM_Class (Control_Type));
            end if;
         end if;
      end if;

      Formal := First_Formal (Subp);
      Actual := First_Actual (Call);

      while Present (Actual) loop

         Formal_Type := Full_Type (Etype (Formal));

         --  Slices are passed by copy to greatly simplify handling their
         --  bounds. A temporary is created and the sliced portion is
         --  assigned. In the case of out and in out parameters, we have
         --  to copy back after the call.

         Test_For_Slice (Actual, Is_Slice, Slice_Prefix, Slice_Subt);

         if Is_Slice then
            declare
               Slice_Temp : constant Local_Var_Id
                 := New_Local_Var ("_slicetmp", JVM_Type (Actual));
               Array_Temp : Local_Var_Id;
               Index_Temp : Local_Var_Id;
               Copy_In    : Boolean;

            begin
               --  If not an in parameter, then save the location of the
               --  temporary as well as the array reference and index value
               --  so we can copy back into the actual slice after the call.

               if Ekind (Formal) /= E_In_Parameter then
                  Temp_Count := Temp_Count + 1;

                  Array_Temp
                    := New_Local_Var ("_arraytmp", JVM_Type (Actual));
                  Index_Temp
                    := New_Local_Var ("_indextmp", Int_Type);

                  Copy_Temps  (Temp_Count) := Slice_Temp;
                  Ref_Temps   (Temp_Count) := Array_Temp;
                  Index_Temps (Temp_Count) := Index_Temp;
               end if;

               Load_Index_Length (First_Index (Slice_Subt));
               Gen_New_Array (JVM_Type (Actual));
               Gen_Store_Local (Slice_Temp);

               --  The actual array slice must be copied into the slice
               --  temporary unless the formal is an out mode parameter
               --  and the array has scalar components. (Out parameters
               --  with nonscalar components may have defaults in which
               --  case Ada requires copy in, plus arrays of composite
               --  components contain references that always need to be
               --  copied across.)

               Copy_In :=
                 Ekind (Component_Type (Slice_Subt)) not in Scalar_Kind
                   or else Ekind (Formal) /= E_Out_Parameter;

               --  Evaluate the slice, and save the result in a temporary
               --  if the parameter will require copy back.

               Evaluate_Expr (Actual);
               if Ekind (Formal) /= E_In_Parameter then
                  --  If the slice must be copied in then we want to leave
                  --  the array reference on the stack as a parameter to
                  --  the call to System.arraycopy.

                  if Copy_In then
                     Gen_Duplicate;
                  end if;
                  Gen_Store_Local (Array_Temp);
               end if;

               --  Evaluate the slice subscript, and save the result in
               --  a temporary if the parameter will require copy back.

               Gen_Array_Subscript (Slice_Prefix, Index_First (Slice_Subt));
               if Ekind (Formal) /= E_In_Parameter then
                  --  If the slice must be copied in then we want to leave
                  --  the subscript value on the stack as a parameter to
                  --  the call to System.arraycopy.

                  if Copy_In then
                     Gen_Duplicate;
                  end if;
                  Gen_Store_Local (Index_Temp);
               end if;

               --  Now copy the slice into the temporary

               if Copy_In then
                  Gen_Load_Local (Slice_Temp);
                  Gen_Push_Int (Uint_0);
                  Gen_Load_Local (Slice_Temp);
                  Gen_Array_Length;
                  Gen_Invoke_API_Method (System_arraycopy);
               end if;

               --  Finally, pass the reference to the slice temporary

               Gen_Load_Local (Slice_Temp);
            end;

         --  Elementary formal of mode out require passing the actual
         --  by copy. We create an object of the appropriate wrapper
         --  type and pass a reference to the wrapper, copying the
         --  actual value in for mode in out (and for mode out access
         --  parameters), and copying back from the wrapper object
         --  to the actual for both out and in out. In general the
         --  address of the actual has to be save in a temporary and
         --  not reevaluated on the copy back.

         elsif Ekind (Formal) /= E_In_Parameter
           and then Ekind (Formal_Type) in Elementary_Kind
         then
            Temp_Count := Temp_Count + 1;

            --  Note: We can't apply JVM_Type to Formal in this case
            --  because the JVM type associated with Formal_Type
            --  is a scalar type (e.g., int) rather than the wrapper
            --  type, so instead we have to get the wrapper type from
            --  Formal's associated local variable.

            Jtype := Type_Of (JVM_Local_Var (Formal));

            --  Create the wrapper object for the formal parameter
            --  and duplicate the address so that it's on the stack
            --  for the call.

            Copy_Temps (Temp_Count) := New_Local_Var ("_out_tmp", Jtype);
            Gen_Default_Object (Class_Of_Type (Jtype));
            Gen_Store_Local (Copy_Temps (Temp_Count));

            Actual_Addr := Evaluate_Addr (Actual);

            Addr_Kinds (Temp_Count) := Actual_Addr.Addr_Kind;

            --  For each kind of actual, save the result of any prefix
            --  or index calculation, and store the value of the actual
            --  into the wrapper object if needed (i.e., if the mode
            --  is 'in out' or if the formal is of an access type).

            case Actual_Addr.Addr_Kind is
               when Local_Address =>
                  if Ekind (Formal) = E_In_Out_Parameter
                    or else Ekind (Formal_Type) in Access_Kind
                  then
                     Gen_Load_Local (Copy_Temps (Temp_Count));
                     Gen_Load_Local (Actual_Addr.Local_Var);
                     Gen_Put_Field  (Wrapper_Field (Jtype));
                  end if;

               when Field_Address =>
                  --  If the computed actual address does not have immunity
                  --  to possible side effects, then save it in a temp to
                  --  prevent reevaluation on copy back. (Indexed components
                  --  and dereferences can show up as field addresses when
                  --  the actual denotes a wrapped object.)

                  if Nkind (Actual) = N_Indexed_Component
                    or else
                      ((Nkind (Actual) = N_Selected_Component
                         or else Nkind (Actual) = N_Explicit_Dereference)
                           and then Nkind (Prefix (Actual)) /= N_Identifier)
                  then
                     Ref_Temps (Temp_Count) :=
                       New_Local_Var ("_ref_tmp", Top_Type);
                     Gen_Store_Local (Ref_Temps (Temp_Count));
                  else
                     Ref_Temps (Temp_Count) := Null_Local_Var;
                  end if;

                  Field_Ids (Temp_Count) := Actual_Addr.Field;

                  if Ekind (Formal) = E_In_Out_Parameter
                    or else Ekind (Formal_Type) in Access_Kind
                  then
                     Gen_Load_Local (Copy_Temps (Temp_Count));

                     if Ref_Temps (Temp_Count) /= Null_Local_Var then
                        Gen_Load_Local (Ref_Temps (Temp_Count));

                     --  If a reference to the actual wasn't saved in a
                     --  temporary then we need to swap the addresses
                     --  before loading the actual value. (Also, if the
                     --  actual is a static field (i.e., global variable),
                     --  then it was not loaded by Evaluate_Addr.)

                     elsif not Is_Static (Actual_Addr.Field) then
                        Gen_Swap;
                     end if;

                     Gen_Get_Field (Actual_Addr.Field);
                     Gen_Put_Field (Wrapper_Field (Jtype));

                  --  If the formal has mode out and the actual address
                  --  was not saved in a reference temporary, then we need
                  --  to pop off the address pushed earlier by Evaluate_Addr.
                  --  Would be nice to avoid this inefficiency, but it's
                  --  it's not clear how to cleanly avoid the earlier address
                  --  evaluation. ???

                  elsif Ekind (Formal) = E_Out_Parameter
                    and then Ref_Temps (Temp_Count) = Null_Local_Var
                    and then not Is_Static (Actual_Addr.Field)
                  then
                     Gen_Pop;
                  end if;

               when Indexed_Address =>
                  Ref_Temps (Temp_Count)
                    := New_Local_Var ("_ref_tmp", JVM_Type (Prefix (Actual)));
                  Index_Temps (Temp_Count)
                    := New_Local_Var
                         ("_index_tmp",
                          JVM_Type (Full_Type (First (Expressions (Actual)))));
                  Gen_Store_Local (Index_Temps (Temp_Count));
                  Gen_Store_Local (Ref_Temps (Temp_Count));
                  if Ekind (Formal) = E_In_Out_Parameter
                    or else Ekind (Formal_Type) in Access_Kind
                  then
                     Gen_Load_Local (Copy_Temps (Temp_Count));
                     Gen_Load_Local (Ref_Temps (Temp_Count));
                     Gen_Load_Local (Index_Temps (Temp_Count));
                     Gen_Load_Array_Element;
                     Gen_Put_Field  (Wrapper_Field (Jtype));
                  end if;

               when others =>
                  pragma Assert (False);
                  raise Program_Error;
            end case;

            Gen_Load_Local (Copy_Temps (Temp_Count));

         --  If this is an access-to-unconstrained-array parameter for an
         --  imported subprogram with convention Java, then dereference
         --  the "all" field of the actual's compound pointer so as to pass
         --  just the reference to the array without passing the bounds
         --  (unless the access type comes from a Java-convention unit,
         --  in which case it's already a simple array reference).

         elsif Convention (Subp) = Convention_Java
           and then Ekind (Full_Type (Actual)) in Access_Kind
           and then Convention (Scope (Full_Type (Actual))) /= Convention_Java
           and then Ekind (Designated_Type (Full_Type (Actual)))
                      in Einfo.Array_Kind
           and then not Is_Constrained (Designated_Type (Full_Type (Actual)))
         then
            --  When the actual is an access attribute this is a bit
            --  inefficient since there's no need to construct the
            --  compound pointer in that case. We should eventually
            --  add special treatment for that case. ???

            Evaluate_Expr (Actual);
            Gen_Get_Field
              (Field (Class_Of_Type (JVM_Type (Full_Type (Actual))), "all"));

         --  Evaluate the actual, unless Cntrl_Formal was set earlier
         --  and matches the current Formal, in which case the actual
         --  was evaluated earlier as the 'this' argument to control
         --  a dispatching call.

         elsif not Present (Cntrl_Formal)
            or else Formal /= Cntrl_Formal
         then
            Evaluate_Expr (Actual, Check_Subtype => Etype (Formal));
         end if;

         --  If the formal subtype is an unconstrained array subtype,
         --  then pass the bounds of the actual as the subsequent
         --  parameters, except when the subprogram is imported.

         if Ekind (Formal_Type) in Einfo.Array_Kind
           and then not Is_Constrained (Full_Subtype (Formal))
           and then not Is_Imported (Subp)
         then
            Load_Array_Bounds (Actual);
         end if;

         Formal := Next_Formal_With_Extras (Formal);
         Actual := Next_Actual (Actual);
      end loop;

      --  If this is a call to a nested method, then pass a reference to
      --  the activation record associated with the method's static parent
      --  as the static link parameter for the called method.

      if not Access_Call
        and then Present (Enclosing_Subprogram (Subp))
        and then not Is_Imported (Subp)
      then
         Load_Static_Link (Enclosing_Method (Subp));
      end if;

      if not Is_Dispatching_Operation (Subp) then
         Gen_Invoke_Method (Subp_Method);

      else

         --  If this is a dispatching call, and the subprogram has
         --  an associated nondispatching method, then generate a
         --  call to the successor of Subp_Method (which by convention
         --  must be the subprogram's dispatching method). Otherwise
         --  simply generate a call to Subp_Method, which will normally
         --  be the nondispatching method of the subprogram, except for
         --  the case where the subprogram is declared in a Java-convention
         --  scope so that it has no nondispatching method (so that case
         --  will result in a dispatching call).

         if Present (Controlling_Argument (Call)) then
            if Has_Nondispatching_Method (Subp) then
               Gen_Invoke_Method (Next_Method (Subp_Method));
            else
               Gen_Invoke_Method (Subp_Method);
            end if;

         --  If the called subprogram is declared in a Java-convention
         --  scope and we're inside a method of a class belonging to
         --  an immediate descendant of the called subprogram's controlling
         --  class, then generate an invokespecial to make a nondispatching
         --  call to the superclass's method. Otherwise we have no choice
         --  but to make a dispatching call to the method, even though
         --  that doesn't match Ada nondispatching semantics. ???

         elsif Convention (Scope (Subp)) = Convention_Java then
            --  Call to a superclass method

            if Superclass (Class_Of (Current_Method))
                 = Class_Of (Subp_Method)
            then
               Gen_Invoke_Special (Subp_Method);

            --  Call to a higher parent, so warn user about nonstandard
            --  call semantics

            elsif Is_Parent_Class
                 (Class_Of (Subp_Method), Child => Class_Of (Current_Method))
            then
               if not GNAT_Mode then
                  Error_Msg_N ("call will invoke superclass method?", Call);
               end if;
               Gen_Invoke_Special (Subp_Method);

            --  Allow the call, which will be a virtual call, even though
            --  the Ada call should be nondispatching, but give a warning.

            else
               if not GNAT_Mode then
                  Error_Msg_N ("call to Java method will dispatch?", Call);
               end if;
               Gen_Invoke_Method (Subp_Method);
            end if;

         --  Normal nondispatching call case

         else
            Gen_Invoke_Method (Subp_Method);
         end if;
      end if;

      --  If this is a call to a dispatching function with a controlling
      --  controlling result, then the result JVM type may actually be
      --  one of the tagged type's parent types (in the case where the
      --  function is overriding), and so we may need to cast the result
      --  to the JVM class associated with function's Ada result type
      --  to avoid type mismatches with the expected type context.

      if Is_Dispatching_Operation (Subp)
        and then Ekind (Subp) = E_Function
        and then Has_Controlling_Result (Subp)
      then
         Gen_Conversion (JVM_Type (Etype (Subp)));
      end if;

      --  Out and in out mode parameters must be copied back into the
      --  elementary and array slice actuals after return from the call.

      if Temp_Count > 0 then
         Temp_Count := 0;

         Formal := First_Formal (Subp);
         Actual := First_Actual (Call);

         while Present (Actual) loop
            if Ekind (Formal) /= E_In_Parameter then
               Test_For_Slice (Actual, Is_Slice, Slice_Prefix, Slice_Subt);

               if Is_Slice then
                  Temp_Count := Temp_Count + 1;
                  Gen_Load_Local (Copy_Temps (Temp_Count));
                  Gen_Push_Int (Uint_0);

                  Gen_Load_Local (Ref_Temps   (Temp_Count));
                  Gen_Load_Local (Index_Temps (Temp_Count));

                  Gen_Load_Local (Copy_Temps (Temp_Count));
                  Gen_Array_Length;

                  Gen_Invoke_API_Method (System_arraycopy);

               elsif Ekind (Full_Type (Formal)) in Elementary_Kind then
                  Temp_Count := Temp_Count + 1;
                  Jtype := Type_Of (JVM_Local_Var (Formal));

                  case Addr_Kinds (Temp_Count) is
                     when Local_Address =>
                        Actual_Addr := Evaluate_Addr (Actual);
                        Gen_Load_Local (Copy_Temps (Temp_Count));
                        Gen_Get_Field  (Wrapper_Field (Jtype));

                        --  The 'all' component is of class Object, so a
                        --  downcast to the formal type is needed to satisfy
                        --  consistency checking of the operand stack.

                        if Ekind (Full_Type (Formal)) in Access_Kind then
                           Gen_Check_Cast
                             (Class_Of_Type (JVM_Type (Full_Type (Formal))));
                        end if;

                        Gen_Store_Local (Actual_Addr.Local_Var);
                     when Field_Address =>
                        if Ref_Temps (Temp_Count) /= Null_Local_Var then
                           Gen_Load_Local (Ref_Temps (Temp_Count));
                        else
                           Actual_Addr := Evaluate_Addr (Actual);
                        end if;
                        Gen_Load_Local (Copy_Temps (Temp_Count));
                        Gen_Get_Field (Wrapper_Field (Jtype));

                        --  The 'all' component is of class Object, so a
                        --  downcast to the formal type is needed to satisfy
                        --  consistency checking of the operand stack.

                        if Ekind (Full_Type (Formal)) in Access_Kind then
                           Gen_Check_Cast
                             (Class_Of_Type (JVM_Type (Full_Type (Formal))));
                        end if;

                        Gen_Put_Field (Field_Ids (Temp_Count));
                     when Indexed_Address =>
                        Gen_Load_Local (Ref_Temps (Temp_Count));
                        Gen_Load_Local (Index_Temps (Temp_Count));
                        Gen_Load_Local (Copy_Temps (Temp_Count));
                        Gen_Get_Field  (Wrapper_Field (Jtype));

                        --  The 'all' component is of class Object, so a
                        --  downcast to the formal type is needed to satisfy
                        --  consistency checking of the operand stack.

                        if Ekind (Full_Type (Formal)) in Access_Kind then
                           Gen_Check_Cast
                             (Class_Of_Type (JVM_Type (Full_Type (Formal))));
                        end if;

                        Gen_Store_Array_Element;
                     when others =>
                        pragma Assert (False);
                        raise Program_Error;
                  end case;
               end if;
            end if;

            Formal := Next_Formal_With_Extras (Formal);
            Actual := Next_Actual (Actual);
         end loop;
      end if;
   end Translate_Subprogram_Call;

end Jx_Ch6;
