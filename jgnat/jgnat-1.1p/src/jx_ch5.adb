------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 5                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.43 $
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
with Debug_A;           use Debug_A;
with Einfo;             use Einfo;
with JVM;               use JVM;
with JVM.API;           use JVM.API;
with JVM.Dbg;           use JVM.Dbg;
with JVM.Map;           use JVM.Map;
with J_String;          use J_String;
with J_Types;           use J_Types;
with Jx_Ch3;            use Jx_Ch3;
with Jx_Ch4;            use Jx_Ch4;
with Jx_Ch6;            use Jx_Ch6;
with Jx_Ch11;           use Jx_Ch11;
with Jx_Decl;           use Jx_Decl;
with Jx_Drive;          use Jx_Drive;
with Jx_Swtch;          use Jx_Swtch;
with Jx_Uplev;          use Jx_Uplev;
with Nlists;            use Nlists;
with Osint;
with Output;            use Output;
with Sem_Util;          use Sem_Util;
with Sinfo;             use Sinfo;
with Sinput;            use Sinput;
with System.Soft_Links; use System.Soft_Links;
with Types;             use Types;
with Uintp;             use Uintp;

package body Jx_Ch5 is

   procedure Assign_Array_Value (Target, Source : Node_Id);
   --  Generates code to copy the Source array value to Target
   --  (using java.lang.System.arraycopy).

   procedure Assign_Multiarray
     (Target     : Node_Id;
      Source     : Node_Id;
      Dimensions : Pos_8);
   --  Generates code to copy the highest-dimension subarrays from
   --  the multidimensional Source array to Target.

   procedure Assign_Record_Value (Target, Source : Node_Id);
   --  Generates code to copy Source's fields to Target.

   procedure Generate_Assignment (Assignment_Stmt : Node_Id);
   --  Generates code to evaluate an Ada assignment statement

   procedure Generate_Case_Statement (Case_Stmt : Node_Id);
   --  Generates code for an Ada case statement

   procedure Generate_If_Statement (If_Stmt : Node_Id);
   --  Generates code for an if statement

   procedure Generate_Loop_Statement (Loop_Stmt : Node_Id);
   --  Generates code for an Ada loop statement

   procedure Generate_Exit_Statement (Exit_Stmt : Node_Id);
   --  Generates code for a loop exit statement

   procedure Generate_Return (Return_Stmt : Node_Id);
   --  Generates code for a subprogram return statement

   procedure Translate_Case_Alternative
     (Altern : Node_Id;
      Obj    : Local_Var_Id := Null_Local_Var);
   --  Call-through procedure to translate the set of statements
   --  associated with a case statement alternative. This procedure
   --  is passed to Generate_Switch from Generate_Case_Statement.
   --  The Obj parameter is ignored and is only present in order
   --  to match the profile of the access type Switch_Action
   --  used by Generate_Switch.

   procedure Gen_At_End_Call (Stmt : Node_Id);
   --  Searches for the innermost handled sequence of statements enclosing
   --  Stmt that has an associated At_End_Proc and, if one is found, generates
   --  a call to the attached procedure.

   -------------------------
   -- Translate_Statement --
   -------------------------

   procedure Translate_Statement (Stmt_Node : Node_Id) is
   begin
      Set_Current_Source_Loc (Sloc (Stmt_Node));

      case Nkind (Stmt_Node) is
         --  >>  when N_Abort_Statement =>

         when N_Assignment_Statement =>

            Generate_Assignment (Stmt_Node);

         when N_Case_Statement =>

            Generate_Case_Statement (Stmt_Node);

         --  >>  when N_Asynchronous_Select =>
         --  >>  when N_Code_Statement =>
         --  >>  when N_Conditional_Entry_Call =>
         --  >>  when N_Delay_Relative_Statement =>
         --  >>  when N_Delay_Until_Statement =>
         --  >>  when N_Entry_Call_Statement =>

         when N_If_Statement =>

            Generate_If_Statement (Stmt_Node);

         when N_Loop_Statement =>

            Generate_Loop_Statement (Stmt_Node);

         when N_Block_Statement =>

            Translate_Declarations (Declarations (Stmt_Node));
            Translate_Handled_Statements
              (Handled_Statement_Sequence (Stmt_Node));

         when N_Exit_Statement =>

            Generate_Exit_Statement (Stmt_Node);

         when N_Free_Statement =>

            --  For now we simply evaluate the argument of the
            --  unchecked deallocation and set it to null.
            --  Eventually we have to take account of the
            --  Storage_Pool and Procedure_To_Call attributes.

            declare
               Acc_Addr : Address_Descriptor
                 := Evaluate_Addr (Expression (Stmt_Node));
            begin
               Gen_Push_Null;
               Store_Elementary_Value (Acc_Addr);
            end;

         when N_Goto_Statement =>
            Gen_Goto (JVM_Label (Entity (Name (Stmt_Node))));

         when N_Label =>
            declare
               Label_Id : Entity_Id := Entity (Identifier (Stmt_Node));

            begin
               Gen_Label (JVM_Label (Label_Id));
            end;

         when N_Implicit_Label_Declaration =>
            Translate (Stmt_Node);

         when N_Null_Statement =>
            null;

         when N_Number_Declaration =>
            null;

         when N_Procedure_Call_Statement =>

            Translate_Subprogram_Call (Stmt_Node);

         --  >>  when N_Requeue_Statement =>

         when N_Return_Statement =>

            Generate_Return (Stmt_Node);

         --  >>  when N_Selective_Accept =>
         --  >>  when N_Timed_Entry_Call =>

         when N_Raise_xxx_Error =>

            Translate_Predefined_Raise (Stmt_Node);

         when N_Raise_Statement =>

            Translate_Raise_Statement (Stmt_Node);

         when N_Exception_Declaration =>

            Translate (Stmt_Node);

         when N_Object_Declaration =>

            Translate (Stmt_Node);

         when N_Renaming_Declaration =>

            Translate (Stmt_Node);

         when N_Itype_Reference =>

            Translate (Stmt_Node);

         when N_Package_Declaration | N_Package_Body =>

            Translate (Stmt_Node);

         when N_Generic_Instantiation =>

            Translate (Stmt_Node);

         when N_Subprogram_Body =>

            Translate (Stmt_Node);

         when N_Subprogram_Declaration | N_Abstract_Subprogram_Declaration =>

            Translate (Specification (Stmt_Node));

         when N_Generic_Declaration =>

            Translate (Stmt_Node);

         when N_Subtype_Declaration =>

            Translate_Subtype (Defining_Entity (Stmt_Node));

         when N_Full_Type_Declaration
            | N_Private_Type_Declaration
            | N_Private_Extension_Declaration
            | N_Incomplete_Type_Declaration =>

            Translate (Stmt_Node);

         when N_Freeze_Entity =>

            Translate_Declarations (Actions (Stmt_Node));

         when N_Task_Type_Declaration | N_Task_Body | N_Task_Definition =>

            Translate (Stmt_Node);

         when N_Protected_Type_Declaration
            | N_Protected_Body
            | N_Protected_Definition =>

            Translate (Stmt_Node);

         when N_Use_Package_Clause | N_Use_Type_Clause =>

            null;

         when N_Pragma =>

            Translate (Stmt_Node);

         when N_Body_Stub =>

            Translate (Stmt_Node);

         when N_Validate_Unchecked_Conversion =>

            --  No checking for now ???

            null;

         when N_At_Clause |
              N_Component_Clause |
              N_Enumeration_Representation_Clause |
              N_Mod_Clause |
              N_Record_Representation_Clause |
              N_Attribute_Definition_Clause =>

            null;

         when others =>
            if Debug_Flag_JJ then
               Osint.Fail
                 ("*** unsupported statement node: ",
                  Node_Kind'Image (Nkind (Stmt_Node)));
            else
               pragma Assert (False);
               raise Program_Error;
            end if;
      end case;
   end Translate_Statement;

   --------------------------
   -- Translate_Statements --
   --------------------------

   procedure Translate_Statements (Statements : List_Id) is
      Stmt_Node : Node_Id := First (Statements);

   begin
      while Present (Stmt_Node) loop
         Debug_A_Entry ("(Ada-to-JVM) ", Stmt_Node);

         begin
            Print_Source_Line (Stmt_Node);

            Translate_Statement (Stmt_Node);

         --  If an exception occurs during statement translation,
         --  then report the error and continue processing at the
         --  next statement. The stack is reset to avoid cascading
         --  stack errors.

         exception
            when others =>
               if Debug_Flag_JJ then
                  Reset_Stack;
                  Write_Str ("*** Unsupported feature at ");
                  Write_Location (Sloc (Stmt_Node));
                  Write_Eol;
                  Write_Str (">>> Exception raised at ");
                  Write_Str (Exception_Message (Get_Current_Excep.all.all));
                  Write_Eol;

               else
                  raise;
               end if;
         end;

         Stmt_Node := Next (Stmt_Node);

         Debug_A_Exit ("(Ada-to-JVM) ", Stmt_Node, " (done)");
      end loop;
   end Translate_Statements;

   ----------------------------------
   -- Translate_Handled_Statements --
   ----------------------------------

   procedure Translate_Handled_Statements (Handled_Statements : Node_Id) is
      Handlers  : List_Id;
      Start_Lbl : Label_Id;
      End_Lbl   : Label_Id;
      Exit_Lbl  : Label_Id;

   begin
      if Present (Handled_Statements) then
         Handlers := Exception_Handlers (Handled_Statements);

         if Present (Handlers) then
            Start_Lbl := New_Label;
            Gen_Label (Start_Lbl);
         end if;

         Translate_Statements (Statements (Handled_Statements));

         if Present (Handlers) then
            End_Lbl  := New_Label;
            Exit_Lbl := New_Label;
            Gen_Goto (Exit_Lbl);

            Gen_Label (End_Lbl);
         end if;

         Translate_Exception_Handlers (Handlers, Start_Lbl, End_Lbl, Exit_Lbl);

         if Present (Handlers) then
            Gen_Label (Exit_Lbl);
         end if;

         --  Translate the at-end part (if any) of the statement sequence

         if Present (At_End_Proc (Handled_Statements)) then
            Gen_At_End_Call (Handled_Statements);
         end if;
      end if;
   end Translate_Handled_Statements;

   ------------------------
   -- Assign_Array_Value --
   ------------------------

   procedure Assign_Array_Value (Target, Source : Node_Id) is
      Dimensions    : Pos_8 := Pos_8 (Number_Dimensions (Full_Type (Target)));
      Target_Type   : Entity_Id := Full_Type (Etype (Target));
      Target_Slice  : Boolean;
      Target_Prefix : Node_Id;
      Target_Subt   : Entity_Id;
      Source_Slice  : Boolean;
      Source_Prefix : Node_Id;
      Source_Subt   : Entity_Id;

   begin
      if Dimensions > 1 then
         Assign_Multiarray (Target, Source, Dimensions);

      --  One-dimensional arrays with elementary components are copied
      --  using the java.lang.System.arraycopy method.

      elsif Number_Dimensions (Target_Type) = 1
        and then Ekind (Full_Type (Component_Type (Target_Type)))
          in Elementary_Kind
        and then not
          Has_Aliased_Components (Full_Type ((Target_Type)))
      then
         Test_For_Slice (Target, Target_Slice, Target_Prefix, Target_Subt);
         Test_For_Slice (Source, Source_Slice, Source_Prefix, Source_Subt);

         --  Generate a call to java.lang.System.arraycopy.
         --  The source and target offsets are both zero for the
         --  normal array assignment case. In the case of slice
         --  assignments we compute the starting offset within
         --  the containing array by calling Gen_Array_Subscript.
         --  The length of the target array is used to determine
         --  the number of elements to copy.

         --  Evaluate the source array and push the offset to copy
         --  from (srcOffset).

         Evaluate_Array_Address (Source);

         if Source_Slice then
            Gen_Array_Subscript (Source_Prefix, Index_First (Source_Subt));
         else
            Gen_Push_Int (Uint_0);
         end if;

         --  Now evaluate the target array and push its length

         Evaluate_Array_Address (Target);

         --  Note: We need to perform an explicit length check if either
         --  the source or the target array is a slice. ???

         --  Push the offset and length for the target array (dstOffset)

         if Target_Slice then
            Gen_Array_Subscript (Target_Prefix, Index_First (Target_Subt));
            Load_Index_Length (First_Index (Target_Subt));

         else
            Gen_Duplicate;
            Gen_Array_Length;
            Gen_Push_Int (Uint_0);

            --  Swap dstOffset with the array length

            Gen_Swap;
         end if;

         --  Finally, invoke java.lang.System.arraycopy (takes
         --  parameters: src object, srcoffset, dest object,
         --  dstoffset, length).

         Gen_Invoke_API_Method (System_arraycopy);

      --  If the array has composite components or aliased elementary
      --  components, then the array components must be copied by invoking
      --  the array type's deep copy operation.

      else
         Test_For_Slice (Target, Target_Slice, Target_Prefix, Target_Subt);
         Test_For_Slice (Source, Source_Slice, Source_Prefix, Source_Subt);

         --  Evaluate the reference to the target array and push the starting
         --  index for the target (dstOffset).

         Evaluate_Array_Address (Target);

         if Target_Slice then
            Gen_Array_Subscript (Target_Prefix, Index_First (Target_Subt));
         else
            Gen_Push_Int (Uint_0);
         end if;

         Evaluate_Array_Address (Source);

         --  Load the array length followed by the starting index
         --  of the evaluated array.

         if Source_Slice then
            Gen_Array_Subscript (Source_Prefix, Index_First (Source_Subt));
            Load_Index_Length (First_Index (Source_Subt));
            Gen_Swap;
         else
            Gen_Duplicate;
            Gen_Array_Length;
            Gen_Push_Int (Uint_0);
         end if;

         Gen_Invoke_Deep_Copy (Base_Type (Target_Subt));

         --  Pop the resulting reference, since the target can never be
         --  null in this context (so the deep copy cannot have allocated
         --  a new array object that needs to be saved back in the target).

         Gen_Pop;
      end if;
   end Assign_Array_Value;

   ------------------------
   -- Assign_Multi_Array --
   ------------------------

   procedure Assign_Multiarray
     (Target     : Node_Id;
      Source     : Node_Id;
      Dimensions : Pos_8)
   is
   begin
      --  Evaluate the target and source arrays, plus push the extraneous
      --  parameters currently used for all array deep copy methods (even
      --  though they're only needed for one-dimensional arrays). ???

      Evaluate_Array_Address (Target);
      Gen_Push_Int (Uint_0);

      Evaluate_Array_Address (Source);
      Gen_Duplicate;
      Gen_Array_Length;
      Gen_Push_Int (Uint_0);

      --  Call the array type's deep copy method and pop the result
      --  (since target will never be null in an assignment statement.

      Gen_Invoke_Deep_Copy (Full_Type (Target));
      Gen_Pop;
   end Assign_Multiarray;

   -------------------------
   -- Assign_Record_Value --
   -------------------------

   procedure Assign_Record_Value (Target, Source : Node_Id) is
   begin
      --  Evaluate the source and target object references

      Evaluate_Expr (Target);
      Evaluate_Expr (Source);

      --  Invoke the record type's deep copy method

      Gen_Invoke_Deep_Copy (Full_Type (Target));

      --  Throw away the result (which is simply the value of target
      --  passed in on the call).

      Gen_Pop;
   end Assign_Record_Value;

   -------------------------
   -- Generate_Assignment --
   -------------------------

   procedure Generate_Assignment (Assignment_Stmt : Node_Id) is
      Target : constant Node_Id := Name (Assignment_Stmt);
      Source : constant Node_Id := Expression (Assignment_Stmt);
      Addr   : Address_Descriptor;

   begin
      case Ekind (Full_Type (Target)) is
         when Elementary_Kind =>
            Addr := Evaluate_Addr (Target);
            Evaluate_Expr (Source, Check_Subtype => Etype (Target));
            Store_Elementary_Value (Addr);

         when Einfo.Array_Kind =>
            Assign_Array_Value (Target, Source);

         when Einfo.Record_Kind =>
            Assign_Record_Value (Target, Source);

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Generate_Assignment;

   --------------------------------
   -- Translate_Case_Alternative --
   --------------------------------

   procedure Translate_Case_Alternative
     (Altern : Node_Id;
      Obj    : Local_Var_Id := Null_Local_Var)
   is
   begin
      Translate_Statements (Statements (Altern));
   end Translate_Case_Alternative;

   -----------------------------
   -- Generate_Case_Statement --
   -----------------------------

   procedure Generate_Case_Statement (Case_Stmt : Node_Id) is
   begin
      Evaluate_Expr (Expression (Case_Stmt));
      Generate_Switch
        (Alternatives (Case_Stmt), Translate_Case_Alternative'Access);
   end Generate_Case_Statement;

   -----------------------------
   -- Generate_Exit_Statement --
   -----------------------------

   procedure Generate_Exit_Statement (Exit_Stmt : Node_Id) is
      Loop_Id     : Entity_Id := Empty;
      Parent_Stmt : Node_Id := Parent (Exit_Stmt);
      Exit_Cond   : Node_Id := Condition (Exit_Stmt);
      Exit_Label  : Label_Id;

   begin
      if Present (Name (Exit_Stmt)) then
         Loop_Id := Entity (Name (Exit_Stmt));

      --  This is an exit from the innermost enclosing loop. Locate
      --  the loop statement and pick up its loop identifier entity.

      else
         while Present (Parent_Stmt)
           and then Nkind (Parent_Stmt) /= N_Loop_Statement
         loop
            Parent_Stmt := Parent (Parent_Stmt);
         end loop;

         pragma Assert (Nkind (Parent_Stmt) = N_Loop_Statement);

         if Present (Identifier (Parent_Stmt)) then
            Loop_Id := Entity (Identifier (Parent_Stmt));
         end if;
      end if;

      --  If the loop being exited has a loop identifier, then create
      --  an exit label for the loop if it doesn't already have one
      --  and associate it with the loop identifier.

      if Present (Loop_Id) then
         if JVM_Entity (Loop_Id) = Null_Label then
            Set_Map (Loop_Id, New_Label);
         end if;

         Exit_Label := JVM_Entity (Loop_Id);

      --  If there is not a loop id, then associate the enclosing loop
      --  statement node itself with the exit label.

      elsif JVM_Entity (Parent_Stmt) = Null_Label then
         Set_Map (Parent_Stmt, New_Label);
         Exit_Label := JVM_Entity (Parent_Stmt);
      end if;

      if Present (Exit_Cond) then
         Evaluate_Expr (Exit_Cond, Exit_Label, True);
         if Exit_Label /= Null_Label then
            Gen_Branch_Not_Equal (Exit_Label);
         end if;
      else
         Gen_Goto (Exit_Label);
      end if;
   end Generate_Exit_Statement;

   ---------------------------
   -- Generate_If_Statement --
   ---------------------------

   procedure Generate_If_Statement (If_Stmt : Node_Id) is
      False_Label : Label_Id := New_Label;
      Save_Label  : Label_Id := False_Label;
      Exit_Label  : constant Label_Id := New_Label;
      Elsifs      : constant List_Id  := Elsif_Parts (If_Stmt);
      Elsif_Part  : Node_Id;
      Else_Stmts  : constant List_Id  := Else_Statements (If_Stmt);

   begin
      Evaluate_Expr (Condition (If_Stmt), Save_Label, False);

      --  If Save_Label was unused during the condition evaluation,
      --  then generate a branch to it now based on the Boolean
      --  top-of-stack value.

      if Save_Label /= Null_Label then
         Gen_Branch_Equal (False_Label);
      end if;

      Translate_Statements (Then_Statements (If_Stmt));
      if Present (Elsifs) or else Present (Else_Stmts) then
         Gen_Goto (Exit_Label);
      end if;

      if Present (Elsifs) then
         Elsif_Part := First (Elsifs);
         while Present (Elsif_Part) loop
            Gen_Label (False_Label);
            False_Label := New_Label;
            Save_Label  := False_Label;
            Evaluate_Expr (Condition (Elsif_Part), Save_Label, False);
            if Save_Label /= Null_Label then
               Gen_Branch_Equal (False_Label);
            end if;

            Translate_Statements (Then_Statements (Elsif_Part));
            if Present (Next (Elsif_Part)) or else Present (Else_Stmts) then
               Gen_Goto (Exit_Label);
            end if;

            Elsif_Part := Next (Elsif_Part);
         end loop;
      end if;

      Gen_Label (False_Label);

      if Present (Else_Stmts) then
         Translate_Statements (Else_Statements (If_Stmt));
      end if;

      if Present (Elsifs) or else Present (Else_Stmts) then
         Gen_Label (Exit_Label);
      end if;
   end Generate_If_Statement;

   ---------------------------
   -- Generate_Loop_Statement --
   ---------------------------

   procedure Generate_Loop_Statement (Loop_Stmt : Node_Id) is
      Iter_Scheme  : constant Node_Id := Iteration_Scheme (Loop_Stmt);
      Repeat_Label : constant Label_Id := New_Label;
      Exit_Label   : Label_Id;
      Save_Label   : Label_Id;
      Loop_Id      : Entity_Id;
      Param_Spec   : Node_Id := Empty;
      Loop_Param   : Entity_Id;
      Loop_Subtype : Node_Id;
      Low_Bnd      : Node_Id;
      High_Bnd     : Node_Id;
      Loop_Index   : Local_Var_Id;
      Loop_Limit   : Local_Var_Id;

   begin

      if Present (Iter_Scheme) then
         pragma Assert (not Present (Condition_Actions (Iter_Scheme)));

         Param_Spec := Loop_Parameter_Specification (Iter_Scheme);

         if Present (Param_Spec) then
            Loop_Param := Defining_Identifier (Param_Spec);
            Loop_Index
              := New_Local_Var (Name ("_loop_param"), JVM_Type (Loop_Param));
            Loop_Limit
              := New_Local_Var (Name ("_loop_limit"), JVM_Type (Loop_Param));
            Set_Map (Loop_Param, Loop_Index);

            Loop_Subtype := Discrete_Subtype_Definition (Param_Spec);

            if Nkind (Loop_Subtype) = N_Range then
               Low_Bnd  := Low_Bound (Loop_Subtype);
               High_Bnd := High_Bound (Loop_Subtype);

            elsif Nkind (Loop_Subtype) = N_Identifier then
               Low_Bnd  := Low_Bound (Scalar_Range (Entity (Loop_Subtype)));
               High_Bnd := High_Bound (Scalar_Range (Entity (Loop_Subtype)));

            elsif Nkind (Loop_Subtype) = N_Subtype_Indication then
               Low_Bnd  := Low_Bound (Scalar_Range (Etype (Loop_Subtype)));
               High_Bnd := High_Bound (Scalar_Range (Etype (Loop_Subtype)));

            else
               pragma Assert (False);
               raise Program_Error;
            end if;

            Evaluate_Expr (Low_Bnd);
            Evaluate_Expr (High_Bnd);

            if Reverse_Present (Param_Spec) then
               Gen_Store_Local (Loop_Index);
               Gen_Store_Local (Loop_Limit);
               Gen_Label (Repeat_Label);
               Gen_Load_Local (Loop_Limit);
               Gen_Load_Local (Loop_Index);
            else
               Gen_Store_Local (Loop_Limit);
               Gen_Store_Local (Loop_Index);
               Gen_Label (Repeat_Label);
               Gen_Load_Local (Loop_Index);
               Gen_Load_Local (Loop_Limit);
            end if;

            Exit_Label := New_Label;
            Gen_Compare_Branch_Greater (Exit_Label);

         elsif Present (Condition (Iter_Scheme)) then
            Gen_Label (Repeat_Label);
            Exit_Label := New_Label;
            Save_Label := Exit_Label;
            Evaluate_Expr (Condition (Iter_Scheme), Save_Label, False);

            --  If Save_Label was unused during the condition evaluation,
            --  then generate a branch to it now based on the Boolean
            --  top-of-stack value.

            if Save_Label /= Null_Label then
               Gen_Branch_Equal (Exit_Label);
            end if;

         else
            pragma Assert (False);
            raise Program_Error;
         end if;

      --  Endless loop case

      else
         Gen_Label (Repeat_Label);
      end if;

      Translate_Statements (Statements (Loop_Stmt));

      if Present (Param_Spec) then
         if Reverse_Present (Param_Spec) then
            Gen_Incr_Local (Loop_Index, Uint_Minus_1);
         else
            Gen_Incr_Local (Loop_Index, Uint_1);
         end if;
      end if;

      Gen_Goto (Repeat_Label);

      if Present (Iter_Scheme) then
         Gen_Label (Exit_Label);
      end if;

      --  If a JVM label is associated with the loop's identifier or
      --  the loop statement itself, then emit the label (it must be
      --  the target of an exit statement).

      if Present (Identifier (Loop_Stmt)) then
         Loop_Id := Entity (Identifier (Loop_Stmt));
         if JVM_Entity (Loop_Id) /= Null_Label then
            Gen_Label (JVM_Entity (Loop_Id));
         end if;
      elsif JVM_Entity (Loop_Stmt) /= Null_Label then
         Gen_Label (JVM_Entity (Loop_Stmt));
      end if;
   end Generate_Loop_Statement;

   ---------------------
   -- Generate_Return --
   ---------------------

   procedure Generate_Return (Return_Stmt : Node_Id) is
   begin
      --  If this is a return from a non-constructor function then
      --  evaluate the return expression (constructors have void
      --  results and so the return expression must be ignored).

      if Present (Expression (Return_Stmt))
        and then Name_String (Name (Current_Method)) /= "<init>"
      then
         if Is_Return_By_Reference_Type (Etype (Expression (Return_Stmt))) then
            Evaluate_Expr (Expression (Return_Stmt));
         else
            Evaluate_With_Copy (Expression (Return_Stmt));
         end if;
      end if;

      --  Generate a call to the nearest enclosing at-end procedure, if any,
      --  prior to the method return.

      Gen_At_End_Call (Return_Stmt);

      Gen_Method_Return;
   end Generate_Return;

   --------------------------
   -- Generate_At_End_Call --
   --------------------------

   procedure Gen_At_End_Call (Stmt : Node_Id) is
      Parent_Stmt : Node_Id := Stmt;
      At_End_Subp : Entity_Id;

   begin
      --  Search for the innermost enclosing handled sequence of statements
      --  with an associated At_End_Proc and generate a call to the procedure.

      while Present (Parent_Stmt)
        and then Nkind (Parent_Stmt) not in N_Proper_Body
      loop
         if Nkind (Parent_Stmt) = N_Handled_Sequence_Of_Statements
           and then Present (At_End_Proc (Parent_Stmt))
         then
            At_End_Subp := Entity (At_End_Proc (Parent_Stmt));

            --  If this is a call to a nested clean-up method, then we must
            --  pass a static link parameter.

            if Present (Enclosing_Subprogram (At_End_Subp))
              and then not Is_Imported (At_End_Subp)
            then
               Load_Static_Link (Enclosing_Method (At_End_Subp));
            end if;

            Gen_Invoke_Method (JVM_Method (At_End_Subp));

            return;
         end if;

         Parent_Stmt := Parent (Parent_Stmt);
      end loop;
   end Gen_At_End_Call;

end Jx_Ch5;
