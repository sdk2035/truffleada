------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  J V M                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.61 $
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

with J_String; use J_String;
with J_Types;  use J_Types;
with JVM.API;  use JVM.API;
with JVM.Code; use JVM.Code;
with JVM.Emit; use JVM.Emit;
with JVM.Info; use JVM.Info;
with JVM.Pool; use JVM.Pool;
with JVM_File; use JVM_File;
with Namet;    use Namet;
with Output;   use Output;
with Types;    use Types;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body JVM is

   Active_Method : Method_Id := Null_Method;
   --  Denotes the method for which code is currently being generated
   --  (also referred to as the 'current method').

   Active_Class  : Class_Id := Null_Class;
   --  Denotes the class associated with Active_Method (also referred to
   --  as the 'current method'). Any constant pool reference items generated
   --  for code in Active_Method will be associated with this class.

   Active_Subr   : Subroutine_Id := Null_Subroutine;
   --  Denotes a subroutine associated with the Active_Method which is
   --  currently having its code generated. There can be at most one
   --  active subroutine.

   Active_Seq    : Code_Sequence;
   --  The code sequence associated with the currently active method
   --  (or active subroutine when Active_Subr /= Null_Subroutine).

   Saved_Seq     : Code_Sequence;
   --  Temporary variable to hold on to the current method's main
   --  code sequence while an entry sequence for the method is
   --  being generated.

   Active_Handlers : Handler_Sequence;
   --  The exception handler entry sequence associated with the currently
   --  active method (or active subroutine if Active_Subr /= Null_Subroutine).

   Active_Stack  : Op_Stack_Id := Null_Op_Stack;
   --  The operand type stack associated with the currently active method
   --  (or active subroutine when Active_Subr /= Null_Subroutine).

   Active_Switch_Stmt : JVM.Code.Instruction;
   --  Represents a switch statement actively being generated for the
   --  current method, if any. If not active, then the Op discriminant
   --  must have the value Xxxunusedxxx.

   Entry_Seq_Active : Boolean := False;
   --  State variable indicating whether an entry code sequence is
   --  currently being generated for the active method.

   Stack_Checking : Boolean := True;
   --  State variable indicating whether checks should be performed
   --  on branch and label generation to ensure that the current method's
   --  evaluation stack is empty. Stack checking can be turned off by
   --  calls to Set_Stack_Checking (Enable => False).

   Stack_Marked : Boolean := False;
   --  State variable indicating whether the current method's operand
   --  stack has been marked via a call to Mark_Stack.

   Debug : Boolean := False;
   --  When true, enables debugging output from routines in this package.

   ------------------------------------
   -- Utility and Debugging Routines --
   ------------------------------------

   procedure Print (S : String);
   --  Writes S as debugging output

   procedure Print (N : Name_Id);
   --  Writes N's associated string as debugging output

   procedure Print_Line (S : String := "");
   --  Writes S following by a new line as debugging output

   ---------------
   -- Set_Trace --
   ---------------

   procedure Set_Trace (Trace : Boolean) is
   begin
      Debug := Trace;
   end Set_Trace;

   -----------
   -- Print --
   -----------

   procedure Print (S : String) is
   begin
      Write_Str (S);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (N : Name_Id) is
   begin
      if N = No_Name then
         Write_Str ("<no name>");
      else
         Write_Name (N);
      end if;
   end Print;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (S : String := "") is
   begin
      Write_Str (S);
      Write_Eol;
   end Print_Line;

   -----------------------------------------------------------------------
   -- Operations for creating and generating JVM classes and interfaces --
   -----------------------------------------------------------------------

   ---------------
   -- New_Class --
   ---------------

   function New_Class
     (Name        : Name_Id;
      Pkg_Name    : String_Id := No_String;
      Src_Name    : Name_Id   := No_Name;
      Super       : Class_Id  := Java_Lang_Object;
      Outer_Class : Class_Id  := Null_Class;
      Public      : Boolean   := True;
      Abstrct     : Boolean   := False;
      Final       : Boolean   := False)
      return        Class_Id
   is
      Class       : constant Class_Id := New_Class;
      Class_Type  : constant Type_Id  := New_Type (Class_Kind);
      Init_Method : Method_Id;

   begin
      pragma Assert (Name /= No_Name);

      Set_Name (Class, Name);
      Set_Name (Class_Type, Name);

      Set_Pkg_Name (Class, Pkg_Name);
      Set_Src_Name (Class, Src_Name);
      Set_Superclass (Class, Super);
      Set_Outer_Class (Class, Outer_Class);
      Set_Is_Public (Class, Public);
      Set_Is_Abstract (Class, Abstrct);
      Set_Is_Final (Class, Final);
      Set_Is_Open (Class, False);

      --  Associate the new class and its class type with one another

      Set_Class_Type (Class, Class_Type);
      Set_Class (Class_Type, Class);

      --  Create the default constructor method for the class

      Init_Method
        := New_Method (Class, J_String.Name ("<init>"), Void_Type, False);

      return Class;
   end New_Class;

   -------------------
   -- New_Interface --
   -------------------

   function New_Interface
     (Name     : Name_Id;
      Pkg_Name : String_Id := No_String;
      Src_Name : Name_Id   := No_Name;
      Public   : Boolean   := True)
      return     Class_Id
   is
      Interface    : constant Class_Id := New_Interface;
      Intface_Type : constant Type_Id  := New_Type (Class_Kind);

   begin
      pragma Assert (Name /= No_Name);

      Set_Name (Interface, Name);
      Set_Name (Intface_Type, Name);
      Set_Pkg_Name (Interface, Pkg_Name);
      Set_Src_Name (Interface, Src_Name);
      Set_Superclass (Interface, Java_Lang_Object);
      Set_Outer_Class (Interface, Null_Class);
      Set_Is_Public (Interface, Public);
      Set_Is_Abstract (Interface, True);

      --  Associate the new class and its class type with one another

      Set_Class_Type (Interface, Intface_Type);
      Set_Class (Intface_Type, Interface);

      return Interface;
   end New_Interface;

   -------------------------
   -- Associate_Interface --
   -------------------------

   procedure Associate_Interface (Class : Class_Id; Interface : Class_Id) is
   begin
      pragma Assert (Is_Interface (Interface));
      Add_Interface (Class, Interface);
   end Associate_Interface;

   ------------------
   -- Is_Interface --
   ------------------

   function Is_Interface (Class_Or_Interface : Class_Id) return Boolean is
   begin
      return JVM.Info.Is_Interface (Class_Or_Interface);
   end Is_Interface;

   -------------------------
   -- Change_To_Interface --
   -------------------------

   procedure Change_To_Interface (Class : Class_Id) is
   begin
      Set_Is_Interface (Class);
   end Change_To_Interface;

   ----------------------
   -- Begin_Class_File --
   ----------------------

   procedure Begin_Class_File (Class : Class_Id) is
   begin
      Set_Is_Open (Class);

      if Debug then
         Print ("*** Opening class file for ");
         Print (Name (Class));
         Print_Line;
         Print_Line;
         Print ("class ");
         Print (Name (Class));
         if Superclass (Class) /= Java_Lang_Object then
            Print (" extends ");
            Print (Name (Superclass (Class)));
         end if;
         Print_Line (" {");
         Print_Line;
      end if;
   end Begin_Class_File;

   --------------------
   -- End_Class_File --
   --------------------

   procedure End_Class_File (Class : Class_Id) is
      Method : Method_Id := First_Method (Class);

   begin
      --  All methods must be either abstract or closed at this point

      while Method /= Null_Method loop
         pragma Assert (Is_Abstract (Method) or else not Is_Open (Method));
         Method := Next_Method (Method);
      end loop;

      --  Generate the physical class file

      JVM.Emit.Produce_Class_File (Class);

      Set_Is_Open (Class, False);

      if Debug then
         Print_Line;
         Print ("} // ");
         Print (Name (Class));
         Print_Line;
         Print_Line;
         Print ("*** Closing class file for ");
         Print (Name (Class));
         Print_Line;
      end if;
   end End_Class_File;

   ------------------------
   -- Class_File_Is_Open --
   ------------------------

   function Class_File_Is_Open (Class : Class_Id) return Boolean is
   begin
      return Is_Open (Class);
   end Class_File_Is_Open;

   ----------
   -- Name --
   ----------

   function Name (Class : Class_Id) return Name_Id is
   begin
      return JVM.Info.Name (Class);
   end Name;

   ----------------
   -- Superclass --
   ----------------

   function Superclass (Class : Class_Id) return Class_Id is
   begin
      return JVM.Info.Superclass (Class);
   end Superclass;

   ---------------------
   -- Is_Parent_Class --
   ---------------------

   function Is_Parent_Class (Test_Class, Child : Class_Id) return Boolean is
   begin
      return Child /= Null_Class
        and then (Test_Class = Superclass (Child)
                   or else Is_Parent_Class (Test_Class, Superclass (Child)));
   end Is_Parent_Class;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Class : Class_Id) return Type_Id is
   begin
      return Class_Type (Class);
   end Type_Of;

   -----------------
   -- First_Field --
   -----------------

   function First_Field (Class : Class_Id) return Field_Id is
   begin
      return JVM.Info.First_Field (Class);
   end First_Field;

   ------------------
   -- First_Method --
   ------------------

   function First_Method (Class : Class_Id) return Method_Id is
   begin
      return JVM.Info.First_Method (Class);
   end First_Method;

   -------------------------
   -- Default_Constructor --
   -------------------------

   function Default_Constructor (Class : Class_Id) return Method_Id is
   begin
      return First_Method (Class);
   end Default_Constructor;

   --------------------
   -- Set_Superclass --
   --------------------

   procedure Set_Superclass (Class : Class_Id; Super : Class_Id) is
   begin
      JVM.Info.Set_Superclass (Class, Super);
   end Set_Superclass;

   ------------------
   -- Set_Abstract --
   ------------------

   procedure Set_Abstract (Class : Class_Id; Abstrct : Boolean := True) is
   begin
      JVM.Info.Set_Is_Abstract (Class, Abstrct);
   end Set_Abstract;

   ---------------------------------------------------
   -- Java Type-related declarations and operations --
   ---------------------------------------------------

   ---------------
   -- Type_Kind --
   ---------------

   function Type_Kind (Typ : Type_Id) return JVM_Type_Kind is
   begin
      return JVM.Info.Type_Kind (Typ);
   end Type_Kind;

   -----------------------
   -- Is_Primitive_Type --
   -----------------------

   function Is_Primitive_Type (Typ : Type_Id) return Boolean is
   begin
      return Typ /= Null_Type
        and then Type_Kind (Typ) in Int_Kind .. Double_Kind;
   end Is_Primitive_Type;

   -----------------------
   -- Is_Reference_Type --
   -----------------------

   function Is_Reference_Type (Typ : Type_Id) return Boolean is
   begin
      return Typ /= Null_Type
        and then Type_Kind (Typ) in Array_Kind .. Class_Kind;
   end Is_Reference_Type;

   --------------------
   -- New_Array_Type --
   --------------------

   function New_Array_Type
     (Element_Type : Type_Id;
      Dimensions   : Pos_8   := 1;
      Type_Name    : Name_Id := No_Name)
      return         Type_Id
   is
      Array_Type : constant Type_Id := New_Type (Array_Kind);

   begin
      Set_Name (Array_Type, Type_Name);
      Set_Element_Type (Array_Type, Element_Type);
      Set_Dimensions (Array_Type, Dimensions);

      return Array_Type;
   end New_Array_Type;

   ----------------
   -- Dimensions --
   ----------------

   function Dimensionality (Arr_Type : Type_Id) return Pos_8 is
   begin
      return Dimensions (Arr_Type);
   end Dimensionality;

   -------------------
   -- Class_Of_Type --
   -------------------

   function Class_Of_Type (Class_Type : Type_Id) return Class_Id is
   begin
      return Class (Class_Type);
   end Class_Of_Type;

   ----------------------------
   -- Literal_Needs_Pool_Ref --
   ----------------------------

   function Literal_Needs_Pool_Ref
     (I_Type  : Type_Id;
      Literal : Uint)
      return    Boolean
   is
      Int_Value : Int;

   begin
      if Type_Kind (I_Type) = Long_Kind then
         return Literal /= Uint_0 and then Literal /= Uint_1;

      else
         Int_Value := UI_To_Int (Literal);
         return Int_Value
                  not in Int (Short_Integer'First) .. Int (Short_Integer'Last);
      end if;
   end Literal_Needs_Pool_Ref;

   ----------------------------
   -- Literal_Needs_Pool_Ref --
   ----------------------------

   function Literal_Needs_Pool_Ref
     (F_Type  : Type_Id;
      Literal : Ureal)
      return    Boolean
   is
   begin
      pragma Assert (Type_Kind (F_Type) in Float_Kind .. Double_Kind);

      if Type_Kind (F_Type) = Float_Kind then
         return Literal < Ureal_0 and then Literal > Ureal_2;
      else
         return Literal /= Ureal_0 and then Literal /= Ureal_1;
      end if;
   end Literal_Needs_Pool_Ref;

   ------------------------------------------
   -- Operations for defining class fields --
   ------------------------------------------

   ---------------
   -- New_Field --
   ---------------

   function New_Field
     (Class    : Class_Id;
      Name     : Name_Id;
      Ftype    : Type_Id;
      Static   : Boolean;
      Final    : Boolean       := False;
      Volatile : Boolean       := False;
      Acc_Mode : Member_Access := Public_Access)
      return     Field_Id
   is
      Field : constant Field_Id := New_Field;

   begin
      Set_Name        (Field, Name);
      Set_Class       (Field, Class);
      Set_Field_Type  (Field, Ftype);
      Set_Is_Static   (Field, Static);
      Set_Is_Final    (Field, Final);
      Set_Is_Volatile (Field, Volatile);
      Set_Access_Mode (Field, Acc_Mode);

      Add_Field (Class, Field);

      if Debug then
         Print ("   ");
         if Static then
            Print ("static ");
         end if;
         if Final then
            Print ("final ");
         end if;
         Print (JVM.Info.Name (Ftype));
         Print (" ");
         Print (Name);
         Print_Line (";");
      end if;

      return Field;
   end New_Field;

   ----------
   -- Name --
   ----------

   function Name (Field : Field_Id) return Name_Id is
   begin
      return JVM.Info.Name (Field);
   end Name;

   ----------------
   -- Next_Field --
   ----------------

   function Next_Field (Field : Field_Id) return Field_Id is
   begin
      return JVM.Info.Next_Field (Field);
   end Next_Field;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Field : Field_Id) return Type_Id is
   begin
      return Field_Type (Field);
   end Type_Of;

   ---------------
   -- Is_Static --
   ---------------

   function Is_Static (Field : Field_Id) return Boolean is
   begin
      return JVM.Info.Is_Static (Field);
   end Is_Static;

   -----------
   -- Field --
   -----------

   function Field (Class : Class_Id; Name : Name_Id) return Field_Id is
      Fld : Field_Id := First_Field (Class);

   begin
      while Fld /= Null_Field loop
         if JVM.Info.Name (Fld) = Name then
            return Fld;
         end if;
         Fld := Next_Field (Fld);
      end loop;

      if Superclass (Class) = Null_Class then
         return Null_Field;
      else
         return Field (Superclass (Class), Name);
      end if;
   end Field;

   -----------
   -- Field --
   -----------

   function Field (Class : Class_Id; Name : String) return Field_Id is
   begin
      return Field (Class, J_String.Name (Name));
   end Field;


   ---------------------------------------------------------------
   -- Operations for defining class methods and local variables --
   ---------------------------------------------------------------

   ----------------
   -- New_Method --
   ----------------

   function New_Method
     (Class    : Class_Id;
      Name     : Name_Id;
      Result   : Type_Id;
      Static   : Boolean;
      Abstrct  : Boolean := False;
      Final    : Boolean := False;
      Synch    : Boolean := False;
      Acc_Mode : Member_Access := Public_Access;
      Parent   : Method_Id := Null_Method)
      return     Method_Id
   is
      Method : constant Method_Id := New_Method;
      This   : Local_Var_Id;

   begin
      Set_Name            (Method, Name);
      Set_Class           (Method, Class);
      Set_Result_Type     (Method, Result);
      Set_Is_Abstract     (Method, Abstrct);
      Set_Is_Static       (Method, Static);
      Set_Is_Final        (Method, Final);
      Set_Is_Synchronized (Method, Synch);
      Set_Access_Mode     (Method, Acc_Mode);
      Set_Parent_Method   (Method, Parent);

      Set_Is_Open (Method);

      --  Add the 'this' parameter in the case of an instance method

      if not Static then
         This := New_Method_Parameter
                   (Method, J_String.Name ("$this"), Type_Of (Class));
      end if;

      Add_Method (Class, Method);

      return Method;
   end New_Method;

   -----------------
   -- Next_Method --
   -----------------

   function Next_Method (Method : Method_Id) return Method_Id is
   begin
      return JVM.Info.Next_Method (Method);
   end Next_Method;

   --------------------------
   -- New_Method_Parameter --
   --------------------------

   function New_Method_Parameter
     (Method : Method_Id;
      Name   : Name_Id;
      Ptype  : Type_Id)
      return   Local_Var_Id
   is
      Param : constant Local_Var_Id := New_Local_Var;

   begin
      Set_Name          (Param, Name);
      Set_Local_Index   (Param, Next_Local_Index (Method));
      Set_Method        (Param, Method);
      Set_Variable_Type (Param, Ptype);
      Set_Is_Param      (Param);

      Add_Local_Var     (Method, Param);

      return Param;
   end New_Method_Parameter;

   --------------------------
   -- New_Method_Parameter --
   --------------------------

   function New_Method_Parameter
     (Method : Method_Id;
      Name   : String;
      Ptype  : Type_Id)
      return   Local_Var_Id
   is
   begin
      return New_Method_Parameter (Method, J_String.Name (Name), Ptype);
   end New_Method_Parameter;

   ----------
   -- Name --
   ----------

   function Name (Method : Method_Id) return Name_Id is
   begin
      return JVM.Info.Name (Method);
   end Name;

   -----------------
   -- Open_Method --
   -----------------

   procedure Open_Method (Method : Method_Id) is
      Code_Seq     : Code_Sequence;
      Handler_Seq  : Handler_Sequence;
      Method_Stack : Op_Stack_Id;

   begin
      pragma Assert (Is_Open (Class (Method))
            and then Is_Open (Method)
            and then not Is_Abstract (Method));

      --  Note: In the current implementation model, the method is
      --  marked open when the method is created, so it doesn't need
      --  to be opened again here; not clear if this is best, but
      --  allows for the check that the method hasn't been closed
      --  without requiring an additional Boolean attribute (the
      --  reason we set the method to open instead of closed on
      --  a call to New_Method is to disallow reopening a method
      --  for code generation once it has been closed).

      Set_Is_Open (Method);

      --  Initialize the method's code sequence

      Code_Seq := Method_Code (Method);
      Start_Sequence (Code_Seq);
      Set_Code (Method, Code_Seq);

      --  Initialize the method's exception handler entry sequence

      Handler_Seq := Method_Handlers (Method);
      Start_Sequence (Handler_Seq);
      Set_Handlers (Method, Handler_Seq);

      --  Initialize the method's operand type stack

      Method_Stack := New_Stack (200);
      Set_Op_Stack (Method, Method_Stack);

      if Debug then
         Print ("   ");
         if Access_Mode (Method) = Public_Access then
            Print ("public ");
         elsif Access_Mode (Method) = Protected_Access then
            Print ("protected ");
         elsif Access_Mode (Method) = Private_Access then
            Print ("private ");
         end if;
         if Is_Abstract (Method) then
            Print ("abstract ");
         end if;
         if Is_Static (Method) then
            Print ("static ");
         end if;
         Print (Name (Result_Type (Method)));
         Print (" ");
         Print (Name (Method));
         Print (" (");
         declare
            Param : Local_Var_Id := First_Local_Var (Method);
         begin
            --  Skip any 'this' parameter

            if not Is_Static (Method) then
               Param := Next_Local_Var (Param);
            end if;

            while Param /= Null_Local_Var and then Is_Param (Param) loop
               Print (Name (Type_Of (Param)));
               Print (" ");
               Print (Name (Param));
               Param := Next_Local_Var (Param);
               if Param /= Null_Local_Var then
                  Print (" ");
               end if;
            end loop;
         end;
         Print_Line (") {");
      end if;
   end Open_Method;

   ------------------
   -- Close_Method --
   ------------------

   procedure Close_Method (Method : Method_Id) is
   begin
      pragma Assert (Is_Open (Method) and then Is_Empty (Op_Stack (Method)));
      pragma Assert (Active_Subroutine (Method) = Null_Subroutine);

      --  If the method does not end with a return or throw instruction,
      --  then we generate a raise of Program_Error at the method end.
      --  Note that this can occur in cases where an Assert pragma occurs
      --  at the end of a subprogram, in which case the front end does
      --  not generate a return statement. But the Java verifier
      --  will complain about falling off the end of the code unless
      --  we end the method with a return or throw. This is one case
      --  where we let knowledge of Ada semantics (Program_Error) leak
      --  through to the JVM package, because this is the easiest place
      --  to detect this situation.

      if Get (Last (Active_Seq)).Op not in Ireturn .. Vreturn
        and then Get (Last (Active_Seq)).Op /= Athrow
      then
         Gen_Default_Object (API_Class (Ada_Program_Error));
         Gen_Exception_Throw;
      end if;

      --  Check that all targeted labels have been generated

      declare
         Label : Label_Id := First_Label (Method);

      begin
         while Label /= Null_Label loop
            pragma Assert
              (not Is_Targeted (Label) or else Location (Label) /= Null_Instr);

            --  If the label instruction has no successor, then it
            --  must be at the end of the method and so we have to
            --  generate an extra instruction in order to satisfy
            --  the verifier. This instruction should not be reachable,
            --  even though it's targeted. This can occur due to
            --  cases like 'if' statements where each branch of
            --  the statement contains a return, but we have still
            --  generated a branch out of the statement (ideally
            --  we should optimize away such branches, but that's
            --  a bit tricky ???).

            if Is_Targeted (Label)
              and then Get (Location (Label)).Next = Null_Instr
            then
               Gen_NOP;
            end if;

            Label := Next_Label (Label);
         end loop;
      end;

      --  Record the maximum word depth of the stack before it gets
      --  deallocated.

      Set_Max_Stack_Depth (Method, Max_Depth (Op_Stack (Method)));

      --  If this is the current active method then update the method's
      --  code and handler sequences and reset the active method and its
      --  associated variables.

      if Active_Method = Method then
         Set_Code (Method, Active_Seq);
         Set_Handlers (Method, Active_Handlers);
         Active_Method := Null_Method;
         Active_Class  := Null_Class;
         Active_Subr   := Null_Subroutine;
         Active_Stack  := Null_Op_Stack;
      end if;


      --  Ensure that the method's operand stack is freed

      Set_Op_Stack (Method, Null_Op_Stack);

      Set_Is_Open (Method, False);

      if Debug then
         Print ("   } // ");
         Print (Name (Method));
         Print_Line;
      end if;
   end Close_Method;

   ------------------------
   -- Set_Current_Method --
   ------------------------

   procedure Set_Current_Method (Method : Method_Id) is
   begin
      pragma Assert (Active_Subr = Null_Subroutine);

      pragma Assert (Is_Open (Class (Method)) and then Is_Open (Method));

      pragma Assert (not Entry_Seq_Active);

      --  Save away the current method's code sequence and operand stack

      if Active_Method /= Null_Method then
         Set_Code (Active_Method, Active_Seq);
         Set_Handlers (Active_Method, Active_Handlers);
         Set_Op_Stack (Active_Method, Active_Stack);
         Set_Stack_Checking (Active_Method, Stack_Checking);
      end if;

      Active_Method   := Method;
      Active_Class    := Class (Method);
      Active_Seq      := Method_Code (Method);
      Active_Handlers := Method_Handlers (Method);
      Active_Stack    := Op_Stack (Method);
      Active_Subr     := Null_Subroutine;
      Stack_Checking  := Is_Stack_Checking (Method);
      Stack_Marked    := Marked (Active_Stack);

      if Debug then
         Print ("*** Switching to method ");
         Print (Name (Method));
         Print_Line;
      end if;
   end Set_Current_Method;

   --------------------
   -- Current_Method --
   --------------------

   function Current_Method return Method_Id is
   begin
      return Active_Method;
   end Current_Method;

   -------------------------------
   -- Start_Entry_Code_Sequence --
   -------------------------------

   procedure Start_Entry_Code_Sequence is
   begin
      pragma Assert (not Entry_Seq_Active);

      Saved_Seq := Active_Seq;
      Active_Seq := Empty_Sequence;
      Start_Sequence (Active_Seq);
      Entry_Seq_Active := True;
   end Start_Entry_Code_Sequence;

   -----------------------------
   -- End_Entry_Code_Sequence --
   -----------------------------

   procedure End_Entry_Code_Sequence is
   begin
      Prepend (Active_Seq, Saved_Seq);
      Active_Seq := Saved_Seq;
      Entry_Seq_Active := False;
   end End_Entry_Code_Sequence;

   ------------
   -- Method --
   ------------

   function Method (Class : Class_Id; Name : Name_Id) return Method_Id is
      Mthd : Method_Id := First_Method (Class);

   begin
      while Mthd /= Null_Method loop
         if JVM.Info.Name (Mthd) = Name then
            return Mthd;
         end if;
         Mthd := Next_Method (Mthd);
      end loop;

      if Superclass (Class) = Null_Class then
         return Null_Method;
      else
         return Method (Superclass (Class), Name);
      end if;
   end Method;

   ------------
   -- Method --
   ------------

   function Method (Class : Class_Id; Name : String) return Method_Id is
   begin
      return Method (Class, J_String.Name (Name));
   end Method;

   ------------
   -- Method --
   ------------

   function Method
     (Class   : Class_Id;
      Name    : Name_Id;
      Result  : Type_Id;
      Param_0 : Type_Id := Null_Type;
      Param_1 : Type_Id := Null_Type)
      return    Method_Id
   is
      Mthd  : Method_Id := First_Method (Class);
      Param : Local_Var_Id;

   begin
      while Mthd /= Null_Method loop
         if JVM.Info.Name (Mthd) = Name
           and then Result_Type (Mthd) = Result
         then
            Param := First_Local_Var (Mthd);

            --  If searching for a parameterless method and Mthd
            --  has no parameters, then return the method.

            if Param_0 = Null_Type then
               if Param = Null_Local_Var or else not Is_Param (Param) then
                  return Mthd;
               end if;

            elsif Param /= Null_Local_Var
              and then Is_Param (Param)
              and then Type_Of (Param) = Param_0
            then
               Param := Next_Local_Var (Param);

               --  If searching for a one-parameter method and Mthd has
               --  exactly one parameter whose type matches Param_0, then
               --  return the method.

               if Param_1 = Null_Type then
                  if Param = Null_Local_Var or else not Is_Param (Param) then
                     return Mthd;
                  end if;

               --  If searching for a two-parameter method and Mthd has
               --  a second parameter, with no successor parameter, whose
               --  type matches Param_1, then return the method.

               elsif Param /= Null_Local_Var
                 and then Is_Param (Param)
                 and then Type_Of (Param) = Param_1
                 and then (Next_Local_Var (Param) = Null_Local_Var
                            or else not Is_Param (Next_Local_Var (Param)))
               then
                  return Mthd;
               end if;
            end if;
         end if;

         Mthd := Next_Method (Mthd);
      end loop;

      if Superclass (Class) = Null_Class then
         return Null_Method;
      else
         return Method (Superclass (Class), Name, Result, Param_0, Param_1);
      end if;
   end Method;

   ------------
   -- Method --
   ------------

   function Method
     (Class   : Class_Id;
      Name    : String;
      Result  : Type_Id;
      Param_0 : Type_Id := Null_Type;
      Param_1 : Type_Id := Null_Type)
      return    Method_Id
   is
   begin
      return Method (Class, J_String.Name (Name), Result, Param_0, Param_1);
   end Method;

   --------------
   -- Class_Of --
   --------------

   function Class_Of (Method : Method_Id) return Class_Id is
   begin
      return Class (Method);
   end Class_Of;

   -------------------
   -- Parent_Method --
   -------------------

   function Parent_Method (Method : Method_Id) return Method_Id is
   begin
      return JVM.Info.Parent_Method (Method);
   end Parent_Method;

   --------------
   -- Class_Of --
   --------------

   function Is_Completed (Method : Method_Id) return Boolean is
   begin
      --  If Is_Open return False, then the method has been fully
      --  generated and closed (note that method's are initially
      --  marked as open, even though Open_Method has not been
      --  called on the method).

      return not Is_Open (Method);
   end Is_Completed;

   ----------------------------
   -- Set_Current_Source_Loc --
   ----------------------------

   procedure Set_Current_Source_Loc (Sloc : Source_Ptr) is
   begin
      Gen_Label (New_Label, Sloc);
   end Set_Current_Source_Loc;

   -------------------
   -- New_Local_Var --
   -------------------

   function New_Local_Var
     (Method : Method_Id;
      Name   : Name_Id;
      Ltype  : Type_Id)
      return   Local_Var_Id
   is
      Local : constant Local_Var_Id := New_Local_Var;

   begin
      pragma Assert (Is_Open (Class (Method)) and then Is_Open (Method));

      Set_Name          (Local, Name);
      Set_Method        (Local, Method);
      Set_Local_Index   (Local, Next_Local_Index (Method));
      Set_Variable_Type (Local, Ltype);
      Set_Is_Param      (Local, False);

      Add_Local_Var     (Method, Local);

      if Debug then
         Print ("      ");
         Print (JVM.Info.Name (Ltype));
         Print (" ");
         Print (Name);
         Print_Line (";");
      end if;

      return Local;
   end New_Local_Var;

   -------------------
   -- New_Local_Var --
   -------------------

   function New_Local_Var
     (Name  : Name_Id;
      Ltype : Type_Id)
      return  Local_Var_Id
   is
   begin
      return New_Local_Var (Current_Method, Name, Ltype);
   end New_Local_Var;

   -------------------
   -- New_Local_Var --
   -------------------

   function New_Local_Var
     (Name  : String;
      Ltype : Type_Id)
      return  Local_Var_Id
   is
   begin
      return New_Local_Var (Current_Method, J_String.Name (Name), Ltype);
   end New_Local_Var;

   ----------------------
   --  First_Local_Var --
   ----------------------

   function First_Local_Var (Method : Method_Id) return Local_Var_Id is
   begin
      return JVM.Info.First_Local_Var (Method);
   end First_Local_Var;

   ---------------------
   --  Next_Local_Var --
   ---------------------

   function Next_Local_Var (Local : Local_Var_Id) return Local_Var_Id is
   begin
      return JVM.Info.Next_Local_Var (Local);
   end Next_Local_Var;

   ----------
   -- Name --
   ----------

   function Name (Local : Local_Var_Id) return Name_Id is
   begin
      return JVM.Info.Name (Local);
   end Name;

   ----------------
   -- This_Local --
   ----------------

   function This_Local (Method : Method_Id) return Local_Var_Id is
   begin
      pragma Assert (not Is_Static (Method));

      return First_Local_Var (Method);
   end This_Local;

   ---------------
   -- Local_Var --
   ---------------

   function Local_Var (Method : Method_Id; Name : Name_Id) return Local_Var_Id
   is
      Local : Local_Var_Id := First_Local_Var (Method);

   begin
      while Local /= Null_Local_Var loop
         if JVM.Info.Name (Local) = Name then
            return Local;
         end if;
         Local := Next_Local_Var (Local);
      end loop;

      pragma Assert (False);
      raise Program_Error;
   end Local_Var;

   ---------------
   -- Local_Var --
   ---------------

   function Local_Var (Method : Method_Id; Name : String) return Local_Var_Id
   is
   begin
      return Local_Var (Method, J_String.Name (Name));
   end Local_Var;

   -------------
   -- Type_Of --
   -------------

   function Type_Of (Local : Local_Var_Id) return Type_Id is
   begin
      return Variable_Type (Local);
   end Type_Of;

   ---------------
   -- Method_Of --
   ---------------

   function Method_Of (Local : Local_Var_Id) return Method_Id is
   begin
      return Method (Local);
   end Method_Of;

   -------------------------
   -- New_String_Constant --
   -------------------------

   function New_String_Constant (Str : String_Id) return String_Const_Id is
   begin
      pragma Assert (Active_Class /= Null_Class);

      return String_Const_Id (String_Item (Active_Class, Str));
   end New_String_Constant;


   --------------------------------------------------------
   -- Operations for generating constant pool references --
   --------------------------------------------------------

   function New_Class_Ref (Class : Class_Id) return Pool_Id;
   --  Creates a constant pool item with tag CONSTANT_Class
   --  denoting Class and associates it with the current
   --  active class.

   function New_Array_Ref (Array_Typ : Type_Id) return Pool_Id;
   --  Creates a constant pool item with tag CONSTANT_Class
   --  associated with the current active class, representing
   --  the class of an array type.

   function New_Field_Ref (Field : Field_Id) return Pool_Id;
   --  Creates a constant pool item with tag CONSTANT_Fieldref
   --  associated with the current active class, representing
   --  a reference to Field.

   function New_Method_Ref (Method : Method_Id) return Pool_Id;
   --  Creates a constant pool item with tag CONSTANT_Methodref
   --  associated with the current active class, representing
   --  a reference to Method.

   -------------------
   -- New_Class_Ref --
   -------------------

   function New_Class_Ref (Class : Class_Id) return Pool_Id is
   begin
      return Class_Item (Active_Class, Class);
   end New_Class_Ref;

   -------------------
   -- New_Array_Ref --
   -------------------

   function New_Array_Ref (Array_Typ : Type_Id) return Pool_Id is
   begin
      return Array_Item (Active_Class, Array_Typ);
   end New_Array_Ref;

   -------------------
   -- New_Field_Ref --
   -------------------

   function New_Field_Ref (Field : Field_Id) return Pool_Id is
   begin
      return Field_Item (Active_Class, Field);
   end New_Field_Ref;

   --------------------
   -- New_Method_Ref --
   --------------------

   function New_Method_Ref (Method : Method_Id) return Pool_Id is
   begin
      return Method_Item (Active_Class, Method);
   end New_Method_Ref;


   -----------------------------------------------------------------
   -- Operations for generating Java Virtual Machine instructions --
   -----------------------------------------------------------------

   procedure Gen_Instr (Instr : JVM.Code.Instruction);
   --  Appends Instr as a new instruction in the currently active
   --  code sequence of. Emits a symbolic representation of the
   --  instruction if Debug is true.

   procedure Gen_Instr (Op : Operation);
   --  Generates an instruction with opcode Op in the currently active
   --  code sequence. Op must indicate a one-byte opcode (i.e., needs
   --  no operands).

   procedure Gen_Instr (Op : Operation; Sint : Int_16);
   --  Generates an instruction with opcode Op and operand Sint in the
   --  currently active code sequence. Op must be either Bipush or Sipush.

   procedure Gen_Instr (Op : Operation; Local : Local_Var_Id);
   --  Generates an instruction with opcode Op and operand Local in the
   --  currently active code sequence. Op must be an instruction with
   --  a local variable index as its single operand (xload, xstore, ret).

   procedure Gen_Instr (Op : Operation; Pool_Item : Pool_Id);
   --  Generates an instruction with opcode Op and operand Pool_Item in
   --  the currently active code sequence. Op must be an instruction with
   --  a constant pool reference as its single operand.

   procedure Gen_Instr (Op : Operation; Target : Label_Id);
   --  Generates an instruction with opcode Op and branch label Target in
   --  the currently active code sequence. Op must be a branch instruction.

   procedure Gen_Instr (Op : Operation; Subroutine : Subroutine_Id);
   --  Generates an instruction with opcode Op and target Subroutine in
   --  the currently active code sequence. Op must be either Jsr or Jsr_W.

   procedure Gen_Newarray (Elmt_Type : Array_Type);
   --  Generates a Newarray instruction for an array with the given element
   --  type in the currently active code sequence.

   procedure Gen_Multianewarray (Arr_CP : Pool_Id; Dimensions : Pos_8);
   --  Generates a Multianewarray instruction for the array type denoted
   --  by Arr_CP, using the specified number of Dimensions, in the currently
   --  active code sequence.

   procedure Push (Operand_Type : Type_Id);
   --  Push the given operand type on the active method's stack

   procedure Pop (Count : Stack_Range := 1);
   --  Pop the given number of types off of the active method's stack

   function Check_Top (T : Type_Id) return Boolean;
   --  Check that T is compatible with the top-of-stack type

   function Check_Top (Kind : JVM_Type_Kind) return Boolean;
   --  Check that Kind is compatible with the top-of-stack type

   function Check_Next (T : Type_Id) return Boolean;
   --  Check that T is compatible with the next to top-of-stack type

   function Check_Next (Kind : JVM_Type_Kind) return Boolean;
   --  Check that Kind is compatible with the next to top-of-stack type

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Instr : JVM.Code.Instruction) is
   begin
      pragma Assert (Active_Method /= Null_Method);

      if Debug and then First (Active_Seq) = Null_Instr then
         Print_Line ("   // begin");
      end if;

      Append (Active_Seq, Instr);

      if Debug then
         if Instr.Op /= Nop or else Instr.Label_Def = Null_Label then
            Print ("         ");
            if Instr.Op = Jump then
               Print ("GOTO");
            elsif Instr.Op = Newobject then
               Print ("NEW");
            elsif Instr.Op = Vreturn then
               Print ("RETURN");
            else
               Print (Operation'Image (Instr.Op));
            end if;
         end if;

         --  Print any operands of the instruction

         case Instr.Op is
            when Nop =>
               if Instr.Label_Def /= Null_Label then
                  Print ("      " & Label_Number (Instr.Label_Def)'Img & "$:");
               end if;

               if Instr.Line_Number /= No_Location then
                  Print ("  " & Source_Ptr'Image (Instr.Line_Number));
               end if;

            when Bipush | Sipush =>
               Print (Instr.Sint'Img);

            when Newarray =>
               Print (" ");
               Print (Instr.Element_Type'Img);

            when Iload  | Lload  | Fload  | Dload  | Aload  |
                 Istore | Lstore | Fstore | Dstore | Astore |
                 Ret =>
               Print (" ");
               Print (Name (Instr.Local));

            when Iinc =>
               Print (" ");
               Print (Name (Instr.Inc_Local));
               Print (Instr.Increment'Img);

            when Ifeq      | Ifne      | Iflt      | Ifge      | Ifgt | Ifle |
                 If_Icmpeq | If_Icmpne | If_Icmplt | If_Icmpge |
                 If_Icmpgt | If_Icmple | If_Acmpeq | If_Acmpne |
                 Ifnull    | Ifnonnull | Jump      | Goto_W    |
                 Jsr       | Jsr_W     =>
               Print (Label_Number (Instr.Target)'Img & "$");

            when Ldc | Ldc_W | Ldc2_W =>
               if Pool_Item_Tag (Instr.Pool_Item) = CONSTANT_String then
                  Print (" """);
                  Print (Str (Pool_String (Instr.Pool_Item)));
                  Print ("""");
               else
                  Print (" <some value>");
                  --  ??? Print (Value (Instr.Pool_Item));
               end if;

            when Getstatic | Putstatic =>
               Print (" ");
               Print (Name (Class (Ref_Field (Instr.Pool_Item))));
               Print (".");
               Print (Name (Ref_Field (Instr.Pool_Item)));

            when Getfield | Putfield =>
               Print (" ");
               Print (Name (Ref_Field (Instr.Pool_Item)));

            when Invokevirtual | Invokespecial |
                 Invokestatic  | Invokeinterface =>
               Print (" ");
               Print (Name (Class (Ref_Method (Instr.Pool_Item))));
               Print (".");
               Print (Name (Ref_Method (Instr.Pool_Item)));

            when Newobject =>
               Print (" ");
               Print (Name (Ref_Class (Instr.Pool_Item)));

            when Anewarray =>
               Print (" ");
               Print (Name (Ref_Class_Type (Instr.Pool_Item)));
               Print ("[]");

            when Checkcast | Instanceof =>
               Print (" ");
               Print (Name (Ref_Class_Type (Instr.Pool_Item)));

            when Multianewarray =>
               Print (" ");
               Print
                 (Name (Element_Type (Ref_Class_Type (Instr.Array_Class))));
               Print ("[]");
               Print (", Dimensions => ");
               Print (Instr.Dimensions'Img);

            when Tableswitch | Lookupswitch =>
               Print (" ");
               Print (Label_Number (Instr.Default_Label)'Img & "$");
               --  Print list of switch pairs... ???

            when others =>
               null;
         end case;
         Print_Line;
      end if;
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Op : Operation) is
      Instr : JVM.Code.Instruction (Op);

   begin
      Gen_Instr (Instr);
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Op : Operation; Sint : Int_16) is
      Sint_Instr : JVM.Code.Instruction (Op);

   begin
      pragma Assert (Op = Bipush or else Op = Sipush);

      Sint_Instr.Sint := Sint;
      Gen_Instr (Sint_Instr);
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Op : Operation; Local : Local_Var_Id) is
      Local_Instr : JVM.Code.Instruction (Op);

   begin
      Local_Instr.Local := Local;
      Gen_Instr (Local_Instr);
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Op : Operation; Pool_Item : Pool_Id) is
      Pool_Ref_Instr : JVM.Code.Instruction (Op);

   begin
      Pool_Ref_Instr.Pool_Item := Pool_Item;
      Gen_Instr (Pool_Ref_Instr);
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Op : Operation; Target : Label_Id) is
      Branch_Instr : JVM.Code.Instruction (Op);

   begin
      Branch_Instr.Target := Target;
      Gen_Instr (Branch_Instr);
      Set_Is_Targeted (Target);
   end Gen_Instr;

   ---------------
   -- Gen_Instr --
   ---------------

   procedure Gen_Instr (Op : Operation; Subroutine : Subroutine_Id) is
      JSR_Instr : JVM.Code.Instruction (Op);

   begin
      JSR_Instr.Target := Subroutine_Label (Subroutine);
      Gen_Instr (JSR_Instr);
      Set_Is_Targeted (JSR_Instr.Target);
   end Gen_Instr;

   ------------------
   -- Gen_Newarray --
   ------------------

   procedure Gen_Newarray (Elmt_Type : Array_Type) is
      Newarray_Instr : JVM.Code.Instruction (Newarray);

   begin
      Newarray_Instr.Element_Type := Elmt_Type;
      Gen_Instr (Newarray_Instr);
   end Gen_Newarray;

   ------------------------
   -- Gen_Multianewarray --
   ------------------------

   procedure Gen_Multianewarray (Arr_CP : Pool_Id; Dimensions : Pos_8) is
      Multianewarray_Instr : JVM.Code.Instruction (Multianewarray);

   begin
      Multianewarray_Instr.Array_Class := Arr_CP;
      Multianewarray_Instr.Dimensions := Dimensions;
      Gen_Instr (Multianewarray_Instr);
   end Gen_Multianewarray;

   ----------
   -- Push --
   ----------

   procedure Push (Operand_Type : Type_Id) is
   begin
      Push (Active_Stack, Operand_Type);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Count : Stack_Range := 1) is
   begin
      Pop (Active_Stack, Count);
   end Pop;

   ---------------
   -- Check_Top --
   ---------------

   function Check_Top (T : Type_Id) return Boolean is
      Stack_Type  : constant Type_Id       := Top (Active_Stack);
      T_Kind      : constant JVM_Type_Kind := Type_Kind (T);
      Super_Class : Class_Id;

   begin
      if Stack_Type = Any_Ref_Type then
         return T_Kind in Array_Kind .. Class_Kind;

      --  Relax the matching for Boolean, Byte, Char, and Short

      elsif T_Kind = Int_Kind then
         return Check_Top (T_Kind);

      --  Allow the pushed class to match any ancestor class

      elsif T_Kind = Class_Kind then

         if Stack_Type = T then
            return True;
         else
            Super_Class := Class_Of_Type (Stack_Type);
            while Super_Class /= Class_Of_Type (T)
              and then Super_Class /= Java_Lang_Object
            loop
               Super_Class := Superclass (Super_Class);
            end loop;
            return Super_Class = Class_Of_Type (T);
         end if;

      elsif T_Kind = Array_Kind then
         return Type_Kind (Stack_Type) = Array_Kind
           and then Dimensions (T) = Dimensions (Stack_Type)
           and then Element_Type (T) = Element_Type (Stack_Type);

      --  Otherwise require exact type match

      else
         return Stack_Type = T;
      end if;
   end Check_Top;

   ---------------
   -- Check_Top --
   ---------------

   function Check_Top (Kind : JVM_Type_Kind) return Boolean is
      Top_Type : constant Type_Id       := Top (Active_Stack);
      Top_Kind : constant JVM_Type_Kind := Type_Kind (Top_Type);

   begin
      if Top_Type = Any_Ref_Type then
         return Top_Kind in Array_Kind .. Class_Kind;
      else
         return Top_Kind = Kind;
      end if;
   end Check_Top;

   ----------------
   -- Check_Next --
   ----------------

   function Check_Next (T : Type_Id) return Boolean is
      Stack_Type  : constant Type_Id := Next_To_Top (Active_Stack);
      T_Kind      : constant JVM_Type_Kind := Type_Kind (T);
      Super_Class : Class_Id;

   begin
      if Stack_Type = Any_Ref_Type then
         return T_Kind in Array_Kind .. Class_Kind;

      --  Relax the matching for Boolean, Byte, Char, and Short

      elsif T_Kind = Int_Kind then
         return Check_Top (T_Kind);

      --  Allow the pushed class to match any ancestor class

      elsif T_Kind = Class_Kind then

         if Stack_Type = T then
            return True;
         else
            Super_Class := Class_Of_Type (Stack_Type);
            while Super_Class /= Class_Of_Type (T)
              and then Super_Class /= Java_Lang_Object
            loop
               Super_Class := Superclass (Super_Class);
            end loop;
            return Super_Class = Class_Of_Type (T);
         end if;

      --  Otherwise require exact type match

      else
         return Stack_Type = T;
      end if;
   end Check_Next;

   ----------------
   -- Check_Next --
   ----------------

   function Check_Next (Kind : JVM_Type_Kind) return Boolean is
   begin
      return Type_Kind (Next_To_Top (Active_Stack)) = Kind;
   end Check_Next;

   -------------------------------------------
   -- Operations for pushing literal values --
   -------------------------------------------

   -------------------
   -- Gen_Push_Null --
   -------------------

   procedure Gen_Push_Null is
   begin
      Gen_Instr (Aconst_Null);
      Push (Any_Ref_Type);
   end Gen_Push_Null;

   ------------------
   -- Gen_Push_Int --
   ------------------

   procedure Gen_Push_Int (Value : Uint) is
      Int_Value : constant Int := UI_To_Int (Value);

   begin
      if Int_Value in -1 .. 5 then
         case Int_Value is
            when -1 =>   Gen_Instr (Iconst_M1);
            when  0 =>   Gen_Instr (Iconst_0);
            when  1 =>   Gen_Instr (Iconst_1);
            when  2 =>   Gen_Instr (Iconst_2);
            when  3 =>   Gen_Instr (Iconst_3);
            when  4 =>   Gen_Instr (Iconst_4);
            when  5 =>   Gen_Instr (Iconst_5);
            when others =>   null;
         end case;

      elsif Int_Value in -128 .. +127 then
         Gen_Instr (Bipush, Int_16 (Int_Value));

      elsif Int_Value
        in Int (Short_Integer'First) .. Int (Short_Integer'Last)
      then
         Gen_Instr (Sipush, Int_16 (Int_Value));

      --  A constant pool entry is required to represent the value

      else
         Gen_Instr (Ldc_W, Integer_Item (Active_Class, Value));
      end if;

      Push (Int_Type);
   end Gen_Push_Int;

   -------------------
   -- Gen_Push_Long --
   -------------------

   procedure Gen_Push_Long (Value : Uint) is
   begin
      if Value = Uint_0 then
         Gen_Instr (Lconst_0);
      elsif Value = Uint_1 then
         Gen_Instr (Lconst_1);
      else
         Gen_Instr (Ldc2_W, Long_Item (Active_Class, Value));
      end if;

      Push (Long_Type);
   end Gen_Push_Long;

   --------------------
   -- Gen_Push_Float --
   --------------------

   procedure Gen_Push_Float (Value : Ureal) is
   begin
      if Value = Ureal_0 then
         Gen_Instr (Fconst_0);
      elsif Value = Ureal_1 then
         Gen_Instr (Fconst_1);
      elsif Value = Ureal_2 then
         Gen_Instr (Fconst_2);
      else
         Gen_Instr (Ldc_W, Float_Item (Active_Class, Value));
      end if;

      Push (Float_Type);
   end Gen_Push_Float;

   ---------------------
   -- Gen_Push_Double --
   ---------------------

   procedure Gen_Push_Double (Value : Ureal) is
   begin
      if Value = Ureal_0 then
         Gen_Instr (Dconst_0);
      elsif Value = Ureal_1 then
         Gen_Instr (Dconst_1);
      else
         Gen_Instr (Ldc2_W, Double_Item (Active_Class, Value));
      end if;

      Push (Double_Type);
   end Gen_Push_Double;

   ---------------------------
   -- Gen_Push_String_Const --
   ---------------------------

   procedure Gen_Push_String_Const (Str : String_Const_Id) is
   begin
      pragma Assert (Parent_Class (Pool_Id (Str)) = Active_Class);

      Gen_Instr (Ldc_W, Pool_Id (Str));
      Push (String_Type);
   end Gen_Push_String_Const;

   ---------------------------
   -- Gen_Push_String_Const --
   ---------------------------

   procedure Gen_Push_String_Const (S : String) is
   begin
      Gen_Instr (Ldc_W, String_Item (Active_Class, Str_Id (S)));
      Push (String_Type);
   end Gen_Push_String_Const;

   ---------------------------------------------------------
   -- Operations for loading and updating local variables --
   ---------------------------------------------------------

   --------------------
   -- Gen_Load_Local --
   --------------------

   procedure Gen_Load_Local (Local : Local_Var_Id) is
   begin
      pragma Assert (Method (Local) = Active_Method);

      case Type_Kind (Type_Of (Local)) is
         when Void_Kind =>
            pragma Assert (False);
            null;
         when Boolean_Kind | Byte_Kind | Char_Kind | Short_Kind | Int_Kind =>
            Gen_Instr (Iload, Local);
         when Long_Kind =>
            Gen_Instr (Lload, Local);
         when Float_Kind =>
            Gen_Instr (Fload, Local);
         when Double_Kind =>
            Gen_Instr (Dload, Local);
         when Array_Kind =>
            Gen_Instr (Aload, Local);
         when Class_Kind =>
            Gen_Instr (Aload, Local);
         when Return_Addr_Kind =>
            Gen_Instr (Aload, Local);
      end case;

      Push (Type_Of (Local));
   end Gen_Load_Local;

   ---------------------
   -- Gen_Store_Local --
   ---------------------

   procedure Gen_Store_Local (Local : Local_Var_Id) is
   begin
      pragma Assert (Method (Local) = Active_Method
                      and then Check_Top (Type_Of (Local)));

      case Type_Kind (Type_Of (Local)) is
         when Void_Kind =>
            pragma Assert (False);
            null;
         when Boolean_Kind | Byte_Kind | Char_Kind | Short_Kind | Int_Kind =>
            Gen_Instr (Istore, Local);
         when Long_Kind =>
            Gen_Instr (Lstore, Local);
         when Float_Kind =>
            Gen_Instr (Fstore, Local);
         when Double_Kind =>
            Gen_Instr (Dstore, Local);
         when Array_Kind =>
            Gen_Instr (Astore, Local);
         when Class_Kind =>
            Gen_Instr (Astore, Local);
         when Return_Addr_Kind =>
            Gen_Instr (Astore, Local);
      end case;

      Pop;
   end Gen_Store_Local;

   --------------------------------------------------------
   -- Operations for loading and updating array elements --
   --------------------------------------------------------

   ----------------------------
   -- Gen_Load_Array_Element --
   ----------------------------

   procedure Gen_Load_Array_Element is
      Elt_Type : constant Type_Id :=
        Element_Type (Next_To_Top (Active_Stack));

   begin
      pragma Assert (Check_Top (JVM.Int_Type)
            and then Check_Next (Array_Kind));

      case Type_Kind (Elt_Type) is
         when Void_Kind =>
            pragma Assert (False);
            null;

         when Boolean_Kind | Byte_Kind | Char_Kind | Short_Kind =>
            pragma Assert (False);
            null;

         when Int_Kind =>
            if Elt_Type = Int_Type then
               Gen_Instr (Iaload);
            elsif Elt_Type = Boolean_Type
              or else Elt_Type = Byte_Type
            then
               Gen_Instr (Baload);
            elsif Elt_Type = Short_Type then
               Gen_Instr (Saload);
            elsif Elt_Type = Char_Type then
               Gen_Instr (Caload);
            else
               pragma Assert (False);
               null;
            end if;

         when Long_Kind =>
            Gen_Instr (Laload);

         when Float_Kind =>
            Gen_Instr (Faload);

         when Double_Kind =>
            Gen_Instr (Daload);

         when Array_Kind | Class_Kind =>
            Gen_Instr (Aaload);

         when Return_Addr_Kind =>
            pragma Assert (False);
            null;
      end case;

      Pop (2);  --  Pop the index and array reference
      Push (Elt_Type);
   end Gen_Load_Array_Element;

   -----------------------------------
   -- Gen_Load_Subarray_Reference --
   -----------------------------------

   procedure Gen_Load_Subarray_Reference is
   begin
      pragma Assert (Check_Top (JVM.Int_Type)
            and then Check_Next (Array_Kind));

      Gen_Instr (Aaload);

      Pop;  --  Pop the index but leave the array reference type
   end Gen_Load_Subarray_Reference;

   -----------------------------
   -- Gen_Store_Array_Element --
   -----------------------------

   procedure Gen_Store_Array_Element is
      Elt_Type : Type_Id;

   begin
      Pop (1);  --  Pop the element value

      pragma Assert (Check_Top (JVM.Int_Type)
            and then Check_Next (Array_Kind));

      Elt_Type := Element_Type (Next_To_Top (Active_Stack));

      case Type_Kind (Elt_Type) is
         when Void_Kind =>
            pragma Assert (False);
            null;

         when Boolean_Kind | Byte_Kind | Char_Kind | Short_Kind =>
            pragma Assert (False);
            null;

         when Int_Kind =>
            if Elt_Type = Int_Type then
               Gen_Instr (Iastore);
            elsif Elt_Type = Boolean_Type
              or else Elt_Type = Byte_Type
            then
               Gen_Instr (Bastore);
            elsif Elt_Type = Short_Type then
               Gen_Instr (Sastore);
            elsif Elt_Type = Char_Type then
               Gen_Instr (Castore);
            else
               pragma Assert (False);
               null;
            end if;

         when Long_Kind =>
            Gen_Instr (Lastore);

         when Float_Kind =>
            Gen_Instr (Fastore);

         when Double_Kind =>
            Gen_Instr (Dastore);

         when Array_Kind | Class_Kind =>
            Gen_Instr (Aastore);

         when Return_Addr_Kind =>
            pragma Assert (False);
            null;
      end case;

      Pop (2);  --  Pop the index and array reference
   end Gen_Store_Array_Element;

   --------------------------------------------------------
   -- Operations for accessing and updating class fields --
   --------------------------------------------------------

   -------------------
   -- Gen_Get_Field --
   -------------------

   procedure Gen_Get_Field (Field : Field_Id) is
   begin
      if Is_Static (Field) then
         Gen_Get_Static_Field (Field);
      else
         Gen_Get_Object_Field (Field);
      end if;
   end Gen_Get_Field;

   -------------------
   -- Gen_Put_Field --
   -------------------

   procedure Gen_Put_Field (Field : Field_Id) is
   begin
      if Is_Static (Field) then
         Gen_Put_Static_Field (Field);
      else
         Gen_Put_Object_Field (Field);
      end if;
   end Gen_Put_Field;

   --------------------------
   -- Gen_Get_Static_Field --
   --------------------------

   procedure Gen_Get_Static_Field (Field : Field_Id) is
   begin
      pragma Assert (Is_Static (Field));

      Gen_Instr (Getstatic, New_Field_Ref (Field));
      Push (Type_Of (Field));
   end Gen_Get_Static_Field;

   --------------------------
   -- Gen_Put_Static_Field --
   --------------------------

   procedure Gen_Put_Static_Field (Field : Field_Id) is
   begin
      pragma Assert (Is_Static (Field));
      pragma Assert (Check_Top (Type_Of (Field)));

      Gen_Instr (Putstatic, New_Field_Ref (Field));
      Pop;  --  Pop the field
   end Gen_Put_Static_Field;

   --------------------------
   -- Gen_Get_Object_Field --
   --------------------------

   procedure Gen_Get_Object_Field (Field : Field_Id) is
   begin
      pragma Assert (not Is_Static (Field));
      pragma Assert (Check_Top (Type_Of (Class (Field))));

      Gen_Instr (Getfield, New_Field_Ref (Field));
      Pop;  --  Pop the instance reference
      Push (Type_Of (Field));
   end Gen_Get_Object_Field;

   --------------------------
   -- Gen_Put_Object_Field --
   --------------------------

   procedure Gen_Put_Object_Field (Field : Field_Id) is
   begin
      pragma Assert (not Is_Static (Field));
      pragma Assert (Check_Top (Type_Of (Field))
                      and then Check_Next (Type_Of (Class (Field))));

      Gen_Instr (Putfield, New_Field_Ref (Field));
      Pop (2);  --  Pop the field and the instance reference
   end Gen_Put_Object_Field;

   ------------------------------------------------
   -- Operations for creating objects and arrays --
   ------------------------------------------------

   --------------------
   -- Gen_New_Object --
   --------------------

   procedure Gen_New_Object (Class : Class_Id) is
   begin
      pragma Assert (not Is_Abstract (Class) and not Is_Interface (Class));

      Gen_Instr (Newobject, New_Class_Ref (Class));

      Push (Type_Of (Class));
   end Gen_New_Object;

   ------------------------
   -- Gen_Default_Object --
   ------------------------

   procedure Gen_Default_Object (Class : Class_Id) is
   begin
      Gen_New_Object (Class);
      Gen_Duplicate;
      Gen_Invoke_Special (Default_Constructor (Class));
   end Gen_Default_Object;

   -------------------
   -- Gen_New_Array --
   -------------------

   procedure Gen_New_Array (Array_Type : Type_Id) is
      Elmt_Type : constant Type_Id := Element_Type (Array_Type);

   begin
      pragma Assert (Check_Top (Int_Type));

      if Dimensions (Array_Type) = 1 then
         if Elmt_Type = Int_Type then
            Gen_Newarray (T_Int);

         elsif Type_Kind (Elmt_Type) = Class_Kind then
            Gen_Instr (Anewarray, New_Class_Ref (Class (Elmt_Type)));

         elsif Elmt_Type = Long_Type then
            Gen_Newarray (T_Long);

         elsif Elmt_Type = Float_Type then
            Gen_Newarray (T_Float);

         elsif Elmt_Type = Double_Type then
            Gen_Newarray (T_Double);

         elsif Elmt_Type = Byte_Type then
            Gen_Newarray (T_Byte);

         elsif Elmt_Type = Boolean_Type then
            Gen_Newarray (T_Boolean);

         elsif Elmt_Type = Short_Type then
            Gen_Newarray (T_Short);

         elsif Elmt_Type = Char_Type then
            Gen_Newarray (T_Char);

         elsif Type_Kind (Elmt_Type) = Array_Kind then
            Gen_Instr (Anewarray, New_Array_Ref (Elmt_Type));

         else
            pragma Assert (False);
            null;
         end if;

      --  Generate an allocation of a multidimensional array
      --  NOTE: Not yet implemented (use Gen_New_Multiarray).

      else
         pragma Assert (False);
         null;
      end if;

      --  Pop the array length value(s)

      Pop (Stack_Range (Dimensions (Array_Type)));
      Push (Array_Type);
   end Gen_New_Array;

   ------------------------
   -- Gen_New_Multiarray --
   ------------------------

   procedure Gen_New_Multiarray (Array_Type : Type_Id) is
      Dimension_Count : Pos_8;

   begin
      pragma Assert (Check_Top (Int_Type));

      if Dimensions (Array_Type) = 1 then
         pragma Assert
           (Type_Kind (Element_Type (Array_Type)) = Array_Kind
             and then Dimensions (Element_Type (Array_Type)) = 1);

         Dimension_Count := 2;

      else
         Dimension_Count := Dimensions (Array_Type);
      end if;

      Gen_Multianewarray (New_Array_Ref (Array_Type), Dimension_Count);

      --  Pop the array length value(s)

      Pop (Stack_Range (Dimension_Count));
      Push (Array_Type);
   end Gen_New_Multiarray;

   ----------------------
   -- Gen_Array_Length --
   ----------------------

   procedure Gen_Array_Length is
   begin
      pragma Assert (Check_Top (Array_Kind));

      Gen_Instr (Arraylength);

      Pop;  --  Pop the array reference
      Push (Int_Type);
   end Gen_Array_Length;

   ---------------------------
   -- Arithmetic operations --
   ---------------------------

   -------------
   -- Gen_Add --
   -------------

   procedure Gen_Add is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (Iadd);
         when Long_Kind =>
            Gen_Instr (Ladd);
         when Float_Kind =>
            Gen_Instr (Fadd);
         when Double_Kind =>
            Gen_Instr (Dadd);
         when others =>
            pragma Assert (False);
            null;
      end case;

      Pop;  --  Pop the top-of-stack type (leaving the second operand type)
   end Gen_Add;

   -------------
   -- Gen_Sub --
   -------------

   procedure Gen_Sub is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (Isub);
         when Long_Kind =>
            Gen_Instr (Lsub);
         when Float_Kind =>
            Gen_Instr (Fsub);
         when Double_Kind =>
            Gen_Instr (Dsub);
         when others =>
            pragma Assert (False);
            null;
      end case;

      Pop;  --  Pop the top-of-stack type (leaving the second operand type)
   end Gen_Sub;

   -------------
   -- Gen_Mul --
   -------------

   procedure Gen_Mul is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (Imul);
         when Long_Kind =>
            Gen_Instr (Lmul);
         when Float_Kind =>
            Gen_Instr (Fmul);
         when Double_Kind =>
            Gen_Instr (Dmul);
         when others =>
            pragma Assert (False);
            null;
      end case;

      Pop;  --  Pop the top-of-stack type (leaving the second operand type)
   end Gen_Mul;

   -------------
   -- Gen_Div --
   -------------

   procedure Gen_Div is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (Idiv);
         when Long_Kind =>
            Gen_Instr (Ldiv);
         when Float_Kind =>
            Gen_Instr (Fdiv);
         when Double_Kind =>
            Gen_Instr (Ddiv);
         when others =>
            pragma Assert (False);
            null;
      end case;

      Pop;  --  Pop the top-of-stack type (leaving the second operand type)
   end Gen_Div;

   -------------
   -- Gen_Rem --
   -------------

   procedure Gen_Rem is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (Irem);
         when Long_Kind =>
            Gen_Instr (Lrem);
         when Float_Kind =>
            Gen_Instr (Frem);
         when Double_Kind =>
            Gen_Instr (Drem);
         when others =>
            pragma Assert (False);
            null;
      end case;

      Pop;  --  Pop the top of stack (use the first operand type as the result)
   end Gen_Rem;

   -------------
   -- Gen_Neg --
   -------------

   procedure Gen_Neg is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (Ineg);
         when Long_Kind =>
            Gen_Instr (Lneg);
         when Float_Kind =>
            Gen_Instr (Fneg);
         when Double_Kind =>
            Gen_Instr (Dneg);
         when others =>
            pragma Assert (False);
            null;
      end case;
   end Gen_Neg;

   --------------------
   -- Gen_Incr_Local --
   --------------------

   procedure Gen_Incr_Local (Local : Local_Var_Id; Value : Uint) is
   begin
      if Type_Kind (Type_Of (Local)) = Int_Kind
        and then Value >= Int (Int_16'First)
        and then Value <= Int (Int_16'Last)
      then
         declare
            Iinc_Instr : JVM.Code.Instruction (Iinc);

         begin
            Iinc_Instr.Inc_Local := Local;
            Iinc_Instr.Increment := Int_16 (UI_To_Int (Value));
            Gen_Instr (Iinc_Instr);
         end;

      --  The type is not Int or the increment is out of range of Int_16,
      --  so expand to an add/store.

      else
         Gen_Load_Local (Local);
         Gen_Push_Int (Value);
         Gen_Conversion (Type_Of (Local));
         Gen_Add;
         Gen_Store_Local (Local);
      end if;
   end Gen_Incr_Local;

   ----------------------------------
   -- Logical and shift operations --
   ----------------------------------

   -------------
   -- Gen_And --
   -------------

   procedure Gen_And is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (Iand);
         when Long_Kind =>
            Gen_Instr (Land);
         when others =>
            pragma Assert (False);
            null;
      end case;

      Pop;  --  Pop the top of stack (use the first operand type as the result)
   end Gen_And;

   ------------
   -- Gen_Or --
   ------------

   procedure Gen_Or is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (Ior);
         when Long_Kind =>
            Gen_Instr (Lor);
         when others =>
            pragma Assert (False);
            null;
      end case;

      Pop;  --  Pop the top of stack (use the first operand type as the result)
   end Gen_Or;

   -------------
   -- Gen_Xor --
   -------------

   procedure Gen_Xor is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (Ixor);
         when Long_Kind =>
            Gen_Instr (Lxor);
         when others =>
            pragma Assert (False);
            null;
      end case;

      Pop;  --  Pop the top of stack (use the first operand type as the result)
   end Gen_Xor;

   -------------
   -- Gen_Not --
   -------------

   procedure Gen_Not is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (Iconst_M1);
            Gen_Instr (Ixor);
         when Long_Kind =>
            Gen_Instr (Iconst_M1);
            Push (Int_Type);
            Gen_Conversion (Long_Type);
            Gen_Instr (Lxor);
            Pop;
         when others =>
            pragma Assert (False);
            null;
      end case;
   end Gen_Not;

   --------------------
   -- Gen_Shift_Left --
   --------------------

   procedure Gen_Shift_Left (Size : Uint) is
   begin
      Pop;  --  Pop the top of stack (use the first operand type as the result)

      --  If the nominal size of the value is less than the size of
      --  its containing type, then we have to mask of the high bits
      --  off the left-shifted value.

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (Ishl);

            if Size < 32 then
               Gen_Push_Int (Uint_2 ** Size - 1);
               Gen_And;
            end if;

         when Long_Kind =>
            Gen_Instr (Lshl);

            if Size < 64 then
               Gen_Push_Int (Uint_2 ** Size - 1);
               Gen_And;
            end if;

         when others =>
            pragma Assert (False);
            null;
      end case;
   end Gen_Shift_Left;

   --------------------------------
   -- Gen_Shift_Right_Arithmetic --
   --------------------------------

   procedure Gen_Shift_Right_Arithmetic (Size : Uint) is
   begin
      --  For sizes less than the JVM type size, it's necessary
      --  to shift the low-order bits to the leftmost end of the
      --  integer and then shift back, followed by a mask, to ensure
      --  proper sign extension.

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            if Size = 32 then
               Pop;  --  Use the first operand type as the result
               Gen_Instr (Ishr);

            else
               pragma Assert (Size < 32);

               --  Compute the bit count for the right shift. We make
               --  this one less than it should be, because of the
               --  edge case where the shift is the full size of the
               --  value and we need sign extension. The JVM will treat
               --  that case as a zero bit shift, so unfortunately
               --  we have to compensate by shifting two times (we
               --  shift the remaining bit at the end before masking).

               Gen_Push_Int (32 - Size - 1);
               Gen_Add;
               Gen_Swap;

               --  Shift the low-order Size bits to the high-order
               --  end of the integer.

               Gen_Push_Int (32 - Size);
               Gen_Instr (Ishl);
               Pop;  --  Pop the shift count

               --  Now shift right to get sign extension

               Gen_Swap;
               Gen_Instr (Ishr);
               Pop;  --  Pop the shift count

               --  Shift the final bit (see explanation above)

               Gen_Push_Int (Uint_1);
               Gen_Instr (Ishr);
               Pop;  --  Pop the shift count

               --  Finally mask off the high order bits to get the result

               Gen_Push_Int (Uint_2 ** Size - 1);
               Gen_And;
            end if;

         when Long_Kind =>
            if Size = 64 then
               Pop;  --  Use the first operand type as the result
               Gen_Instr (Lshr);

            else
               pragma Assert (Size < 64);

               --  Compute the bit count for the right shift. We make
               --  this one less than it should be, because of the
               --  edge case where the shift is the full size of the
               --  value and we need sign extension. The JVM will treat
               --  that case as a zero bit shift, so unfortunately
               --  we have to compensate by shifting two times (we
               --  shift the remaining bit at the end before masking).

               Gen_Push_Int (64 - Size - 1);
               Gen_Add;
               Gen_Swap;

               --  Shift the low-order Size bits to the high-order
               --  end of the integer.

               Gen_Push_Int (64 - Size);
               Gen_Instr (Lshl);
               Pop;  --  Pop the shift count

               --  Now shift right to get sign extension

               Gen_Swap;
               Gen_Instr (Lshr);
               Pop;  --  Pop the shift count

               --  Shift the final bit (see explanation above)

               Gen_Push_Int (Uint_1);
               Gen_Instr (Ishr);
               Pop;  --  Pop the shift count

               --  Finally mask off the high order bits to get the result

               Gen_Push_Int (Uint_2 ** Size - 1);
               Gen_And;
            end if;

         when others =>
            pragma Assert (False);
            null;
      end case;
   end Gen_Shift_Right_Arithmetic;

   -----------------------------
   -- Gen_Shift_Right_Logical --
   -----------------------------

   procedure Gen_Shift_Right_Logical is
   begin
      Pop;  --  Pop the top of stack (use the first operand type as the result)

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (Iushr);
         when Long_Kind =>
            Gen_Instr (Lushr);
         when others =>
            pragma Assert (False);
            null;
      end case;
   end Gen_Shift_Right_Logical;

   ---------------------
   -- Gen_Rotate_Left --
   ---------------------

   procedure Gen_Rotate_Left (Size : Uint) is
      Count_Tmp : constant Local_Var_Id := New_Local_Var ("_rotcnt", Int_Type);
      Prim_Type : JVM_Type_Kind := Type_Kind (Next_To_Top (Active_Stack));

   begin
      --  If Size is less than the primitive type size, then
      --  we need to normalize the shift count value to the
      --  range 0 .. Size - 1, in case it's bigger than Size
      --  (which would otherwise lead to incorrect rotation
      --  results).

      if (Prim_Type = Int_Kind and then Size < 32)
        or else (Prim_Type = Long_Kind and then Size < 64)
      then
         Gen_Push_Int (Size);
         Gen_Rem;
      end if;

      --  Save the normalized count and push an extra copy of the operand value

      Gen_Store_Local (Count_Tmp);
      Gen_Duplicate;

      --  Left shift the right portion of the value into position
      --  (Gen_Shift_Left will take care of any needed masking).

      Gen_Load_Local (Count_Tmp);
      Gen_Shift_Left (Size);

      --  Swap the left-shifted value with the original value and
      --  push the shift count.

      Gen_Swap;
      Gen_Load_Local (Count_Tmp);

      --  Compute the proper right shift (size of value minus shift count)

      Gen_Push_Int (Size);
      Gen_Sub;
      Gen_Neg;

      --  Do the right shift and combine with the left-shifted portion

      Gen_Shift_Right_Logical;
      Gen_Or;
   end Gen_Rotate_Left;

   ----------------------
   -- Gen_Rotate_Right --
   ----------------------

   procedure Gen_Rotate_Right (Size : Uint) is
      Count_Tmp : constant Local_Var_Id := New_Local_Var ("_rotcnt", Int_Type);
      Prim_Type : JVM_Type_Kind := Type_Kind (Next_To_Top (Active_Stack));

   begin
      --  If Size is less than the primitive type size, then
      --  we need to normalize the shift count value to the
      --  range 0 .. Size - 1, in case it's bigger than Size
      --  (which would otherwise lead to incorrect rotation
      --  results).

      if (Prim_Type = Int_Kind and then Size < 32)
        or else (Prim_Type = Long_Kind and then Size < 64)
      then
         Gen_Push_Int (Size);
         Gen_Rem;
      end if;

      --  Save the normalized count and push an extra copy of the operand value

      Gen_Store_Local (Count_Tmp);
      Gen_Duplicate;

      --  Right shift the left portion of the value into position

      Gen_Load_Local (Count_Tmp);
      Gen_Shift_Right_Logical;

      --  Swap the right-shifted value with the original value and
      --  push the shift count.

      Gen_Swap;
      Gen_Load_Local (Count_Tmp);

      --  Compute the proper left shift (size of value minus shift count)

      Gen_Push_Int (Size);
      Gen_Sub;
      Gen_Neg;

      --  Do the left shift and combine with the right-shifted portion
      --  (Gen_Shift_Left will take care of any needed masking).

      Gen_Shift_Left (Size);
      Gen_Or;
   end Gen_Rotate_Right;

   -----------------------------------
   -- Numeric conversion operations --
   -----------------------------------

   procedure Convert_To_Integer;
   --  Generates a conversion of the top (integer) stack item to Int

   procedure Convert_To_Long;
   --  Generates a conversion of the top (integer) stack item to Long

   procedure Convert_To_Float;
   --  Generates a conversion of the top (floating-pt) stack item to Long

   procedure Convert_To_Double;
   --  Generates a conversion of the top (floating-pt) stack item to Double

   ------------------------
   -- Convert_To_Integer --
   ------------------------

   procedure Convert_To_Integer is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            null;  --  Already integer, nothing to do
         when Long_Kind =>
            Gen_Instr (L2i);
         when Float_Kind =>
            Gen_Instr (F2i);
         when Double_Kind =>
            Gen_Instr (D2i);
         when others =>
            pragma Assert (False);
            null;
      end case;
      Pop;
      Push (Int_Type);
   end Convert_To_Integer;

   ---------------------
   -- Convert_To_Long --
   ---------------------

   procedure Convert_To_Long is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (I2l);
         when Long_Kind =>
            null;  --  Already long, nothing to do
         when Float_Kind =>
            Gen_Instr (F2l);
         when Double_Kind =>
            Gen_Instr (D2l);
         when others =>
            pragma Assert (False);
            null;
      end case;
      Pop;
      Push (Long_Type);
   end Convert_To_Long;

   ----------------------
   -- Convert_To_Float --
   ----------------------

   procedure Convert_To_Float is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (I2f);
         when Long_Kind =>
            Gen_Instr (L2f);
         when Float_Kind =>
            null;  --  Already float, nothing to do
         when Double_Kind =>
            Gen_Instr (D2f);
         when others =>
            pragma Assert (False);
            null;
      end case;
      Pop;
      Push (Float_Type);
   end Convert_To_Float;

   -----------------------
   -- Convert_To_Double --
   -----------------------

   procedure Convert_To_Double is
   begin
      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (I2d);
            Pop;
            Push (Double_Type);
         when Long_Kind =>
            Gen_Instr (L2d);
            Pop;
            Push (Long_Type);
         when Float_Kind =>
            Gen_Instr (F2d);
            Pop;
            Push (Float_Type);
         when Double_Kind =>
            null;  --  Already double, nothing to do (or raise error???)
         when others =>
            pragma Assert (False);
            null;
      end case;
      Pop;
      Push (Double_Type);
   end Convert_To_Double;

   --------------------
   -- Gen_Conversion --
   --------------------

   procedure Gen_Conversion (Target_Type : Type_Id) is
      Super_Class  : Class_Id;
      Target_Class : Class_Id;

   begin
      case Type_Kind (Target_Type) is
         when Int_Kind | Boolean_Kind | Byte_Kind | Char_Kind | Short_Kind =>
            Convert_To_Integer;

         when Long_Kind =>
            Convert_To_Long;

         when Float_Kind =>
            Convert_To_Float;

         when Double_Kind =>
            Convert_To_Double;

         when Array_Kind =>
            Gen_Check_Cast (Target_Type);

         when Class_Kind =>
            pragma Assert (Is_Reference_Type (Top (Active_Stack)));

            if Type_Kind (Top (Active_Stack)) = Array_Kind then
               pragma Assert (Class_Of_Type (Target_Type) = Java_Lang_Object);

               null;  -- No action required for converting arrays to Object

            elsif Top (Active_Stack) /= Target_Type
              and then Top (Active_Stack) /= Any_Ref_Type
            then
               Super_Class := Class_Of_Type (Top (Active_Stack));
               Target_Class := Class_Of_Type (Target_Type);

               while Super_Class /= Target_Class
                 and then Super_Class /= Java_Lang_Object
               loop
                  Super_Class := Superclass (Super_Class);
               end loop;

               --  If Target_Type's class is not an ancestor of
               --  the top-of-stack type's class, then this must
               --  a downward conversion, so apply a checkcast.

               if Super_Class /= Target_Class then
                  Gen_Instr (Checkcast, New_Class_Ref (Target_Class));
               end if;

               Pop;
               Push (Target_Type);
            end if;

         when others =>
            pragma Assert (False);
            null;
      end case;
   end Gen_Conversion;

   ---------------------------------
   -- Branch and label generation --
   ---------------------------------

   procedure Gen_Compare_Vs_Zero (Jtype : JVM_Type_Kind);
   --  Generates code to compare a numeric value against zero.
   --  If Jtype is Int_Kind, then does nothing. Intended for
   --  use by Gen_Branch_* routines to handle conditional
   --  branches for Long, Float, and Double values.

   ---------------
   -- New_Label --
   ---------------

   function New_Label return Label_Id is
      Label : constant Label_Id := JVM.Info.New_Label;

   begin
      Set_Method      (Label, Active_Method);
      Set_Location    (Label, Null_Instr);
      Set_Is_Targeted (Label, False);

      Add_Label (Active_Method, Label);

      return Label;
   end New_Label;

   ---------------
   -- Gen_Label --
   ---------------

   procedure Gen_Label
     (Label       : Label_Id;
      Line_Number : Source_Ptr := No_Location)
   is
      Label_Instr : JVM.Code.Instruction (Nop);
      Instr       : JVM.Code.Instruction;

   begin
      pragma Assert (Method (Label) = Active_Method
                      and then Location (Label) = Null_Instr
                      and then
                        (not Stack_Checking or else Is_Empty (Active_Stack)));

      --  If we want to create a new label for a line number, and if the
      --  previous instruction was already a label associated with a line
      --  number, just replace the line number

      if Last (Active_Seq) /= Null_Instr
        and then Line_Number /= No_Location
        and then Get (Last (Active_Seq)).Op = Nop
        and then Get (Last (Active_Seq)).Line_Number /= No_Location
      then
         Instr := Get (Last (Active_Seq));
         Instr.Line_Number := Line_Number;
         Put (Last (Active_Seq), Instr);

      --  Else create a new label

      else

         Label_Instr.Label_Def := Label;
         Label_Instr.Line_Number := Line_Number;
         Gen_Instr (Label_Instr);
         Set_Location (Label, Last (Active_Seq));
      end if;
   end Gen_Label;

   --------------------
   -- Gen_Comparison --
   --------------------

   procedure Gen_Comparison is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>  --  Note: inefficient code at the moment...
            Gen_Conversion (Long_Type);
            Gen_Swap;
            Gen_Conversion (Long_Type);
            Gen_Swap;
            Gen_Instr (Lcmp);
         when Long_Kind =>
            Gen_Instr (Lcmp);
         when Float_Kind =>
            Gen_Instr (Fcmpl);  --  should it be fcmpl or fcmpg???
         when Double_Kind =>
            Gen_Instr (Dcmpl);  --  should it be dcmpl or dcmpg???
         when others =>
            pragma Assert (False);
            null;
      end case;

      Pop (2);  --  Pop both operands
      Push (Int_Type);
   end Gen_Comparison;

   -------------------------
   -- Gen_Compare_Vs_Zero --
   -------------------------

   procedure Gen_Compare_Vs_Zero (Jtype : JVM_Type_Kind) is
   begin
      case Jtype is
         when Int_Kind =>
            null;
         when Long_Kind =>
            Gen_Push_Long (Uint_0);
            Gen_Comparison;
         when Float_Kind =>
            Gen_Push_Float (Ureal_0);
            Gen_Comparison;
         when Double_Kind =>
            Gen_Push_Double (Ureal_0);
            Gen_Comparison;
         when others =>
            pragma Assert (False);
            null;
      end case;
   end Gen_Compare_Vs_Zero;

   ----------------------
   -- Gen_Branch_Equal --
   ----------------------

   procedure Gen_Branch_Equal (Label : Label_Id) is
   begin
      Gen_Compare_Vs_Zero (Type_Kind (Top (Active_Stack)));

      Gen_Instr (Ifeq, Label);
      Pop;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_Equal;

   --------------------------
   -- Gen_Branch_Not_Equal --
   --------------------------

   procedure Gen_Branch_Not_Equal (Label : Label_Id) is
   begin
      Gen_Compare_Vs_Zero (Type_Kind (Top (Active_Stack)));

      Gen_Instr (Ifne, Label);
      Pop;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_Not_Equal;

   ---------------------
   -- Gen_Branch_Less --
   ---------------------

   procedure Gen_Branch_Less (Label : Label_Id) is
   begin
      Gen_Compare_Vs_Zero (Type_Kind (Top (Active_Stack)));

      Gen_Instr (Iflt, Label);
      Pop;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_Less;

   ---------------------------
   -- Gen_Branch_Less_Equal --
   ---------------------------

   procedure Gen_Branch_Less_Equal (Label : Label_Id) is
   begin
      Gen_Compare_Vs_Zero (Type_Kind (Top (Active_Stack)));

      Gen_Instr (Ifle, Label);
      Pop;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_Less_Equal;

   ------------------------
   -- Gen_Branch_Greater --
   ------------------------

   procedure Gen_Branch_Greater (Label : Label_Id) is
   begin
      Gen_Compare_Vs_Zero (Type_Kind (Top (Active_Stack)));

      Gen_Instr (Ifgt, Label);
      Pop;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_Greater;

   ------------------------------
   -- Gen_Branch_Greater_Equal --
   ------------------------------

   procedure Gen_Branch_Greater_Equal (Label : Label_Id) is
   begin
      Gen_Compare_Vs_Zero (Type_Kind (Top (Active_Stack)));

      Gen_Instr (Ifge, Label);
      Pop;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_Greater_Equal;

   ------------------------------
   -- Gen_Compare_Branch_Equal --
   ------------------------------

   procedure Gen_Compare_Branch_Equal (Label : Label_Id) is
   begin
      pragma Assert
        (Type_Kind (Top (Active_Stack)) =
         Type_Kind (Next_To_Top (Active_Stack))
           or else (Is_Reference_Type (Top (Active_Stack))
                    and then Is_Reference_Type (Next_To_Top (Active_Stack))));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (If_Icmpeq, Label);
            Pop (2);  --  Pop both operands
         when Long_Kind | Float_Kind | Double_Kind =>
            Gen_Comparison;
            Gen_Branch_Equal (Label);
         when Array_Kind | Class_Kind =>
            Gen_Instr (If_Acmpeq, Label);
            Pop (2);  --  Pop both operands
         when others =>
            pragma Assert (False);
            null;
      end case;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Compare_Branch_Equal;

   ----------------------------------
   -- Gen_Compare_Branch_Not_Equal --
   ----------------------------------

   procedure Gen_Compare_Branch_Not_Equal (Label : Label_Id) is
   begin
      pragma Assert
        (Type_Kind (Top (Active_Stack)) =
         Type_Kind (Next_To_Top (Active_Stack))
           or else (Is_Reference_Type (Top (Active_Stack))
                    and then Is_Reference_Type (Next_To_Top (Active_Stack))));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (If_Icmpne, Label);
            Pop (2);  --  Pop both operands
         when Long_Kind | Float_Kind | Double_Kind =>
            Gen_Comparison;
            Gen_Branch_Not_Equal (Label);
         when Array_Kind | Class_Kind =>
            Gen_Instr (If_Acmpne, Label);
            Pop (2);  --  Pop both operands
         when others =>
            pragma Assert (False);
            null;
      end case;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Compare_Branch_Not_Equal;

   -----------------------------
   -- Gen_Compare_Branch_Less --
   -----------------------------

   procedure Gen_Compare_Branch_Less (Label : Label_Id) is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (If_Icmplt, Label);
            Pop (2);  --  Pop both operands
         when Long_Kind | Float_Kind | Double_Kind =>
            Gen_Comparison;
            Gen_Branch_Less (Label);
         when Class_Kind =>
            --  Occurs with System.Address comparisons (e.g., in the run-time).
            --  Just compare for equal since there's not much else we can do.

            Gen_Instr (If_Acmpeq, Label);
            Pop (2);  --  Pop both operands
         when others =>
            pragma Assert (False);
            null;
      end case;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Compare_Branch_Less;

   -----------------------------------
   -- Gen_Compare_Branch_Less_Equal --
   -----------------------------------

   procedure Gen_Compare_Branch_Less_Equal (Label : Label_Id) is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (If_Icmple, Label);
            Pop (2);  --  Pop both operands
         when Long_Kind | Float_Kind | Double_Kind =>
            Gen_Comparison;
            Gen_Branch_Less_Equal (Label);
         when Class_Kind =>
            --  Occurs with System.Address comparisons (e.g., in the run-time).
            --  Just compare for equal since there's not much else we can do.

            Gen_Instr (If_Acmpeq, Label);
            Pop (2);  --  Pop both operands
         when others =>
            pragma Assert (False);
            null;
      end case;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Compare_Branch_Less_Equal;

   --------------------------------
   -- Gen_Compare_Branch_Greater --
   --------------------------------

   procedure Gen_Compare_Branch_Greater (Label : Label_Id) is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (If_Icmpgt, Label);
            Pop (2);  --  Pop both operands
         when Long_Kind | Float_Kind | Double_Kind =>
            Gen_Comparison;
            Gen_Branch_Greater (Label);
         when Class_Kind =>
            --  Occurs with System.Address comparisons (e.g., in the run-time).
            --  Just compare for equal since there's not much else we can do.

            Gen_Instr (If_Acmpeq, Label);
            Pop (2);  --  Pop both operands
         when others =>
            pragma Assert (False);
            null;
      end case;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Compare_Branch_Greater;

   --------------------------------------
   -- Gen_Compare_Branch_Greater_Equal --
   --------------------------------------

   procedure Gen_Compare_Branch_Greater_Equal (Label : Label_Id) is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      case Type_Kind (Top (Active_Stack)) is
         when Int_Kind =>
            Gen_Instr (If_Icmpge, Label);
            Pop (2);  --  Pop both operands
         when Long_Kind | Float_Kind | Double_Kind =>
            Gen_Comparison;
            Gen_Branch_Greater_Equal (Label);
         when Class_Kind =>
            --  Occurs with System.Address comparisons (e.g., in the run-time).
            --  Just compare for equal since there's not much else we can do.

            Gen_Instr (If_Acmpeq, Label);
            Pop (2);  --  Pop both operands
         when others =>
            pragma Assert (False);
            null;
      end case;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Compare_Branch_Greater_Equal;

   ------------------------
   -- Gen_Branch_If_Null --
   ------------------------

   procedure Gen_Branch_If_Null (Label : Label_Id) is
   begin
      pragma Assert (Check_Top (Array_Kind) or else Check_Top (Class_Kind));

      Gen_Instr (Ifnull, Label);
      Pop;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_If_Null;

   ------------------------------------
   -- Gen_Compare_Branch_If_Not_Null --
   ------------------------------------

   procedure Gen_Branch_If_Not_Null (Label : Label_Id) is
   begin
      pragma Assert (Check_Top (Array_Kind) or else Check_Top (Class_Kind));

      Gen_Instr (Ifnonnull, Label);
      Pop;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end Gen_Branch_If_Not_Null;

   --------------
   -- Gen_Goto --
   --------------

   procedure Gen_Goto (Label : Label_Id) is
   begin
      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));

      Gen_Instr (Jump, Label);
   end Gen_Goto;

   ------------------------
   -- Set_Stack_Checking --
   ------------------------

   procedure Set_Stack_Checking (Enable : Boolean) is
   begin
      pragma Assert (Stack_Checking /= Enable);

      Stack_Checking := Enable;
   end Set_Stack_Checking;

   -----------------------------
   -- Suppress_Stack_Checking --
   -----------------------------

   procedure Suppress_Stack_Checking (Check_State : out Boolean) is
   begin
      Check_State := Stack_Checking;
      Stack_Checking := False;
   end Suppress_Stack_Checking;

   ----------------------------
   -- Restore_Stack_Checking --
   ----------------------------

   procedure Restore_Stack_Checking (Check_State : Boolean) is
   begin
      Stack_Checking := Check_State;
   end Restore_Stack_Checking;

   ----------------
   -- Mark_Stack --
   ----------------

   procedure Mark_Stack is
   begin
      pragma Assert (not Stack_Checking);

      Mark (Active_Stack);
      Stack_Marked := True;
   end Mark_Stack;

   -------------------
   -- Release_Stack --
   -------------------

   procedure Release_Stack is
   begin
      pragma Assert (not Stack_Checking and then Stack_Marked);

      Release (Active_Stack);
      Stack_Marked := Marked (Active_Stack);
   end Release_Stack;

   -----------------
   -- Reset_Stack --
   -----------------

   procedure Reset_Stack is
   begin
      Reset (Active_Stack);
      Stack_Checking := True;
   end Reset_Stack;

   ---------------
   -- Push_Type --
   ---------------

   procedure Push_Type (JVM_Type : Type_Id) is
   begin
      Push (JVM_Type);
   end Push_Type;

   --------------
   -- Pop_Type --
   --------------

   procedure Pop_Type (Count : Positive := 1) is
   begin
      Pop (Stack_Range (Count));
   end Pop_Type;

   --------------
   -- Top_Type --
   --------------

   function Top_Type return Type_Id is
   begin
      return Top (Active_Stack);
   end Top_Type;


   -----------------------------------
   -- Subroutine-related operations --
   -----------------------------------

   --------------------
   -- New_Subroutine --
   --------------------

   function New_Subroutine return Subroutine_Id is
      Subroutine : constant Subroutine_Id := JVM.Info.New_Subroutine;

   begin
      pragma Assert (Active_Method /= Null_Method);

      Add_Subroutine (Active_Method, Subroutine);
      Set_Method (Subroutine, Active_Method);
      Set_Is_Open (Subroutine);

      return Subroutine;
   end New_Subroutine;

   ---------------------
   -- Open_Subroutine --
   ---------------------

   procedure Open_Subroutine (Subroutine : Subroutine_Id) is
      Code_Seq   : Code_Sequence;
      Subr_Stack : Op_Stack_Id;

   begin
      pragma Assert (Is_Open (Subroutine));

      pragma Assert (Active_Method /= Null_Method
            and then Method (Subroutine) = Active_Method
            and then Active_Subroutine (Active_Method) = Null_Subroutine);

      Set_Active_Subroutine (Active_Method, Subroutine);
      Active_Subr := Subroutine;

      --  Save the active method's code sequence and operand stack

      Set_Code (Active_Method, Active_Seq);
      Set_Op_Stack (Active_Method, Active_Stack);

      Start_Sequence (Code_Seq);
      Active_Seq := Code_Seq;

      Subr_Stack := New_Stack (200);
      Active_Stack := Subr_Stack;

      --  Create a label for the subroutine entry point

      Set_Subroutine_Label (Subroutine, New_Label);

      --  Upon entering a subroutine, the return address is on top of
      --  the stack, so we have to push the return address type here
      --  to ensure consistency. The subroutine should store the address
      --  away in a local variable on entry.

      Push (Subr_Stack, Retaddr_Type);
   end Open_Subroutine;

   ----------------------
   -- Close_Subroutine --
   ----------------------

   procedure Close_Subroutine is
   begin
      pragma Assert (Active_Subr /= Null_Subroutine
                      and then Is_Empty (Active_Stack));

      --  Record the maximum word depth of the stack before it gets
      --  deallocated.

      Set_Max_Stack_Depth (Active_Subr, Max_Depth (Op_Stack (Active_Subr)));
      Free_Stack (Active_Stack);

      Set_Code (Active_Subr, Active_Seq);

      Set_Active_Subroutine (Active_Method, Null_Subroutine);

      --  Restore the active code sequence and operand stack to those
      --  of the active method.

      Set_Is_Open (Active_Subr, False);

      Active_Seq := Method_Code (Active_Method);
      Active_Stack := Op_Stack (Active_Method);

      --  Update the maximum recorded stack depth for the active method
      --  if the subroutine's max depth is greater than the current max
      --  for the method. (Note: we currently require that the stack be
      --  empty when generating a subroutine call (Jsr), which allows
      --  us to update the maximum depth at this point rather than on
      --  subroutine calls. If we relax that restriction, then the stack
      --  will need to be incremented by a called subroutine's max stack
      --  depth at the point of generating a Jsr instruction. ???

      if Max_Stack_Depth (Active_Subr) > Max_Depth (Active_Stack) then
         Set_Max_Depth (Active_Stack, Max_Stack_Depth (Active_Subr));
      end if;

      Active_Subr := Null_Subroutine;
   end Close_Subroutine;

   -------------
   -- Gen_JSR --
   -------------

   procedure Gen_JSR (Subroutine : Subroutine_Id) is
   begin
      pragma Assert (not Is_Open (Subroutine));
      pragma Assert (Is_Empty (Active_Stack));

      Gen_Instr (Jsr, Subroutine);
   end Gen_JSR;

   --------------------------------
   -- Gen_Save_Subroutine_Return --
   --------------------------------

   procedure Gen_Save_Subroutine_Return (Local : Local_Var_Id) is
   begin
      pragma Assert (Is_Empty (Active_Stack));

      Push (Retaddr_Type);
      Gen_Store_Local (Local);
   end Gen_Save_Subroutine_Return;

   ---------------------------
   -- Gen_Subroutine_Return --
   ---------------------------

   procedure Gen_Subroutine_Return (Local : Local_Var_Id) is
   begin
      pragma Assert (Is_Empty (Active_Stack));

      Gen_Instr (Ret, Local);
   end Gen_Subroutine_Return;

   ---------------------------
   -- Case table generation --
   ---------------------------

   ------------------------
   -- Start_Switch_Table --
   ------------------------

   procedure Start_Switch_Table (Default : Label_Id) is
      New_Switch_Stmt : JVM.Code.Instruction (Tableswitch);

   begin
      pragma Assert (Active_Switch_Stmt.Op = Xxxunusedxxx);
      pragma Assert (Check_Top (Int_Type));

      Active_Switch_Stmt := New_Switch_Stmt;
      Active_Switch_Stmt.Default_Label := Default;
      Start_Switch_List (Active_Switch_Stmt.Switch_Pairs);
   end Start_Switch_Table;

   ---------------------
   -- Add_Switch_Pair --
   ---------------------

   procedure Add_Switch_Pair
     (Match_Low  : Uint;
      Match_High : Uint;
      Target     : Label_Id)
   is
   begin
      pragma Assert (Active_Switch_Stmt.Op = Tableswitch
             or else Active_Switch_Stmt.Op = Lookupswitch);

      for Value in UI_To_Int (Match_Low) .. UI_To_Int (Match_High) loop
         Add_Switch_Pair
           (Active_Switch_Stmt.Switch_Pairs, Int_32 (Value), Target);
      end loop;
   end Add_Switch_Pair;

   ----------------------
   -- End_Switch_Table --
   ----------------------

   procedure End_Switch_Table is
   begin
      pragma Assert (Active_Switch_Stmt.Op = Tableswitch
             or else Active_Switch_Stmt.Op = Lookupswitch);

      Gen_Instr (Active_Switch_Stmt);

      Active_Switch_Stmt := (Op => Xxxunusedxxx, Next => Null_Instr);

      Pop;

      pragma Assert (not Stack_Checking or else Is_Empty (Active_Stack));
   end End_Switch_Table;

   -------------------------
   -- Cancel_Switch_Table --
   -------------------------

   procedure Cancel_Switch_Table is
   begin
      pragma Assert (Active_Switch_Stmt.Op = Tableswitch
             or else Active_Switch_Stmt.Op = Lookupswitch);

      Active_Switch_Stmt := (Op => Xxxunusedxxx, Next => Null_Instr);
   end Cancel_Switch_Table;


   ------------------------------
   -- Method return operations --
   ------------------------------

   -----------------------
   -- Gen_Method_Return --
   -----------------------

   procedure Gen_Method_Return is
   begin
      if Result_Type (Active_Method) = Void_Type then
         pragma Assert (Is_Empty (Active_Stack));

         Gen_Instr (Vreturn);

      else
         pragma Assert (Check_Top (Result_Type (Active_Method)));

         case Type_Kind (Result_Type (Active_Method)) is
            when Int_Kind =>
               Gen_Instr (Ireturn);
            when Long_Kind =>
               Gen_Instr (Lreturn);
            when Float_Kind =>
               Gen_Instr (Freturn);
            when Double_Kind =>
               Gen_Instr (Dreturn);
            when Array_Kind | Class_Kind =>
               Gen_Instr (Areturn);
            when others =>
               pragma Assert (False);
               null;
         end case;

         Pop;  --  Pop the method result for consistency
      end if;
   end Gen_Method_Return;

   ----------------------------------
   -- Method invocation operations --
   ----------------------------------

   -----------------------
   -- Gen_Invoke_Method --
   -----------------------

   procedure Gen_Invoke_Method (Method : Method_Id) is
   begin
      if Is_Static (Method) then
         Gen_Invoke_Static (Method);

      elsif Name (Method) = Name ("<init>") then
         Gen_Invoke_Special (Method);

      elsif Is_Interface (Class_Of (Method)) then
         Gen_Invoke_Interface (Method);

      else
         Gen_Invoke_Virtual (Method);
      end if;
   end Gen_Invoke_Method;

   ------------------------
   -- Gen_Invoke_Virtual --
   ------------------------

   procedure Gen_Invoke_Virtual (Method : Method_Id) is
      Param : Local_Var_Id := First_Local_Var (Method);

   begin
      pragma Assert (not Is_Static (Method));

      --  NOTE: Ideally we would check that the type of each
      --  operand on the stack matches the corresponding formal
      --  type, but that would mean traversing the formals in
      --  reverse order, which is a bit of a pain...

      while Param /= Null_Local_Var and then Is_Param (Param) loop
         Pop;
         Param := Next_Local_Var (Param);
      end loop;

      Gen_Instr (Invokevirtual, New_Method_Ref (Method));

      if Result_Type (Method) /= Void_Type then
         Push (Result_Type (Method));
      end if;
   end Gen_Invoke_Virtual;

   ------------------------
   -- Gen_Invoke_Special --
   ------------------------

   procedure Gen_Invoke_Special (Method : Method_Id) is
      Param : Local_Var_Id := First_Local_Var (Method);

   begin
      pragma Assert (not Is_Static (Method));

      --  NOTE: Ideally we would check that the type of each
      --  operand on the stack matches the corresponding formal
      --  type, but that would mean traversing the formals in
      --  reverse order, which is a bit of a pain...

      while Param /= Null_Local_Var and then Is_Param (Param) loop
         Pop;
         Param := Next_Local_Var (Param);
      end loop;

      Gen_Instr (Invokespecial, New_Method_Ref (Method));

      if Result_Type (Method) /= Void_Type then
         Push (Result_Type (Method));
      end if;
   end Gen_Invoke_Special;

   -----------------------
   -- Gen_Invoke_Static --
   -----------------------

   procedure Gen_Invoke_Static (Method : Method_Id) is
      Param : Local_Var_Id := First_Local_Var (Method);

   begin
      pragma Assert (Is_Static (Method));

      --  NOTE: Ideally we would check that the type of each
      --  operand on the stack matches the corresponding formal
      --  type, but that would mean traversing the formals in
      --  reverse order, which is a bit of a pain...

      while Param /= Null_Local_Var and then Is_Param (Param) loop
         Pop;
         Param := Next_Local_Var (Param);
      end loop;

      Gen_Instr (Invokestatic, New_Method_Ref (Method));

      if Result_Type (Method) /= Void_Type then
         Push (Result_Type (Method));
      end if;
   end Gen_Invoke_Static;

   --------------------------
   -- Gen_Invoke_Interface --
   --------------------------

   procedure Gen_Invoke_Interface (Method : Method_Id) is
      Param : Local_Var_Id := First_Local_Var (Method);

   begin
      pragma Assert (not Is_Static (Method));

      --  NOTE: Ideally we would check that the type of each
      --  operand on the stack matches the corresponding formal
      --  type, but that would mean traversing the formals in
      --  reverse order, which is a bit of a pain...

      while Param /= Null_Local_Var and then Is_Param (Param) loop
         Pop;
         Param := Next_Local_Var (Param);
      end loop;

      Gen_Instr (Invokeinterface, New_Method_Ref (Method));

      if Result_Type (Method) /= Void_Type then
         Push (Result_Type (Method));
      end if;
   end Gen_Invoke_Interface;

   -------------------------------------------------
   -- Miscellaneous stack manipulation operations --
   -------------------------------------------------

   -------------
   -- Gen_Pop --
   -------------

   procedure Gen_Pop (Items : Positive := 1) is
   begin
      for Item in 1 .. Items loop
         if Word_Size (Pop (Active_Stack)) = 1 then
            Gen_Instr (Pop);
         else
            Gen_Instr (Pop2);
         end if;
      end loop;
   end Gen_Pop;

   -------------------
   -- Gen_Duplicate --
   -------------------

   procedure Gen_Duplicate is
   begin
      if Word_Size (Top (Active_Stack)) = 1 then
         Gen_Instr (Dup);
      else
         Gen_Instr (Dup2);
      end if;

      Push (Top (Active_Stack));
   end Gen_Duplicate;

   --------------------------
   -- Gen_Double_Duplicate --
   --------------------------

   procedure Gen_Double_Duplicate is
   begin
      pragma Assert (Type_Kind (Top (Active_Stack)) =
                     Type_Kind (Next_To_Top (Active_Stack)));

      if Word_Size (Top (Active_Stack)) = 1 then
         Gen_Instr (Dup2);
      else
         Gen_Swap;
         Gen_Instr (Dup2_X2);
         Gen_Swap;
         Gen_Instr (Dup2_X2);
      end if;

      Push (Top (Active_Stack));
      Push (Top (Active_Stack));
   end Gen_Double_Duplicate;

   --------------
   -- Gen_Swap --
   --------------

   procedure Gen_Swap is
      Type_1 : constant Type_Id := Pop (Active_Stack);
      Type_2 : constant Type_Id := Pop (Active_Stack);

   begin
      if Word_Size (Type_1) = 1 then
         --  Swap 1-word item on TOS with 1-word item next-to-TOS
         if Word_Size (Type_2) = 1 then
            Gen_Instr (Swap);

         --  Swap 1-word item on TOS with 2-word item next-to-TOS
         else
            Gen_Instr (Dup_X2);
            Gen_Instr (Pop);
         end if;
      else
         --  Swap 2-word item on TOS with 1-word item next-to-TOS
         if Word_Size (Type_2) = 1 then
            Gen_Instr (Dup2_X1);
            Gen_Instr (Pop2);

         --  Swap 2-word item on TOS with 2-word item next-to-TOS
         else
            Gen_Instr (Dup2_X2);
            Gen_Instr (Pop2);
         end if;
      end if;

      --  Push the types back in reverse order

      Push (Type_1);
      Push (Type_2);
   end Gen_Swap;

   ----------------------
   -- Other operations --
   ----------------------

   -------------------------
   -- Gen_Exception_Throw --
   -------------------------

   procedure Gen_Exception_Throw is
   begin
      pragma Assert (Check_Top (Class_Kind));

      Gen_Instr (Athrow);

      Pop;  --  Pop the exception instance reference (for consistency)
   end Gen_Exception_Throw;

   ---------------------------
   -- Gen_Exc_Handler_Entry --
   ---------------------------

   procedure Gen_Exc_Handler_Entry
     (Exc_Class   : Class_Id;
      Start_Lbl   : Label_Id;
      End_Lbl     : Label_Id;
      Handler_Lbl : Label_Id)
   is
      Exc_Pool_Id : Pool_Id;
      New_Handler : Handler_Id;

   begin
      if Exc_Class = Null_Class then
         Exc_Pool_Id := Null_Pool_Item;
      else
         Exc_Pool_Id := New_Class_Ref (Exc_Class);
      end if;

      New_Handler
        := New_Handler_Entry (Exc_Pool_Id, Start_Lbl, End_Lbl, Handler_Lbl);

      if Debug then
         Print (">>> Exception handler entry for ");
         if Exc_Class = Null_Class then
            Print ("<others handler>");
         else
            Print (Name (Exc_Class));
         end if;
         Print_Line;
         Print ("      range" & Label_Number (Start_Lbl)'Img & "$ ..");
         Print (Label_Number (End_Lbl)'Img & "$ handled by ");
         Print (Label_Number (Handler_Lbl)'Img & "$");
         Print_Line;
      end if;

      Append (Active_Handlers, New_Handler);
   end Gen_Exc_Handler_Entry;


   ----------------------
   -- Other operations --
   ----------------------

   --------------------
   -- Gen_Check_Cast --
   --------------------

   procedure Gen_Check_Cast (Class : Class_Id) is
   begin
      pragma Assert (Check_Top (Class_Kind));

      Gen_Instr (Checkcast, New_Class_Ref (Class));

      --  Change the stack type to Class's type

      Pop;
      Push (Type_Of (Class));
   end Gen_Check_Cast;

   --------------------
   -- Gen_Check_Cast --
   --------------------

   procedure Gen_Check_Cast (JVM_Type : Type_Id) is
   begin
      pragma Assert (Check_Top (Class_Kind) or else Check_Top (Array_Kind));

      if Type_Kind (JVM_Type) = Array_Kind then
         Gen_Instr (Checkcast, New_Array_Ref (JVM_Type));
      else
         Gen_Instr (Checkcast, New_Class_Ref (Class_Of_Type (JVM_Type)));
      end if;

      --  Change the stack type to the target type

      Pop;
      Push (JVM_Type);
   end Gen_Check_Cast;

   ---------------------
   -- Gen_Instance_Of --
   ---------------------

   procedure Gen_Instance_Of (Class : Class_Id) is
   begin
      pragma Assert (Check_Top (Class_Kind));

      Gen_Instr (Instanceof, New_Class_Ref (Class));

      Pop;  --  Pop the instance reference
      Push (Int_Type);
   end Gen_Instance_Of;

   -------------
   -- Gen_NOP --
   -------------

   procedure Gen_NOP is
   begin
      Gen_Instr (Nop);
   end Gen_NOP;

   -----------------------
   -- Gen_Monitor_Enter --
   -----------------------

   procedure Gen_Monitor_Enter is
   begin
      pragma Assert (Check_Top (Class_Kind));

      Gen_Instr (Monitorenter);

      Pop;  --  Pop the instance reference
   end Gen_Monitor_Enter;

   ----------------------
   -- Gen_Monitor_Exit --
   ----------------------

   procedure Gen_Monitor_Exit is
   begin
      pragma Assert (Check_Top (Class_Kind));

      Gen_Instr (Monitorexit);

      Pop;  --  Pop the instance reference
   end Gen_Monitor_Exit;

   -----------------
   -- Print_Stack --
   -----------------

   procedure Print_Stack is
   begin
      Print_Stack (Active_Stack);
   end Print_Stack;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Predef_Type   : Type_Id;
      Predef_Class  : Class_Id;
      Predef_Method : Method_Id;

   begin
      Predef_Type := New_Type (Void_Kind);
      pragma Assert (Predef_Type = Void_Type);
      Set_Name      (Void_Type, Name ("void"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Boolean_Type);
      Set_Name      (Boolean_Type, Name ("boolean"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Byte_Type);
      Set_Name      (Byte_Type, Name ("byte"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Char_Type);
      Set_Name      (Char_Type, Name ("char"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Short_Type);
      Set_Name      (Short_Type, Name ("short"));

      Predef_Type := New_Type (Int_Kind);
      pragma Assert (Predef_Type = Int_Type);
      Set_Name      (Int_Type, Name ("int"));

      Predef_Type := New_Type (Long_Kind);
      pragma Assert (Predef_Type = Long_Type);
      Set_Name      (Long_Type, Name ("long"));

      Predef_Type := New_Type (Float_Kind);
      pragma Assert (Predef_Type = Float_Type);
      Set_Name      (Float_Type, Name ("float"));

      Predef_Type := New_Type (Double_Kind);
      pragma Assert (Predef_Type = Double_Type);
      Set_Name      (Double_Type, Name ("double"));

      Predef_Type := New_Type (Return_Addr_Kind);
      pragma Assert (Predef_Type = Retaddr_Type);
      Set_Name      (Retaddr_Type, Name ("jvm_return_address"));

      Predef_Type := New_Type (Class_Kind);
      pragma Assert (Predef_Type = Any_Ref_Type);
      Set_Name      (Any_Ref_Type, Name ("<any_ref>"));

      Predef_Class := New_Class;
      pragma Assert (Predef_Class = Java_Lang_Object);
      Set_Name       (Java_Lang_Object, Name ("Object"));
      Set_Pkg_Name   (Java_Lang_Object, Str_Id ("java.lang"));
      Set_Superclass (Java_Lang_Object, Null_Class);
      Set_Is_Public  (Java_Lang_Object);

      Predef_Type := New_Type (Class_Kind);
      pragma Assert (Predef_Type = Java_Lang_Object_Type);
      Set_Name      (Java_Lang_Object_Type, Name ("Object"));
      Set_Class     (Java_Lang_Object_Type, Java_Lang_Object);

      Predef_Class := New_Class;
      pragma Assert (Predef_Class = Java_Lang_String);
      Set_Name       (Java_Lang_String, Name ("String"));
      Set_Pkg_Name   (Java_Lang_String, Str_Id ("java.lang"));
      Set_Superclass (Java_Lang_String, Java_Lang_Object);
      Set_Is_Public  (Java_Lang_String);

      Predef_Type := New_Type (Class_Kind);
      pragma Assert (Predef_Type = String_Type);
      Set_Name      (String_Type, Name ("String"));
      Set_Class     (String_Type, Java_Lang_String);

      --  Declare the default constructor for java.lang.Object

      Predef_Method
        := New_Method (Java_Lang_Object, Name ("<init>"), Void_Type, False);
   end Initialize;

end JVM;
