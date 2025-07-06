------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J X _ U P L E V                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.19 $
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
with Einfo;    use Einfo;
with JVM;      use JVM;
with JVM.API;  use JVM.API;
with J_String; use J_String;
with Jx_Decl;  use Jx_Decl;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;

package body Jx_Uplev is

   function Load_Up_Level_Link (Parent_Method : Method_Id) return AR_Access;
   --  Generate a load of the static link needed for accessing up-level
   --  variables within Parent_Method. This involves chaining up multiple
   --  activation records when the current method is nested multiple
   --  levels down from Parent_Method.

   function Up_Level_Field_Name (Local : Entity_Id) return Name_Id;
   --  Returns a Name_Id to use for a field in an up-level activation
   --  record object. If Local is declared immediately within a subprogram
   --  then its simple name will be returned, otherwise its fully expanded
   --  name will be used. The full name is only needed when conflicts
   --  with simple names are possible, such as for variables declared
   --  within packages or blocks inside a subprogram.

   function Up_Level_Field_Name (Local : Entity_Id) return Name_Id is
   begin
      if Ekind (Scope (Local)) in Subprogram_Kind then
         return Chars (Local);
      else
         return Name (JVM_Expanded_Name (Local));
      end if;
   end Up_Level_Field_Name;

   ----------------------------
   -- Make_Activation_Record --
   ----------------------------

   procedure Make_Activation_Record
     (Method   : Method_Id;
      Name     : Name_Id;
      Pkg_Name : String_Id := No_String)
   is
      Save_Current_Method : constant Method_Id := Current_Method;
      New_AR              : constant AR_Access := new Active_Rec;

   begin
      pragma Assert (AR_Stack.Empty or else AR_Stack.Top.Method /= Method);

      if AR_Stack.Empty then
         New_AR.Parent := null;
      else
         New_AR.Parent := AR_Stack.Top;
      end if;

      AR_Stack.Push (New_AR);

      New_AR.Method := Method;

      --  Create a class that extends the predefined Ada activation record
      --  class. This class will have fields added to it for variables of
      --  the method that are referenced up-level from nested methods.

      New_AR.AR_Class
        := New_Class (J_String.Name ("__AR_" & Name_String (Name)),
                      Pkg_Name => Pkg_Name,
                      Super    => API_Class (Ada_Activation_Rec));

      --  Open the class file and create its <clinit> and <init> methods.
      --  The class file will be closed upon reaching the end of the
      --  activation record's associated method.

      Begin_Class_File (New_AR.AR_Class);
      Generate_Class_Init_Method (New_AR.AR_Class);
      Generate_Default_Constructor (New_AR.AR_Class);

      --  Temporarily switch to the AR's method and generate code to
      --  allocate the method's AR object and save its reference in
      --  a new local variable of the method.

      Set_Current_Method (Method);

      New_AR.AR_Obj
        := New_Local_Var
             (Method, J_String.Name ("__AR"), Type_Of (New_AR.AR_Class));

      --  Enter the code for creating the AR object as entry code for
      --  the method. This is necessary to ensure that the AR gets
      --  created outside of any conditional code sequences, since
      --  otherwise the AR reference could be uninitialized later
      --  in the method (this can occur when nested subprograms
      --  occur within blocks nested within conditional code). Note
      --  that a similar problem can happen with the fields within
      --  an AR, since they may be conditionally initialized and
      --  updated. Unfortunately we don't yet have a solution for
      --  handling the field problem. One possibility is to save
      --  and restore AR fields at the start and end of a block
      --  containing nested subprograms, but that's a difficult
      --  process to implement. ???

      Start_Entry_Code_Sequence;

      Gen_Default_Object (New_AR.AR_Class);
      Gen_Store_Local (New_AR.AR_Obj);

      --  If the parent of the nested method is itself a nested method then
      --  load the parent method's static link parameter and save it as the
      --  static link in the AR.

      if New_AR.Parent /= null then
         Gen_Load_Local (New_AR.AR_Obj);
         Gen_Load_Local (Local_Var (Current_Method, "__AR_SL"));
         Gen_Put_Object_Field (API_Field (AR_Static_Link));
      end if;

      End_Entry_Code_Sequence;

      --  Now switch back to the previous active method.

      Set_Current_Method (Save_Current_Method);
   end Make_Activation_Record;

   ----------------------------
   -- Make_Activation_Record --
   ----------------------------

   procedure Make_Activation_Record (Subp : Entity_Id) is
   begin
      Make_Activation_Record
        (JVM_Method (Subp),
         Name (JVM_Expanded_Name (Subp)),
         Package_Name (Subp));
   end Make_Activation_Record;

   ---------------------------
   -- End_Activation_Record --
   ---------------------------

   procedure End_Activation_Record (Method : Method_Id) is
      AR : constant AR_Access := AR_Stack.Pop;

   begin
      pragma Assert (AR.Method = Method);

      End_Class_File (AR.AR_Class);
   end End_Activation_Record;

   ------------------
   -- Add_AR_Field --
   ------------------

   procedure Add_AR_Field (AR : AR_Access; Field : Field_Id) is
   begin
      AR.Fields := new AR_Field_Rec'(Field, AR.Fields);
   end Add_AR_Field;

   --------------
   -- AR_Field --
   --------------

   function AR_Field (AR : AR_Access; Ada_Obj : Entity_Id) return Field_Id is
   begin
      return Field (AR.AR_Class, Up_Level_Field_Name (Ada_Obj));
   end AR_Field;

   --------------
   -- AR_Field --
   --------------

   function AR_Field (AR : AR_Access; Name : Name_Id) return Field_Id is
   begin
      return Field (AR.AR_Class, Name);
   end AR_Field;

   ------------------------
   -- Load_Up_Level_Link --
   ------------------------

   function Load_Up_Level_Link (Parent_Method : Method_Id) return AR_Access is
      AR_Entry : AR_Access := AR_Stack.Top;
      Chaining : Boolean := False;

   begin
      if Parent_Method = Current_Method then
         Gen_Load_Local (AR_Entry.AR_Obj);

      else
         --  Load the current method's static link parameter

         Gen_Load_Local (Local_Var (Current_Method, "__AR_SL"));

         --  If the current method has an AR then we first step to
         --  the parent's AR.

         if AR_Entry.Method = Current_Method then
            AR_Entry := AR_Entry.Parent;
         end if;

         --  Now, if needed, follow the AR links until reaching the
         --  object's associated AR.

         while AR_Entry /= null
           and then AR_Entry.Method /= Parent_Method
         loop
            Chaining := True;
            Gen_Get_Object_Field (API_Field (AR_Static_Link));
            AR_Entry := AR_Entry.Parent;
         end loop;

         pragma Assert (AR_Entry /= null);

         --  If any up-level links were followed, then we need to
         --  cast the top-of-stack class reference (which denotes
         --  the top-level Ada activation record class) to the
         --  class of the located activation record.

         if Chaining then
            Gen_Check_Cast (AR_Entry.AR_Class);
         end if;
      end if;

      return AR_Entry;
   end Load_Up_Level_Link;

   ---------------------------------
   -- Register_Up_Level_Reference --
   ---------------------------------

   procedure Register_Up_Level_Reference
     (Method : Method_Id; LV : Local_Var_Id)
   is
      Save_Current_Method : Method_Id;
      Local_Field         : Field_Id;
      AR_Entry            : AR_Access;

   begin
      pragma Assert (not AR_Stack.Empty);

      AR_Entry := AR_Stack.Top;

      while AR_Entry /= null and then Method /= AR_Entry.Method loop
         AR_Entry := AR_Entry.Parent;
      end loop;

      pragma Assert (AR_Entry /= null);

      --  If the object's local variable doesn't already exist in its
      --  parent's AR, then this is the first up-level reference to it,
      --  so we have to create a new field for it in the parent's AR
      --  class and copy its current value into the parent's AR object.

      if AR_Field (AR_Entry, Name (LV)) = Null_Field then
         Save_Current_Method := Current_Method;
         Local_Field := New_Field (AR_Entry.AR_Class, Name (LV),
                                   Type_Of (LV), Static => False);

         Add_AR_Field (AR_Entry, Local_Field);

         --  Temporarily switch to the parent method and generate code
         --  to copy the up-level variable into its corresponding field
         --  in the parent's AR object.

         Set_Current_Method (AR_Entry.Method);

         Gen_Load_Local (AR_Entry.AR_Obj);
         Gen_Load_Local (LV);
         Gen_Put_Field (AR_Field (AR_Entry, Name (LV)));

         --  Now restore the referencing method's environment.

         Set_Current_Method (Save_Current_Method);
      end if;
   end Register_Up_Level_Reference;

   ---------------------------------
   -- Register_Up_Level_Reference --
   ---------------------------------

   procedure Register_Up_Level_Reference (Ada_Obj : Entity_Id) is
      Save_Current_Method : Method_Id;
      Local_Field         : Field_Id;
      AR_Entry            : AR_Access;
      Object_Method       : constant Method_Id := Enclosing_Method (Ada_Obj);

   begin
      pragma Assert (not AR_Stack.Empty);

      AR_Entry := AR_Stack.Top;

      while AR_Entry /= null
         and then Object_Method /= AR_Entry.Method
      loop
         AR_Entry := AR_Entry.Parent;
      end loop;

      pragma Assert (AR_Entry /= null);

      --  If the object's local variable doesn't already exist in its
      --  parent's AR, then this is the first up-level reference to it,
      --  so we have to create a new field for it in the parent's AR
      --  class and copy its current value into the parent's AR object.

      if AR_Field (AR_Entry, Up_Level_Field_Name (Ada_Obj)) = Null_Field then
         Local_Field := New_Field (AR_Entry.AR_Class,
                                   Up_Level_Field_Name (Ada_Obj),
                                   Type_Of (JVM_Local_Var (Ada_Obj)),
                                   Static => False);

         Add_AR_Field (AR_Entry, Local_Field);

         --  Temporarily switch to the parent method and generate code
         --  to copy the up-level variable into its corresponding field
         --  in the parent's AR object.

         Save_Current_Method := Current_Method;
         Set_Current_Method (AR_Entry.Method);

         Gen_Load_Local (AR_Entry.AR_Obj);
         Gen_Load_Local (JVM_Local_Var (Ada_Obj));
         Gen_Put_Field (Local_Field);

         --  Now restore the referencing method's environment.

         Set_Current_Method (Save_Current_Method);
      end if;
   end Register_Up_Level_Reference;

   ----------------------------
   -- Access_From_Current_AR --
   ----------------------------

   function Access_From_Current_AR (Ada_Obj : Entity_Id) return Boolean is
   begin
      return not AR_Stack.Empty
        and then AR_Stack.Top.Method = Current_Method
        and then Ekind (Full_Type (Ada_Obj)) in Wrappable_Kind
        and then not Has_Wrapper (Ada_Obj)
        and then
          AR_Field (AR_Stack.Top, Up_Level_Field_Name (Ada_Obj)) /= Null_Field;
   end Access_From_Current_AR;

   ---------------------
   -- Access_AR_Field --
   ---------------------

   function Access_AR_Field (Ada_Obj : Entity_Id) return Field_Id is
      AR_Entry : AR_Access;

   begin
      Register_Up_Level_Reference (Ada_Obj);

      AR_Entry := Load_Up_Level_Link (Enclosing_Method (Ada_Obj));

      return AR_Field (AR_Entry, Up_Level_Field_Name (Ada_Obj));
   end Access_AR_Field;

   ----------------------
   -- Load_Static_Link --
   ----------------------

   procedure Load_Static_Link (Parent_Method : Method_Id) is
      AR_Entry : AR_Access;

   begin
      AR_Entry := Load_Up_Level_Link (Parent_Method);
   end Load_Static_Link;

   -------------------------
   -- Load_Up_Level_Field --
   -------------------------

   procedure Load_Up_Level_Field (Method : Method_Id; Name : Name_Id) is
      AR_Entry : constant AR_Access := Load_Up_Level_Link (Method);

   begin
      Gen_Get_Object_Field (AR_Field (AR_Entry, Name));
   end Load_Up_Level_Field;

   ----------------------
   -- Enclosing_Method --
   ----------------------

   function Enclosing_Method (E : Entity_Id) return Method_Id is
      Subp  : Entity_Id := Enclosing_Subprogram (E);

   begin
      --  Local variables are handled by directly retrieving their
      --  method, rather than applying JVM_Method to the result
      --  of Enclosing_Subprogram. This is because the enclosing
      --  subprogram is not always valid, e.g., in the case of
      --  variables associated with task types (the result of
      --  calling Enclosing_Subprogram may be a further outer
      --  subprogram enclosing the task). Similar difficulties
      --  could possibly occur for other entities such as subprograms,
      --  but it's not clear how to address those cases other than
      --  by using the enclosing subprogram. ???

      if Ekind (E) = E_Variable or else Ekind (E) = E_Constant then
         if Is_Global_Entity (E) then
            return Null_Method;

         else
            return Method_Of (JVM_Local_Var (E));
         end if;

      --  There are cases where Enclosing_Subprogram returns
      --  nonsubprogram entities for subprograms, such as E_Entry
      --  nodes, so we need to get the parent method from the
      --  subprogram's associated method. (This occurs in certain
      --  cases such as when a subprogram is created within a
      --  block statement nested in a subprogram expanded from an
      --  accept statement, because the E_Block's scope attribute
      --  is set to the entry of the accept statement rather than
      --  to the enclosing expander-generated subprogram. This
      --  seems like a bug, but we work around it for now by
      --  means of the JVM.Parent_Method function because there
      --  may be other cases like this where scopes are not set
      --  properly.) ???

      elsif Ekind (E) in Subprogram_Kind then
         return Parent_Method (JVM_Method (E));

      elsif Present (Subp) then
         return JVM_Method (Subp);

      else
         return Null_Method;
      end if;
   end Enclosing_Method;

   ----------------------
   -- Is_Global_Entity --
   ----------------------

   function Is_Global_Entity (E : Entity_Id) return Boolean is
   begin
      return not Present (Enclosing_Subprogram (E));
   end Is_Global_Entity;

end Jx_Uplev;
