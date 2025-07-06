------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J X _ D E C L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.112 $
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

with Atree;          use Atree;
with Einfo;          use Einfo;
with Elists;         use Elists;
with Errout;         use Errout;
with Fname;          use Fname;
with GNAT.Case_Util; use GNAT.Case_Util;
with JVM;            use JVM;
with JVM.API;        use JVM.API;
with JVM.Map;        use JVM.Map;
with J_Basics;       use J_Basics;
with J_String;       use J_String;
with J_Types;        use J_Types;
with Jx_Ch3;         use Jx_Ch3;
with Jx_Uplev;       use Jx_Uplev;
with Sem_Disp;       use Sem_Disp;
with Sem_Eval;       use Sem_Eval;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinput;         use Sinput;
with Snames;         use Snames;
with Stand;          use Stand;
with Types;          use Types;
with Uintp;          use Uintp;

package body Jx_Decl is

   procedure Get_Interface_Names
     (Interface_Name    : Node_Id;
      Java_Package_Name : out String_Id;
      Java_Class_Name   : out Name_Id);
   --  Given a specified Java class interface name of the form
   --  [dotted_package_name.]class_simple_name, splits out and
   --  returns the package name (or No_String if not present)
   --  and class name.

   procedure Get_Interface_Names
     (Interface_Name    : Node_Id;
      Java_Package_Name : out String_Id;
      Java_Class_Name   : out Name_Id;
      Java_Entity_Name  : out Name_Id);
   --  Given a specified Java interface name of the form
   --  [[dotted_package_name.]class_simple_name.]entity_name, splits
   --  out and returns the package name (or No_String if not present),
   --  class name (or No_Name if not present), and entity name.

   function Is_Run_Time_Package (Class_Name : String) return Boolean;
   --  Returns True if P denotes a run-time package (i.e., has a prefix
   --  that is "ada", "gnat", "system", or "interfaces".

   function Homonym_Suffix (E : Entity_Id) return String;
   --  Returns a distinguishing string suffix for E in the case where
   --  where E is overloaded within its scope, otherwise returns the
   --  empty string. The overloading suffix has two leading underscores
   --  followed by a digit sequence that uniquely distinguishes the
   --  overloaded entity from other entities of the same name within
   --  the entity's scope (all calls to this function with a given
   --  entity are guaranteed to return the same suffix).

   function JVM_Entity_Name (E : Entity_Id) return Name_Id;
   --  Returns a name id denoting the appropriate JVM name assigned
   --  to an Ada entity. This is only defined for entities that are
   --  compilation units, types for which a class will be generated,
   --  subprograms, exceptions, and components and global variables
   --  that are associated with Java fields.

   procedure Declare_Subprogram_Access_Type (Subp_Acc_Type : Entity_Id);
   --  Create the root class that represents the access-to-subprogram
   --  type Subp_Acc_Type.

   ----------------------
   -- Associated_Class --
   ----------------------

   function Associated_Class (Ada_Entity : Entity_Id) return Class_Id is
      Parent_Scope : Entity_Id := Scope (Ada_Entity);

   begin
      if Is_Type (Parent_Scope)
        and then Ekind (Full_Type (Parent_Scope)) in Record_Kind
      then
         return JVM_Class (Full_Type (Parent_Scope));

      --  If the entity is a dispatching operation, return the class
      --  associated with its dispatching type.

      elsif Is_Dispatching_Operation (Ada_Entity) then
         return JVM_Class (Full_Type (Find_Dispatching_Type (Ada_Entity)));

      --  If this is a subprogram nested within another subprogram,
      --  then associated the subprogram with the class of its parent.
      --  Thus, if the entity is a nondispatching subprogram nested in
      --  a dispatching operation, we return the class associated with
      --  its parent's dispatching type. In this way the method created
      --  for the subprogram will belong to the class of the dispatching
      --  parent, which ensures that calls to Java superclass methods
      --  will be handled properly via invokespecial calls within nested
      --  subprograms.

      elsif Ekind (Ada_Entity) in Subprogram_Kind
        and then Ekind (Scope (Ada_Entity)) in Subprogram_Kind
      then
         return Associated_Class (Scope (Ada_Entity));

      --  For compilation unit entities, simply return the associated
      --  JVM class entity. Generic instances that are library units
      --  do not have Is_Compilation_Unit set, so we have to check
      --  that case specially by testing if the scope is Standard. ???

      elsif Is_Compilation_Unit (Ada_Entity)
        or else Is_Child_Unit (Ada_Entity)
        or else
          (Is_Generic_Instance (Ada_Entity)
            and then Parent_Scope = Standard_Standard)
      then
         return JVM_Class (Ada_Entity);

      elsif Is_Imported (Ada_Entity)
        and then Convention (Ada_Entity) = Convention_C
      then
         return API_Class (GNAT_libc);

      else
         while Present (Parent_Scope) loop
            if Is_Compilation_Unit (Parent_Scope) then
               --  If the enclosing compilation unit is Standard, then
               --  this may be an entity declared for use in a library
               --  elaboration routine, so it should be associated with
               --  the current compilation class.

               if Parent_Scope = Standard_Standard then
                  return Current_Compilation_Class;
               else
                  return JVM_Class (Parent_Scope);
               end if;
            end if;

            Parent_Scope := Scope (Parent_Scope);
         end loop;
      end if;

      pragma Assert (False);
      raise Program_Error;
   end Associated_Class;

   --------------
   -- JVM_Type --
   --------------

   function JVM_Type (Ada_Node : Node_Id) return Type_Id is
      A_Type  : Entity_Id;
      J_Type  : Type_Id := Null_Type;

   begin
      --  Always go to the base type, since we don't create
      --  JVM type entities for subtypes. If Ada_Node is a
      --  type, then take Base_Type directly because we don't
      --  want to pick up the parent type of a derived type.

      if Nkind (Ada_Node) in N_Entity then
         if Is_Type (Ada_Node) then
            A_Type := Base_Type (Ada_Node);

         --  If the node denotes an aliased object of a scalar or
         --  (non-subprogram) access type, then simply return the
         --  appropriate wrapper type for the object.

         elsif Has_Wrapper (Ada_Node) then
            return Wrapper_Type (Base_Type (Etype (Ada_Node)));

         else
            A_Type := Base_Type (Etype (Ada_Node));
         end if;

      else
         A_Type := Base_Type (Etype (Ada_Node));
      end if;

      if Present (A_Type) then
         J_Type := JVM_Entity (A_Type);
         if J_Type /= Null_Type then
            return J_Type;
         else
            Declare_Type (A_Type);
            return JVM_Entity (A_Type);
         end if;
      else
         return Void_Type;
      end if;
   end JVM_Type;

   ---------------
   -- JVM_Class --
   ---------------

   function JVM_Class (Ada_Entity : Entity_Id) return Class_Id is
      Class      : Class_Id;
      J_Type     : Type_Id;

   begin
      case Ekind (Ada_Entity) is
         when E_Package | Generic_Unit_Kind =>
            Class := JVM_Entity (Ada_Entity);
            if Class = Null_Class then
               Declare_Package_Class (Ada_Entity);
               Class := JVM_Entity (Ada_Entity);
            end if;

         when E_Procedure | E_Function =>

            --  For now we only allow applying JVM_Class to a subprogram in
            --  the case where it's a compilation unit, though it may be
            --  useful to allow it to apply to arbitrary subprograms. ???

            pragma Assert (Is_Compilation_Unit (Ada_Entity)
                            or else Is_Child_Unit (Ada_Entity));

            --  For subprogram library units we either return the class
            --  associated with the subprogram's method if the method is
            --  present, or else we create the class now (in which case
            --  the method has not been declared yet but typically the
            --  result of this call will be used in declaring the method).
            --  This avoids having to associate the class of the subprogram
            --  with a node in the GNAT tree.

            if JVM_Entity (Ada_Entity) /= Null_Method then
               Class := Class_Of (JVM_Entity (Ada_Entity));

            else
               Class := New_Class
                          (Name     => JVM_Entity_Name (Ada_Entity),
                           Pkg_Name => Package_Name (Ada_Entity),
                           Src_Name => Source_Name (Sloc (Ada_Entity)));
            end if;

         when E_Record_Type |
              E_Record_Type_With_Private |
              E_Private_Type |
              E_Limited_Private_Type |
              E_Access_Subprogram_Type |
              E_Task_Type |
              E_Protected_Type =>
            J_Type := JVM_Type (Ada_Entity);
            if J_Type /= Null_Type then
               Class := Class_Of_Type (J_Type);
            else
               Declare_Type (Ada_Entity);
               Class := Class_Of_Type (JVM_Type (Ada_Entity));
            end if;
         when E_Class_Wide_Type =>
            J_Type := JVM_Type (Ada_Entity);
            if J_Type /= Null_Type then
               Class := Class_Of_Type (J_Type);
            else
               --  For a class-wide type we declare its root type
               --  (i.e., the associated specific type).
               Declare_Type (Etype (Ada_Entity));
               Class := Class_Of_Type (JVM_Type (Ada_Entity));
            end if;
         when E_Exception =>
            J_Type := JVM_Entity (Ada_Entity);
            if J_Type /= Null_Type then
               Class := Class_Of_Type (J_Type);
            else
               Declare_Exception_Class (Ada_Entity);
               Class := Class_Of_Type (JVM_Entity (Ada_Entity));
            end if;
         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      return Class;
   end JVM_Class;

   ---------------
   -- JVM_Field --
   ---------------

   function JVM_Field (Ada_Entity : Entity_Id) return Field_Id is
      Kind  : constant Entity_Kind := Ekind (Ada_Entity);
      Field : constant Field_Id := JVM_Entity (Ada_Entity);
      Orig  : Entity_Id;

   begin
      if Field /= Null_Field then
         return Field;
      else
         --  We need to map any components declared for a record
         --  subtype onto the same JVM field entity as their
         --  corresponding (in the case of a linked discriminant)
         --  or original component.

         if Kind = E_Component or else Kind = E_Discriminant then
            Orig := Original_Record_Component (Ada_Entity);

            if Kind = E_Discriminant
              and then Present (Corresponding_Discriminant (Orig))
            then
               Set_Map
                 (Ada_Entity, JVM_Field (Corresponding_Discriminant (Orig)));
               return JVM_Entity (Ada_Entity);

            elsif Present (Orig) and then Ada_Entity /= Orig then
               Set_Map (Ada_Entity, JVM_Field (Orig));
               return JVM_Entity (Ada_Entity);
            end if;
         end if;

         Declare_Field (Associated_Class (Ada_Entity), Ada_Entity);
         return JVM_Entity (Ada_Entity);
      end if;
   end JVM_Field;

   ----------------
   -- JVM_Method --
   ----------------

   function JVM_Method (Ada_Entity : Entity_Id) return Method_Id is
      Method : constant Method_Id := JVM_Entity (Ada_Entity);

   begin
      if Method /= Null_Method then
         return Method;
      else
         Declare_Method (Associated_Class (Ada_Entity), Ada_Entity);
         return JVM_Entity (Ada_Entity);
      end if;
   end JVM_Method;

   -------------------
   -- JVM_Local_Var --
   -------------------

   function JVM_Local_Var (Ada_Entity : Entity_Id) return Local_Var_Id is
      Local_Var : constant Local_Var_Id := JVM_Entity (Ada_Entity);

   begin
      if Local_Var /= Null_Local_Var then
         return Local_Var;
      else
         Declare_Local_Variable (Ada_Entity);
         return JVM_Entity (Ada_Entity);
      end if;
   end JVM_Local_Var;

   -------------------
   -- JVM_Local_Var --
   -------------------

   function JVM_Label (Label : Entity_Id) return Label_Id is
      Lbl : constant Label_Id := JVM_Entity (Label);

   begin
      if Lbl /= Null_Label then
         return Lbl;
      else
         Declare_Label (Label);
         return JVM_Entity (Label);
      end if;
   end JVM_Label;

   ---------------
   -- Full_Type --
   ---------------

   function Full_Type (Ada_Node : Node_Id) return Entity_Id is
      Under_Type : Entity_Id;

   begin
      if Nkind (Ada_Node) in N_Entity and then Is_Type (Ada_Node) then
         Under_Type := Underlying_Type (Base_Type (Ada_Node));
         if not Present (Under_Type) then
            return Base_Type (Ada_Node);
         else
            return Base_Type (Under_Type);
         end if;

      else
         Under_Type := Underlying_Type (Base_Type (Etype (Ada_Node)));
         if not Present (Under_Type) then
            return Base_Type (Etype (Ada_Node));
         else
            return Base_Type (Under_Type);
         end if;
      end if;
   end Full_Type;

   ------------------
   -- Full_Subtype --
   ------------------

   function Full_Subtype (Ada_Node : Node_Id) return Entity_Id is
   begin
      if Nkind (Ada_Node) in N_Entity and then Is_Type (Ada_Node) then
         if Is_Incomplete_Or_Private_Type (Ada_Node)
           and then Present (Underlying_Type (Ada_Node))
         then
            return Underlying_Type (Ada_Node);
         else
            return Ada_Node;
         end if;
      else
         if Is_Incomplete_Or_Private_Type (Etype (Ada_Node))
           and then Present (Underlying_Type (Etype (Ada_Node)))
         then
            return Underlying_Type (Etype (Ada_Node));
         else
            return Etype (Ada_Node);
         end if;
      end if;
   end Full_Subtype;

   -------------------------------
   -- Has_Nondispatching_Method --
   -------------------------------

   function Has_Nondispatching_Method (Subp : Entity_Id) return Boolean is
   begin
      if Present (Alias (Subp)) then
         return Has_Nondispatching_Method (Alias (Subp));
      else
         return Is_Dispatching_Operation (Subp)
           and then not Is_Abstract (Subp)
           and then Convention (Scope (Find_Dispatching_Type (Subp)))
                      /= Convention_Java;
      end if;
   end Has_Nondispatching_Method;

   ----------------------------
   -- Overrides_Interface_Op --
   ----------------------------

   function Overrides_Interface_Op
     (New_Subp     : Entity_Id;
      Intface_Subp : Entity_Id)
      return         Boolean
   is
      New_Formal : Entity_Id := First_Formal (New_Subp);
      Old_Formal : Entity_Id := First_Formal (Intface_Subp);

   begin
      --  Both subprograms must have the same name and a controlling formal
      --  in the first parameter position to qualify for overriding.

      if Chars (New_Subp) = Chars (Intface_Subp)
        and then Present (New_Formal)
        and then Present (Old_Formal)
        and then Is_Controlling_Formal (New_Formal)
        and then Is_Controlling_Formal (Old_Formal)
      then
         Next_Formal (New_Formal);
         Next_Formal (Old_Formal);

         --  The subtypes of all remaining parameters must statically match

         while Present (New_Formal) and then Present (Old_Formal) loop
            if not Subtypes_Statically_Match
                        (Etype (New_Formal), Etype (Old_Formal))
            then
               return False;
            end if;

            Next_Formal (New_Formal);
            Next_Formal (Old_Formal);
         end loop;

         --  If the subprograms have differing numbers of parameters
         --  then this is not an overriding.

         if Present (New_Formal) or else Present (Old_Formal) then
            return False;
         end if;

         --  If the subprograms differ in kind then this is not
         --  an overriding.

         if Ekind (New_Subp) /= Ekind (Intface_Subp) then
            return False;

         --  In the function case, this is an overriding if the result
         --  subtypes match.

         elsif Ekind (New_Subp) = E_Function then
            return Subtypes_Statically_Match
                     (Etype (New_Subp), Etype (Intface_Subp));

         --  If we run the above gantlet successfully this is an overriding

         else
            return True;
         end if;

      else
         return False;
      end if;
   end Overrides_Interface_Op;

   -------------------------
   -- Is_Run_Time_Package --
   -------------------------

   function Is_Run_Time_Package (Class_Name : String) return Boolean is
      Last : Natural := Class_Name'First;

   begin
      while Last <= Class_Name'Last
        and then Class_Name (Last) /= '.'
        and then Class_Name (Last) /= '$'
      loop
         Last := Last + 1;
      end loop;

      declare
         Prefix : constant String := Class_Name (Class_Name'First .. Last - 1);

      begin
         return Prefix = "system"
           or else Prefix = "ada"
           or else Prefix = "gnat"
           or else Prefix = "interfaces";
      end;
   end Is_Run_Time_Package;

   -------------------------
   -- Get_Interface_Names --
   -------------------------

   procedure Get_Interface_Names
     (Interface_Name    : Node_Id;
      Java_Package_Name : out String_Id;
      Java_Class_Name   : out Name_Id)
   is
      Java_Name : constant String := Str (Strval (Interface_Name));
      Index     : Natural := Java_Name'Last;

   begin
      while Index > 0 and then Java_Name (Index) /= '.' loop
         Index := Index - 1;
      end loop;

      Java_Class_Name := Name (Java_Name (Index + 1 .. Java_Name'Last));

      if Index > 1 then
         Java_Package_Name := Str_Id (Java_Name (1 .. Index - 1));

      elsif Is_Run_Time_Package (Java_Name (Index + 1 .. Java_Name'Last)) then
         Java_Package_Name := Ada_Lib_Package;

      else
         Java_Package_Name := No_String;
      end if;
   end Get_Interface_Names;

   -------------------------
   -- Get_Interface_Names --
   -------------------------

   procedure Get_Interface_Names
     (Interface_Name    : Node_Id;
      Java_Package_Name : out String_Id;
      Java_Class_Name   : out Name_Id;
      Java_Entity_Name  : out Name_Id)
   is
      Full_Name  : constant String := Str (Strval (Interface_Name));
      Index      : Natural := Full_Name'Last;
      Class_Last : Natural;

   begin
      while Index > 0 and then Full_Name (Index) /= '.' loop
         Index := Index - 1;
      end loop;

      Java_Entity_Name := Name (Full_Name (Index + 1 .. Full_Name'Last));

      --  If the given interface name is a simple name, then
      --  set the class and package names to empty and return.

      if Index = 0 then
         Java_Class_Name   := No_Name;
         Java_Package_Name := No_String;
         return;
      end if;

      pragma Assert (Full_Name (Index) = '.' and then Index > Full_Name'First);

      Index := Index - 1;

      Class_Last := Index;

      while Index > 0 and then Full_Name (Index) /= '.' loop
         Index := Index - 1;
      end loop;

      Java_Class_Name := Name (Full_Name (Index + 1 .. Class_Last));

      if Index > 1 then
         Java_Package_Name := Str_Id (Full_Name (1 .. Index - 1));

      elsif Is_Run_Time_Package (Full_Name (Index + 1 .. Class_Last)) then
         Java_Package_Name := Ada_Lib_Package;

      else
         Java_Package_Name := No_String;
      end if;
   end Get_Interface_Names;

   --------------------
   -- Homonym_Suffix --
   --------------------

   function Homonym_Suffix (E : Entity_Id) return String is
      Hmnym   : Entity_Id;
      H_Num   : Positive := 1;

   begin
      --  If E is an overloadable entity with homonyms, then traverse its
      --  associated homonym list, counting up the homonyms in the same
      --  scope as the entity. Return a string composed of two leading
      --  underscores followed by the string representation of the
      --  resulting homonym count.

      if Is_Overloadable (E) and then Has_Homonym (E) then
         Hmnym := Homonym (E);

         while Present (Hmnym) loop
            if Scope (E) = Scope (Hmnym) then
               H_Num := H_Num + 1;
            end if;

            Hmnym := Homonym (Hmnym);
         end loop;

         --  If H_Num is the first homonym then we treat it the same as
         --  the case of no homonyms (i.e., no suffix appended). This is
         --  actually necessary to handle the case where a subprogram is
         --  declared in a package specification and has no homonyms in
         --  that spec, but does have homonyms in the body. The problem
         --  is that such a subprogram will not appear to be overloaded
         --  to clients of the spec, but is overloaded from the perspective
         --  of the package body, so we have to ensure it has the same
         --  name in both contexts.

         if H_Num = 1 then
            return "";
         else
            return "__" & Strip (H_Num'Img);
         end if;
      end if;

      --  The entity has no homonyms within its scope, so return
      --  the empty string.

      return "";
   end Homonym_Suffix;

   -----------------------
   -- JVM_Expanded_Name --
   -----------------------

   function JVM_Expanded_Name
     (E     : Entity_Id;
      Full  : Boolean := True)
      return  String
   is
      Parent_Scope : constant Entity_Id := Scope (E);

   begin
      --  If at the top of the scope chain (Standard) then stop recursion
      --  and return the entity's simple name. The check for Is_Type is to
      --  handle the special case of type Access_String, which is created
      --  by the front end and must be mapped to standard$access_string.

      if Parent_Scope = Standard_Standard then
         if Is_Type (E) then
            return "standard$" & Name_String (Chars (E));
         else
            return Name_String (Chars (E)) & Homonym_Suffix (E);
         end if;

      --  If Full is False and the entity is at the top level of a
      --  compilation unit then stop recursion and return the simple
      --  name (used for cases of fields and subprograms, which do not
      --  require a fully expanded scope name prefix).

      elsif not Full and then Is_Compilation_Unit (Parent_Scope) then
         return Name_String (Chars (E)) & Homonym_Suffix (E);

      --  Otherwise return a concatenation of the entity's simple name with
      --  the recursively generated JVM expanded name of the parent scope.

      else
         return JVM_Expanded_Name (Parent_Scope, Full)
                     & "$" & Name_String (Chars (E)) & Homonym_Suffix (E);
      end if;
   end JVM_Expanded_Name;

   ---------------------
   -- JVM_Entity_Name --
   ---------------------

   function JVM_Entity_Name (E : Entity_Id) return Name_Id is
   begin
      --  Library-level units, types, and exceptions always get a fully
      --  expanded name.

      if Is_Compilation_Unit (E)
        or else Is_Child_Unit (E)
        or else Is_Type (E)
        or else Ekind (E) = E_Exception
      then
         return Name (JVM_Expanded_Name (E));

      --  Outer-level entities (i.e., global variables and subprograms
      --  declared immediately within a package compilation unit) are
      --  assigned their simple name.

      elsif Is_Compilation_Unit (Scope (E))
        and then Ekind (Scope (E)) = E_Package
      then
         if Is_Overloadable (E) and then Has_Homonym (E) then
            return Name (Name_String (Chars (E)) & Homonym_Suffix (E));
         else
            return Chars (E);
         end if;

      --  Components are always assigned their simple name

      elsif Ekind (E) = E_Component or else Ekind (E) = E_Discriminant then
         return Chars (E);

      --  Other entities are declared within nested packages and subprograms
      --  and require an expanded name to avoid potential name clashes
      --  with other like-named entities in the containing compilation unit.
      --
      --  (But note that there are still some pathological cases involving
      --  overloaded nested subprograms that will require unique suffix
      --  numbers to be generated for nested entities (TBD). ???)

      else
         return Name (JVM_Expanded_Name (E, Full => False));
      end if;
   end JVM_Entity_Name;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (N : Node_Id) return String_Id is

      function Java_Package (Pkg_Scope : Entity_Id) return String;
      --  Returns the string name of the Java package name associated
      --  with a package Pkg_Scope with convention Java. This is
      --  obtained from the package's interface name, if specified,
      --  or else determined by traversing up the package scopes
      --  enclosing Pkg_Scope to create a dotted name (e.g., "java.lang").

      function Java_Package (Pkg_Scope : Entity_Id) return String is
         Parent_Scope : constant Entity_Id := Scope (Pkg_Scope);
         Class_Name   : Name_Id;
         Package_Name : String_Id;

      begin
         --  If the package has a specified interface name, then get
         --  the (optional) Java package name.

         if Present (Interface_Name (Pkg_Scope)) then
            Get_Interface_Names
              (Interface_Name (Pkg_Scope), Package_Name, Class_Name);
            return Str (Package_Name);

         elsif Parent_Scope = Standard_Standard then
            return "";

         elsif Scope (Parent_Scope) = Standard_Standard then
            return Name_String (Chars (Parent_Scope));

         else
            return Java_Package (Parent_Scope)
                     & "." & Name_String (Chars (Parent_Scope));
         end if;
      end Java_Package;

   --  Start of processing for Package_Name

   begin
      if Nkind (N) in N_Entity then

         --  If this is a Java-imported package, then return the
         --  Java package name associated with the package itself.
         --  (Note: We should be also testing Is_Imported for the
         --  package, but for some reason this is sometimes
         --  incorrectly set to false. Maybe a problem that
         --  occurs for Is_Imported on withed packages ???)

         if Ekind (N) = E_Package
           and then Convention (N) = Convention_Java
         then
            return Str_Id (Java_Package (N));

         --  If the entity is contained in a Java-imported package, return
         --  the Java package name of the entity's enclosing package.

         elsif Ekind (Scope (N)) = E_Package
           and then Convention (Scope (N)) = Convention_Java
         then
            return Str_Id (Java_Package (Scope (N)));

         elsif Sloc (N) = Standard_Location then
            return Ada_Lib_Package;

         --  Otherwise, if the node is contained in a GNAT library unit,
         --  then return the Java package for the Ada library.

         elsif Is_Internal_File_Name (Source_Name (Sloc (N))) then
            if Instantiation (Get_Source_File_Index (Sloc (N))) = No_Location
              or else
                Is_Internal_File_Name
                  (Source_Name
                    (Instantiation (Get_Source_File_Index (Sloc (N)))))
            then
               return Ada_Lib_Package;
            else
               return No_String;
            end if;

         --  No specified package, so just return the empty string id

         else
            return No_String;
         end if;

      elsif Sloc (N) = Standard_Location then
         return Ada_Lib_Package;

      --  If the node is contained in a GNAT library unit,
      --  then return the Java package for the Ada library.

      elsif Is_Internal_File_Name (Source_Name (Sloc (N))) then
         if Instantiation (Get_Source_File_Index (Sloc (N))) = No_Location
           or else
             Is_Internal_File_Name
               (Source_Name
                 (Instantiation (Get_Source_File_Index (Sloc (N)))))
         then
            return Ada_Lib_Package;
         else
            return No_String;
         end if;

      else
         return No_String;
      end if;
   end Package_Name;

   -----------------
   -- Has_Wrapper --
   -----------------

   function Has_Wrapper (Ada_Entity : Entity_Id) return Boolean is
   begin
      return (Is_Aliased (Ada_Entity)
        or else Ekind (Ada_Entity) in E_Out_Parameter .. E_In_Out_Parameter)
          and then
            Ekind (Underlying_Type (Etype (Ada_Entity))) in Wrappable_Kind;
   end Has_Wrapper;

   ------------------
   -- Wrapper_Type --
   ------------------

   function Wrapper_Type (Obj_Or_Type : Entity_Id) return Type_Id is
   begin
      if Is_Type (Obj_Or_Type) then
         pragma Assert (Ekind (Full_Type (Obj_Or_Type)) in Wrappable_Kind);

         if Ekind (Full_Type (Obj_Or_Type)) in Access_Kind then
            return Type_Of (API_Class (Ada_Acc));

         else
            case JVM.Type_Kind (JVM_Type (Full_Type (Obj_Or_Type))) is
               when Int_Kind =>
                  return Type_Of (API_Class (Ada_Int));
               when Long_Kind =>
                  return Type_Of (API_Class (Ada_Lng));
               when JVM.Float_Kind =>
                  return Type_Of (API_Class (Ada_Flt));
               when Double_Kind =>
                  return Type_Of (API_Class (Ada_Dbl));

               --  The 'others' alternative handles the special
               --  case of type System.Address, which gets mapped
               --  to java.lang.Object and doesn't get caught by
               --  the test for Access_Kind above.

               when others =>
                  return Type_Of (API_Class (Ada_Acc));
            end case;
         end if;

      else
         pragma Assert (Has_Wrapper (Obj_Or_Type));

         --  Note that we don't call JVM_Type here. This would be okay,
         --  but it would be unclean since JVM_Type invokes Wrapper_Type
         --  itself on types, and it's better to avoid the possibility
         --  of infinite recursion altogether in case any change is ever
         --  made to JVM_Type to call Wrapper_Type directly on objects.

         return Wrapper_Type (Etype (Obj_Or_Type));
      end if;
   end Wrapper_Type;

   -------------------
   -- Wrapper_Field --
   -------------------

   function Wrapper_Field (JVM_Type : Type_Id) return Field_Id is
   begin
      return Field (Class_Of_Type (JVM_Type), "all");
   end Wrapper_Field;

   -------------------
   -- Wrapper_Field --
   -------------------

   function Wrapper_Field (Obj_Or_Type : Entity_Id) return Field_Id is
   begin
      return Wrapper_Field (Wrapper_Type (Obj_Or_Type));
   end Wrapper_Field;

   ----------------------
   -- Deep_Copy_Method --
   ----------------------

   function Deep_Copy_Method_Name (Typ : Entity_Id) return Name_Id is
   begin
      --  An expanded name is needed for array deep copy methods because
      --  they are associated with an enclosing unit's class rather than
      --  the class of the type itself, so a unique name is needed to
      --  avoid clashes due to multiple array types in the same unit.
      --  We get the name of the root type because derived array types
      --  share the deep copy of their ultimate ancestor type.

      if Ekind (Typ) in Einfo.Array_Kind then
         return Name (JVM_Expanded_Name (Root_Type (Typ)) & "_deep_copy");
      else
         return Name ("_deep_copy");
      end if;
   end Deep_Copy_Method_Name;

   -----------------------
   -- Deep_Clone_Method --
   -----------------------

   function Deep_Clone_Method_Name (Typ : Entity_Id) return Name_Id is
   begin
      return Name ("_deep_clone");
   end Deep_Clone_Method_Name;

   ---------------------
   -- Deep_Copy_Class --
   ---------------------

   function Deep_Copy_Class (Typ : Entity_Id) return Class_Id is
      J_Type : Type_Id := JVM_Type (Typ);

   begin
      if JVM.Type_Kind (J_Type) = JVM.Array_Kind then
         return Associated_Class (Root_Type (Typ));
      else
         return Class_Of_Type (J_Type);
      end if;
   end Deep_Copy_Class;

   ----------------------
   -- Deep_Clone_Class --
   ----------------------

   function Deep_Clone_Class (Typ : Entity_Id) return Class_Id is
   begin
      return Class_Of_Type (JVM_Type (Typ));
   end Deep_Clone_Class;

   ------------------
   -- Declare_Type --
   ------------------

   procedure Declare_Type (Typ : Entity_Id) is
      Full_Base : Entity_Id;

   begin
      --  If the type is an untagged derived type then simply associate
      --  it with its parent's JVM type and return.

      if Is_Derived_Type (Typ) and then not Is_Tagged_Type (Typ) then
         Set_Map (Typ, JVM_Type (Etype (Typ)));
         return;
      end if;

      case Ekind (Typ) is
         when Discrete_Kind =>
            Declare_Discrete_Type (Typ);

         when Einfo.Float_Kind =>
            Declare_Floating_Point_Type (Typ);

         when Fixed_Point_Kind =>
            Declare_Fixed_Point_Type (Typ);

         when E_Class_Wide_Type =>
            --  For a class-wide type we need to declare its associated
            --  specific type if not already established.

            if JVM_Entity (Base_Type (Etype (Typ))) = Null_Type then
               Declare_Type (Base_Type (Etype (Typ)));
            end if;

            --  Now set the JVM type of the class-wide type. Normally this
            --  should happen as part of the specific type's declaration,
            --  but in certain cases there are extra class-wide types
            --  created that reference the specific type but are not
            --  accessible from the specific type's entity (this can
            --  apparently occur as the result of "with type" clauses?).
            --  It's not clear if such class-wide types should occur ???

            if JVM_Entity (Typ) = Null_Type then
               Set_Map (Typ, JVM_Type (Base_Type (Etype (Typ))));
            end if;

         when E_Record_Type =>
            Declare_Record_Class (Typ);

         when E_Array_Type | E_String_Type | E_Enum_Table_Type =>
            Declare_Array_Type (Typ);

         when E_Private_Type |
              E_Limited_Private_Type |
              E_Record_Type_With_Private |
              E_Incomplete_Type =>

            --  If this is an incomplete type whose full type is deferred
            --  to the parent's body, then simply declare it as a record
            --  class.

            if Has_Completion_In_Body (Typ) then
               Declare_Record_Class (Typ);

            --  Declare the full type and set the private or incomplete
            --  type entity to refer to the Type_Id associated with the
            --  full type.

            else
               Full_Base := Base_Type (Full_View (Typ));
               if Type_Id'(JVM_Entity (Full_Base)) = Null_Type then
                  Declare_Type (Full_Base);
               end if;
               Set_Map (Typ, Type_Id'(JVM_Entity (Full_Base)));
            end if;

         when E_Access_Type |
              E_General_Access_Type |
              E_Anonymous_Access_Type |
              E_Access_Attribute_Type =>
            Declare_Access_Type (Typ);

         when E_Allocator_Type =>
            null;  --  ??? Should we ever need to do anything with these ???

         when E_Access_Subprogram_Type =>
            Declare_Subprogram_Access_Type (Typ);

         when E_Access_Protected_Subprogram_Type =>
            Error_Msg_N
              ("access-to-protected-subprogram types not yet supported", Typ);

         when E_Task_Type | E_Protected_Type =>
            --  Associate a task type with the same JVM type associated
            --  with its corresponding record type.

            if JVM_Entity (Typ) = Null_Type then
               Set_Map (Typ, JVM_Type (Corresponding_Record_Type (Typ)));
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Declare_Type;

   ---------------------------
   -- Declare_Discrete_Type --
   ---------------------------

   procedure Declare_Discrete_Type (Disc_Type : Entity_Id) is
   begin
      case Ekind (Disc_Type) is
         when E_Enumeration_Type =>
            Set_Map (Disc_Type, Int_Type);

         when E_Signed_Integer_Type | E_Modular_Integer_Type =>

            --  If the type is System.Address, then we map the type
            --  to java.lang.Object. This is a temporary expedient
            --  to avoid problems that arise from changing package
            --  System directly (e.g., if we change the full type
            --  of System.Address to be an access type, then the
            --  package can no longer be declared Pure, which
            --  interferes with the classification of other
            --  run-time packages). However, this kludge will
            --  probably lead to other problems. ???

            if Name_String (Chars (Disc_Type)) = "address"
              and then Name_String (Chars (Scope (Disc_Type))) = "system"
              and then Is_Library_Level_Entity (Scope (Disc_Type))
            then
               Set_Map (Disc_Type, Type_Of (Java_Lang_Object));

            else
               if RM_Size (Disc_Type) <= 8 then
                  Set_Map (Disc_Type, Byte_Type);

               --  We should also test for types that fit within
               --  16 bits and associate them with Short_Type ???

               elsif RM_Size (Disc_Type) <= 32 then
                  Set_Map (Disc_Type, Int_Type);

               else
                  Set_Map (Disc_Type, Long_Type);
               end if;
            end if;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end Declare_Discrete_Type;

   ---------------------------------
   -- Declare_Floating_Point_Type --
   ---------------------------------

   procedure Declare_Floating_Point_Type (Flt_Type : Entity_Id) is
   begin
      if Esize (Flt_Type) <= 32 then
         Set_Map (Flt_Type, Float_Type);
      else
         Set_Map (Flt_Type, Double_Type);
      end if;
   end Declare_Floating_Point_Type;

   ------------------------------
   -- Declare_Fixed_Point_Type --
   ------------------------------

   procedure Declare_Fixed_Point_Type (Fixed_Type : Entity_Id) is
   begin
      if RM_Size (Fixed_Type) <= 32 then
         Set_Map (Fixed_Type, Int_Type);
      else
         Set_Map (Fixed_Type, Long_Type);
      end if;
   end Declare_Fixed_Point_Type;

   --------------------------
   -- Declare_Record_Class --
   --------------------------

   procedure Declare_Record_Class (Rec_Type : Entity_Id) is
      Rec_Class   : Class_Id;
      Class_Type  : Type_Id;
      Deep_Copy   : Method_Id;
      Deep_Clone  : Method_Id;
      Target      : Local_Var_Id;
      Source      : Local_Var_Id;
      Src_Name    : Name_Id  := Source_Name (Sloc (Rec_Type));
      Superclass  : Class_Id := Java_Lang_Object;
      Abst_Class  : Boolean  := Is_Abstract (Rec_Type);
      Is_Intface  : Boolean  := False;
      Result_Type : Type_Id;

      function Class_Name (Rec_Type : Entity_Id) return Name_Id;
      --  Returns the symbolic name to use for the class associated
      --  with a record type. In general this will be constructed
      --  using the expanded name of the containing package class
      --  concatenated with the simple name of the record type
      --  (and using a double underscore as a separator).

      function Class_Name (Rec_Type : Entity_Id) return Name_Id is
         Pkg_Nm   : String_Id;
         Class_Nm : Name_Id;

      begin
         --  If type is tagged and its enclosing scope has convention Java,
         --  then we declare the class using the interface name of the parent
         --  scope rather than the expanded name of the type.

         if Is_Tagged_Type (Rec_Type)
           and then Convention (Scope (Rec_Type)) = Convention_Java
         then
            if Present (Interface_Name (Scope (Rec_Type))) then
               Get_Interface_Names
                 (Interface_Name (Scope (Rec_Type)), Pkg_Nm, Class_Nm);
               return Class_Nm;

            --  If no interface name was specified for the type's package,
            --  then just use the package's name with the first character
            --  converted to upper case.

            else
               declare
                  Java_Class_Name : String
                    := Name_String (Chars (Scope (Rec_Type)));
               begin
                  Java_Class_Name (1) := To_Upper (Java_Class_Name (1));

                  return Name (Java_Class_Name);
               end;
            end if;

         else
            return JVM_Entity_Name (Rec_Type);
         end if;
      end Class_Name;

   --  Start of processing for Declare_Record_Class

   begin
      --  For now we force the source file associated with a tagged type
      --  declared in a package with a body to be the body's source file
      --  ('.ads' => '.adb'). This is for the benefit of debugging and
      --  other tools that use source line info (the class needs to be
      --  associated with the body source file because the bodies of
      --  its primitive operations are tied to the class and generally
      --  occur in the package body). We also check for a ".dg" suffix
      --  to handle the case of source files created by -gnatD. ???

      if Src_Name /= No_Name
        and then Is_Tagged_Type (Rec_Type)
        and then Ekind (Scope (Rec_Type)) = E_Package
        and then Has_Completion (Scope (Rec_Type))
      then
         declare
            Src_String : String := Name_String (Src_Name);

         begin
            if Src_String (Src_String'Last - 1 .. Src_String'Last) = "dg" then
               Src_String (Src_String'Last - 3) := 'b';
            else
               Src_String (Src_String'Last) := 'b';
            end if;
            Src_Name := Name (Src_String);
         end;
      end if;

      --  If Rec_Type is a tagged type extension, then set its superclass
      --  to be its parent type's associated class. (The complicated test
      --  for full views is needed to handle the case of private types
      --  where the Etype of the full view references the partial view,
      --  so as to prevent treating such cases as extensions. Not clear
      --  why this funny use of Etype occurs. ???)

      if Is_Tagged_Type (Rec_Type)
        and then Etype (Rec_Type) /= Rec_Type
        and then (not Present (Full_View (Etype (Rec_Type)))
                   or else Full_View (Etype (Rec_Type)) /= Rec_Type)
      then
         Superclass := JVM_Class (Etype (Rec_Type));

         --  Check whether this is a Java-convention type that represents
         --  a Java interface type. It must be an abstract type with a
         --  single access discriminant whose name is "self" and whose
         --  designated type is java.lang.Object'class (for now we
         --  don't check all of the above...).

         if Convention (Rec_Type) = Convention_Java
           and then Abst_Class
           and then Has_Discriminants (Rec_Type)
           and then Name_String (Chars (First_Discriminant (Rec_Type)))
                      = "self"
         then
            Is_Intface := True;
         end if;
      end if;

      --  If the type's scope is imported (must be a package), then
      --  we will share the scope's class with the record type. The
      --  scope's superclass will have been set to Java_Lang_Object,
      --  so it needs to be updated here along with the abstract
      --  attribute of the class.

      if Is_Imported (Scope (Rec_Type)) then
         Rec_Class := JVM_Class (Scope (Rec_Type));
         Set_Superclass (Rec_Class, Superclass);
         Set_Abstract (Rec_Class, Abst_Class);

         --  If the type represents a Java interface, then the
         --  existing class must be changed to an interface.

         if Is_Intface then
            Change_To_Interface (Rec_Class);
         end if;

      elsif Is_Intface then
         Rec_Class
           := New_Interface (Name     => Class_Name (Rec_Type),
                             Pkg_Name => Package_Name (Rec_Type),
                             Src_Name => Src_Name);
      else
         Rec_Class
           := New_Class (Name     => Class_Name (Rec_Type),
                         Pkg_Name => Package_Name (Rec_Type),
                         Src_Name => Src_Name,
                         Super    => Superclass,
                         Abstrct  => Abst_Class);
         Associate_Interface (Rec_Class, API_Interface (IO_Serializable));
      end if;


      Class_Type := Type_Of (Rec_Class);

      --  Declare the type's deep copy and clone routines if it needs them.
      --  Eventually we want to suppress these methods for limited types when
      --  possible, but there are difficulties with this determination
      --  (see comments at end of Generate_Record_Type).

      if Convention (Rec_Type) /= Convention_Java
        and then Convention (Scope (Rec_Type)) /= Convention_Java
      then
         Result_Type := Class_Type;

         Deep_Copy
           := New_Method
                (Rec_Class, Name ("_deep_copy"), Result_Type, Static => True);
         Target := New_Method_Parameter (Deep_Copy, "_target", Class_Type);
         Source := New_Method_Parameter (Deep_Copy, "_source", Class_Type);

         --  Deep clone is a parameterless instance method (allows
         --  dispatching and is needed for class-wide object declarations
         --  and allocators). Note that the result type is the class type
         --  associated with the type's ultimate ancestor, to ensure that
         --  the method is treated as overriding.

         if Root_Type (Rec_Type) /= Base_Type (Rec_Type) then
            --  If Rec_Type is already a root-level type then we don't want to
            --  call JVM_Type since that would result in unbounded recursion.

            Result_Type := JVM_Type (Root_Type (Rec_Type));
         end if;

         Deep_Clone
           := New_Method
               (Rec_Class, Name ("_deep_clone"), Result_Type, Static => False);
      end if;

      Set_Map (Rec_Type, Class_Type);

      if Is_Tagged_Type (Rec_Type) then
         Set_Map (Class_Wide_Type (Rec_Type), Class_Type);
      end if;
   end Declare_Record_Class;

   ------------------------
   -- Declare_Array_Type --
   ------------------------

   procedure Declare_Array_Type (Arr_Type : Entity_Id) is
      Comp_Type  : constant Entity_Id := Full_Type (Component_Type (Arr_Type));
      Elmt_Type  : Type_Id;
      Array_Type : Type_Id;
      Array_Name : Name_Id;
      Deep_Copy  : Method_Id;
      Target     : Local_Var_Id;
      Trg_Start  : Local_Var_Id;
      Source     : Local_Var_Id;
      Src_Count  : Local_Var_Id;
      Src_Start  : Local_Var_Id;

   begin
      if Has_Aliased_Components (Arr_Type)
        and then Ekind (Comp_Type) in Wrappable_Kind
      then
         Elmt_Type := Wrapper_Type (Comp_Type);
         Array_Name
           := Name (Name_String (Name (Class_Of_Type (Elmt_Type))) & "[]");

      else
         Elmt_Type := JVM_Type (Comp_Type);

         Array_Name
           := Name (Name_String (Chars (Component_Type (Arr_Type))) & "[]");
      end if;

      Array_Type
        := New_Array_Type
             (Elmt_Type, Pos_8 (Number_Dimensions (Arr_Type)), Array_Name);

      Set_Map (Arr_Type, Array_Type);

      --  If the array type has composite components or aliased elementary
      --  components, then Create a deep copy method w/parameters for the type.

      if not Is_Limited_Record (Full_Type (Component_Type (Arr_Type)))
        and then not Is_Concurrent_Type (Full_Type (Component_Type (Arr_Type)))
        and then
          (Ekind (Full_Type (Component_Type (Arr_Type))) not in Elementary_Kind
            or else Has_Aliased_Components (Arr_Type)
            or else Number_Dimensions (Arr_Type) > 1)
      then
         Deep_Copy
           := New_Method
                (Deep_Copy_Class (Arr_Type),
                 Deep_Copy_Method_Name (Arr_Type),
                 Array_Type, Static => True);

         Target    := New_Method_Parameter (Deep_Copy, "_target", Array_Type);
         Trg_Start := New_Method_Parameter (Deep_Copy, "_trgstart", Int_Type);
         Source    := New_Method_Parameter (Deep_Copy, "_source", Array_Type);
         Src_Count := New_Method_Parameter (Deep_Copy, "_srccount", Int_Type);
         Src_Start := New_Method_Parameter (Deep_Copy, "_srcstart", Int_Type);

         --  There are cases where Itypes need to be translated that are not
         --  via Itype_References. In particular, this occurs in certain cases
         --  for component initializations created for initializing packed
         --  array objects from an aggregate. This ensures that any deep_copy
         --  routine needed by the type will have its body generated. However,
         --  we only want to do this call if the method's class file is being
         --  generated currently.

         if Is_Itype (Arr_Type)
           and then Class_File_Is_Open (Class_Of (Deep_Copy))
         then
            Translate_Type (Arr_Type);
         end if;
      end if;
   end Declare_Array_Type;

   -------------------------
   -- Declare_Access_Type --
   -------------------------

   procedure Declare_Access_Type (Acc_Type : Entity_Id) is
      Designated_Subt  : constant Entity_Id := Designated_Type (Acc_Type);
      Designated_Atype : constant Entity_Id := Full_Type (Designated_Subt);
      Designated_Jtype : Type_Id            := Null_Type;
      Arr_Ref_Class    : Class_Id;
      Arr_Ref_Field    : Field_Id;
      Dimensions       : Pos;

   begin
      --  If the access type designates another access type, then
      --  don't set the designated JVM type. This avoids problems
      --  with infinite recursion when declaring circular access
      --  types and the access type will be associated with an access
      --  wrapper type so we don't need to know the real designated
      --  type at this point.

      if Ekind (Designated_Atype) not in Access_Kind then
         Designated_Jtype := JVM_Type (Designated_Atype);
      end if;

      if Designated_Jtype = Null_Type
        or else JVM.Type_Kind (Designated_Jtype) /= JVM.Array_Kind
        or else Is_Constrained (Designated_Subt)
        or else Convention (Scope (Designated_Atype)) = Convention_Java
      then
         --  If the designated type is a scalar or (non-subprogram) access
         --  type, then the designated JVM type must be a wrapper type.

         if Ekind (Designated_Atype) in Wrappable_Kind then
            Set_Map (Acc_Type, Wrapper_Type (Designated_Atype));
         else
            Set_Map (Acc_Type, Designated_Jtype);
         end if;

      --  The access type designates an unconstrained array type

      else
         Dimensions := Number_Dimensions (Designated_Atype);

         --  If the access-to-unconstrained-array type is declared
         --  in a Java-convention scope, then the access type is
         --  represented as a normal array reference type, instead
         --  of using a class to contain the bounds (the parent
         --  package is imported from Java, so the class would
         --  never have a chance to be generated in any case,
         --  although we could potentially provide precompiled
         --  classes for various dimensionalities with array
         --  components having Object elements). The bounds must
         --  be synthesized when needed.

         if Convention (Scope (Acc_Type)) = Convention_Java then
            Set_Map (Acc_Type, JVM_Type (Designated_Atype));

            return;
         end if;

         --  In the case of an access-to-unconstrained type designating
         --  an array of Character, we use the predefined class associated
         --  with Standard.Access_String. This sharing affords the small
         --  optimization of avoiding an extra class for a common case,
         --  and also happens to ensure that enumeration literal tables
         --  can be legitimately converted (such a conversion occurs in the
         --  current version of function System.Val_Enum.Value_Enumeration).

         if Dimensions = 1
           and then Component_Type (Designated_Atype) = Standard_Character
         then
            Set_Map (Acc_Type, Type_Of (API_Class (Ada_Access_String)));

            return;
         end if;

         --  Create a descriptor class to encapsulate the array reference
         --  and its bounds (???, including a parameterized constructor
         --  that will initialize the descriptor ???).

         Arr_Ref_Class
           := New_Class (JVM_Entity_Name (Acc_Type), Package_Name (Acc_Type));

         Set_Map (Acc_Type, Type_Of (Arr_Ref_Class));

         Arr_Ref_Field := New_Field (Arr_Ref_Class, Name ("all"),
                                     Designated_Jtype, Static => False);

         Arr_Ref_Field := New_Field (Arr_Ref_Class, Name ("first"),
                                     Int_Type, Static => False);

         Arr_Ref_Field := New_Field (Arr_Ref_Class, Name ("last"),
                                     Int_Type, Static => False);

         --  For a multidimensional array, create additional fields for
         --  each dimension's upper and lower bound.

         while Dimensions > 1 loop
            Arr_Ref_Field
               := New_Field (Arr_Ref_Class,
                             Name ("first_" & Image (Int_8 (Dimensions))),
                             Int_Type, Static => False);
            Arr_Ref_Field
               := New_Field (Arr_Ref_Class,
                             Name ("last_" & Image (Int_8 (Dimensions))),
                             Int_Type, Static => False);
            Dimensions := Dimensions - 1;
         end loop;
      end if;
   end Declare_Access_Type;

   ------------------------------------
   -- Declare_Subprogram_Access_Type --
   ------------------------------------

   procedure Declare_Subprogram_Access_Type (Subp_Acc_Type : Entity_Id) is
      Subp_Profile   : Entity_Id := Directly_Designated_Type (Subp_Acc_Type);
      Formal         : Entity_Id := First_Formal (Subp_Profile);
      JVM_Formal     : Local_Var_Id;
      Subp_Acc_Class : constant Class_Id
        := New_Class (Name     => JVM_Entity_Name (Subp_Acc_Type),
                      Pkg_Name => Package_Name (Subp_Acc_Type),
                      Src_Name => Source_Name (Sloc (Subp_Acc_Type)),
                      Abstrct  => True);
      Subp_Method    : constant Method_Id
        := New_Method (Subp_Acc_Class,
                       J_String.Name ("Invoke"),
                       JVM_Type (Etype (Subp_Profile)),
                       Static  => False,
                       Abstrct => True);

   begin
      Set_Map (Subp_Acc_Type, Type_Of (Subp_Acc_Class));

      --  Create the formal parameters for the abstract invocation method
      --  associated with the access-to-subprogram type's class.

      while Present (Formal) loop
         JVM_Formal := New_Method_Parameter
                         (Subp_Method, Chars (Formal), JVM_Type (Formal));
         Set_Map (Formal, JVM_Formal);
         Formal := Next_Formal_With_Extras (Formal);
      end loop;
   end Declare_Subprogram_Access_Type;

   ---------------------------
   -- Declare_Package_Class --
   ---------------------------

   procedure Declare_Package_Class (Pkg_Spec : Entity_Id) is
      Src_Name   : constant Name_Id := Source_Name (Sloc (Pkg_Spec));
      Src_String : String           := Name_String (Src_Name);
      Pkg_Class  : Class_Id;
      Class_Name : Name_Id;
      Pkg_Name   : String_Id;

   begin
      --  For now we force the source file associated with a package having
      --  a body to be the body's source file ('.ads' => '.adb'). This is for
      --  the benefit of debugging and other tools that use source line info.
      --  Eventually we need solve the problem of what to do about classes
      --  that are associated with more than one source file (may require
      --  generating multiple class files). We also check for a ".dg" suffix
      --  to handle the case of source files created by -gnatD. ???

      if Has_Completion (Pkg_Spec) then
         if Src_String (Src_String'Last - 1 .. Src_String'Last) = "dg" then
            Src_String (Src_String'Last - 3) := 'b';
         else
            Src_String (Src_String'Last) := 'b';
         end if;
      end if;

      --  If the package has a specified interface name, then extract the
      --  class name and (optional) Java package name.

      if Present (Interface_Name (Pkg_Spec)) then
         Get_Interface_Names (Interface_Name (Pkg_Spec), Pkg_Name, Class_Name);

      else
         Class_Name := JVM_Entity_Name (Pkg_Spec);
         Pkg_Name   := Package_Name (Pkg_Spec);
      end if;

      Pkg_Class
        := New_Class (Name     => Class_Name,
                      Pkg_Name => Pkg_Name,
                      Src_Name => Name (Src_String),
                      Final    => True);

      Set_Map (Pkg_Spec, Pkg_Class);
   end Declare_Package_Class;

   -------------------
   -- Declare_Field --
   -------------------

   procedure Declare_Field (Class : Class_Id; Obj_Or_Comp : Entity_Id) is
      Kind       : constant Entity_Kind := Ekind (Obj_Or_Comp);
      Field      : Field_Id;
      Orig       : Entity_Id;
      F_Type     : Type_Id := JVM_Type (Obj_Or_Comp);
      F_Class    : Class_Id := Class;
      F_Name     : Name_Id;
      Pkg_Name   : String_Id;
      Class_Name : Name_Id;

   begin
      --  If this is a record component with an associated corresponding
      --  discriminant or original component, then map the new component
      --  to the field of the other component.

      if Kind = E_Component or else Kind = E_Discriminant then

         --  If this is _parent, then a field should not be declared for it

         if Chars (Obj_Or_Comp) = Name_uParent then
            return;
         end if;

         Orig := Original_Record_Component (Obj_Or_Comp);

         if Kind = E_Discriminant
           and then Present (Corresponding_Discriminant (Orig))
         then
            Set_Map
              (Obj_Or_Comp, JVM_Field (Corresponding_Discriminant (Orig)));
            return;

         elsif Present (Orig) and then Obj_Or_Comp /= Orig then
            Set_Map (Obj_Or_Comp, JVM_Field (Orig));
            return;

         --  If the class already contains a field with the same name then
         --  use that field. This can occur legitimately for the case of a
         --  component of an untagged derived record type with discriminants
         --  (the components of the derived type are duplicated in the
         --  GNAT tree and not connected to the corresponding components
         --  in the parent type). However, this could also indicate an
         --  error in processing, but it's not clear how else to solve
         --  this problem (unless we stopped sharing the class of the
         --  parent record type for this case). ???

         elsif
           JVM.Field (F_Class, JVM_Entity_Name (Obj_Or_Comp)) /= Null_Field
         then
            Set_Map (Obj_Or_Comp,
                     JVM.Field (F_Class, JVM_Entity_Name (Obj_Or_Comp)));
            return;
         end if;

      elsif Present (Renamed_Object (Obj_Or_Comp))
        and then Ekind (Full_Type (Obj_Or_Comp)) in Elementary_Kind
        and then Nkind (Renamed_Object (Obj_Or_Comp)) = N_Selected_Component
      then
         F_Type
           := JVM_Type (Full_Type (Prefix (Renamed_Object (Obj_Or_Comp))));
      end if;

      if (Is_Imported (Obj_Or_Comp) or else Is_Exported (Obj_Or_Comp))
        and then Present (Interface_Name (Obj_Or_Comp))
      then
         if Convention (Obj_Or_Comp) = Convention_Java
           and then not Is_Exported (Obj_Or_Comp)
         then
            Get_Interface_Names
              (Interface_Name (Obj_Or_Comp), Pkg_Name, Class_Name, F_Name);

            --  If the interface name included a class name then
            --  construct a new class entity to associate with
            --  the method.

            if Class_Name /= No_Name then
               F_Class := New_Class (Class_Name, Pkg_Name);
            end if;

         else
            F_Name := Name (Str (Strval (Interface_Name (Obj_Or_Comp))));
         end if;
      else
         F_Name := JVM_Entity_Name (Obj_Or_Comp);
      end if;

      Field := New_Field
                 (F_Class, F_Name, F_Type,
                  Static => (Kind = E_Variable or Kind = E_Constant));
      Set_Map (Obj_Or_Comp, Field);
   end Declare_Field;

   --------------------
   -- Declare_Method --
   --------------------

   procedure Declare_Method (Class : Class_Id; Subp : Entity_Id) is
      Method            : Method_Id;
      Formal            : Entity_Id := First_Formal (Subp);
      Bound             : Local_Var_Id;
      AR_Param          : Local_Var_Id;
      Ftyp              : Type_Id;
      Method_Name       : Name_Id   := JVM_Entity_Name (Subp);
      Class_Name        : Name_Id;
      Package_Name      : String_Id;
      Method_Class      : Class_Id  := Class;
      Parent_Subp       : Entity_Id := Enclosing_Subprogram (Subp);
      Parent_Method     : Method_Id := Null_Method;
      Controlling_Type  : Entity_Id;
      Parent_Ctrl_Type  : Entity_Id;
      JVM_Control_Type  : Type_Id   := Null_Type;
      This_Formal       : Entity_Id := Empty;
      Result_Type       : Type_Id;
      Constructor_Class : Class_Id;
      Disp_Method       : Method_Id := Null_Method;
      Disp_Formal       : Local_Var_Id;
      ND_Method         : Method_Id := Null_Method;
      ND_Formal         : Local_Var_Id;

      function Controlling_Parent_Type (Subp : Entity_Id) return Entity_Id;
      --  If Subp is a new operation of a tagged type, then this function
      --  simply returns that type. If Subp overrides some subprogram
      --  associated with a parent type, then the controlling type of
      --  that parent operation is returned (note that this will be the
      --  type associated with an original, new dispatching operation
      --  which may have been overridden multiple times prior to the
      --  declaration of Subp).

      function Dispatching_Name (Subp : Entity_Id) return Name_Id;
      --  Returns the Name_Id for the name of the dispatching method
      --  associated with a dispatching subprogram.

      function Controlling_Parent_Type (Subp : Entity_Id) return Entity_Id is
         Ctrl_Type        : Entity_Id := Find_Dispatching_Type (Subp);
         Prev_Parent_Type : Entity_Id := Full_Type (Ctrl_Type);
         Parent_Type      : Entity_Id := Full_Type (Etype (Prev_Parent_Type));
         Op_Position      : constant Uint := DT_Position (Subp);

      begin
         --  Traverse the parents of Subp's controlling type until we find
         --  one which has fewer primitive operations than the position
         --  number of Subp, in which case we return the type of the
         --  immediate child (Prev_Parent_Type) of the current parent type
         --  which must be the type that declared the original subprogram
         --  which is ultimately overridden by Subp (it will simply be
         --  the controlling type of Subp if Subp is a new operation
         --  to begin with). If no such type is found, then we stop at the
         --  ultimate ancestor type, which perforce must be the type we want.

         while Parent_Type /= Prev_Parent_Type
           and then DT_Entry_Count (Tag_Component (Parent_Type)) >= Op_Position
         loop
            Prev_Parent_Type := Parent_Type;
            Parent_Type := Full_Type (Etype (Parent_Type));
         end loop;

         --  If the subprogram is a new operation, then return the
         --  controlling type rather than its full type, so we can
         --  ensure that the caller will see a match when comparing
         --  the result of this function with the subprogram's
         --  dispatching type.

         if Prev_Parent_Type = Full_Type (Ctrl_Type) then
            return Ctrl_Type;
         else
            return Prev_Parent_Type;
         end if;
      end Controlling_Parent_Type;

      function Dispatching_Name (Subp : Entity_Id) return Name_Id is
      begin
         if Has_Nondispatching_Method (Subp) then
            return Name (Next_Method (JVM_Method (Subp)));
         else
            return Name (JVM_Method (Subp));
         end if;
      end Dispatching_Name;

   --  Start of processing for Declare_Method

   begin
      --  Intrinsic subprograms are not declared since they
      --  have no bodies and will not correspond to any method.

      if Is_Intrinsic_Subprogram (Subp)
        or else Convention (Subp) = Convention_Assembler
      then
         return;
      end if;

      --  If the subprogram is nested within another subprogram, then
      --  create an activation record class for the containing subprogram
      --  that will be used for holding objects that are referenced up-level
      --  from the nested subprogram.

      if Present (Parent_Subp)
         and then not Is_Imported (Subp)
      then
         Parent_Method := Current_Method;

         --  We use the call to the more verbose form of Make_Activation_Record
         --  here because of problems with applying JVM_Method to Parent_Subp
         --  in certain cases (showed up with cases involving accept statements
         --  where Enclosing_Subprogram was returning an E_Entry node due to
         --  an intervening block whose Scope attribute referenced the entry
         --  instead of the expanded enclosing subprogram (see comment in
         --  Jx_Uplev.Enclosing_Method). ???

         if AR_Stack.Empty or else AR_Stack.Top.Method /= Parent_Method then
            Make_Activation_Record
              (Parent_Method,
               Name (JVM_Expanded_Name (Parent_Subp)),
               Jx_Decl.Package_Name (Parent_Subp));
         end if;
      end if;

      --  Map subprograms marked as constructors to <init>

      if Is_Constructor (Subp) then

         --  For now, require that the constructor is a function returning
         --  access to a Java-convention type.

         pragma Assert (Ekind (Subp) = E_Function
           and then Ekind (Etype (Subp)) in Access_Kind
           and then
             (Convention
               (Full_Type
                 (Directly_Designated_Type (Etype (Subp)))) = Convention_Java
               or else
                 Convention
                   (Full_Type
                     (Root_Type (Directly_Designated_Type (Etype (Subp)))))
                       = Convention_Java));

         Constructor_Class
           := JVM_Class (Full_Type (Directly_Designated_Type (Etype (Subp))));

         --  If the function has no parameters, then use the type's default
         --  (no-arg) constructor. (But user-defined constructors with no
         --  formals do not make sense, so they should probably be rejected
         --  by the front end. ???)

         if not Present (Formal) then
            Method := Default_Constructor (Constructor_Class);

         --  If the function has a single 'this' parameter, then associate
         --  the function with the type's default (no-arg) constructor.

         elsif not Present (Next_Formal (Formal))
           and then Name_String (Chars (Formal)) = "this"
         then
            Method := Default_Constructor (Constructor_Class);
            Set_Map (Formal, This_Local (Method));
            This_Formal := Formal;

         --  Otherwise we create a new method and check for a 'this' formal

         else
            Method := New_Method (Constructor_Class, J_String.Name ("<init>"),
                                  Void_Type, Static => False);

            This_Formal := First_Formal (Subp);

            while Present (This_Formal)
              and then Name_String (Chars (This_Formal)) /= "this"
            loop
               Next_Formal (This_Formal);
            end loop;

            --  If the method has a formal with name 'this', then establish
            --  it as the method's 'this' argument.

            if Present (This_Formal) then
               Set_Map (This_Formal, This_Local (Method));
            end if;
         end if;


      --  Calls to imported subprograms with convention C are mapped into
      --  calls to methods in the special run-time support class GNAT_libc.

      elsif Is_Imported (Subp) and then Convention (Subp) = Convention_C then

         if Present (Interface_Name (Subp)) then
            Method_Name := Name (Str (Strval (Interface_Name (Subp))));
         else
            Method_Name := Chars (Subp);
         end if;

         Method := New_Method (API_Class (GNAT_libc),
                               Method_Name,
                               JVM_Type (Subp),
                               Static => True);

      --  Calls to imported subprograms with convention Ada are mapped into
      --  normal calls to methods in the run time.

      elsif Is_Imported (Subp)
        and then Convention (Subp) /= Convention_Java
      then
         --  ??? For now we require that imported subprograms have only Java,
         --  C, and Ada conventions (note that processing for conventions
         --  C and Java is handled separately).

         if Convention (Subp) /= Convention_Ada then
            Error_Msg_N ("subprogram has unsupported import convention", Subp);
         end if;

         --  ??? For now, require that Subp has an interface name.
         --  The front end currently creates a default Interface_Name
         --  in the absence of one given by the user, but we check here
         --  as a defensive measure in case that ever changes.

         if Present (Interface_Name (Subp)) then
            Get_Interface_Names
              (Interface_Name (Subp), Package_Name, Class_Name, Method_Name);

         else
            Error_Msg_N
              ("interface name required for imported subprogram", Subp);
            Class_Name := No_Name;
            Method_Name := Chars (Subp);
         end if;

         --  If the interface name is a simple name (i.e., has no class
         --  name as a prefix), then we associate the subprogram with
         --  class GNAT_libc, just like the case of convention C.
         --  This is a little odd, but is needed to handle cases
         --  where the GNAT run-time uses an Import from Ada, but
         --  we supply the corresponding routine in GNAT_libc (because
         --  otherwise we have no idea where to find the subprogram).
         --  This isn't really a restriction on users, because if
         --  the user wants to import from Ada for some weird reason,
         --  they have to provide a class name in any case. However,
         --  we would really prefer not to have this special-case
         --  treatment for simple interface names, and would rather
         --  issue an error message (if a user does this by accident
         --  he will get an error from the JVM, but it could be
         --  very confusing). ???

         if Class_Name = No_Name then
            Method := New_Method (API_Class (GNAT_libc),
                                  Method_Name,
                                  JVM_Type (Subp),
                                  Static => True);

         else
            Method := New_Method (New_Class (Class_Name, Package_Name),
                                  Method_Name,
                                  JVM_Type (Subp),
                                  Static => True);
         end if;

      --  If this is a derived subprogram, then associate it with the
      --  method associated with its parent subprogram. This will
      --  will have the effect of mapping the derived subprogram to
      --  the method associated with the nearest ancestor that is
      --  a nonderived subprogram. In this way we avoid creating
      --  a new method for the class, which would never have a method
      --  body created for it in any case, and this appropriately
      --  reflects the inherited nature of the derived subprogram.
      --  We then return since no further actions are needed.

      elsif Present (Alias (Subp)) then
         Set_Map (Subp, JVM_Method (Alias (Subp)));

         return;

      --  Treat this as a normal subprogram (but includes calls to subprograms
      --  imported from Java).

      else
         Result_Type := JVM_Type (Subp);

         if Is_Dispatching_Operation (Subp) then
            --  If this is an overriding dispatching operation, then each of
            --  the controlling formals (and result) of the method must have
            --  the type of the corresponding formal (or result) in the parent
            --  method that is being overridden. This is necessary to satisfy
            --  the Java/JVM rules for overriding, which require the formal
            --  types and result in an overriding method to match that of the
            --  overridden parent operation.

            Controlling_Type := Find_Dispatching_Type (Subp);
            Parent_Ctrl_Type := Controlling_Parent_Type (Subp);
            JVM_Control_Type := JVM_Type (Parent_Ctrl_Type);

            --  For interfaces we have to suppress declaration of any methods
            --  implicitly generated by the front end (e.g., Size functions),
            --  since interfaces are only allowed to have abstract operations
            --  and a user declaraing a type implementing the interface must
            --  know about the operations so they can be overridden. Generation
            --  of the body is also suppressed (see Jx_Ch6.Generate_Method).

            if Is_Interface (JVM_Class (Controlling_Type)) then
               if not Comes_From_Source (Subp) then
                  return;
               end if;

               pragma Assert (Is_Abstract (Subp));
            end if;

            --  We establish the appropriate controlling result type here.
            --  The types of controlling formal parameters are set to the
            --  controlling JVM type further below.

            if Has_Controlling_Result (Subp) then
               Result_Type := JVM_Control_Type;
            end if;

            --  If this is an overriding dispatching subprogram, then retrieve
            --  the method name of the subprogram that is overridden to use as
            --  the new method's name.

            if Controlling_Type /= Parent_Ctrl_Type then
               declare
                  Prim_Op : Elmt_Id
                    := First_Elmt (Primitive_Operations (Parent_Ctrl_Type));

               begin
                  while Present (Prim_Op) loop
                     if DT_Position (Node (Prim_Op))
                       = DT_Position (Subp)
                     then
                        Method_Name := Dispatching_Name (Node (Prim_Op));
                        exit;
                     end if;

                     Next_Elmt (Prim_Op);
                  end loop;

                  --  If we did not find the corresponding operation of the
                  --  parent type then something is wrong.

                  pragma Assert (Present (Prim_Op));
               end;

            --  If the type has any access discriminants that designate
            --  a tagged type mapped onto a Java interface, then we have
            --  to search the primitives of each such interface type
            --  for a possible overriding, in which case we want to use
            --  the external name associated with the overridden subprogram.

            elsif Convention (Controlling_Type) = Convention_Java
              and then Has_Discriminants (Controlling_Type)
            then
               declare
                  Discr   : Entity_Id
                    := First_Discriminant (Controlling_Type);
                  Desig   : Entity_Id;
                  Prim_Op : Elmt_Id;

               begin
                  while Present (Discr) loop
                     if Ekind (Etype (Discr)) in Access_Kind then
                        Desig := Directly_Designated_Type (Etype (Discr));

                        if Is_Tagged_Type (Desig)
                          and then Is_Interface (JVM_Class (Desig))
                        then
                           if Is_Class_Wide_Type (Desig) then
                              Desig := Root_Type (Desig);
                           end if;

                           Desig := Full_Type (Desig);

                           if Has_Primitive_Operations (Desig) then
                              Prim_Op
                                := First_Elmt (Primitive_Operations (Desig));

                              while Present (Prim_Op) loop
                                 if Overrides_Interface_Op
                                      (Subp, Node (Prim_Op))
                                 then
                                    Method_Name
                                      := Dispatching_Name (Node (Prim_Op));
                                    exit;
                                 end if;

                                 Next_Elmt (Prim_Op);
                              end loop;
                           end if;
                        end if;
                     end if;

                     Next_Discriminant (Discr);
                  end loop;
               end;
            end if;
         end if;

         --  For Java-imported subprograms with access-to-unconstrained-array
         --  result types, we use the array reference type itself rather than
         --  the Ada bounds wrapper class for the result type.

         if Convention (Subp) = Convention_Java
           and then Ekind (Full_Type (Subp)) in Access_Kind
           and then Ekind (Designated_Type (Full_Type (Subp)))
                      in Einfo.Array_Kind
           and then not Is_Constrained (Designated_Type (Full_Type (Subp)))
         then
            Result_Type := JVM_Type (Designated_Type (Full_Type (Subp)));
         end if;

         --  If the subprogram has convention Java, then retrieve
         --  the specified interface name as the method name.

         if Convention (Subp) = Convention_Java
           and then Present (Interface_Name (Subp))
         then
            Get_Interface_Names
              (Interface_Name (Subp), Package_Name, Class_Name, Method_Name);

            --  If the interface name included a class name then
            --  construct a new class entity to associate with
            --  the method.

            if Class_Name /= No_Name then
               Method_Class := New_Class (Class_Name, Package_Name);

            --  Java-imported subprograms must have a user-provided
            --  class name unless declared within a Java-imported
            --  package, so issue an error if this condition is violated,
            --  and use GNAT_libc as the default class, just to avoid
            --  later blow-ups (we need to use an external class
            --  because otherwise the method will appear incomplete
            --  and End_Class_File will fail).

            elsif Is_Imported (Subp)
              and then (Ekind (Scope (Subp)) /= E_Package
                         or else Convention (Scope (Subp)) /= Convention_Java)
            then
               Error_Msg_N
                 ("class name required for Java import",
                  Interface_Name (Subp));
               Method_Class := API_Class (GNAT_libc);
            end if;
         end if;

         --  For each dispatching operation we create a "shadow" static method
         --  that supports nondispatching calls. The name of this method is
         --  the name of the dispatching method concatenated with "$ND" and
         --  by convention it is created as the successor of the dispatching
         --  method. This special static method will have the same kind of
         --  formal parameters as the dispatching method (including a leading
         --  parameter corresponding to the 'this' parameter of the dispatching
         --  dispatching method). If the subprogram is abstract, then the
         --  special method is not needed since abstract subprograms of tagged
         --  types can only be called via dispatching calls. (Also, if the
         --  convention of the subprogram's is Java, we don't declare the
         --  nondispatching method since the corresponding Java class has
         --  no such operation.)

         --  The nondispatching subprogram will actually be the method
         --  associated with the subprogram (happens at the end of this
         --  routine). This is done because the code for the subprogram
         --  body will actually be generated within the nondispatching method.
         --  The dispatching method is deliberately created immediately
         --  after the nondispatching method here, and this fact is
         --  depended on elsewhere, in particular for generating dispatching
         --  calls (we depend on the dispatching method being the successor
         --  of the nondispatching method).

         if Has_Nondispatching_Method (Subp) then
            ND_Method := New_Method
                           (Class,
                            Name (Name_String (Method_Name) & "$ND"),
                            Result_Type,
                            Static  => True,
                            Abstrct => False,
                            Parent  => Parent_Method);
            ND_Formal
              := New_Method_Parameter
                   (ND_Method, J_String.Name ("$this"), Type_Of (Class));
         end if;

         Method := New_Method
                     (Method_Class,
                      Method_Name,
                      Result_Type,
                      Static  => not Is_Dispatching_Operation (Subp),
                      Abstrct => Is_Abstract (Subp),
                      Parent  => Parent_Method);

         --  If there is a nondispatching method associated with a
         --  dispatching subprogram, then let Method denote the
         --  nondispatching method, and the parameters created below
         --  will be associated with that method instead of the
         --  dispatching method (required since the code will be
         --  generated with the nondispatching method).

         if ND_Method /= Null_Method then
            Disp_Method := Method;
            Method := ND_Method;
         end if;

         --  For dispatching operations, associate the first controlling
         --  formal, if any, with the method's 'this' argument.

         if Is_Dispatching_Operation (Subp) then
            This_Formal := First_Formal (Subp);

            while Present (This_Formal)
              and then not Is_Controlling_Formal (This_Formal)
            loop
               Next_Formal (This_Formal);
            end loop;

            --  If the method has any controlling formals then establish
            --  the first of these as the method's 'this' argument.

            if Present (This_Formal) then
               --  We call First_Local_Var here instead of This_Local because
               --  Method is the subprogram's static (nondispatching) method,
               --  so technically it doesn't have a 'this' parameter (and
               --  This_Local would blow up if we tried to call it).

               Set_Map (This_Formal, First_Local_Var (Method));

            --  If there is no controlling formal available, then this must
            --  be a function with a controlling result. In that case the
            --  method's 'this' parameter will not be associated with any
            --  Ada entity.

            else
               pragma Assert (Base_Type (Etype (Subp)) = Controlling_Type);
               null;
            end if;
         end if;
      end if;

      while Present (Formal) and then Ekind (Formal) in Formal_Kind loop
         Ftyp := JVM_Type (Formal);

         --  For access-to-unconstrained-array formals of Java-convention
         --  subprograms, we use the array reference type itself rather
         --  than the Ada bounds wrapper class for the parameter type,
         --  but only if the designated array type comes from a scope
         --  with convention Java.

         if Convention (Subp) = Convention_Java
           and then Ekind (Full_Type (Formal)) in Access_Kind
           and then Ekind (Designated_Type (Full_Type (Formal)))
                      in Einfo.Array_Kind
           and then not Is_Constrained (Designated_Type (Full_Type (Formal)))
           and then Convention (Scope (Designated_Type (Full_Type (Formal))))
                      = Convention_Java
         then
            Ftyp := JVM_Type (Designated_Type (Full_Type (Formal)));
         end if;

         --  If this is the controlling formal parameter of a dispatching
         --  subprogram, then it will already be associated with the method's
         --  'this' argument, otherwise we create a new method parameter
         --  and associate it with the Ada formal parameter.

         if not Present (This_Formal) or else Formal /= This_Formal then
            if Is_Controlling_Formal (Formal) then
               Set_Map
                 (Formal,
                  New_Method_Parameter
                    (Method, Chars (Formal), JVM_Control_Type));
            else
               Set_Map
                 (Formal, New_Method_Parameter (Method, Chars (Formal), Ftyp));
            end if;
         end if;

         --  Create a pair of parameters for the lower and upper bounds of
         --  an array formal for each dimension of the array. Note that
         --  the bounds are suppressed if the subprogram is imported.

         if Ekind (Etype (Formal)) in Einfo.Array_Kind
           and then not Is_Constrained (Etype (Formal))
           and then not Is_Imported (Subp)
         then
            for Dimension in 1 .. Number_Dimensions (Etype (Formal)) loop
               if Dimension = 1 then
                  Bound := New_Method_Parameter
                             (Method,
                              Name (Name_String (Chars (Formal)) & "__first"),
                              Int_Type);
                  Bound := New_Method_Parameter
                             (Method,
                              Name (Name_String (Chars (Formal)) & "__last"),
                              Int_Type);

               else
                  Bound := New_Method_Parameter
                             (Method,
                              Name (Name_String (Chars (Formal))
                                     & "__first_" & Image (Int_8 (Dimension))),
                              Int_Type);
                  Bound := New_Method_Parameter
                             (Method,
                              Name (Name_String (Chars (Formal))
                                     & "__last_" & Image (Int_8 (Dimension))),
                              Int_Type);
               end if;
            end loop;
         end if;

         Formal := Next_Formal_With_Extras (Formal);
      end loop;

      --  If this is a method for a nested subprogram, then add a parameter
      --  for the method's static link.

      if Present (Enclosing_Subprogram (Subp))
        and then not Is_Imported (Subp)
        and then not AR_Stack.Empty
      then
         AR_Param
           := New_Method_Parameter
                (Method, Name ("__AR_SL"), Type_Of (AR_Stack.Top.AR_Class));
      end if;

      if ND_Method = Null_Method then
         Set_Map (Subp, Method);

      --  We have a nondispatching method, so we have to create an
      --  equivalent set of formal parameters for its associated
      --  dispatching method and associate the subprogram itself
      --  with the nondispatching method. Note that we have to
      --  skip the first parameter, since the dispatching method
      --  already has a 'this' parameter (created implicitly
      --  by New_Method).

      else
         ND_Formal := First_Local_Var (Method);
         ND_Formal := Next_Local_Var (ND_Formal);

         while ND_Formal /= Null_Local_Var loop
            Disp_Formal
              := New_Method_Parameter
                   (Disp_Method, JVM.Name (ND_Formal), Type_Of (ND_Formal));

            ND_Formal := Next_Local_Var (ND_Formal);
         end loop;

         Set_Map (Subp, ND_Method);
      end if;
   end Declare_Method;

   ----------------------------
   -- Declare_Local_Variable --
   ----------------------------

   procedure Declare_Local_Variable (Object : Entity_Id) is
      Local_Var : Local_Var_Id;
      LV_Type   : Type_Id := JVM_Type (Object);

   begin
      if Present (Renamed_Object (Object))
        and then Ekind (Full_Type (Object)) in Elementary_Kind
        and then Nkind (Renamed_Object (Object)) = N_Selected_Component
      then
         LV_Type := JVM_Type (Full_Type (Prefix (Renamed_Object (Object))));
      end if;

      Local_Var := New_Local_Var (Chars (Object), LV_Type);

      Set_Map (Object, Local_Var);
   end Declare_Local_Variable;

   -----------------------------
   -- Declare_Exception_Class --
   -----------------------------

   procedure Declare_Exception_Class (Exc : Entity_Id) is
      Exc_Class  : Class_Id;
      Superclass : Class_Id         := API_Class (Lang_RuntimeException);
      Src_Name   : constant Name_Id := Source_Name (Sloc (Exc));
      Pkg_Name   : String_Id;
      Class_Name : Name_Id;
      Exc_Constr : Method_Id;
      Str_Param  : Local_Var_Id;

   begin
      if Convention (Exc) = Convention_Java
        and then Present (Interface_Name (Exc))
      then
         Get_Interface_Names (Interface_Name (Exc), Pkg_Name, Class_Name);

         --  If the exception is imported, assume that its superclass
         --  is java.lang.Throwable. We don't have a way to determine
         --  its real superclass, but it doesn't really matter since
         --  for the purposes of referencing an imported exception
         --  its superclass isn't needed (so this assignment is
         --  just a formality).

         if Is_Imported (Exc) then
            Superclass := API_Class (Lang_Throwable);
         end if;

      else
         Pkg_Name   := Package_Name (Exc);
         Class_Name := JVM_Entity_Name (Exc);
      end if;

      Exc_Class := New_Class (Name     => Class_Name,
                              Pkg_Name => Pkg_Name,
                              Src_Name => Src_Name,
                              Super    => Superclass);
      Set_Map (Exc, Type_Of (Exc_Class));

      --  Declare the exception class's string-parameterized constructor

      Exc_Constr
        := New_Method (Exc_Class, Name ("<init>"), Void_Type, False);
      Str_Param
        := New_Method_Parameter
             (Exc_Constr, Name ("message"), Type_Of (API_Class (Lang_String)));
   end Declare_Exception_Class;

   -------------------
   -- Declare_Label --
   -------------------

   procedure Declare_Label (Label : Entity_Id) is
   begin
      Set_Map (Label, New_Label);
   end Declare_Label;

   --------------------------------
   -- Generate_Class_Init_Method --
   --------------------------------

   procedure Generate_Class_Init_Method (Class : Class_Id) is
      Clinit : constant Method_Id
        := New_Method (Class, Name ("<clinit>"), Void_Type, Static => True);

   begin
      --  For now we just generate an empty method ???

      Open_Method (Clinit);
      Set_Current_Method (Clinit);
      Method_Stack.Push (Clinit);
      Gen_Method_Return;
      Method_Stack.Pop;
      Close_Method (Clinit);

      if not Method_Stack.Empty then
         Set_Current_Method (Method_Stack.Top);
      end if;
   end Generate_Class_Init_Method;

   ----------------------------------
   -- Generate_Default_Constructor --
   ----------------------------------

   procedure Generate_Default_Constructor (Class : Class_Id) is
      Init : constant Method_Id := Default_Constructor (Class);

   begin
      --  For now we generate a method that just calls the default
      --  constructor for class java.lang.Object. ???

      Open_Method (Init);
      Set_Current_Method (Init);
      Method_Stack.Push (Init);
      Gen_Load_Local (This_Local (Init));
      Gen_Invoke_Special (Default_Constructor (Superclass (Class)));
      Gen_Method_Return;
      Method_Stack.Pop;
      Close_Method (Init);

      if not Method_Stack.Empty then
         Set_Current_Method (Method_Stack.Top);
      end if;
   end Generate_Default_Constructor;

end Jx_Decl;
