------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T B U I L D                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.96 $
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Restrict; use Restrict;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Uintp;    use Uintp;

package body Tbuild is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Unique_Serial_Number;
   --  Add a unique serialization to the string in the Name_Buffer. This
   --  consists of a unit specific serial number, and b/s for body/spec.

   ------------------------------
   -- Add_Unique_Serial_Number --
   ------------------------------

   procedure Add_Unique_Serial_Number is
      Unit_Node : constant Node_Id := Unit (Cunit (Current_Sem_Unit));

   begin
      Add_Nat_To_Name_Buffer (Increment_Serial_Number);

      --  Add either b or s, depending on whether current unit is a spec
      --  or a body. This is needed because we may generate the same name
      --  in a spec and a body otherwise.

      Name_Len := Name_Len + 1;

      if Nkind (Unit_Node) = N_Package_Declaration
        or else Nkind (Unit_Node) = N_Subprogram_Declaration
        or else Nkind (Unit_Node) in N_Generic_Declaration
      then
         Name_Buffer (Name_Len) := 's';
      else
         Name_Buffer (Name_Len) := 'b';
      end if;
   end Add_Unique_Serial_Number;

   ----------------
   -- Checks_Off --
   ----------------

   function Checks_Off (N : Node_Id) return Node_Id is
   begin
      return
        Make_Unchecked_Expression (Sloc (N),
          Expression => N);
   end Checks_Off;

   ----------------
   -- Convert_To --
   ----------------

   function Convert_To (Typ : Entity_Id; Expr : Node_Id) return Node_Id is
      Result : Node_Id;

   begin
      if Present (Etype (Expr))
        and then (Etype (Expr)) = Typ
      then
         return Relocate_Node (Expr);
      else
         Result :=
           Make_Type_Conversion (Sloc (Expr),
             Subtype_Mark => New_Occurrence_Of (Typ, Sloc (Expr)),
             Expression => Relocate_Node (Expr));

         Set_Etype (Result, Typ);
         return Result;
      end if;
   end Convert_To;

   -----------------------
   -- Make_DT_Component --
   -----------------------

   function Make_DT_Component
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      I    : Positive)
      return Node_Id
   is
      X : Node_Id;
      Full_Type : Entity_Id := Typ;

   begin
      if Is_Private_Type (Typ) then
         Full_Type := Underlying_Type (Typ);
      end if;

      X := First_Component (
             Designated_Type (Etype (Access_Disp_Table (Full_Type))));

      for J in 2 .. I loop
         X := Next_Component (X);
      end loop;

      return New_Reference_To (X, Loc);
   end Make_DT_Component;

   --------------------
   -- Make_DT_Access --
   --------------------

   function Make_DT_Access
     (Loc  : Source_Ptr;
      Rec  : Node_Id;
      Typ  : Entity_Id)
      return Node_Id
   is
      Full_Type : Entity_Id := Typ;

   begin
      if Is_Private_Type (Typ) then
         Full_Type := Underlying_Type (Typ);
      end if;

      return
        Unchecked_Convert_To (
          New_Occurrence_Of (Etype (Access_Disp_Table (Full_Type)), Loc),
          Make_Selected_Component (Loc,
            Prefix => New_Copy (Rec),
            Selector_Name =>
              New_Reference_To (Tag_Component (Full_Type), Loc)));
   end Make_DT_Access;

   --------------------------------
   -- Make_Implicit_If_Statement --
   --------------------------------

   function Make_Implicit_If_Statement
     (Node            : Node_Id;
      Condition       : Node_Id;
      Then_Statements : List_Id;
      Elsif_Parts     : List_Id := No_List;
      Else_Statements : List_Id := No_List)
      return            Node_Id
   is
   begin
      Check_Restriction (No_Implicit_Conditionals, Node);
      return Make_If_Statement (Sloc (Node),
        Condition,
        Then_Statements,
        Elsif_Parts,
        Else_Statements);
   end Make_Implicit_If_Statement;

   -------------------------------------
   -- Make_Implicit_Label_Declaration --
   -------------------------------------

   function Make_Implicit_Label_Declaration
     (Loc                 : Source_Ptr;
      Defining_Identifier : Node_Id;
      Label_Construct     : Node_Id)
      return                Node_Id
   is
      N : constant Node_Id :=
            Make_Implicit_Label_Declaration (Loc, Defining_Identifier);

   begin
      Set_Label_Construct (N, Label_Construct);
      return N;
   end Make_Implicit_Label_Declaration;

   ----------------------------------
   -- Make_Implicit_Loop_Statement --
   ----------------------------------

   function Make_Implicit_Loop_Statement
     (Node                   : Node_Id;
      Identifier             : Node_Id := Empty;
      Iteration_Scheme       : Node_Id := Empty;
      Statements             : List_Id;
      Has_Created_Identifier : Boolean := False)
      return                   Node_Id
   is
   begin
      Check_Restriction (No_Implicit_Loops, Node);

      if Present (Iteration_Scheme)
        and then Present (Condition (Iteration_Scheme))
      then
         Check_Restriction (No_Implicit_Conditionals, Node);
      end if;

      return Make_Loop_Statement (Sloc (Node),
        Identifier,
        Iteration_Scheme,
        Statements,
        Has_Created_Identifier);
   end Make_Implicit_Loop_Statement;

   --------------------------
   -- Make_Integer_Literal --
   ---------------------------

   function Make_Integer_Literal
     (Loc    : Source_Ptr;
      Intval : Int)
      return   Node_Id
   is
   begin
      return Make_Integer_Literal (Loc, UI_From_Int (Intval));
   end Make_Integer_Literal;

   ---------------------------
   -- Make_Unsuppress_Block --
   ---------------------------

   --  Generates the following expansion:

   --    declare
   --       pragma Suppress (<check>);
   --    begin
   --       <stmts>
   --    end;

   function Make_Unsuppress_Block
     (Loc   : Source_Ptr;
      Check : Name_Id;
      Stmts : List_Id)
      return  Node_Id
   is
   begin
      return
        Make_Block_Statement (Loc,
          Declarations => New_List (
            Make_Pragma (Loc,
              Chars => Name_Suppress,
              Pragma_Argument_Associations => New_List (
                Make_Pragma_Argument_Association (Loc,
                  Expression => Make_Identifier (Loc, Check))))),

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts));
   end Make_Unsuppress_Block;

   --------------------------
   -- New_Constraint_Error --
   --------------------------

   function New_Constraint_Error (Loc : Source_Ptr) return Node_Id is
      Ident_Node : Node_Id;
      Raise_Node : Node_Id;

   begin
      Ident_Node := New_Node (N_Identifier, Loc);
      Set_Chars (Ident_Node, Chars (Standard_Entity (S_Constraint_Error)));
      Set_Entity (Ident_Node, Standard_Entity (S_Constraint_Error));
      Raise_Node := New_Node (N_Raise_Statement, Loc);
      Set_Name (Raise_Node, Ident_Node);
      return Raise_Node;
   end New_Constraint_Error;

   -----------------------
   -- New_External_Name --
   -----------------------

   function New_External_Name
     (Related_Id   : Name_Id;
      Suffix       : Character := ' ';
      Suffix_Index : Int       := 0;
      Prefix       : Character := ' ')
      return         Name_Id
   is
   begin
      Get_Name_String (Related_Id);

      if Prefix /= ' ' then
         pragma Assert (Is_OK_Internal_Letter (Prefix));

         for J in reverse 1 .. Name_Len loop
            Name_Buffer (J + 1) := Name_Buffer (J);
         end loop;

         Name_Len := Name_Len + 1;
         Name_Buffer (1) := Prefix;
      end if;

      if Suffix /= ' ' then
         pragma Assert (Is_OK_Internal_Letter (Suffix));
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Suffix;
      end if;

      if Suffix_Index /= 0 then
         if Suffix_Index < 0 then
            Add_Unique_Serial_Number;
         else
            Add_Nat_To_Name_Buffer (Suffix_Index);
         end if;
      end if;

      return Name_Find;
   end New_External_Name;

   function New_External_Name
     (Related_Id   : Name_Id;
      Suffix       : String;
      Suffix_Index : Int       := 0;
      Prefix       : Character := ' ')
      return         Name_Id
   is
   begin
      Get_Name_String (Related_Id);

      if Prefix /= ' ' then
         pragma Assert (Is_OK_Internal_Letter (Prefix));

         for J in reverse 1 .. Name_Len loop
            Name_Buffer (J + 1) := Name_Buffer (J);
         end loop;

         Name_Len := Name_Len + 1;
         Name_Buffer (1) := Prefix;
      end if;

      if Suffix /= "" then
         Name_Buffer (Name_Len + 1 .. Name_Len + Suffix'Length) := Suffix;
         Name_Len := Name_Len + Suffix'Length;
      end if;

      if Suffix_Index /= 0 then
         if Suffix_Index < 0 then
            Add_Unique_Serial_Number;
         else
            Add_Nat_To_Name_Buffer (Suffix_Index);
         end if;
      end if;

      return Name_Find;
   end New_External_Name;

   function New_External_Name
     (Suffix       : Character;
      Suffix_Index : Nat)
      return         Name_Id
   is
   begin
      Name_Buffer (1) := Suffix;
      Name_Len := 1;
      Add_Nat_To_Name_Buffer (Suffix_Index);
      return Name_Find;
   end New_External_Name;

   -----------------------
   -- New_Internal_Name --
   -----------------------

   function New_Internal_Name (Id_Char : Character) return Name_Id is
   begin
      pragma Assert (Is_OK_Internal_Letter (Id_Char));
      Name_Buffer (1) := Id_Char;
      Name_Len := 1;
      Add_Unique_Serial_Number;
      return Name_Enter;
   end New_Internal_Name;

   -----------------------
   -- New_Occurrence_Of --
   -----------------------

   function New_Occurrence_Of
     (Def_Id : Entity_Id;
      Loc    : Source_Ptr)
      return   Node_Id
   is
      Occurrence : Node_Id;

   begin
      Occurrence := New_Node (N_Identifier, Loc);
      Set_Chars (Occurrence, Chars (Def_Id));
      Set_Entity (Occurrence, Def_Id);

      if Is_Type (Def_Id) then
         Set_Etype (Occurrence, Def_Id);
      else
         Set_Etype (Occurrence, Etype (Def_Id));
      end if;

      return Occurrence;
   end New_Occurrence_Of;

   ----------------------
   -- New_Reference_To --
   ----------------------

   function New_Reference_To
     (Def_Id : Entity_Id;
      Loc    : Source_Ptr)
      return   Node_Id
   is
      Occurrence : Node_Id;

   begin
      Occurrence := New_Node (N_Identifier, Loc);
      Set_Chars (Occurrence, Chars (Def_Id));
      Set_Entity (Occurrence, Def_Id);
      return Occurrence;
   end New_Reference_To;

   -----------------------
   -- New_Suffixed_Name --
   -----------------------

   function New_Suffixed_Name
     (Related_Id : Name_Id;
      Suffix     : String)
      return       Name_Id
   is
   begin
      Get_Name_String (Related_Id);
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := '_';
      Name_Buffer (Name_Len + 1 .. Name_Len + Suffix'Length) := Suffix;
      Name_Len := Name_Len + Suffix'Length;
      return Name_Find;
   end New_Suffixed_Name;

   -------------------
   -- OK_Convert_To --
   -------------------

   function OK_Convert_To (Typ : Entity_Id; Expr : Node_Id) return Node_Id is
      Result : Node_Id;

   begin
      Result :=
        Make_Type_Conversion (Sloc (Expr),
          Subtype_Mark => New_Occurrence_Of (Typ, Sloc (Expr)),
          Expression   => Relocate_Node (Expr));
      Set_Conversion_OK (Result, True);
      Set_Etype (Result, Typ);
      return Result;
   end OK_Convert_To;

   --------------------------
   -- Unchecked_Convert_To --
   --------------------------

   function Unchecked_Convert_To
     (Typ  : Entity_Id;
      Expr : Node_Id)
      return Node_Id
   is
      Loc    : constant Source_Ptr := Sloc (Expr);
      Result : Node_Id;

   begin
      --  If the expression is already of the correct type, then nothing
      --  to do, except for relocating the node in case this is required.

      if Present (Etype (Expr))
        and then (Base_Type (Etype (Expr)) = Typ
                   or else Etype (Expr) = Typ)
      then
         return Relocate_Node (Expr);

      --  Cases where the inner expression is itself an unchecked conversion
      --  to the same type, and we can thus eliminate the outer conversion.

      elsif Nkind (Expr) = N_Unchecked_Type_Conversion
        and then Entity (Subtype_Mark (Expr)) = Typ
      then
         Result := Relocate_Node (Expr);

      --  All other cases

      else
         Result :=
           Make_Unchecked_Type_Conversion (Loc,
             Subtype_Mark => New_Occurrence_Of (Typ, Loc),
             Expression   => Relocate_Node (Expr));
      end if;

      Set_Etype (Result, Typ);
      return Result;
   end Unchecked_Convert_To;

end Tbuild;
