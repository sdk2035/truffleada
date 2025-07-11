------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ S T R M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.32 $
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
with Rtsfind;  use Rtsfind;
with Sem_Eval; use Sem_Eval;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Exp_Tss;  use Exp_Tss;
with Uintp;    use Uintp;

package body Exp_Strm is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Build_Array_Read_Write_Procedure
     (Nod  : Node_Id;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Nam  : Name_Id);
   --  Common routine shared to build either an array Read procedure or an
   --  array Write procedure, Nam is Name_Read or Name_Write to select which.
   --  Pnam is the defining identifier for the constructed procedure. The
   --  other parameters are as for Build_Array_Read_Procedure except that
   --  the first parameter Nod supplies the Sloc to be used to generate code.

   procedure Build_Record_Read_Write_Procedure
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Nam  : Name_Id);
   --  Common routine shared to build a record Read Write procedure, Nam
   --  is Name_Read or Name_Write to select which. Pnam is the defining
   --  identifier for the constructed procedure. The other parameters are
   --  as for Build_Record_Read_Procedure.

   procedure Build_Stream_Function
     (Loc   : Source_Ptr;
      Typ   : Entity_Id;
      Decl  : out Node_Id;
      Fnam  : Entity_Id;
      Decls : List_Id;
      Stms  : List_Id);
   --  Called to build an array or record stream function. The first three
   --  arguments are the same as Build_Record_Or_Elementary_Input_Function.
   --  Decls and Stms are the declarations and statements for the body and
   --  The parameter Fnam is the name of the constructed function.

   procedure Build_Stream_Procedure
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Stms : List_Id;
      Outp : Boolean);
   --  Called to build an array or record stream procedure. The first three
   --  arguments are the same as Build_Record_Or_Elementary_Output_Procedure.
   --  Stms is the list of statements for the body (the declaration list is
   --  always null), and Pnam is the name of the constructed procedure.

   function Stream_Base_Type (E : Entity_Id) return Entity_Id;
   --  Stream attributes work on the basis of the base type except for the
   --  array case. For the array case, we do not go to the base type, but
   --  to the first subtype if it is constrained. This avoids problems with
   --  incorrect conversions in the packed array case. Stream_Base_Type is
   --  exactly this function (returns the base type, unless we have an array
   --  type whose first subtype is constrained, in which case it returns the
   --  first subtype).

   --------------------------------
   -- Build_Array_Input_Function --
   --------------------------------

   --  The function we build looks like

   --    function InputN (S : access RST) return Typ is
   --      L1 : constant Index_Type_1 := Index_Type_1'Input (S);
   --      H1 : constant Index_Type_1 := Index_Type_1'Input (S);
   --      L2 : constant Index_Type_2 := Index_Type_2'Input (S);
   --      H2 : constant Index_Type_2 := Index_Type_2'Input (S);
   --      ..
   --      Ln : constant Index_Type_n := Index_Type_n'Input (S);
   --      Hn : constant Index_Type_n := Index_Type_n'Input (S);
   --
   --      V : Typ'Base (L1 .. H1, L2 .. H2, ... Ln .. Hn)

   --    begin
   --      Typ'Read (S, V);
   --      return V;
   --    end InputN

   procedure Build_Array_Input_Function
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id)
   is
      Dim    : constant Pos := Number_Dimensions (Typ);
      Lnam   : Name_Id;
      Hnam   : Name_Id;
      Decls  : List_Id;
      Ranges : List_Id;
      Stms   : List_Id;
      Indx   : Node_Id;

   begin
      Decls := New_List;
      Ranges := New_List;
      Indx  := First_Index (Typ);

      for J in 1 .. Dim loop
         Lnam := New_External_Name ('L', J);
         Hnam := New_External_Name ('H', J);

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Lnam),
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Etype (Indx), Loc),
             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Occurrence_Of (Stream_Base_Type (Etype (Indx)), Loc),
                 Attribute_Name => Name_Input,
                 Expressions => New_List (Make_Identifier (Loc, Name_S)))));

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Hnam),
             Constant_Present    => True,
             Object_Definition   =>
                   New_Occurrence_Of (Stream_Base_Type (Etype (Indx)), Loc),
             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Occurrence_Of (Stream_Base_Type (Etype (Indx)), Loc),
                 Attribute_Name => Name_Input,
                 Expressions => New_List (Make_Identifier (Loc, Name_S)))));

         Append_To (Ranges,
           Make_Range (Loc,
             Low_Bound  => Make_Identifier (Loc, Lnam),
             High_Bound => Make_Identifier (Loc, Hnam)));

         Next_Index (Indx);
      end loop;

      --  If the first subtype is constrained, use it directly. Otherwise
      --  build a subtype indication with the proper bounds.

      if Is_Constrained (Stream_Base_Type (Typ)) then
         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             Object_Definition =>
               New_Occurrence_Of (Stream_Base_Type (Typ), Loc)));
      else
         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             Object_Definition =>
               Make_Subtype_Indication (Loc,
                 Subtype_Mark =>
                   New_Occurrence_Of (Stream_Base_Type (Typ), Loc),
                 Constraint =>
                   Make_Index_Or_Discriminant_Constraint (Loc,
                     Constraints => Ranges))));
      end if;

      Stms := New_List (
         Make_Attribute_Reference (Loc,
           Prefix => New_Occurrence_Of (Typ, Loc),
           Attribute_Name => Name_Read,
           Expressions => New_List (
             Make_Identifier (Loc, Name_S),
             Make_Identifier (Loc, Name_V))),

         Make_Return_Statement (Loc,
           Expression => Make_Identifier (Loc, Name_V)));

      Fnam :=
        Make_Defining_Identifier (Loc,
          Chars =>
            New_External_Name (Name_uInput, ' ', Increment_Serial_Number));

      Build_Stream_Function (Loc, Typ, Decl, Fnam, Decls, Stms);
   end Build_Array_Input_Function;

   ----------------------------------
   -- Build_Array_Output_Procedure --
   ----------------------------------

   procedure Build_Array_Output_Procedure
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Stms : List_Id;
      Indx : Node_Id;

   begin
      --  Build series of statements to output bounds

      Indx := First_Index (Typ);
      Stms := New_List;

      for J in 1 .. Number_Dimensions (Typ) loop
         Append_To (Stms,
           Make_Attribute_Reference (Loc,
             Prefix =>
               New_Occurrence_Of (Stream_Base_Type (Etype (Indx)), Loc),
             Attribute_Name => Name_Write,
             Expressions => New_List (
               Make_Identifier (Loc, Name_S),
               Make_Attribute_Reference (Loc,
                 Prefix => Make_Identifier (Loc, Name_V),
                 Attribute_Name => Name_First,
                 Expressions => New_List (
                   Make_Integer_Literal (Loc, J))))));

         Append_To (Stms,
           Make_Attribute_Reference (Loc,
             Prefix =>
               New_Occurrence_Of (Stream_Base_Type (Etype (Indx)), Loc),
             Attribute_Name => Name_Write,
             Expressions => New_List (
               Make_Identifier (Loc, Name_S),
               Make_Attribute_Reference (Loc,
                 Prefix => Make_Identifier (Loc, Name_V),
                 Attribute_Name => Name_Last,
                 Expressions => New_List (
                   Make_Integer_Literal (Loc, J))))));

         Next_Index (Indx);
      end loop;

      --  Append Write attribute to write array elements

      Append_To (Stms,
        Make_Attribute_Reference (Loc,
          Prefix => New_Occurrence_Of (Typ, Loc),
          Attribute_Name => Name_Write,
          Expressions => New_List (
            Make_Identifier (Loc, Name_S),
            Make_Identifier (Loc, Name_V))));

      Pnam :=
        Make_Defining_Identifier (Loc,
          Chars =>
            New_External_Name (Name_uOutput, ' ', Increment_Serial_Number));

      Build_Stream_Procedure (Loc, Typ, Decl, Pnam, Stms, False);
   end Build_Array_Output_Procedure;

   --------------------------------
   -- Build_Array_Read_Procedure --
   --------------------------------

   procedure Build_Array_Read_Procedure
     (Nod  : Node_Id;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc : constant Source_Ptr := Sloc (Nod);

   begin
      Pnam :=
        Make_Defining_Identifier (Loc,
          New_External_Name
            (Name_uRead, ' ', Increment_Serial_Number));

      Build_Array_Read_Write_Procedure (Nod, Typ, Decl, Pnam, Name_Read);
   end Build_Array_Read_Procedure;

   --------------------------------------
   -- Build_Array_Read_Write_Procedure --
   --------------------------------------

   --  The form of the array read/write procedure is as follows:

   --    procedure pnam (S : access RST, V : [out] Typ) is
   --    begin
   --       for L1 in V'Range (1) loop
   --          for L2 in V'Range (2) loop
   --             ...
   --                for Ln in V'Range (n) loop
   --                   Component_Type'Read/Write (S, V (L1, L2, .. Ln));
   --                end loop;
   --             ..
   --          end loop;
   --       end loop
   --    end pnam;

   --  The out keyword for V is supplied in the Read case

   procedure Build_Array_Read_Write_Procedure
     (Nod  : Node_Id;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Nam  : Name_Id)
   is
      Loc : constant Source_Ptr := Sloc (Nod);

      Ndim : constant Pos        := Number_Dimensions (Typ);
      Ctyp : constant Entity_Id  := Component_Type (Typ);

      Stm  : Node_Id;
      Exl  : List_Id;
      RW   : Entity_Id;

   begin
      --  First build the inner attribute call

      Exl := New_List;

      for J in 1 .. Ndim loop
         Append_To (Exl, Make_Identifier (Loc, New_External_Name ('L', J)));
      end loop;

      Stm :=
        Make_Attribute_Reference (Loc,
          Prefix => New_Occurrence_Of (Stream_Base_Type (Ctyp), Loc),
          Attribute_Name => Nam,
          Expressions => New_List (
            Make_Identifier (Loc, Name_S),
            Make_Indexed_Component (Loc,
              Prefix => Make_Identifier (Loc, Name_V),
              Expressions => Exl)));

      --  The corresponding stream attribute for the component type of the
      --  array may be user-defined, and be frozen after the type for which
      --  we are generating the stream subprogram. In that case, freeze the
      --  stream attribute of the component type, whose declaration could not
      --  generate any additional freezing actions in any case. See 5509-003.

      if Nam = Name_Read then
         RW := TSS (Base_Type (Ctyp), Name_uRead);
      else
         RW := TSS (Base_Type (Ctyp), Name_uWrite);
      end if;

      if Present (RW)
        and then not Is_Frozen (RW)
      then
         Set_Is_Frozen (RW);
      end if;

      --  Now this is the big loop to wrap that statement up in a sequence
      --  of loops. The first time around, Stm is the attribute call. The
      --  second and subsequent times, Stm is an inner loop.

      for J in 1 .. Ndim loop
         Stm :=
           Make_Implicit_Loop_Statement (Nod,
             Iteration_Scheme =>
               Make_Iteration_Scheme (Loc,
                 Loop_Parameter_Specification =>
                   Make_Loop_Parameter_Specification (Loc,
                     Defining_Identifier =>
                       Make_Defining_Identifier (Loc,
                         Chars => New_External_Name ('L', Ndim - J + 1)),

                     Discrete_Subtype_Definition =>
                       Make_Attribute_Reference (Loc,
                         Prefix => Make_Identifier (Loc, Name_V),
                         Attribute_Name => Name_Range,

                         Expressions => New_List (
                           Make_Integer_Literal (Loc, Ndim - J + 1))))),

             Statements => New_List (Stm));

      end loop;

      Build_Stream_Procedure
        (Loc, Typ, Decl, Pnam, New_List (Stm), Nam = Name_Read);
   end Build_Array_Read_Write_Procedure;

   ---------------------------------
   -- Build_Array_Write_Procedure --
   ---------------------------------

   procedure Build_Array_Write_Procedure
     (Nod  : Node_Id;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Loc : constant Source_Ptr := Sloc (Nod);

   begin
      Pnam :=
        Make_Defining_Identifier (Loc,
          Chars =>
            New_External_Name (Name_uWrite, ' ', Increment_Serial_Number));

      Build_Array_Read_Write_Procedure (Nod, Typ, Decl, Pnam, Name_Write);
   end Build_Array_Write_Procedure;

   ---------------------------------
   -- Build_Elementary_Input_Call --
   ---------------------------------

   function Build_Elementary_Input_Call (N : Node_Id) return Node_Id is
      Loc     : constant Source_Ptr := Sloc (N);
      P_Type  : constant Entity_Id  := Entity (Prefix (N));
      R_Type  : constant Entity_Id  := Underlying_Type (P_Type);
      Rt_Type : constant Entity_Id  := Root_Type (R_Type);
      P_Size  : constant Uint       := Esize (Rt_Type);
      Strm    : constant Node_Id    := First (Expressions (N));
      Lib_RE  : RE_Id;

   begin
      --  Check for First Boolean and Character. These are enumeration types,
      --  but we treat them specially, since they may require special handling
      --  in the transfer protocol.

      if Rt_Type = Standard_Boolean then
         Lib_RE := RE_I_B;

      elsif Rt_Type = Standard_Character then
         Lib_RE := RE_I_C;

      elsif Rt_Type = Standard_Wide_Character then
         Lib_RE := RE_I_WC;

      --  Floating point types

      elsif Is_Floating_Point_Type (R_Type) then

         if Rt_Type = Standard_Short_Float then
            Lib_RE := RE_I_SF;

         elsif Rt_Type = Standard_Float then
            Lib_RE := RE_I_F;

         elsif Rt_Type = Standard_Long_Float then
            Lib_RE := RE_I_LF;

         elsif Rt_Type = Standard_Long_Long_Float then
            Lib_RE := RE_I_LLF;

         else
            pragma Assert (False);
            raise Program_Error;
         end if;

      --  Signed integer types. Also includes signed fixed-point types and
      --  enumeration types with a signed representation.

      elsif Is_Signed_Integer_Type (R_Type)
        or else
          (Is_Fixed_Point_Type (R_Type)
            and then Expr_Value (Type_Low_Bound (R_Type)) < 0)
        or else
          (Is_Enumeration_Type (R_Type)
            and then Enumeration_Rep (Entity (Type_Low_Bound (R_Type))) < 0)
      then
         if P_Size <= Standard_Short_Short_Integer_Size then
            Lib_RE := RE_I_SSI;

         elsif P_Size <= Standard_Short_Integer_Size then
            Lib_RE := RE_I_SI;

         elsif P_Size <= Standard_Integer_Size then
            Lib_RE := RE_I_I;

         elsif P_Size <= Standard_Long_Integer_Size then
            Lib_RE := RE_I_LI;

         else
            Lib_RE := RE_I_LLI;
         end if;

      --  Unsigned integer types, also includes unsigned fixed-point types
      --  and enumeration types with an unsigned representation (note that
      --  we know they are unsigned because we already tested for signed).

      elsif Is_Modular_Integer_Type (R_Type)
        or else Is_Fixed_Point_Type (R_Type)
        or else Is_Enumeration_Type (R_Type)
      then
         if P_Size <= Standard_Short_Short_Integer_Size then
            Lib_RE := RE_I_SSU;

         elsif P_Size <= Standard_Short_Integer_Size then
            Lib_RE := RE_I_SU;

         elsif P_Size <= Standard_Integer_Size then
            Lib_RE := RE_I_U;

         elsif P_Size <= Standard_Long_Integer_Size then
            Lib_RE := RE_I_LU;

         else
            Lib_RE := RE_I_LLU;
         end if;

      elsif Is_Access_Type (R_Type) then
         if P_Size > System_Address_Size then
            Lib_RE := RE_I_AD;
         else
            Lib_RE := RE_I_AS;
         end if;

      else
         pragma Assert (False);
         raise Program_Error;
      end if;

      --  Call the function, and do an unchecked conversion of the result
      --  to the actual type of the prefix.

      return
        Unchecked_Convert_To (P_Type,
          Make_Function_Call (Loc,
            Name => New_Occurrence_Of (RTE (Lib_RE), Loc),
            Parameter_Associations => New_List (
              Relocate_Node (Strm))));

   end Build_Elementary_Input_Call;

   ---------------------------------
   -- Build_Elementary_Write_Call --
   ---------------------------------

   function Build_Elementary_Write_Call (N : Node_Id) return Node_Id is
      Loc     : constant Source_Ptr := Sloc (N);
      P_Type  : constant Entity_Id  := Entity (Prefix (N));
      R_Type  : constant Entity_Id  := Underlying_Type (P_Type);
      Rt_Type : constant Entity_Id  := Root_Type (R_Type);
      P_Size  : constant Uint       := Esize (Rt_Type);
      Strm    : constant Node_Id    := First (Expressions (N));
      Item    : constant Node_Id    := Next (Strm);
      Lib_RE  : RE_Id;
      Libent  : Entity_Id;

   begin
      --  Find the routine to be called

      --  Check for First Boolean and Character. These are enumeration types,
      --  but we treat them specially, since they may require special handling
      --  in the transfer protocol.

      if Rt_Type = Standard_Boolean then
         Lib_RE := RE_W_B;

      elsif Rt_Type = Standard_Character then
         Lib_RE := RE_W_C;

      elsif Rt_Type = Standard_Wide_Character then
         Lib_RE := RE_W_WC;

      --  Floating point types

      elsif Is_Floating_Point_Type (R_Type) then

         if Rt_Type = Standard_Short_Float then
            Lib_RE := RE_W_SF;

         elsif Rt_Type = Standard_Float then
            Lib_RE := RE_W_F;

         elsif Rt_Type = Standard_Long_Float then
            Lib_RE := RE_W_LF;

         elsif Rt_Type = Standard_Long_Long_Float then
            Lib_RE := RE_W_LLF;

         else
            pragma Assert (False);
            raise Program_Error;
         end if;

      --  Signed integer types. Also includes signed fixed-point types and
      --  signed enumeration types share this circuitry.

      elsif Is_Signed_Integer_Type (R_Type)
        or else
          (Is_Fixed_Point_Type (R_Type)
            and then Expr_Value (Type_Low_Bound (R_Type)) < 0)
        or else
          (Is_Enumeration_Type (R_Type)
            and then Enumeration_Rep (Entity (Type_Low_Bound (R_Type))) < 0)
      then
         if P_Size <= Standard_Short_Short_Integer_Size then
            Lib_RE := RE_W_SSI;

         elsif P_Size <= Standard_Short_Integer_Size then
            Lib_RE := RE_W_SI;

         elsif P_Size <= Standard_Integer_Size then
            Lib_RE := RE_W_I;

         elsif P_Size <= Standard_Long_Integer_Size then
            Lib_RE := RE_W_LI;

         else
            Lib_RE := RE_W_LLI;
         end if;

      --  Unsigned integer types, also includes unsigned fixed-point types
      --  and unsigned enumeration types (note we know they are unsigned
      --  because we already tested for signed above).

      elsif Is_Modular_Integer_Type (R_Type)
        or else Is_Fixed_Point_Type (R_Type)
        or else Is_Enumeration_Type (R_Type)
      then
         if P_Size <= Standard_Short_Short_Integer_Size then
            Lib_RE := RE_W_SSU;

         elsif P_Size <= Standard_Short_Integer_Size then
            Lib_RE := RE_W_SU;

         elsif P_Size <= Standard_Integer_Size then
            Lib_RE := RE_W_U;

         elsif P_Size <= Standard_Long_Integer_Size then
            Lib_RE := RE_W_LU;

         else
            Lib_RE := RE_W_LLU;
         end if;

      elsif Is_Access_Type (R_Type) then
         if P_Size > System_Address_Size then
            Lib_RE := RE_W_AD;
         else
            Lib_RE := RE_W_AS;
         end if;

      else
         pragma Assert (False);
         raise Program_Error;
      end if;

      --  Unchecked-convert parameter to the required type (i.e. the type of
      --  the corresponding parameter, and call the appropriate routine.

      Libent := RTE (Lib_RE);

      return
        Make_Procedure_Call_Statement (Loc,
          Name => New_Occurrence_Of (Libent, Loc),
          Parameter_Associations => New_List (
            Relocate_Node (Strm),
            Unchecked_Convert_To (Etype (Next_Formal (First_Formal (Libent))),
              Relocate_Node (Item))));

   end Build_Elementary_Write_Call;

   -----------------------------------------
   -- Build_Mutable_Record_Read_Procedure --
   -----------------------------------------

   procedure Build_Mutable_Record_Read_Procedure
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Stms  : List_Id;
      Disc  : Entity_Id;
      Comp  : Node_Id;

   begin
      Stms := New_List;
      Disc := First_Discriminant (Typ);

      --  Generate Reads for the discriminants of the type.

      while Present (Disc) loop
         Comp :=
           Make_Selected_Component (Loc,
             Prefix => Make_Identifier (Loc, Name_V),
             Selector_Name => New_Occurrence_Of (Disc, Loc));

         Set_Assignment_OK (Comp);

         Append_To (Stms,
           Make_Attribute_Reference (Loc,
             Prefix => New_Occurrence_Of (Etype (Disc), Loc),
               Attribute_Name => Name_Read,
               Expressions => New_List (
                 Make_Identifier (Loc, Name_S),
                 Comp)));

         Next_Discriminant (Disc);
      end loop;

      --  A mutable type cannot be a tagged type, so we generate a new name
      --  for the stream procedure.

      Pnam :=
        Make_Defining_Identifier (Loc,
          Chars =>
            New_External_Name (Name_uRead, ' ', Increment_Serial_Number));

      Build_Record_Read_Write_Procedure (Loc, Typ, Decl, Pnam, Name_Read);

      --  Read the discriminants before the rest of the components, so
      --  that discriminant values are properly set of variants, etc.
      --  If this is an empty record with discriminants, there are no
      --  previous statements. If this is an unchecked union, the stream
      --  procedure is erroneous, because there are no discriminants to read.

      if Is_Unchecked_Union (Typ) then
         Stms := New_List (Make_Raise_Program_Error (Loc));
      end if;

      if Is_Non_Empty_List (
        Statements (Handled_Statement_Sequence (Decl)))
      then
         Insert_List_Before
           (First (Statements (Handled_Statement_Sequence (Decl))), Stms);
      else
         Set_Statements (Handled_Statement_Sequence (Decl), Stms);
      end if;
   end Build_Mutable_Record_Read_Procedure;

   ------------------------------------------
   -- Build_Mutable_Record_Write_Procedure --
   ------------------------------------------

   procedure Build_Mutable_Record_Write_Procedure
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Stms  : List_Id;
      Disc  : Entity_Id;

   begin
      Stms := New_List;
      Disc := First_Discriminant (Typ);

      --  Generate Writes for the discriminants of the type.

      while Present (Disc) loop

         Append_To (Stms,
           Make_Attribute_Reference (Loc,
             Prefix => New_Occurrence_Of (Etype (Disc), Loc),
               Attribute_Name => Name_Write,
               Expressions => New_List (
                 Make_Identifier (Loc, Name_S),
                 Make_Selected_Component (Loc,
                   Prefix => Make_Identifier (Loc, Name_V),
                   Selector_Name => New_Occurrence_Of (Disc, Loc)))));

         Next_Discriminant (Disc);
      end loop;

      --  A mutable type cannot be a tagged type, so we generate a new name
      --  for the stream procedure.

      Pnam :=
        Make_Defining_Identifier (Loc,
          Chars =>
            New_External_Name (Name_uWrite, ' ', Increment_Serial_Number));

      Build_Record_Read_Write_Procedure (Loc, Typ, Decl, Pnam, Name_Write);

      --  Write the discriminants before the rest of the components, so
      --  that discriminant values are properly set of variants, etc.
      --  If this is an unchecked union, the stream procedure is erroneous
      --  because there are no discriminants to write.

      if Is_Unchecked_Union (Typ) then
         Stms := New_List (Make_Raise_Program_Error (Loc));
      end if;

      if Is_Non_Empty_List (
        Statements (Handled_Statement_Sequence (Decl)))
      then
         Insert_List_Before
            (First (Statements (Handled_Statement_Sequence (Decl))), Stms);
      else
         Set_Statements (Handled_Statement_Sequence (Decl), Stms);
      end if;
   end Build_Mutable_Record_Write_Procedure;

   -----------------------------------------------
   -- Build_Record_Or_Elementary_Input_Function --
   -----------------------------------------------

   --  The function we build looks like

   --    function InputN (S : access RST) return Typ is
   --      C1 : constant Disc_Type_1 := Discr_Type_1'Input (S);
   --      C2 : constant Disc_Type_1 := Discr_Type_2'Input (S);
   --      ...
   --      Cn : constant Disc_Type_1 := Discr_Type_n'Input (S);
   --      V : Typ (C1, C2, .. Cn)

   --    begin
   --      Typ'Read (S, V);
   --      return V;
   --    end InputN

   --  The discriminants are of course only present in the case of a record
   --  with discriminants. In the case of a record with no discriminants, or
   --  an elementary type, then no Cn constants are defined.

   procedure Build_Record_Or_Elementary_Input_Function
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Fnam : out Entity_Id)
   is
      Cn     : Name_Id;
      J      : Pos;
      Decls  : List_Id;
      Constr : List_Id;
      Stms   : List_Id;
      Discr  : Entity_Id;
      Odef   : Node_Id;

   begin
      Decls  := New_List;
      Constr := New_List;

      J := 1;

      if Has_Discriminants (Typ) then
         Discr := First_Discriminant (Typ);

         while Present (Discr) loop
            Cn := New_External_Name ('C', J);

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Make_Defining_Identifier (Loc, Cn),
                Object_Definition   => New_Occurrence_Of (Etype (Discr), Loc),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Occurrence_Of
                        (Stream_Base_Type (Etype (Discr)), Loc),
                    Attribute_Name => Name_Input,
                    Expressions => New_List (Make_Identifier (Loc, Name_S)))));

            Append_To (Constr, Make_Identifier (Loc, Cn));

            Next_Discriminant (Discr);
            J := J + 1;
         end loop;

         Odef :=
           Make_Subtype_Indication (Loc,
             Subtype_Mark => New_Occurrence_Of (Typ, Loc),
             Constraint =>
               Make_Index_Or_Discriminant_Constraint (Loc,
                 Constraints => Constr));

      --  If no discriminants, then just use the type with no constraint

      else
         Odef := New_Occurrence_Of (Typ, Loc);
      end if;

      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
          Object_Definition => Odef));

      Stms := New_List (
         Make_Attribute_Reference (Loc,
           Prefix => New_Occurrence_Of (Typ, Loc),
           Attribute_Name => Name_Read,
           Expressions => New_List (
             Make_Identifier (Loc, Name_S),
             Make_Identifier (Loc, Name_V))),

         Make_Return_Statement (Loc,
           Expression => Make_Identifier (Loc, Name_V)));

      --  For tagged types, we use a canonical name so that it matches the
      --  primitive spec. For all other cases, we use a serialized name so
      --  that multiple generations of the same procedure do not clash.

      if Is_Tagged_Type (Typ) then
         Fnam := Make_Defining_Identifier (Loc, Name_uInput);
      else
         Fnam :=
           Make_Defining_Identifier (Loc,
             Chars =>
               New_External_Name (Name_uInput, ' ', Increment_Serial_Number));
      end if;

      Build_Stream_Function (Loc, Typ, Decl, Fnam, Decls, Stms);
   end Build_Record_Or_Elementary_Input_Function;

   -------------------------------------------------
   -- Build_Record_Or_Elementary_Output_Procedure --
   -------------------------------------------------

   procedure Build_Record_Or_Elementary_Output_Procedure
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
      Stms : List_Id;
      Disc : Entity_Id;

   begin
      Stms := New_List;

      --  Note that of course there will be no discriminants for the
      --  elementary type case, so Has_Discriminants will be False.

      if Has_Discriminants (Typ) then
         Disc := First_Discriminant (Typ);

         while Present (Disc) loop
            Append_To (Stms,
              Make_Attribute_Reference (Loc,
                Prefix =>
                  New_Occurrence_Of (Stream_Base_Type (Etype (Disc)), Loc),
                Attribute_Name => Name_Write,
                Expressions => New_List (
                  Make_Identifier (Loc, Name_S),
                  Make_Selected_Component (Loc,
                    Prefix => Make_Identifier (Loc, Name_V),
                    Selector_Name => New_Occurrence_Of (Disc, Loc)))));

            Next_Discriminant (Disc);
         end loop;
      end if;

      Append_To (Stms,
        Make_Attribute_Reference (Loc,
          Prefix => New_Occurrence_Of (Typ, Loc),
          Attribute_Name => Name_Write,
          Expressions => New_List (
            Make_Identifier (Loc, Name_S),
            Make_Identifier (Loc, Name_V))));

      --  For tagged types, we use a canonical name so that it matches the
      --  primitive spec. For all other cases, we use a serialized name so
      --  that multiple generations of the same procedure do not clash.

      if Is_Tagged_Type (Typ) then
         Pnam := Make_Defining_Identifier (Loc, Name_uOutput);
      else
         Pnam :=
           Make_Defining_Identifier (Loc,
             Chars =>
               New_External_Name
                 (Name_uOutput, ' ', Increment_Serial_Number));
      end if;

      Build_Stream_Procedure (Loc, Typ, Decl, Pnam, Stms, False);
   end Build_Record_Or_Elementary_Output_Procedure;

   ---------------------------------
   -- Build_Record_Read_Procedure --
   ---------------------------------

   procedure Build_Record_Read_Procedure
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
   begin
      --  For tagged types, we use a canonical name so that it matches the
      --  primitive spec. For all other cases, we use a serialized name so
      --  that multiple generations of the same procedure do not clash.

      if Is_Tagged_Type (Typ) then
         Pnam := Make_Defining_Identifier (Loc, Name_uRead);
      else
         Pnam :=
           Make_Defining_Identifier (Loc,
             Chars =>
               New_External_Name (Name_uRead, ' ', Increment_Serial_Number));
      end if;

      Build_Record_Read_Write_Procedure (Loc, Typ, Decl, Pnam, Name_Read);
   end Build_Record_Read_Procedure;

   ---------------------------------------
   -- Build_Record_Read_Write_Procedure --
   ---------------------------------------

   --  The form of the record read/write procedure is as shown by the
   --  following example for a case with one discriminant case variant:

   --    procedure pnam (S : access RST, V : [out] Typ) is
   --    begin
   --       Component_Type'Read/Write (S, V.component);
   --       Component_Type'Read/Write (S, V.component);
   --       ...
   --       Component_Type'Read/Write (S, V.component);
   --
   --       case V.discriminant is
   --          when choices =>
   --             Component_Type'Read/Write (S, V.component);
   --             Component_Type'Read/Write (S, V.component);
   --             ...
   --             Component_Type'Read/Write (S, V.component);
   --
   --          when choices =>
   --             Component_Type'Read/Write (S, V.component);
   --             Component_Type'Read/Write (S, V.component);
   --             ...
   --             Component_Type'Read/Write (S, V.component);
   --          ...
   --       end case;
   --    end pnam;

   --  The out keyword for V is supplied in the Read case

   procedure Build_Record_Read_Write_Procedure
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Nam  : Name_Id)
   is
      Rdef : Node_Id;
      Stms : List_Id;
      Typt : Entity_Id;

      function Make_Component_List_Attributes (CL : Node_Id) return List_Id;
      --  Returns a sequence of attributes to process the components that
      --  are referenced in the given component list.

      function Make_Field_Attribute (C : Entity_Id) return Node_Id;
      --  Given C, the entity for a discriminant or component, build
      --  an attribute for the corresponding field values.

      function Make_Field_Attributes (Clist : List_Id) return List_Id;
      --  Given Clist, a component items list, construct series of attributes
      --  for fieldwise processing of the corresponding components.

      ------------------------------------
      -- Make_Component_List_Attributes --
      ------------------------------------

      function Make_Component_List_Attributes (CL : Node_Id) return List_Id is
         CI : constant List_Id := Component_Items (CL);
         VP : constant Node_Id := Variant_Part (CL);

         Result : List_Id;
         Alts   : List_Id;
         V      : Node_Id;
         DC     : Node_Id;
         DCH    : List_Id;

      begin
         Result := Make_Field_Attributes (CI);

         --  If a component is an unchecked union, there is no discriminant
         --  and we cannot generate a read/write procedure for it.

         if Present (VP) then
            if Is_Unchecked_Union (Scope (Entity (Name (VP)))) then
               return New_List (Make_Raise_Program_Error (Sloc (VP)));
            end if;

            V := First_Non_Pragma (Variants (VP));
            Alts := New_List;
            while Present (V) loop

               DCH := New_List;
               DC := First (Discrete_Choices (V));
               while Present (DC) loop
                  Append_To (DCH, New_Copy_Tree (DC));
                  Next (DC);
               end loop;

               Append_To (Alts,
                 Make_Case_Statement_Alternative (Loc,
                   Discrete_Choices => DCH,
                   Statements =>
                     Make_Component_List_Attributes (Component_List (V))));
               Next_Non_Pragma (V);
            end loop;

            --  Note: in the following, we make sure that we use new occurrence
            --  of for the selector, since there are cases in which we make a
            --  reference to a hidden discriminant that is not visible.

            Append_To (Result,
              Make_Case_Statement (Loc,
                Expression =>
                  Make_Selected_Component (Loc,
                    Prefix => Make_Identifier (Loc, Name_V),
                    Selector_Name =>
                      New_Occurrence_Of (Entity (Name (VP)), Loc)),
                Alternatives => Alts));

         end if;

         return Result;
      end Make_Component_List_Attributes;

      --------------------------
      -- Make_Field_Attribute --
      --------------------------

      function Make_Field_Attribute (C : Entity_Id) return Node_Id is
      begin
         return
           Make_Attribute_Reference (Loc,
             Prefix =>
               New_Occurrence_Of (Stream_Base_Type (Etype (C)), Loc),
             Attribute_Name => Nam,
             Expressions => New_List (
               Make_Identifier (Loc, Name_S),
               Make_Selected_Component (Loc,
                 Prefix => Make_Identifier (Loc, Name_V),
                 Selector_Name => New_Occurrence_Of (C, Loc))));
      end Make_Field_Attribute;

      ---------------------------
      -- Make_Field_Attributes --
      ---------------------------

      function Make_Field_Attributes (Clist : List_Id) return List_Id is
         Item   : Node_Id;
         Result : List_Id;

      begin
         Result := New_List;

         if Present (Clist) then
            Item := First (Clist);

            --  Loop through components, skipping all internal components,
            --  which are not part of the value (e.g. _Tag), except that we
            --  don't skip the _Parent, since we do want to process that
            --  recursively.

            while Present (Item) loop
               if Nkind (Item) = N_Component_Declaration
                 and then
                   (Chars (Defining_Identifier (Item)) = Name_uParent
                     or else
                    not Is_Internal_Name (Chars (Defining_Identifier (Item))))
               then
                  Append_To
                    (Result,
                     Make_Field_Attribute (Defining_Identifier (Item)));
               end if;

               Next (Item);
            end loop;
         end if;

         return Result;
      end Make_Field_Attributes;

   --  Start of processing for Build_Record_Read_Write_Procedure

   begin
      --  For the protected type case, use corresponding record

      if Is_Protected_Type (Typ) then
         Typt := Corresponding_Record_Type (Typ);
      else
         Typt := Typ;
      end if;

      --  Note that we do nothing with the discriminants, since Read and
      --  Write do not read or write the discriminant values. All handling
      --  of discriminants occurs in the Input and Output subprograms.

      Rdef := Type_Definition (Declaration_Node (Underlying_Type (Typt)));
      Stms := Empty_List;

      --  In record extension case, the fields we want, including the _Parent
      --  field representing the parent type, are to be found in the extension.
      --  Note that we will naturally process the _Parent field using the type
      --  of the parent, and hence its stream attributes, which is appropriate.

      if Nkind (Rdef) = N_Derived_Type_Definition then
         Rdef := Record_Extension_Part (Rdef);
      end if;

      if Present (Component_List (Rdef)) then
         Append_List_To (Stms,
           Make_Component_List_Attributes (Component_List (Rdef)));
      end if;

      Build_Stream_Procedure
        (Loc, Typ, Decl, Pnam, Stms, Nam = Name_Read);

   end Build_Record_Read_Write_Procedure;

   ----------------------------------
   -- Build_Record_Write_Procedure --
   ----------------------------------

   procedure Build_Record_Write_Procedure
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : out Entity_Id)
   is
   begin
      --  For tagged types, we use a canonical name so that it matches the
      --  primitive spec. For all other cases, we use a serialized name so
      --  that multiple generations of the same procedure do not clash.

      if Is_Tagged_Type (Typ) then
         Pnam := Make_Defining_Identifier (Loc, Name_uWrite);
      else
         Pnam :=
           Make_Defining_Identifier (Loc,
             Chars =>
               New_External_Name (Name_uWrite, ' ', Increment_Serial_Number));
      end if;

      Build_Record_Read_Write_Procedure (Loc, Typ, Decl, Pnam, Name_Write);
   end Build_Record_Write_Procedure;

   -------------------------------
   -- Build_Stream_Attr_Profile --
   -------------------------------

   function Build_Stream_Attr_Profile
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Nam  : Name_Id)
      return List_Id
   is
      Profile : List_Id;

   begin
      Profile := New_List (
        Make_Parameter_Specification (Loc,
          Defining_Identifier =>  Make_Defining_Identifier (Loc, Name_S),
          Parameter_Type      =>
          Make_Access_Definition (Loc,
             Subtype_Mark => New_Reference_To (
               Class_Wide_Type (RTE (RE_Root_Stream_Type)), Loc))));

      if Nam /= Name_uInput then
         Append_To (Profile,
           Make_Parameter_Specification (Loc,
             Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
             Out_Present         => (Nam = Name_uRead),
             Parameter_Type      => New_Reference_To (Typ, Loc)));
      end if;

      return Profile;
   end Build_Stream_Attr_Profile;

   ---------------------------
   -- Build_Stream_Function --
   ---------------------------

   procedure Build_Stream_Function
     (Loc   : Source_Ptr;
      Typ   : Entity_Id;
      Decl  : out Node_Id;
      Fnam  : Entity_Id;
      Decls : List_Id;
      Stms  : List_Id)
   is
      Spec : Node_Id;

   begin
      --  Construct function specification

      Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name => Fnam,

          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier => Make_Defining_Identifier (Loc, Name_S),
              Parameter_Type =>
                Make_Access_Definition (Loc,
                  Subtype_Mark => New_Reference_To (
                    Class_Wide_Type (RTE (RE_Root_Stream_Type)), Loc)))),

          Subtype_Mark => New_Occurrence_Of (Typ, Loc));

      Decl :=
        Make_Subprogram_Body (Loc,
          Specification => Spec,
          Declarations => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stms));

   end Build_Stream_Function;

   ----------------------------
   -- Build_Stream_Procedure --
   ----------------------------

   procedure Build_Stream_Procedure
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      Decl : out Node_Id;
      Pnam : Entity_Id;
      Stms : List_Id;
      Outp : Boolean)
   is
      Spec : Node_Id;

   begin
      --  Construct procedure specification

      Spec :=
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name => Pnam,

          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier => Make_Defining_Identifier (Loc, Name_S),
              Parameter_Type =>
                Make_Access_Definition (Loc,
                  Subtype_Mark => New_Reference_To (
                    Class_Wide_Type (RTE (RE_Root_Stream_Type)), Loc))),

            Make_Parameter_Specification (Loc,
              Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
              Out_Present         => Outp,
              Parameter_Type      => New_Occurrence_Of (Typ, Loc))));

      Decl :=
        Make_Subprogram_Body (Loc,
          Specification => Spec,
          Declarations => Empty_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stms));

   end Build_Stream_Procedure;

   ----------------------
   -- Stream_Base_Type --
   ----------------------

   function Stream_Base_Type (E : Entity_Id) return Entity_Id is
   begin
      if Is_Array_Type (E)
        and then Is_First_Subtype (E)
      then
         return E;

      else
         return Base_Type (E);
      end if;
   end Stream_Base_Type;

end Exp_Strm;
