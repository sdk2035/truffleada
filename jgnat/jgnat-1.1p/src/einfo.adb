------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                E I N F O                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.591 $
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Output;   use Output;

package body Einfo is

   use Atree.Unchecked_Access;
   --  This is one of the packages that is allowed direct untyped access to
   --  the fields in a node, since it provides the next level abstraction
   --  which incorporates appropriate checks.

   ----------------------------------------------
   -- Usage of Fields in Defining Entity Nodes --
   ----------------------------------------------

   --  The first five of these fields are defined in Sinfo, since they in
   --  the base part of the node. The access routines for these fields and
   --  the corresponding set procedures are defined in Sinfo. These fields
   --  are present in all entities.

   --    Chars                           Name1
   --    Next_Entity                     Node2
   --    Scope                           Node3
   --    Homonym                         Node4
   --    Etype                           Node5

   --   Remaining fields are present only in extended nodes (i.e. entities)

   --  The following fields are present in all entities

   --    First_Rep_Item                  Node6
   --    Freeze_Node                     Node7

   --  The usage of each field (and the entity kinds to which it applies)
   --  depends on the particular field (see Einfo spec for details).

   --    Associated_Node_For_Itype       Node8
   --    Dependent_Instances             Elist8
   --    Hiding_Loop_Variable           Node8
   --    Mechanism                       Uint8 (but returns Mechanism_Type)

   --    Class_Wide_Type                 Node9
   --    Size_Check_Code                 Node9
   --    Renaming_Map                    Uint9

   --    Discriminal_Link                Node10
   --    Handler_Records                 List10
   --    Referenced_Object               Node10

   --    Component_First_Bit             Uint11
   --    Full_View                       Node11
   --    Entry_Component                 Node11
   --    Enumeration_Pos                 Uint11
   --    Protected_Body_Subprogram       Node11
   --    Block_Node                      Node11

   --    Barrier_Function                Node12
   --    Default_Expr_Function           Node12
   --    Enumeration_Rep                 Uint12
   --    Esize                           Uint12
   --    Next_Inlined_Subprogram         Node12

   --    Associated_Storage_Pool         Node13
   --    Body_Entity                     Node13
   --    Component_Clause                Node13
   --    Component_Size                  Uint13
   --    Debug_Renaming_Link             Node13
   --    Extra_Accessibility             Node13
   --    Finalization_Chain_Entity       Node13
   --    Primitive_Operations            Elist13
   --    RM_Size                         Uint13

   --    Associated_Final_Chain          Node14
   --    CR_Discriminant                 Node14
   --    Girder_Constraint               Elist14
   --    Entry_Cancel_Parameter          Node14
   --    Extra_Constrained               Node14
   --    Generic_Renamings               Elist14
   --    Inner_Instances                 Elist14
   --    Enum_Pos_To_Rep                 Node14
   --    Packed_Array_Type               Node14
   --    Protected_Operation             Node14

   --    Access_Disp_Table               Node15
   --    Discriminant_Number             Uint15
   --    DT_Position                     Uint15
   --    DT_Entry_Count                  Uint15
   --    Entry_Bodies_Array              Node15
   --    Entry_Parameters_Type           Node15
   --    Extra_Formal                    Node15
   --    Related_Instance                Node15
   --    Scale_Value                     Uint15
   --    Storage_Size_Variable           Node15
   --    String_Literal_Low_Bound        Node15
   --    Shared_Mem_Read_Proc            Node15

   --    Cloned_Subtype                  Node16
   --    DTC_Entity                      Node16
   --    Entry_Formal                    Node16
   --    First_Private_Entity            Node16
   --    String_Literal_Length           Uint16
   --    Table_High_Bound                Node16
   --    Underlying_Full_View            Node16
   --    Unset_Reference                 Node16

   --    Actual_Subtype                  Node17
   --    Digits_Value                    Uint17
   --    Discriminal                     Node17
   --    First_Entity                    Node17
   --    First_Index                     Node17
   --    First_Literal                   Node17
   --    Master_Id                       Node17
   --    Modulus                         Uint17
   --    Object_Ref                      Node17
   --    Prival                          Node17

   --    Alias                           Node18
   --    Corresponding_Concurrent_Type   Node18
   --    Corresponding_Record_Type       Node18
   --    Delta_Value                     Ureal18
   --    Enclosing_Scope                 Node18
   --    Equivalent_Type                 Node18
   --    Lit_Name_Table                  Node18
   --    Private_Dependents              Elist18
   --    Renamed_Entity                  Node18
   --    Renamed_Object                  Node18

   --    Corresponding_Discriminant      Node19
   --    Corresponding_Equality          Node19
   --    Elaboration_Entity              Node19
   --    Parent_Subtype                  Node19
   --    Related_Array_Object            Node19
   --    Spec_Entity                     Node19
   --    Task_Body_Procedure             Node19

   --    Component_Type                  Node20
   --    Default_Value                   Node20
   --    Directly_Designated_Type        Node20
   --    Discriminant_Checking_Func      Node20
   --    Discriminant_Default_Value      Node20
   --    Last_Entity                     Node20
   --    Register_Exception_Call         Node20
   --    Scalar_Range                    Node20

   --    Discriminant_Constraint         Elist21
   --    Small_Value                     Ureal21
   --    Accept_Address                  Elist21
   --    Interface_Name                  Node21

   --    Corresponding_Remote_Type       Node22
   --    Enumeration_Rep_Expr            Node22
   --    Exception_Code                  Uint22
   --    Original_Record_Component       Node22
   --    Private_View                    Node22
   --    Protected_Formal                Node22
   --    Scope_Depth                     Uint22
   --    Shared_Mem_Assign_Proc          Node22

   --    Alignment                       Uint23
   --    First_Optional_Parameter        Node23
   --    Shadow_Entities                 List23

   ---------------------------------------------
   -- Usage of Flags in Defining Entity Nodes --
   ---------------------------------------------

   --  All flags are unique, there is no overlaying, so each flag is physically
   --  present in every entity. However, for many of the flags, it only makes
   --  sense for them to be set true for certain subsets of entity kinds. See
   --  the spec of Einfo for further details.

   --  Note: Flag1-Flag3 are absent from this list, since these flag positions
   --  are used for the flags Analyzed, Comes_From_Source, and Error_Posted,
   --  which are common to all nodes, including entity nodes.

   --    Is_Frozen                      Flag4
   --    Has_Discriminants              Flag5
   --    Is_Dispatching_Operation       Flag6
   --    Is_Immediately_Visible         Flag7
   --    In_Use                         Flag8
   --    Is_Potentially_Use_Visible     Flag9
   --    Is_Public                      Flag10
   --    Is_Inlined                     Flag11
   --    Is_Constrained                 Flag12
   --    Is_Generic_Type                Flag13
   --    Depends_On_Private             Flag14
   --    Is_Aliased                     Flag15
   --    Is_Volatile                    Flag16
   --    Is_Internal                    Flag17
   --    Has_Delayed_Freeze             Flag18
   --    Is_Abstract                    Flag19
   --    Is_Concurrent_Record_Type      Flag20
   --    Has_Master_Entity              Flag21
   --    Needs_No_Actuals               Flag22
   --    Has_Storage_Size_Clause        Flag23
   --    Is_Imported                    Flag24
   --    Is_Limited_Record              Flag25
   --    Has_Completion                 Flag26
   --    Has_Pragma_Controlled          Flag27
   --    Is_Statically_Allocated        Flag28
   --    Has_Size_Clause                Flag29
   --    Has_Task                       Flag30
   --    Suppress_Access_Checks         Flag31
   --    Suppress_Accessibility_Checks  Flag32
   --    Suppress_Discriminant_Checks   Flag33
   --    Suppress_Division_Checks       Flag34
   --    Suppress_Elaboration_Checks    Flag35
   --    Suppress_Index_Checks          Flag36
   --    Suppress_Length_Checks         Flag37
   --    Suppress_Overflow_Checks       Flag38
   --    Suppress_Range_Checks          Flag39
   --    Suppress_Storage_Checks        Flag40
   --    Suppress_Tag_Checks            Flag41
   --    Is_Controlled                  Flag42
   --    Has_Controlled_Component       Flag43
   --    Is_Pure                        Flag44
   --    In_Private_Part                Flag45
   --    Has_Alignment_Clause           Flag46
   --    Has_Exit                       Flag47
   --    In_Package_Body                Flag48
   --    Reachable                      Flag49
   --    Delay_Subprogram_Descriptors   Flag50
   --    Is_Packed                      Flag51
   --    Is_Entry_Formal                Flag52
   --    Is_Private_Descendant          Flag53
   --    Return_Present                 Flag54
   --    Is_Tagged_Type                 Flag55
   --    Has_Homonym                    Flag56
   --    Is_Hidden                      Flag57
   --    Non_Binary_Modulus             Flag58
   --    Is_Preelaborated               Flag59
   --    Is_Shared_Passive              Flag60
   --    Is_Remote_Types                Flag61
   --    Is_Remote_Call_Interface       Flag62
   --    Is_Character_Type              Flag63
   --    Is_Intrinsic_Subprogram        Flag64
   --    Has_Record_Rep_Clause          Flag65
   --    Has_Enumeration_Rep_Clause     Flag66
   --    Has_Small_Clause               Flag67
   --    Has_Component_Size_Clause      Flag68
   --    Is_Access_Constant             Flag69
   --    Is_First_Subtype               Flag70
   --    Has_Completion_In_Body         Flag71
   --    Has_Unknown_Discriminants      Flag72
   --    Is_Child_Unit                  Flag73
   --    Is_CPP_Class                   Flag74
   --    Has_Non_Standard_Rep           Flag75
   --    Is_Constructor                 Flag76
   --    Is_Destructor                  Flag77
   --    Is_Tag                         Flag78
   --    Has_All_Calls_Remote           Flag79
   --    Is_Constr_Subt_For_U_Nominal   Flag80
   --    Is_Asynchronous                Flag81
   --    Has_Gigi_Rep_Item              Flag82
   --    Has_Machine_Radix_Clause       Flag83
   --    Machine_Radix_10               Flag84
   --    Is_Atomic                      Flag85
   --    Has_Atomic_Components          Flag86
   --    Has_Volatile_Components        Flag87
   --    Discard_Names                  Flag88
   --    Is_Interrupt_Handler           Flag89
   --    Returns_By_Ref                 Flag90
   --    Is_Itype                       Flag91
   --    Size_Known_At_Compile_Time     Flag92
   --    Has_Subprogram_Descriptor      Flag93
   --    Is_Generic_Actual_Type         Flag94
   --    Uses_Sec_Stack                 Flag95
   --    Warnings_Off                   Flag96
   --    Is_Controlling_Formal          Flag97
   --    Has_Controlling_Result         Flag98
   --    Is_Exported                    Flag99
   --    Has_Specified_Layout           Flag100
   --    Has_Nested_Block_With_Handler  Flag101
   --    Is_Called                      Flag102
   --    Is_Completely_Hidden           Flag103
   --    Address_Taken                  Flag104
   --    Suppress_Init_Proc             Flag105
   --    Is_Limited_Composite           Flag106
   --    Is_Private_Composite           Flag107
   --    Default_Expressions_Processed  Flag108
   --    Is_Non_Static_Subtype          Flag109
   --    Has_External_Tag_Rep_Clause    Flag110
   --    Is_Formal_Subprogram           Flag111
   --    Is_Renaming_Of_Object          Flag112
   --    No_Return                      Flag113
   --    Delay_Cleanups                 Flag114
   --    Not_Source_Assigned            Flag115
   --    Is_Visible_Child_Unit          Flag116
   --    Is_Unchecked_Union             Flag117
   --    Is_For_Access_Subtype          Flag118
   --    Has_Convention_Pragma          Flag119
   --    Has_Primitive_Operations       Flag120
   --    Has_Pragma_Pack                Flag121
   --    Is_Bit_Packed_Array            Flag122
   --    Has_Unchecked_Union            Flag123
   --    Is_Eliminated                  Flag124
   --    C_Pass_By_Copy                 Flag125
   --    Is_Instantiated                Flag126
   --    Is_Valued_Procedure            Flag127
   --    (used for Component_Alignment) Flag128
   --    (used for Component_Alignment) Flag129
   --    Is_Generic_Instance            Flag130
   --    No_Pool_Assigned               Flag131
   --    Is_AST_Entry                   Flag132
   --    Is_VMS_Exception               Flag133
   --    Is_Optional_Parameter          Flag134
   --    Has_Aliased_Components         Flag135
   --    Is_Machine_Code_Subprogram     Flag137
   --    Is_Packed_Array_Type           Flag138
   --    Has_Biased_Representation      Flag139
   --    Has_Complex_Representation     Flag140
   --    Is_Constr_Subt_For_UN_Aliased  Flag141
   --    Has_Missing_Return             Flag142
   --    Has_Recursive_Call             Flag143
   --    Is_Unsigned_Type               Flag144
   --    Strict_Alignment               Flag145
   --    Elaborate_All_Desirable        Flag146
   --    Needs_Debug_Info               Flag147
   --    Suppress_Elaboration_Warnings  Flag148
   --    Is_Compilation_Unit            Flag149
   --    Has_Pragma_Elaborate_Body      Flag150
   --    Vax_Float                      Flag151
   --    Entry_Accepted                 Flag152
   --    Is_Psected                     Flag153
   --    Has_Per_Object_Constraint      Flag154
   --    Has_Private_Declaration        Flag155
   --    Referenced                     Flag156
   --    Has_Pragma_Inline              Flag157
   --    Finalize_Storage_Only          Flag158
   --    From_With_Type                 Flag159
   --    Is_Package_Body_Entity         Flag160
   --    Has_Qualified_Name             Flag161
   --    Nonzero_Is_True                Flag162
   --    Is_True_Constant               Flag163
   --    Reverse_Bit_Order              Flag164
   --    Suppress_Style_Checks          Flag165
   --    Debug_Info_Off                 Flag166
   --    Sec_Stack_Needed_For_Return    Flag167
   --    Materialize_Entity             Flag168
   --    Function_Returns_With_DSP      Flag169
   --    Is_Known_Valid                 Flag170
   --    Is_Hidden_Open_Scope           Flag171

   --    (unused)                       Flag172
   --    (unused)                       Flag173
   --    (unused)                       Flag174
   --    (unused)                       Flag175
   --    (unused)                       Flag176
   --    (unused)                       Flag177
   --    (unused)                       Flag178
   --    (unused)                       Flag179
   --    (unused)                       Flag180
   --    (unused)                       Flag181
   --    (unused)                       Flag182
   --    (unused)                       Flag183

   --------------------------------
   -- Attribute Access Functions --
   --------------------------------

   function Accept_Address (Id : E) return L is
   begin
      return Elist21 (Id);
   end Accept_Address;

   function Access_Disp_Table (Id : E) return E is
   begin
      pragma Assert (Is_Tagged_Type (Id));
      return Node15 (Base_Type (Underlying_Type (Base_Type (Id))));
   end Access_Disp_Table;

   function Actual_Subtype (Id : E) return E is
   begin
      pragma Assert
         (Ekind (Id) = E_Constant
           or else Ekind (Id) = E_Variable
           or else Ekind (Id) = E_Generic_In_Out_Parameter
           or else Ekind (Id) in  E_In_Parameter .. E_In_Out_Parameter);
      return Node17 (Id);
   end Actual_Subtype;

   function Address_Taken (Id : E) return B is
   begin
      return Flag104 (Id);
   end Address_Taken;

   function Alias (Id : E) return E is
   begin
      pragma Assert
        (Is_Overloadable (Id) or else Ekind (Id) = E_Subprogram_Type);
      return Node18 (Id);
   end Alias;

   function Alignment (Id : E) return U is
   begin
      return Uint23 (Id);
   end Alignment;

   function Associated_Formal_Package (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      return Node12 (Id);
   end Associated_Formal_Package;

   function Associated_Node_For_Itype (Id : E) return N is
   begin
      return Node8 (Id);
   end Associated_Node_For_Itype;

   function Associated_Storage_Pool (Id : E) return E is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Node13 (Id);
   end Associated_Storage_Pool;

   function Associated_Final_Chain (Id : E) return E is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Node14 (Id);
   end Associated_Final_Chain;

   function Barrier_Function (Id : E) return N is
   begin
      pragma Assert (Is_Entry (Id));
      return Node12 (Id);
   end Barrier_Function;

   function Block_Node (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Block);
      return Node11 (Id);
   end Block_Node;

   function Body_Entity (Id : E) return E is
   begin
      pragma Assert
        (Ekind (Id) = E_Package or else Ekind (Id) = E_Generic_Package);
      return Node13 (Id);
   end Body_Entity;

   function C_Pass_By_Copy (Id : E) return B is
   begin
      pragma Assert (Is_Record_Type (Id));
      return Flag125 (Implementation_Base_Type (Id));
   end C_Pass_By_Copy;

   function Class_Wide_Type (Id : E) return E is
   begin
      pragma Assert (Is_Type (Id));
      return Node9 (Id);
   end Class_Wide_Type;

   function Cloned_Subtype (Id : E) return E is
   begin
      pragma Assert
        (Ekind (Id) = E_Record_Subtype
         or else Ekind (Id) = E_Class_Wide_Subtype);
      return Node16 (Id);
   end Cloned_Subtype;

   function Component_Clause (Id : E) return N is
   begin
      pragma Assert
        (Ekind (Id) = E_Component or else Ekind (Id) = E_Discriminant);
      return Node13 (Id);
   end Component_Clause;

   function Component_First_Bit (Id : E) return U is
   begin
      pragma Assert
        (Ekind (Id) = E_Component or else Ekind (Id) = E_Discriminant);
      return Uint11 (Id);
   end Component_First_Bit;

   function Component_Size (Id : E) return U is
   begin
      pragma Assert (Is_Array_Type (Id));
      return Uint13 (Implementation_Base_Type (Id));
   end Component_Size;

   function Component_Type (Id : E) return E is
   begin
      return Node20 (Implementation_Base_Type (Id));
   end Component_Type;

   function Corresponding_Concurrent_Type (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Record_Type);
      return Node18 (Id);
   end Corresponding_Concurrent_Type;

   function Corresponding_Discriminant (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      return Node19 (Id);
   end Corresponding_Discriminant;

   function Corresponding_Equality (Id : E) return E is
   begin
      pragma Assert
        (Ekind (Id) = E_Function
          and then not Comes_From_Source (Id)
          and then Chars (Id) = Name_Op_Ne);
      return Node19 (Id);
   end Corresponding_Equality;

   function Corresponding_Record_Type (Id : E) return E is
   begin
      pragma Assert (Is_Concurrent_Type (Id));
      return Node18 (Id);
   end Corresponding_Record_Type;

   function Corresponding_Remote_Type (Id : E) return E is
   begin
      return Node22 (Id);
   end Corresponding_Remote_Type;

   function CR_Discriminant (Id : E) return E is
   begin
      return Node14 (Id);
   end CR_Discriminant;

   function Default_Expr_Function (Id : E) return E is
   begin
      pragma Assert (Is_Formal (Id));
      return Node12 (Id);
   end Default_Expr_Function;

   function Default_Expressions_Processed (Id : E) return B is
   begin
      return Flag108 (Id);
   end Default_Expressions_Processed;

   function Default_Value (Id : E) return N is
   begin
      pragma Assert (Is_Formal (Id));
      return Node20 (Id);
   end Default_Value;

   function Delay_Cleanups (Id : E) return B is
   begin
      return Flag114 (Id);
   end Delay_Cleanups;

   function Delay_Subprogram_Descriptors (Id : E) return B is
   begin
      return Flag50 (Id);
   end Delay_Subprogram_Descriptors;

   function Delta_Value (Id : E) return R is
   begin
      pragma Assert (Is_Fixed_Point_Type (Id));
      return Ureal18 (Id);
   end Delta_Value;

   function Dependent_Instances (Id : E) return L is
   begin
      pragma Assert (Is_Generic_Instance (Id));
      return Elist8 (Id);
   end Dependent_Instances;

   function Depends_On_Private (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag14 (Id);
   end Depends_On_Private;

   function Digits_Value (Id : E) return U is
   begin
      pragma Assert
        (Is_Floating_Point_Type (Id)
          or else Is_Decimal_Fixed_Point_Type (Id));
      return Uint17 (Id);
   end Digits_Value;

   function Directly_Designated_Type (Id : E) return E is
   begin
      return Node20 (Id);
   end Directly_Designated_Type;

   function Discard_Names (Id : E) return B is
   begin
      return Flag88 (Id);
   end Discard_Names;

   function Discriminal (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      return Node17 (Id);
   end Discriminal;

   function Discriminal_Link (Id : E) return N is
   begin
      return Node10 (Id);
   end Discriminal_Link;

   function Discriminant_Checking_Func (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Component);
      return Node20 (Id);
   end Discriminant_Checking_Func;

   function Discriminant_Constraint (Id : E) return L is
   begin
      pragma Assert (Is_Composite_Type (Id) and then Has_Discriminants (Id));
      return Elist21 (Id);
   end Discriminant_Constraint;

   function Discriminant_Default_Value (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      return Node20 (Id);
   end Discriminant_Default_Value;

   function Discriminant_Number (Id : E) return U is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      return Uint15 (Id);
   end Discriminant_Number;

   function Debug_Info_Off (Id : E) return B is
   begin
      return Flag166 (Id);
   end Debug_Info_Off;

   function Debug_Renaming_Link (Id : E) return E is
   begin
      return Node13 (Id);
   end Debug_Renaming_Link;

   function DTC_Entity (Id : E) return E is
   begin
      pragma Assert
        (Ekind (Id) = E_Function or else Ekind (Id) = E_Procedure);
      return Node16 (Id);
   end DTC_Entity;

   function DT_Entry_Count (Id : E) return U is
   begin
      pragma Assert (Ekind (Id) = E_Component  and then Is_Tag (Id));
      return Uint15 (Id);
   end DT_Entry_Count;

   function DT_Position (Id : E) return U is
   begin
      pragma Assert
        ((Ekind (Id) = E_Function
            or else Ekind (Id) = E_Procedure)
          and then Present (DTC_Entity (Id)));
      return Uint15 (Id);
   end DT_Position;

   function Elaborate_All_Desirable (Id : E) return B is
   begin
      return Flag146 (Id);
   end Elaborate_All_Desirable;

   function Elaboration_Entity (Id : E) return E is
   begin
      pragma Assert
        (Is_Subprogram (Id)
           or else
         Ekind (Id) = E_Package
           or else
         Is_Generic_Unit (Id));
      return Node19 (Id);
   end Elaboration_Entity;

   function Enclosing_Scope (Id : E) return E is
   begin
      return Node18 (Id);
   end Enclosing_Scope;

   function Entry_Accepted (Id : E) return B is
   begin
      pragma Assert (Is_Entry (Id));
      return Flag152 (Id);
   end Entry_Accepted;

   function Entry_Bodies_Array (Id : E) return E is
   begin
      return Node15 (Id);
   end Entry_Bodies_Array;

   function Entry_Cancel_Parameter (Id : E) return E is
   begin
      return Node14 (Id);
   end Entry_Cancel_Parameter;

   function Entry_Component (Id : E) return E is
   begin
      return Node11 (Id);
   end Entry_Component;

   function Entry_Formal (Id : E) return E is
   begin
      return Node16 (Id);
   end Entry_Formal;

   function Entry_Index_Constant (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Entry_Index_Parameter);
      return Node18 (Id);
   end Entry_Index_Constant;

   function Entry_Parameters_Type (Id : E) return E is
   begin
      return Node15 (Id);
   end Entry_Parameters_Type;

   function Enumeration_Pos (Id : E) return Uint is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Literal);
      return Uint11 (Id);
   end Enumeration_Pos;

   function Enumeration_Rep (Id : E) return U is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Literal);
      return Uint12 (Id);
   end Enumeration_Rep;

   function Enumeration_Rep_Expr (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Literal);
      return Node22 (Id);
   end Enumeration_Rep_Expr;

   function Enum_Pos_To_Rep (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Type);
      return Node14 (Id);
   end Enum_Pos_To_Rep;

   function Equivalent_Type (Id : E) return E is
   begin
      pragma Assert
        (Ekind (Id) = E_Class_Wide_Subtype               or else
         Ekind (Id) = E_Access_Protected_Subprogram_Type or else
         Ekind (Id) = E_Access_Subprogram_Type           or else
         Ekind (Id) = E_Exception_Type);
      return Node18 (Id);
   end Equivalent_Type;

   function Esize (Id : E) return Uint is
   begin
      return Uint12 (Id);
   end Esize;

   function Exception_Code (Id : E) return Uint is
   begin
      pragma Assert (Ekind (Id) = E_Exception);
      return Uint22 (Id);
   end Exception_Code;

   function Extra_Formal (Id : E) return E is
   begin
      return Node15 (Id);
   end Extra_Formal;

   function Extra_Accessibility (Id : E) return E is
   begin
      pragma Assert (Is_Formal (Id) or else Ekind (Id) = E_Variable);
      return Node13 (Id);
   end Extra_Accessibility;

   function Extra_Constrained (Id : E) return E is
   begin
      pragma Assert (Is_Formal (Id) or else Ekind (Id) = E_Variable);
      return Node14 (Id);
   end Extra_Constrained;

   function Finalization_Chain_Entity (Id : E) return E is
   begin
      return Node13 (Id);
   end Finalization_Chain_Entity;

   function Finalize_Storage_Only (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag158 (Base_Type (Id));
   end Finalize_Storage_Only;

   function First_Entity (Id : E) return E is
   begin
      return Node17 (Id);
   end First_Entity;

   function First_Index (Id : E) return N is
   begin
      return Node17 (Id);
   end First_Index;

   function First_Literal (Id : E) return E is
   begin
      return Node17 (Id);
   end First_Literal;

   function First_Optional_Parameter (Id : E) return E is
   begin
      pragma Assert
        (Ekind (Id) = E_Function or else Ekind (Id) = E_Procedure);
      return Node23 (Id);
   end First_Optional_Parameter;

   function First_Private_Entity (Id : E) return E is
   begin
      return Node16 (Id);
   end First_Private_Entity;

   function First_Rep_Item (Id : E) return E is
   begin
      return Node6 (Id);
   end First_Rep_Item;

   function Freeze_Node (Id : E) return N is
   begin
      return Node7 (Id);
   end Freeze_Node;

   function From_With_Type (Id : E) return B is
   begin
      return Flag159 (Id);
   end From_With_Type;

   function Full_View (Id : E) return E is
   begin
      pragma Assert (Is_Type (Id) or else Ekind (Id) = E_Constant);
      return Node11 (Id);
   end Full_View;

   function Function_Returns_With_DSP (Id : E) return B is
   begin
      pragma Assert
        (Is_Subprogram (Id) or else Ekind (Id) = E_Subprogram_Type);
      return Flag169 (Id);
   end Function_Returns_With_DSP;

   function Generic_Renamings (Id : E) return L is
   begin
      return Elist14 (Id);
   end Generic_Renamings;

   function Girder_Constraint (Id : E) return L is
   begin
      pragma Assert
        (Is_Composite_Type (Id) and then not Is_Array_Type (Id));
      return Elist14 (Id);
   end Girder_Constraint;

   function Handler_Records (Id : E) return S is
   begin
      return List10 (Id);
   end Handler_Records;

   function Has_Aliased_Components (Id : E) return B is
   begin
      return Flag135 (Implementation_Base_Type (Id));
   end Has_Aliased_Components;

   function Has_Alignment_Clause (Id : E) return B is
   begin
      return Flag46 (Id);
   end Has_Alignment_Clause;

   function Has_All_Calls_Remote (Id : E) return B is
   begin
      return Flag79 (Id);
   end Has_All_Calls_Remote;

   function Has_Atomic_Components (Id : E) return B is
   begin
      return Flag86 (Implementation_Base_Type (Id));
   end Has_Atomic_Components;

   function Has_Biased_Representation (Id : E) return B is
   begin
      return Flag139 (Id);
   end Has_Biased_Representation;

   function Has_Completion (Id : E) return B is
   begin
      return Flag26 (Id);
   end Has_Completion;

   function Has_Completion_In_Body (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag71 (Id);
   end Has_Completion_In_Body;

   function Has_Complex_Representation (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag140 (Implementation_Base_Type (Id));
   end Has_Complex_Representation;

   function Has_Component_Size_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Array_Type (Id));
      return Flag68 (Implementation_Base_Type (Id));
   end Has_Component_Size_Clause;

   function Has_Controlled_Component (Id : E) return B is
   begin
      return Flag43 (Base_Type (Id));
   end Has_Controlled_Component;

   function Has_Controlling_Result (Id : E) return B is
   begin
      return Flag98 (Id);
   end Has_Controlling_Result;

   function Has_Convention_Pragma (Id : E) return B is
   begin
      return Flag119 (Id);
   end Has_Convention_Pragma;

   function Has_Delayed_Freeze (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag18 (Id);
   end Has_Delayed_Freeze;

   function Has_Discriminants (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag5 (Id);
   end Has_Discriminants;

   function Has_Enumeration_Rep_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Enumeration_Type (Id));
      return Flag66 (Id);
   end Has_Enumeration_Rep_Clause;

   function Has_Exit (Id : E) return B is
   begin
      return Flag47 (Id);
   end Has_Exit;

   function Has_External_Tag_Rep_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Tagged_Type (Id));
      return Flag110 (Id);
   end Has_External_Tag_Rep_Clause;

   function Has_Gigi_Rep_Item (Id : E) return B is
   begin
      return Flag82 (Id);
   end Has_Gigi_Rep_Item;

   function Has_Homonym (Id : E) return B is
   begin
      return Flag56 (Id);
   end Has_Homonym;

   function Has_Machine_Radix_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Decimal_Fixed_Point_Type (Id));
      return Flag83 (Id);
   end Has_Machine_Radix_Clause;

   function Has_Master_Entity (Id : E) return B is
   begin
      return Flag21 (Id);
   end Has_Master_Entity;

   function Has_Missing_Return (Id : E) return B is
   begin
      pragma Assert
        (Ekind (Id) = E_Function or else Ekind (Id) = E_Generic_Function);
      return Flag142 (Id);
   end Has_Missing_Return;

   function Has_Nested_Block_With_Handler (Id : E) return B is
   begin
      return Flag101 (Id);
   end Has_Nested_Block_With_Handler;

   function Has_Non_Standard_Rep (Id : E) return B is
   begin
      return Flag75 (Implementation_Base_Type (Id));
   end Has_Non_Standard_Rep;

   function Has_Per_Object_Constraint (Id : E) return B is
   begin
      return Flag154 (Id);
   end Has_Per_Object_Constraint;

   function Has_Pragma_Controlled (Id : E) return B is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Flag27 (Implementation_Base_Type (Id));
   end Has_Pragma_Controlled;

   function Has_Pragma_Elaborate_Body (Id : E) return B is
   begin
      return Flag150 (Id);
   end Has_Pragma_Elaborate_Body;

   function Has_Pragma_Inline (Id : E) return B is
   begin
      return Flag157 (Id);
   end Has_Pragma_Inline;

   function Has_Pragma_Pack (Id : E) return B is
   begin
      pragma Assert (Is_Record_Type (Id) or else Is_Array_Type (Id));
      return Flag121 (Implementation_Base_Type (Id));
   end Has_Pragma_Pack;

   function Has_Primitive_Operations (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag120 (Base_Type (Id));
   end Has_Primitive_Operations;

   function Has_Private_Declaration (Id : E) return B is
   begin
      return Flag155 (Id);
   end Has_Private_Declaration;

   function Has_Qualified_Name (Id : E) return B is
   begin
      return Flag161 (Id);
   end Has_Qualified_Name;

   function Has_Record_Rep_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Record_Type (Id));
      return Flag65 (Id);
   end Has_Record_Rep_Clause;

   function Has_Recursive_Call (Id : E) return B is
   begin
      pragma Assert (Is_Subprogram (Id));
      return Flag143 (Id);
   end Has_Recursive_Call;

   function Has_Size_Clause (Id : E) return B is
   begin
      return Flag29 (Id);
   end Has_Size_Clause;

   function Has_Small_Clause (Id : E) return B is
   begin
      return Flag67 (Id);
   end Has_Small_Clause;

   function Has_Specified_Layout (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag100 (Id);
   end Has_Specified_Layout;

   function Has_Storage_Size_Clause (Id : E) return B is
   begin
      pragma Assert (Is_Access_Type (Id) or else Is_Task_Type (Id));
      return Flag23 (Implementation_Base_Type (Id));
   end Has_Storage_Size_Clause;

   function Has_Subprogram_Descriptor (Id : E) return B is
   begin
      return Flag93 (Id);
   end Has_Subprogram_Descriptor;

   function Has_Task (Id : E) return B is
   begin
      return Flag30 (Base_Type (Id));
   end Has_Task;

   function Has_Unchecked_Union (Id : E) return B is
   begin
      return Flag123 (Base_Type (Id));
   end Has_Unchecked_Union;

   function Has_Unknown_Discriminants (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag72 (Id);
   end Has_Unknown_Discriminants;

   function Has_Volatile_Components (Id : E) return B is
   begin
      return Flag87 (Implementation_Base_Type (Id));
   end Has_Volatile_Components;

   function Hiding_Loop_Variable (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      return Node8 (Id);
   end Hiding_Loop_Variable;

   function In_Package_Body (Id : E) return B is
   begin
      return Flag48 (Id);
   end In_Package_Body;

   function In_Private_Part (Id : E) return B is
   begin
      return Flag45 (Id);
   end In_Private_Part;

   function Inner_Instances (Id : E) return L is
   begin
      return Elist14 (Id);
   end Inner_Instances;

   function In_Use (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag8 (Id);
   end In_Use;

   function Interface_Name (Id : E) return N is
   begin
      return Node21 (Id);
   end Interface_Name;

   function Is_Abstract (Id : E) return B is
   begin
      return Flag19 (Id);
   end Is_Abstract;

   function Is_Access_Constant (Id : E) return B is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Flag69 (Id);
   end Is_Access_Constant;

   function Is_Aliased (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag15 (Id);
   end Is_Aliased;

   function Is_AST_Entry (Id : E) return B is
   begin
      pragma Assert (Is_Entry (Id));
      return Flag132 (Id);
   end Is_AST_Entry;

   function Is_Asynchronous (Id : E) return B is
   begin
      pragma Assert
        (Ekind (Id) = E_Procedure or else Is_Type (Id));
      return Flag81 (Id);
   end Is_Asynchronous;

   function Is_Atomic (Id : E) return B is
   begin
      return Flag85 (Id);
   end Is_Atomic;

   function Is_Bit_Packed_Array (Id : E) return B is
   begin
      return Flag122 (Implementation_Base_Type (Id));
   end Is_Bit_Packed_Array;

   function Is_Called (Id : E) return B is
   begin
      pragma Assert
        (Ekind (Id) = E_Procedure or else Ekind (Id) = E_Function);
      return Flag102 (Id);
   end Is_Called;

   function Is_Character_Type (Id : E) return B is
   begin
      return Flag63 (Id);
   end Is_Character_Type;

   function Is_Child_Unit (Id : E) return B is
   begin
      return Flag73 (Id);
   end Is_Child_Unit;

   function Is_Compilation_Unit (Id : E) return B is
   begin
      return Flag149 (Id);
   end Is_Compilation_Unit;

   function Is_Completely_Hidden (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      return Flag103 (Id);
   end Is_Completely_Hidden;

   function Is_Constrained (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag12 (Id);
   end Is_Constrained;

   function Is_Constr_Subt_For_U_Nominal (Id : E) return B is
   begin
      return Flag80 (Id);
   end Is_Constr_Subt_For_U_Nominal;

   function Is_Constr_Subt_For_UN_Aliased (Id : E) return B is
   begin
      return Flag141 (Id);
   end Is_Constr_Subt_For_UN_Aliased;

   function Is_Constructor (Id : E) return B is
   begin
      return Flag76 (Id);
   end Is_Constructor;

   function Is_Controlled (Id : E) return B is
   begin
      return Flag42 (Base_Type (Id));
   end Is_Controlled;

   function Is_Controlling_Formal (Id : E) return B is
   begin
      pragma Assert (Is_Formal (Id));
      return Flag97 (Id);
   end Is_Controlling_Formal;

   function Is_CPP_Class (Id : E) return B is
   begin
      return Flag74 (Id);
   end Is_CPP_Class;

   function Is_Destructor (Id : E) return B is
   begin
      return Flag77 (Id);
   end Is_Destructor;

   function Is_Dispatching_Operation (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag6 (Id);
   end Is_Dispatching_Operation;

   function Is_Entry_Formal (Id : E) return B is
   begin
      return Flag52 (Id);
   end Is_Entry_Formal;

   function Is_Eliminated (Id : E) return B is
   begin
      return Flag124 (Id);
   end Is_Eliminated;

   function Is_Exported (Id : E) return B is
   begin
      return Flag99 (Id);
   end Is_Exported;

   function Is_Formal_Subprogram (Id : E) return B is
   begin
      return Flag111 (Id);
   end Is_Formal_Subprogram;

   function Is_For_Access_Subtype (Id : E) return B is
   begin
      pragma Assert
        (Ekind (Id) = E_Record_Subtype
          or else
         Ekind (Id) = E_Private_Subtype);
      return Flag118 (Id);
   end Is_For_Access_Subtype;

   function Is_Frozen (Id : E) return B is
   begin
      return Flag4 (Id);
   end Is_Frozen;

   function Is_First_Subtype (Id : E) return B is
   begin
      return Flag70 (Id);
   end Is_First_Subtype;

   function Is_Generic_Instance (Id : E) return B is
   begin
      return Flag130 (Id);
   end Is_Generic_Instance;

   function Is_Generic_Type (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag13 (Id);
   end Is_Generic_Type;

   function Is_Generic_Actual_Type (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag94 (Id);
   end Is_Generic_Actual_Type;

   function Is_Hidden (Id : E) return B is
   begin
      return Flag57 (Id);
   end Is_Hidden;

   function Is_Hidden_Open_Scope (Id : E) return B is
   begin
      return Flag171 (Id);
   end Is_Hidden_Open_Scope;

   function Is_Immediately_Visible (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag7 (Id);
   end Is_Immediately_Visible;

   function Is_Imported (Id : E) return B is
   begin
      return Flag24 (Id);
   end Is_Imported;

   function Is_Inlined (Id : E) return B is
   begin
      return Flag11 (Id);
   end Is_Inlined;

   function Is_Instantiated (Id : E) return B is
   begin
      return Flag126 (Id);
   end Is_Instantiated;

   function Is_Internal (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag17 (Id);
   end Is_Internal;

   function Is_Interrupt_Handler (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag89 (Id);
   end Is_Interrupt_Handler;

   function Is_Intrinsic_Subprogram (Id : E) return B is
   begin
      return Flag64 (Id);
   end Is_Intrinsic_Subprogram;

   function Is_Itype (Id : E) return B is
   begin
      return Flag91 (Id);
   end Is_Itype;

   function Is_Known_Valid (Id : E) return B is
   begin
      return Flag170 (Id);
   end Is_Known_Valid;

   function Is_Limited_Composite (Id : E) return B is
   begin
      return Flag106 (Id);
   end Is_Limited_Composite;

   function Is_Limited_Record (Id : E) return B is
   begin
      return Flag25 (Id);
   end Is_Limited_Record;

   function Is_Machine_Code_Subprogram (Id : E) return B is
   begin
      pragma Assert (Is_Subprogram (Id));
      return Flag137 (Id);
   end Is_Machine_Code_Subprogram;

   function Is_Non_Static_Subtype (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag109 (Id);
   end Is_Non_Static_Subtype;

   function Is_Optional_Parameter (Id : E) return B is
   begin
      pragma Assert (Is_Formal (Id));
      return Flag134 (Id);
   end Is_Optional_Parameter;

   function Is_Package_Body_Entity (Id : E) return B is
   begin
      return Flag160 (Id);
   end Is_Package_Body_Entity;

   function Is_Packed (Id : E) return B is
   begin
      return Flag51 (Implementation_Base_Type (Id));
   end Is_Packed;

   function Is_Packed_Array_Type (Id : E) return B is
   begin
      return Flag138 (Id);
   end Is_Packed_Array_Type;

   function Is_Potentially_Use_Visible (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag9 (Id);
   end Is_Potentially_Use_Visible;

   function Is_Preelaborated (Id : E) return B is
   begin
      return Flag59 (Id);
   end Is_Preelaborated;

   function Is_Private_Composite (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag107 (Id);
   end Is_Private_Composite;

   function Is_Private_Descendant (Id : E) return B is
   begin
      return Flag53 (Id);
   end Is_Private_Descendant;

   function Is_Psected (Id : E) return B is
   begin
      return Flag153 (Id);
   end Is_Psected;

   function Is_Public (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag10 (Id);
   end Is_Public;

   function Is_Pure (Id : E) return B is
   begin
      return Flag44 (Id);
   end Is_Pure;

   function Is_Remote_Call_Interface (Id : E) return B is
   begin
      return Flag62 (Id);
   end Is_Remote_Call_Interface;

   function Is_Remote_Types (Id : E) return B is
   begin
      return Flag61 (Id);
   end Is_Remote_Types;

   function Is_Renaming_Of_Object (Id : E) return B is
   begin
      return Flag112 (Id);
   end Is_Renaming_Of_Object;

   function Is_Shared_Passive (Id : E) return B is
   begin
      return Flag60 (Id);
   end Is_Shared_Passive;

   function Is_Statically_Allocated (Id : E) return B is
   begin
      return Flag28 (Id);
   end Is_Statically_Allocated;

   function Is_Tag (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag78 (Id);
   end Is_Tag;

   function Is_Tagged_Type (Id : E) return B is
   begin
      return Flag55 (Id);
   end Is_Tagged_Type;

   function Is_True_Constant (Id : E) return B is
   begin
      return Flag163 (Id);
   end Is_True_Constant;

   function Is_Unchecked_Union (Id : E) return B is
   begin
      return Flag117 (Id);
   end Is_Unchecked_Union;

   function Is_Unsigned_Type (Id : E) return B is
   begin
      pragma Assert (Is_Type (Id));
      return Flag144 (Id);
   end Is_Unsigned_Type;

   function Is_Valued_Procedure (Id : E) return B is
   begin
      pragma Assert (Ekind (Id) = E_Procedure);
      return Flag127 (Id);
   end Is_Valued_Procedure;

   function Is_Visible_Child_Unit (Id : E) return B is
   begin
      pragma Assert (Is_Child_Unit (Id));
      return Flag116 (Id);
   end Is_Visible_Child_Unit;

   function Is_VMS_Exception (Id : E) return B is
   begin
      return Flag133 (Id);
   end Is_VMS_Exception;

   function Is_Volatile (Id : E) return B is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Flag16 (Id);
   end Is_Volatile;

   function Last_Entity (Id : E) return E is
   begin
      return Node20 (Id);
   end Last_Entity;

   function Lit_Name_Table (Id : E) return E is
   begin
      return Node18 (Id);
   end Lit_Name_Table;

   function Machine_Radix_10 (Id : E) return B is
   begin
      pragma Assert (Is_Decimal_Fixed_Point_Type (Id));
      return Flag84 (Id);
   end Machine_Radix_10;

   function Master_Id (Id : E) return E is
   begin
      return Node17 (Id);
   end Master_Id;

   function Materialize_Entity (Id : E) return B is
   begin
      return Flag168 (Id);
   end Materialize_Entity;

   function Mechanism (Id : E) return M is
   begin
      pragma Assert (Ekind (Id) = E_Function or else Is_Formal (Id));
      return UI_To_Int (Uint8 (Id));
   end Mechanism;

   function Modulus (Id : E) return Uint is
   begin
      pragma Assert (Is_Modular_Integer_Type (Id));
      return Uint17 (Base_Type (Id));
   end Modulus;

   function Needs_Debug_Info (Id : E) return B is
   begin
      return Flag147 (Id);
   end Needs_Debug_Info;

   function Needs_No_Actuals (Id : E) return B is
   begin
      pragma Assert
        (Is_Overloadable (Id)
          or else Ekind (Id) = E_Subprogram_Type
          or else Ekind (Id) = E_Entry_Family);
      return Flag22 (Id);
   end Needs_No_Actuals;

   function Next_Inlined_Subprogram (Id : E) return E is
   begin
      return Node12 (Id);
   end Next_Inlined_Subprogram;

   function No_Pool_Assigned (Id : E) return B is
   begin
      pragma Assert (Is_Access_Type (Id));
      return Flag131 (Root_Type (Id));
   end No_Pool_Assigned;

   function No_Return (Id : E) return B is
   begin
      pragma Assert
        (Ekind (Id) = E_Procedure or else Ekind (Id) = E_Generic_Procedure);
      return Flag113 (Id);
   end No_Return;

   function Non_Binary_Modulus (Id : E) return B is
   begin
      pragma Assert (Is_Modular_Integer_Type (Id));
      return Flag58 (Base_Type (Id));
   end Non_Binary_Modulus;

   function Nonzero_Is_True (Id : E) return B is
   begin
      pragma Assert (Root_Type (Id) = Standard_Boolean);
      return Flag162 (Base_Type (Id));
   end Nonzero_Is_True;

   function Not_Source_Assigned (Id : E) return B is
   begin
      return Flag115 (Id);
   end Not_Source_Assigned;

   function Object_Ref (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Protected_Body);
      return Node17 (Id);
   end Object_Ref;

   function Original_Record_Component (Id : E) return E is
   begin
      return Node22 (Id);
   end Original_Record_Component;

   function Packed_Array_Type (Id : E) return E is
   begin
      pragma Assert (Is_Array_Type (Id));
      return Node14 (Id);
   end Packed_Array_Type;

   function Parent_Subtype (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Record_Type);
      return Node19 (Id);
   end Parent_Subtype;

   function Primitive_Operations (Id : E) return L is
   begin
      pragma Assert (Is_Tagged_Type (Id));
      return Elist13 (Id);
   end Primitive_Operations;

   function Prival (Id : E) return E is
   begin
      pragma Assert (Is_Protected_Private (Id));
      return Node17 (Id);
   end Prival;

   function Private_Dependents (Id : E) return L is
   begin
      pragma Assert (Is_Incomplete_Or_Private_Type (Id));
      return Elist18 (Id);
   end Private_Dependents;

   function Private_View (Id : E) return N is
   begin
      pragma Assert (Is_Private_Type (Id));
      return Node22 (Id);
   end Private_View;

   function Privals_Chain (Id : E) return L is
   begin
      pragma Assert (Is_Overloadable (Id)
        or else Ekind (Id) = E_Entry_Family);
      return Elist14 (Id);
   end Privals_Chain;

   function Protected_Body_Subprogram (Id : E) return E is
   begin
      pragma Assert (Is_Subprogram (Id) or else Is_Entry (Id));
      return Node11 (Id);
   end Protected_Body_Subprogram;

   function Protected_Formal (Id : E) return E is
   begin
      pragma Assert (Is_Formal (Id));
      return Node22 (Id);
   end Protected_Formal;

   function Protected_Operation (Id : E) return N is
   begin
      pragma Assert (Is_Protected_Private (Id));
      return Node14 (Id);
   end Protected_Operation;

   function Reachable (Id : E) return B is
   begin
      return Flag49 (Id);
   end Reachable;

   function Referenced (Id : E) return B is
   begin
      return Flag156 (Id);
   end Referenced;

   function Referenced_Object (Id : E) return N is
   begin
      pragma Assert (Is_Type (Id));
      return Node10 (Id);
   end Referenced_Object;

   function Register_Exception_Call (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Exception);
      return Node20 (Id);
   end Register_Exception_Call;

   function Related_Array_Object (Id : E) return E is
   begin
      pragma Assert (Is_Array_Type (Id));
      return Node19 (Id);
   end Related_Array_Object;

   function Related_Instance (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      return Node15 (Id);
   end Related_Instance;

   function Renamed_Entity (Id : E) return N is
   begin
      return Node18 (Id);
   end Renamed_Entity;

   function Renamed_Object (Id : E) return N is
   begin
      return Node18 (Id);
   end Renamed_Object;

   function Renaming_Map (Id : E) return U is
   begin
      return Uint9 (Id);
   end Renaming_Map;

   function Return_Present (Id : E) return B is
   begin
      return Flag54 (Id);
   end Return_Present;

   function Returns_By_Ref (Id : E) return B is
   begin
      return Flag90 (Id);
   end Returns_By_Ref;

   function Reverse_Bit_Order (Id : E) return B is
   begin
      pragma Assert (Is_Record_Type (Id));
      return Flag164 (Base_Type (Id));
   end Reverse_Bit_Order;

   function RM_Size (Id : E) return U is
   begin
      pragma Assert (Is_Discrete_Or_Fixed_Point_Type (Id));
      return Uint13 (Id);
   end RM_Size;

   function Scalar_Range (Id : E) return N is
   begin
      return Node20 (Id);
   end Scalar_Range;

   function Scale_Value (Id : E) return U is
   begin
      return Uint15 (Id);
   end Scale_Value;

   function Scope_Depth (Id : E) return U is
   begin
      return Uint22 (Id);
   end Scope_Depth;

   function Sec_Stack_Needed_For_Return (Id : E) return B is
   begin
      return Flag167 (Id);
   end Sec_Stack_Needed_For_Return;

   function Shadow_Entities (Id : E) return S is
   begin
      pragma Assert
        (Ekind (Id) = E_Package or else Ekind (Id) = E_Generic_Package);
      return List23 (Id);
   end Shadow_Entities;

   function Shared_Mem_Assign_Proc (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      return Node22 (Id);
   end Shared_Mem_Assign_Proc;

   function Shared_Mem_Read_Proc (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      return Node15 (Id);
   end Shared_Mem_Read_Proc;

   function Size_Check_Code (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Constant or else Ekind (Id) = E_Variable);
      return Node9 (Id);
   end Size_Check_Code;

   function Size_Known_At_Compile_Time (Id : E) return B is
   begin
      return Flag92 (Id);
   end Size_Known_At_Compile_Time;

   function Small_Value (Id : E) return R is
   begin
      pragma Assert (Is_Fixed_Point_Type (Id));
      return Ureal21 (Id);
   end Small_Value;

   function Spec_Entity (Id : E) return E is
   begin
      pragma Assert
        (Ekind (Id) = E_Package_Body or else Is_Formal (Id));
      return Node19 (Id);
   end Spec_Entity;

   function Storage_Size_Variable (Id : E) return E is
   begin
      pragma Assert (Is_Access_Type (Id) or else Is_Task_Type (Id));
      return Node15 (Implementation_Base_Type (Id));
   end Storage_Size_Variable;

   function Strict_Alignment (Id : E) return B is
   begin
      return Flag145 (Implementation_Base_Type (Id));
   end Strict_Alignment;

   function String_Literal_Length (Id : E) return U is
   begin
      return Uint16 (Id);
   end String_Literal_Length;

   function String_Literal_Low_Bound (Id : E) return N is
   begin
      return Node15 (Id);
   end String_Literal_Low_Bound;

   function Suppress_Access_Checks (Id : E) return B is
   begin
      return Flag31 (Id);
   end Suppress_Access_Checks;

   function Suppress_Accessibility_Checks (Id : E) return B is
   begin
      return Flag32 (Id);
   end Suppress_Accessibility_Checks;

   function Suppress_Discriminant_Checks (Id : E) return B is
   begin
      return Flag33 (Id);
   end Suppress_Discriminant_Checks;

   function Suppress_Division_Checks (Id : E) return B is
   begin
      return Flag34 (Id);
   end Suppress_Division_Checks;

   function Suppress_Elaboration_Checks (Id : E) return B is
   begin
      return Flag35 (Id);
   end Suppress_Elaboration_Checks;

   function Suppress_Elaboration_Warnings (Id : E) return B is
   begin
      return Flag148 (Id);
   end Suppress_Elaboration_Warnings;

   function Suppress_Index_Checks (Id : E) return B is
   begin
      return Flag36 (Id);
   end Suppress_Index_Checks;

   function Suppress_Init_Proc (Id : E) return B is
   begin
      return Flag105 (Base_Type (Id));
   end Suppress_Init_Proc;

   function Suppress_Length_Checks (Id : E) return B is
   begin
      return Flag37 (Id);
   end Suppress_Length_Checks;

   function Suppress_Overflow_Checks (Id : E) return B is
   begin
      return Flag38 (Id);
   end Suppress_Overflow_Checks;

   function Suppress_Range_Checks (Id : E) return B is
   begin
      return Flag39 (Id);
   end Suppress_Range_Checks;

   function Suppress_Storage_Checks (Id : E) return B is
   begin
      return Flag40 (Id);
   end Suppress_Storage_Checks;

   function Suppress_Style_Checks (Id : E) return B is
   begin
      return Flag165 (Id);
   end Suppress_Style_Checks;

   function Suppress_Tag_Checks (Id : E) return B is
   begin
      return Flag41 (Id);
   end Suppress_Tag_Checks;

   function Table_High_Bound (Id : E) return N is
   begin
      return Node16 (Id);
   end Table_High_Bound;

   function Task_Body_Procedure (Id : E) return E is
   begin
      return Node19 (Id);
   end Task_Body_Procedure;

   function Underlying_Full_View (Id : E) return E is
   begin
      pragma Assert (Ekind (Id) in Private_Kind);
      return Node16 (Id);
   end Underlying_Full_View;

   function Unset_Reference (Id : E) return N is
   begin
      return Node16 (Id);
   end Unset_Reference;

   function Uses_Sec_Stack (Id : E) return B is
   begin
      return Flag95 (Id);
   end Uses_Sec_Stack;

   function Vax_Float (Id : E) return B is
   begin
      return Flag151 (Base_Type (Id));
   end Vax_Float;

   function Warnings_Off (Id : E) return B is
   begin
      return Flag96 (Id);
   end Warnings_Off;

   ------------------------------
   -- Classification Functions --
   ------------------------------

   function Is_Access_Type                      (Id : E) return B is
   begin
      return Ekind (Id) in Access_Kind;
   end Is_Access_Type;

   function Is_Array_Type                       (Id : E) return B is
   begin
      return Ekind (Id) in Array_Kind;
   end Is_Array_Type;

   function Is_Class_Wide_Type                  (Id : E) return B is
   begin
      return Ekind (Id) in Class_Wide_Kind;
   end Is_Class_Wide_Type;

   function Is_Composite_Type                   (Id : E) return B is
   begin
      return Ekind (Id) in Composite_Kind;
   end Is_Composite_Type;

   function Is_Concurrent_Body                  (Id : E) return B is
   begin
      return Ekind (Id) in
        Concurrent_Body_Kind;
   end Is_Concurrent_Body;

   function Is_Concurrent_Record_Type           (Id : E) return B is
   begin
      return Flag20 (Id);
   end Is_Concurrent_Record_Type;

   function Is_Concurrent_Type                  (Id : E) return B is
   begin
      return Ekind (Id) in Concurrent_Kind;
   end Is_Concurrent_Type;

   function Is_Decimal_Fixed_Point_Type         (Id : E) return B is
   begin
      return Ekind (Id) in
        Decimal_Fixed_Point_Kind;
   end Is_Decimal_Fixed_Point_Type;

   function Is_Digits_Type                      (Id : E) return B is
   begin
      return Ekind (Id) in Digits_Kind;
   end Is_Digits_Type;

   function Is_Discrete_Type                    (Id : E) return B is
   begin
      return Ekind (Id) in Discrete_Kind;
   end Is_Discrete_Type;

   function Is_Discrete_Or_Fixed_Point_Type     (Id : E) return B is
   begin
      return Ekind (Id) in Discrete_Or_Fixed_Point_Kind;
   end Is_Discrete_Or_Fixed_Point_Type;

   function Is_Elementary_Type                  (Id : E) return B is
   begin
      return Ekind (Id) in Elementary_Kind;
   end Is_Elementary_Type;

   function Is_Entry                            (Id : E) return B is
   begin
      return Ekind (Id) in Entry_Kind;
   end Is_Entry;

   function Is_Enumeration_Type                 (Id : E) return B is
   begin
      return Ekind (Id) in
        Enumeration_Kind;
   end Is_Enumeration_Type;

   function Is_Fixed_Point_Type                 (Id : E) return B is
   begin
      return Ekind (Id) in
        Fixed_Point_Kind;
   end Is_Fixed_Point_Type;

   function Is_Floating_Point_Type              (Id : E) return B is
   begin
      return Ekind (Id) in Float_Kind;
   end Is_Floating_Point_Type;

   function Is_Formal                           (Id : E) return B is
   begin
      return Ekind (Id) in Formal_Kind;
   end Is_Formal;

   function Is_Generic_Unit                     (Id : E) return B is
   begin
      return Ekind (Id) in Generic_Unit_Kind;
   end Is_Generic_Unit;

   function Is_Incomplete_Or_Private_Type       (Id : E) return B is
   begin
      return Ekind (Id) in
        Incomplete_Or_Private_Kind;
   end Is_Incomplete_Or_Private_Type;

   function Is_Integer_Type                     (Id : E) return B is
   begin
      return Ekind (Id) in Integer_Kind;
   end Is_Integer_Type;

   function Is_Modular_Integer_Type             (Id : E) return B is
   begin
      return Ekind (Id) in
        Modular_Integer_Kind;
   end Is_Modular_Integer_Type;

   function Is_Named_Number                     (Id : E) return B is
   begin
      return Ekind (Id) in Named_Kind;
   end Is_Named_Number;

   function Is_Numeric_Type                     (Id : E) return B is
   begin
      return Ekind (Id) in Numeric_Kind;
   end Is_Numeric_Type;

   function Is_Ordinary_Fixed_Point_Type        (Id : E) return B is
   begin
      return Ekind (Id) in
        Ordinary_Fixed_Point_Kind;
   end Is_Ordinary_Fixed_Point_Type;

   function Is_Object                           (Id : E) return B is
   begin
      return Ekind (Id) in Object_Kind;
   end Is_Object;

   function Is_Overloadable                     (Id : E) return B is
   begin
      return Ekind (Id) in Overloadable_Kind;
   end Is_Overloadable;

   function Is_Private_Type                     (Id : E) return B is
   begin
      return Ekind (Id) in Private_Kind;
   end Is_Private_Type;

   function Is_Protected_Type                   (Id : E) return B is
   begin
      return Ekind (Id) in Protected_Kind;
   end Is_Protected_Type;

   function Is_Real_Type                        (Id : E) return B is
   begin
      return Ekind (Id) in Real_Kind;
   end Is_Real_Type;

   function Is_Record_Type                      (Id : E) return B is
   begin
      return Ekind (Id) in Record_Kind;
   end Is_Record_Type;

   function Is_Scalar_Type                      (Id : E) return B is
   begin
      return Ekind (Id) in Scalar_Kind;
   end Is_Scalar_Type;

   function Is_Signed_Integer_Type              (Id : E) return B is
   begin
      return Ekind (Id) in
        Signed_Integer_Kind;
   end Is_Signed_Integer_Type;

   function Is_Subprogram                       (Id : E) return B is
   begin
      return Ekind (Id) in Subprogram_Kind;
   end Is_Subprogram;

   function Is_Task_Type                        (Id : E) return B is
   begin
      return Ekind (Id) in Task_Kind;
   end Is_Task_Type;

   function Is_Type                             (Id : E) return B is
   begin
      return Ekind (Id) in Type_Kind;
   end Is_Type;

   ------------------------------
   -- Attribute Set Procedures --
   ------------------------------

   procedure Set_Accept_Address (Id : E; V : L) is
   begin
      Set_Elist21 (Id, V);
   end Set_Accept_Address;

   procedure Set_Access_Disp_Table (Id : E; V : E) is
   begin
      pragma Assert (Is_Tagged_Type (Id));
      Set_Node15 (Base_Type (Id), V);
   end Set_Access_Disp_Table;

   procedure Set_Actual_Subtype (Id : E; V : E) is
   begin
      pragma Assert
         (Ekind (Id) = E_Constant
           or else Ekind (Id) = E_Variable
           or else Ekind (Id) = E_Generic_In_Out_Parameter
           or else Ekind (Id) in  E_In_Parameter .. E_In_Out_Parameter);
      Set_Node17 (Id, V);
   end Set_Actual_Subtype;

   procedure Set_Address_Taken (Id : E; V : B := True) is
   begin
      Set_Flag104 (Id, V);
   end Set_Address_Taken;

   procedure Set_Alias (Id : E; V : E) is
   begin
      pragma Assert
        (Is_Overloadable (Id) or else Ekind (Id) = E_Subprogram_Type);
      Set_Node18 (Id, V);
   end Set_Alias;

   procedure Set_Alignment (Id : E; V : U) is
   begin
      Set_Uint23 (Id, V);
   end Set_Alignment;

   procedure Set_Associated_Formal_Package (Id : E; V : E) is
   begin
      Set_Node12 (Id, V);
   end Set_Associated_Formal_Package;

   procedure Set_Associated_Node_For_Itype (Id : E; V : E) is
   begin
      Set_Node8 (Id, V);
   end Set_Associated_Node_For_Itype;

   procedure Set_Associated_Storage_Pool (Id : E; V : E) is
   begin
      pragma Assert (Is_Access_Type (Id));
      Set_Node13 (Id, V);
   end Set_Associated_Storage_Pool;

   procedure Set_Associated_Final_Chain (Id : E; V : E) is
   begin
      pragma Assert (Is_Access_Type (Id));
      Set_Node14 (Id, V);
   end Set_Associated_Final_Chain;

   procedure Set_Barrier_Function (Id : E; V : N) is
   begin
      pragma Assert (Is_Entry (Id));
      Set_Node12 (Id, V);
   end Set_Barrier_Function;

   procedure Set_Block_Node (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) = E_Block);
      Set_Node11 (Id, V);
   end Set_Block_Node;

   procedure Set_Body_Entity (Id : E; V : E) is
   begin
      pragma Assert
        (Ekind (Id) = E_Package or else Ekind (Id) = E_Generic_Package);
      Set_Node13 (Id, V);
   end Set_Body_Entity;

   procedure Set_C_Pass_By_Copy (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Record_Type (Id) and then Id = Base_Type (Id));
      Set_Flag125 (Id, V);
   end Set_C_Pass_By_Copy;

   procedure Set_Class_Wide_Type (Id : E; V : E) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Node9 (Id, V);
   end Set_Class_Wide_Type;

   procedure Set_Cloned_Subtype (Id : E; V : E) is
   begin
      pragma Assert
        (Ekind (Id) = E_Record_Subtype
         or else Ekind (Id) = E_Class_Wide_Subtype);
      Set_Node16 (Id, V);
   end Set_Cloned_Subtype;

   procedure Set_Component_Clause (Id : E; V : N) is
   begin
      pragma Assert
        (Ekind (Id) = E_Component or else Ekind (Id) = E_Discriminant);
      Set_Node13 (Id, V);
   end Set_Component_Clause;

   procedure Set_Component_First_Bit (Id : E; V : U) is
   begin
      pragma Assert
        (Ekind (Id) = E_Component or else Ekind (Id) = E_Discriminant);
      Set_Uint11 (Id, V);
   end Set_Component_First_Bit;

   procedure Set_Component_Size (Id : E; V : U) is
   begin
      pragma Assert (Is_Array_Type (Id));
      Set_Uint13 (Base_Type (Id), V);
   end Set_Component_Size;

   procedure Set_Component_Type (Id : E; V : E) is
   begin
      Set_Node20 (Id, V);
   end Set_Component_Type;

   procedure Set_Corresponding_Concurrent_Type (Id : E; V : E) is
   begin
      pragma Assert
        (Ekind (Id) = E_Record_Type and then Is_Concurrent_Type (V));
      Set_Node18 (Id, V);
   end Set_Corresponding_Concurrent_Type;

   procedure Set_Corresponding_Discriminant (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      Set_Node19 (Id, V);
   end Set_Corresponding_Discriminant;

   procedure Set_Corresponding_Equality (Id : E; V : E) is
   begin
      pragma Assert
        (Ekind (Id) = E_Function
          and then not Comes_From_Source (Id)
          and then Chars (Id) = Name_Op_Ne);
      Set_Node19 (Id, V);
   end Set_Corresponding_Equality;

   procedure Set_Corresponding_Record_Type (Id : E; V : E) is
   begin
      pragma Assert (Is_Concurrent_Type (Id));
      Set_Node18 (Id, V);
   end Set_Corresponding_Record_Type;

   procedure Set_Corresponding_Remote_Type (Id : E; V : E) is
   begin
      Set_Node22 (Id, V);
   end Set_Corresponding_Remote_Type;

   procedure Set_CR_Discriminant (Id : E; V : E) is
   begin
      Set_Node14 (Id, V);
   end Set_CR_Discriminant;

   procedure Set_Default_Expr_Function (Id : E; V : E) is
   begin
      pragma Assert (Is_Formal (Id));
      Set_Node12 (Id, V);
   end Set_Default_Expr_Function;

   procedure Set_Default_Expressions_Processed (Id : E; V : B := True) is
   begin
      Set_Flag108 (Id, V);
   end Set_Default_Expressions_Processed;

   procedure Set_Default_Value (Id : E; V : N) is
   begin
      pragma Assert (Is_Formal (Id));
      Set_Node20 (Id, V);
   end Set_Default_Value;

   procedure Set_Delay_Cleanups (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Subprogram (Id)
           or else Is_Task_Type (Id)
           or else Ekind (Id) = E_Block);
      Set_Flag114 (Id, V);
   end Set_Delay_Cleanups;

   procedure Set_Delay_Subprogram_Descriptors (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Subprogram (Id)
           or else Ekind (Id) = E_Package
           or else Ekind (Id) = E_Package_Body);
      Set_Flag50 (Id, V);
   end Set_Delay_Subprogram_Descriptors;

   procedure Set_Delta_Value (Id : E; V : R) is
   begin
      pragma Assert (Is_Fixed_Point_Type (Id));
      Set_Ureal18 (Id, V);
   end Set_Delta_Value;

   procedure Set_Digits_Value (Id : E; V : U) is
   begin
      pragma Assert
        (Is_Floating_Point_Type (Id)
          or else Is_Decimal_Fixed_Point_Type (Id));
      Set_Uint17 (Id, V);
   end Set_Digits_Value;

   procedure Set_Directly_Designated_Type (Id : E; V : E) is
   begin
      Set_Node20 (Id, V);
   end Set_Directly_Designated_Type;

   procedure Set_Discard_Names (Id : E; V : B := True) is
   begin
      Set_Flag88 (Id, V);
   end Set_Discard_Names;

   procedure Set_Discriminal (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      Set_Node17 (Id, V);
   end Set_Discriminal;

   procedure Set_Discriminal_Link (Id : E; V : E) is
   begin
      Set_Node10 (Id, V);
   end Set_Discriminal_Link;

   procedure Set_Discriminant_Checking_Func (Id  : E; V : E) is
   begin
      pragma Assert
        (Ekind (Id) = E_Component and Ekind (Scope (Id)) in Record_Kind);
      Set_Node20 (Id, V);
   end Set_Discriminant_Checking_Func;

   procedure Set_Discriminant_Constraint (Id : E; V : L) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Elist21 (Id, V);
   end Set_Discriminant_Constraint;

   procedure Set_Discriminant_Default_Value (Id : E; V : N) is
   begin
      Set_Node20 (Id, V);
   end Set_Discriminant_Default_Value;

   procedure Set_Discriminant_Number (Id : E; V : U) is
   begin
      Set_Uint15 (Id, V);
   end Set_Discriminant_Number;

   procedure Set_Debug_Info_Off (Id : E; V : B := True) is
   begin
      Set_Flag166 (Id, V);
   end Set_Debug_Info_Off;

   procedure Set_Debug_Renaming_Link (Id : E; V : E) is
   begin
      Set_Node13 (Id, V);
   end Set_Debug_Renaming_Link;

   procedure Set_DTC_Entity (Id : E; V : E) is
   begin
      pragma Assert
        (Ekind (Id) = E_Function or else Ekind (Id) = E_Procedure);
      Set_Node16 (Id, V);
   end Set_DTC_Entity;

   procedure Set_DT_Entry_Count (Id : E; V : U) is
   begin
      pragma Assert (Ekind (Id) = E_Component);
      Set_Uint15 (Id, V);
   end Set_DT_Entry_Count;

   procedure Set_DT_Position (Id : E; V : U) is
   begin
      pragma Assert (Ekind (Id) = E_Function or else Ekind (Id) = E_Procedure);
      Set_Uint15 (Id, V);
   end Set_DT_Position;

   procedure Set_Elaborate_All_Desirable (Id : E; V : B := True) is
   begin
      Set_Flag146 (Id, V);
   end Set_Elaborate_All_Desirable;

   procedure Set_Elaboration_Entity (Id : E; V : E) is
   begin
      pragma Assert
        (Is_Subprogram (Id)
           or else
         Ekind (Id) = E_Package
           or else
         Is_Generic_Unit (Id));
      Set_Node19 (Id, V);
   end Set_Elaboration_Entity;

   procedure Set_Enclosing_Scope (Id : E; V : E) is
   begin
      Set_Node18 (Id, V);
   end Set_Enclosing_Scope;

   procedure Set_Entry_Accepted (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Entry (Id));
      Set_Flag152 (Id, V);
   end Set_Entry_Accepted;

   procedure Set_Entry_Bodies_Array (Id : E; V : E) is
   begin
      Set_Node15 (Id, V);
   end Set_Entry_Bodies_Array;

   procedure Set_Entry_Cancel_Parameter (Id : E; V : E) is
   begin
      Set_Node14 (Id, V);
   end Set_Entry_Cancel_Parameter;

   procedure Set_Entry_Component (Id : E; V : E) is
   begin
      Set_Node11 (Id, V);
   end Set_Entry_Component;

   procedure Set_Entry_Formal (Id : E; V : E) is
   begin
      Set_Node16 (Id, V);
   end Set_Entry_Formal;

   procedure Set_Entry_Index_Constant (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Entry_Index_Parameter);
      Set_Node18 (Id, V);
   end Set_Entry_Index_Constant;

   procedure Set_Entry_Parameters_Type (Id : E; V : E) is
   begin
      Set_Node15 (Id, V);
   end Set_Entry_Parameters_Type;

   procedure Set_Enumeration_Pos (Id : E; V : U) is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Literal);
      Set_Uint11 (Id, V);
   end Set_Enumeration_Pos;

   procedure Set_Enumeration_Rep (Id : E; V : U) is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Literal);
      Set_Uint12 (Id, V);
   end Set_Enumeration_Rep;

   procedure Set_Enumeration_Rep_Expr (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Literal);
      Set_Node22 (Id, V);
   end Set_Enumeration_Rep_Expr;

   procedure Set_Enum_Pos_To_Rep (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Enumeration_Type);
      Set_Node14 (Id, V);
   end Set_Enum_Pos_To_Rep;

   procedure Set_Equivalent_Type (Id : E; V : E) is
   begin
      pragma Assert
        (Ekind (Id) = E_Class_Wide_Type                  or else
         Ekind (Id) = E_Class_Wide_Subtype               or else
         Ekind (Id) = E_Access_Protected_Subprogram_Type or else
         Ekind (Id) = E_Access_Subprogram_Type           or else
         Ekind (Id) = E_Exception_Type);
      Set_Node18 (Id, V);
   end Set_Equivalent_Type;

   procedure Set_Esize (Id : E; V : U) is
   begin
      Set_Uint12 (Id, V);
   end Set_Esize;

   procedure Set_Exception_Code (Id : E; V : U) is
   begin
      pragma Assert (Ekind (Id) = E_Exception);
      Set_Uint22 (Id, V);
   end Set_Exception_Code;

   procedure Set_Extra_Formal (Id : E; V : E) is
   begin
      Set_Node15 (Id, V);
   end Set_Extra_Formal;

   procedure Set_Extra_Accessibility (Id : E; V : E) is
   begin
      pragma Assert (Is_Formal (Id) or else Ekind (Id) = E_Variable);
      Set_Node13 (Id, V);
   end Set_Extra_Accessibility;

   procedure Set_Extra_Constrained (Id : E; V : E) is
   begin
      pragma Assert (Is_Formal (Id) or else Ekind (Id) = E_Variable);
      Set_Node14 (Id, V);
   end Set_Extra_Constrained;

   procedure Set_Finalization_Chain_Entity (Id : E; V : E) is
   begin
      Set_Node13 (Id, V);
   end Set_Finalization_Chain_Entity;

   procedure Set_Finalize_Storage_Only (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag158 (Base_Type (Id), V);
   end Set_Finalize_Storage_Only;

   procedure Set_First_Entity (Id : E; V : E) is
   begin
      Set_Node17 (Id, V);
   end Set_First_Entity;

   procedure Set_First_Index (Id : E; V : N) is
   begin
      Set_Node17 (Id, V);
   end Set_First_Index;

   procedure Set_First_Literal (Id : E; V : E) is
   begin
      Set_Node17 (Id, V);
   end Set_First_Literal;

   procedure Set_First_Optional_Parameter (Id : E; V : E) is
   begin
      pragma Assert
        (Ekind (Id) = E_Function or else Ekind (Id) = E_Procedure);
      Set_Node23 (Id, V);
   end Set_First_Optional_Parameter;

   procedure Set_First_Private_Entity (Id : E; V : E) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Node16 (Id, V);
   end Set_First_Private_Entity;

   procedure Set_First_Rep_Item (Id : E; V : N) is
   begin
      Set_Node6 (Id, V);
   end Set_First_Rep_Item;

   procedure Set_Freeze_Node (Id : E; V : N) is
   begin
      Set_Node7 (Id, V);
   end Set_Freeze_Node;

   procedure Set_From_With_Type (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Type (Id)
          or else Ekind (Id) = E_Package);
      Set_Flag159 (Id, V);
   end Set_From_With_Type;

   procedure Set_Full_View (Id : E; V : E) is
   begin
      pragma Assert (Is_Type (Id) or else Ekind (Id) = E_Constant);
      Set_Node11 (Id, V);
   end Set_Full_View;

   procedure Set_Function_Returns_With_DSP (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Subprogram (Id) or else Ekind (Id) = E_Subprogram_Type);
      Set_Flag169 (Id, V);
   end Set_Function_Returns_With_DSP;

   procedure Set_Dependent_Instances (Id : E; V : L) is
   begin
      pragma Assert (Is_Generic_Instance (Id));
      Set_Elist8 (Id, V);
   end Set_Dependent_Instances;

   procedure Set_Generic_Renamings (Id : E; V : L) is
   begin
      Set_Elist14 (Id, V);
   end Set_Generic_Renamings;

   procedure Set_Girder_Constraint (Id : E; V : L) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Elist14 (Id, V);
   end Set_Girder_Constraint;

   procedure Set_Handler_Records (Id : E; V : S) is
   begin
      Set_List10 (Id, V);
   end Set_Handler_Records;

   procedure Set_Has_Aliased_Components (Id : E; V : B := True) is
   begin
      pragma Assert (Base_Type (Id) = Id);
      Set_Flag135 (Id, V);
   end Set_Has_Aliased_Components;

   procedure Set_Has_Alignment_Clause (Id : E; V : B := True) is
   begin
      Set_Flag46 (Id, V);
   end Set_Has_Alignment_Clause;

   procedure Set_Has_All_Calls_Remote (Id : E; V : B := True) is
   begin
      Set_Flag79 (Id, V);
   end Set_Has_All_Calls_Remote;

   procedure Set_Has_Atomic_Components (Id : E; V : B := True) is
   begin
      pragma Assert (not Is_Type (Id) or else Base_Type (Id) = Id);
      Set_Flag86 (Id, V);
   end Set_Has_Atomic_Components;

   procedure Set_Has_Biased_Representation (Id : E; V : B := True) is
   begin
      pragma Assert
        ((V = False) or else (Is_Discrete_Type (Id) or Is_Object (Id)));
      Set_Flag139 (Id, V);
   end Set_Has_Biased_Representation;

   procedure Set_Has_Completion (Id : E; V : B := True) is
   begin
      Set_Flag26 (Id, V);
   end Set_Has_Completion;

   procedure Set_Has_Completion_In_Body (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Incomplete_Type);
      Set_Flag71 (Id, V);
   end Set_Has_Completion_In_Body;

   procedure Set_Has_Complex_Representation (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Record_Type (Id));
      Set_Flag140 (Implementation_Base_Type (Id), V);
   end Set_Has_Complex_Representation;

   procedure Set_Has_Component_Size_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Array_Type (Id));
      Set_Flag68 (Implementation_Base_Type (Id), V);
   end Set_Has_Component_Size_Clause;

   procedure Set_Has_Controlled_Component (Id : E; V : B := True) is
   begin
      pragma Assert (Base_Type (Id) = Id);
      Set_Flag43 (Id, V);
   end Set_Has_Controlled_Component;

   procedure Set_Has_Controlling_Result (Id : E; V : B := True) is
   begin
      Set_Flag98 (Id, V);
   end Set_Has_Controlling_Result;

   procedure Set_Has_Convention_Pragma (Id : E; V : B := True) is
   begin
      Set_Flag119 (Id, V);
   end Set_Has_Convention_Pragma;

   procedure Set_Has_Delayed_Freeze (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag18 (Id, V);
   end Set_Has_Delayed_Freeze;

   procedure Set_Has_Discriminants (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag5 (Id, V);
   end Set_Has_Discriminants;

   procedure Set_Has_Enumeration_Rep_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Enumeration_Type (Id));
      Set_Flag66 (Id, V);
   end Set_Has_Enumeration_Rep_Clause;

   procedure Set_Has_Exit (Id : E; V : B := True) is
   begin
      Set_Flag47 (Id, V);
   end Set_Has_Exit;

   procedure Set_Has_External_Tag_Rep_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Tagged_Type (Id));
      Set_Flag110 (Id, V);
   end Set_Has_External_Tag_Rep_Clause;

   procedure Set_Has_Gigi_Rep_Item (Id : E; V : B := True) is
   begin
      Set_Flag82 (Id, V);
   end Set_Has_Gigi_Rep_Item;

   procedure Set_Has_Homonym (Id : E; V : B := True) is
   begin
      Set_Flag56 (Id, V);
   end Set_Has_Homonym;

   procedure Set_Has_Machine_Radix_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Decimal_Fixed_Point_Type (Id));
      Set_Flag83 (Id, V);
   end Set_Has_Machine_Radix_Clause;

   procedure Set_Has_Master_Entity (Id : E; V : B := True) is
   begin
      Set_Flag21 (Id, V);
   end Set_Has_Master_Entity;

   procedure Set_Has_Missing_Return (Id : E; V : B := True) is
   begin
      pragma Assert
        (Ekind (Id) = E_Function or else Ekind (Id) = E_Generic_Function);
      Set_Flag142 (Id, V);
   end Set_Has_Missing_Return;

   procedure Set_Has_Nested_Block_With_Handler (Id : E; V : B := True) is
   begin
      Set_Flag101 (Id, V);
   end Set_Has_Nested_Block_With_Handler;

   procedure Set_Has_Non_Standard_Rep (Id : E; V : B := True) is
   begin
      pragma Assert (Base_Type (Id) = Id);
      Set_Flag75 (Id, V);
   end Set_Has_Non_Standard_Rep;

   procedure Set_Has_Pragma_Controlled (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Access_Type (Id));
      Set_Flag27 (Base_Type (Id), V);
   end Set_Has_Pragma_Controlled;

   procedure Set_Has_Pragma_Elaborate_Body (Id : E; V : B := True) is
   begin
      Set_Flag150 (Id, V);
   end Set_Has_Pragma_Elaborate_Body;

   procedure Set_Has_Pragma_Inline (Id : E; V : B := True) is
   begin
      Set_Flag157 (Id, V);
   end Set_Has_Pragma_Inline;

   procedure Set_Has_Pragma_Pack (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Array_Type (Id) or else Is_Record_Type (Id));
      Set_Flag121 (Implementation_Base_Type (Id), V);
   end Set_Has_Pragma_Pack;

   procedure Set_Has_Per_Object_Constraint (Id : E; V : B := True) is
   begin
      Set_Flag154 (Id, V);
   end Set_Has_Per_Object_Constraint;

   procedure Set_Has_Primitive_Operations (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag120 (Base_Type (Id), V);
   end Set_Has_Primitive_Operations;

   procedure Set_Has_Private_Declaration (Id : E; V : B := True) is
   begin
      Set_Flag155 (Id, V);
   end Set_Has_Private_Declaration;

   procedure Set_Has_Qualified_Name (Id : E; V : B := True) is
   begin
      Set_Flag161 (Id, V);
   end Set_Has_Qualified_Name;

   procedure Set_Has_Record_Rep_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Record_Type (Id));
      Set_Flag65 (Id, V);
   end Set_Has_Record_Rep_Clause;

   procedure Set_Has_Recursive_Call (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Subprogram (Id));
      Set_Flag143 (Id, V);
   end Set_Has_Recursive_Call;

   procedure Set_Has_Size_Clause (Id : E; V : B := True) is
   begin
      Set_Flag29 (Id, V);
   end Set_Has_Size_Clause;

   procedure Set_Has_Small_Clause (Id : E; V : B := True) is
   begin
      Set_Flag67 (Id, V);
   end Set_Has_Small_Clause;

   procedure Set_Has_Specified_Layout (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag100 (Id, V);
   end Set_Has_Specified_Layout;

   procedure Set_Has_Storage_Size_Clause (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Access_Type (Id) or else Is_Task_Type (Id));
      pragma Assert (Base_Type (Id) = Id);
      Set_Flag23 (Id, V);
   end Set_Has_Storage_Size_Clause;

   procedure Set_Has_Subprogram_Descriptor (Id : E; V : B := True) is
   begin
      Set_Flag93 (Id, V);
   end Set_Has_Subprogram_Descriptor;

   procedure Set_Has_Task (Id : E; V : B := True) is
   begin
      pragma Assert (Base_Type (Id) = Id);
      Set_Flag30 (Id, V);
   end Set_Has_Task;

   procedure Set_Has_Unchecked_Union (Id : E; V : B := True) is
   begin
      pragma Assert (Base_Type (Id) = Id);
      Set_Flag123 (Id, V);
   end Set_Has_Unchecked_Union;

   procedure Set_Has_Unknown_Discriminants (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag72 (Id, V);
   end Set_Has_Unknown_Discriminants;

   procedure Set_Has_Volatile_Components (Id : E; V : B := True) is
   begin
      pragma Assert (not Is_Type (Id) or else Base_Type (Id) = Id);
      Set_Flag87 (Id, V);
   end Set_Has_Volatile_Components;

   procedure Set_Hiding_Loop_Variable (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      Set_Node8 (Id, V);
   end Set_Hiding_Loop_Variable;

   procedure Set_In_Package_Body (Id : E; V : B := True) is
   begin
      Set_Flag48 (Id, V);
   end Set_In_Package_Body;

   procedure Set_In_Private_Part (Id : E; V : B := True) is
   begin
      Set_Flag45 (Id, V);
   end Set_In_Private_Part;

   procedure Set_Inner_Instances (Id : E; V : L) is
   begin
      Set_Elist14 (Id, V);
   end Set_Inner_Instances;

   procedure Set_In_Use (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag8 (Id, V);
   end Set_In_Use;

   procedure Set_Interface_Name (Id : E; V : N) is
   begin
      Set_Node21 (Id, V);
   end Set_Interface_Name;

   procedure Set_Is_Abstract (Id : E; V : B := True) is
   begin
      Set_Flag19 (Id, V);
   end Set_Is_Abstract;

   procedure Set_Is_Access_Constant (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Access_Type (Id));
      Set_Flag69 (Id, V);
   end Set_Is_Access_Constant;

   procedure Set_Is_Aliased (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag15 (Id, V);
   end Set_Is_Aliased;

   procedure Set_Is_AST_Entry (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Entry (Id));
      Set_Flag132 (Id, V);
   end Set_Is_AST_Entry;

   procedure Set_Is_Asynchronous (Id : E; V : B := True) is
   begin
      pragma Assert
        (Ekind (Id) = E_Procedure or else Is_Type (Id));
      Set_Flag81 (Id, V);
   end Set_Is_Asynchronous;

   procedure Set_Is_Atomic (Id : E; V : B := True) is
   begin
      Set_Flag85 (Id, V);
   end Set_Is_Atomic;

   procedure Set_Is_Bit_Packed_Array (Id : E; V : B := True) is
   begin
      Set_Flag122 (Implementation_Base_Type (Id), V);
   end Set_Is_Bit_Packed_Array;

   procedure Set_Is_Called (Id : E; V : B := True) is
   begin
      pragma Assert
        (Ekind (Id) = E_Procedure or else Ekind (Id) = E_Function);
      Set_Flag102 (Id, V);
   end Set_Is_Called;

   procedure Set_Is_Character_Type (Id : E; V : B := True) is
   begin
      Set_Flag63 (Id, V);
   end Set_Is_Character_Type;

   procedure Set_Is_Child_Unit (Id : E; V : B := True) is
   begin
      Set_Flag73 (Id, V);
   end Set_Is_Child_Unit;

   procedure Set_Is_Compilation_Unit (Id : E; V : B := True) is
   begin
      Set_Flag149 (Id, V);
   end Set_Is_Compilation_Unit;

   procedure Set_Is_Completely_Hidden (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Discriminant);
      Set_Flag103 (Id, V);
   end Set_Is_Completely_Hidden;

   procedure Set_Is_Constrained (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag12 (Id, V);
   end Set_Is_Constrained;

   procedure Set_Is_Constr_Subt_For_U_Nominal (Id : E; V : B := True) is
   begin
      Set_Flag80 (Id, V);
   end Set_Is_Constr_Subt_For_U_Nominal;

   procedure Set_Is_Constr_Subt_For_UN_Aliased (Id : E; V : B := True) is
   begin
      Set_Flag141 (Id, V);
   end Set_Is_Constr_Subt_For_UN_Aliased;

   procedure Set_Is_Constructor (Id : E; V : B := True) is
   begin
      Set_Flag76 (Id, V);
   end Set_Is_Constructor;

   procedure Set_Is_Controlled (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag42 (Id, V);
   end Set_Is_Controlled;

   procedure Set_Is_Controlling_Formal (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Formal (Id));
      Set_Flag97 (Id, V);
   end Set_Is_Controlling_Formal;

   procedure Set_Is_CPP_Class (Id : E; V : B := True) is
   begin
      Set_Flag74 (Id, V);
   end Set_Is_CPP_Class;

   procedure Set_Is_Destructor (Id : E; V : B := True) is
   begin
      Set_Flag77 (Id, V);
   end Set_Is_Destructor;

   procedure Set_Is_Dispatching_Operation (Id : E; V : B := True) is
   begin
      pragma Assert
        (V = False
           or else
         Is_Overloadable (Id)
           or else
         Ekind (Id) = E_Subprogram_Type);

      Set_Flag6 (Id, V);
   end Set_Is_Dispatching_Operation;

   procedure Set_Is_Entry_Formal (Id : E; V : B := True) is
   begin
      Set_Flag52 (Id, V);
   end Set_Is_Entry_Formal;

   procedure Set_Is_Eliminated (Id : E; V : B := True) is
   begin
      Set_Flag124 (Id, V);
   end Set_Is_Eliminated;

   procedure Set_Is_Exported (Id : E; V : B := True) is
   begin
      Set_Flag99 (Id, V);
   end Set_Is_Exported;

   procedure Set_Is_Formal_Subprogram (Id : E; V : B := True) is
   begin
      Set_Flag111 (Id, V);
   end Set_Is_Formal_Subprogram;

   procedure Set_Is_First_Subtype (Id : E; V : B := True) is
   begin
      Set_Flag70 (Id, V);
   end Set_Is_First_Subtype;

   procedure Set_Is_For_Access_Subtype (Id : E; V : B := True) is
   begin
      pragma Assert
        (Ekind (Id) = E_Record_Subtype
          or else
         Ekind (Id) = E_Private_Subtype);
      Set_Flag118 (Id, V);
   end Set_Is_For_Access_Subtype;

   procedure Set_Is_Frozen (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag4 (Id, V);
   end Set_Is_Frozen;

   procedure Set_Is_Generic_Type (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag13 (Id, V);
   end Set_Is_Generic_Type;

   procedure Set_Is_Generic_Actual_Type (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag94 (Id, V);
   end Set_Is_Generic_Actual_Type;

   procedure Set_Is_Generic_Instance (Id : E; V : B := True) is
   begin
      Set_Flag130 (Id, V);
   end Set_Is_Generic_Instance;

   procedure Set_Is_Hidden (Id : E; V : B := True) is
   begin
      Set_Flag57 (Id, V);
   end Set_Is_Hidden;

   procedure Set_Is_Hidden_Open_Scope (Id : E; V : B := True) is
   begin
      Set_Flag171 (Id, V);
   end Set_Is_Hidden_Open_Scope;

   procedure Set_Is_Immediately_Visible (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag7 (Id, V);
   end Set_Is_Immediately_Visible;

   procedure Set_Is_Imported (Id : E; V : B := True) is
   begin
      Set_Flag24 (Id, V);
   end Set_Is_Imported;

   procedure Set_Is_Inlined (Id : E; V : B := True) is
   begin
      Set_Flag11 (Id, V);
   end Set_Is_Inlined;

   procedure Set_Is_Instantiated (Id : E; V : B := True) is
   begin
      Set_Flag126 (Id, V);
   end Set_Is_Instantiated;

   procedure Set_Is_Internal (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag17 (Id, V);
   end Set_Is_Internal;

   procedure Set_Is_Interrupt_Handler (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag89 (Id, V);
   end Set_Is_Interrupt_Handler;

   procedure Set_Is_Intrinsic_Subprogram (Id : E; V : B := True) is
   begin
      Set_Flag64 (Id, V);
   end Set_Is_Intrinsic_Subprogram;

   procedure Set_Is_Itype (Id : E; V : B := True) is
   begin
      Set_Flag91 (Id, V);
   end Set_Is_Itype;

   procedure Set_Is_Known_Valid (Id : E; V : B := True) is
   begin
      Set_Flag170 (Id, V);
   end Set_Is_Known_Valid;

   procedure Set_Is_Limited_Composite (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag106 (Id, V);
   end Set_Is_Limited_Composite;

   procedure Set_Is_Limited_Record (Id : E; V : B := True) is
   begin
      Set_Flag25 (Id, V);
   end Set_Is_Limited_Record;

   procedure Set_Is_Machine_Code_Subprogram (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Subprogram (Id));
      Set_Flag137 (Id, V);
   end Set_Is_Machine_Code_Subprogram;

   procedure Set_Is_Non_Static_Subtype (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag109 (Id, V);
   end Set_Is_Non_Static_Subtype;

   procedure Set_Is_Optional_Parameter (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Formal (Id));
      Set_Flag134 (Id, V);
   end Set_Is_Optional_Parameter;

   procedure Set_Is_Package_Body_Entity (Id : E; V : B := True) is
   begin
      Set_Flag160 (Id, V);
   end Set_Is_Package_Body_Entity;

   procedure Set_Is_Packed (Id : E; V : B := True) is
   begin
      pragma Assert (Base_Type (Id) = Id);
      Set_Flag51 (Id, V);
   end Set_Is_Packed;

   procedure Set_Is_Packed_Array_Type (Id : E; V : B := True) is
   begin
      Set_Flag138 (Id, V);
   end Set_Is_Packed_Array_Type;

   procedure Set_Is_Potentially_Use_Visible (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag9 (Id, V);
   end Set_Is_Potentially_Use_Visible;

   procedure Set_Is_Preelaborated (Id : E; V : B := True) is
   begin
      Set_Flag59 (Id, V);
   end Set_Is_Preelaborated;

   procedure Set_Is_Private_Composite (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Flag107 (Id, V);
   end Set_Is_Private_Composite;

   procedure Set_Is_Private_Descendant (Id : E; V : B := True) is
   begin
      Set_Flag53 (Id, V);
   end Set_Is_Private_Descendant;

   procedure Set_Is_Psected (Id : E; V : B := True) is
   begin
      Set_Flag153 (Id, V);
   end Set_Is_Psected;

   procedure Set_Depends_On_Private (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag14 (Id, V);
   end Set_Depends_On_Private;

   procedure Set_Is_Public (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag10 (Id, V);
   end Set_Is_Public;

   procedure Set_Is_Pure (Id : E; V : B := True) is
   begin
      Set_Flag44 (Id, V);
   end Set_Is_Pure;

   procedure Set_Is_Remote_Call_Interface (Id : E; V : B := True) is
   begin
      Set_Flag62 (Id, V);
   end Set_Is_Remote_Call_Interface;

   procedure Set_Is_Remote_Types (Id : E; V : B := True) is
   begin
      Set_Flag61 (Id, V);
   end Set_Is_Remote_Types;

   procedure Set_Is_Renaming_Of_Object (Id : E; V : B := True) is
   begin
      Set_Flag112 (Id, V);
   end Set_Is_Renaming_Of_Object;

   procedure Set_Is_Shared_Passive (Id : E; V : B := True) is
   begin
      Set_Flag60 (Id, V);
   end Set_Is_Shared_Passive;

   procedure Set_Is_Statically_Allocated (Id : E; V : B := True) is
   begin
      Set_Flag28 (Id, V);
   end Set_Is_Statically_Allocated;

   procedure Set_Is_Tagged_Type (Id : E; V : B := True) is
   begin
      Set_Flag55 (Id, V);
   end Set_Is_Tagged_Type;

   procedure Set_Is_True_Constant (Id : E; V : B := True) is
   begin
      Set_Flag163 (Id, V);
   end Set_Is_True_Constant;

   procedure Set_Is_Unchecked_Union (Id : E; V : B := True) is
   begin
      pragma Assert (Base_Type (Id) = Id);
      Set_Flag117 (Id, V);
   end Set_Is_Unchecked_Union;

   procedure Set_Is_Unsigned_Type (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Discrete_Or_Fixed_Point_Type (Id));
      Set_Flag144 (Id, V);
   end Set_Is_Unsigned_Type;

   procedure Set_Is_Concurrent_Record_Type (Id : E; V : B := True) is
   begin
      Set_Flag20 (Id, V);
   end Set_Is_Concurrent_Record_Type;

   procedure Set_Is_Tag (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag78 (Id, V);
   end Set_Is_Tag;

   procedure Set_Is_Valued_Procedure (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Procedure);
      Set_Flag127 (Id, V);
   end Set_Is_Valued_Procedure;

   procedure Set_Is_Visible_Child_Unit (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Child_Unit (Id));
      Set_Flag116 (Id, V);
   end Set_Is_Visible_Child_Unit;

   procedure Set_Is_VMS_Exception (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Exception);
      Set_Flag133 (Id, V);
   end Set_Is_VMS_Exception;

   procedure Set_Is_Volatile (Id : E; V : B := True) is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      Set_Flag16 (Id, V);
   end Set_Is_Volatile;

   procedure Set_Last_Entity (Id : E; V : E) is
   begin
      Set_Node20 (Id, V);
   end Set_Last_Entity;

   procedure Set_Lit_Name_Table (Id : E; V : E) is
   begin
      Set_Node18 (Id, V);
   end Set_Lit_Name_Table;

   procedure Set_Machine_Radix_10 (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Decimal_Fixed_Point_Type (Id));
      Set_Flag84 (Id, V);
   end Set_Machine_Radix_10;

   procedure Set_Master_Id (Id : E; V : E) is
   begin
      Set_Node17 (Id, V);
   end Set_Master_Id;

   procedure Set_Materialize_Entity (Id : E; V : B := True) is
   begin
      Set_Flag168 (Id, V);
   end Set_Materialize_Entity;

   procedure Set_Mechanism (Id : E; V : M) is
   begin
      pragma Assert (Ekind (Id) = E_Function or else Is_Formal (Id));
      Set_Uint8 (Id, UI_From_Int (V));
   end Set_Mechanism;

   procedure Set_Modulus (Id : E; V : U) is
   begin
      pragma Assert (Ekind (Id) = E_Modular_Integer_Type);
      Set_Uint17 (Id, V);
   end Set_Modulus;

   procedure Set_Needs_Debug_Info (Id : E; V : B := True) is
   begin
      Set_Flag147 (Id, V);
   end Set_Needs_Debug_Info;

   procedure Set_Needs_No_Actuals (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Overloadable (Id)
          or else Ekind (Id) = E_Subprogram_Type
          or else Ekind (Id) = E_Entry_Family);
      Set_Flag22 (Id, V);
   end Set_Needs_No_Actuals;

   procedure Set_Next_Inlined_Subprogram (Id : E; V : E) is
   begin
      Set_Node12 (Id, V);
   end Set_Next_Inlined_Subprogram;

   procedure Set_No_Pool_Assigned (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Access_Type (Id) and then Root_Type (Id) = Id);
      Set_Flag131 (Id, V);
   end Set_No_Pool_Assigned;

   procedure Set_No_Return (Id : E; V : B := True) is
   begin
      pragma Assert
        (Ekind (Id) = E_Procedure or else Ekind (Id) = E_Generic_Procedure);
      Set_Flag113 (Id, V);
   end Set_No_Return;

   procedure Set_Non_Binary_Modulus (Id : E; V : B := True) is
   begin
      pragma Assert (Ekind (Id) = E_Modular_Integer_Type);
      Set_Flag58 (Id, V);
   end Set_Non_Binary_Modulus;

   procedure Set_Nonzero_Is_True (Id : E; V : B := True) is
   begin
      pragma Assert
        (Root_Type (Id) = Standard_Boolean
          and then Ekind (Id) = E_Enumeration_Type);
      Set_Flag162 (Id, V);
   end Set_Nonzero_Is_True;

   procedure Set_Not_Source_Assigned (Id : E; V : B := True) is
   begin
      Set_Flag115 (Id, V);
   end Set_Not_Source_Assigned;

   procedure Set_Object_Ref (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Protected_Body);
      Set_Node17 (Id, V);
   end Set_Object_Ref;

   procedure Set_Original_Record_Component (Id : E; V : E) is
   begin
      Set_Node22 (Id, V);
   end Set_Original_Record_Component;

   procedure Set_Packed_Array_Type (Id : E; V : E) is
   begin
      pragma Assert (Is_Array_Type (Id));
      Set_Node14 (Id, V);
   end Set_Packed_Array_Type;

   procedure Set_Parent_Subtype (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Record_Type);
      Set_Node19 (Id, V);
   end Set_Parent_Subtype;

   procedure Set_Primitive_Operations (Id : E; V : L) is
   begin
      pragma Assert (Is_Tagged_Type (Id));
      Set_Elist13 (Id, V);
   end Set_Primitive_Operations;

   procedure Set_Prival (Id : E; V : E) is
   begin
      pragma Assert (Is_Protected_Private (Id));
      Set_Node17 (Id, V);
   end Set_Prival;

   procedure Set_Private_Dependents (Id : E; V : L) is
   begin
      pragma Assert (Is_Incomplete_Or_Private_Type (Id));
      Set_Elist18 (Id, V);
   end Set_Private_Dependents;

   procedure Set_Private_View (Id : E; V : N) is
   begin
      pragma Assert (Is_Private_Type (Id));
      Set_Node22 (Id, V);
   end Set_Private_View;

   procedure Set_Privals_Chain (Id : E; V : L) is
   begin
      pragma Assert (Is_Overloadable (Id)
        or else Ekind (Id) = E_Entry_Family);
      Set_Elist14 (Id, V);
   end Set_Privals_Chain;

   procedure Set_Protected_Body_Subprogram (Id : E; V : E) is
   begin
      pragma Assert (Is_Subprogram (Id) or else Is_Entry (Id));
      Set_Node11 (Id, V);
   end Set_Protected_Body_Subprogram;

   procedure Set_Protected_Formal (Id : E; V : E) is
   begin
      pragma Assert (Is_Formal (Id));
      Set_Node22 (Id, V);
   end Set_Protected_Formal;

   procedure Set_Protected_Operation (Id : E; V : N) is
   begin
      pragma Assert (Is_Protected_Private (Id));
      Set_Node14 (Id, V);
   end Set_Protected_Operation;

   procedure Set_Reachable (Id : E; V : B := True) is
   begin
      Set_Flag49 (Id, V);
   end Set_Reachable;

   procedure Set_Referenced (Id : E; V : B := True) is
   begin
      Set_Flag156 (Id, V);
   end Set_Referenced;

   procedure Set_Referenced_Object (Id : E; V : N) is
   begin
      pragma Assert (Is_Type (Id));
      Set_Node10 (Id, V);
   end Set_Referenced_Object;

   procedure Set_Register_Exception_Call (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) = E_Exception);
      Set_Node20 (Id, V);
   end Set_Register_Exception_Call;

   procedure Set_Related_Array_Object (Id : E; V : E) is
   begin
      pragma Assert (Is_Array_Type (Id));
      Set_Node19 (Id, V);
   end Set_Related_Array_Object;

   procedure Set_Related_Instance (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Package);
      Set_Node15 (Id, V);
   end Set_Related_Instance;

   procedure Set_Renamed_Entity (Id : E; V : N) is
   begin
      Set_Node18 (Id, V);
   end Set_Renamed_Entity;

   procedure Set_Renamed_Object (Id : E; V : N) is
   begin
      Set_Node18 (Id, V);
   end Set_Renamed_Object;

   procedure Set_Renaming_Map (Id : E; V : U) is
   begin
      Set_Uint9 (Id, V);
   end Set_Renaming_Map;

   procedure Set_Return_Present (Id : E; V : B := True) is
   begin
      Set_Flag54 (Id, V);
   end Set_Return_Present;

   procedure Set_Returns_By_Ref (Id : E; V : B := True) is
   begin
      Set_Flag90 (Id, V);
   end Set_Returns_By_Ref;

   procedure Set_Reverse_Bit_Order (Id : E; V : B := True) is
   begin
      pragma Assert
        (Is_Record_Type (Id) and then Id = Base_Type (Id));
      Set_Flag164 (Id, V);
   end Set_Reverse_Bit_Order;

   procedure Set_RM_Size (Id : E; V : U) is
   begin
      pragma Assert (Is_Discrete_Or_Fixed_Point_Type (Id));
      Set_Uint13 (Id, V);
   end Set_RM_Size;

   procedure Set_Scalar_Range (Id : E; V : N) is
   begin
      Set_Node20 (Id, V);
   end Set_Scalar_Range;

   procedure Set_Scale_Value (Id : E; V : U) is
   begin
      Set_Uint15 (Id, V);
   end Set_Scale_Value;

   procedure Set_Scope_Depth (Id : E; V : U) is
   begin
      Set_Uint22 (Id, V);
   end Set_Scope_Depth;

   procedure Set_Sec_Stack_Needed_For_Return (Id : E; V : B := True) is
   begin
      Set_Flag167 (Id, V);
   end Set_Sec_Stack_Needed_For_Return;

   procedure Set_Shadow_Entities (Id : E; V : S) is
   begin
      pragma Assert
        (Ekind (Id) = E_Package or else Ekind (Id) = E_Generic_Package);
      Set_List23 (Id, V);
   end Set_Shadow_Entities;

   procedure Set_Shared_Mem_Assign_Proc (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      Set_Node22 (Id, V);
   end Set_Shared_Mem_Assign_Proc;

   procedure Set_Shared_Mem_Read_Proc (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Variable);
      Set_Node15 (Id, V);
   end Set_Shared_Mem_Read_Proc;

   procedure Set_Size_Check_Code (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) = E_Constant or else Ekind (Id) = E_Variable);
      Set_Node9 (Id, V);
   end Set_Size_Check_Code;

   procedure Set_Size_Known_At_Compile_Time (Id : E; V : B := True) is
   begin
      Set_Flag92 (Id, V);
   end Set_Size_Known_At_Compile_Time;

   procedure Set_Small_Value (Id : E; V : R) is
   begin
      pragma Assert (Is_Fixed_Point_Type (Id));
      Set_Ureal21 (Id, V);
   end Set_Small_Value;

   procedure Set_Spec_Entity (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) = E_Package_Body or else Is_Formal (Id));
      Set_Node19 (Id, V);
   end Set_Spec_Entity;

   procedure Set_Storage_Size_Variable (Id : E; V : E) is
   begin
      pragma Assert (Is_Access_Type (Id) or else Is_Task_Type (Id));
      pragma Assert (Base_Type (Id) = Id);
      Set_Node15 (Id, V);
   end Set_Storage_Size_Variable;

   procedure Set_Strict_Alignment (Id : E; V : B := True) is
   begin
      pragma Assert (Base_Type (Id) = Id);
      Set_Flag145 (Id, V);
   end Set_Strict_Alignment;

   procedure Set_String_Literal_Length (Id : E; V : U) is
   begin
      pragma Assert (Ekind (Id) = E_String_Literal_Subtype);
      Set_Uint16 (Id, V);
   end Set_String_Literal_Length;

   procedure Set_String_Literal_Low_Bound (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) = E_String_Literal_Subtype);
      Set_Node15 (Id, V);
   end Set_String_Literal_Low_Bound;

   procedure Set_Suppress_Access_Checks (Id : E; V : B := True) is
   begin
      Set_Flag31 (Id, V);
   end Set_Suppress_Access_Checks;

   procedure Set_Suppress_Accessibility_Checks (Id : E; V : B := True) is
   begin
      Set_Flag32 (Id, V);
   end Set_Suppress_Accessibility_Checks;

   procedure Set_Suppress_Discriminant_Checks (Id : E; V : B := True) is
   begin
      Set_Flag33 (Id, V);
   end Set_Suppress_Discriminant_Checks;

   procedure Set_Suppress_Division_Checks (Id : E; V : B := True) is
   begin
      Set_Flag34 (Id, V);
   end Set_Suppress_Division_Checks;

   procedure Set_Suppress_Elaboration_Checks (Id : E; V : B := True) is
   begin
      Set_Flag35 (Id, V);
   end Set_Suppress_Elaboration_Checks;

   procedure Set_Suppress_Elaboration_Warnings (Id : E; V : B := True) is
   begin
      Set_Flag148 (Id, V);
   end Set_Suppress_Elaboration_Warnings;

   procedure Set_Suppress_Index_Checks (Id : E; V : B := True) is
   begin
      Set_Flag36 (Id, V);
   end Set_Suppress_Index_Checks;

   procedure Set_Suppress_Init_Proc (Id : E; V : B := True) is
   begin
      pragma Assert (Is_Type (Id) and then Id = Base_Type (Id));
      Set_Flag105 (Id, V);
   end Set_Suppress_Init_Proc;

   procedure Set_Suppress_Length_Checks (Id : E; V : B := True) is
   begin
      Set_Flag37 (Id, V);
   end Set_Suppress_Length_Checks;

   procedure Set_Suppress_Overflow_Checks (Id : E; V : B := True) is
   begin
      Set_Flag38 (Id, V);
   end Set_Suppress_Overflow_Checks;

   procedure Set_Suppress_Range_Checks (Id : E; V : B := True) is
   begin
      Set_Flag39 (Id, V);
   end Set_Suppress_Range_Checks;

   procedure Set_Suppress_Storage_Checks (Id : E; V : B := True) is
   begin
      Set_Flag40 (Id, V);
   end Set_Suppress_Storage_Checks;

   procedure Set_Suppress_Style_Checks (Id : E; V : B := True) is
   begin
      Set_Flag165 (Id, V);
   end Set_Suppress_Style_Checks;

   procedure Set_Suppress_Tag_Checks (Id : E; V : B := True) is
   begin
      Set_Flag41 (Id, V);
   end Set_Suppress_Tag_Checks;

   procedure Set_Table_High_Bound (Id : E; V : N) is
   begin
      pragma Assert (Ekind (Id) = E_Enum_Table_Type);
      Set_Node16 (Id, V);
   end Set_Table_High_Bound;

   procedure Set_Task_Body_Procedure (Id : E; V : E) is
   begin
      Set_Node19 (Id, V);
   end Set_Task_Body_Procedure;

   procedure Set_Underlying_Full_View (Id : E; V : E) is
   begin
      pragma Assert (Ekind (Id) in Private_Kind);
      Set_Node16 (Id, V);
   end Set_Underlying_Full_View;

   procedure Set_Unset_Reference (Id : E; V : N) is
   begin
      Set_Node16 (Id, V);
   end Set_Unset_Reference;

   procedure Set_Uses_Sec_Stack (Id : E; V : B := True) is
   begin
      Set_Flag95 (Id, V);
   end Set_Uses_Sec_Stack;

   procedure Set_Vax_Float (Id : E; V : B := True) is
   begin
      pragma Assert (Id = Base_Type (Id));
      Set_Flag151 (Id, V);
   end Set_Vax_Float;

   procedure Set_Warnings_Off (Id : E; V : B := True) is
   begin
      Set_Flag96 (Id, V);
   end Set_Warnings_Off;

   -----------------------------------
   -- Field Initialization Routines --
   -----------------------------------

   procedure Init_Alignment (Id : E) is
   begin
      Set_Uint23 (Id, Uint_0);
   end Init_Alignment;

   procedure Init_Alignment (Id : E; V : Int) is
   begin
      Set_Uint23 (Id, UI_From_Int (V));
   end Init_Alignment;

   procedure Init_Component_Size (Id : E) is
   begin
      Set_Uint13 (Id, Uint_0);
   end Init_Component_Size;

   procedure Init_Component_Size (Id : E; V : Int) is
   begin
      Set_Uint13 (Id, UI_From_Int (V));
   end Init_Component_Size;

   procedure Init_Digits_Value (Id : E) is
   begin
      Set_Uint17 (Id, Uint_0);
   end Init_Digits_Value;

   procedure Init_Digits_Value (Id : E; V : Int) is
   begin
      Set_Uint17 (Id, UI_From_Int (V));
   end Init_Digits_Value;

   procedure Init_Esize (Id : E) is
   begin
      Set_Uint12 (Id, Uint_0);
   end Init_Esize;

   procedure Init_Esize (Id : E; V : Int) is
   begin
      Set_Uint12 (Id, UI_From_Int (V));
   end Init_Esize;

   procedure Init_RM_Size (Id : E) is
   begin
      Set_Uint13 (Id, Uint_0);
   end Init_RM_Size;

   procedure Init_RM_Size (Id : E; V : Int) is
   begin
      Set_Uint13 (Id, UI_From_Int (V));
   end Init_RM_Size;

   --------------------
   -- Address_Clause --
   --------------------

   function Address_Clause (Id : E) return N is
      Ritem : Node_Id;

   begin
      Ritem := First_Rep_Item (Id);
      while Present (Ritem) loop
         if Nkind (Ritem) = N_Attribute_Definition_Clause
           and then Chars (Ritem) = Name_Address
         then
            return Ritem;
         else
            Ritem := Next_Rep_Item (Ritem);
         end if;
      end loop;

      return Empty;
   end Address_Clause;

   ----------------------
   -- Alignment_Clause --
   ----------------------

   function Alignment_Clause (Id : E) return N is
      Ritem : Node_Id;

   begin
      Ritem := First_Rep_Item (Id);
      while Present (Ritem) loop
         if Nkind (Ritem) = N_Attribute_Definition_Clause
           and then Chars (Ritem) = Name_Alignment
         then
            return Ritem;
         else
            Ritem := Next_Rep_Item (Ritem);
         end if;
      end loop;

      return Empty;
   end Alignment_Clause;

   ----------------------
   -- Ancestor_Subtype --
   ----------------------

   function Ancestor_Subtype       (Id : E) return E is
   begin
      --  If this is first subtype, or is a base type, then there is no
      --  ancestor subtype, so we return Empty to indicate this fact.

      if Is_First_Subtype (Id)
        or else Id = Base_Type (Id)
      then
         return Empty;
      end if;

      declare
         D : constant Node_Id := Declaration_Node (Id);

      begin
         --  If we have a subtype declaration, get the ancestor subtype

         if Nkind (D) = N_Subtype_Declaration then
            if Nkind (Subtype_Indication (D)) = N_Subtype_Indication then
               return Entity (Subtype_Mark (Subtype_Indication (D)));
            else
               return Entity (Subtype_Indication (D));
            end if;

         --  If not, then no subtype indication is available

         else
            return Empty;
         end if;
      end;
   end Ancestor_Subtype;

   -------------------
   -- Append_Entity --
   -------------------

   procedure Append_Entity (Id : Entity_Id; V : Entity_Id) is
   begin
      if Last_Entity (V) = Empty then
         Set_First_Entity (V, Id);
      else
         Set_Next_Entity (Last_Entity (V), Id);
      end if;

      Set_Next_Entity (Id, Empty);
      Set_Scope (Id, V);
      Set_Last_Entity (V, Id);
   end Append_Entity;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Id : E) return E is
   begin
      case Ekind (Id) is
         when E_Enumeration_Subtype          |
              E_Signed_Integer_Subtype       |
              E_Modular_Integer_Subtype      |
              E_Floating_Point_Subtype       |
              E_Ordinary_Fixed_Point_Subtype |
              E_Decimal_Fixed_Point_Subtype  |
              E_Array_Subtype                |
              E_String_Subtype               |
              E_Record_Subtype               |
              E_Private_Subtype              |
              E_Record_Subtype_With_Private  |
              E_Limited_Private_Subtype      |
              E_Access_Subtype               |
              E_Protected_Subtype            |
              E_Task_Subtype                 |
              E_String_Literal_Subtype       |
              E_Class_Wide_Subtype           =>
            return Etype (Id);

         when E_Incomplete_Type =>
            if Present (Etype (Id)) then
               return Etype (Id);
            else
               return Id;
            end if;

         when others =>
            return Id;
      end case;
   end Base_Type;

   -------------------------
   -- Component_Alignment --
   -------------------------

   --  Component Alignment is encoded using two flags, Flag128/129 as
   --  follows. Note that both flags False = Align_Default, so that the
   --  default initialization of flags to False initializes component
   --  alignment to the default value as required.

   --     Flag128      Flag129      Value
   --     -------      -------      -----
   --      False        False       Calign_Default
   --      False        True        Calign_Component_Size
   --      True         False       Calign_Component_Size_4
   --      True         True        Calign_Storage_Unit

   function Component_Alignment (Id : E) return C is
      BT : Node_Id := Base_Type (Id);

   begin
      pragma Assert (Is_Array_Type (Id) or else Is_Record_Type (Id));

      if Flag128 (BT) then
         if Flag129 (BT) then
            return Calign_Storage_Unit;
         else
            return Calign_Component_Size_4;
         end if;

      else
         if Flag129 (BT) then
            return Calign_Component_Size;
         else
            return Calign_Default;
         end if;
      end if;
   end Component_Alignment;

   --------------------
   -- Constant_Value --
   --------------------

   function Constant_Value (Id : E) return N is
      D      : constant Node_Id := Declaration_Node (Id);
      Full_D : Node_Id;

   begin
      --  If we have no declaration node, then return no constant value.
      --  Not clear how this can happen, but it does sometimes ???
      --  To investigate, remove this check and compile discrim_po.adb.

      if No (D) then
         return Empty;

      --  Normal case where a declaration node is present

      elsif Nkind (D) = N_Object_Renaming_Declaration then
         return Renamed_Object (Id);

      --  If this is a component declaration whose entity is constant, it
      --  is a prival within a protected function. It does not have
      --  a constant value.

      elsif Nkind (D) = N_Component_Declaration then
         return Empty;

      else
         if Present (Expression (D)) then
            return (Expression (D));

         elsif Present (Full_View (Id)) then
            Full_D := Parent (Full_View (Id));

            --  The full view may have been rewritten as an object renaming.

            if Nkind (Full_D) = N_Object_Renaming_Declaration then
               return Name (Full_D);
            else
               return Expression (Full_D);
            end if;
         else
            return Empty;
         end if;
      end if;
   end Constant_Value;

   ----------------------
   -- Declaration_Node --
   ----------------------

   function Declaration_Node (Id : E) return N is
      P : Node_Id;

   begin
      if Ekind (Id) = E_Incomplete_Type
        and then Present (Full_View (Id))
      then
         P := Parent (Full_View (Id));
      else
         P := Parent (Id);
      end if;

      loop
         if Nkind (P) /= N_Selected_Component
           and then Nkind (P) /= N_Expanded_Name
           and then
             not (Nkind (P) = N_Defining_Program_Unit_Name
                   and then Is_Child_Unit (Id))
         then
            return P;
         else
            P := Parent (P);
         end if;
      end loop;

   end Declaration_Node;

   ---------------------
   -- Designated_Type --
   ---------------------

   function Designated_Type (Id : E) return E is
      Desig_Type : E;

   begin
      Desig_Type := Directly_Designated_Type (Id);

      if (Ekind (Desig_Type) = E_Incomplete_Type
        and then Present (Full_View (Desig_Type)))
      then
         return Full_View (Desig_Type);

      elsif Is_Class_Wide_Type (Desig_Type)
        and then Ekind (Etype (Desig_Type)) = E_Incomplete_Type
        and then Present (Full_View (Etype (Desig_Type)))
        and then Present (Class_Wide_Type (Full_View (Etype (Desig_Type))))
      then
         return Class_Wide_Type (Full_View (Etype (Desig_Type)));

      else
         return Desig_Type;
      end if;
   end Designated_Type;

   -----------------------------
   -- Enclosing_Dynamic_Scope --
   -----------------------------

   function Enclosing_Dynamic_Scope (Id : E) return E is
      S  : Entity_Id;

   begin
      S := Scope (Id);
      while S /= Standard_Standard
        and then not Is_Dynamic_Scope (S)
      loop
         S := Scope (S);
      end loop;

      return S;
   end Enclosing_Dynamic_Scope;

   ----------------------
   -- Entry_Index_Type --
   ----------------------

   function Entry_Index_Type (Id : E) return N is
   begin
      pragma Assert (Ekind (Id) = E_Entry_Family);
      return Etype (Discrete_Subtype_Definition (Parent (Id)));
   end Entry_Index_Type;

   ------------------------------
   -- Esize_Known_By_Front_End --
   ------------------------------

   function Esize_Known_By_Front_End (Id : E) return Boolean is
   begin
      --  Scalar types always have a front end specified size

      if Is_Scalar_Type (Etype (Id)) then
         return True;

      --  For other types, an Esize of zero indicates unspecified

      else
         return Esize (Id) /= Uint_0;
      end if;
   end Esize_Known_By_Front_End;

   ---------------------
   -- First_Component --
   ---------------------

   function First_Component (Id : E) return E is
      Comp_Id : E;

   begin
      pragma Assert
        (Is_Record_Type (Id) or else Is_Incomplete_Or_Private_Type (Id));

      Comp_Id := First_Entity (Id);

      while Present (Comp_Id) loop
         exit when Ekind (Comp_Id) = E_Component;
         Comp_Id := Next_Entity (Comp_Id);
      end loop;

      return Comp_Id;
   end First_Component;

   ------------------------
   -- First_Discriminant --
   ------------------------

   function First_Discriminant (Id : E) return E is
      Ent : Entity_Id;

   begin
      pragma Assert
        (Has_Discriminants (Id)
          or else Has_Unknown_Discriminants (Id));

      Ent := First_Entity (Id);

      --  The discriminants are not necessarily contiguous, because access
      --  discriminants will generate itypes. They are not the first entities
      --  either, because tag and controller record must be ahead of them.

      if Chars (Ent) = Name_uTag then
         Ent := Next_Entity (Ent);
      end if;

      if Chars (Ent) = Name_uController then
         Ent := Next_Entity (Ent);
      end if;

      --  Skip all hidden girder discriminants if any.

      while Present (Ent) loop
         exit when Ekind (Ent) = E_Discriminant
           and then not Is_Completely_Hidden (Ent);


         Ent := Next_Entity (Ent);
      end loop;

      pragma Assert (Ekind (Ent) = E_Discriminant);

      return Ent;
   end First_Discriminant;

   ------------------
   -- First_Formal --
   ------------------

   function First_Formal (Id : E) return E is
      Formal : E;

   begin
      pragma Assert
        (Is_Overloadable (Id)
          or else Ekind (Id) = E_Entry_Family
          or else Ekind (Id) = E_Subprogram_Body
          or else Ekind (Id) = E_Subprogram_Type);

      if Ekind (Id) = E_Enumeration_Literal then
         return Empty;

      else
         Formal := First_Entity (Id);

         if Present (Formal) and then Is_Formal (Formal) then
            return Formal;
         else
            return Empty;
         end if;
      end if;
   end First_Formal;

   -------------------------------
   -- First_Girder_Discriminant --
   -------------------------------

   function First_Girder_Discriminant (Id : E) return E is
      Ent : Entity_Id;

      function Has_Completely_Hidden_Discriminant (Id : E) return Boolean;
      --  Scans the Discriminants to see whether any are Completely_Hidden
      --  (the mechanism for describing non-specified girder discriminants)

      function Has_Completely_Hidden_Discriminant (Id : E) return Boolean is
         Ent : Entity_Id := Id;

      begin
         pragma Assert (Ekind (Id) = E_Discriminant);

         while Present (Ent) and then Ekind (Ent) = E_Discriminant loop

            if Is_Completely_Hidden (Ent) then
               return True;
            end if;

            Ent := Next_Entity (Ent);
         end loop;

         return False;
      end Has_Completely_Hidden_Discriminant;

   --  Start of processing for First_Girder_Discriminant

   begin
      pragma Assert
        (Has_Discriminants (Id)
          or else Has_Unknown_Discriminants (Id));

      Ent := First_Entity (Id);

      if Chars (Ent) = Name_uTag then
         Ent := Next_Entity (Ent);
      end if;

      if Chars (Ent) = Name_uController then
         Ent := Next_Entity (Ent);
      end if;

      if Has_Completely_Hidden_Discriminant (Ent) then

         while Present (Ent) loop
            exit when Is_Completely_Hidden (Ent);
            Ent := Next_Entity (Ent);
         end loop;

      end if;

      pragma Assert (Ekind (Ent) = E_Discriminant);

      return Ent;
   end First_Girder_Discriminant;

   -------------------
   -- First_Subtype --
   -------------------

   function First_Subtype (Id : E) return E is
      B   : constant Entity_Id := Base_Type (Id);
      F   : constant Node_Id   := Freeze_Node (B);
      Ent : Entity_Id;

   begin
      --  If the base type has no freeze node, it is a type in standard,
      --  and always acts as its own first subtype unless it is one of
      --  the predefined integer types. If the type is formal, it is also
      --  a first subtype, and its base type has no freeze node. On the other
      --  hand, a subtype of a generic formal is not its own first_subtype.
      --  Its base type, if anonymous, is attached to the formal type decl.
      --  from which the first subtype is obtained.

      if No (F) then

         if B = Base_Type (Standard_Integer) then
            return Standard_Integer;

         elsif B = Base_Type (Standard_Long_Integer) then
            return Standard_Long_Integer;

         elsif B = Base_Type (Standard_Short_Short_Integer) then
            return Standard_Short_Short_Integer;

         elsif B = Base_Type (Standard_Short_Integer) then
            return Standard_Short_Integer;

         elsif B = Base_Type (Standard_Long_Long_Integer) then
            return Standard_Long_Long_Integer;

         elsif Is_Generic_Type (Id) then
            if Present (Parent (B)) then
               return Defining_Identifier (Parent (B));
            else
               return Defining_Identifier (Associated_Node_For_Itype (B));
            end if;

         else
            return B;
         end if;

      --  Otherwise we check the freeze node, if it has a First_Subtype_Link
      --  then we use that link, otherwise (happens with some Itypes), we use
      --  the base type itself.

      else
         Ent := First_Subtype_Link (F);

         if Present (Ent) then
            return Ent;
         else
            return B;
         end if;
      end if;
   end First_Subtype;

   -----------------
   -- Get_RM_Size --
   -----------------

   function Get_RM_Size (Id : E) return Uint is
   begin
      if Is_Discrete_Or_Fixed_Point_Type (Id) then
         return RM_Size (Id);
      else
         return Esize (Id);
      end if;
   end Get_RM_Size;

   ------------------------
   -- Has_Attach_Handler --
   ------------------------

   function Has_Attach_Handler (Id : E) return B is
      Ritem : Node_Id;

   begin
      pragma Assert (Is_Protected_Type (Id));

      Ritem := First_Rep_Item (Id);
      while Present (Ritem) loop
         if Nkind (Ritem) = N_Pragma
           and then Chars (Ritem) = Name_Attach_Handler
         then
            return True;
         else
            Ritem := Next_Rep_Item (Ritem);
         end if;
      end loop;

      return False;
   end Has_Attach_Handler;

   -----------------
   -- Has_Entries --
   -----------------

   function Has_Entries (Id : E) return B is
      Result : Boolean := False;
      Ent    : Entity_Id;

   begin
      pragma Assert (Is_Concurrent_Type (Id));
      Ent := First_Entity (Id);

      while Present (Ent) loop
         if Is_Entry (Ent) then
            Result := True;
            exit;
         end if;

         Ent := Next_Entity (Ent);
      end loop;

      return Result;
   end Has_Entries;

   ----------------------------
   -- Has_Foreign_Convention --
   ----------------------------

   function Has_Foreign_Convention (Id : E) return B is
   begin
      return Convention (Id) >= Foreign_Convention'First;
   end Has_Foreign_Convention;

   ---------------------------
   -- Has_Interrupt_Handler --
   ---------------------------

   function Has_Interrupt_Handler (Id : E) return B is
      Ritem : Node_Id;

   begin
      pragma Assert (Is_Protected_Type (Id));

      Ritem := First_Rep_Item (Id);
      while Present (Ritem) loop
         if Nkind (Ritem) = N_Pragma
           and then Chars (Ritem) = Name_Interrupt_Handler
         then
            return True;
         else
            Ritem := Next_Rep_Item (Ritem);
         end if;
      end loop;

      return False;
   end Has_Interrupt_Handler;

   --------------------------
   -- Has_Private_Ancestor --
   --------------------------

   function Has_Private_Ancestor (Id : E) return B is
      R  : constant Entity_Id := Root_Type (Id);
      T1 : Entity_Id := Id;

   begin
      loop
         if Is_Private_Type (T1) then
            return True;

         elsif T1 = R then
            return False;

         else
            T1 := Etype (T1);
         end if;
      end loop;
   end Has_Private_Ancestor;

   ------------------------------
   -- Implementation_Base_Type --
   ------------------------------

   function Implementation_Base_Type (Id : E) return E is
      Bastyp : Entity_Id;
      Imptyp : Entity_Id;

   begin
      Bastyp := Base_Type (Id);

      if Is_Incomplete_Or_Private_Type (Bastyp) then
         Imptyp := Underlying_Type (Bastyp);

         --  If we have an implementation type, then just return it,
         --  otherwise we return the Base_Type anyway. This can only
         --  happen in error situations and should avoid some error bombs.

         if Present (Imptyp) then
            return Imptyp;
         else
            return Bastyp;
         end if;

      else
         return Bastyp;
      end if;
   end Implementation_Base_Type;

   ---------------------
   -- Is_Boolean_Type --
   ---------------------

   function Is_Boolean_Type (Id : E) return B is
   begin
      return Root_Type (Id) = Standard_Boolean;
   end Is_Boolean_Type;

   ---------------------
   -- Is_By_Copy_Type --
   ---------------------

   function Is_By_Copy_Type (Id : E) return B is
   begin
      --  If Id is a private type whose full declaration has not been seen,
      --  we assume for now that it is not a By_Copy type. Clearly this
      --  attribute should not be used before the type is frozen, but it is
      --  needed to build the associated record of a protected type. Another
      --  place where some lookahead for a full view is needed ???

      return
        Is_Elementary_Type (Id)
          or else (Is_Private_Type (Id)
                     and then Present (Underlying_Type (Id))
                     and then Is_Elementary_Type (Underlying_Type (Id)));
   end Is_By_Copy_Type;

   ---------------------
   -- Is_Derived_Type --
   ---------------------

   function Is_Derived_Type (Id : E) return B is
      Par : Node_Id;

   begin
      if Base_Type (Id) /= Root_Type (Id)
        and then not Is_Generic_Type (Id)
        and then not Is_Class_Wide_Type (Id)
      then
         if not Is_Numeric_Type (Root_Type (Id)) then
            return True;

         else
            Par := Parent (First_Subtype (Id));

            return Present (Par)
              and then Nkind (Par) = N_Full_Type_Declaration
              and then Nkind (Type_Definition (Par))
                = N_Derived_Type_Definition;
         end if;

      else
         return False;
      end if;
   end Is_Derived_Type;

   ----------------------
   -- Is_Dynamic_Scope --
   ----------------------

   function Is_Dynamic_Scope (Id : E) return B is
   begin
      return
        Ekind (Id) = E_Block
          or else
        Ekind (Id) = E_Function
          or else
        Ekind (Id) = E_Procedure
          or else
        Ekind (Id) = E_Subprogram_Body
          or else
        Ekind (Id) = E_Task_Type
          or else
        Ekind (Id) = E_Entry
          or else
        Ekind (Id) = E_Entry_Family;
   end Is_Dynamic_Scope;

   ---------------------------
   -- Is_Indefinite_Subtype --
   ---------------------------

   function Is_Indefinite_Subtype (Id : Entity_Id) return B is
      K : constant Entity_Kind := Ekind (Id);

   begin
      if Is_Constrained (Id) then
         return False;

      elsif K in Array_Kind
        or else K in Class_Wide_Kind
        or else Has_Unknown_Discriminants (Id)
      then
         return True;

      --  Known discriminants: indefinite if there are no default values

      elsif K in Record_Kind
        or else Is_Incomplete_Or_Private_Type (Id)
        or else Is_Concurrent_Type (Id)
      then
         return (Has_Discriminants (Id)
           and then No (Discriminant_Default_Value (First_Discriminant (Id))));

      else
         return False;
      end if;
   end Is_Indefinite_Subtype;

   ---------------------
   -- Is_Limited_Type --
   ---------------------

   function Is_Limited_Type (Id : E) return B is
      Btype : constant E := Base_Type (Id);

   begin
      if not Is_Type (Id) then
         return False;

      elsif Ekind (Btype) = E_Limited_Private_Type
        or else Is_Limited_Composite (Btype)
      then
         return True;

      elsif Is_Concurrent_Type (Btype) then
         return True;

      --  Otherwise we will look around to see if there is some other reason
      --  for it to be limited, except that if an error was posted on the
      --  entity, then just assume it is non-limited, because it can cause
      --  trouble to recurse into a murky erroneous entity!

      elsif Error_Posted (Id) then
         return False;

      elsif Is_Record_Type (Btype) then
         if Is_Limited_Record (Root_Type (Btype)) then
            return True;

         elsif Is_Class_Wide_Type (Btype) then
            return Is_Limited_Type (Root_Type (Btype));

         else
            declare
               C : E := First_Component (Btype);

            begin
               while Present (C) loop
                  if Is_Limited_Type (Etype (C)) then
                     return True;
                  end if;

                  C := Next_Component (C);
               end loop;
            end;

            return False;
         end if;

      elsif Is_Array_Type (Btype) then
         return Is_Limited_Type (Component_Type (Btype));

      else
         return False;
      end if;
   end Is_Limited_Type;

   ----------------
   -- Is_Package --
   ----------------

   function Is_Package (Id : E) return B is
   begin
      return
        Ekind (Id) = E_Package
          or else
        Ekind (Id) = E_Generic_Package;
   end Is_Package;

   --------------------------
   -- Is_Protected_Private --
   --------------------------

   function Is_Protected_Private (Id : E) return B is

   begin
      pragma Assert (Ekind (Id) = E_Component);
      return Is_Protected_Type (Scope (Id));
   end Is_Protected_Private;

   ------------------------------
   -- Is_Protected_Record_Type --
   ------------------------------

   function Is_Protected_Record_Type (Id : E) return B is
   begin
      return
        Is_Concurrent_Record_Type (Id)
          and then Is_Protected_Type (Corresponding_Concurrent_Type (Id));
   end Is_Protected_Record_Type;

   --------------------------
   -- Is_By_Reference_Type --
   --------------------------

   function Is_By_Reference_Type (Id : E) return B is
      Btype : constant Entity_Id := Base_Type (Id);

   begin
      if Error_Posted (Id)
        or else Error_Posted (Btype)
      then
         return False;

      elsif Is_Private_Type (Btype) then
         declare
            Utyp : constant Entity_Id := Underlying_Type (Btype);

         begin
            if No (Utyp) then
               return False;
            else
               return Is_By_Reference_Type (Utyp);
            end if;
         end;

      elsif Is_Concurrent_Type (Btype) then
         return True;

      elsif Is_Record_Type (Btype) then

         if Is_Limited_Record (Btype)
           or else Is_Tagged_Type (Btype)
           or else Is_Volatile (Btype)
         then
            return True;

         else
            declare
               C : Entity_Id := First_Component (Btype);

            begin
               while Present (C) loop
                  if Is_By_Reference_Type (Etype (C))
                    or else Is_Volatile (Etype (C))
                  then
                     return True;
                  end if;

                  C := Next_Component (C);
               end loop;
            end;

            return False;
         end if;

      elsif Is_Array_Type (Btype) then
         return
           Is_Volatile (Btype)
             or else Is_By_Reference_Type (Component_Type (Btype))
             or else Is_Volatile (Component_Type (Btype))
             or else Has_Volatile_Components (Btype);

      else
         return False;
      end if;
   end Is_By_Reference_Type;

   ---------------------------------
   -- Is_Return_By_Reference_Type --
   ---------------------------------

   function Is_Return_By_Reference_Type (Id : E) return B is
      Btype : constant Entity_Id := Base_Type (Id);

   begin
      if Is_Private_Type (Btype) then
         declare
            Utyp : constant Entity_Id := Underlying_Type (Btype);

         begin
            if No (Utyp) then
               return False;
            else
               return Is_Return_By_Reference_Type (Utyp);
            end if;
         end;

      elsif Is_Concurrent_Type (Btype) then
         return True;

      elsif Is_Record_Type (Btype) then
         if Is_Limited_Record (Btype) then
            return True;

         elsif Is_Class_Wide_Type (Btype) then
            return Is_Return_By_Reference_Type (Root_Type (Btype));

         else
            declare
               C : Entity_Id := First_Component (Btype);

            begin
               while Present (C) loop
                  if Is_Return_By_Reference_Type (Etype (C)) then
                     return True;
                  end if;

                  C := Next_Component (C);
               end loop;
            end;

            return False;
         end if;

      elsif Is_Array_Type (Btype) then
         return Is_Return_By_Reference_Type (Component_Type (Btype));

      else
         return False;
      end if;
   end Is_Return_By_Reference_Type;

   -------------------------
   -- Is_Task_Record_Type --
   -------------------------

   function Is_Task_Record_Type (Id : E) return B is
   begin
      return
        Is_Concurrent_Record_Type (Id)
          and then Is_Task_Type (Corresponding_Concurrent_Type (Id));
   end Is_Task_Record_Type;

   --------------------
   -- Is_String_Type --
   --------------------

   function Is_String_Type (Id : E) return B is
   begin
      return Ekind (Id) in String_Kind
        or else (Is_Array_Type (Id)
                  and then Number_Dimensions (Id) = 1
                  and then Is_Character_Type (Component_Type (Id)));
   end Is_String_Type;

   --------------------
   -- Next_Component --
   --------------------

   function Next_Component (Id : E) return E is
      Comp_Id : E;

   begin
      Comp_Id := Next_Entity (Id);

      while Present (Comp_Id) loop
         exit when Ekind (Comp_Id) = E_Component;
         Comp_Id := Next_Entity (Comp_Id);
      end loop;

      return Comp_Id;
   end Next_Component;

   -----------------------
   -- Next_Discriminant --
   -----------------------

   --  This function actually implements both Next_Discriminant and
   --  Next_Girder_Discriminant by making sure that the Discriminant
   --  returned is of the same variety as Id.

   function Next_Discriminant (Id : E) return E is

      --  Derived Tagged types with private extensions look like this...
      --
      --       E_Discriminant d1
      --       E_Discriminant d2
      --       E_Component    _tag
      --       E_Discriminant d1
      --       E_Discriminant d2
      --       ...
      --  so it is critical not to go past the leading discriminants.

      D : E := Id;

   begin
      pragma Assert (Ekind (Id) = E_Discriminant);

      loop
         D := Next_Entity (D);
         if not Present (D)
           or else (Ekind (D) /= E_Discriminant
                      and then not Is_Itype (D))
         then
            return Empty;
         end if;

         exit when Ekind (D) = E_Discriminant
           and then (Is_Completely_Hidden (D) = Is_Completely_Hidden (Id));
      end loop;

      return D;
   end Next_Discriminant;

   -----------------
   -- Next_Formal --
   -----------------

   function Next_Formal (Id : E) return E is
      P : E;

   begin
      --  Follow the chain of declared entities as long as the kind of
      --  the entity corresponds to a formal parameter. Skip internal
      --  entities that may have been created for implicit subtypes,
      --  in the process of analyzing default expressions.

      P := Id;

      loop
         P := Next_Entity (P);

         if No (P) or else Is_Formal (P) then
            return P;
         elsif not Is_Internal (P) then
            return Empty;
         end if;
      end loop;
   end Next_Formal;

   -----------------------------
   -- Next_Formal_With_Extras --
   -----------------------------

   function Next_Formal_With_Extras (Id : E) return E is
   begin
      if Present (Extra_Formal (Id)) then
         return Extra_Formal (Id);

      else
         return Next_Formal (Id);
      end if;
   end Next_Formal_With_Extras;

   ------------------------------
   -- Next_Girder_Discriminant --
   ------------------------------

   function Next_Girder_Discriminant (Id : E) return E is
   begin
      --  See comment in Next_Discriminant

      return Next_Discriminant (Id);
   end Next_Girder_Discriminant;

   ----------------
   -- Next_Index --
   ----------------

   function Next_Index (Id : Node_Id) return Node_Id is
   begin
      return Next (Id);
   end Next_Index;

   ------------------
   -- Next_Literal --
   ------------------

   function Next_Literal (Id : E) return E is
   begin
      pragma Assert (Nkind (Id) in N_Entity);
      return Next (Id);
   end Next_Literal;

   -----------------------
   -- Number_Dimensions --
   -----------------------

   function Number_Dimensions (Id : E) return Pos is
      N : Int;
      T : Node_Id;

   begin
      if Ekind (Id) in String_Kind then
         return 1;

      else
         N := 0;
         T := First_Index (Id);

         while Present (T) loop
            N := N + 1;
            T := Next (T);
         end loop;

         return N;
      end if;
   end Number_Dimensions;

   --------------------------
   -- Number_Discriminants --
   --------------------------

   function Number_Discriminants (Id : E) return Pos is
      N     : Int;
      Discr : Entity_Id;

   begin
      N := 0;
      Discr := First_Discriminant (Id);

      while Present (Discr) loop
         N := N + 1;
         Discr := Next_Discriminant (Discr);
      end loop;

      return N;
   end Number_Discriminants;

   --------------------
   -- Number_Entries --
   --------------------

   function Number_Entries (Id : E) return Nat is
      N      : Int;
      Ent    : Entity_Id;

   begin
      pragma Assert (Is_Concurrent_Type (Id));
      N := 0;
      Ent := First_Entity (Id);

      while Present (Ent) loop
         if Is_Entry (Ent) then
            N := N + 1;
         end if;

         Ent := Next_Entity (Ent);
      end loop;

      return N;
   end Number_Entries;

   --------------------
   -- Number_Formals --
   --------------------

   function Number_Formals (Id : E) return Pos is
      N      : Int;
      Formal : Entity_Id;

   begin
      N := 0;
      Formal := First_Formal (Id);

      while Present (Formal) loop
         N := N + 1;
         Formal := Next_Formal (Formal);
      end loop;

      return N;
   end Number_Formals;

   --------------------
   -- Parameter_Mode --
   --------------------

   function Parameter_Mode (Id : E) return Formal_Kind is
   begin
      return Ekind (Id);
   end Parameter_Mode;

   ------------------------------
   -- Requires_Transient_Scope --
   ------------------------------

   --  A transient scope is required when variable-sized temporaries are
   --  allocated in the primary or secondary stack, or when finalization
   --  actions must be generated before the next instruction

   function Requires_Transient_Scope (Id : E) return B is
      Typ : constant Entity_Id := Underlying_Type (Id);

   begin
      --  This is a private type which is not completed yet. This can only
      --  happen in a default expression (of a formal parameter or of a
      --  record component). Do not expand transient scope in this case

      if No (Typ) then
         return False;

      elsif Typ = Standard_Void_Type then
         return False;

      --  The back-end has trouble allocating variable-size temporaries so
      --  we generate them in the front-end and need a transient scope to
      --  reclaim them properly

      elsif not Size_Known_At_Compile_Time (Typ) then
         return True;

      --  Unconstrained discriminated records always require a variable
      --  length temporary, since the length may depend on the variant.

      elsif Is_Record_Type (Typ)
        and then Has_Discriminants (Typ)
        and then not Is_Constrained (Typ)
      then
         return True;

      --  Functions returning tagged types may dispatch on result so their
      --  returned value is allocated on the secondary stack. Controlled
      --  type temporaries need finalization.

      elsif Is_Tagged_Type (Typ)
        or else Has_Controlled_Component (Typ)
      then
         return True;

      --  Unconstrained array types are returned on the secondary stack

      elsif Is_Array_Type (Typ) then
         return not Is_Constrained (Typ);
      end if;

      return False;
   end Requires_Transient_Scope;

   ---------------
   -- Root_Type --
   ---------------

   function Root_Type (Id : E) return E is
      T, Etyp : E;

   begin
      pragma Assert (Nkind (Id) in N_Entity);

      T := Base_Type (Id);

      if Ekind (T) = E_Class_Wide_Type then
         return Etype (T);

      else
         loop
            Etyp := Etype (T);

            if T = Etyp then
               return T;

            elsif Is_Private_Type (T) and then Etyp = Full_View (T) then
               return T;

            elsif Is_Private_Type (Etyp) and then Full_View (Etyp) = T then
               return T;
            end if;

            T := Etyp;
         end loop;
      end if;

      raise Program_Error;
   end Root_Type;

   ---------------------
   -- Scope_Depth_Set --
   ---------------------

   function Scope_Depth_Set (Id : E) return B is
   begin
      return Field22 (Id) /= Union_Id (Empty);
   end Scope_Depth_Set;

   -----------------------------
   -- Set_Component_Alignment --
   -----------------------------

   --  Component Alignment is encoded using two flags, Flag128/129 as
   --  follows. Note that both flags False = Align_Default, so that the
   --  default initialization of flags to False initializes component
   --  alignment to the default value as required.

   --     Flag128      Flag129      Value
   --     -------      -------      -----
   --      False        False       Calign_Default
   --      False        True        Calign_Component_Size
   --      True         False       Calign_Component_Size_4
   --      True         True        Calign_Storage_Unit

   procedure Set_Component_Alignment (Id : E; V : C) is
   begin
      pragma Assert ((Is_Array_Type (Id) or else Is_Record_Type (Id))
                       and then Id = Base_Type (Id));

      case V is
         when Calign_Default          =>
            Set_Flag128 (Id, False);
            Set_Flag129 (Id, False);

         when Calign_Component_Size   =>
            Set_Flag128 (Id, False);
            Set_Flag129 (Id, True);

         when Calign_Component_Size_4 =>
            Set_Flag128 (Id, True);
            Set_Flag129 (Id, False);

         when Calign_Storage_Unit     =>
            Set_Flag128 (Id, True);
            Set_Flag129 (Id, True);
      end case;
   end Set_Component_Alignment;

   -----------------
   -- Size_Clause --
   -----------------

   function Size_Clause (Id : E) return N is
      Ritem : Node_Id;

   begin
      Ritem := First_Rep_Item (Id);
      while Present (Ritem) loop
         if Nkind (Ritem) = N_Attribute_Definition_Clause
           and then Chars (Ritem) = Name_Size
         then
            return Ritem;
         else
            Ritem := Next_Rep_Item (Ritem);
         end if;
      end loop;

      return Empty;
   end Size_Clause;

   ------------------
   -- Subtype_Kind --
   ------------------

   function Subtype_Kind (K : Entity_Kind) return Entity_Kind is
      Kind : Entity_Kind;

   begin
      case K is
         when Access_Kind                 => Kind := E_Access_Subtype;

         when E_Array_Type                |
              E_Array_Subtype             => Kind := E_Array_Subtype;

         when E_Class_Wide_Type           |
              E_Class_Wide_Subtype        => Kind := E_Class_Wide_Subtype;

         when E_Decimal_Fixed_Point_Type  |
              E_Decimal_Fixed_Point_Subtype
                                          => Kind :=
                                               E_Decimal_Fixed_Point_Subtype;

         when E_Ordinary_Fixed_Point_Type |
              E_Ordinary_Fixed_Point_Subtype
                                          => Kind :=
                                               E_Ordinary_Fixed_Point_Subtype;

         when E_Private_Type              |
              E_Private_Subtype           => Kind := E_Private_Subtype;

         when E_Limited_Private_Type      |
              E_Limited_Private_Subtype   => Kind := E_Limited_Private_Subtype;

         when E_Record_Type_With_Private  |
              E_Record_Subtype_With_Private
                                      => Kind := E_Record_Subtype_With_Private;

         when E_Record_Type               |
              E_Record_Subtype            => Kind := E_Record_Subtype;

         when E_String_Type               |
              E_String_Subtype            => Kind := E_String_Subtype;

         when Enumeration_Kind            => Kind := E_Enumeration_Subtype;
         when Float_Kind                  => Kind := E_Floating_Point_Subtype;
         when Signed_Integer_Kind         => Kind := E_Signed_Integer_Subtype;
         when Modular_Integer_Kind        => Kind := E_Modular_Integer_Subtype;
         when Protected_Kind              => Kind := E_Protected_Subtype;
         when Task_Kind                   => Kind := E_Task_Subtype;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;

      return Kind;
   end Subtype_Kind;

   -------------------
   -- Tag_Component --
   -------------------

   function Tag_Component (Id : E) return E is
      Comp : Entity_Id;
      Typ  : Entity_Id := Id;

   begin
      pragma Assert (Is_Tagged_Type (Typ));

      if Is_Class_Wide_Type (Typ) then
         Typ := Root_Type (Typ);
      end if;

      if Is_Private_Type (Typ) then
         Typ := Underlying_Type (Typ);
      end if;

      Comp := First_Entity (Typ);
      while Present (Comp) loop
         if Is_Tag (Comp) then
            return Comp;
         end if;

         Comp := Next_Entity (Comp);
      end loop;

      --  no tag component found

      return Empty;
   end Tag_Component;

   ---------------------
   -- Type_High_Bound --
   ---------------------

   function Type_High_Bound (Id : E) return Node_Id is
   begin
      if Nkind (Scalar_Range (Id)) = N_Subtype_Indication then
         return High_Bound (Range_Expression (Constraint (Scalar_Range (Id))));
      else
         return High_Bound (Scalar_Range (Id));
      end if;
   end Type_High_Bound;

   --------------------
   -- Type_Low_Bound --
   --------------------

   function Type_Low_Bound (Id : E) return Node_Id is
   begin
      if Nkind (Scalar_Range (Id)) = N_Subtype_Indication then
         return Low_Bound (Range_Expression (Constraint (Scalar_Range (Id))));
      else
         return Low_Bound (Scalar_Range (Id));
      end if;
   end Type_Low_Bound;

   ---------------------
   -- Underlying_Type --
   ---------------------

   function Underlying_Type (Id : E) return E is
   begin

      --  For record_with_private the underlying type is always the direct
      --  full view. Never try to take the full view of the parent it
      --  doesn't make sense.

      if Ekind (Id) = E_Record_Type_With_Private then
         return Full_View (Id);

      elsif Ekind (Id) in Incomplete_Or_Private_Kind then

         --  If we have an incomplete or private type with a full view,
         --  then we return the Underlying_Type of this full view

         if Present (Full_View (Id)) then
            return Underlying_Type (Full_View (Id));

         --  Otherwise check for the case where we have a derived type or
         --  subtype, and if so get the Underlying_Type of the parent type.

         elsif Etype (Id) /= Id then
            return Underlying_Type (Etype (Id));

         --  Otherwise we have an incomplete or private type that has
         --  no full view, which means that we have not encountered the
         --  completion, so return Empty to indicate the underlying type
         --  is not yet known.

         else
            return Empty;
         end if;

      --  For non-incomplete, non-private types, return the type itself
      --  Also for entities that are not types at all return the entity
      --  itself.

      else
         return Id;
      end if;
   end Underlying_Type;

   ------------------------
   -- Write_Entity_Flags --
   ------------------------

   procedure Write_Entity_Flags (Id : Entity_Id; Prefix : String) is

      procedure W (Flag_Name : String; Flag : Boolean);
      --  Write out given flag if it is set

      procedure W (Flag_Name : String; Flag : Boolean) is
      begin
         if Flag then
            Write_Str (Prefix);
            Write_Str (Flag_Name);
            Write_Str (" = True");
            Write_Eol;
         end if;
      end W;

   --  Start of processing for Write_Entity_Flags

   begin
      if (Is_Array_Type (Id) or else Is_Record_Type (Id))
        and then Base_Type (Id) = Id
      then
         Write_Str (Prefix);
         Write_Str ("Component_Alignment = ");

         case Component_Alignment (Id) is
            when Calign_Default =>
               Write_Str ("Calign_Default");

            when Calign_Component_Size =>
               Write_Str ("Calign_Component_Size");

            when Calign_Component_Size_4 =>
               Write_Str ("Calign_Component_Size_4");

            when Calign_Storage_Unit =>
               Write_Str ("Calign_Storage_Unit");
         end case;

         Write_Eol;
      end if;

      W ("Address_Taken",                 Flag104 (Id));
      W ("C_Pass_By_Copy",                Flag125 (Id));
      W ("Debug_Info_Off",                Flag166 (Id));
      W ("Default_Expressions_Processed", Flag108 (Id));
      W ("Delay_Cleanups",                Flag114 (Id));
      W ("Delay_Subprogram_Descriptors",  Flag50  (Id));
      W ("Depends_On_Private",            Flag14  (Id));
      W ("Discard_Names",                 Flag88  (Id));
      W ("Elaborate_All_Desirable",       Flag146 (Id));
      W ("Entry_Accepted",                Flag152 (Id));
      W ("Finalize_Storage_Only",         Flag158 (Id));
      W ("Function_Returns_With_DSP",     Flag169 (Id));
      W ("Has_Aliased_Components",        Flag135 (Id));
      W ("Has_Alignment_Clause",          Flag46  (Id));
      W ("Has_All_Calls_Remote",          Flag79  (Id));
      W ("Has_Atomic_Components",         Flag86  (Id));
      W ("Has_Biased_Representation",     Flag139 (Id));
      W ("Has_Completion",                Flag26  (Id));
      W ("Has_Completion_In_Body",        Flag71  (Id));
      W ("Has_Complex_Representation",    Flag140 (Id));
      W ("Has_Component_Size_Clause",     Flag68  (Id));
      W ("Has_Controlled_Component",      Flag43  (Id));
      W ("Has_Controlling_Result",        Flag98  (Id));
      W ("Has_Convention_Pragma",         Flag119 (Id));
      W ("Has_Delayed_Freeze",            Flag18  (Id));
      W ("Has_Discriminants",             Flag5   (Id));
      W ("Has_Enumeration_Rep_Clause",    Flag66  (Id));
      W ("Has_Exit",                      Flag47  (Id));
      W ("Has_External_Tag_Rep_Clause",   Flag110 (Id));
      W ("Has_Gigi_Rep_Item",             Flag82  (Id));
      W ("Has_Homonym",                   Flag56  (Id));
      W ("From_With_Type",                Flag159 (Id));
      W ("Has_Machine_Radix_Clause",      Flag83  (Id));
      W ("Has_Master_Entity",             Flag21  (Id));
      W ("Has_Missing_Return",            Flag142 (Id));
      W ("Has_Nested_Block_With_Handler", Flag101 (Id));
      W ("Has_Non_Standard_Rep",          Flag75  (Id));
      W ("Has_Per_Object_Constraint",     Flag154 (Id));
      W ("Has_Pragma_Controlled",         Flag27  (Id));
      W ("Has_Pragma_Elaborate_Body",     Flag150 (Id));
      W ("Has_Pragma_Inline",             Flag157 (Id));
      W ("Has_Pragma_Pack",               Flag121 (Id));
      W ("Has_Primitive_Operations",      Flag120 (Id));
      W ("Has_Private_Declaration",       Flag155 (Id));
      W ("Has_Qualified_Name",            Flag161 (Id));
      W ("Has_Record_Rep_Clause",         Flag65  (Id));
      W ("Has_Recursive_Call",            Flag143 (Id));
      W ("Has_Size_Clause",               Flag29  (Id));
      W ("Has_Small_Clause",              Flag67  (Id));
      W ("Has_Specified_Layout",          Flag100 (Id));
      W ("Has_Storage_Size_Clause",       Flag23  (Id));
      W ("Has_Subprogram_Descriptor",     Flag93  (Id));
      W ("Has_Task",                      Flag30  (Id));
      W ("Has_Unchecked_Union",           Flag123 (Id));
      W ("Has_Unknown_Discriminants",     Flag72  (Id));
      W ("Has_Volatile_Components",       Flag87  (Id));
      W ("In_Package_Body",               Flag48  (Id));
      W ("In_Private_Part",               Flag45  (Id));
      W ("In_Use",                        Flag8   (Id));
      W ("Is_AST_Entry",                  Flag132 (Id));
      W ("Is_Abstract",                   Flag19  (Id));
      W ("Is_Access_Constant",            Flag69  (Id));
      W ("Is_Aliased",                    Flag15  (Id));
      W ("Is_Asynchronous",               Flag81  (Id));
      W ("Is_Atomic",                     Flag85  (Id));
      W ("Is_Bit_Packed_Array",           Flag122 (Id));
      W ("Is_CPP_Class",                  Flag74  (Id));
      W ("Is_Called",                     Flag102 (Id));
      W ("Is_Character_Type",             Flag63  (Id));
      W ("Is_Child_Unit",                 Flag73  (Id));
      W ("Is_Compilation_Unit",           Flag149 (Id));
      W ("Is_Completely_Hidden",          Flag103 (Id));
      W ("Is_Concurrent_Record_Type",     Flag20  (Id));
      W ("Is_Constr_Subt_For_UN_Aliased", Flag141 (Id));
      W ("Is_Constr_Subt_For_U_Nominal",  Flag80  (Id));
      W ("Is_Constrained",                Flag12  (Id));
      W ("Is_Constructor",                Flag76  (Id));
      W ("Is_Controlled",                 Flag42  (Id));
      W ("Is_Controlling_Formal",         Flag97  (Id));
      W ("Is_Destructor",                 Flag77  (Id));
      W ("Is_Dispatching_Operation",      Flag6   (Id));
      W ("Is_Eliminated",                 Flag124 (Id));
      W ("Is_Entry_Formal",               Flag52  (Id));
      W ("Is_Exported",                   Flag99  (Id));
      W ("Is_First_Subtype",              Flag70  (Id));
      W ("Is_For_Access_Subtype",         Flag118 (Id));
      W ("Is_Formal_Subprogram",          Flag111 (Id));
      W ("Is_Frozen",                     Flag4   (Id));
      W ("Is_Generic_Actual_Type",        Flag94  (Id));
      W ("Is_Generic_Instance",           Flag130 (Id));
      W ("Is_Generic_Type",               Flag13  (Id));
      W ("Is_Hidden",                     Flag57  (Id));
      W ("Is_Hidden_Open_Scope",          Flag171 (Id));
      W ("Is_Immediately_Visible",        Flag7   (Id));
      W ("Is_Imported",                   Flag24  (Id));
      W ("Is_Inlined",                    Flag11  (Id));
      W ("Is_Instantiated",               Flag126 (Id));
      W ("Is_Internal",                   Flag17  (Id));
      W ("Is_Interrupt_Handler",          Flag89  (Id));
      W ("Is_Intrinsic_Subprogram",       Flag64  (Id));
      W ("Is_Itype",                      Flag91  (Id));
      W ("Is_Known_Valid",                Flag170 (Id));
      W ("Is_Limited_Composite",          Flag106 (Id));
      W ("Is_Limited_Record",             Flag25  (Id));
      W ("Is_Non_Static_Subtype",         Flag109 (Id));
      W ("Is_Optional_Parameter",         Flag134 (Id));
      W ("Is_Package_Body_Entity",        Flag160 (Id));
      W ("Is_Packed",                     Flag51  (Id));
      W ("Is_Packed_Array_Type",          Flag138 (Id));
      W ("Is_Potentially_Use_Visible",    Flag9   (Id));
      W ("Is_Preelaborated",              Flag59  (Id));
      W ("Is_Private_Composite",          Flag106 (Id));
      W ("Is_Private_Descendant",         Flag53  (Id));
      W ("Is_Psected",                    Flag153 (Id));
      W ("Is_Public",                     Flag10  (Id));
      W ("Is_Pure",                       Flag44  (Id));
      W ("Is_Remote_Call_Interface",      Flag62  (Id));
      W ("Is_Remote_Types",               Flag61  (Id));
      W ("Is_Renaming_Of_Object",         Flag112 (Id));
      W ("Is_Shared_Passive",             Flag60  (Id));
      W ("Is_Statically_Allocated",       Flag28  (Id));
      W ("Is_Tag",                        Flag78  (Id));
      W ("Is_Tagged_Type",                Flag55  (Id));
      W ("Is_True_Constant",              Flag163 (Id));
      W ("Is_Unchecked_Union",            Flag117 (Id));
      W ("Is_Unsigned_Type",              Flag144 (Id));
      W ("Is_VMS_Exception",              Flag133 (Id));
      W ("Is_Valued_Procedure",           Flag127 (Id));
      W ("Is_Visible_Child_Unit",         Flag116 (Id));
      W ("Is_Volatile",                   Flag16  (Id));
      W ("Machine_Radix_10",              Flag84  (Id));
      W ("Materialize_Entity",            Flag168 (Id));
      W ("Needs_Debug_Info",              Flag147 (Id));
      W ("Needs_No_Actuals",              Flag22  (Id));
      W ("No_Pool_Assigned",              Flag131 (Id));
      W ("No_Return",                     Flag113 (Id));
      W ("Non_Binary_Modulus",            Flag58  (Id));
      W ("Nonzero_Is_True",               Flag162 (Id));
      W ("Not_Source_Assigned",           Flag115 (Id));
      W ("Reachable",                     Flag49  (Id));
      W ("Referenced",                    Flag156 (Id));
      W ("Return_Present",                Flag54  (Id));
      W ("Returns_By_Ref",                Flag90  (Id));
      W ("Reverse_Bit_Order",             Flag164 (Id));
      W ("Sec_Stack_Needed_For_Return",   Flag167 (Id));
      W ("Size_Known_At_Compile_Time",    Flag92  (Id));
      W ("Strict_Alignment",              Flag145 (Id));
      W ("Suppress_Access_Checks",        Flag31  (Id));
      W ("Suppress_Accessibility_Checks", Flag32  (Id));
      W ("Suppress_Discriminant_Checks",  Flag33  (Id));
      W ("Suppress_Division_Checks",      Flag34  (Id));
      W ("Suppress_Elaboration_Checks",   Flag35  (Id));
      W ("Suppress_Elaboration_Warnings", Flag148 (Id));
      W ("Suppress_Index_Checks",         Flag36  (Id));
      W ("Suppress_Init_Proc",            Flag105 (Id));
      W ("Suppress_Length_Checks",        Flag37  (Id));
      W ("Suppress_Overflow_Checks",      Flag38  (Id));
      W ("Suppress_Range_Checks",         Flag39  (Id));
      W ("Suppress_Storage_Checks",       Flag40  (Id));
      W ("Suppress_Style_Checks",         Flag165 (Id));
      W ("Suppress_Tag_Checks",           Flag41  (Id));
      W ("Uses_Sec_Stack",                Flag95  (Id));
      W ("Vax_Float",                     Flag151 (Id));
      W ("Warnings_Off",                  Flag96  (Id));

   end Write_Entity_Flags;

   -----------------------
   -- Write_Entity_Info --
   -----------------------

   procedure Write_Entity_Info (Id : Entity_Id; Prefix : String) is

      procedure Write_Kind (Id : Entity_Id);
      --  Write Ekind field of entity

      procedure Write_Attribute (Which : String; Nam : E);
      --  Write attribute value with given string name


      procedure Write_Kind (Id : Entity_Id) is
         K : constant String := Entity_Kind'Image (Ekind (Id));

      begin
         Write_Str (Prefix);
         Write_Str ("   Kind    ");

         if Is_Type (Id) and then Is_Tagged_Type (Id) then
            Write_Str ("TAGGED ");
         end if;

         Write_Str (K (3 .. K'Length));
         Write_Str (" ");

         if Is_Type (Id) and then Depends_On_Private (Id) then
            Write_Str ("Depends_On_Private ");
         end if;
      end Write_Kind;

      procedure Write_Attribute (Which : String; Nam : E) is
      begin
         Write_Str (Prefix);
         Write_Str (Which);
         Write_Int (Int (Nam));
         Write_Str (" ");
         Write_Name (Chars (Nam));
         Write_Str (" ");
      end Write_Attribute;

   --  Start of processing for Write_Entity_Info

   begin
      Write_Eol;
      Write_Attribute ("Name ", Id);
      Write_Int (Int (Id));
      Write_Eol;
      Write_Kind (Id);
      Write_Eol;
      Write_Attribute ("   Type    ", Etype (Id));
      Write_Eol;
      Write_Attribute ("   Scope   ", Scope (Id));
      Write_Eol;

      case Ekind (Id) is

         when Discrete_Kind =>
            Write_Str ("Bounds: Id = ");

            if Present (Scalar_Range (Id)) then
               Write_Int (Int (Type_Low_Bound (Id)));
               Write_Str (" .. Id = ");
               Write_Int (Int (Type_High_Bound (Id)));
            else
               Write_Str ("Empty");
            end if;

            Write_Eol;

         when Array_Kind =>
            declare
               Index : E;

            begin
               Write_Attribute ("   Component Type    ",
                                                   Component_Type (Id));
               Write_Eol;
               Write_Str (Prefix);
               Write_Str ("   Indices ");

               Index := First_Index (Id);

               while Present (Index) loop
                  Write_Attribute (" ", Etype (Index));
                  Index := Next_Index (Index);
               end loop;

               Write_Eol;
            end;

         when Access_Kind =>
               Write_Attribute
                 ("   Directly Designated Type ",
                  Directly_Designated_Type (Id));
               Write_Eol;

         when Overloadable_Kind =>
            if Present (Homonym (Id)) then
               Write_Str ("   Homonym   ");
               Write_Name (Chars (Homonym (Id)));
               Write_Str ("   ");
               Write_Int (Int (Homonym (Id)));
               Write_Eol;
            end if;

            Write_Eol;

         when E_Component =>
            if Ekind (Scope (Id)) in Record_Kind then
               Write_Attribute (
                  "   Original_Record_Component   ",
                  Original_Record_Component (Id));
               Write_Int (Int (Original_Record_Component (Id)));
               Write_Eol;
            end if;

         when others => null;
      end case;
   end Write_Entity_Info;

   -----------------------
   -- Write_Field6_Name --
   -----------------------

   procedure Write_Field6_Name (Id : Entity_Id) is
   begin
      Write_Str ("First_Rep_Item");
   end Write_Field6_Name;

   -----------------------
   -- Write_Field7_Name --
   -----------------------

   procedure Write_Field7_Name (Id : Entity_Id) is
   begin
      Write_Str ("Freeze_Node");
   end Write_Field7_Name;

   -----------------------
   -- Write_Field8_Name --
   -----------------------

   procedure Write_Field8_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Formal_Kind                                |
              E_Function                                 =>
            Write_Str ("Mechanism");

         when Type_Kind                                  =>
            Write_Str ("Associated_Node_For_Itype");

         when E_Package                                  =>
            Write_Str ("Dependent_Instances");

         when E_Variable                                 =>
            Write_Str ("Hiding_Loop_Variable");

         when others                                     =>
            Write_Str ("Field8??");
      end case;
   end Write_Field8_Name;

   -----------------------
   -- Write_Field9_Name --
   -----------------------

   procedure Write_Field9_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Type_Kind                                  =>
            Write_Str ("Class_Wide_Type");

         when E_Constant | E_Variable                    =>
            Write_Str ("Size_Check_Code");

         when E_Function                                 |
              E_Generic_Function                         |
              E_Generic_Package                          |
              E_Generic_Procedure                        |
              E_Package                                  |
              E_Procedure                                =>
            Write_Str ("Renaming_Map");

         when others                                     =>
            Write_Str ("Field9??");
      end case;
   end Write_Field9_Name;

   ------------------------
   -- Write_Field10_Name --
   ------------------------

   procedure Write_Field10_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Type_Kind                                  =>
            Write_Str ("Referenced_Object");

         when E_In_Parameter                             |
              E_Constant                                 =>
            Write_Str ("Discriminal_Link");

         when E_Function                                 |
              E_Package                                  |
              E_Package_Body                             |
              E_Procedure                                =>
            Write_Str ("Handler_Records");

         when others                                     =>
            Write_Str ("Field10??");
      end case;
   end Write_Field10_Name;

   ------------------------
   -- Write_Field11_Name --
   ------------------------

   procedure Write_Field11_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Formal_Kind                                =>
            Write_Str ("Entry_Component");

         when E_Component                                |
              E_Discriminant                             =>
            Write_Str ("Component_First_Bit");

         when E_Constant                                 =>
            Write_Str ("Full_View");

         when E_Enumeration_Literal                      =>
            Write_Str ("Enumeration_Pos");

         when E_Block                                    =>
            Write_Str ("Block_Node");

         when E_Function                                 |
              E_Procedure                                |
              E_Entry                                    |
              E_Entry_Family                             =>
            Write_Str ("Protected_Body_Subprogram");

         when Type_Kind                                  =>
            Write_Str ("Full_View");

         when others                                     =>
            Write_Str ("Field11??");
      end case;
   end Write_Field11_Name;

   ------------------------
   -- Write_Field12_Name --
   ------------------------

   procedure Write_Field12_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Entry_Kind                                 =>
            Write_Str ("Barrier_Function");

         when E_Enumeration_Literal                      =>
            Write_Str ("Enumeration_Rep");

         when Type_Kind                                  |
              E_Component                                |
              E_Constant                                 |
              E_Discriminant                             |
              E_Variable                                 =>
            Write_Str ("Esize");

         when E_Function                                 |
              E_Procedure                                =>
            Write_Str ("Next_Inlined_Subprogram");

         when E_In_Parameter                             =>
            Write_Str ("Default_Expr_Function");

         when E_Package                                  =>
            Write_Str ("Associated_Formal_Package");

         when others                                     =>
            Write_Str ("Field12??");
      end case;
   end Write_Field12_Name;

   ------------------------
   -- Write_Field13_Name --
   ------------------------

   procedure Write_Field13_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Access_Kind                                =>
            Write_Str ("Associated_Storage_Pool");

         when Array_Kind                                 =>
            Write_Str ("Component_Size");

         when E_Component                                |
              E_Discriminant                             =>
            Write_Str ("Component_Clause");

         when Class_Wide_Kind                            |
              E_Record_Type                              |
              E_Record_Subtype                           |
              Private_Kind                               =>
            Write_Str ("Primitive_Operations");

         when E_Block                                    |
              Concurrent_Kind                            |
              E_Function                                 |
              E_Procedure                                |
              Entry_Kind                                 =>
            Write_Str ("Finalization_Chain_Entity");

         when E_Enumeration_Literal                      =>
            Write_Str ("Debug_Renaming_Link");

         when E_Package                                  =>
            Write_Str ("Body_Entity");

         when Formal_Kind                                |
              E_Variable                                 =>
            Write_Str ("Extra_Accessibility");

         when Discrete_Kind                              |
              Fixed_Point_Kind                           =>
            Write_Str ("RM_Size");

         when others                                     =>
            Write_Str ("Field13??");
      end case;
   end Write_Field13_Name;

   ------------------------
   -- Write_Field14_Name --
   ------------------------

   procedure Write_Field14_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Access_Kind                                =>
            Write_Str ("Associated_Final_Chain");

         when Array_Kind                                 =>
            Write_Str ("Packed_Array_Type");

         when E_Block                                    =>
            Write_Str ("Entry_Cancel_Parameter");

         when E_Component                                =>
            Write_Str ("Protected_Operation");

         when E_Discriminant                             =>
            Write_Str ("CR_Discriminant");

         when E_Enumeration_Type                         =>
            Write_Str ("Enum_Pos_To_Rep");

         when Formal_Kind                                |
              E_Variable                                 =>
            Write_Str ("Extra_Constrained");

         when Concurrent_Kind                            |
              Incomplete_Or_Private_Kind                 |
              Class_Wide_Kind                            |
              E_Record_Type                              |
              E_Record_Subtype                           =>
            Write_Str ("Girder_Constraint");

         when E_Function                                 |
              E_Generic_Function                         |
              E_Package                                  |
              E_Generic_Package                          |
              E_Procedure                                |
              E_Generic_Procedure                        =>
            Write_Str ("Generic_Renamings");

         when Entry_Kind                                 =>
            Write_Str ("Privals_Chain");

         when others                                     =>
            Write_Str ("Field14??");
      end case;
   end Write_Field14_Name;

   ------------------------
   -- Write_Field15_Name --
   ------------------------

   procedure Write_Field15_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Access_Kind                                |
              Task_Kind                                  =>
            Write_Str ("Storage_Size_Variable");

         when Decimal_Fixed_Point_Kind                   =>
            Write_Str ("Scale_Value");

         when E_Discriminant                             =>
            Write_Str ("Discriminant_Number");

         when Formal_Kind                                =>
            Write_Str ("Extra_Formal");

         when Record_Kind                                =>
            Write_Str ("Access_Disp_Table");

         when E_Function                                 |
              E_Procedure                                =>
            Write_Str ("DT_Position");

         when Entry_Kind                                 =>
            Write_Str ("Entry_Parameters_Type");

         when E_Component                                =>
            Write_Str ("DT_Entry_Count");

         when E_Package                                  =>
            Write_Str ("Related_Instance");

         when E_Protected_Type                           =>
            Write_Str ("Entry_Bodies_Array");

         when E_String_Literal_Subtype                   =>
            Write_Str ("String_Literal_Low_Bound");

         when E_Variable                                 =>
            Write_Str ("Shared_Mem_Read_Proc");

         when others                                     =>
            Write_Str ("Field15??");
      end case;
   end Write_Field15_Name;

   ------------------------
   -- Write_Field16_Name --
   ------------------------

   procedure Write_Field16_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Private_Kind                               =>
            Write_Str ("Underlying_Full_View");

         when E_Component                                =>
            Write_Str ("Entry_Formal");

         when E_Function                                 |
              E_Procedure                                =>
            Write_Str ("DTC_Entity");

         when E_Package                                  |
              E_Generic_Package                          |
              Concurrent_Kind                            =>
            Write_Str ("First_Private_Entity");

         when E_String_Literal_Subtype                   =>
            Write_Str ("String_Literal_Length");

         when E_Enum_Table_Type                          =>
            Write_Str ("Table_High_Bound");

         when E_Variable                                 |
              E_Out_Parameter                            =>
            Write_Str ("Unset_Reference");

         when E_Record_Subtype                           |
              E_Class_Wide_Subtype                       =>
            Write_Str ("Cloned_Subtype");

         when others                                     =>
            Write_Str ("Field16??");
      end case;
   end Write_Field16_Name;

   ------------------------
   -- Write_Field17_Name --
   ------------------------

   procedure Write_Field17_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Digits_Kind                                =>
            Write_Str ("Digits_Value");

         when E_Component                                =>
            Write_Str ("Prival");

         when E_Discriminant                             =>
            Write_Str ("Discriminal");

         when E_Block                                    |
              Class_Wide_Kind                            |
              Concurrent_Kind                            |
              Private_Kind                               |
              E_Entry                                    |
              E_Entry_Family                             |
              E_Function                                 |
              E_Generic_Function                         |
              E_Generic_Package                          |
              E_Generic_Procedure                        |
              E_Loop                                     |
              E_Operator                                 |
              E_Package                                  |
              E_Package_Body                             |
              E_Procedure                                |
              E_Record_Type                              |
              E_Record_Subtype                           |
              E_Subprogram_Body                          |
              E_Subprogram_Type                          =>
            Write_Str ("First_Entity");

         when Array_Kind                                 =>
            Write_Str ("First_Index");

         when E_Protected_Body                           =>
            Write_Str ("Object_Ref");

         when Enumeration_Kind                           =>
            Write_Str ("First_Literal");

         when Access_Kind                                =>
            Write_Str ("Master_Id");

         when Modular_Integer_Kind                       =>
            Write_Str ("Modulus");

         when Formal_Kind                                |
               E_Constant                                |
               E_Generic_In_Out_Parameter                |
               E_Variable                                =>
            Write_Str ("Actual_Subtype");

         when others                                     =>
            Write_Str ("Field17??");
      end case;
   end Write_Field17_Name;

   -----------------------
   -- Write_Field18_Name --
   -----------------------

   procedure Write_Field18_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Enumeration_Literal                      |
              E_Function                                 |
              E_Operator                                 |
              E_Procedure                                =>
            Write_Str ("Alias");

         when E_Record_Type                =>
            Write_Str ("Corresponding_Concurrent_Type");

         when E_Entry_Index_Parameter                    =>
            Write_Str ("Entry_Index_Constant");

         when E_Class_Wide_Subtype                       |
              E_Access_Protected_Subprogram_Type         |
              E_Access_Subprogram_Type                   |
              E_Exception_Type                           =>
            Write_Str ("Equivalent_Type");

         when Enumeration_Kind                           =>
            Write_Str ("Lit_Name_Table");

         when Fixed_Point_Kind                           =>
            Write_Str ("Delta_Value");

         when E_Constant                                 |
              E_Variable                                 =>
            Write_Str ("Renamed_Object");

         when E_Exception                                |
              E_Package                                  |
              E_Generic_Function                         |
              E_Generic_Procedure                        |
              E_Generic_Package                          =>
            Write_Str ("Renamed_Entity");

         when Incomplete_Or_Private_Kind                 =>
            Write_Str ("Private_Dependents");

         when Concurrent_Kind                            =>
            Write_Str ("Corresponding_Record_Type");

         when E_Label                                    |
              E_Loop                                     |
              E_Block                                    =>
            Write_Str ("Enclosing_Scope");

         when others                                     =>
            Write_Str ("Field18??");
      end case;
   end Write_Field18_Name;

   -----------------------
   -- Write_Field19_Name --
   -----------------------

   procedure Write_Field19_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Array_Type                               |
              E_Array_Subtype                            =>
            Write_Str ("Related_Array_Object");

         when E_Discriminant                             =>
            Write_Str ("Corresponding_Discriminant");

         when Task_Kind                                  =>
            Write_Str ("Task_Body_Procedure");

         when E_Function                                 =>
            if not Comes_From_Source (Id)
                 and then
               Chars (Id) = Name_Op_Ne
            then
               Write_Str ("Corresponding_Equality");

            elsif Comes_From_Source (Id) then
               Write_Str ("Elaboration_Entity");

            else
               Write_Str ("Field19??");
            end if;

         when E_Package_Body                             |
              Formal_Kind                                =>
            Write_Str ("Spec_Entity");

         when E_Procedure                                |
              E_Package                                  |
              Generic_Unit_Kind                          =>
            Write_Str ("Elaboration_Entity");

         when E_Record_Type                              =>
            Write_Str ("Parent_Subtype");

         when others                                     =>
            Write_Str ("Field19??");
      end case;
   end Write_Field19_Name;

   -----------------------
   -- Write_Field20_Name --
   -----------------------

   procedure Write_Field20_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Array_Kind                                 =>
            Write_Str ("Component_Type");

         when E_In_Parameter                            |
              E_Generic_In_Parameter                     =>
            Write_Str ("Default_Value");

         when Access_Kind                                =>
            Write_Str ("Directly_Designated_Type");

         when E_Component                                =>
            Write_Str ("Discriminant_Checking_Func");

         when E_Discriminant                             =>
            Write_Str ("Discriminant_Default_Value");

         when E_Block                                    |
              Class_Wide_Kind                            |
              Concurrent_Kind                            |
              Private_Kind                               |
              E_Entry                                    |
              E_Entry_Family                             |
              E_Function                                 |
              E_Generic_Function                         |
              E_Generic_Package                          |
              E_Generic_Procedure                        |
              E_Loop                                     |
              E_Operator                                 |
              E_Package                                  |
              E_Package_Body                             |
              E_Procedure                                |
              E_Record_Type                              |
              E_Record_Subtype                           |
              E_Subprogram_Body                          |
              E_Subprogram_Type                          =>

            Write_Str ("Last_Entity");

         when Scalar_Kind                                =>
            Write_Str ("Scalar_Range");

         when others                                     =>
            Write_Str ("Field20??");
      end case;
   end Write_Field20_Name;

   -----------------------
   -- Write_Field21_Name --
   -----------------------

   procedure Write_Field21_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Constant                                 |
              E_Exception                                |
              E_Function                                 |
              E_Generic_Function                         |
              E_Procedure                                |
              E_Generic_Procedure                        |
              E_Variable                                 =>
            Write_Str ("Interface_Name");

         when Concurrent_Kind                            |
              Incomplete_Or_Private_Kind                 |
              Class_Wide_Kind                            |
              E_Record_Type                              |
              E_Record_Subtype                           =>
            Write_Str ("Discriminant_Constraint");

         when Entry_Kind                                 =>
            Write_Str ("Accept_Address");

         when Fixed_Point_Kind                           =>
            Write_Str ("Small_Value");

         when others                                     =>
            Write_Str ("Field21??");
      end case;
   end Write_Field21_Name;

   -----------------------
   -- Write_Field22_Name --
   -----------------------

   procedure Write_Field22_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when E_Component                                |
              E_Discriminant                             =>
            Write_Str ("Original_Record_Component");

         when E_Enumeration_Literal                      =>
            Write_Str ("Enumeration_Rep_Expr");

         when E_Exception                                =>
            Write_Str ("Exception_Code");

         when Formal_Kind                                =>
            Write_Str ("Protected_Formal");

         when E_Record_Type                              =>
            Write_Str ("Corresponding_Remote_Type");

         when E_Block                                    |
              E_Entry                                    |
              E_Entry_Family                             |
              E_Function                                 |
              E_Loop                                     |
              E_Package                                  |
              E_Package_Body                             |
              E_Generic_Package                          |
              E_Generic_Function                         |
              E_Generic_Procedure                        |
              E_Procedure                                |
              E_Protected_Type                           |
              E_Subprogram_Body                          |
              E_Task_Type                                =>
            Write_Str ("Scope_Depth");

         when E_Record_Type_With_Private                 |
              E_Record_Subtype_With_Private              |
              E_Private_Type                             |
              E_Private_Subtype                          |
              E_Limited_Private_Type                     |
              E_Limited_Private_Subtype                  =>
            Write_Str ("Private_View");

         when E_Variable                                 =>
            Write_Str ("Shared_Mem_Assign_Proc");

         when others                                     =>
            Write_Str ("Field22??");
      end case;
   end Write_Field22_Name;

   -----------------------
   -- Write_Field23_Name --
   -----------------------

   procedure Write_Field23_Name (Id : Entity_Id) is
   begin
      case Ekind (Id) is
         when Type_Kind                                  |
              Object_Kind                                =>
            Write_Str ("Alignment");

         when E_Function                                 |
              E_Procedure                                =>
            Write_Str ("First_Optional_Parameter");

         when E_Package                                  |
              E_Generic_Package                          =>
            Write_Str ("Shadow_Entities");

         when others                                     =>
            Write_Str ("Field23??");
      end case;
   end Write_Field23_Name;

   -------------------------
   -- Iterator Procedures --
   -------------------------

   procedure Proc_Next_Component           (N : in out Node_Id) is
   begin
      N := Next_Component (N);
   end Proc_Next_Component;

   procedure Proc_Next_Discriminant        (N : in out Node_Id) is
   begin
      N := Next_Discriminant (N);
   end Proc_Next_Discriminant;

   procedure Proc_Next_Formal              (N : in out Node_Id) is
   begin
      N := Next_Formal (N);
   end Proc_Next_Formal;

   procedure Proc_Next_Formal_With_Extras  (N : in out Node_Id) is
   begin
      N := Next_Formal_With_Extras (N);
   end Proc_Next_Formal_With_Extras;

   procedure Proc_Next_Girder_Discriminant (N : in out Node_Id) is
   begin
      N := Next_Girder_Discriminant (N);
   end Proc_Next_Girder_Discriminant;

   procedure Proc_Next_Index               (N : in out Node_Id) is
   begin
      N := Next_Index (N);
   end Proc_Next_Index;

   procedure Proc_Next_Inlined_Subprogram  (N : in out Node_Id) is
   begin
      N := Next_Inlined_Subprogram (N);
   end Proc_Next_Inlined_Subprogram;

   procedure Proc_Next_Literal             (N : in out Node_Id) is
   begin
      N := Next_Literal (N);
   end Proc_Next_Literal;

end Einfo;
