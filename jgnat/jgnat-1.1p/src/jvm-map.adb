------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J V M . M A P                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.7 $                             --
--                                                                          --
--           Copyright (C) 1998-1999 Ada Core Technologies, Inc.            --
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

with Atree;      use Atree;
with Einfo;      use Einfo;
with Sinfo;      use Sinfo;
with Types;      use Types;
with GNAT.Table;

package body JVM.Map is

   package Entity_Map is new GNAT.Table (
     Table_Component_Type => JVM_Id,
     Table_Index_Type     => Entity_Id,
     Table_Low_Bound      => Entity_Id'First,
     Table_Initial        => 10_000,
     Table_Increment      => 100);


   --------------------
   -- Initialize_Map --
   --------------------

   procedure Initialize_Map (Last_Entity : Entity_Id) is
   begin
      Entity_Map.Set_Last (Last_Entity);
      for Index in Entity_Id'First .. Last_Entity loop
         Entity_Map.Table (Index) := Null_JVM_Id;
      end loop;
   end Initialize_Map;

   -------------
   -- Set_Map --
   -------------

   procedure Set_Map (Ada_Entity : Entity_Id; C : JVM.Class_Id) is
   begin
      pragma Assert (Entity_Map.Table (Ada_Entity) = Null_JVM_Id);
      pragma Assert (Ekind (Ada_Entity) = E_Package
                       or else Ekind (Ada_Entity) = E_Function
                       or else Ekind (Ada_Entity) = E_Procedure
                       or else Ekind (Ada_Entity) = E_Exception
                       or else Ekind (Ada_Entity) = E_Exception_Type
                       or else Ekind (Ada_Entity) = E_Access_Subprogram_Type
                       or else Ekind (Ada_Entity) in Einfo.Record_Kind
                       or else Ekind (Ada_Entity) in Einfo.Generic_Unit_Kind);

      Entity_Map.Table (Ada_Entity) := JVM_Id (C);
   end Set_Map;

   procedure Set_Map (Ada_Entity : Entity_Id; T : JVM.Type_Id) is
   begin
      pragma Assert (Entity_Map.Table (Ada_Entity) = Null_JVM_Id);
      pragma Assert (Ekind (Ada_Entity) in Einfo.Type_Kind
             or else Ekind (Ada_Entity) = E_Exception
             or else Ekind (Ada_Entity) = E_Void);

      Entity_Map.Table (Ada_Entity) := JVM_Id (T);
   end Set_Map;

   procedure Set_Map (Ada_Entity : Entity_Id; F : JVM.Field_Id) is
   begin
      pragma Assert (Entity_Map.Table (Ada_Entity) = Null_JVM_Id);
      pragma Assert (Ekind (Ada_Entity) = E_Component
             or else Ekind (Ada_Entity) = E_Discriminant
             or else Ekind (Ada_Entity) = E_Variable
             or else Ekind (Ada_Entity) = E_Constant);

      Entity_Map.Table (Ada_Entity) := JVM_Id (F);
   end Set_Map;

   procedure Set_Map (Ada_Entity : Entity_Id; M : JVM.Method_Id) is
   begin
      pragma Assert (Entity_Map.Table (Ada_Entity) = Null_JVM_Id);
      pragma Assert (Ekind (Ada_Entity) = E_Function
             or else Ekind (Ada_Entity) = E_Procedure);

      Entity_Map.Table (Ada_Entity) := JVM_Id (M);
   end Set_Map;

   procedure Set_Map (Ada_Entity : Entity_Id; L : JVM.Local_Var_Id) is
   begin
      pragma Assert (Entity_Map.Table (Ada_Entity) = Null_JVM_Id);
      pragma Assert (Ekind (Ada_Entity) = E_Variable
             or else Ekind (Ada_Entity) = E_Constant
             or else Ekind (Ada_Entity) = E_Loop_Parameter
             or else Ekind (Ada_Entity) in Einfo.Formal_Kind);

      Entity_Map.Table (Ada_Entity) := JVM_Id (L);
   end Set_Map;

   procedure Set_Map (Ada_Node : Node_Id; L : JVM.Label_Id) is
   begin
      pragma Assert (Entity_Map.Table (Ada_Node) = Null_JVM_Id);
      pragma Assert (Nkind (Ada_Node) = N_Loop_Statement
             or else (Nkind (Ada_Node) in N_Entity
                       and then (Ekind (Ada_Node) = E_Label
                                  or else Ekind (Ada_Node) = E_Block
                                  or else Ekind (Ada_Node) = E_Loop)));

      Entity_Map.Table (Ada_Node) := JVM_Id (L);
   end Set_Map;

   ----------------
   -- JVM_Entity --
   ----------------

   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Class_Id is
   begin
      --  Need to add an operation to the interface of JVM.Info that
      --  allows determining the kind of the JVM entity so an assertion
      --  can be performed here ???
      return Class_Id (Entity_Map.Table (Ada_Entity));
   end JVM_Entity;

   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Type_Id is
   begin
      return Type_Id (Entity_Map.Table (Ada_Entity));
   end JVM_Entity;

   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Field_Id is
   begin
      return Field_Id (Entity_Map.Table (Ada_Entity));
   end JVM_Entity;

   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Method_Id is
   begin
      return Method_Id (Entity_Map.Table (Ada_Entity));
   end JVM_Entity;

   function JVM_Entity (Ada_Entity : Entity_Id) return JVM.Local_Var_Id is
   begin
      return Local_Var_Id (Entity_Map.Table (Ada_Entity));
   end JVM_Entity;

   function JVM_Entity (Ada_Node : Node_Id) return JVM.Label_Id is
   begin
      return Label_Id (Entity_Map.Table (Ada_Node));
   end JVM_Entity;

end JVM.Map;
