------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 8                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.8 $
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
with Jx_Ch4;    use Jx_Ch4;
with Jx_Decl;   use Jx_Decl;
with Jx_Uplev;  use Jx_Uplev;
with Sem_Util;  use Sem_Util;
with Sinfo;     use Sinfo;
with Types;     use Types;

package body Jx_Ch8 is

   -------------------------------
   -- Translate_Object_Renaming --
   -------------------------------

   procedure Translate_Object_Renaming (Obj_Renaming : Node_Id) is
      Obj_Entity : constant Entity_Id := Defining_Entity (Obj_Renaming);
      Obj_Name   : constant Node_Id   := Renamed_Object (Obj_Entity);
      Is_Global  : constant Boolean   := Is_Global_Entity (Obj_Entity);
      Obj_Type   : constant Entity_Id := Full_Type (Obj_Entity);
      Obj_Field  : Field_Id;
      Local_Var  : Local_Var_Id;

   begin
      --  The case of renamings of composite components is handled by
      --  evaluating the renamed object name and saving the reference
      --  in a new JVM entity associated with the renaming entity.

      if Ekind (Obj_Type) in Composite_Kind then
         if Ekind (Obj_Type) in Einfo.Array_Kind then
            Evaluate_Array_Address (Obj_Name);
         else
            Evaluate_Expr (Obj_Name);
         end if;

         if Is_Global then
            Declare_Field (Current_Compilation_Class, Obj_Entity);
            Obj_Field := JVM_Field (Obj_Entity);
            Gen_Put_Static_Field (Obj_Field);

         else
            Declare_Local_Variable (Obj_Entity);
            Local_Var := JVM_Local_Var (Obj_Entity);
            Gen_Store_Local (Local_Var);
         end if;

      --  Handle renaming of elementary objects where the name of the
      --  renamed object is not an entity. For renamings of entities we
      --  simply reevaluate the object name on each reference to the renaming.
      --  The more general case requires saving a partial evaluation of the
      --  name and involves more complex actions on evaluating later references
      --  to the renaming. For now we only support non-entity renamings for
      --  selected components and function cals. The case where the object
      --  name is an indexed name will require saving both the reference to
      --  the array and the index, and reloading both of those when evaluating
      --  a reference to the renaming. It's not clear how to cleanly associate
      --  the renaming with two pieces of information in the indexed name case.
      --  There are also a few other cases we don't support yet, such as
      --  renaming of dereferenced access values. ???

      elsif not Is_Entity_Name (Obj_Name) then
         if Nkind (Obj_Name) = N_Selected_Component
           or else Nkind (Obj_Name) = N_Function_Call
         then
            if Nkind (Obj_Name) = N_Selected_Component then
               Evaluate_Expr (Prefix (Obj_Name));

            else
               Evaluate_Expr (Obj_Name);
            end if;

            if Is_Global then
               Declare_Field (Current_Compilation_Class, Obj_Entity);
               Obj_Field := JVM_Field (Obj_Entity);
               Gen_Put_Static_Field (Obj_Field);

            else
               Declare_Local_Variable (Obj_Entity);
               Local_Var := JVM_Local_Var (Obj_Entity);
               Gen_Store_Local (Local_Var);
            end if;

         else
            Error_Msg_N
              ("unsupported form of scalar object renaming", Obj_Name);
         end if;
      end if;
   end Translate_Object_Renaming;

end Jx_Ch8;
