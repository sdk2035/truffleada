------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               J X _ C H 7                                --
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with Errout;   use Errout;
with JVM;      use JVM;
with J_String; use J_String;
with Jx_Decl;  use Jx_Decl;
with Jx_Drive; use Jx_Drive;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Types;    use Types;

package body Jx_Ch7 is

   ----------------------------
   -- Generate_Package_Class --
   ----------------------------

   function Generate_Package_Class (Pkg : Node_Id) return Boolean is
      Pkg_Entity  : Entity_Id;
      Pkg_Class   : Class_Id;
      Elab_Method : Method_Id;
      Other_Class : Class_Id;
      Pkg_Spec    : Node_Id;

   begin
      if Nkind (Pkg) = N_Package_Body then
         Pkg_Spec := Parent (Corresponding_Spec (Pkg));
      else
         Pkg_Spec   := Pkg;
      end if;

      if Nkind (Pkg_Spec) = N_Defining_Program_Unit_Name then
         Pkg_Spec := Parent (Pkg_Spec);
      end if;

      Pkg_Entity := Defining_Entity (Pkg_Spec);

      --  We exit immediately rather than generating a class if this
      --  is an imported package spec with convention Java.

      if Is_Imported (Pkg_Entity)
        and then Convention (Pkg_Entity) = Convention_Java
        and then Nkind (Pkg) /= N_Package_Body
      then
         return False;
      end if;

      Declare_Package_Class (Pkg_Entity);
      Pkg_Class := JVM_Class (Pkg_Entity);

      Class_Stack.Push (Pkg_Class);
      Begin_Class_File (Pkg_Class);
      Current_Compilation_Class := Current_Class;

      Generate_Class_Init_Method (Pkg_Class);
      Generate_Default_Constructor (Pkg_Class);

      Elab_Method := New_Method (Pkg_Class, Name ("_elabs"), Void_Type, True);
      Open_Method (Elab_Method);
      Set_Current_Method (Elab_Method);
      Method_Stack.Push (Elab_Method);

      if Nkind (Pkg) = N_Package_Body then
         Translate (Pkg_Spec);
      else
         Translate (Pkg);
      end if;

      Gen_Method_Return;
      Method_Stack.Pop;
      Close_Method (Elab_Method);

      if Nkind (Pkg) = N_Package_Body then
         Elab_Method
           := New_Method (Pkg_Class, Name ("_elabb"), Void_Type, True);
         Open_Method (Elab_Method);
         Set_Current_Method (Elab_Method);
         Method_Stack.Push (Elab_Method);
         Translate (Pkg);
         Gen_Method_Return;
         Method_Stack.Pop;
         Close_Method (Elab_Method);
      end if;

      pragma Assert (Method_Stack.Empty);

      --  Note that we don't close the package's class files if
      --  there have been any compilation errors (the JGNAT back
      --  end can generate errors). There are a few other places
      --  where class files are closed where we don't yet check
      --  this. We should probably add an extra level of abstraction
      --  for closing class files that would do the check for errors. ???

      while Class_Stack.Top /= Pkg_Class loop
         Other_Class := Class_Stack.Top;
         if not Compilation_Errors then
            End_Class_File (Other_Class);
         end if;
         Class_Stack.Pop;
      end loop;

      pragma Assert (Class_Stack.Top = Pkg_Class);

      if not Compilation_Errors then
         End_Class_File (Pkg_Class);
      end if;
      Class_Stack.Pop;
      return True;
   end Generate_Package_Class;

end Jx_Ch7;
