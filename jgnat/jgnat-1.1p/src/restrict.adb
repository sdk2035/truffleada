------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             R E S T R I C T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.31 $
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
with Casing;   use Casing;
with Errout;   use Errout;
with Exp_Util; use Exp_Util;
with Fname;    use Fname;
with Lib;      use Lib;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Stand;    use Stand;
with Uname;    use Uname;

package body Restrict is

   function Suppress_Restriction_Message (N : Node_Id) return Boolean;
   --  N is the node for a possible restriction violation message, but
   --  the message is to be suppressed if this is an internal file and
   --  this file is not the main unit.

   -------------------
   -- Abort_Allowed --
   -------------------

   function Abort_Allowed return Boolean is
   begin
      return
        Restrictions (No_Abort_Statements) = False
          or else
        Restriction_Parameters (Max_Asynchronous_Select_Nesting) /= 0;
   end Abort_Allowed;

   ------------------------------------
   -- Check_Elaboration_Code_Allowed --
   ------------------------------------

   procedure Check_Elaboration_Code_Allowed (N : Node_Id) is
   begin
      Namet.Unlock;
      Check_Restriction (No_Elaboration_Code, N);
      Namet.Lock;
   end Check_Elaboration_Code_Allowed;

   ---------------------------
   -- Check_Restricted_Unit --
   ---------------------------

   procedure Check_Restricted_Unit (U : Unit_Name_Type; N : Node_Id) is
   begin
      if Suppress_Restriction_Message (N) then
         return;

      elsif Is_Spec_Name (U) then
         declare
            Fnam : constant File_Name_Type :=
                     Get_File_Name (U, Subunit => False);
            R_Id : Restriction_Id;

         begin
            if not Is_Predefined_File_Name (Fnam) then
               return;

            --  Ada child unit spec, needs checking against list

            else
               --  Pad name to 8 characters with blanks

               Get_Name_String (Fnam);
               Name_Len := Name_Len - 4;

               while Name_Len < 8 loop
                  Name_Len := Name_Len + 1;
                  Name_Buffer (Name_Len) := ' ';
               end loop;

               for J in Unit_Array'Range loop
                  if Name_Len = 8
                    and then Name_Buffer (1 .. 8) = Unit_Array (J).Filenm
                  then
                     R_Id := Unit_Array (J).Res_Id;
                     Violations (R_Id) := True;

                     if Restrictions (R_Id) then
                        declare
                           S : constant String := Restriction_Id'Image (R_Id);

                        begin
                           Error_Msg_Unit_1 := U;

                           Error_Msg_N
                             ("dependence on $ not allowed,", N);

                           Name_Buffer (1 .. S'Last) := S;
                           Name_Len := S'Length;
                           Set_Casing (All_Lower_Case);
                           Error_Msg_Name_1 := Name_Enter;
                           Error_Msg_Sloc := Restrictions_Loc (R_Id);

                           Error_Msg_N
                             ("\violates pragma Restriction (%) #", N);
                           return;
                        end;
                     end if;
                  end if;
               end loop;
            end if;
         end;
      end if;
   end Check_Restricted_Unit;

   -----------------------
   -- Check_Restriction --
   -----------------------

   --  Case of simple identifier (no parameter)

   procedure Check_Restriction (R : Restriction_Id; N : Node_Id) is
   begin
      Violations (R) := True;

      if Restrictions (R)
        and then not Suppress_Restriction_Message (N)
      then
         declare
            S : constant String := Restriction_Id'Image (R);

         begin
            Name_Buffer (1 .. S'Last) := S;
            Name_Len := S'Length;
            Set_Casing (All_Lower_Case);
            Error_Msg_Name_1 := Name_Enter;
            Error_Msg_Sloc := Restrictions_Loc (R);
            Error_Msg_N ("violation of restriction %#", N);
         end;
      end if;
   end Check_Restriction;

   --  Case where a parameter is present (but no count)

   procedure Check_Restriction
     (R : Restriction_Parameter_Id;
      N : Node_Id)
   is
   begin
      if Restriction_Parameters (R) = Uint_0
        and then not Suppress_Restriction_Message (N)
      then
         declare
            Loc : constant Source_Ptr := Sloc (N);
            S   : constant String :=
                    Restriction_Parameter_Id'Image (R);

         begin
            Error_Msg_NE
              ("& will be raised at run time?!", N, Standard_Storage_Error);
            Name_Buffer (1 .. S'Last) := S;
            Name_Len := S'Length;
            Set_Casing (All_Lower_Case);
            Error_Msg_Name_1 := Name_Enter;
            Error_Msg_Sloc := Restriction_Parameters_Loc (R);
            Error_Msg_N ("violation of restriction %?#!", N);

            Insert_Action (N,
              Make_Raise_Storage_Error (Loc));
         end;
      end if;
   end Check_Restriction;

   --  Case where a parameter is present, with a count

   procedure Check_Restriction
     (R : Restriction_Parameter_Id;
      V : Uint;
      N : Node_Id)
   is
   begin
      if Restriction_Parameters (R) /= No_Uint
        and then V > Restriction_Parameters (R)
        and then not Suppress_Restriction_Message (N)
      then
         declare
            S : constant String := Restriction_Parameter_Id'Image (R);

         begin
            Name_Buffer (1 .. S'Last) := S;
            Name_Len := S'Length;
            Set_Casing (All_Lower_Case);
            Error_Msg_Name_1 := Name_Enter;
            Error_Msg_Sloc := Restriction_Parameters_Loc (R);
            Error_Msg_N ("maximum value exceeded for restriction %#", N);
         end;
      end if;
   end Check_Restriction;

   ----------------------------------
   -- Disallow_In_No_Run_Time_Mode --
   ----------------------------------

   procedure Disallow_In_No_Run_Time_Mode (Enode : Node_Id) is
   begin
      if No_Run_Time then
         Error_Msg_N
           ("this construct not allowed in No_Run_Time mode", Enode);
      end if;
   end Disallow_In_No_Run_Time_Mode;

   ------------------------
   -- Get_Restriction_Id --
   ------------------------

   function Get_Restriction_Id
     (N    : Name_Id)
      return Restriction_Id
   is
      J : Restriction_Id;

   begin
      Get_Name_String (N);
      Set_Casing (All_Upper_Case);

      J := Restriction_Id'First;
      while J /= Not_A_Restriction_Id loop
         declare
            S : constant String := Restriction_Id'Image (J);

         begin
            exit when S = Name_Buffer (1 .. Name_Len);
         end;

         J := Restriction_Id'Succ (J);
      end loop;

      return J;
   end Get_Restriction_Id;

   ----------------------------------
   -- Get_Restriction_Parameter_Id --
   ----------------------------------

   function Get_Restriction_Parameter_Id
     (N    : Name_Id)
      return Restriction_Parameter_Id
   is
      J : Restriction_Parameter_Id;

   begin
      Get_Name_String (N);
      Set_Casing (All_Upper_Case);

      J := Restriction_Parameter_Id'First;
      while J /= Not_A_Restriction_Parameter_Id loop
         declare
            S : constant String := Restriction_Parameter_Id'Image (J);

         begin
            exit when S = Name_Buffer (1 .. Name_Len);
         end;

         J := Restriction_Parameter_Id'Succ (J);
      end loop;

      return J;
   end Get_Restriction_Parameter_Id;

   ------------------------
   -- Restricted_Profile --
   ------------------------

   --  This implementation must be coordinated with Set_Restricted_Profile

   ------------------------
   -- Restricted_Profile --
   ------------------------

   --  This must be coordinated with Set_Restricted_Profile

   function Restricted_Profile return Boolean is
   begin
      return     Restrictions (No_Abort_Statements)
        and then Restrictions (No_Asynchronous_Control)
        and then Restrictions (No_Entry_Queue)
        and then Restrictions (No_Task_Hierarchy)
        and then Restrictions (No_Task_Allocators)
        and then Restrictions (No_Dynamic_Priorities)
        and then Restrictions (No_Terminate_Alternatives)
        and then Restrictions (No_Dynamic_Interrupts)
        and then Restrictions (No_Protected_Type_Allocators)
        and then Restrictions (No_Local_Protected_Objects)
        and then Restrictions (No_Requeue)
        and then Restrictions (No_Task_Attributes)
        and then Restriction_Parameters (Max_Asynchronous_Select_Nesting) =  0
        and then Restriction_Parameters (Max_Task_Entries)                =  0
        and then Restriction_Parameters (Max_Protected_Entries)           <= 1
        and then Restriction_Parameters (Max_Select_Alternatives)         =  0;
   end Restricted_Profile;

   -------------------
   -- Set_Ravenscar --
   -------------------

   procedure Set_Ravenscar is
   begin
      Set_Restricted_Profile;
      Restrictions (Boolean_Entry_Barriers)  := True;
      Restrictions (No_Select_Statements)    := True;
      Restrictions (No_Calendar)             := True;
      Restrictions (Static_Storage_Size)     := True;
      Restrictions (No_Entry_Queue)          := True;
      Restrictions (No_Relative_Delay)       := True;
      Restrictions (No_Task_Termination)     := True;
   end Set_Ravenscar;

   ----------------------------
   -- Set_Restricted_Profile --
   ----------------------------

   --  This must be coordinated with Restricted_Profile

   procedure Set_Restricted_Profile is
   begin
      Restrictions (No_Abort_Statements)          := True;
      Restrictions (No_Asynchronous_Control)      := True;
      Restrictions (No_Entry_Queue)               := True;
      Restrictions (No_Task_Hierarchy)            := True;
      Restrictions (No_Task_Allocators)           := True;
      Restrictions (No_Dynamic_Priorities)        := True;
      Restrictions (No_Terminate_Alternatives)    := True;
      Restrictions (No_Dynamic_Interrupts)        := True;
      Restrictions (No_Protected_Type_Allocators) := True;
      Restrictions (No_Local_Protected_Objects)   := True;
      Restrictions (No_Requeue)                   := True;
      Restrictions (No_Task_Attributes)           := True;

      Restriction_Parameters (Max_Asynchronous_Select_Nesting) :=  Uint_0;
      Restriction_Parameters (Max_Task_Entries)                :=  Uint_0;
      Restriction_Parameters (Max_Select_Alternatives)         :=  Uint_0;

      if Restriction_Parameters (Max_Protected_Entries) /= Uint_0 then
         Restriction_Parameters (Max_Protected_Entries) := Uint_1;
      end if;
   end Set_Restricted_Profile;

   ----------------------------------
   -- Suppress_Restriction_Message --
   ----------------------------------

   function Suppress_Restriction_Message (N : Node_Id) return Boolean is
   begin
      --  If main unit is library unit, then we will output message

      if In_Extended_Main_Source_Unit (N) then
         return False;

      --  If loaded by rtsfind, then suppress message

      elsif Sloc (N) <= No_Location then
         return True;

      --  Otherwise suppress message if internal file

      else
         return
           Is_Internal_File_Name (Unit_File_Name (Get_Source_Unit (N)));
      end if;
   end Suppress_Restriction_Message;

   ---------------------
   -- Tasking_Allowed --
   ---------------------

   function Tasking_Allowed return Boolean is
   begin
      return
        Restriction_Parameters (Max_Tasks) /= 0;
   end Tasking_Allowed;

end Restrict;
