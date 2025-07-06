------------------------------------------------------------------------------
--                                                                          --
--                   JGNAT RUN-TIME LIBRARY (GNARL) COMPONENTS              --
--                                                                          --
--                  S Y S T E M . T A S K I N G . D E B U G                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.3 $
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- JGNAT -  The GNAT Ada 95 tool chain for the Java (TM) Virtual Machine is --
--          maintained by Ada Core Technologies, Inc. - http://www.gnat.com --
--                                                                          --
------------------------------------------------------------------------------

--  This is the Java specific version of this package
--  It currently only provides null debug functions.

--  This package encapsulates all direct interfaces to task debugging services
--  that are needed by gdb with gnat mode (1.13 and higher)

--  Note : This file *must* be compiled with debugging information

--  Do not add any dependency to GNARL packages since this package is used
--  in both normal and resticted (ravenscar) environments.

with System.Task_Primitives.Operations;

package body System.Tasking.Debug is

   use Interfaces.C;

   package STPO renames System.Task_Primitives.Operations;

   type Trace_Flag_Set is array (Character) of Boolean;

   Trace_On : Trace_Flag_Set := ('A' .. 'Z' => False, others => True);

   ------------------------
   -- Task_Creation_Hook --
   ------------------------

   procedure Task_Creation_Hook (Thread : OS_Interface.Thread_Id) is
      pragma Inspection_Point (Thread);
   begin
      null;
   end Task_Creation_Hook;

   ---------------------------
   -- Task_Termination_Hook --
   ---------------------------

   procedure Task_Termination_Hook is
   begin
      null;
   end Task_Termination_Hook;

   ----------
   -- Self --
   ----------

   function Self return Task_ID is
   begin
      return STPO.Self;
   end Self;

   --------------------
   -- Set_User_State --
   --------------------

   procedure Set_User_State (Value : Integer) is
   begin
      STPO.Self.User_State := Value;
   end Set_User_State;

   ---------------------
   -- Print_Task_Info --
   ---------------------

   procedure Print_Task_Info (T : ST.Task_ID) is
   begin
      null;
   end Print_Task_Info;

   ----------------------------
   -- Print_Task_Info_Header --
   ----------------------------

   procedure Print_Task_Info_Header is
   begin
      null;
   end Print_Task_Info_Header;

   ------------------------
   -- Print_Current_Task --
   ------------------------

   procedure Print_Current_Task is
   begin
      Print_Task_Info (STPO.Self);
   end Print_Current_Task;

   ----------------------
   -- Print_List_Tasks --
   ----------------------

   procedure List_Tasks is
      C : ST.Task_ID;

   begin
      Print_Task_Info_Header;
      C := All_Tasks_List;

      while C /= null loop
         Print_Task_Info (C);
         C := C.Common.All_Tasks_Link;
      end loop;
   end List_Tasks;

   -----------------------
   -- Print_Accept_Info --
   -----------------------

   procedure Print_Accept_Info (T : ST.Task_ID) is
   begin
      null;
   end Print_Accept_Info;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Self_ID  : ST.Task_ID;
      Msg      : String;
      Other_ID : ST.Task_ID;
      Flag     : Character) is
   begin
      null;
   end Trace;

   procedure Trace
     (Self_ID : ST.Task_ID;
      Msg     : String;
      Flag    : Character) is
   begin
      Trace (Self_ID, Msg, null, Flag);
   end Trace;

   procedure Trace
     (Msg : String;
      Flag : Character)
   is
      Self_ID : constant ST.Task_ID := STPO.Self;

   begin
      Trace (Self_ID, Msg, null, Flag);
   end Trace;

   procedure Trace
     (Msg      : String;
      Other_ID : ST.Task_ID;
      Flag     : Character)
   is
      Self_ID : constant ST.Task_ID := STPO.Self;

   begin
      Trace (Self_ID, Msg, null, Flag);
   end Trace;

   ---------------
   -- Set_Trace --
   ---------------

   procedure Set_Trace
     (Flag  : Character;
      Value : Boolean := True)
   is
   begin
      Trace_On (Flag) := Value;
   end Set_Trace;

   -----------
   -- Image --
   -----------

   function Image (T : ST.Task_ID) return String is
   begin
      return "";
   end Image;

   -----------------------
   -- Suspend_All_Tasks --
   -----------------------

   procedure Suspend_All_Tasks (Thread_Self : OS_Interface.Thread_Id) is
      C : ST.Task_ID;
      R : Boolean;

   begin
      C := All_Tasks_List;

      while C /= null loop
         R := STPO.Suspend_Task (C, Thread_Self);
         C := C.Common.All_Tasks_Link;
      end loop;
   end Suspend_All_Tasks;

   ------------------------
   -- Continue_All_Tasks --
   ------------------------

   procedure Resume_All_Tasks (Thread_Self : OS_Interface.Thread_Id) is
      C : ST.Task_ID;
      R : Boolean;

   begin
      C := All_Tasks_List;

      while C /= null loop
         R := STPO.Resume_Task (C, Thread_Self);
         C := C.Common.All_Tasks_Link;
      end loop;
   end Resume_All_Tasks;

end System.Tasking.Debug;
