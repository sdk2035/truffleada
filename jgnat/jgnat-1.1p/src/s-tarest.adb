------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K I N G . R E S T R I C T E D . S T A G E S      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.9 $
--                                                                          --
--            Copyright (C) 1991-2000 Florida State University              --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

--  This is a simplified version of the System.Tasking.Stages package,
--  intended to be used in a restricted run time.

--  This package represents the high level tasking interface used by the
--  compiler to expand Ada 95 tasking constructs into simpler run time calls.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Parameters;
--  used for Size_Type

with System.Task_Info;
--  used for Task_Info_Type
--           Task_Image_Type

with System.Task_Primitives.Operations;
--  used for Enter_Task
--           Write_Lock
--           Unlock
--           Wakeup
--           Get_Priority
--           Lock/Unlock_All_Tasks_List

with System.Soft_Links;
--  used for the non-tasking routines (*_NT) that refer to global data.
--  They are needed here before the tasking run time has been elaborated.
--  used for Create_TSD
--  This package also provides initialization routines for task specific data.
--  The GNARL must call these to be sure that all non-tasking
--  Ada constructs will work.

with System.Secondary_Stack;
--  used for SS_Init;

with System.Storage_Elements;
--  used for Storage_Array;

package body System.Tasking.Restricted.Stages is

   package STPO renames System.Task_Primitives.Operations;
   package SSL  renames System.Soft_Links;
   package SSE  renames System.Storage_Elements;
   package SST  renames System.Secondary_Stack;

   use System.Task_Primitives;
   use System.Task_Primitives.Operations;
   use System.Task_Info;

   Global_Task_Lock : aliased System.Task_Primitives.RTS_Lock;
   --  This is a global lock; it is used to execute in mutual exclusion
   --  from all other tasks.  It is only used by Task_Lock,
   --  Task_Unlock, and Final_Task_Unlock.

   -----------------------------------------------------------------
   -- Tasking versions of services needed by non-tasking programs --
   -----------------------------------------------------------------

   procedure Task_Lock;
   --  Locks out other tasks. Preceding a section of code by Task_Lock and
   --  following it by Task_Unlock creates a critical region. This is used
   --  for ensuring that a region of non-tasking code (such as code used to
   --  allocate memory) is tasking safe. Note that it is valid for calls to
   --  Task_Lock/Task_Unlock to be nested, and this must work properly, i.e.
   --  only the corresponding outer level Task_Unlock will actually unlock.

   procedure Task_Unlock;
   --  Releases lock previously set by call to Task_Lock. In the nested case,
   --  all nested locks must be released before other tasks competing for the
   --  tasking lock are released.

   function Get_Jmpbuf_Address return Address;
   procedure Set_Jmpbuf_Address (Addr : Address);

   function Get_Sec_Stack_Addr return Address;
   procedure Set_Sec_Stack_Addr (Addr : Address);

   function Get_Exc_Stack_Addr return Address;
   --  Get the exception stack for the current task

   procedure Set_Exc_Stack_Addr (Self_ID : Address; Addr : Address);
   --  Self_ID is the Task_ID of the task that gets the exception stack.
   --  For Self_ID = Null_Address, the current task gets the exception stack.

   function  Get_Machine_State_Addr return Address;
   procedure Set_Machine_State_Addr (Addr : Address);

   function Get_Current_Excep return SSL.EOA;

   procedure Timed_Delay_T (Time : Duration; Mode : Integer);

   ------------------------
   --  Local Subprograms --
   ------------------------

   procedure Task_Wrapper (Self_ID : Task_ID);
   --  This is the procedure that is called by the GNULL from the
   --  new context when a task is created. It waits for activation
   --  and then calls the task body procedure. When the task body
   --  procedure completes, it terminates the task.

   procedure Terminate_Task (Self_ID : Task_ID);
   --  Terminate the calling task.
   --  This should only be called by the Task_Wrapper procedure.

   procedure Vulnerable_Complete_Activation (Self_ID : Task_ID);
   --  Signal to Self_ID's activator that Self_ID has
   --  completed activation.

   ---------------
   -- Task_Lock --
   ---------------

   procedure Task_Lock is
   begin
      STPO.Write_Lock (Global_Task_Lock'Access);
   end Task_Lock;

   -----------------
   -- Task_Unlock --
   -----------------

   procedure Task_Unlock is
   begin
      STPO.Unlock (Global_Task_Lock'Access);
   end Task_Unlock;

   -----------------------
   --  Soft-Link Bodies --
   -----------------------

   function Get_Jmpbuf_Address return  Address is
      Me : constant Task_ID := STPO.Self;

   begin
      return Me.Common.Compiler_Data.Jmpbuf_Address;
   end Get_Jmpbuf_Address;

   procedure Set_Jmpbuf_Address (Addr : Address) is
      Me : constant Task_ID := STPO.Self;

   begin
      Me.Common.Compiler_Data.Jmpbuf_Address := Addr;
   end Set_Jmpbuf_Address;

   function Get_Sec_Stack_Addr return  Address is
      Me : constant Task_ID := STPO.Self;

   begin
      return Me.Common.Compiler_Data.Sec_Stack_Addr;
   end Get_Sec_Stack_Addr;

   procedure Set_Sec_Stack_Addr (Addr : Address) is
      Me : constant Task_ID := STPO.Self;

   begin
      Me.Common.Compiler_Data.Sec_Stack_Addr := Addr;
   end Set_Sec_Stack_Addr;

   function Get_Exc_Stack_Addr return Address is
      Me : constant Task_ID := STPO.Self;

   begin
      return Me.Common.Compiler_Data.Exc_Stack_Addr;
   end Get_Exc_Stack_Addr;

   procedure Set_Exc_Stack_Addr (Self_ID : Address; Addr : Address) is
      Me : Task_ID := To_Task_Id (Self_ID);

   begin
      if Me = Null_Task then
         Me := STPO.Self;
      end if;

      Me.Common.Compiler_Data.Exc_Stack_Addr := Addr;
   end Set_Exc_Stack_Addr;

   function Get_Machine_State_Addr return Address is
      Me : constant Task_ID := STPO.Self;

   begin
      return Me.Common.Compiler_Data.Machine_State_Addr;
   end Get_Machine_State_Addr;

   procedure Set_Machine_State_Addr (Addr : Address) is
      Me : constant Task_ID := STPO.Self;

   begin
      Me.Common.Compiler_Data.Machine_State_Addr := Addr;
   end Set_Machine_State_Addr;

   function Get_Current_Excep return SSL.EOA is
      Me : constant Task_ID := STPO.Self;

   begin
      return Me.Common.Compiler_Data.Current_Excep'Access;
   end Get_Current_Excep;

   procedure Timed_Delay_T (Time : Duration; Mode : Integer) is
   begin
      STPO.Timed_Delay (STPO.Self, Time, Mode);
   end Timed_Delay_T;

   ------------------
   -- Task_Wrapper --
   ------------------

   --  The task wrapper is a procedure that is called first for each task
   --  task body, and which in turn calls the compiler-generated task body
   --  procedure. The wrapper's main job is to do initialization for the task.
   --  It also has some locally declared objects that server as per-task local
   --  data. Task finalization is done by Complete_Task, which is called from
   --  an at-end handler that the compiler generates.

   --  The variable ID in the task wrapper is used to implement the Self
   --  function on targets where there is a fast way to find the stack base
   --  of the current thread, since it should be at a fixed offset from the
   --  stack base.

   --  The variable Magic_Number is also used in such implementations
   --  of Self, to check whether the current task is an Ada task, as
   --  compared to other-language threads.

   --  Both act as constants, once initialized, but need to be marked as
   --  volatile or aliased to prevent the compiler from optimizing away the
   --  storage. See System.Task_Primitives.Operations.Self for more info.

   procedure Task_Wrapper (Self_ID : Task_ID) is
      ID : Task_ID := Self_ID;
      pragma Volatile (ID);

      --  Do not delete this variable.
      --  In some targets, we need this variable to implement a fast Self.

      use type System.Parameters.Size_Type;
      use type SSE.Storage_Offset;

      Secondary_Stack : aliased SSE.Storage_Array
        (1 .. ID.Common.Compiler_Data.Pri_Stack_Info.Size *
           SSE.Storage_Offset (Parameters.Sec_Stack_Ratio) / 100);
      Secondary_Stack_Address : System.Address := Secondary_Stack'Address;

   begin
      if not Parameters.Sec_Stack_Dynamic then
         ID.Common.Compiler_Data.Sec_Stack_Addr := Secondary_Stack'Address;
         SST.SS_Init (Secondary_Stack_Address, Integer (Secondary_Stack'Last));
      end if;

      --  Set the guard page at the bottom of the stack.
      --  The call to unprotect the page is done in Terminate_Task

      Stack_Guard (Self_ID, True);

      --  Initialize low-level TCB components, that
      --  cannot be initialized by the creator.

      Enter_Task (Self_ID);

      --  We lock All_Tasks_L to wait for activator to finish activating
      --  the rest of the chain, so that everyone in the chain comes out
      --  in priority order.
      --  This also protects the value of Self_ID.Activator.Wait_Count

      Lock_All_Tasks_List;
      Unlock_All_Tasks_List;

      begin
         --  We are separating the following portion of the code in order to
         --  place the exception handlers in a different block.
         --  In this way we do not call Set_Jmpbuf_Address (which needs
         --  Self) before we set Self in Enter_Task

         --  Call the task body procedure.

         Self_ID.Common.Task_Entry_Point (Self_ID.Common.Task_Arg);
         Terminate_Task (Self_ID);

      exception
         when others =>
            Terminate_Task (Self_ID);
      end;
   end Task_Wrapper;

   -------------------------
   --  Restricted GNARLI  --
   -------------------------

   ----------------------------
   -- Create_Restricted_Task --
   ----------------------------

   procedure Create_Restricted_Task
     (Priority      : Integer;
      Size          : System.Parameters.Size_Type;
      Task_Info     : System.Task_Info.Task_Info_Type;
      State         : Task_Procedure_Access;
      Discriminants : System.Address;
      Elaborated    : Access_Boolean;
      Chain         : in out Activation_Chain;
      Task_Image    : System.Task_Info.Task_Image_Type;
      Created_Task  : out Task_ID)
   is
      T             : Task_ID;
      Self_ID       : constant Task_ID := STPO.Self;
      Base_Priority : System.Any_Priority;
      Success       : Boolean;

   begin
      if Priority = Unspecified_Priority then
         Base_Priority := Self_ID.Common.Base_Priority;
      else
         Base_Priority := System.Any_Priority (Priority);
      end if;

      begin
         T := New_ATCB (0);
      exception
         when others =>
            raise Storage_Error;
      end;

      --  All_Tasks_L is used by Abort_Dependents and Abort_Tasks.
      --  Up to this point, it is possible that we may be part of
      --  a family of tasks that is being aborted.

      Lock_All_Tasks_List;
      Write_Lock (Self_ID);

      --  With no task hierarchy, the parent of all non-Environment tasks that
      --  are created must be the Environment task

      Initialize_ATCB
        (Self_ID, State, Discriminants, Self_ID, Elaborated, Base_Priority,
         Task_Info, Size, Library_Task_Level, T, Success);

      --  If we do our job right then there should never be any failures,
      --  which was probably said about the Titanic; so just to be safe,
      --  let's retain this code for now

      if not Success then
         Unlock (Self_ID);
         Unlock_All_Tasks_List;
         raise Storage_Error;
      end if;

      T.Common.Task_Image := Task_Image;
      Unlock (Self_ID);
      Unlock_All_Tasks_List;

      --  Create TSD as early as possible in the creation of a task, since it
      --  may be used by the operation of Ada code within the task.

      SSL.Create_TSD (T.Common.Compiler_Data);
      T.Common.Activation_Link := Chain.T_ID;
      Chain.T_ID := T;
      Created_Task := T;
   end Create_Restricted_Task;

   -------------------------------
   -- Activate_Restricted_Tasks --
   -------------------------------

   --  Note that locks of activator and activated task are both locked
   --  here. This is necessary because C.State and Self.Wait_Count
   --  have to be synchronized. This is safe from deadlock because
   --  the activator is always created before the activated task.
   --  That satisfies our in-order-of-creation ATCB locking policy.

   --  At one point, we may also lock the parent, if the parent is
   --  different from the activator.  That is also consistent with the
   --  lock ordering policy, since the activator cannot be created
   --  before the parent.

   --  Since we are holding both the activator's lock, and Task_Wrapper
   --  locks that before it does anything more than initialize the
   --  low-level ATCB components, it should be safe to wait to update
   --  the counts until we see that the thread creation is successful.

   procedure Activate_Restricted_Tasks
     (Chain_Access : Activation_Chain_Access)
   is
      Self_ID        : constant Task_ID := STPO.Self;
      P              : Task_ID;
      C              : Task_ID;
      Activate_Prio  : System.Any_Priority;
      Success        : Boolean;

   begin
      pragma Assert (Self_ID.Common.Wait_Count = 0);

      --  Lock All_Tasks_L, to prevent activated tasks
      --  from racing ahead before we finish activating the chain.

      --  ?????
      --  Is there some less heavy-handed way?
      --  In an earlier version, we used the activator's lock here,
      --  but that violated the locking order rule when we had
      --  to lock the parent later.

      Lock_All_Tasks_List;

      --  Activate all the tasks in the chain.
      --  Creation of the thread of control was deferred until
      --  activation. So create it now.

      C := Chain_Access.T_ID;

      while C /= null loop
         if C.Common.State /= Terminated then
            pragma Assert (C.Common.State = Unactivated);

            P := C.Common.Parent;
            Write_Lock (P);
            Write_Lock (C);

            if C.Common.Base_Priority < Get_Priority (Self_ID) then
               Activate_Prio := Get_Priority (Self_ID);
            else
               Activate_Prio := C.Common.Base_Priority;
            end if;

            STPO.Create_Task
              (C, Task_Wrapper'Address,
               Parameters.Size_Type
                 (C.Common.Compiler_Data.Pri_Stack_Info.Size),
               Activate_Prio, Success);

            --  There would be a race between the created task and
            --  the creator to do the following initialization,
            --  if we did not have a Lock/Unlock_All_Tasks_List pair
            --  in the task wrapper, to prevent it from racing ahead.

            if Success then
               C.Common.State := Runnable;

            else
               --  No need to set Awake_Count, State, etc. here since the loop
               --  below will do that for any Unactivated tasks.

               Self_ID.Common.Activation_Failed := True;
            end if;

            Unlock (C);
            Unlock (P);
         end if;

         C := C.Common.Activation_Link;
      end loop;

      Unlock_All_Tasks_List;

      --  Close the entries of any tasks that failed thread creation,
      --  and count those that have not finished activation.

      Write_Lock (Self_ID);
      Self_ID.Common.State := Activator_Sleep;

      C := Chain_Access.T_ID;

      while C /= null loop
         Write_Lock (C);

         if C.Common.State = Unactivated then
            C.Common.Activator := null;
            C.Common.State := Terminated;

         elsif C.Common.Activator /= null then
            Self_ID.Common.Wait_Count := Self_ID.Common.Wait_Count + 1;
         end if;

         Unlock (C);
         P := C.Common.Activation_Link;
         C.Common.Activation_Link := null;
         C := P;
      end loop;

      --  Wait for the activated tasks to complete activation.
      --  It is unsafe to abort any of these tasks until the count goes to
      --  zero.

      loop
         exit when Self_ID.Common.Wait_Count = 0;
         Sleep (Self_ID, Activator_Sleep);
      end loop;

      Self_ID.Common.State := Runnable;
      Unlock (Self_ID);

      --  Remove the tasks from the chain.

      Chain_Access.T_ID := null;

      if Self_ID.Common.Activation_Failed then
         Self_ID.Common.Activation_Failed := False;
         raise Tasking_Error;
      end if;
   end Activate_Restricted_Tasks;

   ------------------------------------
   -- Complete_Restricted_Activation --
   ------------------------------------

   procedure Complete_Restricted_Activation is
   begin
      Vulnerable_Complete_Activation (STPO.Self);
   end Complete_Restricted_Activation;

   ------------------------------------
   -- Vulnerable_Complete_Activation --
   ------------------------------------

   --  As in several other places, the locks of the activator and activated
   --  task are both locked here. This follows our deadlock prevention lock
   --  ordering policy, since the activated task must be created after the
   --  activator.

   procedure Vulnerable_Complete_Activation (Self_ID : Task_ID) is
      Activator : Task_ID := Self_ID.Common.Activator;

   begin
      Write_Lock (Activator);
      Write_Lock (Self_ID);

      pragma Assert (Self_ID.Common.Activator /= null);

      --  Remove dangling reference to Activator,
      --  since a task may outlive its activator.

      Self_ID.Common.Activator := null;

      --  Wake up the activator, if it is waiting for a chain
      --  of tasks to activate, and we are the last in the chain
      --  to complete activation

      if Activator.Common.State = Activator_Sleep then
         Activator.Common.Wait_Count := Activator.Common.Wait_Count - 1;

         if Activator.Common.Wait_Count = 0 then
            Wakeup (Activator, Activator_Sleep);
         end if;
      end if;

      --  The activator raises a Tasking_Error if any task
      --  it is activating is completed before the activation is
      --  done. However, if the reason for the task completion is
      --  an abortion, we do not raise an exception. ARM 9.2(5).

      if Self_ID.Common.State = Terminated then
         Activator.Common.Activation_Failed := True;
      end if;

      Unlock (Self_ID);

      --  After the activation, active priority should be the same
      --  as base priority.   We must unlock the Activator first,
      --  though, since it should not wait if we have lower priority.

      Unlock (Activator);

      if Get_Priority (Self_ID) /= Self_ID.Common.Base_Priority then
         Write_Lock (Self_ID);
         Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
         Unlock (Self_ID);
      end if;
   end Vulnerable_Complete_Activation;

   ------------------------------
   -- Complete_Restricted_Task --
   ------------------------------

   procedure Complete_Restricted_Task is
      Self_ID  : constant Task_ID := STPO.Self;

   begin
      Self_ID.Common.State := Terminated;

      if Self_ID.Common.Activator /= null then
         Vulnerable_Complete_Activation (Self_ID);
      end if;
   end Complete_Restricted_Task;

   --------------------
   -- Terminate_Task --
   --------------------

   procedure Terminate_Task (Self_ID : Task_ID) is
   begin
      pragma Assert (Self_ID.Common.Activator = null);
      pragma Assert (Self_ID.Common.Parent = STPO.Environment_Task);

      --  Unprotect the guard page if needed.

      Stack_Guard (Self_ID, False);

      Self_ID.Common.State := Terminated;

      --  WARNING
      --  past this point, this thread must assume that the ATCB
      --  has been deallocated. It should not be accessed again.

      STPO.Exit_Task;
   end Terminate_Task;

   ---------------------------
   -- Restricted_Terminated --
   ---------------------------

   function Restricted_Terminated (T : Task_ID) return Boolean is
   begin
      return T.Common.State = Terminated;
   end Restricted_Terminated;

   ---------------------------
   -- Finalize_Global_Tasks --
   ---------------------------

   --  This is needed to support the compiler interface; it will only be called
   --  by the Environment task. Instead, it will cause the Environment to block
   --  forever, since none of the dependent tasks are expected to terminate

   procedure Finalize_Global_Tasks is
      Self_ID : constant Task_ID := STPO.Self;

   begin
      pragma Assert (Self_ID = STPO.Environment_Task);

      Write_Lock (Self_ID);
      Sleep (Self_ID, Master_Completion_Sleep);
      Unlock (Self_ID);

      --  Should never return from Master Completion Sleep

      raise Program_Error;
   end Finalize_Global_Tasks;

begin
   --  Initialize lock used to implement mutual exclusion between all tasks

   STPO.Initialize_Lock (Global_Task_Lock'Access, STPO.Global_Task_Level);

   --  Notify that the tasking run time has been elaborated so that
   --  the tasking version of the soft links can be used.

   SSL.Lock_Task              := Task_Lock'Access;
   SSL.Unlock_Task            := Task_Unlock'Access;
   SSL.Get_Jmpbuf_Address     := Get_Jmpbuf_Address'Access;
   SSL.Set_Jmpbuf_Address     := Set_Jmpbuf_Address'Access;
   SSL.Get_Sec_Stack_Addr     := Get_Sec_Stack_Addr'Access;
   SSL.Set_Sec_Stack_Addr     := Set_Sec_Stack_Addr'Access;
   SSL.Get_Exc_Stack_Addr     := Get_Exc_Stack_Addr'Access;
   SSL.Set_Exc_Stack_Addr     := Set_Exc_Stack_Addr'Access;
   SSL.Get_Machine_State_Addr := Get_Machine_State_Addr'Access;
   SSL.Set_Machine_State_Addr := Set_Machine_State_Addr'Access;
   SSL.Get_Current_Excep      := Get_Current_Excep'Access;
   SSL.Clock                  := STPO.Clock'Access;
   SSL.Timed_Delay            := Timed_Delay_T'Access;

   --  No need to create a new Secondary Stack, since we will use the
   --  default one created in s-secsta.adb

   SSL.Set_Sec_Stack_Addr     (SSL.Get_Sec_Stack_Addr_NT);
   SSL.Set_Exc_Stack_Addr     (Null_Address, SSL.Get_Exc_Stack_Addr_NT);
   SSL.Set_Jmpbuf_Address     (SSL.Get_Jmpbuf_Address_NT);
   SSL.Set_Machine_State_Addr (SSL.Get_Machine_State_Addr_NT);
end System.Tasking.Restricted.Stages;
