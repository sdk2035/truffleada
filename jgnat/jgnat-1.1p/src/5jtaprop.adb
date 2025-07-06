------------------------------------------------------------------------------
--                                                                          --
--                  JGNAT RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.14 $
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

--  This is a Java version of this package.

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Tasking.Debug;
--  used for Known_Tasks

with System.Tasking;
--  used for Ada_Task_Control_Block
--           Task_ID
--           ATCB components and types

with System.OS_Interface;

with Interfaces.Java.Lang.Object;
with Interfaces.Java.Lang.Thread;

with Interfaces.Java.Lang.System;
--  used for currentTimeMillis

with System.OS_Primitives;
--  used for Delay_Modes

with Unchecked_Conversion;

package body System.Task_Primitives.Operations is

   use Tasking;
   use OS_Interface;
   use Parameters;
   use Tasking.Debug;
   use OS_Primitives;

   use Interfaces.Java;
   use Interfaces.Java.Lang.Object;
   use Interfaces.Java.Lang.Thread;

   package IJL_Thread renames Interfaces.Java.Lang.Thread;

   ------------------
   --  Local Data  --
   ------------------

   Environment_Task_ID : Task_ID;
   --  A variable to hold Task_ID for the environment task.
   --  If we use this variable to get the Task_ID, we need the following
   --  ATCB_Key only for non-Ada threads.

   All_Tasks_L : aliased System.Task_Primitives.RTS_Lock;
   --  See comments on locking rules in System.Locking_Rules (spec).

   Next_Serial_Number : Task_Serial_Number := 100;
   --  We start at 100, to reserve some special values for
   --  using in error checking.
   --  The following are internal configuration constants needed.

   Dynamic_Priority_Support : constant Boolean := True;
   --  controls whether we poll for pending priority changes during sleeps

   Priority_Ceiling_Emulation : constant Boolean := True;
   --  controls whether we emulate priority ceiling locking

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_Task_Id is new Unchecked_Conversion (System.Address, Task_ID);

   function To_Address is new Unchecked_Conversion (Task_ID, System.Address);

   type Ptr is access Task_ID;
   function To_Ptr is new Unchecked_Conversion (System.Address, Ptr);

   function "+" is new Unchecked_Conversion (Lang.Object.Ref, Lang.Thread.Ref);
   function "+" is new Unchecked_Conversion (Thread_Id, Lang.Thread.Ref);
   function "+" is new Unchecked_Conversion (Lang.Object.Ref, Thread_Id);
   function "+" is new Unchecked_Conversion
     (Lang.Thread.Ref_Class, Lang.Object.Ref);

   subtype int is Interfaces.Java.int;
   subtype long is Interfaces.Java.long;

   procedure Split_MS_NS
     (Rel_Time : in Duration;
      MS       : out long;
      NS       : out int);
   --  Split up Rel_Time into pair of values suitable for
   --  use with Java.Lang.Threads.sleep
   --  Rel_Time must be null or positive.

   procedure Split_MS_NS
     (Rel_Time : Duration;
      MS       : out long;
      NS       : out int)
   is
   begin
      pragma Assert (Rel_Time >= 0.0);

      --  The fractional part of Java_Time, Java_Time - IP, is the number of
      --  nanoseconds in Rel_Time, divided by one million. So, NS is equal
      --  to that number, multiplied by 1E6. As we want an integer number
      --  of nanoseconds, we then calculate its integer part.

      MS := long (Rel_Time * 1.0E3);
      NS := int ((Rel_Time - Duration (Long_Float (MS) / 1.0E3)) * 1.0E9);
   end Split_MS_NS;

   ----------
   -- Self --
   ----------

   function Self return Task_ID is
      T : Task_ID;
   begin
      T := To_Task_Id (Get_Self_Id);
      pragma Assert (T /= Null_Task);
      return T;
   end Self;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   --  Note: mutexes and cond_variables needed per-task basis are
   --        initialized in Intialize_TCB and the Storage_Error is
   --        handled. Other mutexes (such as All_Tasks_L, Memory_Lock...)
   --        used in RTS is initialized before any status change of RTS.
   --        Therefore rasing Storage_Error in the following routines
   --        should be able to be handled safely.

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : access Lock)
   is
   begin
      if Priority_Ceiling_Emulation then
         L.Ceiling := Prio;
      end if;

      L.L := new_Object;
   end Initialize_Lock;

   procedure Initialize_Lock (L : access RTS_Lock; Level : Lock_Level) is
   begin
      L.all := new_Object;
   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : access Lock) is
   begin
      L.L := null;
   end Finalize_Lock;

   procedure Finalize_Lock (L : access RTS_Lock) is
   begin
      L.all := null;
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   procedure Write_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
      Self_ID : constant Task_ID := Self;
   begin
      if Priority_Ceiling_Emulation then
         if Self_ID.Common.LL.Active_Priority > L.Ceiling then
            Ceiling_Violation := True;
            return;
         end if;

         L.Saved_Priority := Self_ID.Common.LL.Active_Priority;

         if Self_ID.Common.LL.Active_Priority < L.Ceiling then
            Self_ID.Common.LL.Active_Priority := L.Ceiling;
         end if;

         monitorenter (L.L);
         Ceiling_Violation := False;

      else
         monitorenter (L.L);
         Ceiling_Violation := False;
      end if;
   end Write_Lock;

   procedure Write_Lock (L : access RTS_Lock) is
   begin
      monitorenter (L.all);
   end Write_Lock;

   procedure Write_Lock (T : Task_ID) is
   begin
      monitorenter (T.Common.LL.L);
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock (L : access Lock; Ceiling_Violation : out Boolean) is
   begin
      Write_Lock (L, Ceiling_Violation);
   end Read_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : access Lock) is
      Self_ID : constant Task_ID := Self;
   begin
      if Priority_Ceiling_Emulation then
         monitorexit (L.L);

         if Self_ID.Common.LL.Active_Priority > L.Saved_Priority then
            Self_ID.Common.LL.Active_Priority := L.Saved_Priority;
         end if;

      else
         monitorexit (L.L);
      end if;
   end Unlock;

   procedure Unlock (L : access RTS_Lock) is
   begin
      monitorexit (L.all);
   end Unlock;

   procedure Unlock (T : Task_ID) is
   begin
      monitorexit (T.Common.LL.L);
   end Unlock;

   --  For the time delay implementation, we need to make sure we
   --  achieve following criteria:

   --  1) We have to delay at least for the amount requested.
   --  2) We have to give up CPU even though the actual delay does not
   --     result in blocking.
   --  3) Except for restricted run-time systems that do not support
   --     ATC or task abort, the delay must be interrupted by the
   --     abort_task operation.
   --  4) The implementation has to be efficient so that the delay overhead
   --     is relatively cheap.
   --  (1)-(3) are Ada requirements. Even though (2) is an Annex-D
   --     requirement we still want to provide the effect in all cases.
   --     The reason is that users may want to use short delays to implement
   --     their own scheduling effect in the absence of language provided
   --     scheduling policies.

   -----------
   -- Clock --
   -----------

   function Clock return Duration is
      Millisecs : Interfaces.Java.long := Lang.System.Current_Time_Millis;

   begin
      return Duration (Millisecs / 1000)
        + Duration (Millisecs rem 1000) / 1000;
   end Clock;

   -------------------
   -- RT_Resolution --
   -------------------

   function RT_Resolution return Duration is
   begin
      return 10#1.0#E-6;
   end RT_Resolution;

   -----------
   -- Yield --
   -----------

   procedure Yield (Do_Yield : Boolean := True) is
   begin
      if Do_Yield then
         Lang.Thread.Yield;
      end if;
   end Yield;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (T    : Task_ID;
      Prio : System.Any_Priority;
      Loss_Of_Inheritance : Boolean := False)
   is
      --  First : constant System.Priority := System.Priority'First;
      --  Last  : constant System.Priority := System.Priority'Last;

   begin
      T.Common.Current_Priority := Prio;

      if Priority_Ceiling_Emulation then
         if T.Common.LL.Active_Priority < Prio then
            T.Common.LL.Active_Priority := Prio;
         end if;
      end if;

      --  if Prio <= Last then
      --     Set_Priority
      --       (+T.Common.LL.Thread,
      --        int (MIN_PRIORITY + (MAX_PRIORITY - 1 - MIN_PRIORITY) *
      --             (Prio - First) / (Last - First)));
      --  else
      --     --  Interrupt_Priority
      --     Set_Priority (+T.Common.LL.Thread, int (MAX_PRIORITY));
      --  end if;
   end Set_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : Task_ID) return System.Any_Priority is
   begin
      return T.Common.Current_Priority;
   end Get_Priority;

   ----------------
   -- Enter_Task --
   ----------------

   procedure Enter_Task (Self_ID : Task_ID) is
   begin
      Self_ID.Common.LL.Thread := +Current_Thread;

      Lock_All_Tasks_List;
      for I in Known_Tasks'Range loop
         if Known_Tasks (I) = null then
            Known_Tasks (I) := Self_ID;
            Self_ID.Known_Tasks_Index := I;
            exit;
         end if;
      end loop;
      Unlock_All_Tasks_List;
   end Enter_Task;

   --------------
   -- New_ATCB --
   --------------

   function New_ATCB (Entry_Num : Task_Entry_Index) return Task_ID is
   begin
      return new Ada_Task_Control_Block (Entry_Num);
   end New_ATCB;

   ----------------------
   --  Initialize_TCB  --
   ----------------------

   procedure Initialize_TCB (Self_ID : Task_ID; Succeeded : out Boolean) is
   begin
      --  Give the task a unique serial number.

      Self_ID.Serial_Number := Next_Serial_Number;
      Next_Serial_Number := Next_Serial_Number + 1;

      pragma Assert (Next_Serial_Number /= 0);

      Self_ID.Common.LL.L := new_Object;
      Succeeded := True;
   end Initialize_TCB;

   -----------------
   -- Create_Task --
   -----------------

   procedure Create_Task
     (T          : Task_ID;
      Wrapper    : System.Address;
      Stack_Size : System.Parameters.Size_Type;
      Priority   : System.Any_Priority;
      Succeeded  : out Boolean)
   is
      Thread : Thread_Id;

      First  : constant System.Priority := System.Priority'First;
      Last   : constant System.Priority := System.Priority'Last;

   begin
      --  We are forced to ignore Stack_Size.

      pragma Assert (T.Common.Activator /= null);

      Thread := System.OS_Interface.new_Thread (To_Address (T));

      --  ????
      --  Consider modifying GNULL to allow passing in the
      --  task name as the thread name.
      --  This is not essential, but might be of some value if there is a
      --  thread-based debugger.

      --  All Ada tasks, except for the environment task, should be
      --  daemons, since Ada enforces its own program termination rules.
      --  Likewise, we cannot use Thread.Join, or Thread_Group.

      Set_Daemon (+Thread, True);

      if Priority <= Last then
         Set_Priority
           (+Thread,
            int (MIN_PRIORITY + (IJL_Thread.MAX_PRIORITY - 1 - MIN_PRIORITY) *
                 (Priority - First) / (Last - First)));
      else
         --  Interrupt_Priority
         Set_Priority (+Thread, int (IJL_Thread.MAX_PRIORITY));
      end if;

      --  Start the newly created thread

      Start (+Thread);

      Succeeded := True;

      --  This presumes that notification of failure of thread creation
      --  raises a Java exception and that is automatically propagated
      --  as an Ada exception.

   exception
      when others =>
         --  We generally do not allow for exception propagation with
         --  the runtime system.

         Succeeded := False;
   end Create_Task;

   ------------------
   -- Finalize_TCB --
   ------------------

   procedure Finalize_TCB (T : Task_ID) is
   begin
      T.Common.LL.Thread := null;

      if T.Known_Tasks_Index /= -1 then
         Known_Tasks (T.Known_Tasks_Index) := null;
      end if;
   end Finalize_TCB;

   ---------------
   -- Exit_Task --
   ---------------

   --  This procedure must be called with abort deferred.
   --  It can no longer call Self or access
   --  the current task's ATCB, since the ATCB has been deallocated.

   procedure Exit_Task is
   begin
      null;
      --  Trust the JVM to stop the thread when it reaches the
      --  end of its body.
   end Exit_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_ID) is
   begin
      pragma Assert (T /= Self);

      --  ????
      --  It is tempting to call Java.Lang.Thread.stop, to raise an
      --  asynchronous exception, here.  However, we don't (yet?) know
      --  of a way to defer the effect of that.  Therefore, we assume
      --  for now that abort will need to be synchronous only.

      null;
   end Abort_Task;

   -------------
   --  Sleep  --
   -------------

   procedure Sleep (Self_ID : Task_ID; Reason  : Task_States) is
   begin
      pragma Assert (Self_ID = Self);

      if Dynamic_Priority_Support
        and then Self_ID.Pending_Priority_Change
      then
         Self_ID.Pending_Priority_Change := False;
         Self_ID.Common.Base_Priority := Self_ID.New_Base_Priority;
         --  if Self_ID.Common.Base_Priority < MIN_PRIORITY then
         --     Set_Priority (+Self_ID.Common.LL.Thread, int (MIN_PRIORITY));
         --  elsif Self_ID.Common.Base_Priority > MAX_PRIORITY then
         --     Set_Priority (+Self_ID.Common.LL.Thread, int (MAX_PRIORITY));
         --  else
         --     Set_Priority (+Self_ID.Common.LL.Thread,
         --       int (Self_ID.Common.Base_Priority));
         --  end if;
      end if;

      Wait (Self_ID.Common.LL.L);
   end Sleep;

   --  Note that we are relying heaviliy here on the GNAT feature
   --  that Calendar.Time, System.Real_Time.Time, Duration, and
   --  System.Real_Time.Time_Span are all represented in the same
   --  way, i.e., as a 64-bit count of nanoseconds.
   --  This allows us to always pass the timeout value as a Duration.

   --  ?????  .........
   --  We are taking liberties here with the semantics of the delays.
   --  That is, we make no distinction between delays on the Calendar clock
   --  and delays on the Real_Time clock.  That is technically incorrect, if
   --  the Calendar clock happens to be reset or adjusted.
   --  To solve this defect will require modification to the compiler
   --  interface, so that it can pass through more information, to tell
   --  us here which clock to use!

   --  cond_timedwait will return if any of the following happens:
   --  1) some other task did cond_signal on this condition variable
   --     In this case, the return value is 0
   --  2) the call just returned, for no good reason
   --     This is called a "spurious wakeup".
   --     In this case, the return value may also be 0.
   --  3) the time delay expires
   --     In this case, the return value is ETIME
   --  4) this task received a signal, which was handled by some
   --     handler procedure, and now the thread is resuming execution
   --     UNIX calls this an "interrupted" system call.
   --     In this case, the return value is EINTR

   --  If the cond_timedwait returns 0 or EINTR, it is still
   --  possible that the time has actually expired, and by chance
   --  a signal or cond_signal occurred at around the same time.

   --  We have also observed that on some OS's the value ETIME
   --  will be returned, but the clock will show that the full delay
   --  has not yet expired.

   --  For these reasons, we need to check the clock after return
   --  from cond_timedwait.  If the time has expired, we will set
   --  Timedout = True.

   --  This check might be omitted for systems on which the
   --  cond_timedwait() never returns early or wakes up spuriously.

   --  Annex D requires that completion of a delay cause the task
   --  to go to the end of its priority queue, regardless of whether
   --  the task actually was suspended by the delay.  Since
   --  cond_timedwait does not do this on Solaris, we add a call
   --  to thr_yield at the end.  We might do this at the beginning,
   --  instead, but then the round-robin effect would not be the
   --  same; the delayed task would be ahead of other tasks of the
   --  same priority that awoke while it was sleeping.

   --  For Timed_Sleep, we are expecting possible cond_signals
   --  to indicate other events (e.g., completion of a RV or
   --  completion of the abortable part of an async. select),
   --  we want to always return if interrupted. The caller will
   --  be responsible for checking the task state to see whether
   --  the wakeup was spurious, and to go back to sleep again
   --  in that case.  We don't need to check for pending abort
   --  or priority change on the way in our out; that is the
   --  caller's responsibility.

   --  For Timed_Delay, we are not expecting any cond_signals or
   --  other interruptions, except for priority changes and aborts.
   --  Therefore, we don't want to return unless the delay has
   --  actually expired, or the call has been aborted.  In this
   --  case, since we want to implement the entire delay statement
   --  semantics, we do need to check for pending abort and priority
   --  changes.  We can quietly handle priority changes inside the
   --  procedure, since there is no entry-queue reordering involved.

   -----------------
   -- Timed_Sleep --
   -----------------

   --  This is for use within the run-time system, so abort is
   --  assumed to be already deferred, and the caller should be
   --  holding its own ATCB lock.
   --  Yielded should be False unles we know for certain that the
   --  operation resulted in the calling task going to the end of
   --  the dispatching queue for its priority.
   --  ?????
   --  This version presumes the worst, so Yielded is always False.
   --  On some targets, if cond_timedwait always yields, we could
   --  set Yielded to True just before the cond_timedwait call.

   procedure Timed_Sleep
     (Self_ID  : Task_ID;
      Time     : Duration;
      Mode     : Delay_Modes;
      Reason   : System.Tasking.Task_States;
      Timedout : out Boolean;
      Yielded  : out Boolean)
   is
      Check_Time  : constant Duration := Clock;
      Abs_Time    : Duration;
      Request_MS  : long;
      Request_NS  : int;

   begin
      pragma Assert (Self_ID = Self);

      Timedout := True;
      Yielded  := False;

      if Mode = Relative then
         Abs_Time := Time + Check_Time;
      else
         Abs_Time := Time;
      end if;

      if Abs_Time > Check_Time then
         if not (Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level
                 or else (Dynamic_Priority_Support
                          and then Self_ID.Pending_Priority_Change)) then
            Split_MS_NS (Abs_Time - Check_Time, Request_MS, Request_NS);
            Wait (Self_ID.Common.LL.L, Request_MS, Request_NS);

            if Abs_Time > Clock then
               Timedout := False;
            end if;
         end if;
      end if;
   end Timed_Sleep;

   -----------------
   -- Timed_Delay --
   -----------------

   --  This is for use in implementing delay statements, so
   --  we assume the caller is abort-deferred but is holding
   --  no locks.

   procedure Timed_Delay
     (Self_ID  : Task_ID;
      Time     : Duration;
      Mode     : Delay_Modes)
   is
      Check_Time  : Duration := Clock;
      Abs_Time    : Duration;
      Request_MS  : long;
      Request_NS  : int;

   begin
      pragma Assert (Self_ID = Self);

      --  Only the little window between deferring abort and
      --  locking Self_ID is the reason we need to
      --  check for pending abort and priority change below! :(

      Write_Lock (Self_ID);

      if Mode = Relative then
         Abs_Time := Time + Check_Time;
      else
         Abs_Time := Time;
      end if;

      if Abs_Time > Check_Time then
         Self_ID.Common.State := Delay_Sleep;

         loop
            if Dynamic_Priority_Support
              and then Self_ID.Pending_Priority_Change
            then
               Self_ID.Pending_Priority_Change := False;
               Self_ID.Common.Base_Priority := Self_ID.New_Base_Priority;
               Set_Priority (Self_ID, Self_ID.Common.Base_Priority);
            end if;

            exit when Self_ID.Pending_ATC_Level < Self_ID.ATC_Nesting_Level;

            Split_MS_NS (Abs_Time - Check_Time, Request_MS, Request_NS);
            Wait (Self_ID.Common.LL.L, Request_MS, Request_NS);

            Check_Time := Clock;
            exit when Abs_Time <= Check_Time;
         end loop;

         Self_ID.Common.State := Runnable;
      end if;

      Unlock (Self_ID);
      Lang.Thread.Yield;
   end Timed_Delay;

   ------------
   -- Wakeup --
   ------------

   procedure Wakeup (T : Task_ID; Reason : Task_States) is
   begin
      Notify (T.Common.LL.L);
   end Wakeup;

   -------------
   --  Checks --
   -------------

   --  Dummy versions. The only currently working versions is for solaris
   --  (native).

   function Check_Exit (Self_ID : Task_ID) return Boolean is
   begin
      return True;
   end Check_Exit;

   function Check_No_Locks (Self_ID : Task_ID) return Boolean is
   begin
      return True;
   end Check_No_Locks;

   ----------------------
   -- Environment_Task --
   ----------------------

   function Environment_Task return Task_ID is
   begin
      return Environment_Task_ID;
   end Environment_Task;

   -------------------------
   -- Lock_All_Tasks_List --
   -------------------------

   procedure Lock_All_Tasks_List is
   begin
      Write_Lock (All_Tasks_L'Access);
   end Lock_All_Tasks_List;

   ---------------------------
   -- Unlock_All_Tasks_List --
   ---------------------------

   procedure Unlock_All_Tasks_List is
   begin
      Unlock (All_Tasks_L'Access);
   end Unlock_All_Tasks_List;

   -------------------
   --  Stack_Guard  --
   -------------------

   --  Trust the JVM for stack checking.

   procedure Stack_Guard (T : ST.Task_ID; On : Boolean) is
   begin
      null;
   end Stack_Guard;

   -------------------
   -- Get_Thread_Id --
   -------------------

   function Get_Thread_Id (T : in System.Tasking.Task_ID) return Thread_Id is
   begin
      return +T.Common.LL.Thread;
   end Get_Thread_Id;

   ------------------
   -- Suspend_Task --
   ------------------

   function Suspend_Task (T : ST.Task_ID; Thread_Self : Thread_Id)
     return Boolean is
   begin
      return False;
   end Suspend_Task;

   -----------------
   -- Resume_Task --
   -----------------

   function Resume_Task (T : ST.Task_ID; Thread_Self : Thread_Id)
     return Boolean is
   begin
      return False;
   end Resume_Task;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : ST.Task_ID) is
   begin
      Environment_Task_ID := Environment_Task;

      Initialize_Lock (All_Tasks_L'Access, All_Tasks_Level);
      --  Initialize the lock used to synchronize chain of all ATCBs

      Enter_Task (Environment_Task_ID);
   end Initialize;

end System.Task_Primitives.Operations;
