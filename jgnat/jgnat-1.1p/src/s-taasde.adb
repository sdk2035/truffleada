------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--           S Y S T E M . T A S K I N G . A S Y N C _ D E L A Y S          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.3 $
--                                                                          --
--           Copyright (C) 1998-2000 Ada Core Technologies, Inc.            --
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

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with Ada.Exceptions;
--  Used for Raise_Exception

with System.Task_Primitives.Operations;
--  Used for Write_Lock,
--           Unlock,
--           Self,
--           Clock,
--           Self,
--           Timed_Sleep,
--           Wakeup,
--           Yield

with System.Tasking.Utilities;
--  Used for Make_Independent

with System.Tasking.Initialization;
--  Used for Defer_Abort
--           Undefer_Abort

with System.Tasking.Debug;
--  Used for Trace

with System.OS_Primitives;
--  used for Max_Sensible_Delay

with Ada.Task_Identification;
--  used for Task_ID type

with Unchecked_Conversion;

package body System.Tasking.Async_Delays is

   package STPO renames System.Task_Primitives.Operations;
   package ST renames System.Tasking;
   package STU renames System.Tasking.Utilities;
   package STI renames System.Tasking.Initialization;
   package OSP renames System.OS_Primitives;

   function To_System is new Unchecked_Conversion
     (Ada.Task_Identification.Task_Id, Task_ID);

   Timer_Server_ID : ST.Task_ID;

   Timer_Attention : Boolean := False;
   pragma Atomic (Timer_Attention);

   task Timer_Server is
      pragma Interrupt_Priority (System.Any_Priority'Last);
   end Timer_Server;

   --  The timer queue is a circular doubly linked list, ordered by absolute
   --  wakeup time. The first item in the queue is Timer_Queue.Succ.
   --  It is given a Resume_Time that is larger than any legitimate wakeup
   --  time, so that the ordered insertion will always stop searching when it
   --  gets back to the queue header block.

   Timer_Queue : aliased Delay_Block;

   ------------------
   -- Time_Enqueue --
   ------------------

   --  Allocate a queue element for the wakeup time T and put it in the
   --  queue in wakeup time order.  Assume we are on an asynchronous
   --  select statement with delay trigger.  Put the calling task to
   --  sleep until either the delay expires or is cancelled.

   --  We use one entry call record for this delay, since we have
   --  to increment the ATC nesting level, but since it is not a
   --  real entry call we do not need to use any of the fields of
   --  the call record.  The following code implements a subset of
   --  the actions for the asynchronous case of Protected_Entry_Call,
   --  much simplified since we know this never blocks, and does not
   --  have the full semantics of a protected entry call.

   procedure Time_Enqueue
     (T : Duration;
      D : Delay_Block_Access)
   is
      Self_Id : constant Task_ID  := STPO.Self;
      Q       : Delay_Block_Access;

      use type ST.Task_ID;
      --  for visibility of operator "="

   begin
      pragma Debug (Debug.Trace (Self_Id, "Async_Delay", 'P'));
      pragma Assert (Self_Id.Deferral_Level = 1,
        "async delay from within abort-deferred region");

      if Self_Id.ATC_Nesting_Level = ATC_Level'Last then
         Ada.Exceptions.Raise_Exception (Storage_Error'Identity,
           "not enough ATC nesting levels");
      end if;

      Self_Id.ATC_Nesting_Level := Self_Id.ATC_Nesting_Level + 1;

      pragma Debug
        (Debug.Trace (Self_Id, "ASD: entered ATC level: " &
         ATC_Level'Image (Self_Id.ATC_Nesting_Level), 'A'));

      D.Level := Self_Id.ATC_Nesting_Level;
      D.Self_Id := Self_Id;
      D.Resume_Time := T;

      STI.Defer_Abort (Self_Id);
      STPO.Write_Lock (Timer_Server_ID);

      --  Previously, there was code here to dynamically create
      --  the Timer_Server task, if one did not already exist.
      --  That code had a timing window that could allow multiple
      --  timer servers to be created. Luckily, the need for
      --  postponing creation of the timer server should now be
      --  gone, since this package will only be linked in if
      --  there are calls to enqueue calls on the timer server.

      --  Insert D in the timer queue, at the position determined
      --  by the wakeup time T.

      Q := Timer_Queue.Succ;

      while Q.Resume_Time < T loop
         Q := Q.Succ;
      end loop;

      --  Q is the block that has Resume_Time equal to or greater than
      --  T. After the insertion we want Q to be the successor of D.

      D.Succ := Q;
      D.Pred := Q.Pred;
      D.Pred.Succ := D;
      Q.Pred := D;

      --  If the new element became the head of the queue,
      --  signal the Timer_Server to wake up.

      if Timer_Queue.Succ = D then
         Timer_Attention := True;
         STPO.Wakeup (Timer_Server_ID, ST.Timer_Server_Sleep);
      end if;

      STPO.Unlock (Timer_Server_ID);
      STI.Undefer_Abort (Self_Id);
   end Time_Enqueue;

   ------------------
   -- Timer_Server --
   ------------------

   task body Timer_Server is
      Next_Wakeup_Time : Duration := Duration'Last;
      Timedout         : Boolean;
      Yielded          : Boolean;
      Now              : Duration;
      Dequeued,
      Tpred,
      Tsucc            : Delay_Block_Access;
      Dequeued_Task    : Task_ID;

      --  Initialize_Timer_Queue returns null, but has critical side-effects
      --  of initializing the timer queue.

   begin
      Timer_Server_ID := STPO.Self;
      STU.Make_Independent;

      --  Initialize the timer queue to empty, and make the wakeup time of the
      --  header node be larger than any real wakeup time we will ever use.

      loop
         STI.Defer_Abort (Timer_Server_ID);
         STPO.Write_Lock (Timer_Server_ID);

         --  The timer server needs to catch pending aborts after finalization
         --  of library packages. If it doesn't poll for it, the server will
         --  sometimes hang.

         if not Timer_Attention then
            Timer_Server_ID.Common.State := ST.Timer_Server_Sleep;

            if Next_Wakeup_Time = Duration'Last then
               Timer_Server_ID.User_State := 1;
               Next_Wakeup_Time := STPO.Clock + OSP.Max_Sensible_Delay;

            else
               Timer_Server_ID.User_State := 2;
            end if;

            STPO.Timed_Sleep
              (Timer_Server_ID, Next_Wakeup_Time,
               OSP.Absolute_RT, ST.Timer_Server_Sleep,
               Timedout, Yielded);
            Timer_Server_ID.Common.State := ST.Runnable;
         end if;

         --  Service all of the wakeup requests on the queue whose times have
         --  been reached, and update Next_Wakeup_Time to next wakeup time
         --  after that (the wakeup time of the head of the queue if any, else
         --  a time far in the future).

         Timer_Server_ID.User_State := 3;
         Timer_Attention := False;

         Now := STPO.Clock;

         while Timer_Queue.Succ.Resume_Time <= Now loop

            --  Dequeue the waiting task from the front of the queue.

            pragma Debug (System.Tasking.Debug.Trace
              ("Timer service: waking up waiting task", 'E'));

            Dequeued := Timer_Queue.Succ;
            Timer_Queue.Succ := Dequeued.Succ;
            Dequeued.Succ.Pred := Dequeued.Pred;
            Dequeued.Succ := Dequeued;
            Dequeued.Pred := Dequeued;

            --  We want to abort the queued task to the level of the async.
            --  select statement with the delay. To do that, we need to lock
            --  the ATCB of that task, but to avoid deadlock we need to release
            --  the lock of the Timer_Server. This leaves a window in which
            --  another task might perform an enqueue or dequeue operation on
            --  the timer queue, but that is OK because we always restart the
            --  next iteration at the head of the queue.

            STPO.Unlock (Timer_Server_ID);
            STPO.Write_Lock (Dequeued.Self_Id);
            Dequeued_Task := Dequeued.Self_Id;
            Dequeued.Timed_Out := True;
            STI.Locked_Abort_To_Level
              (Timer_Server_ID, Dequeued_Task, Dequeued.Level - 1);
            STPO.Unlock (Dequeued_Task);
            STPO.Write_Lock (Timer_Server_ID);
         end loop;

         Next_Wakeup_Time := Timer_Queue.Succ.Resume_Time;

         --  Service returns the Next_Wakeup_Time.
         --  The Next_Wakeup_Time is either an infinity (no delay request)
         --  or the wakeup time of the queue head. This value is used for
         --  an actual delay in this server.

         STPO.Unlock (Timer_Server_ID);
         STI.Undefer_Abort (Timer_Server_ID);
      end loop;
   end Timer_Server;

   ---------------------------
   -- Enqueue_Time_Duration --
   ---------------------------

   function Enqueue_Duration
     (T : in Duration;
      D : Delay_Block_Access) return Boolean is
   begin
      if T <= 0.0 then
         D.Timed_Out := True;
         STPO.Yield;
         return False;

      else
         STI.Defer_Abort (STPO.Self);
         Time_Enqueue
           (STPO.Clock + Duration'Min (T, OSP.Max_Sensible_Delay), D);
         return True;
      end if;
   end Enqueue_Duration;

   ------------------------
   -- Cancel_Async_Delay --
   ------------------------

   --  This should (only) be called from the compiler-generated cleanup routine
   --  for an async. select statement with delay statement as trigger. The
   --  effect should be to remove the delay from the timer queue, and exit one
   --  ATC nesting level.
   --  The usage and logic are similar to Cancel_Protected_Entry_Call, but
   --  simplified because this is not a true entry call.

   procedure Cancel_Async_Delay (D : Delay_Block_Access) is
      Dpred,
      Dsucc : Delay_Block_Access;

   begin
      --  Note that we mark the delay as being cancelled
      --  using a level value that is reserved.

      --  make this operation idempotent

      if D.Level = ATC_Level_Infinity then
         return;
      end if;

      D.Level := ATC_Level_Infinity;

      --  remove self from timer queue

      STI.Defer_Abort_Nestable (D.Self_Id);
      STPO.Write_Lock (Timer_Server_ID);
      Dpred := D.Pred;
      Dsucc := D.Succ;
      Dpred.Succ := Dsucc;
      Dsucc.Pred := Dpred;
      D.Succ := D;
      D.Pred := D;
      STPO.Unlock (Timer_Server_ID);

      --  Note that the above deletion code is required to be
      --  idempotent, since the block may have been dequeued
      --  previously by the Timer_Server.

      --  leave the asynchronous select

      STPO.Write_Lock (D.Self_Id);
      STU.Exit_One_ATC_Level (D.Self_Id);
      STPO.Unlock (D.Self_Id);
      STI.Undefer_Abort_Nestable (D.Self_Id);
   end Cancel_Async_Delay;

   function Timed_Out (D : Delay_Block_Access) return Boolean is
   begin
      return D.Timed_Out;
   end Timed_Out;

begin
   Timer_Queue.Succ := Timer_Queue'Unchecked_Access;
   Timer_Queue.Pred := Timer_Queue'Unchecked_Access;
   Timer_Queue.Resume_Time := Duration'Last;
   Timer_Server_ID := To_System (Timer_Server'Identity);
end System.Tasking.Async_Delays;
