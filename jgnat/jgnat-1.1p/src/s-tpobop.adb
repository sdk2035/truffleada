------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--    S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S .     --
--                            O P E R A T I O N S                           --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.11 $
--                                                                          --
--            Copyright (C) 1991-1999, Florida State University             --
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

--  This package contains all the extended primitives related to
--  Protected_Objects with entries.
--  The handling of protected objects with no entries is done in
--  System.Tasking.Protected_Objects, the simple routines for protected
--  objects with entries in System.Tasking.Protected_Objects.Entries.
--  The split between Entries and Operations is needed to break circular
--  dependencies inside the run time.

--  This package contains all primitives related to Protected_Objects.
--  Note: the compiler generates direct calls to this interface, via Rtsfind.

with Ada.Exceptions;
--  Used for Exception_ID
--           Null_Id
--           Raise_Exception

with System.Task_Primitives.Operations;
--  used for Initialize_Lock
--           Write_Lock
--           Unlock
--           Get_Priority
--           Wakeup

with System.Tasking.Entry_Calls;
--  used for Wait_For_Completion
--           Wait_Until_Abortable

with System.Tasking.Initialization;
--  Used for Defer_Abort,
--           Undefer_Abort,
--           Change_Base_Priority

pragma Elaborate_All (System.Tasking.Initialization);
--  This insures that tasking is initialized if any protected objects are
--  created.

with System.Tasking.Queuing;
--  used for Enqueue
--           Broadcast_Program_Error
--           Select_Protected_Entry_Call
--           Onqueue
--           Count_Waiting

with System.Tasking.Rendezvous;
--  used for Task_Do_Or_Queue

with System.Tasking.Debug;
--  used for Trace

package body System.Tasking.Protected_Objects.Operations is

   package STPO renames System.Task_Primitives.Operations;

   use Task_Primitives;
   use Tasking;
   use Ada.Exceptions;
   use Entries;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Update_For_Queue_To_PO
     (Entry_Call : Entry_Call_Link;
      With_Abort : Boolean);
   pragma Inline (Update_For_Queue_To_PO);
   --  Update the state of an existing entry call to reflect
   --  the fact that it is being enqueued, based on
   --  whether the current queuing action is with or without abort.
   --  Call this only while holding the PO's lock.
   --  It returns with the PO's lock still held.

   --------------
   -- Enqueued --
   --------------

   function Enqueued (Block : Communication_Block) return Boolean is
   begin
      return Block.Enqueued;
   end Enqueued;

   ---------------------------------
   -- Cancel_Protected_Entry_Call --
   ---------------------------------

   --  Compiler interface only.  Do not call from within the RTS.
   --  This should have analogous effect to Cancel_Task_Entry_Call,
   --  setting the value of Block.Cancelled instead of returning
   --  the parameter value Cancelled.

   --  The effect should be idempotent, since the call may already
   --  have been dequeued.

   --  source code:

   --      select r.e;
   --         ...A...
   --      then abort
   --         ...B...
   --      end select;

   --  expanded code:

   --      declare
   --         X : protected_entry_index := 1;
   --         B80b : communication_block;
   --         _init_proc (B80b);
   --      begin
   --         begin
   --            A79b : label
   --            A79b : declare
   --               procedure _clean is
   --               begin
   --                  if enqueued (B80b) then
   --                     cancel_protected_entry_call (B80b);
   --                  end if;
   --                  return;
   --               end _clean;
   --            begin
   --               protected_entry_call (rTV!(r)._object'unchecked_access, X,
   --                 null_address, asynchronous_call, B80b, objectF => 0);
   --               if enqueued (B80b) then
   --                  ...B...
   --               end if;
   --            at end
   --               _clean;
   --            end A79b;
   --         exception
   --            when _abort_signal =>
   --               abort_undefer.all;
   --               null;
   --         end;
   --         if not cancelled (B80b) then
   --            x := ...A...
   --         end if;
   --      end;

   --  If the entry call completes after we get into the abortable part,
   --  Abort_Signal should be raised and ATC will take us to the at-end
   --  handler, which will call _clean.

   --  If the entry call returns with the call already completed,
   --  we can skip this, and use the "if enqueued()" to go past
   --  the at-end handler, but we will still call _clean.

   --  If the abortable part completes before the entry call is Done,
   --  it will call _clean.

   --  If the entry call or the abortable part raises an exception,
   --  we will still call _clean, but the value of Cancelled should not matter.

   --  Whoever calls _clean first gets to decide whether the call
   --  has been "cancelled".

   --  Enqueued should be true if there is any chance that the call
   --  is still on a queue.  It seems to be safe to make it True if
   --  the call was Onqueue at some point before return from
   --  Protected_Entry_Call.

   --  Cancelled should be true iff the abortable part completed
   --  and succeeded in cancelling the entry call before it completed.

   --  ?????
   --  The need for Enqueued is less obvious.
   --  The  "if enqueued()" tests are not necessary, since both
   --  Cancel_Protected_Entry_Call and Protected_Entry_Call must
   --  do the same test internally, with locking.  The one that
   --  makes cancellation conditional may be a useful heuristic
   --  since at least 1/2 the time the call should be off-queue
   --  by that point.  The other one seems totally useless, since
   --  Protected_Entry_Call must do the same check and then
   --  possibly wait for the call to be abortable, internally.

   --  We can check Call.State here without locking the caller's mutex,
   --  since the call must be over after returning from Wait_For_Completion.
   --  No other task can access the call record at this point.

   procedure Cancel_Protected_Entry_Call
     (Block : in out Communication_Block)
   is
   begin
      Entry_Calls.Try_To_Cancel_Entry_Call (Block.Cancelled);
   end Cancel_Protected_Entry_Call;

   ---------------
   -- Cancelled --
   ---------------

   function Cancelled (Block : Communication_Block) return Boolean is
   begin
      return Block.Cancelled;
   end Cancelled;

   --------------------
   -- PO_Do_Or_Queue --
   --------------------

   procedure PO_Do_Or_Queue
     (Self_ID    : Task_ID;
      Object     : Protection_Entries_Access;
      Entry_Call : Entry_Call_Link;
      With_Abort : Boolean)
   is
      E : Protected_Entry_Index := Protected_Entry_Index (Entry_Call.E);
      New_Object        : Protection_Entries_Access;
      Ceiling_Violation : Boolean;
      Barrier_Value     : Boolean;

   begin
      --  When the Action procedure for an entry body returns, it is either
      --  completed (having called [Exceptional_]Complete_Entry_Body) or it
      --  is queued, having executed a requeue statement.

      Barrier_Value :=
        Object.Entry_Bodies (
          Object.Find_Body_Index (Object.Compiler_Info, E)).
            Barrier (Object.Compiler_Info, E);

      if Barrier_Value then

         --  Not abortable while service is in progress.

         if Entry_Call.State = Now_Abortable then
            Entry_Call.State := Was_Abortable;
         end if;

         Object.Call_In_Progress := Entry_Call;

         pragma Debug
          (Debug.Trace (Self_ID, "PODOQ: start entry body", 'P'));
         Object.Entry_Bodies (
           Object.Find_Body_Index (Object.Compiler_Info, E)).Action (
             Object.Compiler_Info, Entry_Call.Uninterpreted_Data, E);

         if Object.Call_In_Progress /= null then

            --  Body of current entry served call to completion

            Object.Call_In_Progress := null;
            Initialization.Wakeup_Entry_Caller (Self_ID, Entry_Call, Done);

         else
            --  Body of current entry requeued the call
            New_Object := To_Protection (Entry_Call.Called_PO);

            if New_Object = null then

               --  Call was requeued to a task

               if not Rendezvous.Task_Do_Or_Queue
                 (Self_ID, Entry_Call,
                  With_Abort => Entry_Call.Requeue_With_Abort)
               then
                  Queuing.Broadcast_Program_Error
                   (Self_ID, Object, Entry_Call);
               end if;
               return;
            end if;

            if Object /= New_Object then
               --  Requeue is on a different object

               STPO.Write_Lock (New_Object.L'Access, Ceiling_Violation);

               if Ceiling_Violation then
                  Object.Call_In_Progress := null;
                  Queuing.Broadcast_Program_Error
                   (Self_ID, Object, Entry_Call);

               else
                  PO_Do_Or_Queue (Self_ID, New_Object, Entry_Call, With_Abort);
                  PO_Service_Entries (Self_ID, New_Object);
                  Unlock_Entries (New_Object);
               end if;

            else
               --  Requeue is on same protected object

               if Entry_Call.Requeue_With_Abort
                 and then Entry_Call.Cancellation_Attempted
               then
                  --  If this is a requeue with abort and someone tried
                  --  to cancel this call, cancel it at this point.

                  Entry_Call.State := Cancelled;
                  return;
               end if;

               if not With_Abort or else
                 Entry_Call.Mode /= Conditional_Call
               then
                  E := Protected_Entry_Index (Entry_Call.E);
                  Queuing.Enqueue
                    (New_Object.Entry_Queues (E), Entry_Call);
                  Update_For_Queue_To_PO (Entry_Call, With_Abort);

               else
                  --  ?????
                  --  Can we convert this recursion to a loop?

                  PO_Do_Or_Queue (Self_ID, New_Object, Entry_Call, With_Abort);
               end if;
            end if;
         end if;

      elsif Entry_Call.Mode /= Conditional_Call or else
        not With_Abort then
         Queuing.Enqueue (Object.Entry_Queues (E), Entry_Call);
         Update_For_Queue_To_PO (Entry_Call, With_Abort);

      else
         --  Conditional_Call and With_Abort

         STPO.Write_Lock (Entry_Call.Self);
         pragma Assert (Entry_Call.State >= Was_Abortable);
         Initialization.Wakeup_Entry_Caller (Self_ID, Entry_Call, Cancelled);
         STPO.Unlock (Entry_Call.Self);
      end if;

   exception
      when others =>
         Queuing.Broadcast_Program_Error (Self_ID, Object, Entry_Call);
   end PO_Do_Or_Queue;

   ---------------------
   -- Service_Entries --
   ---------------------

   procedure Service_Entries (Object : Protection_Entries_Access) is
      Self_ID : constant Task_ID := STPO.Self;
   begin
      PO_Service_Entries (Self_ID, Object);
   end Service_Entries;

   ------------------------
   -- PO_Service_Entries --
   ------------------------

   procedure PO_Service_Entries
     (Self_ID : Task_ID;
      Object : Protection_Entries_Access)
   is
      Entry_Call : Entry_Call_Link;
      E          : Protected_Entry_Index;
      Caller     : Task_ID;
      New_Object : Protection_Entries_Access;
      Ceiling_Violation : Boolean;

   begin
      loop
         Queuing.Select_Protected_Entry_Call (Self_ID, Object, Entry_Call);

         if Entry_Call /= null then
            E := Protected_Entry_Index (Entry_Call.E);

            --  Not abortable while service is in progress.

            if Entry_Call.State = Now_Abortable then
               Entry_Call.State := Was_Abortable;
            end if;

            Object.Call_In_Progress := Entry_Call;

            begin
               pragma Debug
                (Debug.Trace (Self_ID, "POSE: start entry body", 'P'));
               Object.Entry_Bodies (
                 Object.Find_Body_Index (Object.Compiler_Info, E)).Action (
                   Object.Compiler_Info, Entry_Call.Uninterpreted_Data, E);
            exception
               when others =>
                  Queuing.Broadcast_Program_Error
                    (Self_ID, Object, Entry_Call);
            end;

            if Object.Call_In_Progress /= null then
               Object.Call_In_Progress := null;
               Caller := Entry_Call.Self;
               STPO.Write_Lock (Caller);
               Initialization.Wakeup_Entry_Caller (Self_ID, Entry_Call, Done);
               STPO.Unlock (Caller);

            else
               --  Call needs to be requeued

               New_Object := To_Protection (Entry_Call.Called_PO);

               if New_Object = null then

                  --  Call is to be requeued to a task entry

                  if not Rendezvous.Task_Do_Or_Queue
                    (Self_ID, Entry_Call,
                     With_Abort => Entry_Call.Requeue_With_Abort)
                  then
                     Queuing.Broadcast_Program_Error
                      (Self_ID, Object, Entry_Call);
                  end if;

               else
                  --  Call should be requeued to a PO

                  if Object /= New_Object then
                     --  Requeue is to different PO

                     STPO.Write_Lock (New_Object.L'Access, Ceiling_Violation);

                     if Ceiling_Violation then
                        Object.Call_In_Progress := null;
                        Queuing.Broadcast_Program_Error
                          (Self_ID, Object, Entry_Call);

                     else
                        PO_Do_Or_Queue (Self_ID, New_Object, Entry_Call,
                          Entry_Call.Requeue_With_Abort);
                        PO_Service_Entries (Self_ID, New_Object);
                        Unlock_Entries (New_Object);
                     end if;

                  else
                     --  Requeue is to same protected object

                     --  ??? Try to compensate apparent failure of the
                     --  scheduler on some OS (e.g VxWorks) to give higher
                     --  priority tasks a chance to run (see CXD6002).

                     STPO.Yield (False);

                     if Entry_Call.Requeue_With_Abort
                       and then Entry_Call.Cancellation_Attempted
                     then
                        --  If this is a requeue with abort and someone tried
                        --  to cancel this call, cancel it at this point.

                        Entry_Call.State := Cancelled;
                        exit;
                     end if;

                     if not Entry_Call.Requeue_With_Abort or else
                       Entry_Call.Mode /= Conditional_Call
                     then
                        E := Protected_Entry_Index (Entry_Call.E);
                        Queuing.Enqueue
                          (New_Object.Entry_Queues (E), Entry_Call);
                        Update_For_Queue_To_PO (Entry_Call,
                          Entry_Call.Requeue_With_Abort);

                     else
                        PO_Do_Or_Queue (Self_ID, New_Object, Entry_Call,
                          Entry_Call.Requeue_With_Abort);
                     end if;
                  end if;
               end if;
            end if;

         else
            exit;
         end if;
      end loop;
   end PO_Service_Entries;

   --------------------------
   -- Protected_Entry_Call --
   --------------------------

   --  Compiler interface only.  Do not call from within the RTS.

   --  select r.e;
   --     ...A...
   --  else
   --     ...B...
   --  end select;

   --  declare
   --     X : protected_entry_index := 1;
   --     B85b : communication_block;
   --     _init_proc (B85b);
   --  begin
   --     protected_entry_call (rTV!(r)._object'unchecked_access, X,
   --       null_address, conditional_call, B85b, objectF => 0);
   --     if cancelled (B85b) then
   --        ...B...
   --     else
   --        ...A...
   --     end if;
   --  end;

   --  See also Cancel_Protected_Entry_Call for code expansion of
   --  asynchronous entry call.

   --  The initial part of this procedure does not need to lock the
   --  the calling task's ATCB, up to the point where the call record
   --  first may be queued (PO_Do_Or_Queue), since before that no
   --  other task will have access to the record.

   --  If this is a call made inside of an abort deferred region,
   --  the call should be never abortable.

   --  If the call was not queued abortably, we need to wait
   --  until it is before proceeding with the abortable part.

   --  There are some heuristics here, just to save time for
   --  frequently occurring cases.  For example, we check
   --  Initially_Abortable to try to avoid calling the procedure
   --  Wait_Until_Abortable, since the normal case for async.
   --  entry calls is to be queued abortably.

   --  Another heuristic uses the Block.Enqueued to try to avoid
   --  calling Cancel_Protected_Entry_Call if the call can be
   --  served immediately.

   procedure Protected_Entry_Call
     (Object              : Protection_Entries_Access;
      E                   : Protected_Entry_Index;
      Uninterpreted_Data  : System.Address;
      Mode                : Call_Modes;
      Block               : out Communication_Block)
   is
      Self_ID             : Task_ID  := STPO.Self;
      Entry_Call          : Entry_Call_Link;
      Initially_Abortable : Boolean;
      Ceiling_Violation   : Boolean;

   begin
      pragma Debug
        (Debug.Trace (Self_ID, "Protected_Entry_Call", 'P'));

      if Self_ID.ATC_Nesting_Level = ATC_Level'Last then
         Raise_Exception (Storage_Error'Identity,
           "not enough ATC nesting levels");
      end if;

      Initialization.Defer_Abort (Self_ID);
      STPO.Write_Lock (Object.L'Access, Ceiling_Violation);

      if Ceiling_Violation then

         --  Failed ceiling check

         Initialization.Undefer_Abort (Self_ID);
         raise Program_Error;
      end if;

      Block.Self := Self_ID;
      Self_ID.ATC_Nesting_Level := Self_ID.ATC_Nesting_Level + 1;
      pragma Debug
        (Debug.Trace (Self_ID, "PEC: entered ATC level: " &
         ATC_Level'Image (Self_ID.ATC_Nesting_Level), 'A'));
      Entry_Call :=
         Self_ID.Entry_Calls (Self_ID.ATC_Nesting_Level)'Access;
      Entry_Call.Next := null;
      Entry_Call.Mode := Mode;
      Entry_Call.Cancellation_Attempted := False;

      if Self_ID.Deferral_Level > 1 then
         Entry_Call.State := Never_Abortable;
      else
         Entry_Call.State := Now_Abortable;
      end if;

      Entry_Call.E := Entry_Index (E);
      Entry_Call.Prio := STPO.Get_Priority (Self_ID);
      Entry_Call.Uninterpreted_Data := Uninterpreted_Data;
      Entry_Call.Called_PO := To_Address (Object);
      Entry_Call.Called_Task := null;
      Entry_Call.Exception_To_Raise := Ada.Exceptions.Null_Id;

      PO_Do_Or_Queue (Self_ID, Object, Entry_Call, With_Abort => True);
      Initially_Abortable := Entry_Call.State = Now_Abortable;
      PO_Service_Entries (Self_ID, Object);

      Unlock_Entries (Object);

      --  Try to prevent waiting later (in Cancel_Protected_Entry_Call)
      --  for completed or cancelled calls.  (This is a heuristic, only.)

      if Entry_Call.State >= Done then

         --  Once State >= Done it will not change any more.

         Self_ID.ATC_Nesting_Level := Self_ID.ATC_Nesting_Level - 1;
         pragma Debug
           (Debug.Trace (Self_ID, "PEC: exited to ATC level: " &
            ATC_Level'Image (Self_ID.ATC_Nesting_Level), 'A'));
         Block.Enqueued := False;
         Block.Cancelled := Entry_Call.State = Cancelled;
         Initialization.Undefer_Abort (Self_ID);
         Entry_Calls.Check_Exception (Self_ID, Entry_Call);
         return;

      else
         --  In this case we cannot conclude anything,
         --  since State can change concurrently.
         null;
      end if;

      --  Now for the general case.

      if Mode = Asynchronous_Call then

         --  Try to avoid an expensive call.

         if not Initially_Abortable then
            Entry_Calls.Wait_Until_Abortable (Self_ID, Entry_Call);
         end if;

      elsif Mode < Asynchronous_Call then

         --  Simple_Call or Conditional_Call

         STPO.Write_Lock (Self_ID);
         Entry_Calls.Wait_For_Completion (Self_ID, Entry_Call);
         STPO.Unlock (Self_ID);
         Block.Cancelled := Entry_Call.State = Cancelled;

      else
         pragma Assert (False);
         null;
      end if;

      Initialization.Undefer_Abort (Self_ID);
      Entry_Calls.Check_Exception (Self_ID, Entry_Call);

   end Protected_Entry_Call;

   --------------------------------
   -- Timed_Protected_Entry_Call --
   --------------------------------

   --  Compiler interface only.  Do not call from within the RTS.

   procedure Timed_Protected_Entry_Call
     (Object                : Protection_Entries_Access;
      E                     : Protected_Entry_Index;
      Uninterpreted_Data    : System.Address;
      Timeout               : Duration;
      Mode                  : Delay_Modes;
      Entry_Call_Successful : out Boolean)
   is
      Self_ID           : Task_ID  := STPO.Self;
      Entry_Call        : Entry_Call_Link;
      Ceiling_Violation : Boolean;

   begin
      if Self_ID.ATC_Nesting_Level = ATC_Level'Last then
         Raise_Exception (Storage_Error'Identity,
           "not enough ATC nesting levels");
      end if;

      Initialization.Defer_Abort (Self_ID);
      STPO.Write_Lock (Object.L'Access, Ceiling_Violation);

      if Ceiling_Violation then
         Initialization.Undefer_Abort (Self_ID);
         raise Program_Error;
      end if;

      Self_ID.ATC_Nesting_Level := Self_ID.ATC_Nesting_Level + 1;
      pragma Debug
        (Debug.Trace (Self_ID, "TPEC: exited to ATC level: " &
         ATC_Level'Image (Self_ID.ATC_Nesting_Level), 'A'));
      Entry_Call :=
        Self_ID.Entry_Calls (Self_ID.ATC_Nesting_Level)'Access;
      Entry_Call.Next := null;
      Entry_Call.Mode := Timed_Call;
      Entry_Call.Cancellation_Attempted := False;

      if Self_ID.Deferral_Level > 1 then
         Entry_Call.State := Never_Abortable;
      else
         Entry_Call.State := Now_Abortable;
      end if;

      Entry_Call.E := Entry_Index (E);
      Entry_Call.Prio := STPO.Get_Priority (Self_ID);
      Entry_Call.Uninterpreted_Data := Uninterpreted_Data;
      Entry_Call.Called_PO := To_Address (Object);
      Entry_Call.Called_Task := null;
      Entry_Call.Exception_To_Raise := Ada.Exceptions.Null_Id;

      PO_Do_Or_Queue (Self_ID, Object, Entry_Call, With_Abort => True);
      PO_Service_Entries (Self_ID, Object);

      Unlock_Entries (Object);

      --  Try to avoid waiting for completed or cancelled calls.

      if Entry_Call.State >= Done then
         Self_ID.ATC_Nesting_Level := Self_ID.ATC_Nesting_Level - 1;
         pragma Debug
           (Debug.Trace (Self_ID, "TPEC: exited to ATC level: " &
            ATC_Level'Image (Self_ID.ATC_Nesting_Level), 'A'));
         Entry_Call_Successful := Entry_Call.State = Done;
         Initialization.Undefer_Abort (Self_ID);
         Entry_Calls.Check_Exception (Self_ID, Entry_Call);
         return;
      end if;

      Entry_Calls.Wait_For_Completion_With_Timeout
        (Self_ID, Entry_Call, Timeout, Mode);
      Initialization.Undefer_Abort (Self_ID);
      Entry_Call_Successful := Entry_Call.State = Done;
      Entry_Calls.Check_Exception (Self_ID, Entry_Call);
   end Timed_Protected_Entry_Call;

   -------------------------
   -- Complete_Entry_Body --
   -------------------------

   procedure Complete_Entry_Body (Object : Protection_Entries_Access) is
   begin
      Exceptional_Complete_Entry_Body (Object, Ada.Exceptions.Null_Id);
   end Complete_Entry_Body;

   -------------------------------------
   -- Exceptional_Complete_Entry_Body --
   -------------------------------------

   procedure Exceptional_Complete_Entry_Body
     (Object : Protection_Entries_Access;
      Ex     : Ada.Exceptions.Exception_Id)
   is
      Entry_Call : constant Entry_Call_Link := Object.Call_In_Progress;

   begin
      pragma Debug
       (Debug.Trace (STPO.Self, "Exceptional_Complete_Entry_Body", 'P'));

      --  We must have abort deferred, since we are inside
      --  a protected operation.

      if Entry_Call /= null then

         --  The call was not requeued.

         Entry_Call.Exception_To_Raise := Ex;

--  ?????
--  The caller should do the following, after return from this
--  procedure, if Call_In_Progress /= null
--       Write_Lock (Entry_Call.Self);
--       Initialization.Wakeup_Entry_Caller (STPO.Self, Entry_Call, Done);
--       Unlock (Entry_Call.Self);

      end if;
   end Exceptional_Complete_Entry_Body;

   -----------------------------
   -- Requeue_Protected_Entry --
   -----------------------------

   --  Compiler interface only.  Do not call from within the RTS.

   --  entry e when b is
   --  begin
   --     b := false;
   --     ...A...
   --     requeue e2;
   --  end e;

   --  procedure rPT__E10b (O : address; P : address; E :
   --    protected_entry_index) is
   --     type rTVP is access rTV;
   --     freeze rTVP []
   --     _object : rTVP := rTVP!(O);
   --  begin
   --     declare
   --        rR : protection renames _object._object;
   --        vP : integer renames _object.v;
   --        bP : boolean renames _object.b;
   --     begin
   --        b := false;
   --        ...A...
   --        requeue_protected_entry (rR'unchecked_access, rR'
   --          unchecked_access, 2, false, objectF => 0, new_objectF =>
   --          0);
   --        return;
   --     end;
   --     complete_entry_body (_object._object'unchecked_access, objectF =>
   --       0);
   --     return;
   --  exception
   --     when others =>
   --        abort_undefer.all;
   --        exceptional_complete_entry_body (_object._object'
   --          unchecked_access, current_exception, objectF => 0);
   --        return;
   --  end rPT__E10b;

   procedure Requeue_Protected_Entry
     (Object     : Protection_Entries_Access;
      New_Object : Protection_Entries_Access;
      E          : Protected_Entry_Index;
      With_Abort : Boolean)
   is
      Entry_Call : constant Entry_Call_Link := Object.Call_In_Progress;

   begin
      pragma Debug
        (Debug.Trace (STPO.Self, "Requeue_Protected_Entry", 'P'));
      pragma Assert (STPO.Self.Deferral_Level > 0);

      Entry_Call.E := Entry_Index (E);
      Entry_Call.Called_PO := To_Address (New_Object);
      Entry_Call.Called_Task := null;
      Entry_Call.Requeue_With_Abort := With_Abort;
      Object.Call_In_Progress := null;
   end Requeue_Protected_Entry;

   -------------------------------------
   -- Requeue_Task_To_Protected_Entry --
   -------------------------------------

   --  Compiler interface only.

   --    accept e1 do
   --      ...A...
   --      requeue r.e2;
   --    end e1;

   --    A79b : address;
   --    L78b : label
   --    begin
   --       accept_call (1, A79b);
   --       ...A...
   --       requeue_task_to_protected_entry (rTV!(r)._object'
   --         unchecked_access, 2, false, new_objectF => 0);
   --       goto L78b;
   --       <<L78b>>
   --       complete_rendezvous;
   --    exception
   --       when all others =>
   --          exceptional_complete_rendezvous (get_gnat_exception);
   --    end;

   procedure Requeue_Task_To_Protected_Entry
     (New_Object : Protection_Entries_Access;
      E          : Protected_Entry_Index;
      With_Abort : Boolean)
   is
      Self_ID       : constant Task_ID := STPO.Self;
      Entry_Call    : constant Entry_Call_Link := Self_ID.Common.Call;

   begin
      Initialization.Defer_Abort (Self_ID);
      STPO.Write_Lock (Self_ID);
      Entry_Call.Needs_Requeue := True;
      Entry_Call.Requeue_With_Abort := With_Abort;
      Entry_Call.Called_PO := To_Address (New_Object);
      Entry_Call.Called_Task := null;
      STPO.Unlock (Self_ID);
      Entry_Call.E := Entry_Index (E);
      Initialization.Undefer_Abort (Self_ID);
   end Requeue_Task_To_Protected_Entry;

   --  ??????
   --  Do we really need to lock Self_ID above?
   --  Might the caller be trying to cancel?
   --  If so, it should fail, since the call state should not be
   --  abortable while the call is in service.

   ---------------------
   -- Protected_Count --
   ---------------------

   function Protected_Count
     (Object : Protection_Entries'Class;
      E      : Protected_Entry_Index)
      return   Natural
   is
   begin
      return Queuing.Count_Waiting (Object.Entry_Queues (E));
   end Protected_Count;

   ----------------------------
   -- Protected_Entry_Caller --
   ----------------------------

   function Protected_Entry_Caller (Object : Protection_Entries'Class)
     return Task_ID is
   begin
      return Object.Call_In_Progress.Self;
   end Protected_Entry_Caller;

   ----------------------------
   -- Update_For_Queue_To_PO --
   ----------------------------

   --  Update the state of an existing entry call, based on
   --  whether the current queuing action is with or without abort.
   --  Call this only while holding the server's lock.
   --  It returns with the server's lock released.

   New_State : constant array (Boolean, Entry_Call_State)
     of Entry_Call_State :=
       (True =>
         (Never_Abortable   => Never_Abortable,
          Not_Yet_Abortable => Now_Abortable,
          Was_Abortable     => Now_Abortable,
          Now_Abortable     => Now_Abortable,
          Done              => Done,
          Cancelled         => Cancelled),
        False =>
         (Never_Abortable   => Never_Abortable,
          Not_Yet_Abortable => Not_Yet_Abortable,
          Was_Abortable     => Was_Abortable,
          Now_Abortable     => Now_Abortable,
          Done              => Done,
          Cancelled         => Cancelled)
       );

   procedure Update_For_Queue_To_PO
     (Entry_Call : Entry_Call_Link;
      With_Abort : Boolean)
   is
      Old : Entry_Call_State := Entry_Call.State;

   begin
      pragma Assert (Old < Done);

      Entry_Call.State := New_State (With_Abort, Entry_Call.State);

      if Entry_Call.Mode = Asynchronous_Call then
         if Old < Was_Abortable and then
           Entry_Call.State = Now_Abortable
         then
            STPO.Write_Lock (Entry_Call.Self);

            if Entry_Call.Self.Common.State = Async_Select_Sleep then
               STPO.Wakeup (Entry_Call.Self, Async_Select_Sleep);
            end if;

            STPO.Unlock (Entry_Call.Self);
         end if;

      elsif Entry_Call.Mode = Conditional_Call then
         pragma Assert (Entry_Call.State < Was_Abortable);
         null;
      end if;
   end Update_For_Queue_To_PO;

end System.Tasking.Protected_Objects.Operations;
