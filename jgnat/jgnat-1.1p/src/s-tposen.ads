------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S .    --
--                          S I N G L E _ E N T R Y                         --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.3 $
--                                                                          --
--             Copyright (C) 1991-1999 Florida State University             --
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

--  This package provides an optimized version of Protected_Objects.Operations
--  and Protected_Objects.Entries making the following assumptions:
--
--  PO have only one entry
--  There is only one caller at a time (No_Entry_Queue)
--  There is no dynamic priority support (No_Dynamic_Priorities)
--  No Abort Statements
--    (No_Abort_Statements, Max_Asynchronous_Select_Nesting => 0)
--  PO are at library level
--  None of the tasks will terminate (no need for finalization)
--
--  This interface is intended to be used in the ravenscar profile, the
--  compiler is responsible for ensuring that the conditions mentioned above
--  are respected, except for the No_Entry_Queue restriction that is checked
--  dynamically in this package, since the check cannot be performed at compile
--  time, and is relatively cheap (see body).
--
--  This package is part of the high level tasking interface used by the
--  compiler to expand Ada 95 tasking constructs into simpler run time calls
--  (aka GNARLI, GNU Ada Run-time Library Interface)
--
--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes
--  in exp_ch9.adb and possibly exp_ch7.adb

package System.Tasking.Protected_Objects.Single_Entry is
   pragma Elaborate_Body;

   ---------------------------------
   -- Compiler Interface (GNARLI) --
   ---------------------------------

   --  The compiler will expand in the GNAT tree the following construct:
   --
   --  protected PO is
   --     entry E;
   --     procedure P;
   --  private
   --     Open : Boolean := False;
   --  end PO;
   --
   --  protected body PO is
   --     entry E when Open is
   --        ...variable declarations...
   --     begin
   --        ...B...
   --     end E;
   --
   --     procedure P is
   --        ...variable declarations...
   --     begin
   --        ...C...
   --     end P;
   --  end PO;
   --
   --  as follows:
   --
   --  protected type poT is
   --     entry e;
   --     procedure p;
   --  private
   --     open : boolean := false;
   --  end poT;
   --  type poTV is limited record
   --     open : boolean := false;
   --     _object : aliased protection_entry;
   --  end record;
   --  procedure poPT__E1s (O : address; P : address; E :
   --    protected_entry_index);
   --  function poPT__B2s (O : address; E : protected_entry_index) return
   --    boolean;
   --  procedure poPT__pN (_object : in out poTV);
   --  procedure poPT__pP (_object : in out poTV);
   --  poTA : aliased entry_body := (
   --     barrier => poPT__B2s'unrestricted_access,
   --     action => poPT__E1s'unrestricted_access);
   --  freeze poTV [
   --     procedure _init_proc (_init : in out poTV) is
   --     begin
   --        _init.open := false;
   --        _init_proc (_init._object);
   --        initialize_protection_entry (_init._object'unchecked_access,
   --          unspecified_priority, _init'address, poTA'
   --          unrestricted_access);
   --        return;
   --     end _init_proc;
   --  ]
   --  po : poT;
   --  _init_proc (poTV!(po));
   --
   --  function poPT__B2s (O : address; E : protected_entry_index) return
   --    boolean is
   --     type poTVP is access poTV;
   --     _object : poTVP := poTVP!(O);
   --     poR : protection_entry renames _object._object;
   --     openP : boolean renames _object.open;
   --  begin
   --     return open;
   --  end poPT__B2s;
   --
   --  procedure poPT__E1s (O : address; P : address; E :
   --    protected_entry_index) is
   --     type poTVP is access poTV;
   --     _object : poTVP := poTVP!(O);
   --  begin
   --     B1b : declare
   --        poR : protection_entry renames _object._object;
   --        openP : boolean renames _object.open;
   --        ...variable declarations...
   --     begin
   --        ...B...
   --     end B1b;
   --     complete_single_entry_body (_object._object'unchecked_access);
   --     return;
   --  exception
   --     when all others =>
   --        exceptional_complete_single_entry_body (_object._object'
   --          unchecked_access, get_gnat_exception);
   --        return;
   --  end poPT__E1s;
   --
   --  procedure poPT__pN (_object : in out poTV) is
   --     poR : protection_entry renames _object._object;
   --     openP : boolean renames _object.open;
   --     ...variable declarations...
   --  begin
   --     ...C...
   --     return;
   --  end poPT__pN;
   --
   --  procedure poPT__pP (_object : in out poTV) is
   --     procedure _clean is
   --     begin
   --        service_entry (_object._object'unchecked_access);
   --        unlock_entry (_object._object'unchecked_access);
   --        return;
   --     end _clean;
   --  begin
   --     lock_entry (_object._object'unchecked_access);
   --     B5b : begin
   --        poPT__pN (_object);
   --     at end
   --        _clean;
   --     end B5b;
   --     return;
   --  end poPT__pP;

   type Protection_Entry is limited private;
   --  This type contains the GNARL state of a protected object. The
   --  application-defined portion of the state (i.e. private objects)
   --  is maintained by the compiler-generated code.

   type Protection_Entry_Access is access all Protection_Entry;

   procedure Initialize_Protection_Entry
     (Object            : Protection_Entry_Access;
      Ceiling_Priority  : Integer;
      Compiler_Info     : System.Address;
      Entry_Body        : Entry_Body_Access);
   --  Initialize the Object parameter so that it can be used by the run time
   --  to keep track of the runtime state of a protected object.

   procedure Lock_Entry (Object : Protection_Entry_Access);
   --  Lock a protected object for write access. Upon return, the caller
   --  owns the lock to this object, and no other call to Lock or
   --  Lock_Read_Only with the same argument will return until the
   --  corresponding call to Unlock has been made by the caller.

   procedure Lock_Read_Only_Entry
     (Object : Protection_Entry_Access);
   --  Lock a protected object for read access.  Upon return, the caller
   --  owns the lock for read access, and no other calls to Lock
   --  with the same argument will return until the corresponding call
   --  to Unlock has been made by the caller.  Other cals to Lock_Read_Only
   --  may (but need not) return before the call to Unlock, and the
   --  corresponding callers will also own the lock for read access.

   procedure Unlock_Entry (Object : Protection_Entry_Access);
   --  Relinquish ownership of the lock for the object represented by
   --  the Object parameter.  If this ownership was for write access, or
   --  if it was for read access where there are no other read access
   --  locks outstanding, one (or more, in the case of Lock_Read_Only)
   --  of the tasks waiting on this lock (if any) will be given the
   --  lock and allowed to return from the Lock or Lock_Read_Only call.

   procedure Service_Entry (Object : Protection_Entry_Access);
   --  Service the entry queue of the specified object, executing the
   --  corresponding body of any queued entry call that is waiting on True
   --  barrier. This is used when the state of a protected object may have
   --  changed, in particular after the execution of the statement sequence of
   --  a protected procedure.
   --  This must be called with abortion deferred and with the corresponding
   --  object locked.

   procedure Protected_Single_Entry_Call
     (Object              : Protection_Entry_Access;
      Uninterpreted_Data  : System.Address;
      Mode                : Call_Modes);
   --  Make a protected entry call to the specified object.
   --  Pend a protected entry call on the protected object represented
   --  by Object. A pended call is not queued; it may be executed immediately
   --  or queued, depending on the state of the entry barrier.
   --
   --    Uninterpreted_Data
   --      This will be returned by Next_Entry_Call when this call is serviced.
   --      It can be used by the compiler to pass information between the
   --      caller and the server, in particular entry parameters.
   --
   --    Mode
   --      The kind of call to be pended

   procedure Timed_Protected_Single_Entry_Call
     (Object                : Protection_Entry_Access;
      Uninterpreted_Data    : System.Address;
      Timeout               : Duration;
      Mode                  : Delay_Modes;
      Entry_Call_Successful : out Boolean);
   --  Same as the Protected_Entry_Call but with time-out specified.
   --  This routine is used to implement timed entry calls.

   procedure Complete_Single_Entry_Body
     (Object : Protection_Entry_Access);
   pragma Inline (Complete_Single_Entry_Body);
   --  Called from within an entry body procedure, indicates that the
   --  corresponding entry call has been serviced.

   procedure Exceptional_Complete_Single_Entry_Body
     (Object : Protection_Entry_Access;
      Ex     : Ada.Exceptions.Exception_Id);
   --  Perform all of the functions of Complete_Entry_Body.  In addition,
   --  report in Ex the exception whose propagation terminated the entry
   --  body to the runtime system.

   function Protected_Count_Entry (Object : Protection_Entry)
     return Natural;
   --  Return the number of entry calls on Object (0 or 1).

   function Protected_Single_Entry_Caller (Object : Protection_Entry)
     return Task_ID;
   --  Return value of E'Caller, where E is the protected entry currently
   --  being handled. This will only work if called from within an
   --  entry body, as required by the LRM (C.7.1(14)).

private
   type Protection_Entry is record
      L                 : aliased Task_Primitives.Lock;
      Compiler_Info     : System.Address;
      Call_In_Progress  : Entry_Call_Link;
      Ceiling           : System.Any_Priority;
      Entry_Body        : Entry_Body_Access;
      Entry_Queue       : Entry_Call_Link;
   end record;
   pragma Volatile (Protection_Entry);
   for Protection_Entry'Alignment use Standard'Maximum_Alignment;
   --  Use maximum alignement so that one can convert a protection_entry_access
   --  to a task_id.

end System.Tasking.Protected_Objects.Single_Entry;
