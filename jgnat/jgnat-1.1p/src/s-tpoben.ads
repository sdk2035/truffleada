------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--     S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S .    --
--                               E N T R I E S                              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.8 $
--                                                                          --
--          Copyright (C) 1992-1999, Free Software Foundation, Inc.         --
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

--  This package contains all the simple primitives related to
--  Protected_Objects with entries (i.e init, lock, unlock).
--  The handling of protected objects with no entries is done in
--  System.Tasking.Protected_Objects, the complex routines for protected
--  objects with entries in System.Tasking.Protected_Objects.Operations.
--  The split between Entries and Operations is needed to break circular
--  dependencies inside the run time.

--  Note: the compiler generates direct calls to this interface, via Rtsfind.
--  Any changes to this interface may require corresponding compiler changes.

with Ada.Finalization;
--  used for Limited_Controlled

with Unchecked_Conversion;

package System.Tasking.Protected_Objects.Entries is
   pragma Elaborate_Body;

   subtype Positive_Protected_Entry_Index is
     Protected_Entry_Index range  1 .. Protected_Entry_Index'Last;

   type Find_Body_Index_Access is access
     function
       (O : System.Address;
        E : Protected_Entry_Index)
        return Protected_Entry_Index;

   type Protected_Entry_Body_Array is
     array (Positive_Protected_Entry_Index range <>) of Entry_Body;
   --  This is an array of the executable code for all entry bodies of
   --  a protected type.

   type Protected_Entry_Body_Access is access all Protected_Entry_Body_Array;

   type Protected_Entry_Queue_Array is
     array (Protected_Entry_Index range <>) of Entry_Queue;

   --  This type contains the GNARL state of a protected object. The
   --  application-defined portion of the state (i.e. private objects)
   --  is maintained by the compiler-generated code.
   --  note that there is a simplified version of this type declared in
   --  System.Tasking.PO_Simple that handle the simple case (no entries).

   type Protection_Entries (Num_Entries : Protected_Entry_Index) is new
     Ada.Finalization.Limited_Controlled
   with record
      L                 : aliased Task_Primitives.Lock;
      Compiler_Info     : System.Address;
      Call_In_Progress  : Entry_Call_Link;
      Ceiling           : System.Any_Priority;
      Old_Base_Priority : System.Any_Priority;
      Pending_Action    : Boolean;
      --  Flag indicating that priority has been dipped temporarily
      --  in order to avoid violating the priority ceiling of the lock
      --  associated with this protected object, in Lock_Server.
      --  The flag tells Unlock_Server or Unlock_And_Update_Server to
      --  restore the old priority to Old_Base_Priority. This is needed
      --  because of situations (bad language design?) where one
      --  needs to lock a PO but to do so would violate the priority
      --  ceiling.  For example, this can happen when an entry call
      --  has been requeued to a lower-priority object, and the caller
      --  then tries to cancel the call while its own priority is higher
      --  than the ceiling of the new PO.

      Entry_Bodies      : Protected_Entry_Body_Access;

      --  The following function maps the entry index in a call (which denotes
      --  the queue to the proper entry) into the body of the entry.

      Find_Body_Index   : Find_Body_Index_Access;
      Entry_Queues      : Protected_Entry_Queue_Array (1 .. Num_Entries);
   end record;
   pragma Volatile (Protection_Entries);

   --  No default initial values for this type, since call records
   --  will need to be re-initialized before every use.

   type Protection_Entries_Access is access all Protection_Entries'Class;
   --  See comments in s-tassta.adb about the implicit call to Current_Master
   --  generated by this declaration.

   function To_Protection_Entries is new Unchecked_Conversion
     (Protection_Access, Protection_Entries_Access);

   function To_Address is
     new Unchecked_Conversion (Protection_Entries_Access, System.Address);
   function To_Protection is
     new Unchecked_Conversion (System.Address, Protection_Entries_Access);

   function Has_Interrupt_Or_Attach_Handler
     (Object : Protection_Entries_Access) return Boolean;
   --  Returns True if an Interrupt_Handler or Attach_Handler pragma applies
   --  to the protected object. That is to say this primitive returns False for
   --  Protection, but is overriden to return True when interrupt handlers are
   --  declared so the check required by C.3.1(11) can be implemented in
   --  System.Tasking.Protected_Objects.Initialize_Protection.

   procedure Initialize_Protection_Entries
     (Object           : Protection_Entries_Access;
      Ceiling_Priority : Integer;
      Compiler_Info    : System.Address;
      Entry_Bodies     : Protected_Entry_Body_Access;
      Find_Body_Index  : Find_Body_Index_Access);
   --  Initialize the Object parameter so that it can be used by the runtime
   --  to keep track of the runtime state of a protected object.

   procedure Lock_Entries (Object : Protection_Entries_Access);
   --  Lock a protected object for write access. Upon return, the caller
   --  owns the lock to this object, and no other call to Lock or
   --  Lock_Read_Only with the same argument will return until the
   --  corresponding call to Unlock has been made by the caller.

   procedure Lock_Read_Only_Entries (Object : Protection_Entries_Access);
   --  Lock a protected object for read access. Upon return, the caller
   --  owns the lock for read access, and no other calls to Lock with the
   --  same argument will return until the corresponding call to Unlock
   --  has been made by the caller. Other calls to Lock_Read_Only may (but
   --  need not) return before the call to Unlock, and the corresponding
   --  callers will also own the lock for read access.
   --
   --  Note: we are not currently using this interface, it is provided
   --  for possible future use. At the current time, everyone uses Lock
   --  for both read and write locks.

   procedure Unlock_Entries (Object : Protection_Entries_Access);
   --  Relinquish ownership of the lock for the object represented by
   --  the Object parameter. If this ownership was for write access, or
   --  if it was for read access where there are no other read access
   --  locks outstanding, one (or more, in the case of Lock_Read_Only)
   --  of the tasks waiting on this lock (if any) will be given the
   --  lock and allowed to return from the Lock or Lock_Read_Only call.

private

   procedure Finalize (Object : in out Protection_Entries);
   --  Clean up a Protection object; in particular, finalize the associated
   --  Lock object.

end System.Tasking.Protected_Objects.Entries;
