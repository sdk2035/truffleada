------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.41 $
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

pragma Polling (Off);
--  We must turn polling off for this unit, because otherwise we get
--  elaboration circularities with ourself.

with System;
with System.Standard_Library;

package Ada.Exceptions is

   type Exception_Id is private;
   Null_Id : constant Exception_Id;

   type Exception_Occurrence is limited private;
   type Exception_Occurrence_Access is access all Exception_Occurrence;

   Null_Occurrence : constant Exception_Occurrence;

   function Exception_Name (X : Exception_Occurrence) return String;
   --  Same as Exception_Name (Exception_Identity (X))

   function Exception_Name (Id : Exception_Id) return String;

   procedure Raise_Exception (E : in Exception_Id; Message : in String := "");
   pragma Export (Ada, Raise_Exception, "__gnat_raise_exception");
   --  The Export allows this routine to be accessed from Pure units.
   --  Note: it would be really nice to give a pragma No_Return for this
   --  procedure, but it would be wrong, since Raise_Exception does return
   --  if given the null exception. However we do special case the name in
   --  the test in the compiler for issuing a warning for a missing return
   --  after this call. Program_Error seems reasonable enough in such a case.

   function Exception_Message (X : Exception_Occurrence) return String;

   procedure Reraise_Occurrence (X : Exception_Occurrence);

   function Exception_Identity (X : Exception_Occurrence) return Exception_Id;

   function Exception_Information (X : Exception_Occurrence) return String;
   --  The format of the exception information is as follows:
   --
   --    exception name (as in Exception_Name)
   --    message (or a null line if no message)
   --    PID=nnnn
   --    16#xxxx_xxxx# or 16#xxxx_xxxx_xxxx_xxxx#
   --    16#xxxx_xxxx# or 16#xxxx_xxxx_xxxx_xxxx#
   --    ...
   --
   --  The lines are separated by an ASCII.CR/ASCII.LF sequence.
   --  The nnnn is the partition Id given as decimal digits.
   --  One 16#...# lines represent traceback program counter locations,
   --  in order with the first one being the exception location. All hex
   --  characters are upper case letters.

   --  Note on ordering: the compiler uses the Save_Occurrence procedure, but
   --  not the function from Rtsfind, so it is important that the procedure
   --  come first, since Rtsfind finds the first matching entity.

   procedure Save_Occurrence
     (Target :    out Exception_Occurrence;
      Source : in     Exception_Occurrence);

   function Save_Occurrence
     (Source : in Exception_Occurrence)
      return Exception_Occurrence_Access;

   function Allocate_Machine_State return System.Address;
   --  Calls target-dependent routine to allocate a Machine_State.
   --  A separate Machine_State is allocated for each task and
   --  stored in the TSD. The Machine_State is used to restore
   --  registers while propagating exceptions using the Zero Cost
   --  exception model. The returned value is the address of the
   --  allocated machine state.

   procedure Deallocate_Machine_State (M : in out System.Address);
   --  Deallocates a Machine_State allocated created by
   --  Allocate_Machine_State. The parameter M is the address of
   --  the machine state to be deallocated.

   function Current_Target_Exception return Exception_Occurrence;
   --  This routine should return the current raised exception on targets
   --  which have built-in exception handling such as the Java Virtual
   --  Machine. For other targets this routine is simply ignored. Currently,
   --  only JGNAT uses this. See 4jexcept.ads for details.

private
   package SSL renames System.Standard_Library;

   subtype EOA is Exception_Occurrence_Access;

   Exception_Msg_Max_Length : constant := 200;

   ------------------
   -- Exception_Id --
   ------------------

   subtype Code_Loc is System.Address;
   --  Code location used in building exception tables and for call
   --  addresses when propagating an exception (also traceback table)
   --  Values of this type are created by using Label'Address or
   --  extracted from machine states using Get_Code_Loc.

   Null_Loc : constant Code_Loc := System.Null_Address;
   --  Null code location, used to flag outer level frame

   type Exception_Id is new SSL.Exception_Data_Ptr;

   function EId_To_String (X : Exception_Id) return String;
   function String_To_EId (S : String) return Exception_Id;
   pragma Stream_Convert (Exception_Id, String_To_EId, EId_To_String);
   --  Functions for implementing Exception_Id stream attributes

   Null_Id : constant Exception_Id := null;

   -------------------------
   -- Private Subprograms --
   -------------------------

   function Exception_Name_Simple (X : Exception_Occurrence) return String;
   --  Like Exception_Name, but returns the simple non-qualified name of
   --  the exception. This is used to implement the Exception_Name function
   --  in Current_Exceptions (the DEC compatible unit). It is called from
   --  the compiler generated code (using Rtsfind, which does not respect
   --  the private barrier, so we can place this function in the private
   --  part where the compiler can find it, but the spec is unchanged.)

   procedure Raise_No_Msg (E : Exception_Id);
   pragma No_Return (Raise_No_Msg);
   --  Raises an exception with no message with given exception id value.
   --  Abort is deferred before the raise call.

   procedure Raise_From_Signal_Handler
     (E : Exception_Id;
      M : SSL.Big_String_Ptr);
   pragma Export
     (Ada, Raise_From_Signal_Handler,
           "ada__exceptions__raise_from_signal_handler");
   pragma No_Return (Raise_From_Signal_Handler);
   --  This routine is used to raise an exception from a signal handler.
   --  The signal handler has already stored the machine state (i.e. the
   --  state that corresponds to the location at which the signal was
   --  raised). E is the Exception_Id specifying what exception is being
   --  raised, and M is a pointer to a null-terminated string which is the
   --  message to be raised. Note that this routine never returns, so it is
   --  permissible to simply jump to this routine, rather than call it. This
   --  may be appropriate for systems where the right way to get out of a
   --  signal handler is to alter the PC value in the machine state or in
   --  some other way ask the operating system to return here rather than
   --  to the original location.

   procedure Raise_With_C_Msg
     (E : Exception_Id;
      M : SSL.Big_String_Ptr);
   pragma Export (Ada, Raise_With_C_Msg, "ada__exceptions__raise_with_c_msg");
   pragma No_Return (Raise_With_C_Msg);
   --  Raises an exception with with given exception id value and message.
   --  M is a null terminated string with the message to be raised. Abort
   --  is deferred before the raise call.

   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence);
   --  Exactly like Reraise_Occurrence, except that abort is not deferred
   --  before the call. This is used in generated code when it is known
   --  that abort is already deferred.

   procedure SDP_Table_Build
     (SDP_Addresses   : System.Address;
      SDP_Count       : Natural;
      Elab_Addresses  : System.Address;
      Elab_Addr_Count : Natural);
   pragma Export (C, SDP_Table_Build, "__gnat_SDP_Table_Build");
   --  This is the routine that is called at the start of execution to build
   --  and sort the list of subprogram descriptor pointers:
   --
   --    SDP_Addresses    Address of the start of the list of addresses of
   --                     __gnat_unit_name__SDP values constructed for each
   --                     unit, (see System.Exceptions).
   --
   --    SDP_Count        Number of entries in SDP_Addresses
   --
   --    Elab_Addresses   Address of the start of a list of addresses of
   --                     generated Ada elaboration routines, as well as
   --                     one extra entry for the generated main program.
   --                     These are used to generate the dummy SDP's that
   --                     mark the outer scope.
   --
   --    Elab_Addr_Count  Number of entries in Elab_Addresses

   procedure Break_Start;
   pragma Export (C, Break_Start, "__gnat_break_start");
   --  This is a dummy procedure that is called at the start of execution.
   --  Its sole purpose is to provide a well defined point for the placement
   --  of a main program breakpoint. We put the routine in Ada.Exceptions so
   --  that the standard mechanism of always stepping up from breakpoints
   --  within Ada.Exceptions leaves us sitting in the main program.

   -----------------------
   -- Polling Interface --
   -----------------------

   --  The GNAT compiler has an option to generate polling calls to the Poll
   --  routine in this package. Specifying the -gnatP option for a compilation
   --  causes a call to Ada.Exceptions.Poll to be generated on every subprogram
   --  entry and on every iteration of a loop, thus avoiding the possibility of
   --  a case of unbounded time between calls.

   --  This polling interface may be used for instrumentation or debugging
   --  purposes (e.g. implementing watchpoints in software or in the debugger).

   --  In the GNAT technology itself, this interface is used to implement
   --  immediate aynschronous transfer of control and immediate abort on
   --  targets which do not provide for one thread interrupting another.

   --  Note: this used to be in a separate unit called System.Poll, but that
   --  caused horrible circular elaboration problems between System.Poll and
   --  Ada.Exceptions. One way of solving such circularities is unification!

   procedure Poll;
   --  Check for asynchronous abort. Note that we do not inline the body.
   --  This makes the interface more useful for debugging purposes.

   --------------------------
   -- Exception_Occurrence --
   --------------------------

   Max_Tracebacks : constant := 50;
   --  Maximum number of trace backs stored in exception occurrence

   type Tracebacks_Array is array (1 .. Max_Tracebacks) of Code_Loc;
   --  Traceback array stored in exception occurrence

   type Exception_Occurrence is record
      Id : Exception_Id;
      --  Exception_Identity for this exception occurrence
      --  WARNING System.System.Finalization_Implementation.Finalize_List
      --  relies on the fact that this field is always first in the exception
      --  occurrence

      Msg_Length : Natural := 0;
      --  Length of message (zero = no message)

      Msg : String (1 .. Exception_Msg_Max_Length);
      --  Characters of message

      Cleanup_Flag : Boolean;
      --  The cleanup flag is normally False, it is set True for an exception
      --  occurrence passed to a cleanup routine, and will still be set True
      --  when the cleanup routine does a Reraise_Occurrence call using this
      --  exception occurrence. This is used to avoid recording a bogus trace
      --  back entry from this reraise call.

      Pid : Natural;
      --  Partition_Id for partition raising exception

      Num_Tracebacks : Natural range 0 .. Max_Tracebacks := 0;
      --  Number of traceback entries stored

      Tracebacks : Tracebacks_Array;
      --  Stored tracebacks (in Tracebacks (1 .. Num_Tracebacks)
   end record;

   function EO_To_String (X : Exception_Occurrence) return String;
   function String_To_EO (S : String) return Exception_Occurrence;
   pragma Stream_Convert (Exception_Occurrence, String_To_EO, EO_To_String);
   --  Functions for implementing Exception_Occurrence stream attributes

   Null_Occurrence : constant Exception_Occurrence := (
     Id             => Null_Id,
     Msg_Length     => 0,
     Msg            => (others => ' '),
     Cleanup_Flag   => False,
     Pid            => 0,
     Num_Tracebacks => 0,
     Tracebacks     => (others => Null_Loc));

end Ada.Exceptions;
