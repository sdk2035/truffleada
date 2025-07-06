------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.99 $
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

pragma Polling (Off);
--  We must turn polling off for this unit, because otherwise we get
--  elaboration circularities with System.Exception_Tables.

with GNAT.Heap_Sort_A;        use GNAT.Heap_Sort_A;

with System;                  use System;
with System.Exception_Table;  use System.Exception_Table;
with System.Exceptions;       use System.Exceptions;
with System.Standard_Library; use System.Standard_Library;
with System.Storage_Elements; use System.Storage_Elements;
with System.Soft_Links;       use System.Soft_Links;
with System.Machine_State_Operations; use System.Machine_State_Operations;
with System.Traceback;

with Unchecked_Conversion;

package body Ada.Exceptions is

   procedure builtin_longjmp (buffer : Address; Flag : Integer);
   pragma No_Return (builtin_longjmp);
   pragma Import (C, builtin_longjmp, "_gnat_builtin_longjmp");

   pragma Suppress (All_Checks);
   --  We definitely do not want exceptions occurring within this unit, or
   --  we are in big trouble. If an exceptional situation does occur, better
   --  that it not be raised, since raising it can cause confusing chaos.

   subtype Big_Subprogram_Descriptor_List
     is Subprogram_Descriptor_List (Natural);

   type Subprogram_Descriptor_List_Ptr is
     access all Subprogram_Descriptor_List;

   Subprogram_Descriptors : Subprogram_Descriptor_List_Ptr;
   --  This location is initialized by Register_Exceptions to point to a
   --  list of pointers to procedure descriptors, sorted into ascending
   --  order of PC addresses.

   Num_Subprogram_Descriptors : Natural;
   --  Number of subprogram desctiptors, the useful descriptors are stored
   --  in Subprogram_Descriptors (1 .. Num_Subprogram_Descriptors). There
   --  can be unused entries at the end of the array due to elimination of
   --  duplicated entries (which can arise from use of pragma Import).

   Exception_Tracebacks : Integer;
   pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
   --  Boolean indicating whether tracebacks should be stored in exception
   --  occurrences.

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  Note: the exported subprograms in this package body are called directly
   --  from C clients using the given external name, even though they are not
   --  technically visible in the Ada sense.

   procedure AAA;
   --  Mark start of procedures in this unit

   procedure ZZZ;
   --  Mark end of procedures in this package

   Address_Image_Length : constant :=
                            13 + 10 * Boolean'Pos (Standard'Address_Size > 32);
   --  Length of string returned by Address_Image function

   function Address_Image (A : System.Address) return String;
   --  Returns at string of the form 0xhhhhhhhhh for 32-bit addresses
   --  or 0xhhhhhhhhhhhhhhhh for 64-bit addresses. Hex characters are
   --  in lower case.

   procedure Raise_Current_Excep (E : Exception_Id);
   pragma No_Return (Raise_Current_Excep);
   pragma Export (C, Raise_Current_Excep, "__gnat_raise_nodefer_with_msg");
   --  This is the lowest level raise routine. It raises the exception
   --  referenced by Current_Excep.all in the TSD, without deferring
   --  abort (the caller must ensure that abort is deferred on entry).
   --  The parameter E is ignored.
   --
   --  This external name for Raise_Current_Excep is historical, and probably
   --  should be changed but for now we keep it, because gdb knows about it.
   --  The parameter is also present for historical compatibility. ???

   procedure Raise_Exception_No_Defer
      (E : in Exception_Id; Message : in String := "");
   pragma Export (Ada, Raise_Exception_No_Defer,
     "ada__exceptions__raise_exception_no_defer");
   pragma No_Return (Raise_Exception_No_Defer);
   --  Similar to Raise_Exception, but with no abort deferral

   procedure Raise_With_Msg (E : Exception_Id);
   pragma No_Return (Raise_With_Msg);
   pragma Export (C, Raise_With_Msg, "__gnat_raise_with_msg");
   --  Raises an exception with given exception id value. A message
   --  is associated with the raise, and has already been stored in the
   --  exception occurrence referenced by the Current_Excep in the TSD.
   --  Abort is deferred before the raise call.

   procedure Reraise;
   pragma No_Return (Reraise);
   pragma Export (C, Reraise, "__gnat_reraise");
   --  Reraises the exception referenced by the Current_Excep field of
   --  the TSD (all fields of this exception occurrence are set). Abort
   --  is deferred before the reraise operation.

   procedure Raise_With_Location
     (E : Exception_Id;
      F : SSL.Big_String_Ptr;
      L : Integer);
   pragma No_Return (Raise_With_Location);
   --  Raise an exception with given exception id value. A filename and line
   --  number is associated with the raise and is stored in the exception
   --  occurrence.

   procedure Raise_Constraint_Error
     (File : SSL.Big_String_Ptr; Line : Integer);
   pragma No_Return (Raise_Constraint_Error);
   pragma Export (C, Raise_Constraint_Error, "__gnat_raise_constraint_error");
   --  Raise constraint error with file:line information

   procedure Raise_Program_Error
     (File : SSL.Big_String_Ptr; Line : Integer);
   pragma No_Return (Raise_Program_Error);
   pragma Export (C, Raise_Program_Error, "__gnat_raise_program_error");
   --  Raise program error with file:line information

   procedure Raise_Storage_Error
     (File : SSL.Big_String_Ptr; Line : Integer);
   pragma No_Return (Raise_Storage_Error);
   pragma Export (C, Raise_Storage_Error, "__gnat_raise_storage_error");
   --  Raise storage error with file:line information

   function SDP_Table_Sort_Lt (Op1, Op2 : Natural) return Boolean;
   --  Used in call to sort SDP table (SDP_Table_Build), compares two elements

   procedure SDP_Table_Sort_Move (From : Natural; To : Natural);
   --  Used in call to sort SDP table (SDP_Table_Build), moves one element

   procedure Set_Exception_C_Msg
     (Id   : Exception_Id;
      Msg  : SSL.Big_String_Ptr;
      Line : Integer := 0);
   --  This routine is called to setup the exception referenced by the
   --  Current_Excep field in the TSD to contain the indicated Id value
   --  and message. Msg is a null terminated string. when Line > 0,
   --  Msg is the filename and line the line number of the exception location.

   procedure Unhandled_Exception_Terminate;
   pragma No_Return (Unhandled_Exception_Terminate);
   --  This procedure is called to terminate execution following an unhandled
   --  exception. The exception information, including traceback if available
   --  is output, and execution is then terminated. Note that at the point
   --  where this routine is called, the stack has typically been destroyed

   ---------------------------------
   -- Debugger Interface Routines --
   ---------------------------------

   --  The routines here are null routines that normally have no effect.
   --  they are provided for the debugger to place breakpoints on their
   --  entry points to get control on an exception.

   procedure Notify_Exception
     (Id        : Exception_Id;
      Handler   : Code_Loc;
      Is_Others : Boolean);
   pragma Export (C, Notify_Exception, "__gnat_notify_exception");
   --  This routine is called whenever an exception is signalled. The Id
   --  parameter is the Exception_Id of the exception being raised. The
   --  second parameter Handler is Null_Loc if the exception is unhandled,
   --  and is otherwise the entry point of the handler that will handle
   --  the exception. Is_Others is True if the handler is an others handler
   --  and False otherwise. In the unhandled exception case, if possible
   --  (and certainly if zero cost exception handling is active), the
   --  stack is still intact when this procedure is called. Note that this
   --  routine is entered before any finalization handlers are entered if
   --  the exception is unhandled by a "real" exception handler.

   procedure Unhandled_Exception;
   pragma Export (C, Unhandled_Exception, "__gnat_unhandled_exception");
   --  This routine is called in addition to Notify_Exception in the
   --  unhandled exception case. The fact that there are two routines
   --  which are somewhat redundant is historical. Notify_Exception
   --  certainly is complete enough, but GDB still uses this routine.

   --------------------------------
   -- Import Run-Time C Routines --
   --------------------------------

   --  The purpose of the following pragma Imports is to ensure that we
   --  generate appropriate subprogram descriptors for all C routines in
   --  the standard GNAT library that can raise exceptions. This ensures
   --  that the exception propagation can properly find these routines

   pragma Warnings (Off);        -- so old compiler does not complain
   pragma Propagate_Exceptions;

   procedure Unhandled_Terminate;
   pragma Import (C, Unhandled_Terminate, "__gnat_unhandled_terminate");

   procedure GNAT_Malloc (Size : Integer_Address);
   pragma Import (C, GNAT_Malloc, "__gnat_malloc");

   procedure GNAT_Realloc (Ptr : Address; Size : Integer_Address);
   pragma Import (C, GNAT_Realloc, "__gnat_realloc");

   procedure Propagate_Exception (Mstate : Machine_State);
   pragma No_Return (Propagate_Exception);
   --  This procedure propagates the exception represented by the occurrence
   --  referenced by Current_Excep in the TSD for the current task. M is
   --  the initial machine state, representing the site of the exception
   --  raise operation. Propagate_Exception searches the exception tables
   --  for an applicable handler, calling Pop_Frame as needed. If and when
   --  it locates an applicable handler Propagate_Exception makes a call
   --  to Enter_Handler to actually enter the handler. If the search is
   --  unable to locate an applicable handler, execution is terminated by
   --  calling Unhandled_Exception_Terminate.

   procedure Call_Chain (Excep : EOA);
   --  Store up to Max_Tracebacks in Excep, corresponding to the current
   --  call chain.

   -----------------------
   -- Polling Interface --
   -----------------------

   type Unsigned is mod 2 ** 32;

   Counter : Unsigned := 0;
   --  This counter is provided for convenience. It can be used in Poll to
   --  perform periodic but not systematic operations.

   procedure Poll is separate;
   --  The actual polling routine is separate, so that it can easily
   --  be replaced with a target dependent version.

   ---------
   -- AAA --
   ---------

   --  This dummy procedure gives us the start of the PC range for addresses
   --  within the exception unit itself. We hope that gigi/gcc keep all the
   --  procedures in their original order!

   procedure AAA is
   begin
      null;
   end AAA;

   -------------------
   -- Address_Image --
   -------------------

   function Address_Image (A : Address) return String is
      S : String (1 .. 18);
      P : Natural;
      N : Integer_Address;

      H : constant array (Integer range 0 .. 15) of Character :=
                                                         "0123456789abcdef";
   begin
      P := S'Last;
      N := To_Integer (A);
      while N /= 0 loop
         S (P) := H (Integer (N mod 16));
         P := P - 1;
         N := N / 16;
      end loop;

      S (P - 1) := '0';
      S (P) := 'x';
      return S (P - 1 .. S'Last);
   end Address_Image;

   ----------------------------
   -- Allocate_Machine_State --
   ----------------------------

   function Allocate_Machine_State return System.Address is
   begin
      return System.Address
        (System.Machine_State_Operations.Allocate_Machine_State);
   end Allocate_Machine_State;

   -----------------
   -- Break_Start --
   -----------------

   procedure Break_Start is
   begin
      null;
   end Break_Start;

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain (Excep : EOA) is
   begin
      if Excep.Num_Tracebacks /= 0 then
         --  This is a reraise, no need to store a new (wrong) chain.
         return;
      end if;

      System.Traceback.Call_Chain
        (Excep.Tracebacks'Address,
         Max_Tracebacks,
         Excep.Num_Tracebacks,
         AAA'Address,
         ZZZ'Address);
   end Call_Chain;

   ------------------------------
   -- Current_Target_Exception --
   ------------------------------

   function Current_Target_Exception return Exception_Occurrence is
   begin
      return Null_Occurrence;
   end Current_Target_Exception;

   ----------------------------
   -- Deallocate_Machine_State --
   ----------------------------

   procedure Deallocate_Machine_State (M : in out System.Address) is
   begin
      Free_Machine_State (Machine_State (M));
   end Deallocate_Machine_State;

   -------------------
   -- EId_To_String --
   -------------------

   function EId_To_String (X : Exception_Id) return String is
   begin
      if X = Null_Id then
         return "";
      else
         return Exception_Name (X);
      end if;
   end EId_To_String;

   ------------------
   -- EO_To_String --
   ------------------

   --  We use the null string to represent the null occurrence, otherwise
   --  we output the Exception_Information string for the occurrence.

   function EO_To_String (X : Exception_Occurrence) return String is
   begin
      if X.Id = Null_Id then
         return "";
      else
         return Exception_Information (X);
      end if;
   end EO_To_String;

   ------------------------
   -- Exception_Identity --
   ------------------------

   function Exception_Identity
     (X    : Exception_Occurrence)
      return Exception_Id
   is
   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      else
         return X.Id;
      end if;
   end Exception_Identity;

   ---------------------------
   -- Exception_Information --
   ---------------------------

   --  The format of the string is:

   --    Exception_Name: nnnnn
   --    Message: mmmmm
   --    PID: ppp
   --    Call stack traceback locations:
   --    0xhhhh 0xhhhh 0xhhhh ... 0xhhh

   --  where

   --    nnnn is the fully qualified name of the exception in all upper
   --    case letters. This line is always present.

   --    mmmm is the message (this line present only if message is non-null)

   --    ppp is the Process Id value as a decimal integer (this line is
   --    present only if the Process Id is non-zero). Currently we are
   --    not making use of this field.

   --    The Call stack traceback locations line and the following values
   --    are present only if at least one traceback location was recorded.
   --    the values are given in C style format, with lower case letters
   --    for a-f, and only as many digits present as are necessary.

   --  The line terminator sequence at the end of each line, including the
   --  last line is a CR-LF sequence (16#0D# followed by 16#0A#).

   --  The Exception_Name and Message lines are omitted in the abort
   --  signal case, since this is not really an exception, and the only
   --  use of this routine is internal for printing termination output.

   --  Warning: if the format of the generated string is changed, please note
   --  that an equivalent modification to the routine String_To_EO must be
   --  made to preserve proper functioning of the stream attributes.

   function Exception_Information (X : Exception_Occurrence) return String is
      Msg  : constant String  := Exception_Message (X);
      Name : constant String  := Exception_Name (X);
      Len  : constant Natural := Name'Length;
      Ptr  : Natural := 0;

      --  Allocate string of more than enough length

      Info : String (1 .. Len +
                          Msg'Length +
                          120 +
                          X.Num_Tracebacks * 18);

      procedure Add_Info_Nat (N : Natural);
      --  Little internal routine to add CR.

      procedure Add_Info_NL;
      --  Little internal routine to add CR/LF to information

      procedure Add_Info_String (S : String);
      --  Little internal routine to add given string to information

      procedure Add_Info_Nat (N : Natural) is
      begin
         if N > 9 then
            Add_Info_Nat (N / 10);
         end if;

         Ptr := Ptr + 1;
         Info (Ptr) := Character'Val (Character'Pos ('0') + N mod 10);
      end Add_Info_Nat;

      procedure Add_Info_NL is
      begin
         Ptr := Ptr + 1;
         Info (Ptr) := ASCII.CR;
         Ptr := Ptr + 1;
         Info (Ptr) := ASCII.LF;
      end Add_Info_NL;

      procedure Add_Info_String (S : String) is
      begin
         Info (Ptr + 1 .. Ptr + S'Length) := S;
         Ptr := Ptr + S'Length;
      end Add_Info_String;

   --  Start of processing for Exception_Information

   begin
      --  Output exception name and message except for _ABORT_SIGNAL, where
      --  these two lines are omitted (see discussion above).

      if Name (1) /= '_' then
         Add_Info_String ("Exception name: ");
         Add_Info_String (Name);
         Add_Info_NL;

         if Msg'Length /= 0 then
            Add_Info_String ("Message: ");
            Add_Info_String (Msg);
            Add_Info_NL;
         end if;
      end if;

      --  Output PID line if non-zero

      if X.Pid /= 0 then
         Add_Info_String ("PID: ");
         Add_Info_Nat (X.Pid);
         Add_Info_NL;
      end if;

      --  Output tracebacks if present

      if X.Num_Tracebacks > 0 then
         Add_Info_String ("Call stack traceback locations:");
         Add_Info_NL;

         for J in 1 .. X.Num_Tracebacks loop
            Add_Info_String (Address_Image (X.Tracebacks (J)));
            exit when J = X.Num_Tracebacks;
            Add_Info_String (" ");
         end loop;

         Add_Info_NL;
      end if;

      return Info (1 .. Ptr);
   end Exception_Information;

   -----------------------
   -- Exception_Message --
   -----------------------

   function Exception_Message (X : Exception_Occurrence) return String is
   begin
      if X.Id = Null_Id then
         raise Constraint_Error;
      end if;

      return X.Msg (1 .. X.Msg_Length);
   end Exception_Message;

   --------------------
   -- Exception_Name --
   --------------------

   function Exception_Name (Id : Exception_Id) return String is
   begin
      if Id = null then
         raise Constraint_Error;
      end if;

      return Id.Full_Name.all (1 .. Id.Name_Length - 1);
   end Exception_Name;

   function Exception_Name (X : Exception_Occurrence) return String is
   begin
      return Exception_Name (X.Id);
   end Exception_Name;

   ---------------------------
   -- Exception_Name_Simple --
   ---------------------------

   function Exception_Name_Simple (X : Exception_Occurrence) return String is
      Name : constant String := Exception_Name (X);
      P    : Natural;

   begin
      P := Name'Length;
      while P > 1 loop
         exit when Name (P - 1) = '.';
         P := P - 1;
      end loop;

      return Name (P .. Name'Length);
   end Exception_Name_Simple;

   -------------------------
   -- Propagate_Exception --
   -------------------------

   procedure Propagate_Exception (Mstate : Machine_State) is
      Excep  : constant EOA := Get_Current_Excep.all;
      Loc    : Code_Loc;
      Lo, Hi : Natural;
      Pdesc  : Natural;
      Hrec   : Handler_Record_Ptr;
      Info   : Subprogram_Info_Type;

      type Machine_State_Record is
        new Storage_Array (1 .. Machine_State_Length);
      for Machine_State_Record'Alignment use Standard'Maximum_Alignment;

      procedure Duplicate_Machine_State (Dest, Src : Machine_State);
      --  Copy Src into Dest, assuming that a Machine_State is pointing to
      --  an area of Machine_State_Length bytes.

      procedure Duplicate_Machine_State (Dest, Src : Machine_State) is
         type Machine_State_Record_Access is access Machine_State_Record;
         function To_MSR is new Unchecked_Conversion
           (Machine_State, Machine_State_Record_Access);

      begin
         To_MSR (Dest).all := To_MSR (Src).all;
      end Duplicate_Machine_State;

      --  Data for handling the finalization handler case. A simple approach
      --  in this routine would simply to unwind stack frames till we find a
      --  handler and then enter it. But this is undesirable in the case where
      --  we have only finalization handlers, and no "real" handler, i.e. a
      --  case where we have an unhandled exception.

      --  In this case we prefer to signal unhandled exception with the stack
      --  intact, and entering finalization handlers would destroy the stack
      --  state. To deal with this, as we unwind the stack, we note the first
      --  finalization handler, and remember it in the following variables.
      --  We then continue to unwind. If and when we find a "real", i.e. non-
      --  finalization handler, then we use these variables to pass control to
      --  the finalization handler.

      FH_Found : Boolean := False;
      --  Set when a finalization handler is found

      FH_Mstate : aliased Machine_State_Record;
      --  Records the machine state for the finalization handler

      FH_Handler : Code_Loc;
      --  Record handler address for finalization handler

      FH_Num_Trb : Natural;
      --  Save number of tracebacks for finalization handler

   begin
      --  Loop through stack frames as exception propagates

      Main_Loop : loop
         <<Continue>>
         Loc := Get_Code_Loc (Mstate);
         exit when Loc = Null_Loc;

         --  Record location unless it is inside this unit. Note: this
         --  test should really say Code_Address, but Address is the same
         --  as Code_Address for unnested subprograms, and Code_Address
         --  would cause a bootstrap problem

         if Loc < AAA'Address or else Loc > ZZZ'Address then

            --  Record location unless we already recorded max tracebacks

            if Excep.Num_Tracebacks /= Max_Tracebacks then

               --  Do not record location if it is the return point from
               --  a reraise call from within a cleanup handler

               if not Excep.Cleanup_Flag then
                  Excep.Num_Tracebacks := Excep.Num_Tracebacks + 1;
                  Excep.Tracebacks (Excep.Num_Tracebacks) := Loc;

               --  For reraise call from cleanup handler, skip entry and
               --  clear the flag so that we will start to record again

               else
                  Excep.Cleanup_Flag := False;
               end if;
            end if;
         end if;

         --  Do binary search on procedure table

         Lo := 1;
         Hi := Num_Subprogram_Descriptors;

         --  Binary search loop

         loop
            Pdesc := (Lo + Hi) / 2;

            --  Note that Loc is expected to be the procedure's call point
            --  and not the return point.

            if Loc < Subprogram_Descriptors (Pdesc).Code then
               Hi := Pdesc - 1;

            elsif Pdesc < Num_Subprogram_Descriptors
              and then Loc > Subprogram_Descriptors (Pdesc + 1).Code
            then
               Lo := Pdesc + 1;

            else
               exit;
            end if;

            --  This happens when the current Loc is completely outside of
            --  the range of the program, which usually means that we reached
            --  the top level frame (e.g __start). In this case we have an
            --  unhandled exception.

            exit Main_Loop when Hi < Lo;
         end loop;

         --  Come here with Subprogram_Descriptors (Pdesc) referencing the
         --  procedure descriptor that applies to this PC value. Now do a
         --  serial search to see if any handler is applicable to this PC
         --  value, and to the exception that we are propagating

         for J in 1 .. Subprogram_Descriptors (Pdesc).Num_Handlers loop
            Hrec := Subprogram_Descriptors (Pdesc).Handler_Records (J);

            if Loc >= Hrec.Lo and then Loc < Hrec.Hi then

               --  PC range is applicable, see if handler is for this exception

               --  First test for case of "all others" (finalization) handler.
               --  We do not enter such a handler until we are sure there is
               --  a real handler further up the stack.

               if Hrec.Id = All_Others_Id then

                  --  If this is the first finalization handler, then
                  --  save the machine state so we can enter it later
                  --  without having to repeat the search.

                  if not FH_Found then
                     FH_Found   := True;
                     Duplicate_Machine_State
                       (Machine_State (FH_Mstate'Address), Mstate);
                     FH_Handler := Hrec.Handler;
                     FH_Num_Trb := Excep.Num_Tracebacks;
                  end if;

               --  Normal (non-finalization exception with matching Id)

               elsif Excep.Id = Hrec.Id
                 or else (Hrec.Id = Others_Id
                            and not Excep.Id.Not_Handled_By_Others)
               then
                  --  Notify the debugger that we have found a handler
                  --  and are about to propagate an exception.

                  Notify_Exception
                    (Excep.Id, Hrec.Handler, Hrec.Id = Others_Id);

                  --  If we already encountered a finalization handler, then
                  --  reset the context to that handler, and enter it.

                  if FH_Found then
                     Excep.Num_Tracebacks := FH_Num_Trb;
                     Excep.Cleanup_Flag   := True;
                     Enter_Handler
                       (Machine_State (FH_Mstate'Address), FH_Handler);

                  --  If we have not encountered a finalization handler,
                  --  then enter the current handler.

                  else
                     Enter_Handler (Mstate, Hrec.Handler);
                  end if;
               end if;
            end if;
         end loop;

         Info := Subprogram_Descriptors (Pdesc).Subprogram_Info;
         exit when Info = No_Info;
         Pop_Frame (Mstate, Info);
      end loop Main_Loop;

      --  Fall through if no "real" exception handler found. First thing
      --  is to call the dummy Unhandled_Exception routine with the stack
      --  intact, so that the debugger can get control.

      Unhandled_Exception;

      --  Also make the appropriate Notify_Exception call for the debugger.

      Notify_Exception (Excep.Id, Null_Loc, False);

      --  If there were finalization handlers, then enter the top one.
      --  Just because there is no handler does not mean we don't have
      --  to still execute all finalizations and cleanups before
      --  terminating. Note that the process of calling cleanups
      --  does not disturb the back trace stack, since he same
      --  exception occurrence gets reraised, and new traceback
      --  entries added as we go along.

      if FH_Found then
         Excep.Num_Tracebacks := FH_Num_Trb;
         Excep.Cleanup_Flag   := True;
         Enter_Handler (Machine_State (FH_Mstate'Address), FH_Handler);
      end if;

      --  If no cleanups, then this is the real unhandled termination

      Unhandled_Exception_Terminate;

   end Propagate_Exception;

   -------------------------
   -- Raise_Current_Excep --
   -------------------------

   procedure Raise_Current_Excep (E : Exception_Id) is

      pragma Inspection_Point (E);
      --  This is so the debugger can reliably inspect the parameter

      Jumpbuf_Ptr : constant Address := Get_Jmpbuf_Address.all;
      Mstate_Ptr  : constant Machine_State :=
                      Machine_State (Get_Machine_State_Addr.all);

   begin
      --  If the jump buffer pointer is non-null, it means that a jump
      --  buffer was allocated (obviously that happens only in the case
      --  of zero cost exceptions not implemented, or if a jump buffer
      --  was manually set up by C code).

      if Jumpbuf_Ptr /= Null_Address then
         if Exception_Tracebacks /= 0 then
            Call_Chain (Get_Current_Excep.all);
         end if;

         builtin_longjmp (Jumpbuf_Ptr, 1);

      --  If we have no jump buffer, then either zero cost exception
      --  handling is in place, or we have no handlers anyway. In
      --  either case we have an unhandled exception. If zero cost
      --  exception handling is in place, propagate the exception

      elsif Subprogram_Descriptors /= null then
         Set_Machine_State (Mstate_Ptr);
         Propagate_Exception (Mstate_Ptr);

      --  Otherwise, we know the exception is unhandled by the absence
      --  of an allocated jump buffer. Note that this means that we also
      --  have no finalizations to do other than at the outer level.

      else
         if Exception_Tracebacks /= 0 then
            Call_Chain (Get_Current_Excep.all);
         end if;

         Unhandled_Exception;
         Notify_Exception (E, Null_Loc, False);
         Unhandled_Exception_Terminate;
      end if;
   end Raise_Current_Excep;

   --------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (E       : in Exception_Id;
      Message : in String := "")
   is
      Len : constant Natural :=
              Natural'Min (Message'Length, Exception_Msg_Max_Length);

      Excep : constant EOA := Get_Current_Excep.all;

   begin
      if E /= null then
         Excep.Msg_Length := Len;
         Excep.Msg (1 .. Len) := Message (1 .. Len);
         Raise_With_Msg (E);
      end if;
   end Raise_Exception;

   -------------------------------
   -- Raise_From_Signal_Handler --
   -------------------------------

   procedure Raise_From_Signal_Handler
     (E : Exception_Id;
      M : SSL.Big_String_Ptr)
   is
      Jumpbuf_Ptr : constant Address := Get_Jmpbuf_Address.all;
      Mstate_Ptr  : constant Machine_State :=
                      Machine_State (Get_Machine_State_Addr.all);

   begin
      Set_Exception_C_Msg (E, M);
      Abort_Defer.all;

      --  Now we raise the exception. The following code is essentially
      --  identical to the Raise_Current_Excep routine, except that in the
      --  zero cost exception case, we do not call Set_Machine_State, since
      --  the signal handler that passed control here has already set the
      --  machine state directly.

      --  If the jump buffer pointer is non-null, it means that a jump
      --  buffer was allocated (obviously that happens only in the case
      --  of zero cost exceptions not implemented, or if a jump buffer
      --  was manually set up by C code).

      if Jumpbuf_Ptr /= Null_Address then
         builtin_longjmp (Jumpbuf_Ptr, 1);

      --  If we have no jump buffer, then either zero cost exception
      --  handling is in place, or we have no handlers anyway. In
      --  either case we have an unhandled exception. If zero cost
      --  exception handling is in place, propagate the exception

      elsif Subprogram_Descriptors /= null then
         Propagate_Exception (Mstate_Ptr);

      --  Otherwise, we know the exception is unhandled by the absence
      --  of an allocated jump buffer. Note that this means that we also
      --  have no finalizations to do other than at the outer level.

      else
         Unhandled_Exception;
         Unhandled_Exception_Terminate;
      end if;
   end Raise_From_Signal_Handler;

   ------------------
   -- Raise_No_Msg --
   ------------------

   procedure Raise_No_Msg (E : Exception_Id) is
      Excep : constant EOA := Get_Current_Excep.all;

   begin
      Excep.Msg_Length := 0;
      Raise_With_Msg (E);
   end Raise_No_Msg;

   -------------------------
   -- Raise_With_Location --
   -------------------------

   procedure Raise_With_Location
     (E : Exception_Id;
      F : SSL.Big_String_Ptr;
      L : Integer) is
   begin
      Set_Exception_C_Msg (E, F, L);
      Abort_Defer.all;
      Raise_Current_Excep (E);
   end Raise_With_Location;

   ----------------------------
   -- Raise_Constraint_Error --
   ----------------------------

   procedure Raise_Constraint_Error
     (File : SSL.Big_String_Ptr; Line : Integer) is
   begin
      Raise_With_Location (Constraint_Error_Def'Access, File, Line);
   end Raise_Constraint_Error;

   -------------------------
   -- Raise_Program_Error --
   -------------------------

   procedure Raise_Program_Error
     (File : SSL.Big_String_Ptr; Line : Integer) is
   begin
      Raise_With_Location (Program_Error_Def'Access, File, Line);
   end Raise_Program_Error;

   -------------------------
   -- Raise_Storage_Error --
   -------------------------

   procedure Raise_Storage_Error
     (File : SSL.Big_String_Ptr; Line : Integer) is
   begin
      Raise_With_Location (Storage_Error_Def'Access, File, Line);
   end Raise_Storage_Error;

   ----------------------
   -- Raise_With_C_Msg --
   ----------------------

   procedure Raise_With_C_Msg
     (E    : Exception_Id;
      M    : SSL.Big_String_Ptr) is
   begin
      Set_Exception_C_Msg (E, M);
      Abort_Defer.all;
      Raise_Current_Excep (E);
   end Raise_With_C_Msg;

   --------------------
   -- Raise_With_Msg --
   --------------------

   procedure Raise_With_Msg (E : Exception_Id) is
      Excep : constant EOA := Get_Current_Excep.all;

   begin
      Excep.Id             := E;
      Excep.Num_Tracebacks := 0;
      Excep.Cleanup_Flag   := False;
      Excep.Pid            := Local_Partition_ID;
      Abort_Defer.all;
      Raise_Current_Excep (E);
   end Raise_With_Msg;

   -------------
   -- Reraise --
   -------------

   procedure Reraise is
      Excep : constant EOA := Get_Current_Excep.all;

   begin
      Abort_Defer.all;
      Raise_Current_Excep (Excep.Id);
   end Reraise;

   ------------------------
   -- Reraise_Occurrence --
   ------------------------

   procedure Reraise_Occurrence (X : Exception_Occurrence) is
   begin
      if X.Id /= null then
         Abort_Defer.all;
         Save_Occurrence (Get_Current_Excep.all.all, X);
         Raise_Current_Excep (X.Id);
      end if;
   end Reraise_Occurrence;

   ---------------------------------
   -- Reraise_Occurrence_No_Defer --
   ---------------------------------

   procedure Reraise_Occurrence_No_Defer (X : Exception_Occurrence) is
   begin
      Save_Occurrence (Get_Current_Excep.all.all, X);
      Raise_Current_Excep (X.Id);
   end Reraise_Occurrence_No_Defer;

   ---------------------
   -- Save_Occurrence --
   ---------------------

   procedure Save_Occurrence
     (Target : out Exception_Occurrence;
      Source : in  Exception_Occurrence)
   is
   begin
      Target.Id             := Source.Id;
      Target.Msg_Length     := Source.Msg_Length;
      Target.Num_Tracebacks := Source.Num_Tracebacks;
      Target.Pid            := Source.Pid;
      Target.Cleanup_Flag   := Source.Cleanup_Flag;

      Target.Msg (1 .. Target.Msg_Length) :=
        Source.Msg (1 .. Target.Msg_Length);

      Target.Tracebacks (1 .. Target.Num_Tracebacks) :=
        Source.Tracebacks (1 .. Target.Num_Tracebacks);
   end Save_Occurrence;

   function Save_Occurrence
     (Source : in Exception_Occurrence)
      return   EOA
   is
      Target : EOA := new Exception_Occurrence;

   begin
      Save_Occurrence (Target.all, Source);
      return Target;
   end Save_Occurrence;

   ---------------------
   -- SDP_Table_Build --
   ---------------------

   procedure SDP_Table_Build
     (SDP_Addresses   : System.Address;
      SDP_Count       : Natural;
      Elab_Addresses  : System.Address;
      Elab_Addr_Count : Natural)
   is
      type SDLP_Array is array (1 .. SDP_Count) of Subprogram_Descriptors_Ptr;
      type SDLP_Array_Ptr is access all SDLP_Array;

      function To_SDLP_Array_Ptr is new Unchecked_Conversion
        (System.Address, SDLP_Array_Ptr);

      T : constant SDLP_Array_Ptr := To_SDLP_Array_Ptr (SDP_Addresses);

      type Elab_Array is array (1 .. Elab_Addr_Count) of Code_Loc;
      type Elab_Array_Ptr is access all Elab_Array;

      function To_Elab_Array_Ptr is new Unchecked_Conversion
        (System.Address, Elab_Array_Ptr);

      EA : constant Elab_Array_Ptr := To_Elab_Array_Ptr (Elab_Addresses);

      Ndes : Natural;

   begin
      --  First count number of subprogram descriptors. This count includes
      --  entries with duplicated code addresses (resulting from Import).

      Ndes := Elab_Addr_Count;
      for J in T'Range loop
         Ndes := Ndes + T (J).Count;
      end loop;

      --  Now allocate the table (extra zero'th element is for sort call)

      Subprogram_Descriptors := new Subprogram_Descriptor_List (0 .. Ndes);

      --  First copy in the elaboration routine addresses, building dummy
      --  SDP's for them as we go through the list.

      Ndes := 0;
      for J in EA'Range loop
         Ndes := Ndes + 1;
         Subprogram_Descriptors (Ndes) := new Subprogram_Descriptor_0;

         Subprogram_Descriptors (Ndes).all :=
           Subprogram_Descriptor'
             (Num_Handlers    => 0,
              Code            => EA (J),
              Subprogram_Info => EA (J),
              Handler_Records => (1 .. 0 => null));
      end loop;

      --  Now copy in pointers to SDP addresses of application subprograms

      for J in T'Range loop
         for K in 1 .. T (J).Count loop
            Ndes := Ndes + 1;
            Subprogram_Descriptors (Ndes) := T (J).SDesc (K);
         end loop;
      end loop;

      --  Now we need to sort the table into ascending PC order

      Sort (Ndes, SDP_Table_Sort_Move'Access, SDP_Table_Sort_Lt'Access);

      --  Now eliminate duplicate entries. Note that in the case where
      --  entries have duplicate code addresses, the code for the Lt
      --  routine ensures that the interesting one (i.e. the one with
      --  handler entries if there are any) comes first.

      Num_Subprogram_Descriptors := 1;

      for J in 2 .. Ndes loop
         if Subprogram_Descriptors (J).Code /=
            Subprogram_Descriptors (Num_Subprogram_Descriptors).Code
         then
            Num_Subprogram_Descriptors := Num_Subprogram_Descriptors + 1;
            Subprogram_Descriptors (Num_Subprogram_Descriptors) :=
              Subprogram_Descriptors (J);
         end if;
      end loop;

   end SDP_Table_Build;

   -----------------------
   -- SDP_Table_Sort_Lt --
   -----------------------

   function SDP_Table_Sort_Lt (Op1, Op2 : Natural) return Boolean is
      SDC1 : constant Code_Loc := Subprogram_Descriptors (Op1).Code;
      SDC2 : constant Code_Loc := Subprogram_Descriptors (Op2).Code;

   begin
      if SDC1 < SDC2 then
         return True;

      elsif SDC1 > SDC2 then
         return False;

      --  For two descriptors for the same procedure, we want the more
      --  interesting one first. A descriptor with an exception handler
      --  is more interesting than one without. This happens if the less
      --  interesting one came from a pragma Import.

      else
         return Subprogram_Descriptors (Op1).Num_Handlers /= 0
           and then Subprogram_Descriptors (Op2).Num_Handlers = 0;
      end if;
   end SDP_Table_Sort_Lt;

   --------------------------
   -- SDP_Table_Sort_Move --
   --------------------------

   procedure SDP_Table_Sort_Move (From : Natural; To : Natural) is
   begin
      Subprogram_Descriptors (To) := Subprogram_Descriptors (From);
   end SDP_Table_Sort_Move;

   -------------------------
   -- Set_Exception_C_Msg --
   -------------------------

   procedure Set_Exception_C_Msg
     (Id   : Exception_Id;
      Msg  : Big_String_Ptr;
      Line : Integer := 0)
   is
      Excep  : constant EOA := Get_Current_Excep.all;
      Val    : Integer := Line;
      Remind : Integer;
      Size   : Integer := 1;

   begin
      Excep.Id             := Id;
      Excep.Num_Tracebacks := 0;
      Excep.Pid            := Local_Partition_ID;
      Excep.Msg_Length     := 0;
      Excep.Cleanup_Flag   := False;

      while Msg (Excep.Msg_Length + 1) /= ASCII.NUL
        and then Excep.Msg_Length < Exception_Msg_Max_Length
      loop
         Excep.Msg_Length := Excep.Msg_Length + 1;
         Excep.Msg (Excep.Msg_Length) := Msg (Excep.Msg_Length);
      end loop;

      if Line > 0 then
         --  Compute the number of needed characters

         while Val > 0 loop
            Val := Val / 10;
            Size := Size + 1;
         end loop;

         --  If enough characters are available, put the line number

         if Excep.Msg_Length <= Exception_Msg_Max_Length - Size then
            Excep.Msg (Excep.Msg_Length + 1) := ':';
            Excep.Msg_Length := Excep.Msg_Length + Size;
            Val := Line;
            Size := 0;

            while Val > 0 loop
               Remind := Val rem 10;
               Val := Val / 10;
               Excep.Msg (Excep.Msg_Length - Size) :=
                 Character'Val (Remind + Character'Pos ('0'));
               Size := Size + 1;
            end loop;
         end if;
      end if;
   end Set_Exception_C_Msg;

   -------------------
   -- String_To_EId --
   -------------------

   function String_To_EId (S : String) return Exception_Id is
   begin
      if S = "" then
         return Null_Id;
      else
         return Exception_Id (Internal_Exception (S));
      end if;
   end String_To_EId;

   X : Exception_Occurrence;
   --  This should be inside String_To_EO, it is outside to temporarily
   --  get around a bootstrap problem.

   ------------------
   -- String_To_EO --
   ------------------

   function String_To_EO (S : String) return Exception_Occurrence is
      From : Natural;
      To   : Integer;

      procedure Bad_EO;
      --  Signal bad exception occurrence string

      procedure Next_String;
      --  On entry, To points to last character of previous line of the
      --  message, terminated by CR/LF. On return, From .. To are set to
      --  specify the next string, or From > To if there are no more lines.

      procedure Bad_EO is
      begin
         Raise_Exception
           (Program_Error'Identity,
            "bad exception occurrence in stream input");
      end Bad_EO;

      procedure Next_String is
      begin
         From := To + 3;

         if From < S'Last then
            To := From + 1;

            while To < S'Last - 2 loop
               if To >= S'Last then
                  Bad_EO;
               elsif S (To + 1) = ASCII.CR then
                  exit;
               else
                  To := To + 1;
               end if;
            end loop;
         end if;
      end Next_String;

   --  Start of processing for String_To_EO

   begin
      if S = "" then
         return Null_Occurrence;

      else
         X.Cleanup_Flag := False;

         To := S'First - 3;
         Next_String;

         if S (From .. From + 15) /= "Exception name: " then
            Bad_EO;
         end if;

         X.Id := Exception_Id (Internal_Exception (S (From + 16 .. To)));

         Next_String;

         if From <= To and then S (From) = 'M' then
            if S (From .. From + 8) /= "Message: " then
               Bad_EO;
            end if;

            X.Msg_Length := To - From - 8;
            X.Msg (1 .. X.Msg_Length) := S (From + 9 .. To);
            Next_String;

         else
            X.Msg_Length := 0;
         end if;

         X.Pid := 0;

         if From <= To and then S (From) = 'P' then
            if S (From .. From + 3) /= "PID:" then
               Bad_EO;
            end if;

            From := From + 5; -- skip past PID: space

            while From <= To loop
               X.Pid := X.Pid * 10 +
                          (Character'Pos (S (From)) - Character'Pos ('0'));
               From := From + 1;
            end loop;

            Next_String;
         end if;

         X.Num_Tracebacks := 0;

         if From <= To then
            if S (From .. To) /= "Call stack traceback locations:" then
               Bad_EO;
            end if;

            Next_String;
            loop
               exit when From > To;

               declare
                  Ch : Character;
                  C  : Integer_Address;
                  N  : Integer_Address;

               begin

                  if S (From) /= '0'
                    or else S (From + 1) /= 'x'
                  then
                     Bad_EO;
                  else
                     From := From + 2;
                  end if;

                  C := 0;
                  while From <= To loop
                     Ch := S (From);

                     if Ch in '0' .. '9' then
                        N :=
                          Character'Pos (S (From)) - Character'Pos ('0');

                     elsif Ch in 'a' .. 'f' then
                        N :=
                          Character'Pos (S (From)) - Character'Pos ('a') + 10;

                     elsif Ch = ' ' then
                        From := From + 1;
                        exit;

                     else
                        Bad_EO;
                     end if;

                     C := C * 16 + N;

                     From := From + 1;
                  end loop;

                  if X.Num_Tracebacks = Max_Tracebacks then
                     Bad_EO;
                  end if;

                  X.Num_Tracebacks := X.Num_Tracebacks + 1;
                  X.Tracebacks (X.Num_Tracebacks) := To_Address (C);
               end;
            end loop;
         end if;

         return X;
      end if;
   end String_To_EO;

   -------------------------
   -- Unhandled_Exception --
   -------------------------

   procedure Unhandled_Exception is
   begin
      null;
   end Unhandled_Exception;

   ----------------------
   -- Notify_Exception --
   ----------------------

   procedure Notify_Exception
     (Id        : Exception_Id;
      Handler   : Code_Loc;
      Is_Others : Boolean)
   is
   begin
      null;
   end Notify_Exception;

   -----------------------------------
   -- Unhandled_Exception_Terminate --
   -----------------------------------

   adafinal_Called : Boolean := False;
   --  Used to prevent recursive call to adafinal in the event that
   --  adafinal processing itself raises an unhandled exception.

   type Subprogram_Ptr is access procedure;
   adafinal_Ptr : Subprogram_Ptr;
   pragma Import (C, adafinal_Ptr, "__gl_adafinal_ptr");
   --  Used to get hold of adafinal address in generated binder program

   type FILEs is new System.Address;
   type int is new Integer;

   procedure Unhandled_Exception_Terminate is
      Excep : constant EOA    := Get_Current_Excep.all;
      Msg   : constant String := Exception_Message (Excep.all);
      Nline : constant String := String'(1 => ASCII.LF);

      procedure To_Stderr (S : String);
      --  Little routine to output string to stderr

      procedure To_Stderr (S : String) is
         procedure put_char_stderr (C : int);
         pragma Import (C, put_char_stderr, "put_char_stderr");

      begin
         for J in 1 .. S'Length loop
            if S (J) /= ASCII.CR then
               put_char_stderr (Character'Pos (S (J)));
            end if;
         end loop;
      end To_Stderr;

   --  Start of processing for Unhandled_Exception_Terminate

   begin
      --  First call adafinal

      if not adafinal_Called then
         adafinal_Called := True;
         adafinal_Ptr.all;
      end if;

      --  Check for special case of raising _ABORT_SIGNAL, which is not
      --  really an exception at all. We recognize this by the fact that
      --  it is the only exception whose name starts with underscore.

      if Exception_Name (Excep.all) (1) = '_' then
         To_Stderr (Nline);
         To_Stderr ("Execution terminated by abort of environment task");
         To_Stderr (Nline);

      --  If no tracebacks, we print the unhandled exception in the old style
      --  (i.e. the style used before ZCX was implemented). We do this to
      --  retain compatibility, especially with the nightly scripts, but
      --  this can be removed at some point ???

      elsif Excep.Num_Tracebacks = 0 then
         To_Stderr (Nline);
         To_Stderr ("raised ");
         To_Stderr (Exception_Name (Excep.all));

         if Msg'Length /= 0 then
            To_Stderr (" : ");
            To_Stderr (Msg);
         end if;

         To_Stderr (Nline);

      --  New style, zero cost exception case

      else
         To_Stderr (Nline);
         To_Stderr ("Execution terminated by unhandled exception");
         To_Stderr (Nline);
         To_Stderr (Exception_Information (Excep.all));
      end if;

      --  Perform system dependent shutdown code

      declare
         procedure Unhandled_Terminate;
         pragma No_Return (Unhandled_Terminate);
         pragma Import
           (C, Unhandled_Terminate, "__gnat_unhandled_terminate");

      begin
         Unhandled_Terminate;
      end;

   end Unhandled_Exception_Terminate;

   ------------------------------
   -- Raise_Exception_No_Defer --
   ------------------------------

   procedure Raise_Exception_No_Defer
     (E       : in Exception_Id;
      Message : in String := "")
   is
      Len : constant Natural :=
              Natural'Min (Message'Length, Exception_Msg_Max_Length);

      Excep : constant EOA := Get_Current_Excep.all;

   begin
      Excep.Msg_Length := Len;
      Excep.Msg (1 .. Len) := Message (1 .. Len);
      Excep.Id             := E;
      Excep.Num_Tracebacks := 0;
      Excep.Cleanup_Flag   := False;
      Excep.Pid            := Local_Partition_ID;

      --  DO NOT CALL Abort_Defer.all; !!!!

      Raise_Current_Excep (E);
   end Raise_Exception_No_Defer;

   ---------
   -- ZZZ --
   ---------

   --  This dummy procedure gives us the end of the PC range for addresses
   --  within the exception unit itself. We hope that gigi/gcc keeps all the
   --  procedures in their original order!

   procedure ZZZ is
   begin
      null;
   end ZZZ;

begin
   --  Allocate the Non-Tasking Machine_State

   Set_Machine_State_Addr_NT (Allocate_Machine_State);
end Ada.Exceptions;
