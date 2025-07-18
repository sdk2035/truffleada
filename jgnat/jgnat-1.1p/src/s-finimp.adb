------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    S Y S T E M . F I N A L I Z A T I O N _ I M P L E M E N T A T I O N   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.44 $
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

with Ada.Exceptions;
with Ada.Tags;
with Ada.Unchecked_Conversion;
with System.Storage_Elements;
with System.Soft_Links;

package body System.Finalization_Implementation is

   use Ada.Exceptions;
   use System.Finalization_Root;

   package SSL renames System.Soft_Links;

   package SSE renames System.Storage_Elements;
   use type SSE.Storage_Offset;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_Finalizable_Ptr is
     new Ada.Unchecked_Conversion (Address, Finalizable_Ptr);

   function To_Addr is
     new Ada.Unchecked_Conversion (Finalizable_Ptr, Address);

   type RC_Ptr is access all Record_Controller;

   function To_RC_Ptr is
     new Ada.Unchecked_Conversion (Address, RC_Ptr);

   procedure Raise_Exception_No_Defer
     (E       : in Exception_Id;
      Message : in String := "");
   pragma Import (Ada, Raise_Exception_No_Defer,
     "ada__exceptions__raise_exception_no_defer");
   pragma No_Return (Raise_Exception_No_Defer);
   --  Raise an exception without deferring abort. Note that we have to
   --  use this rather kludgy Ada Import interface, since this subprogram
   --  is not available in the visible spec of Ada.Exceptions.

   procedure Raise_From_Finalize
     (L          : Finalizable_Ptr;
      From_Abort : Boolean;
      E_Occ      : Exception_Occurrence);
   --  Deal with an exception raised during finalization of a list. L is a
   --  pointer to the list of element not yet finalized. From_Abort is true
   --  if the finalization actions come from an abort rather than a normal
   --  exit. E_Occ represents the exception being raised.

   function RC_Offset (T : Ada.Tags.Tag) return SSE.Storage_Offset;
   pragma Import (Ada, RC_Offset, "ada__tags__get_rc_offset");

   function Parent_Size (Obj : Address) return SSE.Storage_Count;
   pragma Import (Ada, Parent_Size, "ada__tags__parent_size");

   function Get_RC_Dynamically (Obj : Address) return Address;
   --  Given an the address of an object (obj) of a tagged extension with
   --  controlled component, computes the address of the record controller
   --  located just after the _parent field

   --------------------------
   -- Attach_To_Final_List --
   --------------------------

   procedure Attach_To_Final_List
     (L       : in out Finalizable_Ptr;
      Obj     : in out Finalizable;
      Nb_Link : Short_Short_Integer)
   is
   begin
      --  Simple case: attachement to a one way list

      if Nb_Link = 1 then
         Obj.Next         := L;
         L                := Obj'Unchecked_Access;

      --  Dynamically allocated objects: they are attached to a doubly
      --  linked list, so that an element can be finalized at any moment
      --  by means of an unchecked deallocation. Attachement is
      --  protected against multi-threaded access.

      elsif Nb_Link = 2 then

         Locked_Processing : begin
            SSL.Lock_Task.all;
            Obj.Next    := L.Next;
            Obj.Prev    := L.Next.Prev;
            L.Next.Prev := Obj'Unchecked_Access;
            L.Next      := Obj'Unchecked_Access;
            SSL.Unlock_Task.all;

         exception
            when others =>
               SSL.Unlock_Task.all;
               raise;
         end Locked_Processing;

      --  Attachement of arrays to the final list (used only for objects
      --  returned by function). Obj, in this case is the last element,
      --  but all other elements are already threaded after it. We just
      --  attach the rest of the final list at the end of the array list.

      elsif Nb_Link = 3 then
         declare
            P : Finalizable_Ptr := Obj'Unchecked_Access;

         begin
            while P.Next /= null loop
               P := P.Next;
            end loop;

            P.Next := L;
            L := Obj'Unchecked_Access;
         end;
      end if;

   end Attach_To_Final_List;

   -----------------------------
   -- Detach_From_Final_List --
   -----------------------------

   --  We know that the detach object is neither at the beginning nor at the
   --  end of the list, thank's to the dummy First and Last Elements but the
   --  object may not be attached at all if it is Finalize_Sortage_Only

   procedure Detach_From_Final_List (Obj : in out Finalizable) is
   begin

      --  When objects are not properly attached to a doubly linked
      --  list do not try to detach them. The only case where it can
      --  happen is when dealing with Finalize_Storage_Only objects
      --  which are not always attached.

      if Obj.Next /= null and then Obj.Prev /= null then
         SSL.Lock_Task.all;
         Obj.Next.Prev := Obj.Prev;
         Obj.Prev.Next := Obj.Next;
         SSL.Unlock_Task.all;
      end if;

   exception
      when others =>
         SSL.Unlock_Task.all;
         raise;
   end Detach_From_Final_List;

   --------------------------
   --  Raise_From_Finalize --
   --------------------------

   procedure Raise_From_Finalize
     (L          : Finalizable_Ptr;
      From_Abort : Boolean;
      E_Occ      : Exception_Occurrence)
   is
      Msg : constant String := Exception_Message (E_Occ);
      P   : Finalizable_Ptr := L;
      Q   : Finalizable_Ptr;

   begin
      --  We already got an exception. We now finalize the remainder of
      --  the list, ignoring all further exceptions.

      while P /= null loop
         Q := P.Next;

         begin
            Finalize (P.all);
         exception
            when others => null;
         end;

         P := Q;
      end loop;

      --  If finalization from an Abort, then nothing to do

      if From_Abort then
         null;

      --  If no message, then add our own message saying what happened

      elsif Msg = "" then
         Raise_Exception_No_Defer
           (E       => Program_Error'Identity,
            Message => "exception " &
                       Exception_Name (E_Occ) &
                       " raised during finalization");

      --  If there was a message, pass it on

      else
         Raise_Exception_No_Defer (Program_Error'Identity, Msg);
      end if;
   end Raise_From_Finalize;

   -------------------
   -- Finalize_List --
   -------------------

   procedure Finalize_List (L : Finalizable_Ptr) is
      P : Finalizable_Ptr := L;
      Q : Finalizable_Ptr;

      type Fake_Exception_Occurence is record
         Id : Exception_Id;
      end record;
      type Ptr is access all Fake_Exception_Occurence;

      --  Let's get the current exception before starting to finalize in
      --  order to check if we are in the abort case if an exception is
      --  raised.

      function To_Ptr is new
         Ada.Unchecked_Conversion (Exception_Occurrence_Access, Ptr);
      X : Exception_Id :=
        To_Ptr (System.Soft_Links.Get_Current_Excep.all).Id;

   begin
      while P /= null loop
         Q := P.Next;
         Finalize (P.all);
         P := Q;
      end loop;

   exception
      when E_Occ : others =>
         Raise_From_Finalize (
           Q,
           X = Standard'Abort_Signal'Identity,
           E_Occ);
   end Finalize_List;

   --------------------------
   -- Finalize_Global_List --
   --------------------------

   procedure Finalize_Global_List is
   begin
      --  There are three case here:
      --  a. the application uses tasks, in which case Finalize_Global_Tasks
      --     will defer abortion
      --  b. the application doesn't use tasks but uses other tasking
      --     constructs, such as ATCs and protected objects. In this case,
      --     the binder will call Finalize_Global_List instead of
      --     Finalize_Global_Tasks, letting abort undeferred, and leading
      --     to assertion failures in the GNULL
      --  c. the application doesn't use any tasking construct in which case
      --     deferring abort isn't necessary.
      --
      --  Until another solution is found to deal with case b, we need to
      --  call abort_defer here to pass the checks, but we do not need to
      --  undefer abortion, since Finalize_Global_List is the last procedure
      --  called before exiting the partition.

      SSL.Abort_Defer.all;
      Finalize_List (Global_Final_List);
   end Finalize_Global_List;

   ------------------
   -- Finalize_One --
   ------------------

   procedure Finalize_One (Obj : in out  Finalizable) is
   begin
      Detach_From_Final_List (Obj);
      Finalize (Obj);

   exception
      when E_Occ : others => Raise_From_Finalize (null, False, E_Occ);
   end Finalize_One;

   ------------------------
   -- Get_RC_Dynamically --
   ------------------------

   function Get_RC_Dynamically (Obj : Address) return Address is

      --  define a faked record controller to avoid generating
      --  unnecessary expanded code for controlled types

      type Faked_Record_Controller is record
         Tag, Prec, Next : Address;
      end record;

      --  Reconstruction of a type with characteristics
      --  comparable to the original type

      D : constant := Storage_Unit - 1;

      type Faked_Type_Of_Obj is record
         Parent : SSE.Storage_Array
           (1 .. (Parent_Size (Obj) + D) / Storage_Unit);
         Controller : Faked_Record_Controller;
      end record;

      type Obj_Ptr is access all Faked_Type_Of_Obj;
      function To_Obj_Ptr is new Ada.Unchecked_Conversion (Address, Obj_Ptr);

   begin
      return To_Obj_Ptr (Obj).Controller'Address;
   end Get_RC_Dynamically;

   --------------------------
   --  Deep_Tag_Initialize --
   --------------------------

   procedure Deep_Tag_Initialize
     (L : in out SFR.Finalizable_Ptr;
      A :        System.Address;
      B :        Short_Short_Integer)
   is
      V      : constant SFR.Finalizable_Ptr := To_Finalizable_Ptr (A);
      Offset : constant SSE.Storage_Offset := RC_Offset (V'Tag);

      Controller : RC_Ptr;

   begin
      --  This procedure should not be called if the object has no
      --  controlled components

      if Offset = 0 then

         raise Program_Error;

      --  Has controlled components

      else
         if Offset > 0 then
            Controller := To_RC_Ptr (A + Offset);
         else
            Controller := To_RC_Ptr (Get_RC_Dynamically (A));
         end if;
      end if;

      Initialize (Controller.all);
      Attach_To_Final_List (L, Controller.all, B);

      --  Is controlled

      if V.all in Finalizable then
         Initialize (V.all);
         Attach_To_Final_List (Controller.F, Finalizable (Controller.all), 1);
      end if;
   end Deep_Tag_Initialize;

   ----------------------
   --  Deep_Tag_Adjust --
   ----------------------

   procedure Deep_Tag_Adjust
     (L : in out SFR.Finalizable_Ptr;
      A : System.Address;
      B : Short_Short_Integer)
   is
      V      : constant SFR.Finalizable_Ptr := To_Finalizable_Ptr (A);
      Offset : constant SSE.Storage_Offset := RC_Offset (V'Tag);

      Controller : RC_Ptr;

   begin
      --  Has controlled components

      if Offset /= 0 then
         if Offset > 0 then
            Controller := To_RC_Ptr (A + Offset);
         else
            Controller := To_RC_Ptr (Get_RC_Dynamically (A));
         end if;

         Adjust (Controller.all);
         Attach_To_Final_List (L, Controller.all, B);

      --  Is controlled

      elsif V.all in Finalizable then
         Adjust (V.all);
         Attach_To_Final_List (L, Finalizable (V.all), 1);
      end if;
   end Deep_Tag_Adjust;

   ------------------------
   --  Deep_Tag_Finalize --
   ------------------------

   procedure Deep_Tag_Finalize
     (L : in out SFR.Finalizable_Ptr;
      A : System.Address;
      B : Boolean)
   is
      V      : constant SFR.Finalizable_Ptr := To_Finalizable_Ptr (A);
      Offset : constant SSE.Storage_Offset := RC_Offset (V'Tag);

      Controller : RC_Ptr;

   begin
      --  Has controlled components

      if Offset /= 0 then
         if Offset > 0 then
            Controller := To_RC_Ptr (A + Offset);
         else
            Controller := To_RC_Ptr (Get_RC_Dynamically (A));
         end if;

         if B then
            Finalize_One (Controller.all);
         else
            Finalize (Controller.all);
         end if;

      --  Is controlled

      elsif V.all in Finalizable then
         if B then
            Finalize_One (V.all);
         else
            Finalize (V.all);
         end if;
      end if;
   end Deep_Tag_Finalize;

   ----------------------
   --  Deep_Tag_Attach --
   -----------------------

   procedure Deep_Tag_Attach
     (L : in out SFR.Finalizable_Ptr;
      A : System.Address;
      B : Short_Short_Integer)
   is
      V      : constant SFR.Finalizable_Ptr := To_Finalizable_Ptr (A);
      Offset : constant SSE.Storage_Offset  := RC_Offset (V'Tag);

      Controller : RC_Ptr;

   begin
      if Offset /= 0 then
         if Offset > 0 then
            Controller := To_RC_Ptr (A + Offset);
         else
            Controller := To_RC_Ptr (Get_RC_Dynamically (A));
         end if;

         Attach_To_Final_List (L, Controller.all, B);

      --  Is controlled

      elsif V.all in Finalizable then
         Attach_To_Final_List (L, V.all, B);
      end if;
   end Deep_Tag_Attach;

   ----------------------------------
   -- Record_Controller Management --
   ----------------------------------

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Limited_Record_Controller) is
   begin
      null;
   end Initialize;

   procedure Initialize (Object : in out Record_Controller) is
   begin
      Object.My_Address := Object'Address;
   end Initialize;

   -------------
   --  Adjust --
   -------------

   procedure Adjust (Object : in out Record_Controller) is

      First_Comp : Finalizable_Ptr;
      My_Offset : constant SSE.Storage_Offset :=
                    Object.My_Address - Object'Address;

      procedure Ptr_Adjust (Ptr : in out Finalizable_Ptr);
      --  Substract the offset to the pointer

      procedure Reverse_Adjust (P : Finalizable_Ptr);
      --  Ajust the components in the reverse order in which they are stored
      --  on the finalization list. (Adjust and Finalization are not done in
      --  the same order)

      procedure Ptr_Adjust (Ptr : in out Finalizable_Ptr) is
      begin
         if Ptr /= null then
            Ptr := To_Finalizable_Ptr (To_Addr (Ptr) - My_Offset);
         end if;
      end Ptr_Adjust;

      procedure Reverse_Adjust (P : Finalizable_Ptr) is
      begin
         if P /= null then
            Ptr_Adjust (P.Next);
            Reverse_Adjust (P.Next);
            Adjust (P.all);
            Object.F := P;   --  Successfully adjusted, so place in list.
         end if;
      end Reverse_Adjust;

   --  Start of processing for Adjust

   begin
      --  Adjust the components and their finalization pointers next.
      --  We must protect against an exception in some call to Adjust, so
      --  we keep pointing to the list of successfully adjusted components,
      --  which can be finalized if an exception is raised.

      First_Comp := Object.F;
      Object.F := null;               --  nothing adjusted yet.
      Ptr_Adjust (First_Comp);        --  set addresss of first component.
      Reverse_Adjust (First_Comp);

      --  Then Adjust the controller itself

      Object.My_Address := Object'Address;

   exception
      when others =>
         --  Finalize those components that were successfully adjusted, and
         --  propagate exception. The object itself is not yet attached to
         --  global finalization list, so we cannot rely on the outer call
         --  to Clean to take care of these components.

         Finalize (Object);
         raise;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize   (Object : in out Limited_Record_Controller) is
   begin
      Finalize_List (Object.F);
   end Finalize;

end System.Finalization_Implementation;
