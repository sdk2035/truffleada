------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                            G N A T .  T A B L E                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.6 $
--                                                                          --
--            Copyright (C) 1998-1999 Ada Core Technologies, Inc.           --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with System; use System;

package body GNAT.Table is

   Min : constant Integer := Integer (Table_Low_Bound);
   --  Subscript of the minimum entry in the currently allocated table

   Max : Integer;
   --  Subscript of the maximum entry in the currently allocated table

   Length : Integer := 0;
   --  Number of entries in currently allocated table. The value of zero
   --  ensures that we initially allocate the table.

   Last_Val : Integer;
   --  Current value of Last.

   type size_t is new Integer;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Reallocate;
   --  Reallocate the existing table according to the current value stored
   --  in Max. Works correctly to do an initial allocation if the table
   --  is currently null.

   --------------
   -- Allocate --
   --------------

   function Allocate (Num : Integer := 1) return Table_Index_Type is
      Old_Last : constant Integer := Last_Val;

   begin
      Last_Val := Last_Val + Num;

      if Last_Val > Max then
         Reallocate;
      end if;

      return Table_Index_Type (Old_Last + 1);
   end Allocate;

   --------------------
   -- Decrement_Last --
   --------------------

   procedure Decrement_Last is
   begin
      Last_Val := Last_Val - 1;
   end Decrement_Last;

   --------------------
   -- Increment_Last --
   --------------------

   procedure Increment_Last is
   begin
      Last_Val := Last_Val + 1;

      if Last_Val > Max then
         Reallocate;
      end if;
   end Increment_Last;

   ----------
   -- Init --
   ----------

   procedure Init is
      Old_Length : Integer := Length;

   begin
      Last_Val := Min - 1;
      Max      := Min + Table_Initial - 1;
      Length   := Max - Min + 1;

      --  If table is same size as before (happens when table is never
      --  expanded which is a common case), then simply reuse it. Note
      --  that this also means that an explicit Init call right after
      --  the implicit one in the package body is harmless.

      if Old_Length = Length then
         return;

      --  Otherwise we can use Reallocate to get a table of the right size.
      --  Note that Reallocate works fine to allocate a table of the right
      --  initial size when it is first allocated.

      else
         Reallocate;
      end if;
   end Init;

   ----------
   -- Last --
   ----------

   function Last return Table_Index_Type is
   begin
      return Table_Index_Type (Last_Val);
   end Last;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate is

      function realloc
        (memblock : Table_Ptr;
         size     : size_t)
         return     Table_Ptr;
      pragma Import (C, realloc);

      function malloc
        (size     : size_t)
         return     Table_Ptr;
      pragma Import (C, malloc);

      New_Size : size_t;

   begin
      if Max < Last_Val then
         pragma Assert (not Locked);

         while Max < Last_Val loop

            --  Increase length using the table increment factor, but make
            --  sure that we add at least ten elements (this avoids a loop
            --  for silly small increment values)

            Length := Integer'Max
                        (Length * (100 + Table_Increment) / 100,
                         Length + 10);
            Max := Min + Length - 1;
         end loop;
      end if;

      New_Size :=
        size_t ((Max - Min + 1) *
                (Table_Type'Component_Size / Storage_Unit));

      if Table = null then
         Table := malloc (New_Size);

      elsif New_Size > 0 then
         Table :=
           realloc
             (memblock => Table,
              size     => New_Size);
      end if;

      if Length /= 0 and then Table = null then
         raise Storage_Error;
      end if;

   end Reallocate;

   -------------
   -- Release --
   -------------

   procedure Release is
   begin
      Length := Last_Val - Integer (Table_Low_Bound) + 1;
      Max    := Last_Val;
      Reallocate;
   end Release;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (New_Val : Table_Index_Type) is
      Old_Last : Integer;

   begin
      if Integer (New_Val) < Last_Val then
         Last_Val := Integer (New_Val);
      else
         Old_Last := Last_Val;
         Last_Val := Integer (New_Val);

         if Last_Val > Max then
            Reallocate;
         end if;
      end if;
   end Set_Last;

begin
   Init;
end GNAT.Table;
