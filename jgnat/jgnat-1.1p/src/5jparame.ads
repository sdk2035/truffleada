------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . P A R A M E T E R S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.9 $
--                                                                          --
--           Copyright (C) 1999-2000 Ada Core Technologies, Inc.            --
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

--  This is the JGNAT specific vesrion of System.Parameters.

package System.Parameters is
   pragma Pure;
   pragma Elaborate_Body;

   ---------------------------------------
   -- Task And Stack Allocation Control --
   ---------------------------------------

   type Task_Storage_Size is new Integer;
   --  Type used in tasking units for task storage size

   type Size_Type is new Task_Storage_Size;
   --  Type used to provide task storage size to runtime

   Unspecified_Size : constant Size_Type := Size_Type'First;
   --  Value used to indicate that no size type is set

   subtype Ratio is Size_Type range -1 .. 100;
   Dynamic : constant Size_Type := -1;
   --  Secondary_Stack_Ratio is a constant between 0 and 100 wich
   --  determines the percentage of the allocate task stack that is
   --  used by the secondary stack (the rest being the primary stack).
   --  The special value of minus one indicates that the secondary
   --  stack is to be allocated from the heap instead.

   Sec_Stack_Ratio : constant Ratio := -1;
   --  This constant defines the handling of the secondary stack

   Sec_Stack_Dynamic : constant Boolean := Sec_Stack_Ratio = Dynamic;
   --  Convenient Boolean for testing for dynmaic secondary stack

   function Default_Stack_Size return Size_Type;
   --  Default task stack size used if none is specified

   function Minimum_Stack_Size return Size_Type;
   --  Minimum task stack size permitted

   function Adjust_Storage_Size (Size : Size_Type) return Size_Type;
   --  Given the storage size stored in the TCB, return the Storage_Size
   --  value required by the RM for the Storage_Size attribute. The
   --  required adjustment is as follows:
   --
   --    when Size = Unspecified_Size, return Default_Stack_Size
   --    when Size < Minimum_Stack_Size, return Minimum_Stack_Size
   --    otherwise return given Size

   ----------------------------------------------
   -- Characteristics of types in Interfaces.C --
   ----------------------------------------------

   wchar_t_bits : constant := 16;
   --  Number of bits in C type wchar_t. Used in the definition of type
   --  Interfaces.C.wchar_t, which is an unsigned type of this length.

   long_bits : constant := 64;
   --  Number of bits in type long and unsigned_long

   ----------------------------------------------
   -- Behavior of Pragma Finalize_Storage_Only --
   ----------------------------------------------

   --  Garbage_Collected is a Boolean constant whose value indicates the
   --  effect of the pragma Finalize_Storage_Entry on a controlled type.

   --    Garbage_Collected = False

   --      The system releases all storage on program termination only,
   --      but not other garbage collection occurs, so finalization calls
   --      are ommitted only for outer level onjects can be omitted if
   --      pragma Finalize_Storage_Only is used.

   --    Garbage_Collected = True

   --      The system provides full garbage collection, so it is never
   --      necessary to release storage for controlled objects for which
   --      a pragma Finalize_Storage_Only is used.

   Garbage_Collected : constant Boolean := True;
   --  The storage mode for this system (release on program exit)

end System.Parameters;
