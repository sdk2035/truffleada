------------------------------------------------------------------------------
--                                                                          --
--                  JGNAT RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                 S Y S T E M . T A S K _ P R I M I T I V E S              --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.4 $
--                                                                          --
--           Copyright (C) 1998-1999 Ada Core Technologies, Inc.            --
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

--  This package provides low-level support for most tasking features.

with Interfaces.Java.Lang.Object;

package System.Task_Primitives is

   type Lock is limited private;
   type Lock_Ptr is access all Lock;
   --  Should be used for implementation of protected objects.

   type RTS_Lock is limited private;
   type RTS_Lock_Ptr is access all RTS_Lock;
   --  Should be used inside the runtime system.
   --  The difference between Lock and the RTS_Lock is that the later
   --  one serves only as a semaphore so that do not check for
   --  ceiling violations.

   type Task_Body_Access is access procedure;
   --  Pointer to the task body's entry point (or possibly a wrapper
   --  declared local to the GNARL).

   type Private_Data is limited private;
   --  Any information that the GNULLI needs maintained on a per-task
   --  basis.  A component of this type is guaranteed to be included
   --  in the Ada_Task_Control_Block.

private

   type Private_Task_Serial_Number is mod 2 ** 64;
   --  Used to give each task a unique serial number.

   type RTS_Lock is new Interfaces.Java.Lang.Object.Ref;
   type Owner_ID is access all Integer;

   procedure monitorenter (Lock : RTS_Lock);
   pragma Import (Asm, monitorenter);

   procedure monitorexit (Lock : RTS_Lock);
   pragma Import (Asm, monitorexit);

   type Lock is record
      L : aliased RTS_Lock;
      Ceiling : System.Any_Priority := System.Any_Priority'First;
      Saved_Priority : System.Any_Priority := System.Any_Priority'First;
      Owner : Owner_ID;
      Next  : Lock_Ptr;
      Level : Private_Task_Serial_Number := 0;
      Buddy : Owner_ID;
      Frozen : Boolean := False;
   end record;

   type Private_Data is record
      Thread : Interfaces.Java.Lang.Object.Ref;
      pragma Atomic (Thread);

      L      : aliased RTS_Lock;

      Active_Priority : System.Any_Priority := System.Any_Priority'First;
      --  Simulated active priority,
      --  used only if Priority_Ceiling_Support is True.

      Locking : Lock_Ptr;
      Locks   : Lock_Ptr;
      Wakeups : Natural := 0;
   end record;

end System.Task_Primitives;
