------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       G N A T . T A S K _ L O C K                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $
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

--  Simple task lock and unlock routines

--  A small package containing a task lock and unlock routines for creating
--  a critical region. The lock involved is a global lock, shared by all
--  tasks, and by all calls to these routines, so these routines should be
--  used with care to avoid unnecessary reduction of concurrency.

package GNAT.Task_Lock is
pragma Elaborate_Body (Task_Lock);

   procedure Lock;
   pragma Inline (Lock);
   --  Acquires the global lock, starts the execution of a critical region
   --  which no other task can enter until the locking task calls Unlock

   procedure Unlock;
   pragma Inline (Unlock);
   --  Releases the global lock, allowing another task to successfully
   --  complete a Lock operation. Terminates the critical region.

   --  The recommended protocol for using these two procedures is as
   --  follows:

   --    Locked_Processing : begin
   --       Lock;
   --       ...
   --       TSL.Unlock;
   --
   --    exception
   --       when others =>
   --          Unlock;
   --          raise;
   --    end Locked_Processing;

   --  This ensures that the lock is not left set if an exception is raised
   --  explicitly or implicitly during the critical locked region.

end GNAT.Task_Lock;
