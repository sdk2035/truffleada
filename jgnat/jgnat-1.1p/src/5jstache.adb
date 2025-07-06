------------------------------------------------------------------------------
--                                                                          --
--                  JGNAT RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                  S Y S T E M . S T A C K _ C H E C K I N G               --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.2 $
--                                                                          --
--                Copyright (C) 1999 Ada Core Technologies, Inc.            --
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

--  This is a dummy version of this package for use with JGNAT.

with System.Storage_Elements; use System.Storage_Elements;

package body System.Stack_Checking is

   --  This package is not needed by the JGNAT implementation so
   --  operations are empty or return Null_Stack.

   function Set_Stack_Info (Stack : access Stack_Access) return Stack_Access;


   -----------------
   -- Stack_Check --
   -----------------

   function Stack_Check (Stack_Address : System.Address) return Stack_Access is
   begin
      return Null_Stack;
   end Stack_Check;


   ----------------------------
   -- Invalidate_Stack_Cache --
   ----------------------------

   procedure Invalidate_Stack_Cache (Any_Stack : Stack_Access) is
   begin
      null;
   end Invalidate_Stack_Cache;


   --------------------
   -- Set_Stack_Info --
   --------------------

   function Set_Stack_Info (Stack : access Stack_Access) return Stack_Access is
   begin
      return Null_Stack;
   end Set_Stack_Info;

   --------------------
   -- Set_Stack_Size --
   --------------------

   procedure Set_Stack_Size
     (Stack_Size : System.Storage_Elements.Storage_Offset)
   is
   begin
      null;
   end Set_Stack_Size;

   --  Specify the stack size for the current frame.

   ------------------------
   -- Update_Stack_Cache --
   ------------------------

   procedure Update_Stack_Cache (Stack : Stack_Access) is
   begin
      null;
   end Update_Stack_Cache;

end System.Stack_Checking;
