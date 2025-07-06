------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S T O R A G E _ E L E M E N T S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $                              --
--                                                                          --
--           Copyright (C) 1998-2000 Ada Core Technologies, Inc.            --
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

package body System.Storage_Elements is

   pragma Suppress (All_Checks);

   --  Address arithmetic

   function "+" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return Null_Address;
   end "+";

   function "+" (Left : Storage_Offset; Right : Address) return Address is
   begin
      return Null_Address;
   end "+";

   function "-" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return Null_Address;
   end "-";

   function "-" (Left, Right : Address) return Storage_Offset is
   begin
      return 0;
   end "-";

   function "mod" (Left : Address; Right : Storage_Offset)
     return Storage_Offset is
   begin
      return 0;
   end "mod";

   --  Conversion to/from integers

   function To_Address (Value : Integer_Address) return Address is
   begin
      return Null_Address;
   end To_Address;

   function To_Integer (Value : Address) return Integer_Address is
      function Hash_Code (A : Address) return Integer;
      pragma Import (C, Hash_Code, "hash_code");
   begin
      --  Hash_Code will blow up if passed a null address, so we check
      --  for null and simply return 0.

      if Value = Null_Address then
         return 0;
      else
         return Integer_Address (Hash_Code (Value));
      end if;
   end To_Integer;

end System.Storage_Elements;
