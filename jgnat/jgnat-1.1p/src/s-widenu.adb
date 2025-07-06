------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                      S Y S T E M . W I D _ E N U M                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.7 $                              --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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

with Unchecked_Conversion;

package body System.Wid_Enum is

   -----------------------
   -- Width_Enumeration --
   -----------------------

   function Width_Enumeration
     (Table  : Address;
      Lo, Hi : Natural)
      return   Natural
   is
      type String_Access is access String;
      for String_Access'Size use Standard'Address_Size;

      type Enum_Table is array (Natural) of String_Access;

      type Enum_Table_Ptr is access Enum_Table;
      function A_To_T is new Unchecked_Conversion (Address, Enum_Table_Ptr);

      T : constant Enum_Table_Ptr := A_To_T (Table);
      W : Natural;

   begin
      W := 0;

      for J in Lo .. Hi loop
         W := Natural'Max (W, T (J)'Length);
      end loop;

      return W;
   end Width_Enumeration;

end System.Wid_Enum;
