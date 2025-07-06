------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G E T _ T A R G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.3 $                             --
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
-- JGNAT -  The GNAT Ada 95 tool chain for the Java (TM) Virtual Machine is --
--          maintained by Ada Core Technologies, Inc. - http://www.gnat.com --
--                                                                          --
------------------------------------------------------------------------------

--  This is the JGNAT target dependent version of package Get_Targ.  This
--  package provides the values related to types on the Java Virtual Machine
--  (the target system).  It is only needed for the elaboration of ttypes.

with Types; use Types;

package Get_Targ is
pragma Preelaborate (Get_Targ);

   Get_Bits_Per_Unit          : constant :=  8;
   Get_Bits_Per_Word          : constant := 32;
   Get_Char_Size              : constant :=  8;
   Get_Wide_Char_Size         : constant := 16;
   Get_Short_Size             : constant := 16;
   Get_Int_Size               : constant := 32;
   Get_Long_Size              : constant := 64;
   Get_Long_Long_Size         : constant := 64;
   Get_Float_Size             : constant := 32;
   Get_Double_Size            : constant := 64;
   Get_Long_Double_Size       : constant := 64;
   Get_Pointer_Size           : constant := 32;
   Get_Maximum_Alignment      : constant :=  4;
   Get_Float_Words_BE         : constant :=  1;
   Get_Words_BE               : constant :=  1;
   Get_Bytes_BE               : constant :=  1;
   Get_Bits_BE                : constant :=  1;
   Get_Strict_Alignment       : constant :=  1;
   Get_Max_Priority           : constant := 30;
   Get_Max_Interrupt_Priority : constant := 31;

   function Width_From_Size  (Size : Pos) return Pos;
   function Digits_From_Size (Size : Pos) return Pos;
   --  Calculate values for 'Width or 'Digits from 'Size

end Get_Targ;
