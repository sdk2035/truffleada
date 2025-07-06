------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             B A C K _ E N D                              --
--                                                                          --
--                                 B o d y                                  --
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

--  This is the JGNAT target dependent version of procedure Back_End.

with Debug;
with Jx_Drive;
with Lib;
with Opt;
with Output;  use Output;
with Types;

package body Back_End is

   procedure Call_Back_End (Mode : Back_End_Mode_Type) is
      pragma Warnings (Off, Mode);
   begin
      if (Opt.Verbose_Mode or Opt.Full_List)
        and then (not Debug.Debug_Flag_7)
      then
         Write_Eol;
         Write_Str ("JGNAT,");
         Write_Str (" Copyright 1998-1999 Ada Core Technologies, Inc.");
         Write_Str (" (http://www.gnat.com)");
         Write_Eol;
         Write_Eol;
      end if;

      Jx_Drive.GNAT_To_JVM (Lib.Cunit (Types.Main_Unit));
   end Call_Back_End;

end Back_End;
