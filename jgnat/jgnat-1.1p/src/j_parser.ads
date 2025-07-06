------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J _ P A R S E R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.2 $                             --
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

--  This package is intended to be a small parser for java files.
--  It ignores statements and concentrates only on the declarations.

with J_Types;

package J_Parser is

   procedure Parse_File (Filename : String;
                         Stream   : J_Types.Stream_Of_U1);
   --  Parse the java file Filename, and creates a table for quick
   --  references later. This table is deleted and recreated each
   --  time this function is called with a different Filename
   --
   --  If Stream is of null length, we try to open the file as a normal file.
   --  If Stream is not null, the file is supposed to be found in an archive

   function Parameter_Name (Func_Name  : String;
                            Descriptor : String;
                            Param      : J_Types.U2)
                            return       String;
   --  Return the name for the Param-nth parameter if function Func_Name in
   --  the file previously parsed by Parse_File.


end J_Parser;
