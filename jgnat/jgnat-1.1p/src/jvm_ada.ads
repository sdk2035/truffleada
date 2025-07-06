------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          J V M 2 A D A _ L I B                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.14 $                             --
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

with Hostparm;
with J_Types;  use J_Types;

package JVM_Ada is

   type String_Ptr is access String;

   ----------------------
   -- Options Handling --
   ----------------------

   Keep_Original_Identifiers : Boolean := False;
   --  When False the identifiers encoutered in JVM .class file are mangled,
   --  whenever needed to turn them in Ada identifiers. When True,
   --  identifiers are left as is.

   Output_Dir : String_Ptr := new String'(Hostparm.Normalized_CWD);
   --  Output directory for Ada files

   Overwrite_Files : Boolean := False;
   --  When set, overwrite any Ada spec file if present

   Quiet_Mode : Boolean := False;
   --  When set, be quiet in the output

   Skip_Sun_Classes : Boolean := True;
   --  When set do not map Sun public classes into Ada Specs

   Verbose_Mode : Boolean := False;
   --  When set, be verbose in the output

   procedure Search_Classes_In (Zip : String);
   --  When looking for a class file search also in archive Zip

   procedure Search_Sources_In (Zip : String);
   --  When looking for a source file search also in archive Zip

   -----------------------
   -- Classes functions --
   -----------------------

   procedure Convert_To_Ada (Bytes : Stream_Of_U1);
   --  Convert the .class file corresponding to Bytes into a .ads file

   procedure Convert_Directory_To_Ada (Name : String);
   --  Creates a very simple .ads file corresponding to a directory in
   --  the Java class tree (like java/lang ...). 'Name' should have
   --  '/' separators between identifiers.

end JVM_Ada;
