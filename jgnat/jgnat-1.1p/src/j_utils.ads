------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J _ U T I L S                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                             $Revision: 1.14 $                             --
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
-- JGNAT -  The GNAT Ada 95 tool chain for the Java (TM) Virtual Machine is --
--          maintained by Ada Core Technologies, Inc. - http://www.gnat.com --
--                                                                          --
------------------------------------------------------------------------------

--  This file contains general utilities used by the JGNAT tools.

--  WARNING: this package should NOT be used in the JGNAT compiler
--  since this package drags in a number of Ada library units which
--  would create unnecessary dependences on the Ada library.

with J_Types;  use J_Types;
with JVM_File; use JVM_File;
with J_Zip;

package J_Utils is

   ----------------------
   -- Message Handling --
   ----------------------

   procedure Set_Message_Prefix (S : String);
   --  Sets the prefix string that is displayed by routines Fatal_Error and
   --  Print_Msg before displaying any error message. Before calling this
   --  routine the prefix is the empty string.

   procedure Fatal_Error (S : String);
   --  Prints S prefixed with the string which was passed to the last call of
   --  Set_Message_Prefix and raises Fatal_Exception.

   Fatal_Exception : exception;

   procedure Print_Msg (S : String);
   --  Same as above, except that no exception is raised

   -----------------------------------
   -- General Class File Processing --
   -----------------------------------

   type Zip_Archive_Access is access J_Zip.Archive_Directory;
   type Archive_Directory_Access is record
      Stream  : Stream_Of_U1_Ptr;
      Archive : Zip_Archive_Access;
   end record;
   function Get_Current_Archive return Archive_Directory_Access;
   --  If a zip archive is being processed by Process_Classes, then this
   --  function will return a pointer to this archive. If no archive is
   --  processed, this function will return null. This function is
   --  typically used inside one of the two generic procedure parameters:
   --  Process_Class or Process_Directory below.

   generic
      Verbose : in out Boolean;
      --  When set procedure Process and Process_File below operate in verbose
      --  mode, i.e. they print a number of infornmative messages to the
      --  standard output.

      with procedure Write_Usage                              is <>;
      with procedure Process_Switches                         is <>;
      with procedure Process_Class     (Bytes : Stream_Of_U1) is <>;
      with procedure Process_Directory (Name  : String)       is <>;

   package Command_Line is
      procedure Process;
      --  This routine completely handles command line processing as follows:
      --
      --    1. First it calls Process_Switches to process all the switches on
      --       the command line.
      --
      --    2. Then it processes every input file found on the command line by
      --       calling routine Process_File below on each such input file. If
      --       no file was given on the command line routine Write_Usage is
      --       called. This routines should print usage information to standard
      --       output.

      procedure Process_File (File_Name : String);
      --  File_Name is either:
      --
      --    (a) the name of a .class file (e.g. "math.class"), or
      --    (b) the name of a .class file without the ".class" suffix
      --        (e.g. "math"), or
      --    (c) the name of a zip archive (e.g. "jre.zip"), or
      --    (d) the name of a .class file contained inside a zip archive
      --        (e.g. "jre.zip/java/lang/math.class").
      --
      --  If the file does not exist the execution is halted and an error
      --  message issued. Otherwise:
      --
      --    1. If the file denotes a a single class file, procedure
      --       Process_Class is invoked on it.

      --    2. If the file denotes a zip archive (possibly organized in several
      --       directories), Process_Class is invoked on each individual .class
      --       file and Process_Directory is invoked on each directory
      --       encountered in the archive.
   end Command_Line;

   ----------------------------------------
   -- Miscellaneous Class File Utilities --
   ----------------------------------------

   function Source_File_Name (CF : Class_File) return Utf8.Table;
   --  Returns the source file name, if any, from which the class file CF was
   --  generated. If no file name is found, returns an empty Utf8 table.

   function Get_Code_Attribute (M : Member_Info) return Member_Attribute_Info;
   --  Returns the code attribute of method M. If no such attribute exists
   --  raise an exception.

   type PC_Src_Map is array (Instruction_Index range <>) of U2;
   function Get_PC_To_Src_Map (C : Code_Attribute.Table) return PC_Src_Map;
   --  Given a method whose code attribute table is C, returns the map between
   --  each bytecode offset (also know as PC) of the method and the number of
   --  the corresponding source line.  Specifically let L be the PC_Src_Map
   --  returned, and let K be a bytecode offset in the current method, then
   --  L (K) is either zero (if no line in the original source maps onto that
   --  offset) or it contains the line number such that the code generated for
   --  it starts at offset K in the method's bytecode. Beware that the last
   --  bytecode offset of the table returned might not be the last bytecode
   --  offset of the method.

   function Get_Var_Info
     (C    : Code_Attribute.Table;
      Var  : Local_Variable_Index;
      PC   : Byte_Index)
      return Variable_Info;
   --  Given the code attribute table C of a method, a local variable slot
   --  number Var in the method as well as a bytecode offset PC, return the
   --  Variable_Info record for the variable in the original source code that
   --  corresponds to Var for that bytecode offset. If no Variable_Info record
   --  is found, return a Variable_Info record where the Name_Index field is
   --  set to CP_Empty.

end J_Utils;
