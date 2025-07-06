------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            G N A T . D I R E C T O R Y _ O P E R A T I O N S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.7 $
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

--  Directory operations

--  This package provides routines for manipulating directories. A directory
--  can be treated as a file, using open and close routines, and a scanning
--  routine is provided for iterating through the entries in a directory.

package GNAT.Directory_Operations is

   subtype Dir_Name_Str is String;
   --  A subtype used in this package to represent string values that are
   --  directory names. A directory name is a prefix for files that appear
   --  with in the directory. This means that for Unix systems, the string
   --  includes a final '/', and for DOS-like systems, it includes a final
   --  '\' character. It can also include drive letters if the operating
   --  system provides for this. The final '/' or '\' in a Dir_Name_Str is
   --  optional when passed as a procedure or function in parameter.

   type Dir_Type is limited private;
   --  A value used to reference a directory. Conceptually this value includes
   --  the identity of the directory, and a sequential position within it.

   Null_Dir : constant Dir_Type;
   --  Represent the value for an uninitialized or closed directory.

   Directory_Error : exception;
   --  Exception raised if the directory cannot be opened, read, closed,
   --  created or if it is not possible to change the current execution
   --  environment directory.

   procedure Change_Dir (Dir_Name : Dir_Name_Str);
   --  Changes the working directory of the current execution environment
   --  to the directory named by Dir_Name.
   --
   --  Raises Directory_Error if Dir_Name does not exist.

   procedure Make_Dir (Dir_Name : Dir_Name_Str);
   --  Create a new directory named Dir_Name.
   --
   --  Raises Directory_Error if Dir_Name cannot be created.

   function Get_Current_Dir return Dir_Name_Str;
   --  Returns the current working directory for the execution environment.

   procedure Get_Current_Dir (Dir : out Dir_Name_Str; Last : out Natural);
   --  Returns the current working directory for the execution
   --  environment. The name is returned in Dir_Name; Last is the index in
   --  Dir_Name such that Dir_Name (Last) is the last character written. If
   --  Dir_Name is too small for the directory name, the name will be
   --  truncated before beeing copied to Dir_Name.

   procedure Open (Dir : out Dir_Type; Dir_Name : in Dir_Name_Str);
   --  Opens the directory named by Dir_Name and returns a Dir_Type value
   --  that refers to this directory, and is positioned at the first entry.
   --
   --  Raises Directory_Error if Dir_Name cannot be accessed. In that case
   --  Dir will be set to Null_Dir.

   procedure Close (Dir : in out Dir_Type);
   --  Closes the directory stream refered to by Dir. After calling Close
   --  Is_Open will return False. Dir will be set to Null_Dir.
   --
   --  Raises Directory_Error if Dir has not be opened (Dir = Null_Dir).

   function Is_Open (Dir : Dir_Type) return Boolean;
   --  Returns True if Dir is open, or False otherwise.

   procedure Read
     (Dir  : in out Dir_Type;
      Str  : out String;
      Last : out Natural);
   --  Reads the next entry from the directory and sets Str to the name
   --  of that entry. Last is the index in Str such that Str (Last) is the
   --  last character written. Last is 0 when there is no more file in the
   --  directory. If Str is too small for the file name, the file name will
   --  be truncated before beeing copied to Str. The list of files returned
   --  includes directories in systems providing a hierarchical directory
   --  structure, including . (the current directory) and .. (the parent
   --  directory) in systems providing these entries. The directory is
   --  returned in target-OS form.
   --
   --  Raises Directory_Error if Dir has not be opened (Dir = Null_Dir).

   function Read_Is_Thread_Safe return Boolean;
   --  Indicates if procedure Read is thread safe. On systems where the
   --  target system supports this functionality, Read is thread safe,
   --  and this function returns True (e.g. this will be the case on any
   --  Unix or Unix-like system providing a correct implementation of the
   --  function readdir_r). If the system cannot provide a thread safe
   --  implementation of Read, then this function returns False.

private

   type Dir_Type_Value;
   type Dir_Type is access Dir_Type_Value;

   Null_Dir : constant Dir_Type := null;

end GNAT.Directory_Operations;
