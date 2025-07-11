------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 M A K E                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.18 $                             --
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  The following package implements the facilities to recursively
--  compile (a la make), bind and/or link a set of sources. This package
--  gives the individual routines for performing such tasks as well as
--  the routine gnatmake below that puts it all together.

with GNAT.OS_Lib; use GNAT.OS_Lib;  --  defines Argument_List
with Types;       use Types;

package Make is

   procedure Display_Commands (Display : Boolean := True);
   --  The default behavior of Make commands (Compile_Sources, Bind, Link)
   --  is to display them on stderr. This behavior can be changed repeatedly
   --  by invoking this procedure.

   --  If a compilation, bind or link failed one of the following 3 exceptions
   --  is raised. These need to be handled by the calling routines.

   Compilation_Failed : exception;
   --  Raised by Compile_Sources if a compilation failed.

   Bind_Failed : exception;
   --  Raised by Bind below if the bind failed.

   Link_Failed : exception;
   --  Raised by Link below if the link failed.

   procedure Bind (ALI_File : File_Name_Type; Args : Argument_List);
   --  Binds ALI_File. Args are the arguments to pass to the binder.
   --  Args must have a lower bound of 1.

   procedure Link (ALI_File : File_Name_Type; Args : Argument_List);
   --  Links ALI_File. Args are the arguments to pass to the linker.
   --  Args must have a lower bound of 1.

   procedure Initialize;
   --  Performs default and package initialization. Therefore,
   --  Compile_Sources can be called by an external unit.

   procedure Scan_Make_Arg (Argv : String);
   --  Scan make arguments. Argv is a single argument to be processed.

   procedure Extract_Failure
     (File  : out File_Name_Type;
      Unit  : out Unit_Name_Type;
      Found : out Boolean);
   --  Extracts the first failure report from Bad_Compilation table.

   procedure Compile_Sources
     (Main_Source           : File_Name_Type;
      Args                  : Argument_List;
      First_Compiled_File   : out Name_Id;
      Most_Recent_Obj_File  : out Name_Id;
      Most_Recent_Obj_Stamp : out Time_Stamp_Type;
      Main_Unit             : out Boolean;
      Compilation_Failures  : out Natural;
      Check_Readonly_Files  : Boolean  := False;
      Do_Not_Execute        : Boolean  := False;
      Force_Compilations    : Boolean  := False;
      Keep_Going            : Boolean  := False;
      In_Place_Mode         : Boolean  := False;
      Initialize_ALI_Data   : Boolean  := True;
      Max_Process           : Positive := 1);
   --  Compile_Sources will recursively compile all the sources needed by
   --  Main_Source. Before calling this routine make sure Namet has been
   --  initialized. This routine can be called repeatedly with different
   --  Main_Source file as long as all the source (-I flags), library
   --  (-B flags) and ada library (-A flags) search paths between calls are
   --  *exactly* the same. The default directory must also be the same.
   --
   --    Args contains the arguments to use during the compilations.
   --    The lower bound of Args must be 1.
   --
   --    First_Compiled_File is set to the name of the first file that is
   --    compiled or that needs to be compiled. This is set to No_Name if no
   --    compilations were needed.
   --
   --    Most_Recent_Obj_File is set to the full name of the most recent
   --    object file found when no compilations are needed, that is when
   --    First_Compiled_File is set to No_Name. When First_Compiled_File
   --    is set then Most_Recent_Obj_File is set to No_Name.
   --
   --    Most_Recent_Obj_Stamp is the time stamp of Most_Recent_Obj_File.
   --
   --    Main_Unit is set to True if Main_Source can be a main unit.
   --    If Do_Not_Execute is False and First_Compiled_File /= No_Name
   --    the value of Main_Unit is always False.
   --    Is this used any more??? It is certainly not used by gnatmake???
   --
   --    Compilation_Failures is a count of compilation failures. This count
   --    is used to extract compilation failure reports with Extract_Failure.
   --
   --    Check_Readonly_Files set it to True to compile source files
   --    which library files are read-only. When compiling GNAT predefined
   --    files the "-gnatg" flag is used.
   --
   --    Do_Not_Execute set it to True to find out the first source that
   --    needs to be recompiled, but without recompiling it. This file is
   --    saved in First_Compiled_File.
   --
   --    Force_Compilations forces all compilations no matter what but
   --    recompiles read-only files only if Check_Readonly_Files
   --    is set.
   --
   --    Keep_Going when True keep compiling even in the presence of
   --    compilation errors.
   --
   --    In_Place_Mode when True save library/object files in their object
   --    directory if they already exist; otherwise, in the source directory.
   --
   --    Initialize_ALI_Data set it to True when you want to intialize ALI
   --    data-structures. This is what you should do most of the time.
   --    (especially the first time around when you call this routine).
   --    This parameter is set to False to preserve previously recorded
   --    ALI file data.
   --
   --    Max_Process is the maximum number of processes that should be spawned
   --    to carry out compilations.
   --
   --  Flags in Package Opt Affecting Compile_Sources
   --  -----------------------------------------------
   --
   --    Check_Object_Consistency set it to False to omit all consistency
   --      checks between an .ali file and its corresponding object file.
   --      When this flag is set to true, every time an .ali is read,
   --      package Osint checks that the corresponding object file
   --      exists and is more recent than the .ali.
   --
   --  Use of Name Table Info
   --  ----------------------
   --
   --  All file names manipulated by Compile_Sources are entered into the
   --  Names table. The Byte field of a source file is used to mark it.
   --
   --  Calling Compile_Sources Several Times
   --  -------------------------------------
   --
   --  Upon return from Compile_Sources all the ALI data structures are left
   --  intact for further browsing. HOWEVER upon entry to this routine ALI
   --  data structures are re-initialized if parameter Initialize_ALI_Data
   --  above is set to true. Typically this is what you want the first time
   --  you call Compile_Sources. You should not load an ali file, call this
   --  routine with flag Initialize_ALI_Data set to True and then expect
   --  that ALI information to be around after the call. Note that the first
   --  time you call Compile_Sources you better set Initialize_ALI_Data to
   --  True unless you have called Initialize_ALI yourself.
   --
   --  Compile_Sources ALGORITHM : Compile_Sources (Main_Source)
   --  -------------------------
   --
   --  1. Insert Main_Source in a Queue (Q) and mark it.
   --
   --  2. Let unit.adb be the file at the head of the Q. If unit.adb is
   --     missing but its corresponding ali file is in an Ada library directory
   --     (see below) then, remove unit.adb from the Q and goto step 4.
   --     Otherwise, look at the files under the D (dependency) section of
   --     unit.ali. If unit.ali does not exist or some of the time stamps do
   --     not match, (re)compile unit.adb.
   --
   --     An Ada library directory is a directory containing Ada specs, ali
   --     and object files but no source files for the bodies. An Ada library
   --     directory is communicated to gnatmake by means of some switch so that
   --     gnatmake can skip the sources whole ali are in that directory.
   --     There are two reasons for skipping the sources in this case. Firstly,
   --     Ada libraries typically come without full sources but binding and
   --     linking against those libraries is still possible. Secondly, it would
   --     be very wasteful for gnatmake to systematically check the consistency
   --     of every external Ada library used in a program. The binder is
   --     already in charge of catching any potential inconsistencies.
   --
   --  3. Look into the W section of unit.ali and insert into the Q all
   --     unmarked source files. Mark all files newly inserted in the Q.
   --     Specifically, assuming that the W section looks like
   --
   --     W types%s               types.adb               types.ali
   --     W unchecked_deallocation%s
   --     W xref_tab%s            xref_tab.adb            xref_tab.ali
   --
   --     Then xref_tab.adb and types.adb are inserted in the Q if they are not
   --     already marked.
   --     Note that there is no file listed under W unchecked_deallocation%s
   --     so no generic body should ever be explicitely compiled (unless the
   --     Main_Source at the start was a generic body).
   --
   --  4. Repeat steps 2 and 3 above until the Q is empty
   --
   --  Note that the above algorithm works because the units withed in
   --  subunits are transitively included in the W section (with section) of
   --  the main unit. Likewise the withed units in a generic body needed
   --  during a compilation are also transitively included in the W section
   --  of the originally compiled file.

   procedure Gnatmake;
   --  The driver of gnatmake. This routine puts it all together.
   --  This utility can be used to automatically (re)compile (using
   --  Compile_Sources), bind (using Bind) and link (using Link) a set of
   --  ada sources. For more information on gnatmake and its precise usage
   --  please refer to the gnat documentation.
   --
   --  Flags in Package Opt Affecting Gnatmake
   --  ---------------------------------------
   --
   --    Check_Readonly_Files:     True  when -a present in command line
   --    Check_Object_Consistency: Set to True by Gnatmake
   --    Compile_Only:             True  when -c present in command line
   --    Force_Compilations:       True  when -f present in command line
   --    Maximum_Processes:        Number of processes given by -jnum
   --    Keep_Going:               True  when -k present in command line
   --    List_Dependencies:        True  when -l present in command line
   --    Do_Not_Execute            True  when -n present in command line
   --    Quiet_Output:             True  when -q present in command line
   --    Minimal_Recompilation:    True  when -m present in command line
   --    Verbose_Mode:             True  when -v present in command line

end Make;
