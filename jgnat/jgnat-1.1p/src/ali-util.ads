------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             A L I . U T I L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
--                                                                          --
--          Copyright (C) 1992-1999 Free Software Foundation, Inc.          --
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

--  This child unit provides utility data structures and procedures used
--  for manipulation of ALI data by the gnatbind and gnatmake.

package ALI.Util is

   -----------------------
   -- Source File Table --
   -----------------------

   --  A source file table entry is built for every source file that is
   --  in the source dependency table of any of the ALI files that make
   --  up the current program.

   No_Source_Id : constant Source_Id := Source_Id'First;
   --  Special value indicating no Source table entry

   First_Source_Entry : constant Source_Id := No_Source_Id + 1;
   --  Id of first actual entry in table

   type Source_Record is record

      Sfile : File_Name_Type;
      --  Name of source file

      Stamp : Time_Stamp_Type;
      --  Time stamp value. If Check_Source_Files is set and the source
      --  file is located, then Stamp is set from the source file. Otherwise
      --  Stamp is set from the latest stamp value found in any of the
      --  ALI files for the current program.

      Source_Found : Boolean;
      --  This flag is set to True if the corresponding source file was
      --  located and the Stamp value was set from the actual source file.
      --  It is always false if Check_Source_Files is not set.

      All_Timestamps_Match : Boolean;
      --  This flag is set only if all files referencing this source file
      --  have a matching time stamp, and also, if Source_Found is True,
      --  then the stamp of the source file also matches. If this flag is
      --  True, then checksums for this file are never referenced. We only
      --  use checksums if there are time stamp mismatches.

      All_Checksums_Match : Boolean;
      --  This flag is set only if all files referencing this source file
      --  have checksums, and if all these checksums match. If this flag
      --  is set to True, then the binder will ignore a timestamp mismatch.
      --  An absent checksum causes this flag to be set False, and a mismatch
      --  of checksums also causes it to be set False. The checksum of the
      --  actual source file (if Source_Found is True) is included only if
      --  All_Timestamps_Match is False (since checksums are only interesting
      --  if we have time stamp mismatches, and we want to avoid computing the
      --  checksum of the source file if it is not needed.)

      Checksum : Word;
      --  If no dependency line has a checksum for this source file (i.e. the
      --  corresponding entries in the source dependency records all have the
      --  Checksum_Present flag set False), then this field is undefined. If
      --  at least one dependency entry has a checksum present, then this
      --  field contains one of the possible checksum values that has been
      --  seen. This is used to set All_Checksums_Match properly.

   end record;

   package Source is new Table.Table (
     Table_Component_Type => Source_Record,
     Table_Index_Type     => Source_Id,
     Table_Low_Bound      => First_Source_Entry,
     Table_Initial        => 1000,
     Table_Increment      => 200,
     Table_Name           => "Source");

   procedure Initialize_ALI_Source;
   --  Initialize Source table

   --------------------------------------------------
   -- Subprograms for Manipulating ALI Information --
   --------------------------------------------------

   procedure Read_ALI (Id : ALI_Id);
   --  Process an ALI file which has been read and scanned by looping
   --  through all withed units in the ALI file; checking if they have
   --  been processed; and for each that hasn't, reading, scanning, and
   --  recursively processing.

   procedure Set_Source_Table (A : ALI_Id);
   --  Build source table entry corresponding to the ALI file whose id is A.

   procedure Set_Source_Table;
   --  Build the entire source table.

   function Time_Stamp_Mismatch (A : ALI_Id) return File_Name_Type;
   --  Looks in the Source_Table and checks time stamp mismatches between
   --  the sources there and the sources in the Sdep section of ali file whose
   --  id is A. If no time stamp mismatches are found No_File is returned.
   --  Otherwise return the first file for which there is a mismatch.
   --  Note that in check source files mode (Check_Source_Files = True), the
   --  time stamp in the Source_Table should be the actual time stamp of the
   --  source files. In minimal recompilation mode (Minimal_Recompilation set
   --  to True, no mismatch is found if the file's timestamp has not changed.

   --------------------------------------------
   -- Subprograms for manipulating checksums --
   --------------------------------------------

   function Get_File_Checksum (Fname : Name_Id) return Word;
   --  Compute checksum for the given file. As far as possible, this circuit
   --  computes exactly the same value computed by the compiler, but it does
   --  not matter if it gets it wrong in marginal cases, since the only result
   --  is to miss some smart recompilation cases, correct functioning is not
   --  affecte by a mis-computation. Returns an impossible checksum value,
   --  with the upper bit set, if the file is missing or has an error.

end ALI.Util;
