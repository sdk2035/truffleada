------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S I N P U T                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.62 $
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

--  This package contains the input routines used for reading the
--  input source file. The actual I/O routines are in OS_Interface,
--  with this module containing only the system independent processing.

--  General Note: throughout the compiler, we use the term line or source
--  line to refer to a physical line in the source, terminated by the end of
--  physical line sequence. See Skip_Line_Terminators procedure for a full
--  description of the difference between logical and physical lines.

with Alloc;
with Casing; use Casing;
with Table;
with Types;  use Types;

package Sinput is

   -----------------------
   -- Source File Table --
   -----------------------

   --  The source file table has an entry for each source file read in for
   --  this run of the compiler. This table is (default) initialized when
   --  the compiler is loaded, and simply accumulates entries as compilation
   --  proceeds and the Sinput.L.Load_Source_File procedure is called to load
   --  required source files.

   --  Virtual entries are also created for generic templates when they are
   --  instantiated, as described in a separate section later on.

   --  In the case where there are multiple main units (e.g. in the case of
   --  the cross-reference tool), this table is not reset between these units,
   --  so that a given source file is only read once if it is used by two
   --  separate main units.

   --  The entries in the table are accessed using a Source_File_Index that
   --  ranges from 1 to Last_Source_File. Each entry has the following fields

   --  File_Name : File_Name_Type
   --    Name of the source file (simple name with no directory information).
   --    Set by Sinput.L.Load_Source_File and cannot be subequently changed.

   --  Full_File_Name : File_Name_Type
   --    Full file name (full name with directory info), used for generation
   --    of error messages, etc. Set by Sinput.L.Load_Source_File and cannot
   --    be subsequently changed.

   --  Reference_Name : File_Name_Type
   --    Name to be used for source file references in error messages where
   --    only the simple name of the file is required. Identical to File_Name
   --    unless pragma Source_Reference is used to change it. Only processing
   --    for the Source_Reference pragma circuit may set this field.

   --  Full_Ref_Name : File_Name_Type
   --    Name to be used for source file references in error messages where
   --    the full name of the file is required. Identical to Full_File_Name
   --    unless pragma Source_Reference is used to change it. Only processing
   --    for the Source_Reference pragma may set this field.

   --  Debug_Source_Name : File_Name_Type
   --    Name to be used for source file references in debugging information
   --    where only the simple name of the file is required. Identical to
   --    Full_Ref_Name unless the -gnatD (debug source file) switch is used.
   --    Only processing in Sprint that generates this file is permitted to
   --    set this field.

   --  Line_Offset : Int;
   --    Line number value to be added to physical line numbers in file to
   --    get logical line number for error messages. Normally zero unless
   --    reset by pragma Source_Reference. This value is seldom referenced by
   --    clients who should use Logical_To_Physical and Physical_To_Logical
   --    instead, but it is set by pragma Source_Reference processing. Note
   --    that this value is signed (it is set to -1 for the first file in the
   --    original file, to allow for the added Source_Reference pragma line).

   --  Has_Line_Offset : Boolean
   --    Set True if Source_Reference pragma is present

   --  Source_Text : Source_Buffer_Ptr
   --    Text of source file. Note that every source file has a distinct set
   --    of non-overlapping logical bounds, so it is possible to determine
   --    which file is referenced from a given subscript (Source_Ptr) value.
   --    Set by Sinput.L.Load_Source_File and cannot be subsequently changed.

   --  Source_First : Source_Ptr;
   --    Subscript of first character in Source_Text. Note that this cannot
   --    be obtained as Source_Text'First, because we use virtual origin
   --    addressing. Set by Sinput.L procedures when the entry is first
   --    created and never subsequently changed.

   --  Source_Last : Source_Ptr;
   --    Subscript of last character in Source_Text. Note that this cannot
   --    be obtained as Source_Text'Last, because we use virtual origin
   --    addressing, so this value is always Source_Ptr'Last. Set by
   --    Sinput.L procedures when the entry is first created and never
   --    subsequently changed.

   --  Time_Stamp : Time_Stamp_Type;
   --    Time stamp of the source file. Set by Sinput.L.Load_Source_File,
   --    and cannot be subsequently changed.

   --  Source_Checksum : Word;
   --    Computed checksum for contents of source file. See separate section
   --    later on in this spec for a description of the checksum algorithm.

   --  Num_Source_Lines : Nat
   --    Number of source lines in the file. While a file is being read,
   --    it is the number of lines so far scanned. Read only for clients.

   --  Keyword_Casing : Casing_Type;
   --    Casing style used in file for keyword casing. This is initialized
   --    to Unknown, and then set from the first occurrence of a keyword.
   --    This value is used only for formatting of error messages.

   --  Identifier_Casing : Casing_Type;
   --    Casing style used in file for identifier casing. This is initialized
   --    to Unknown, and then set from an identifier in the program as soon as
   --    one is found whose casing is sufficiently clear to make a decision.
   --    This value is used for formatting of error messages, and also is used
   --    in the detection of keywords misused as identifiers.

   --  Instantiation : Source_Ptr;
   --    Source file location of the instantiation if this source file entry
   --    represents a generic instantiation. Set to No_Location for the case
   --    of a normal non-instantiation entry. See section below for details.
   --    This field is read-only for clients.

   --  Template : Source_File_Index;
   --    Source file index of the source file containing the template if this
   --    is a generic instantiation. Set to No_Source_File for the normal case
   --    of a non-instantiation entry. See Sinput-L for details. This field is
   --    read-only for clients.

   --  The source file table is accessed by clients using the following
   --  subprogram interface:

   function Debug_Source_Name (S : Source_File_Index) return File_Name_Type;
   function File_Name         (S : Source_File_Index) return File_Name_Type;
   function Full_File_Name    (S : Source_File_Index) return File_Name_Type;
   function Full_Ref_Name     (S : Source_File_Index) return File_Name_Type;
   function Has_Line_Offset   (S : Source_File_Index) return Boolean;
   function Identifier_Casing (S : Source_File_Index) return Casing_Type;
   function Instantiation     (S : Source_File_Index) return Source_Ptr;
   function Keyword_Casing    (S : Source_File_Index) return Casing_Type;
   function Line_Offset       (S : Source_File_Index) return Int;
   function Num_Source_Lines  (S : Source_File_Index) return Nat;
   function Reference_Name    (S : Source_File_Index) return File_Name_Type;
   function Source_Checksum   (S : Source_File_Index) return Word;
   function Source_First      (S : Source_File_Index) return Source_Ptr;
   function Source_Last       (S : Source_File_Index) return Source_Ptr;
   function Source_Text       (S : Source_File_Index) return Source_Buffer_Ptr;
   function Template          (S : Source_File_Index) return Source_File_Index;
   function Time_Stamp        (S : Source_File_Index) return Time_Stamp_Type;

   procedure Set_Keyword_Casing    (S : Source_File_Index; C : Casing_Type);
   procedure Set_Identifier_Casing (S : Source_File_Index; C : Casing_Type);

   --  The following routines may be used only by the processing for
   --  the Source_Reference pragma, and by no other client.

   procedure Set_Debug_Source_Name (S : Source_File_Index; N : File_Name_Type);
   procedure Set_Reference_Name    (S : Source_File_Index; N : File_Name_Type);
   procedure Set_Full_Ref_Name     (S : Source_File_Index; N : File_Name_Type);
   procedure Set_Has_Line_Offset   (S : Source_File_Index; V : Boolean);
   procedure Set_Line_Offset       (S : Source_File_Index; V : Int);

   function Last_Source_File return Source_File_Index;
   --  Index of last source file table entry

   function Num_Source_Files return Nat;
   --  Number of source file table entries

   procedure Initialize;
   --  Initialize internal tables

   procedure Lock;
   --  Lock internal tables

   Main_Source_File : Source_File_Index;
   --  This is set to the source file index of the main unit

   -----------------------
   -- Checksum Handling --
   -----------------------

   --  As a source file is scanned, a checksum is computed by taking all the
   --  non-blank characters in the file, excluding comment characters, the
   --  minus-minus sequence starting a comment, and all control characters
   --  except ESC.

   --  These characters are used to compute a 31-bit checksum which is stored
   --  in the variable Scans.Checksum, as follows:

   --    If a character, C, is not part of a wide character sequence, then
   --    either the character itself, or its lower case equivalent if it
   --    is a letter outside a string literal is used in the computation:

   --      Checksum := Checksum + Checksum + Character'Pos (C);
   --      if Checksum > 16#8000_0000# then
   --         Checksum := (Checksum + 1) and 16#7FFF_FFFF#;
   --      end if;

   --    For a wide character sequence, the checksum is computed using the
   --    corresponding character code value C, as follows:

   --      Checksum := Checksum + Checksum + Char_Code'Pos (C);
   --      if Checksum > 16#8000_0000# then
   --         Checksum := (Checksum + 1) and 16#7FFF_FFFF#;
   --      end if;

   --  This algorithm ensures that the checksum includes all semantically
   --  significant aspects of the program represented by the source file,
   --  but is insensitive to layout, presence or contents of comments, wide
   --  character representation method, or casing conventions outside strings.

   --  Scans.Checksum is initialized to zero at the start of scanning a file,
   --  and copied into the Source_Checksum field of the file table entry when
   --  the end of file is encountered.

   -------------------------------------
   -- Handling Generic Instantiations --
   -------------------------------------

   --  As described in Sem_Ch12, a generic instantiation involves making a
   --  copy of the tree of the generic template. The source locations in
   --  this tree directly reference the source of the template. However it
   --  is also possible to find the location of the instantiation.

   --  This is achieved as follows. When an instantiation occurs, a new entry
   --  is made in the source file table. This entry points to the same source
   --  text, i.e. the file that contains the instantiation, but has a distinct
   --  set of Source_Ptr index values. The separate range of Sloc values avoids
   --  confusion, and means that the Sloc values can still be used to uniquely
   --  identify the source file table entry. It is possible for both entries
   --  to point to the same text, because of the virtual origin pointers used
   --  in the source table.

   --  The Instantiation field of this source file index entry, usually set
   --  to No_Source_File, instead contains the Sloc of the instantiation. In
   --  the case of nested instantiations, this Sloc may itself refer to an
   --  instantiation, so the complete chain can be traced.

   --  Two routines are used to build these special entries in the source
   --  file table. Create_Instantiation_Source is first called to build
   --  the virtual source table entry for the instantiation, and then the
   --  Sloc values in the copy are adjusted using Adjust_Instantiation_Sloc.
   --  See child unit Sinput.L for details on these two routines.

   -----------------
   -- Global Data --
   -----------------

   Current_Source_File : Source_File_Index;
   --  Source_File table index of source file currently being scanned

   Current_Source_Unit : Unit_Number_Type;
   --  Unit number of source file currently being scanned. The special value
   --  of No_Unit indicates that the configuration pragma file is currently
   --  being scanned (this has no entry in the unit table).

   Source_gnat_adc : Source_File_Index := No_Source_File;
   --  This is set if a gnat.adc file is present to reference this file

   Source : Source_Buffer_Ptr;
   --  Current source (copy of Source_File.Table (Current_Source_Unit).Source)

   Internal_Source : aliased Source_Buffer (1 .. 81);
   --  This buffer is used internally in the compiler when the lexical analyzer
   --  is used to scan a string from within the compiler. The procedure is to
   --  establish Internal_Source_Ptr as the value of Source, set the string to
   --  be scanned, appropriately terminated, in this buffer, and set Scan_Ptr
   --  to point to the start of the buffer. It is a fatal error if the scanner
   --  signals an error while scanning a token in this internal buffer.

   Internal_Source_Ptr : constant Source_Buffer_Ptr :=
                           Internal_Source'Unrestricted_Access;
   --  Pointer to internal source buffer

   -----------------
   -- Subprograms --
   -----------------

   procedure Backup_Line (P : in out Source_Ptr);
   --  Back up the argument pointer to the start of the previous line. On
   --  entry, P points to the start of a physical line in the source buffer.
   --  On return, P is updated to point to the start of the previous line.
   --  The caller has checked that a Line_Terminator character precedes P so
   --  that there definitely is a previous line in the source buffer.

   procedure Build_Location_String (Loc : Source_Ptr);
   --  This function builds a string literal of the form "name:line",
   --  where name is the file name corresponding to Loc, and line is
   --  the line number. In the event that instantiations are involved,
   --  additional suffixes of the same form are appended after the
   --  separating string " instantiated at ". The returned string is
   --  stored in Name_Buffer, terminated by ASCII.Nul, with Name_Length
   --  indicating the length not including the terminating Nul.

   function Get_Column_Number (P : Source_Ptr) return Column_Number;
   --  The ones-origin column number of the specified Source_Ptr value is
   --  determined and returned. Tab characters if present are assumed to
   --  represent the standard 1,9,17.. spacing pattern.

   function Get_Line_Number (P : Source_Ptr) return Logical_Line_Number;
   --  The line number of the specified source position is obtained by
   --  doing a binary search on the source positions in the lines table
   --  for the unit containing the given source position. The returned
   --  value has already been adjusted by adding the Line_Offset value.

   function Get_Source_File_Index (S : Source_Ptr) return Source_File_Index;
   --  Return file table index of file identified by given source pointer
   --  value. This call must always succeed, since any valid source pointer
   --  value belongs to some previously loaded source file.

   function Instantiation_Depth (S : Source_Ptr) return Nat;
   --  Determine instantiation depth for given Sloc value. A value of
   --  zero means that the given Sloc is not in an instantiation.

   function Line_Start (P : Source_Ptr) return Source_Ptr;
   --  Finds the source position of the start of the line containing the
   --  given source location.

   function Line_Start
     (L    : Logical_Line_Number;
      S    : Source_File_Index)
      return Source_Ptr;
   --  Finds the source position of the start of the given line in the
   --  given source file, using a logical line number to identify the line.

   function Logical_To_Physical
     (Line : Logical_Line_Number;
      S    : Source_File_Index)
      return Nat;
   --  Given a logical line number in source file whose source index is S,
   --  return the corresponding physical line number. Note that if a source
   --  reference pragma is present, its logical line number is zero.

   function Original_Location (S : Source_Ptr) return Source_Ptr;
   --  Given a source pointer S, returns the corresponding source pointer
   --  value ignoring instantiation copies. For locations that do not
   --  correspond to instantiation copies of templates, the argument is
   --  returned unchanged. For locations that do correspond to copies of
   --  templates from instantiations, the location within the original
   --  template is returned. This is useful in canonicalizing locations.

   function Instantiation_Location (S : Source_Ptr) return Source_Ptr;
   pragma Inline (Instantiation_Location);
   --  Given a source pointer S, returns the corresponding source pointer
   --  value of the instantiation if this location is within an instance.
   --  If S is not within an instance, then this returns No_Location.

   function Top_Level_Location (S : Source_Ptr) return Source_Ptr;
   --  Given a source pointer S, returns the argument unchanged if it is
   --  not in an instantiation. If S is in an instantiation, then it returns
   --  the location of the top level instantiation, i.e. the outer level
   --  instantiation in the nested case.

   function Physical_To_Logical
     (Line : Nat;
      S    : Source_File_Index)
      return Logical_Line_Number;
   --  Given a physical line number in source file whose source index is S,
   --  return the corresponding logical line number. Note that if a source
   --  reference pragma is present, its logical line number is zero.

   procedure Skip_Line_Terminators
     (P        : in out Source_Ptr;
      Physical : out Boolean);
   --  On entry, Source (P) points to the line terminator character that
   --  terminates a line. The result set in P is the location of the first
   --  character of the following line (after skipping the sequence of line
   --  terminator characters terminating the current line). In addition, if
   --  the terminator sequence ends a physical line (the definition of what
   --  constitutes a physical line is embodied in the implementation of this
   --  function), and it is the first time this sequence is encountered, then
   --  an entry is made in the lines table to record the location for further
   --  use by functions such as Get_Line_Number. Physical is set to True if
   --  the line terminator was the end of a physical line.

   function Source_Offset (S : Source_Ptr) return Nat;
   --  Returns the zero-origin offset of the given source location from the
   --  start of its corresponding unit. This is used for creating canonical
   --  names in some situations.

   procedure Write_Location (P : Source_Ptr);
   --  Writes out a string of the form fff:nn:cc, where fff, nn, cc are the
   --  file name, line number and column corresponding to the given source
   --  location. No_Location and Standard_Location appear as the strings
   --  <no location> and <standard location>. If the location is within an
   --  instantiation, then the instance location is appended, enclosed in
   --  square brackets (which can nest if necessary). Note that this routine
   --  is used only for internal compiler debugging output purposes (which is
   --  why the somewhat cryptic use of brackets is acceptable).

   procedure Write_Time_Stamp (S : Source_File_Index);
   --  Writes time stamp of specified file in YY-MM-DD HH:MM.SS format

   procedure Tree_Write;
   --  Writes out internal tables to current tree file using Tree_Write

   procedure Tree_Read;
   --  Initializes internal tables from current tree file using Tree_Read

private
   pragma Inline (File_Name);
   pragma Inline (Full_File_Name);
   pragma Inline (Identifier_Casing);
   pragma Inline (Instantiation);
   pragma Inline (Keyword_Casing);
   pragma Inline (Last_Source_File);
   pragma Inline (Line_Offset);
   pragma Inline (Has_Line_Offset);
   pragma Inline (Num_Source_Files);
   pragma Inline (Num_Source_Lines);
   pragma Inline (Reference_Name);
   pragma Inline (Set_Keyword_Casing);
   pragma Inline (Set_Identifier_Casing);
   pragma Inline (Set_Line_Offset);
   pragma Inline (Set_Has_Line_Offset);
   pragma Inline (Set_Reference_Name);
   pragma Inline (Source_First);
   pragma Inline (Source_Last);
   pragma Inline (Source_Text);
   pragma Inline (Template);
   pragma Inline (Time_Stamp);

   -------------------------
   -- Source_Lines Tables --
   -------------------------

   type Lines_Table_Type is array (Pos) of Source_Ptr;
   --  Type used for lines table. The entries are indexed by physical line
   --  numbers. The values are the starting Source_Ptr values for the start
   --  of the corresponding physical line. Note that we make this a bogus
   --  big array, sized as required, so that we avoid the use of fat pointers.
   --  This type is also used for logical lines (source reference pragma).

   type Lines_Table_Ptr is access all Lines_Table_Type;
   --  Type used for pointers to line tables

   -----------------------
   -- Source_File Table --
   -----------------------

   type Source_File_Record is record

      File_Name : File_Name_Type;
      --  Source file name (simple name with no directory info)

      Reference_Name : File_Name_Type;
      --  File name used for error messages (same as File_Name unless
      --  reset by use of pragma Source_Reference).

      Debug_Source_Name : File_Name_Type;
      --  File name used for debug information (same as Reference_Name
      --  unless Debug_Generated_Code is set).

      Full_File_Name : File_Name_Type;
      --  Full file name (full name with directory info)

      Full_Ref_Name : File_Name_Type;
      --  Full file name used for error messages (same as Full_File_Name
      --  unless reset by use of pragma Source_Reference).

      Line_Offset : Int;
      --  Offset value for line number references, to be added to physical
      --  line numbers to obtain logical line numbers. Normally set to zero
      --  but can be reset by use of pragma Source_Reference.

      Has_Line_Offset : Boolean;
      --  Set to True, if Line_Offset set by Source_Reference pragma

      Source_Text : Source_Buffer_Ptr;
      --  Text of source file. Note that every source file has a distinct set
      --  of non-overlapping bounds, so it is possible to determine which file
      --  is referenced from a given subscript (Source_Ptr) value.

      Source_First : Source_Ptr;
      --  Subscript of first character in Source_Text. Note that this cannot
      --  be obtained as Source_Text'First, because we use virtual origin
      --  addressing, so this value is always zero.

      Source_Last : Source_Ptr;
      --  Subscript of last character in Source_Text. Note that this cannot
      --  be obtained as Source_Text'Last, because we use virtual origin
      --  addressing, so this value is always Source_Ptr'Last.

      Time_Stamp : Time_Stamp_Type;
      --  Time stamp of the source file

      Source_Checksum : Word;
      --  Computed checksum for source file

      Num_Source_Lines : Nat;
      --  Number of entries in Lines_Table, i.e. the subscript of the last
      --  entry stored in this table. On completion of compilation of a unit
      --  (status = loaded), this is the number of source lines in the file.

      Keyword_Casing : Casing_Type;
      --  Casing style used in file for keyword casing. Initialized to
      --  Unknown, and then set from the first occurrence of a keyword.
      --  This value is used only for formatting of error messages.

      Identifier_Casing : Casing_Type;
      --  Casing style used in file for identifier casing. Initialized to
      --  Unknown, and then set from an identifier in the program as soon as
      --  one is found whose casing is sufficiently clear to make a decision.
      --  This value is used for formatting of error messages, and also is
      --  used in the detection of keywords misused as identifiers.

      Instantiation : Source_Ptr;
      --  Source file location of the instantiation if this source file entry
      --  represents a generic instantiation. Set to No_Location for the case
      --  of a normal non-instantiation entry. See Sinput-L for details.

      Template : Source_File_Index;
      --  Source file index of the source file containing the template if
      --  this is a generic instantiation. Set to No_Source_File for the
      --  normal case of a non-instantiation entry. See Sinput-L for details.

      --  The following fields are for internal use only (i.e. only in the
      --  body of Sinput or its children, with no direct access by clients).

      Sloc_Adjust : Source_Ptr;
      --  A value to be added to Sloc values for this file to reference the
      --  corresponding lines table. This is zero for the non-instantiation
      --  case, and set so that the adition references the ultimate template
      --  for the instantiation case. See Sinput-L for further details.

      Lines_Table : Lines_Table_Ptr;
      --  Pointer to lines table for this source. Updated as additional
      --  lines are accessed using the Skip_Line_Terminators procedure.
      --  Note: the lines table for an instantiation entry refers to the
      --  original line numbers of the template see Sinput-L for details.

      Lines_Table_Max : Pos;
      --  Maximum subscript currently allocated in Lines_Table

   end record;

   package Source_File is new Table.Table (
     Table_Component_Type => Source_File_Record,
     Table_Index_Type     => Source_File_Index,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.Source_File_Initial,
     Table_Increment      => Alloc.Source_File_Increment,
     Table_Name           => "Source_File");

   -----------------
   -- Subprograms --
   -----------------

   procedure Alloc_Lines_Table (S : in out Source_File_Record; New_Max : Pos);
   --  Allocate or reallocate the lines table for the given source
   --  file so that it can accomodate at least New_Max lines.

   procedure Increment_Line_Table_Size
     (S : in out Source_File_Record;
      P : Source_Ptr);
   --  Increment line table size by one (reallocating the lines table if
   --  needed, and set the new entry to contain the value P. Also bumps
   --  the Source_Line_Count field.

end Sinput;
