------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  L I B                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.94 $
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

--  This package contains routines for accessing and outputting the library
--  information. It contains the routine to load subsidiary units.

with Alloc;
with Table;
with Types;  use Types;

package Lib is

   --------------------------------------------
   -- General Approach to Library Management --
   --------------------------------------------

   --  As described in GNote #1, when a unit is compiled, all its subsidiary
   --  units are recompiled, including the following:

   --    (a) Corresponding spec for a body
   --    (b) Parent spec of a child library spec
   --    (d) With'ed specs
   --    (d) Parent body of a subunit
   --    (e) Subunits corresponding to any specified stubs
   --    (f) Bodies of inlined subprograms that are called
   --    (g) Bodies of generic subprograms or packages that are instantiated
   --    (h) Bodies of packages containing either of the above two items
   --    (i) Specs and bodies of runtime units
   --    (j) Parent specs for with'ed child library units

   --  If a unit is being compiled only for syntax checking, then no subsidiary
   --  units are loaded, the the syntax check applies only to the main unit,
   --  i.e. the one contained in the source submitted to the library.

   --  If a unit is being compiled for syntax and semantic checking, then only
   --  cases (a)-(d) loads are performed, since the full semantic checking can
   --  be carried out without needing (e)-(i) loads. In this case no object
   --  file, or library information file, is generated, so the missing units
   --  do not affect the results.

   --  Specifications of library subprograms, subunits, and generic specs
   --  and bodies, can only be compiled in syntax/semantic checking mode,
   --  since no code is ever generated directly for these units. In the case
   --  of subunits, only the compilation of the ultimate parent unit generates
   --  actual code. If a subunit is submitted to the compiler in syntax/
   --  semantic checking mode, the parent (or parents in the nested case) are
   --  semantically checked only up to the point of the corresponding stub.

   --  If code is being generated, then all the above units are required,
   --  although the need for bodies of inlined procedures can be suppressed
   --  by the use of a switch that sets the mode to ignore pragma Inline
   --  statements.

   --  The two main sections of the front end, Par and Sem, are recursive.
   --  Compilation proceeds unit by unit making recursive calls as necessary.
   --  The process is controlled from the GNAT main program, which makes calls
   --  to Par and Sem sequence for the main unit.

   --  Par parses the given unit, and then, after the parse is complete, uses
   --  the Par.Load subprogram to load all its subsidiary units in categories
   --  (a)-(d) above, installing pointers to the loaded units in the parse
   --  tree, as described in a later section of this spec. If any of these
   --  required units is missing, a fatal error is signalled, so that no
   --  attempt is made to run Sem in such cases, since it is assumed that
   --  too many cascaded errors would result, and the confusion would not
   --  be helpful.

   --  Following the call to Par on the main unit, the entire tree of required
   --  units is thus loaded, and Sem is called on the main unit. The parameter
   --  passed to Sem is the unit to be analyzed. The visibility table, which
   --  is a single global structure, starts out containing only the entries
   --  for the visible entities in Standard. Every call to Sem establishes a
   --  new scope stack table, pushing an entry for Standard on entry to provide
   --  the proper initial scope environment.

   --  Sem first proceeds to perform semantic analysis on the currently loaded
   --  units as follows:

   --    In the case of a body (case (a) above), Sem analyzes the corresponding
   --    spec, using a recursive call to Sem. As is always expected to be the
   --    case with calls to Sem, any entities installed in the visibility table
   --    are removed on exit from Sem, so that these entities have to be
   --    reinstalled on return to continue the analysis of the body which of
   --    course needs visibility of these entities.
   --
   --    In the case of the parent of a child spec (case (b) above), a similar
   --    call is made to Sem to analyze the parent. Again, on return, the
   --    entities from the analyzed parent spec have to be installed in the
   --    visibility table of the caller (the child unit), which must have
   --    visibility to the entities in its parent spec.

   --    For with'ed specs (case (c) above), a recursive call to Sem is made
   --    to analyze each spec in turn. After all the spec's have been analyzed,
   --    but not till that point, the entities from all the with'ed units are
   --    reinstalled in the visibility table so that the caller can proceed
   --    with the analysis of the unit doing the with's with the necessary
   --    entities made either potentially use visible or visible by selection
   --    as needed.

   --    Case (d) arises when Sem is passed a subunit to analyze. This means
   --    that the main unit is a subunit, and the unit passed to Sem is either
   --    the main unit, or one of its ancestors that is still a subunit. Since
   --    analysis must start at the top of the tree, Sem essentially cancels
   --    the current call by immediately making a call to analyze the parent
   --    (when this call is finished it immediately returns, so logically this
   --    call is like a goto). The subunit will then be analyzed at the proper
   --    time as described for the stub case. Note that we also turn off the
   --    indication that code should be generated in this case, since the only
   --    time we generate code for subunits is when compiling the main parent.

   --    Case (e), subunits corresponding to stubs, are handled as the stubs
   --    are encountered. There are three sub-cases:

   --      If the subunit has already been loaded, then this means that the
   --      main unit was a subunit, and we are back on our way down to it
   --      after following the initial processing described for case (d).
   --      In this case we analyze this particular subunit, as described
   --      for the case where we are generating code, but when we get back
   --      we are all done, since the rest of the parent is irrelevant. To
   --      get out of the parent, we raise the exception Subunit_Found, which
   --      is handled at the outer level of Sem.

   --      The cases where the subunit has not already been loaded correspond
   --      to cases where the main unit was a parent. In this case the action
   --      depends on whether or not we are generating code. If we are not
   --      generating code, then this is the case where we can simply ignore
   --      the subunit, since in checking mode we don't even want to insist
   --      that the subunit exist, much less waste time checking it.

   --      If we are generating code, then we need to load and analyze
   --      all subunits. This is achieved with a call to Lib.Load to load
   --      and parse the unit, followed by processing that installs the
   --      context clause of the subunit, analyzes the subunit, and then
   --      removes the context clause (from the visibility chains of the
   --      parent). Note that we do *not* do a recursive call to Sem in
   --      this case, precisely because we need to do the analysis of the
   --      subunit with the current visibility table and scope stack.

   --    Case (f) applies only to subprograms for which a pragma Inline is
   --    given, providing that the compiler is operating in the mode where
   --    pragma Inline's are activated. When the expander encounters a call
   --    to such a subprogram, it loads the body of the subprogram if it has
   --    not already been loaded, and calls Sem to process it.

   --    Case (g) is similar to case (f), except that the body of a generic
   --    is unconditionally required, regardless of compiler mode settings.
   --    As in the subprogram case, when the expander encounters a generic
   --    instantiation, it loads the generic body of the subprogram if it
   --    has not already been loaded, and calls Sem to process it.

   --    Case (h) arises when a package contains either an inlined subprogram
   --    which is called, or a generic which is instantiated. In this case the
   --    body of the package must be loaded and analyzed with a call to Sem.

   --    Case (i) is handled by adding implicit with clauses to the context
   --    clauses of all units that potentially reference the relevant runtime
   --    entities. Note that since we have the full set of units available,
   --    the parser can always determine the set of runtime units that is
   --    needed. These with clauses do not have associated use clauses, so
   --    all references to the entities must be by selection. Once the with
   --    clauses have been added, subsequent processing is as for normal
   --    with clauses.

   --    Case (j) is also handled by adding appropriate implicit with clauses
   --    to any unit that withs a child unit. Again there is no use clause,
   --    and subsequent processing proceeds as for an explicit with clause.

   --  Sem thus completes the loading of all required units, except those
   --  required for inline subprogram bodies or inlined generics. If any
   --  of these load attempts fails, then the expander will not be called,
   --  even if code was to be generated. If the load attempts all succeed
   --  then the expander is called, though the attempt to generate code may
   --  still fail if an error occurs during a load attempt for an inlined
   --  body or a generic body.

   -------------------------------------------
   -- Special Handling of Subprogram Bodies --
   -------------------------------------------

   --  A subprogram body (in an adb file) may stand for both a spec and a
   --  body. A simple model (and one that was adopted through version 2.07),
   --  is simply to assume that such an adb file acts as its own spec if no
   --  ads file is present.

   --  However, this is not correct. RM 10.1.4(4) requires that such a body
   --  act as a spec unless a subprogram declaration of the same name is
   --  already present. The correct interpretation of this in GNAT library
   --  terms is to ignore an existing ads file of the same name unless this
   --  ads file contains a subprogram declaration with the same name.

   --  If there is an ads file with a unit other than a subprogram declaration
   --  with the same name, then a fatal message is output, noting that this
   --  irrelevant file must be deleted before the body can be compiled. See
   --  ACVC test CA1020D to see how this processing is required.

   -----------------
   -- Global Data --
   -----------------

   Current_Sem_Unit : Unit_Number_Type := Main_Unit;
   --  Unit number of unit currently being analyzed/expanded. This is set when
   --  ever a new unit is entered, saving and restoring the old value, so that
   --  it always reflects the unit currently being analyzed. The initial value
   --  of Main_Unit ensures that a proper value is set initially, and in
   --  particular for analysis of configuration pragmas in gnat.adc.

   Main_Unit_Entity : Entity_Id;
   --  Entity of main unit, same as Cunit_Entity (Main_Unit) except where
   --  Main_Unit is a body with a separate spec, in which case it is the
   --  entity for the spec.

   Unit_Exception_Table_Present : Boolean;
   --  Set true if a unit exception table is present for the unit (i.e.
   --  zero cost exception handling is active and there is at least one
   --  subprogram in the extended unit).

   ---------------------------------
   -- Compilation Arguments Table --
   ---------------------------------

   --  This table records the compilation switches used to compile the
   --  main unit. The table includes only switches starting with a minus,
   --  and excludes -quiet, -dumpbase, and -o switches, since the latter
   --  are typically artifacts of the gcc/gnat1 interface.

   --  This table is set as part of the processing in Lib.Writ. It can also
   --  be reset in -gnatc mode from the data in an existing ali file, and is
   --  read and written by the Tree_Read and Tree_Write routines for ASIS.

   package Compilation_Arguments is new Table.Table (
     Table_Component_Type => String_Ptr,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 30,
     Table_Increment      => 100,
     Table_Name           => "Compilation_Arguments");

   -----------------
   -- Units Table --
   -----------------

   --  The units table has an entry for each unit (source file) read in by the
   --  current compilation. The table is indexed by the unit number value,
   --  The first entry in the table, subscript Main_Unit, is for the main file.
   --  Each entry in this units table contains the following data.

   --    Unit_File_Name
   --      The name of the source file containing the unit. Set when the entry
   --      is created by a call to Lib.Load, and then cannot be changed.

   --    Source_Index
   --      The index in the source file table of the corresponding source file.
   --      Set when the entry is created by a call to Lib.Load and then cannot
   --      be changed.

   --    Error_Location
   --      This is copied from the Sloc field of the Enode argument passed
   --      to Load_Unit. It refers to the enclosing construct which caused
   --
   --      this unit to be loaded, e.g. most typically the with clause that
   --      referenced the unit, and is used for error handling in Par.Load.

   --    Expected_Unit
   --      This is the expected unit name for a file other than the main unit,
   --      since these are cases where we load the unit using Lib.Load and we
   --      know the unit that is expected. It must be the same as Unit_Name
   --      if it is set (see test in Par.Load). Expected_Unit is set to
   --      No_Name for the main unit.

   --    Unit_Name
   --      The name of the unit. Initialized to No_Name by Lib.Load, and then
   --      set by the parser when the unit is parsed to the unit name actually
   --      found in the file (which should, in the absence of errors) be the
   --      same name as Expected_Unit.

   --    Cunit
   --      Pointer to the N_Compilation_Unit node. Initially set to Empty by
   --      Lib.Load, and then reset to the required node by the parser when
   --      the unit is parsed.

   --    Cunit_Entity
   --      Pointer to the entity node for the compilation unit. Initially set
   --      to Empty by Lib.Load, and then reset to the required entity by the
   --      parser when the unit is parsed.

   --    Dependency_Num
   --      This is the number of the unit within the generated dependency
   --      lines (D lines in the ALI file) which are sorted into alphabetical
   --      order. The number is ones origin, so a value of 2 refers to the
   --      second generated D line. The Dependency_Number values are set
   --      as the D lines are generated, and are used to generate proper
   --      unit references in the generated xref information.

   --    Fatal_Error
   --      A flag that is initialized to False, and gets set to True if a fatal
   --      error occurs during the processing of a unit. A fatal error is one
   --      defined as serious enough to stop the next phase of the compiler
   --      from running (i.e. fatal error during parsing stops semantics,
   --      fatal error during semantics stops code generation). Note that
   --      currently, errors of any kind cause Fatal_Error to be set, but
   --      eventually perhaps only errors labeled as Fatal_Errors should be
   --      this severe if we decide to try Sem on sources with minor errors.

   --    Generate_Code
   --      This flag is set True for all units in the current file for which
   --      code is to be generated. This includes the unit explicitly compiled,
   --      together with its specification, and any subunits.

   --    Has_RACW
   --      A Boolean flag, initially set to False when a unit entry is created,
   --      and set to True if the unit defines a remote access to class wide
   --      (RACW) object. This is used for controlling generation of the RA
   --      attribute in the ali file.

   --    Ident_String
   --      N_String_Literal node from a valid pragma Ident that applies to
   --      this unit. If no Ident pragma applies to the unit, then Empty.

   --    Loading
   --      A flag that is used to catch circular WITH dependencies. It is set
   --      True when an entry is initially created in the file table, and set
   --      False when the load is completed, or ends with an error.

   --    Main_Priority
   --      This field is used to indicate the priority of a possible main
   --      program, as set by a pragma Priority. A value of -1 indicates
   --      that the default priority is to be used (and is also used for
   --      entries that do not correspond to possible main programs).

   --    Serial_Number
   --      This field holds a serial number used by New_Internal_Name to
   --      generate unique temporary numbers on a unit by unit basis. The
   --      only access to this field is via the Increment_Serial_Number
   --      routine which increments the current value and returns it. This
   --      serial number is separate for each unit.

   --    Version
   --      This field holds the version of the unit, which is computed as
   --      the exclusive or of the checksums of this unit, and all its
   --      semantically dependent units. Access to the version number field
   --      is not direct, but is done through the routines described below.
   --      When a unit table entry is created, this field is initialized to
   --      the checksum of the corresponding source file. Version_Update is
   --      then called to reflect the contributions of any unit on which this
   --      unit is semantically dependent.

   --    Dependent_Unit
   --      This is a Boolean flag, which is set True to indicate that this
   --      entry is for a semantically dependent unit. This flag is nearly
   --      always set True, the only exception is for a unit that is loaded
   --      by an Rtsfind request in No_Run_Time mode, where the entity that
   --      is obtained by Rtsfind.RTE is for an inlined subprogram or other
   --      entity for which a dependency need not be created.

   --  The units table is reset to empty at the start of the compilation of
   --  each main unit by Lib.Initialize. Entries are then added by calls to
   --  the Lib.Load procedure. The following subprograms are used to access
   --  and modify entries in the Units table. Individual entries are accessed
   --  using a unit number value which ranges from Main_Unit (the first entry,
   --  which is always for the current main unit) to Last_Unit.

   Default_Main_Priority : constant Int := -1;
   --  Value used in Main_Priority field to indicate default main priority

   function Cunit            (U : Unit_Number_Type) return Node_Id;
   function Cunit_Entity     (U : Unit_Number_Type) return Entity_Id;
   function Dependent_Unit   (U : Unit_Number_Type) return Boolean;
   function Dependency_Num   (U : Unit_Number_Type) return Nat;
   function Error_Location   (U : Unit_Number_Type) return Source_Ptr;
   function Expected_Unit    (U : Unit_Number_Type) return Unit_Name_Type;
   function Fatal_Error      (U : Unit_Number_Type) return Boolean;
   function Generate_Code    (U : Unit_Number_Type) return Boolean;
   function Ident_String     (U : Unit_Number_Type) return Node_Id;
   function Has_RACW         (U : Unit_Number_Type) return Boolean;
   function Loading          (U : Unit_Number_Type) return Boolean;
   function Main_Priority    (U : Unit_Number_Type) return Int;
   function Source_Index     (U : Unit_Number_Type) return Source_File_Index;
   function Unit_File_Name   (U : Unit_Number_Type) return File_Name_Type;
   function Unit_Name        (U : Unit_Number_Type) return Unit_Name_Type;
   --  Get value of named field from given units table entry

   procedure Set_Cunit          (U : Unit_Number_Type; N : Node_Id);
   procedure Set_Cunit_Entity   (U : Unit_Number_Type; E : Entity_Id);
   procedure Set_Error_Location (U : Unit_Number_Type; W : Source_Ptr);
   procedure Set_Fatal_Error    (U : Unit_Number_Type; B : Boolean := True);
   procedure Set_Generate_Code  (U : Unit_Number_Type; B : Boolean := True);
   procedure Set_Has_RACW       (U : Unit_Number_Type; B : Boolean := True);
   procedure Set_Ident_String   (U : Unit_Number_Type; N : Node_Id);
   procedure Set_Loading        (U : Unit_Number_Type; B : Boolean := True);
   procedure Set_Main_Priority  (U : Unit_Number_Type; P : Int);
   procedure Set_Unit_Name      (U : Unit_Number_Type; N : Unit_Name_Type);
   --  Set value of named field for given units table entry. Note that we
   --  do not have an entry for each possible field, since some of the fields
   --  can only be set by specialized interfaces (defined below).

   function Version_Get (U : Unit_Number_Type) return Word_Hex_String;
   --  Returns the version as a string with 8 hex digits (upper case letters)

   function Last_Unit return Unit_Number_Type;
   --  Unit number of last allocated unit

   function Num_Units return Nat;
   --  Number of units currently in unit table

   function Entity_Is_In_Main_Unit (E : Entity_Id) return Boolean;
   --  Returns True if the entity E is declared in the main unit, or, in
   --  its corresponding spec, or one of its subunits. Entities declared
   --  within generic instantiations return True if the instantiation is
   --  itself "in the main unit" by this definition. Otherwise False.

   function Get_Source_Unit (N : Node_Id) return Unit_Number_Type;
   pragma Inline (Get_Source_Unit);
   function Get_Source_Unit (S : Source_Ptr) return Unit_Number_Type;
   --  Return unit number of file identified by given source pointer value.
   --  This call must always succeed, since any valid source pointer value
   --  belongs to some previously loaded module. If the given source pointer
   --  value is within an instantiation, this function returns the unit
   --  number of the templace, i.e. the unit containing the source code
   --  corresponding to the given Source_Ptr value. The version taking
   --  a Node_Id argument, N, simply applies the function to Sloc (N).

   function Get_Code_Unit (N : Node_Id) return Unit_Number_Type;
   pragma Inline (Get_Code_Unit);
   function Get_Code_Unit (S : Source_Ptr) return Unit_Number_Type;
   --  This is like Get_Source_Unit, except that in the instantiation case,
   --  it uses the location of the top level instantiation, rather than the
   --  template, so it returns the unit number containing the code that
   --  corresponds to the node N, or the source location S.

   function In_Same_Source_Unit (N1, N2 : Node_Or_Entity_Id) return Boolean;
   pragma Inline (In_Same_Source_Unit);
   --  Determines if the two nodes or entities N1 and N2 are in the same
   --  source unit, the criterion being that Get_Source_Unit yields the
   --  same value for each argument.

   function In_Same_Code_Unit (N1, N2 : Node_Or_Entity_Id) return Boolean;
   pragma Inline (In_Same_Source_Unit);
   --  Determines if the two nodes or entities N1 and N2 are in the same
   --  code unit, the criterion being that Get_Code_Unit yields the same
   --  value for each argument.

   function In_Same_Extended_Unit (S1, S2 : Source_Ptr) return Boolean;
   --  Determines if the two source locations S1 and S2 are in the same
   --  extended unit, where an extended unit is defined as a unit and all
   --  its subunits (considered recursively, i.e. subunits or subunits are
   --  included). Returns true if S1 and S2 are in the same extended unit
   --  and False otherwise.

   function In_Extended_Main_Code_Unit (N : Node_Id) return Boolean;
   --  Return True if the node is in the generated code of the extended main
   --  unit, defined as the main unit, its specification (if any), and all
   --  its subunits (considered recursively). Units for which this enquiry
   --  returns True are those for which code will be generated. Nodes from
   --  instantiations are included in the extended main unit for this call.

   function In_Extended_Main_Source_Unit (N : Node_Id) return Boolean;
   --  Return True if the node is in the source text of the extended main
   --  unit, defined as the main unit, its specification (if any), and all
   --  its subunits (considered recursively). Units for which this enquiry
   --  returns True are those for which code will be generated. This differs
   --  from In_Extended_Main_Code_Unit only in that instantiations are not
   --  included for the purposes of this call.

   function Earlier_In_Extended_Unit (S1, S2 : Source_Ptr) return Boolean;
   --  Given two Sloc values  for which In_Same_Extended_Unit is true,
   --  determine if S1 appears before S2. Returns True if S1 appears before
   --  S2, and False otherwise. The result is undefined if S1 and S2 are
   --  not in the same extended unit.

   function Get_Cunit_Unit_Number (N : Node_Id) return Unit_Number_Type;
   --  Return unit number of the unit whose N_Compilation_Unit node is the
   --  one passed as an argument. This must always succeed since the node
   --  could not have been built without making a unit table entry.

   function Get_Cunit_Entity_Unit_Number
     (E    : Entity_Id)
      return Unit_Number_Type;
   --  Return unit number of the unit whose compilation unit spec entity is
   --  the one passed as an argument. This must always succeed since the
   --  entity could not have been built without making a unit table entry.

   function Increment_Serial_Number return Nat;
   --  Increment Serial_Number field for current unit, and return the
   --  incremented value.

   procedure Replace_Linker_Option_String
     (S : String_Id; Match_String : String);
   --  Relace an existing Linker_Option if the prefix Match_String
   --  matches, otherwise call Store_Linker_Option_String.

   procedure Store_Linker_Option_String (S : String_Id);
   --  This procedure is called to register the string from a pragma
   --  Linker_Option. The argument is the Id of the string to register.

   procedure Initialize;
   --  Initialize internal tables

   procedure Lock;
   --  Lock internal tables before calling back end

   procedure Tree_Write;
   --  Writes out internal tables to current tree file using Tree_Write

   procedure Tree_Read;
   --  Initializes internal tables from current tree file using Tree_Read

   function Is_Loaded (Uname : Unit_Name_Type) return Boolean;
   --  Determines if unit with given name is already loaded, i.e. there is
   --  already an entry in the file table with this unit name for which the
   --  corresponding file was found and parsed. Note that the Fatal_Error flag
   --  of this entry must be checked before proceeding with further processing.

   procedure Version_Referenced (S : String_Id);
   --  This routine is called from Exp_Attr to register the use of a Version
   --  or Body_Version attribute. The argument is the external name used to
   --  access the version string.

   procedure List (File_Names_Only : Boolean := False);
   --  Lists units in active library (i.e. generates output consisting of a
   --  sorted listing of the units represented in File table, with the
   --  exception of the main unit). If File_Names_Only is set to True, then
   --  the list includes only file names, and no other information. Otherwise
   --  the unit name and time stamp are also output. File_Names_Only also
   --  restricts the list to exclude any predefined files.

   function Generic_Separately_Compiled (E : Entity_Id) return Boolean;
   --  Most generic units must be separately compiled. Since we always use
   --  macro substitution for generics, the resulting object file is a dummy
   --  one with no code, but the ali file has the normal form, and we need
   --  this ali file so that the binder can work out a correct order of
   --  elaboration. However, we do not need to separate compile generics
   --  if the generic files are language defined, since in this case there
   --  are no order of elaborration problems, and we can simply incorporate
   --  the context clause of the generic unit into the client. There are two
   --  reasons for making this exception for predefined units. First, clearly
   --  it is more efficient not to introduce extra unnecessary files. Second,
   --  the old version of GNAT did not compile any generic units. That was
   --  clearly incorrect in some cases of complex order of elaboration and
   --  was fixed in version 3.10 of GNAT. However, the transition would have
   --  caused bootstrap path problems in the case of generics used in the
   --  compiler itself. The only such generics are predefined ones. This
   --  function returns True if the given generic unit entity E is for a
   --  generic unit that should be separately compiled, and false otherwise.

private
   pragma Inline (Cunit);
   pragma Inline (Cunit_Entity);
   pragma Inline (Dependency_Num);
   pragma Inline (Dependent_Unit);
   pragma Inline (Fatal_Error);
   pragma Inline (Generate_Code);
   pragma Inline (Has_RACW);
   pragma Inline (Increment_Serial_Number);
   pragma Inline (Loading);
   pragma Inline (Main_Priority);
   pragma Inline (Set_Cunit);
   pragma Inline (Set_Cunit_Entity);
   pragma Inline (Set_Fatal_Error);
   pragma Inline (Set_Generate_Code);
   pragma Inline (Set_Has_RACW);
   pragma Inline (Set_Loading);
   pragma Inline (Set_Main_Priority);
   pragma Inline (Set_Unit_Name);
   pragma Inline (Source_Index);
   pragma Inline (Unit_File_Name);
   pragma Inline (Unit_Name);

   type Unit_Record is record
      Unit_File_Name   : File_Name_Type;
      Unit_Name        : Unit_Name_Type;
      Expected_Unit    : Unit_Name_Type;
      Source_Index     : Source_File_Index;
      Cunit            : Node_Id;
      Cunit_Entity     : Node_Id;
      Dependency_Num   : Int;
      Dependent_Unit   : Boolean;
      Fatal_Error      : Boolean;
      Generate_Code    : Boolean;
      Has_RACW         : Boolean;
      Ident_String     : Node_Id;
      Loading          : Boolean;
      Main_Priority    : Int;
      Serial_Number    : Nat;
      Version          : Word;
      Error_Location   : Source_Ptr;
      Serial_Ref_Unit  : Unit_Number_Type;
   end record;

   package Units is new Table.Table (
     Table_Component_Type => Unit_Record,
     Table_Index_Type     => Unit_Number_Type,
     Table_Low_Bound      => Main_Unit,
     Table_Initial        => Alloc.Units_Initial,
     Table_Increment      => Alloc.Units_Increment,
     Table_Name           => "Units");

   --  The following table stores strings from pragma Linker_Option lines

   package Linker_Option_Lines is new Table.Table (
     Table_Component_Type => String_Id,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.Linker_Option_Lines_Initial,
     Table_Increment      => Alloc.Linker_Option_Lines_Increment,
     Table_Name           => "Linker_Option_Lines");

   Load_Msg_Sloc : Source_Ptr;
   --  Location for placing error messages (a token in the main source text)
   --  This is set from Sloc (Enode) by Load only in the case where this Sloc
   --  is in the main source file. This ensures that not found messages and
   --  circular dependency messages reference the original with in this source.

   type Unit_Ref_Table is array (Pos range <>) of Unit_Number_Type;
   --  Type to hold list of indirect references to unit number table

   --  The Load_Stack table contains a list of unit numbers (indexes into the
   --  unit table) of units being loaded on a single dependency chain. The
   --  First entry is the main unit. The second entry, if present is a unit
   --  on which the first unit depends, etc. This stack is used to generate
   --  error messages showing the dependency chain if a file is not found.
   --  The Load function makes an entry in this table when it is called, and
   --  removes the entry just before it returns.

   package Load_Stack is new Table.Table (
     Table_Component_Type => Unit_Number_Type,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Load_Stack_Initial,
     Table_Increment      => Alloc.Load_Stack_Increment,
     Table_Name           => "Load_Stack");

   procedure Sort (Tbl : in out Unit_Ref_Table);
   --  This procedure sorts the given unit reference table in order of
   --  ascending unit names, where the ordering relation is as described
   --  by the comparison routines provided by package Uname.

   --  The Version_Ref table records Body_Version and Version attribute
   --  references. The entries are simply the strings for the external
   --  names that correspond to the referenced values.

   package Version_Ref is new Table.Table (
     Table_Component_Type => String_Id,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Version_Ref");

end Lib;
