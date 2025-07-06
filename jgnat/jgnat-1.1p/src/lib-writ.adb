------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . W R I T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.143 $
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with ALI;      use ALI;
with Atree;    use Atree;
with Casing;   use Casing;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Fname;    use Fname;
with Lib.Util; use Lib.Util;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Gnatvsn;  use Gnatvsn;
with Opt;      use Opt;
with Osint;    use Osint;
with Restrict; use Restrict;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Uname;    use Uname;

with System.WCh_Con; use System.WCh_Con;

package body Lib.Writ is

   ---------------
   -- Write_ALI --
   ---------------

   procedure Write_ALI (Object : Boolean) is

      -----------------------------------
      -- Format of Library Information --
      -----------------------------------

      --  This section  describes the format of the library information that is
      --  associated with object files. The exact method of this association is
      --  potentially implementation dependent and is described and implemented
      --  in package From the point of view of the description here, all we
      --  need to know is that the information is represented as a string of
      --  characters that is somehow associated with an object file, and can be
      --  retrieved. If no library information exists for a given object file,
      --  then we take this as equivalent to the non-existence of the object
      --  file, as if source file has not been previously compiled.

      --  The library information is written as a series of lines of the form:

      --    Key_Character parameter parameter ...

      ------------------
      -- Header Lines --
      ------------------

      --  The initial header lines in the file give information about the
      --  compilation environment, and identify other special information
      --  such as main program parameters.

      --  ----------------
      --  -- V  Version --
      --  ----------------

      --    V "xxxxxxxxxxxxxxxx"
      --
      --      This line indicates the library output version, as defined in
      --      Gnatvsn. It ensures that separate object modules of a program are
      --      consistent. It has to be changed if anything changes which would
      --      affect successful binding of separately compiled modules.
      --      Examples of such changes are modifications in the format of the
      --      library info described in this package, or modifications to
      --      calling sequences, or to the way that data is represented.

      --  ---------------------
      --  -- M  Main Program --
      --  ---------------------

      --    M type [priority] [T=time-slice] W=?

      --      This line appears only if the main unit for this file is
      --      suitable for use as a main program. The parameters are:

      --        type

      --          P for a parameterless procedure
      --          F for a function returning a value of integral type
      --            (used for writing a main program returning an exit status)

      --        priority

      --          Present only if there was a valid pragma Priority in the
      --          corresponding unit to set the main task priority. It is
      --          an unsigned decimal integer.

      --        T=time-slice

      --          Present only if there was a valid pragma Time_Slice in the
      --          corresponding unit. It is an unsigned decimal integer in
      --          the range 0 .. 10**9 giving the time slice value in units
      --          of milliseconds. The actual significance of this parameter
      --          is target dependent.

      --        W=?

      --          This parameter indicates the wide character encoding
      --          method used when compiling the main program file. The ?
      --          character is the single character used in the -gnatW?
      --          switch. This is used to provide the default wide-character
      --          encoding for Wide_Text_IO files.

      --  -----------------
      --  -- A  Argument --
      --  -----------------

      --    A argument

      --      One of these lines appears for each of the arguments present
      --      in the call to the gnat1 program. This can be used if it is
      --      necessary to reconstruct this call (e.g. for fix and continue)

      --  -------------------
      --  -- P  Parameters --
      --  -------------------

      --    P <<parameters>>

      --      Indicates various information that applies to the compilation
      --      of the corresponding source unit. Parameters is a sequence of
      --      zero or more two letter codes that indicate configuration
      --      pragmas and other parameters that apply:
      --
      --      Present if the unit uses tasking directly or indirectly and
      --      has one or more valid xxx_Policy pragmas that apply to the unit.
      --      The arguments are as follows:
      --
      --         CE   Compilation errors. If this is present it means that the
      --              ali file resulted from a compilation with the -gnatQ
      --              switch set, and illegalities were detected. The ali
      --              file contents may not be completely reliable, but the
      --              format will be correct and complete. Note that NO is
      --              always present if CE is present.
      --
      --         FD   Configuration pragmas apply to this unit which specify a
      --              possibly non-standard floating point format (VAX float
      --              with Long_Float using D_Float)
      --
      --         FG   Configuration pragmas apply to this unit which specify a
      --              possibly non-standard floating-point format (VAX float
      --              with Long_Float using G_Float).
      --
      --         FI   Configuration pragmas apply to this unit which specify a
      --              possibly non-standard floating-point format (IEEE float)
      --
      --         Lx   Indicates that a valid Locking_Policy pragma applies to
      --              the unit, where x is the first character (upper case) of
      --              the policy name (e.g. 'C' for Ceiling_Locking)
      --
      --         NO   No object. This flag indicates that the unit was not
      --              compiled to produce an object. This can occur as a
      --              result of the use of -gnatc, or if no object can be
      --              produced (e.g. when a package spec is compiled
      --              instead of the body, or a subunit on its own).
      --
      --         NR  No_Run_Time pragma in effect
      --
      --         NS  Normalize_Scalars pragma in effect for this file
      --
      --         Qx  Indicates that a valid Queuing_Policy pragma applies to
      --             the unit, where x is the first character (upper case) of
      --             the policy name (e.g. 'P' for Priority_Queuing).
      --
      --         Tx  Indicates that a valid Task_Dispatching_Policy pragma
      --             applies to the unit, where x is the first character
      --             (upper case) of the policy name (e.g. 'F' for
      --             FIFO_Within_Priorities).
      --
      --         UA  Unreserve_All_Interrupts pragma processed
      --
      --         UX  Generated code contains unit exception table pointer
      --             (i.e. it uses zero-cost exceptions, and there is at
      --             least one subprogram present).
      --
      --         ZX  Unit uses zero-cost exceptions and has generated
      --             exception tables. If ZX is not present, the old
      --             longjmp/setjmp exceptions are in use.
      --
      --      Note that language defined units never output policy (Lx,Tx,Qx)
      --      parameters. Language defined units must correctly handle all
      --      possible cases. These values are checked for consistency by the
      --      binder and then copied to the generated binder output file.

      --  ---------------------
      --  -- R  Restrictions --
      --  ---------------------

      --    R <<restriction-characters>>

      --      This line records information regarding restrictions. The
      --      parameter is a string of characters, one for each entry in
      --      Restrict.Partition_Restrictions, in order. There are three
      --      settings possible settings for each restriction:

      --        r   Restricted. Unit was compiled under control of a pragma
      --            Restrictions for the corresponding restriction. In
      --            this case the unit certainly does not violate the
      --            Restriction, since this would have been detected by
      --            the compiler.

      --        n   Not used. The unit was not compiled under control of a
      --            pragma Restrictions for the corresponding restriction,
      --            and does not make any use of the referenced feature.

      --        v   Violated. The unit was not compiled uner control of a
      --            pragma Restrictions for the corresponding restriction,
      --            and it does indeed use the referenced feature.

      --      This information is used in the binder to check consistency,
      --      i.e. to detect cases where one unit has "r" and another unit
      --      has "v", which is not permitted, since these restrictions
      --      are partition-wide.

      ----------------------------
      -- Compilation Unit Lines --
      ----------------------------

      --  Following these header lines, a set of information lines appears for
      --  each compilation unit that appears in the corresponding object file.
      --  In particular, when a package body or subprogram body is compiled,
      --  there will be two sets of information, one for the spec and one for
      --  the body. with the entry for the body appearing first. This is the
      --  only case in which a single ALI file contains more than one unit (in
      --  particular note that subunits do *not* count as compilation units for
      --  this purpose, and generate no library information, since they are
      --  inlined).

      --  --------------------
      --  -- U  Unit Header --
      --  --------------------

      --  The lines for each compilation unit have the following form.

      --    U unit-name source-name version <<attributes>>
      --
      --      This line identifies the unit to which this section of the
      --      library information file applies. The first three parameters are
      --      the unit name in internal format, as described in package Uname,
      --      and the name of the source file containing the unit.
      --
      --      Version is the version given as eight hexadecimal characters
      --      with upper case letters. This value is the exclusive or of the
      --      source checksums of the unit and all its semantically dependent
      --      units.
      --
      --      The <<attributes>> are a series of two letter codes indicating
      --      information about the unit:
      --
      --         EB  Unit has pragma Elaborate_Body
      --
      --         GE  Unit is a generic declaration, or corresponding body
      --
      --         IL  Unit source uses a style with identifiers in all lower
      --         IU  case (IL) or all upper case (IU). If the standard mixed-
      --             case usage is detected, or the compiler cannot determine
      --             the style, then no I parameter will appear.
      --
      --         KM  Unit source uses a style with keywords in mixed case
      --         KU  (KM) or all upper case (KU). If the standard lower-case
      --             usage is detected, or the compiler cannot determine the
      --             style, then no K parameter will appear.
      --
      --         NE  Unit has no elaboration routine. All subprogram bodies
      --             and specs are in this category. Package bodies and specs
      --             may or may not have NE set, depending on whether or not
      --             elaboration code is required. Set if N_Compilation_Unit
      --             node has flag Has_No_Elaboration_Code set.
      --
      --         PK  Unit is package, rather than a subprogram
      --
      --         PU  Unit has pragma Pure
      --
      --         PR  Unit has pragma Preelaborate
      --
      --         RA  Unit declares a Remote Access to Class-Wide (RACW) type
      --
      --         RC  Unit has pragma Remote_Call_Interface
      --
      --         RT  Unit has pragma Remote_Types
      --
      --         SP  Unit has pragma Shared_Passive.
      --
      --         SU  Unit is a subprogram, rather than a package
      --
      --      The attributes may appear in any order, separated by spaces.

      --  ---------------------
      --  -- W  Withed Units --
      --  ---------------------

      --  Following each U line, is a series of lines of the form

      --    W unit-name [source-name lib-name] [E] [EA] [ED]
      --
      --      One of these lines is present for each unit that is mentioned in
      --      an explicit with clause by the current unit. The first parameter
      --      is the unit name in internal format. The second parameter is the
      --      file name of the file that must be compiled to compile this unit
      --      (which is usually the file for the body, except for packages
      --      which have no body). The third parameter is the file name of the
      --      library information file that contains the results of compiling
      --      this unit. The optional modifiers are used as follows:
      --
      --        E   pragma Elaborate applies to this unit
      --
      --        EA  pragma Elaborate_All applies to this unit
      --
      --        ED  Elaborate_All_Desirable set for this unit, which means
      --            that there is no Elaborate_All, but the analysis suggests
      --            that Program_Error may be raised if the Elaborate_All
      --            conditions cannot be satisfied. The binder will attempt
      --            to treat ED as EA if it can.
      --
      --      The parameter source-name and lib-name are omitted for the case
      --      of a generic unit compiled with earlier versions of GNAT which
      --      did not generate object or ali files for generics.

      ---------------------
      -- Reference Lines --
      ---------------------

      --  The reference lines contain information about references from
      --  any of the units in the compilation (including, body version
      --  and version attributes, linker options pragmas and source
      --  dependencies.

      --  -----------------------
      --  -- L  Linker_Options --
      --  -----------------------

      --  Following the unit information is an optional series of lines that
      --  indicates the usage of pragma Linker_Options. For each appearence
      --  of pragma Linker_Actions in any of the units for which unit lines
      --  are present, a line of the form:

      --    L "string"

      --      where string is the string from the unit line enclosed in quotes.
      --      Within the quotes the following can occur:

      --        c    graphic characters in range 20-7E other than " or {
      --        ""   indicating a single " character
      --        {hh} indicating a character whose code is hex hh (0-9,A-F)
      --        {00} [ASCII.NUL] is used as a separator character
      --             to separate multiple arguments of a single
      --             Linker_Options pragma.

      --      For further details, see Stringt.Write_String_Table_Entry. Note
      --      that wide characters in the form {hhhh} cannot be produced, since
      --      pragma Linker_Option accepts only String, not Wide_String.

      --  ------------------------------------
      --  -- E  External Version References --
      --  ------------------------------------

      --  One of these lines is present for each use of 'Body_Version or
      --  'Version in any of the units of the compilation. These are used
      --  by the linker to determine which version symbols must be output.
      --  The format is simply:

      --    E name

      --  where name is the external name, i.e. the unit name with either
      --  a S or a B for spec or body version referenced (Body_Version
      --  always references the body, Version references the Spec, except
      --  in the case of a reference to a subprogram with no separate spec).
      --  Upper half and wide character codes are encoded using the same
      --  method as in Namet (Uhh for upper half, Whhhh for wide character,
      --  where hh are hex digits).

      --  ---------------------
      --  -- D  Dependencies --
      --  ---------------------

      --  The dependency lines indicate the source files on which the compiled
      --  units depend. This is used by the binder for consistency checking.

      --    D source-name time-stamp checksum [subunit-name]

      --      The time-stamp field contains the time stamp of the
      --      corresponding source file. See types.ads for details on
      --      time stamp representation.

      --      The checksum is an 8-hex digit representation of the source
      --      file checksum, with letters given in upper case.

      --      The subunit name is present only if the dependency line is for
      --      a subunit. It contains the fully qualified name of the subunit
      --      in all lower case letters.

      --      Note: blank lines are ignored when the library information is
      --      read, and separate sections of the file are separated by blank
      --      lines to ease readability. Blanks between fields are also
      --      ignored.

      --------------------------
      -- Cross-Reference Data --
      --------------------------

      --  The cross-reference data follows the dependency lines. See
      --  the spec of Lib.Xref for details on the format of this data.

      ----------------
      -- Local Data --
      ----------------

      With_Flags : array (Units.First .. Units.Last) of Boolean;
      --  Array of flags to show which units are with'ed

      Elab_Flags : array (Units.First .. Units.Last) of Boolean;
      --  Array of flags to show which units have pragma Elaborate set

      Elab_All_Flags : array (Units.First .. Units.Last) of Boolean;
      --  Array of flags to show which units have pragma Elaborate All set

      Elab_Des_Flags : array (Units.First .. Units.Last) of Boolean;
      --  Array of flags to show which units have Elaborate_All_Desirable set

      Sdep_Table : Unit_Ref_Table (1 .. Pos (Units.Last - Units.First + 1));
      --  Sorted table of source dependencies

      Num_Sdep : Nat := 0;
      --  Number of active entries in Sdep_Table

      -----------------------
      -- Local Subprograms --
      -----------------------

      procedure Collect_Withs (Cunit : Node_Id);
      --  Collect with lines for entries in the context clause of the
      --  given compilation unit, Cunit.

      procedure Set_Compilation_Arguments;
      --  Store compilation arguments, see Compilation_Arguments table.

      procedure Update_Tables_From_ALI_File;
      --  Given an up to date ALI file (see Up_To_Date_ALI_file_Exists
      --  function), update tables from the ALI information, including
      --  specifically the Compilation_Arguments table.

      function Up_To_Date_ALI_File_Exists return Boolean;
      --  If there exists an ALI file that is up to date, then this function
      --  initializes the tables in the ALI spec to contain information on
      --  this file (using Scan_ALI) and returns True. If no file exists,
      --  or the file is not up to date, then False is returned.

      procedure Write_Unit_Information (Unit_Num : Unit_Number_Type);
      --  Write out the library information for one unit for which code is
      --  generated (includes unit line and with lines).

      procedure Write_With_Lines;
      --  Write out with lines collected by calls to Collect_Withs

      -------------------
      -- Collect_Withs --
      -------------------

      procedure Collect_Withs (Cunit : Node_Id) is
         Item : Node_Id;
         Unum : Unit_Number_Type;

      begin
         Item := First (Context_Items (Cunit));
         while Present (Item) loop

            if Nkind (Item) = N_With_Clause then
               Unum := Get_Cunit_Unit_Number (Library_Unit (Item));
               With_Flags (Unum) := True;

               if Elaborate_Present (Item) then
                  Elab_Flags (Unum) := True;
               end if;

               if Elaborate_All_Present (Item) then
                  Elab_All_Flags (Unum) := True;
               end if;

               if Elaborate_All_Desirable (Cunit_Entity (Unum)) then
                  Elab_Des_Flags (Unum) := True;
               end if;
            end if;

            Next (Item);
         end loop;
      end Collect_Withs;

      -------------------------------
      -- Set_Compilation_Arguments --
      -------------------------------

      procedure Set_Compilation_Arguments is

         --  Import argc and argc from toplev.c. This is a bit low
         --  level and sometime we should make a better abstraction.

         save_argc : Nat;
         pragma Import (C, save_argc);

         subtype Big_String is String (Positive);
         type BSP is access Big_String;

         type Arg_Array is array (Nat) of BSP;
         type Arg_Array_Ptr is access Arg_Array;

         save_argv : Arg_Array_Ptr;
         pragma Import (C, save_argv);

         Testq : constant String := "quiet"    & ASCII.NUL;
         Testd : constant String := "dumpbase" & ASCII.NUL;
         --  Option switches that get deleted from the stored arguments

         A : Nat;

      begin
         A := 1;
         while A < save_argc loop

            --  We only capture switches and nothing else. In particular
            --  we do not pick up file or directory names (including the
            --  source file name, or files/directories after -I or -o)

            if save_argv (A).all (1) /= '-'

            --  Skip -o. Note that we will always skip the following argument
            --  if there is a separate file name for the -o switch.

              or else save_argv (A).all (2) = 'o'

            --  Skip -I. Note that we will always skip the following argument
            --  if there is a separate directory name for the -I switch.

              or else save_argv (A).all (2) = 'I'

            --  Skip over -quiet

              or else save_argv (A).all (2 .. Testq'Length + 1) = Testq
            then
               A := A + 1;

            --  Skip over -dumpbase and following argument

            elsif save_argv (A).all (2 .. Testd'Length + 1) = Testd then
               A := A + 2;

            --  Otherwise acquire switch

            else
               for J in 1 .. Natural'Last loop
                  if save_argv (A).all (J) = ASCII.NUL then
                     Compilation_Arguments.Increment_Last;
                     Compilation_Arguments.Table
                       (Compilation_Arguments.Last) :=
                         new String'(save_argv (A).all (1 .. J - 1));
                     exit;
                  end if;
               end loop;

               A := A + 1;
            end if;
         end loop;
      end Set_Compilation_Arguments;

      ---------------------------------
      -- Update_Tables_From_ALI_File --
      ---------------------------------

      procedure Update_Tables_From_ALI_File is
      begin
         --  Build Compilation_Arguments table

         Compilation_Arguments.Init;

         for J in First_Arg_Entry .. Args.Last loop
            Compilation_Arguments.Increment_Last;
            Compilation_Arguments.Table (Compilation_Arguments.Last) :=
              Args.Table (J);
         end loop;
      end Update_Tables_From_ALI_File;

      --------------------------------
      -- Up_To_Date_ALI_File_Exists --
      --------------------------------

      function Up_To_Date_ALI_File_Exists return Boolean is
         Name : File_Name_Type;
         Text : Text_Buffer_Ptr;
         Id   : Sdep_Id;
         Sind : Source_File_Index;

      begin
         Opt.Check_Object_Consistency := True;
         Read_Library_Info (Name, Text);

         --  Return if we could not find an ALI file

         if Text = null then
            return False;
         end if;

         --  Return if ALI file has bad format

         Initialize_ALI;

         if Scan_ALI (Name, Text, False, Err => True) = No_ALI_Id then
            return False;
         end if;

         --  If we have an OK ALI file, check if it is up to date
         --  Note that we assume that the ALI read has all the entries
         --  we have in our table, plus some additional ones (that can
         --  come from expansion).

         Id := First_Sdep_Entry;
         for J in 1 .. Num_Sdep loop
            Sind := Units.Table (Sdep_Table (J)).Source_Index;

            while Sdep.Table (Id).Sfile /= File_Name (Sind) loop
               if Id = Sdep.Last then
                  return False;
               else
                  Id := Id + 1;
               end if;
            end loop;

            if Sdep.Table (Id).Stamp /= Time_Stamp (Sind) then
               return False;
            end if;
         end loop;

         return True;
      end Up_To_Date_ALI_File_Exists;

      ----------------------------
      -- Write_Unit_Information --
      ----------------------------

      procedure Write_Unit_Information (Unit_Num : Unit_Number_Type) is
         Unode : constant Node_Id   := Cunit (Unit_Num);
         Ukind : constant Node_Kind := Nkind (Unit (Unode));
         Pnode : Node_Id;

      begin
         Write_Info_Initiate ('U');
         Write_Info_Char (' ');
         Write_Info_Name (Unit_Name (Unit_Num));
         Write_Info_Tab (25);
         Write_Info_Name (Unit_File_Name (Unit_Num));

         Write_Info_Tab (49);
         Write_Info_Str (Version_Get (Unit_Num));

         --  We set the Elaborate_Body indication if either an explicit pragma
         --  was present, or if this is an instantiation. RM 12.3(20) requires
         --  that the body be immediately elaborated after the spec. We would
         --  normally do that anyway, but the EB we generate here ensures that
         --  this gets done even when we use the -p gnatbind switch.

         if Has_Pragma_Elaborate_Body (Cunit_Entity (Unit_Num))
           or else (Ukind = N_Package_Declaration
                     and then Is_Generic_Instance (Cunit_Entity (Unit_Num))
                     and then Present (Corresponding_Body (Unit (Unode))))
         then
            Write_Info_Str (" EB");
         end if;

         if Has_No_Elaboration_Code (Cunit (Unit_Num)) then
            Write_Info_Str (" NE");
         end if;

         if Is_Preelaborated (Cunit_Entity (Unit_Num)) then
            Write_Info_Str (" PR");
         end if;

         if Is_Pure (Cunit_Entity (Unit_Num)) then
            Write_Info_Str (" PU");
         end if;

         if Has_RACW (Unit_Num) then
            Write_Info_Str (" RA");
         end if;

         if Is_Remote_Call_Interface (Cunit_Entity (Unit_Num)) then
            Write_Info_Str (" RC");
         end if;

         if Is_Remote_Types (Cunit_Entity (Unit_Num)) then
            Write_Info_Str (" RT");
         end if;

         if Is_Shared_Passive (Cunit_Entity (Unit_Num)) then
            Write_Info_Str (" SP");
         end if;

         if Ukind = N_Subprogram_Declaration
           or else Ukind = N_Subprogram_Body
         then
            Write_Info_Str (" SU");

         elsif Ukind = N_Package_Declaration
                 or else
               Ukind = N_Generic_Package_Declaration
         then
            Write_Info_Str (" PK");

         elsif Ukind = N_Package_Body then

            --  If this is a wrapper for a subprogram instance, mark it as
            --  a subprogram.

            if (Nkind (Original_Node (Unit (Unode)))
                  = N_Function_Instantiation
              or else
                Nkind (Original_Node (Unit (Unode)))
                  = N_Procedure_Instantiation)
            then
               Write_Info_Str (" SU");

            else
               Write_Info_Str (" PK");
            end if;
         end if;

         if Ukind in N_Generic_Declaration
           or else
             (Present (Library_Unit (Unode))
                and then
              Nkind (Unit (Library_Unit (Unode))) in N_Generic_Declaration)
         then
            Write_Info_Str (" GE");
         end if;

         if not Is_Internal_File_Name (Unit_File_Name (Unit_Num), True) then
            case Identifier_Casing (Source_Index (Unit_Num)) is
               when All_Lower_Case => Write_Info_Str (" IL");
               when All_Upper_Case => Write_Info_Str (" IU");
               when others         => null;
            end case;

            case Keyword_Casing (Source_Index (Unit_Num)) is
               when Mixed_Case     => Write_Info_Str (" KM");
               when All_Upper_Case => Write_Info_Str (" KU");
               when others         => null;
            end case;
         end if;

         Write_Info_EOL;

         --  Generate with lines, first those that are directly with'ed

         for J in With_Flags'Range loop
            With_Flags (J) := False;
            Elab_Flags (J) := False;
            Elab_All_Flags (J) := False;
            Elab_Des_Flags (J) := False;
         end loop;

         Collect_Withs (Cunit (Unit_Num));

         --  For a body, we must also check for any subunits which belong to
         --  it and which have context clauses of their own, since these
         --  with'ed units are part of its own elaboration dependencies.

         if Nkind (Unit (Cunit (Unit_Num))) in N_Unit_Body then
            for S in Units.First .. Units.Last loop

               --  We are only interested in subunits

               if Nkind (Unit (Cunit (S))) = N_Subunit then
                  Pnode := Library_Unit (Cunit (S));

                  --  In gnatc mode, the errors in the subunits will not
                  --  have been recorded, but the analysis of the subunit
                  --  may have failed. There is no information to add to
                  --  ALI file in this case.

                  if No (Pnode) then
                     exit;
                  end if;

                  --  Find ultimate parent of the subunit

                  while Nkind (Unit (Pnode)) = N_Subunit loop
                     Pnode := Library_Unit (Pnode);
                  end loop;

                  --  See if it belongs to current unit, and if so, include
                  --  its with_clauses.

                  if Pnode = Cunit (Unit_Num) then
                     Collect_Withs (Cunit (S));
                  end if;
               end if;
            end loop;
         end if;

         Write_With_Lines;
      end Write_Unit_Information;

      ----------------------
      -- Write_With_Lines --
      ----------------------

      procedure Write_With_Lines is
         With_Table : Unit_Ref_Table (1 .. Pos (Units.Last - Units.First + 1));
         Num_Withs  : Int := 0;
         Unum       : Unit_Number_Type;
         Cunit      : Node_Id;
         Cunite     : Entity_Id;
         Uname      : Unit_Name_Type;
         Fname      : File_Name_Type;
         Pname      : constant Unit_Name_Type :=
                        Get_Parent_Spec_Name (Unit_Name (Main_Unit));
         Body_Fname : File_Name_Type;

      begin
         --  Loop to build the with table. A with on the main unit itself
         --  is ignored (AARM 10.2(14a)). Such a with-clause can occur if
         --  the main unit is a subprogram with no spec, and a subunit of
         --  it unecessarily withs the parent.

         for J in Units.First + 1 .. Units.Last loop

            --  Add element to with table if it is with'ed or if it is the
            --  parent spec of the main unit (case of main unit is a child
            --  unit). The latter with is not needed for semantic purposes,
            --  but is required by the binder for elaboration purposes.

            if (With_Flags (J) or else Unit_Name (J) = Pname)
              and then Units.Table (J).Dependent_Unit
            then
               Num_Withs := Num_Withs + 1;
               With_Table (Num_Withs) := J;
            end if;
         end loop;

         --  Sort and output the table

         Sort (With_Table (1 .. Num_Withs));

         for J in 1 .. Num_Withs loop
            Unum   := With_Table (J);
            Cunit  := Units.Table (Unum).Cunit;
            Cunite := Units.Table (Unum).Cunit_Entity;
            Uname  := Units.Table (Unum).Unit_Name;
            Fname  := Units.Table (Unum).Unit_File_Name;

            Write_Info_Initiate ('W');
            Write_Info_Char (' ');
            Write_Info_Name (Uname);

            --  Now we need to figure out the names of the files that contain
            --  the with'ed unit. These will usually be the files for the body,
            --  except except in the case of a package that has no body.

            if (Nkind (Unit (Cunit)) not in N_Generic_Declaration
                  and then
                Nkind (Unit (Cunit)) not in N_Generic_Renaming_Declaration)
              or else Generic_Separately_Compiled (Cunite)
            then
               Write_Info_Tab (25);

               if Is_Spec_Name (Uname) then
                  Body_Fname :=
                    Get_File_Name (Get_Body_Name (Uname), Subunit => False);
               else
                  Body_Fname := Get_File_Name (Uname, Subunit => False);
               end if;

               --  A package is considered to have a body if it requires
               --  a body or if a body is present in Ada 83 mode.

               if Body_Required (Cunit)
                 or else (Ada_83
                           and then Full_Source_Name (Body_Fname) /= No_File)
               then
                  Write_Info_Name (Body_Fname);
                  Write_Info_Tab (49);
                  Write_Info_Name (Lib_File_Name (Body_Fname));
               else
                  Write_Info_Name (Fname);
                  Write_Info_Tab (49);
                  Write_Info_Name (Lib_File_Name (Fname));
               end if;

               if Elab_Flags (Unum) then
                  Write_Info_Str ("  E");
               end if;

               if Elab_All_Flags (Unum) then
                  Write_Info_Str ("  EA");
               end if;

               if Elab_Des_Flags (Unum) then
                  Write_Info_Str ("  ED");
               end if;
            end if;

            Write_Info_EOL;
         end loop;
      end Write_With_Lines;

      ----------
      -- Writ --
      ----------

   begin
      --  Build sorted source dependency table. We do this right away,
      --  because it is referenced by Up_To_Date_ALI_File_Exists.

      for Unit in Units.First .. Units.Last loop
         Num_Sdep := Num_Sdep + 1;
         Sdep_Table (Num_Sdep) := Unit;
      end loop;

      Lib.Sort (Sdep_Table (1 .. Num_Sdep));

      --  If we are not generating code, and there is an up to date
      --  ali file accessible, read it, and acquire the compilation
      --  arguments from this file.

      if Operating_Mode /= Generate_Code then
         if Up_To_Date_ALI_File_Exists then
            Update_Tables_From_ALI_File;
            return;
         end if;
      end if;

      --  Otherwise acquire compilation arguments and prepare to write
      --  out a new ali file.

      Set_Compilation_Arguments;
      Create_Output_Library_Info;

      --  Output version line

      Write_Info_Initiate ('V');
      Write_Info_Str (" """);
      Write_Info_Str (Library_Version);
      Write_Info_Char ('"');

      Write_Info_EOL;

      --  Output main program line if this is acceptable main program

      declare
         U : Node_Id := Unit (Units.Table (Main_Unit).Cunit);
         S : Node_Id;

         procedure M_Parameters;
         --  Output parameters for main program line

         procedure M_Parameters is
         begin
            if Main_Priority (Main_Unit) /= Default_Main_Priority then
               Write_Info_Char (' ');
               Write_Info_Nat (Main_Priority (Main_Unit));
            end if;

            if Opt.Time_Slice_Set then
               Write_Info_Str (" T=");
               Write_Info_Nat (Opt.Time_Slice_Value);
            end if;

            Write_Info_Str (" W=");
            Write_Info_Char
              (WC_Encoding_Letters (Wide_Character_Encoding_Method));

            Write_Info_EOL;
         end M_Parameters;

      begin
         if Nkind (U) = N_Subprogram_Body
           or else (Nkind (U) = N_Package_Body
                      and then
                        (Nkind (Original_Node (U)) = N_Function_Instantiation
                           or else
                         Nkind (Original_Node (U)) =
                                                  N_Procedure_Instantiation))
         then
            --  If the unit is a subprogram instance, the entity for the
            --  subprogram is the alias of the visible entity, which is the
            --  related instance of the wrapper package. We retrieve the
            --  subprogram declaration of the desired entity.

            if Nkind (U) = N_Package_Body then
               U := Parent (Parent (
                   Alias (Related_Instance (Defining_Unit_Name
                     (Specification (Unit (Library_Unit (Parent (U)))))))));
            end if;

            S := Specification (U);

            if not Present (Parameter_Specifications (S)) then
               if Nkind (S) = N_Procedure_Specification then
                  Write_Info_Initiate ('M');
                  Write_Info_Str (" P");
                  M_Parameters;

               else
                  declare
                     Nam : Node_Id := Defining_Unit_Name (S);

                  begin
                     --  If it is a child unit, get its simple name.

                     if Nkind (Nam) = N_Defining_Program_Unit_Name then
                        Nam := Defining_Identifier (Nam);
                     end if;

                     if Is_Integer_Type (Etype (Nam)) then
                        Write_Info_Initiate ('M');
                        Write_Info_Str (" F");
                        M_Parameters;
                     end if;
                  end;
               end if;
            end if;
         end if;
      end;

      --  Write command argmument ('A') lines

      for A in 1 .. Compilation_Arguments.Last loop
         Write_Info_Initiate ('A');
         Write_Info_Char (' ');
         Write_Info_Str (Compilation_Arguments.Table (A).all);
         Write_Info_Terminate;
      end loop;

      --  Output parameters ('P') line

      Write_Info_Initiate ('P');

      if Compilation_Errors then
         Write_Info_Str (" CE");
      end if;

      if Opt.Float_Format /= ' ' then
         Write_Info_Str (" F");

         if Opt.Float_Format = 'I' then
            Write_Info_Char ('I');

         elsif Opt.Float_Format_Long = 'D' then
            Write_Info_Char ('D');

         else
            Write_Info_Char ('G');
         end if;
      end if;

      if Tasking_Used
        and then not Is_Predefined_File_Name (Unit_File_Name (Main_Unit))
      then
         if Locking_Policy /= ' ' then
            Write_Info_Str  (" L");
            Write_Info_Char (Locking_Policy);
         end if;

         if Queuing_Policy /= ' ' then
            Write_Info_Str  (" Q");
            Write_Info_Char (Queuing_Policy);
         end if;

         if Task_Dispatching_Policy /= ' ' then
            Write_Info_Str  (" T");
            Write_Info_Char (Task_Dispatching_Policy);
            Write_Info_Char (' ');
         end if;
      end if;

      if not Object then
         Write_Info_Str (" NO");
      end if;

      if No_Run_Time then
         Write_Info_Str (" NR");
      end if;

      if Normalize_Scalars then
         Write_Info_Str (" NS");
      end if;

      if Unreserve_All_Interrupts then
         Write_Info_Str (" UA");
      end if;

      if Zero_Cost_Exceptions_On_Target then
         if Unit_Exception_Table_Present then
            Write_Info_Str (" UX");
         end if;

         Write_Info_Str (" ZX");
      end if;

      Write_Info_EOL;

      --  Output restrictions line

      Write_Info_Initiate ('R');
      Write_Info_Char (' ');

      for J in Partition_Restrictions loop
         if Main_Restrictions (J) then
            Write_Info_Char ('r');
         elsif Violations (J) then
            Write_Info_Char ('v');
         else
            Write_Info_Char ('n');
         end if;
      end loop;

      Write_Info_EOL;

      --  Loop through file table to output information for all units for which
      --  we have generated code, as marked by the Generate_Code flag.

      for Unit in Units.First .. Units.Last loop
         if Units.Table (Unit).Generate_Code
           or else Unit = Main_Unit
         then
            Write_Info_EOL; -- blank line
            Write_Unit_Information (Unit);
         end if;
      end loop;

      Write_Info_EOL; -- blank line

      --  Output linker option lines

      for J in 1 .. Linker_Option_Lines.Last loop
         declare
            S : constant String_Id := Linker_Option_Lines.Table (J);
            C : Character;

         begin
            Write_Info_Initiate ('L');
            Write_Info_Str (" """);

            for J in 1 .. String_Length (S) loop
               C := Get_Character (Get_String_Char (S, J));

               if C in Character'Val (16#20#) .. Character'Val (16#7E#)
                 and then C /= '{'
               then
                  Write_Info_Char (C);

                  if C = '"' then
                     Write_Info_Char (C);
                  end if;

               else
                  declare
                     Hex : array (0 .. 15) of Character := "0123456789ABCDEF";

                  begin
                     Write_Info_Char ('{');
                     Write_Info_Char (Hex (Character'Pos (C) / 16));
                     Write_Info_Char (Hex (Character'Pos (C) mod 16));
                     Write_Info_Char ('}');
                  end;
               end if;
            end loop;

            Write_Info_Char ('"');
            Write_Info_EOL;
         end;
      end loop;

      --  Output external version reference lines

      for J in 1 .. Version_Ref.Last loop
         Write_Info_Initiate ('E');
         Write_Info_Char (' ');

         for K in 1 .. String_Length (Version_Ref.Table (J)) loop
            Write_Info_Char_Code (Get_String_Char (Version_Ref.Table (J), K));
         end loop;

         Write_Info_EOL;
      end loop;

      --  Prepare to output the source dependency lines

      declare
         Unum : Unit_Number_Type;
         --  Number of unit being output

         Sind : Source_File_Index;
         --  Index of corresponding source file

      begin
         for J in 1 .. Num_Sdep loop
            Unum := Sdep_Table (J);
            Sind := Units.Table (Unum).Source_Index;
            Units.Table (Unum).Dependency_Num := J;

            if Units.Table (Unum).Dependent_Unit then
               Write_Info_Initiate ('D');
               Write_Info_Char (' ');
               Write_Info_Name (File_Name (Sind));
               Write_Info_Tab (25);
               Write_Info_Str (String (Time_Stamp (Sind)));
               Write_Info_Char (' ');
               Write_Info_Str (Get_Hex_String (Source_Checksum (Sind)));

               --  If subunit, add unit name, omitting the %b at the end

               if Nkind (Unit (Cunit (Unum))) = N_Subunit then
                  Get_Decoded_Name_String (Unit_Name (Unum));
                  Write_Info_Char (' ');
                  Write_Info_Str (Name_Buffer (1 .. Name_Len - 2));
               end if;

               Write_Info_EOL;
            end if;
         end loop;
      end;

      Output_References;
      Write_Info_Terminate;
      Close_Output_Library_Info;

   end Write_ALI;

end Lib.Writ;
