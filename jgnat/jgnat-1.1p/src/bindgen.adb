------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              B I N D G E N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.161 $
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

with ALI;         use ALI;
with Binde;       use Binde;
with Casing;      use Casing;
with Fname;       use Fname;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gnatvsn;     use Gnatvsn;
with Hostparm;
with Namet;       use Namet;
with Opt;         use Opt;
with Osint;       use Osint;
with Output;      use Output;
with Types;       use Types;
with Sdefault;    use Sdefault;

with GNAT.Heap_Sort_A;     use GNAT.Heap_Sort_A;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Bindgen is

   Statement_Buffer : String (1 .. 1000);
   --  Buffer used for constructing output statements

   Last : Natural := 0;
   --  Last location in Statement_Buffer currently set

   With_Finalization : Boolean := False;
   --  Flag which indicates whether the program uses finalization
   --  (presence of the unit System.Finalization_Implementation)

   With_Tasking : Boolean := False;
   --  Flag which indicates whether the program uses tasking
   --  (presence of the unit System.Tasking.Stages)

   With_GNARL : Boolean := False;
   --  Flag which indicates whether the program uses the GNARL library
   --  (presence of the unit System.OS_Interface)

   Num_Elab_Calls : Nat := 0;
   --  Number of generated calls to elaboration routines

   Run_Path_Option_Ptr : chars_ptr;
   pragma Import (C, Run_Path_Option_Ptr, "run_path_option");
   --  Pointer to a string representing the native linker option
   --  which specifies the path where the dynamic loader should find shared
   --  libraries. Returns a null string if this system doesn't support it.

   Object_Library_Extension_Ptr : chars_ptr;
   pragma Import (C, Object_Library_Extension_Ptr,
                  "object_library_extension");
   --  Pointer to a string specifying the default extension for
   --  object libraries, e.g. Unix uses ".a", VMS uses ".olb".

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure WBI (Info : String) renames Osint.Write_Binder_Info;
   --  Convenient shorthand used throughout

   function ABE_Boolean_Required (U : Unit_Id) return Boolean;
   --  Given a unit id value U, determines if the corresponding unit requires
   --  an access-before-elaboration check variable, i.e. it is a non-predefined
   --  body for which no pragma Elaborate, Elaborate_All or Elaborate_Body is
   --  present, and thus could require ABE checks.

   procedure Resolve_Binder_Options;
   --  Set the value of With_Tasking and With_Finalization

   procedure Gen_Adainit_Ada;
   --  Generates the Adainit procedure (Ada code case)

   procedure Gen_Adainit_C;
   --  Generates the Adainit procedure (C code case)

   procedure Gen_Adafinal_Ada;
   --  Generate the Adafinal procedure (Ada code case)

   procedure Gen_Adafinal_C;
   --  Generate the Adafinal procedure (C code case)

   procedure Gen_Elab_Calls_Ada;
   --  Generate sequence of elaboration calls (Ada code case)

   procedure Gen_Elab_Calls_C;
   --  Generate sequence of elaboration calls (C code case)

   procedure Gen_Elab_Defs_C;
   --  Generate sequence of definitions for elaboration routines (C code case)

   procedure Gen_Exception_Table_Ada;
   --  Generate binder exception table (Ada code case). This consists of
   --  declarations followed by a begin followed by a call. If zero cost
   --  exceptions are not active, then only the begin is generated.

   procedure Gen_Exception_Table_C;
   --  Generate binder exception table (C code case). This has no effect
   --  if zero cost exceptions are not active, otherwise it generates a
   --  set of declarations followed by a call.

   procedure Gen_Main_Ada;
   --  Generate procedure main (Ada code case)

   procedure Gen_Main_C;
   --  Generate main() procedure (C code case)

   procedure Gen_Object_Files_Options;
   --  Output comments containing a list of the full names of the object
   --  files to be linked and the list of linker options supplied by
   --  Linker_Options pragmas in the source. (C and Ada code case)

   procedure Gen_Output_File_Ada (Filename : String);
   --  Generate output file (Ada code case)

   procedure Gen_Output_File_C (Filename : String);
   --  Generate output file (C code case)

   procedure Gen_Versions_Ada;
   --  Output series of definitions for unit versions (Ada code case)

   procedure Gen_Versions_C;
   --  Output series of definitions for unit versions (C code case)

   function Get_Ada_Main_Name return String;
   --  This function is used in the Ada main output case to compute a usable
   --  name for the generated main program. The normal main program name is
   --  Ada_Main, but this won't work if the user has a unit with this name.
   --  This function tries Ada_Main first, and if there is such a clash, then
   --  it tries Ada_Name_01, Ada_Name_02 ... Ada_Name_99 in sequence.

   function Get_Main_Name return String;
   --  This function is used in the Ada main output case to compute the
   --  correct external main program. It is "main" by default, except on
   --  VxWorks where it is the name of the Ada main name without the "_ada".
   --  the -Mname binder option overrides the default with name.

   function Lt_Linker_Option (Op1, Op2 : Natural) return Boolean;
   --  Compare linker options, when sorting, first according to
   --  Is_Internal_File (internal files come later) and then by elaboration
   --  order position (latest to earliest) except its not possible to
   --  distinguish between a linker option in the spec and one in the body.

   procedure Move_Linker_Option (From : Natural; To : Natural);
   --  Move routine for sorting linker options

   procedure Public_Version_Warning;
   --  Emit a warning concerning the use of the Public version under
   --  certain circumstances. See details in body.

   procedure Set_Char (C : Character);
   --  Set given character in Statement_Buffer at the Last + 1 position
   --  and increment Last by one to reflect the stored character.

   procedure Set_Int (N : Int);
   --  Set given value in decimal in Statement_Buffer with no spaces
   --  starting at the Last + 1 position, and updating Last past the value.
   --  A minus sign is output for a negative value.

   procedure Set_Main_Program_Name;
   --  Given the main program name in Name_Buffer (length in Name_Len)
   --  generate the name of the routine to be used in the call. The name
   --  is generated starting at Last + 1, and Last is updated past it.

   procedure Set_String (S : String);
   --  Sets characters of given string in Statement_Buffer, starting at the
   --  Last + 1 position, and updating last past the string value.

   procedure Set_Unit_Name;
   --  Given a unit name in the Name_Buffer, copies it to Statement_Buffer,
   --  starting at the Last + 1 position, and updating last past the value.
   --  changing periods to double underscores, and updating Last appropriately.

   procedure Tab_To (N : Natural);
   --  If Last is greater than or equal to N, no effect, otherwise store
   --  blanks in Statement_Buffer bumping Last, until Last = N.

   procedure Write_Info_Ada_C (Ada : String; C : String; Common : String);
   --  For C code case, write C & Common, for Ada case write Ada & Common
   --  to current binder output file using Write_Binder_Info.

   procedure Write_Statement_Buffer;
   --  Write out contents of statement buffer up to Last, and reset Last to 0

   --------------------------
   -- ABE_Boolean_Required --
   --------------------------

   procedure Resolve_Binder_Options is
   begin
      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);

         --  if the program uses finalization we must make sure to finalize
         --  global objects too at the end of the program.

         if Name_Buffer (1 .. 34) = "system.finalization_implementation" then
            With_Finalization := True;
         elsif Name_Buffer (1 .. 21) = "system.tasking.stages" then
            With_Tasking := True;
         elsif Name_Buffer (1 .. 19) = "system.os_interface" then
            With_GNARL := True;
         end if;
      end loop;
   end Resolve_Binder_Options;

   --------------------------
   -- ABE_Boolean_Required --
   --------------------------

   function ABE_Boolean_Required (U : Unit_Id) return Boolean is
      Typ   : constant Unit_Type := Units.Table (U).Utype;
      Unit : Unit_Id;

   begin
      if Typ /= Is_Body then
         return False;

      else
         Unit := U + 1;

         return (not Units.Table (Unit).Pure)
                   and then
                (not Units.Table (Unit).Preelab)
                   and then
                (not Units.Table (Unit).Elaborate_Body)
                   and then
                (not Units.Table (Unit).Predefined);
      end if;
   end ABE_Boolean_Required;

   ---------------------
   -- Gen_Adainit_Ada --
   ---------------------

   procedure Gen_Adainit_Ada is
   begin
      --  The flag Ada_Init_Flag is used to ensure that only the first
      --  call to adainit has an effect (RM B.1(39)). It is false (zero)
      --  before the first call, and true thereafter.

      if not Bind_Main_Program then
         WBI ("   Ada_Init_Flag : Boolean := False;");
      end if;

      WBI ("   procedure adainit is");

      --  Normal case (no pragma No_Run_Time). The global values are
      --  assigned using the runtime routine Set_Globals (we have to use
      --  the routine call, rather than define the globals in the binder
      --  file to deal with cross-library calls in some systems.

      if not No_Run_Time_Specified then
         WBI ("");
         WBI ("      procedure Set_Globals");
         WBI ("        (Main_Priority            : Integer;");
         WBI ("         Time_Slice_Value         : Integer;");
         WBI ("         WC_Encoding              : Character;");
         WBI ("         Locking_Policy           : Character;");
         WBI ("         Queuing_Policy           : Character;");
         WBI ("         Task_Dispatching_Policy  : Character;");
         WBI ("         Adafinal                 : System.Address;");
         WBI ("         Unreserve_All_Interrupts : Boolean;");
         WBI ("         Exception_Tracebacks     : Boolean);");
         WBI ("      pragma Import (C, Set_Globals, ""__gnat_set_globals"");");
         WBI ("");

         Gen_Exception_Table_Ada;

         if not Bind_Main_Program then
            WBI ("      if Ada_Init_Flag then");
            WBI ("         return;");
            WBI ("      else");
            WBI ("         Ada_Init_Flag := True;");
            WBI ("      end if;");
         end if;

         WBI ("      Set_Globals");

         Set_String ("        (Main_Priority            => ");
         Set_Int    (ALIs.Table (ALIs.First).Main_Priority);
         Set_Char   (',');
         Write_Statement_Buffer;

         Set_String ("         Time_Slice_Value         => ");

         if Task_Dispatching_Policy_Specified = 'F'
           and then ALIs.Table (ALIs.First).Time_Slice_Value = -1
         then
            Set_Int (0);
         else
            Set_Int (ALIs.Table (ALIs.First).Time_Slice_Value);
         end if;

         Set_Char   (',');
         Write_Statement_Buffer;

         Set_String ("         WC_Encoding              => '");
         Set_Char   (ALIs.Table (ALIs.First).WC_Encoding);
         Set_String ("',");
         Write_Statement_Buffer;

         Set_String ("         Locking_Policy           => '");
         Set_Char   (Locking_Policy_Specified);
         Set_String ("',");
         Write_Statement_Buffer;

         Set_String ("         Queuing_Policy           => '");
         Set_Char   (Queuing_Policy_Specified);
         Set_String ("',");
         Write_Statement_Buffer;

         Set_String ("         Task_Dispatching_Policy  => '");
         Set_Char   (Task_Dispatching_Policy_Specified);
         Set_String ("',");
         Write_Statement_Buffer;

         WBI ("         Adafinal                 => adafinal'Address,");

         Set_String ("         Unreserve_All_Interrupts => ");

         if Unreserve_All_Interrupts_Specified then
            Set_String ("True");
         else
            Set_String ("False");
         end if;

         Set_String (",");
         Write_Statement_Buffer;

         Set_String ("         Exception_Tracebacks     => ");

         if Exception_Tracebacks then
            Set_String ("True");
         else
            Set_String ("False");
         end if;

         Set_String (");");
         Write_Statement_Buffer;

      --  Case of pragma No_Run_Time present. Globals are not needed since
      --  there are no runtime routines to make use of them, and no routine
      --  to store them in any case! Also no exception tables are needed.

      else
         WBI ("   begin");

         if not Bind_Main_Program then
            WBI ("      if Ada_Init_Flag then");
            WBI ("         return;");
            WBI ("      else");
            WBI ("         Ada_Init_Flag := True;");
            WBI ("      end if;");
         end if;
      end if;

      Gen_Elab_Calls_Ada;

      WBI ("      null;");
      WBI ("   end adainit;");
   end Gen_Adainit_Ada;

   -------------------
   -- Gen_Adainit_C --
   --------------------

   procedure Gen_Adainit_C is
   begin
      WBI ("void adainit ()");
      WBI ("{");

      --  Code for normal case (no pragma No_Run_Time in use)

      if not No_Run_Time_Specified then

         --  The flag ada__init_flag is used to ensure that only the first
         --  call to adainit has an effect (RM B.1(39)). It is false (zero)
         --  before the first call, and true thereafter.

         if not Bind_Main_Program then
            WBI ("   static int ada__init_flag = 0;");
         end if;

         Gen_Exception_Table_C;

         if not Bind_Main_Program then
            WBI ("   if (ada__init_flag) return;");
            WBI ("   ada__init_flag++;");
         end if;

         --  Generate call to set the runtime global variables defined in
         --  a-init.c. We define the varables in a-init.c, rather than in
         --  the binder generated file itself to avoid undefined externals
         --  when the runtime is linked as a shareable image library.

         --  We call the routine from inside adainit() because this works for
         --  both programs with and those without binder generated "main"
         --  functions. This means, of course, that adafinal() must be defined
         --  before adainit(), so we can pass its address.

         WBI ("   __gnat_set_globals (");

         Set_String ("      ");
         Set_Int (ALIs.Table (ALIs.First).Main_Priority);
         Set_Char (',');
         Tab_To (15);
         Set_String ("/* Main_Priority              */");
         Write_Statement_Buffer;

         Set_String ("      ");

         if Task_Dispatching_Policy = 'F'
           and then ALIs.Table (ALIs.First).Time_Slice_Value = -1
         then
            Set_Int (0);
         else
            Set_Int (ALIs.Table (ALIs.First).Time_Slice_Value);
         end if;

         Set_Char   (',');
         Tab_To (15);
         Set_String ("/* Time_Slice_Value           */");
         Write_Statement_Buffer;

         Set_String ("      '");
         Set_Char   (ALIs.Table (ALIs.First).WC_Encoding);
         Set_String ("',");
         Tab_To (15);
         Set_String ("/* WC_Encoding                */");
         Write_Statement_Buffer;

         Set_String ("      '");
         Set_Char (Locking_Policy_Specified);
         Set_String ("',");
         Tab_To (15);
         Set_String ("/* Locking_Policy             */");
         Write_Statement_Buffer;

         Set_String ("      '");
         Set_Char (Queuing_Policy_Specified);
         Set_String ("',");
         Tab_To (15);
         Set_String ("/* Queuing_Policy             */");
         Write_Statement_Buffer;

         Set_String ("      '");
         Set_Char (Task_Dispatching_Policy_Specified);
         Set_String ("',");
         Tab_To (15);
         Set_String ("/* Tasking_Dispatching_Policy */");
         Write_Statement_Buffer;

         Set_String ("      ");
         Set_String ("adafinal,");
         Tab_To (15);
         Set_String ("/* Finalization routine address */");
         Write_Statement_Buffer;

         Set_String ("      ");
         Set_Int    (Boolean'Pos (Unreserve_All_Interrupts_Specified));
         Set_String (",");
         Tab_To (15);
         Set_String ("/* Unreserve_All_Interrupts */");
         Write_Statement_Buffer;

         Set_String ("      ");
         Set_Int    (Boolean'Pos (Exception_Tracebacks));
         Set_String (");");
         Tab_To (15);
         Set_String ("/* Exception_Tracebacks */");
         Write_Statement_Buffer;

      --  Case where No_Run_Time pragma is present (no globals required)

      else
         --  The flag ada__init_flag is used to ensure that only the first
         --  call to adainit has an effect (RM B.1(39)). It is false (zero)
         --  before the first call, and true thereafter.

         if not Bind_Main_Program then
            WBI ("   static int ada__init_flag = 0;");
            WBI ("   if (ada__init_flag) return;");
            WBI ("   ada__init_flag++;");
         end if;
      end if;

      WBI ("");
      Gen_Elab_Calls_C;
      WBI ("}");
   end Gen_Adainit_C;

   ----------------------
   -- Gen_Adafinal_Ada --
   ----------------------

   procedure Gen_Adafinal_Ada is
   begin
      WBI ("");
      WBI ("   procedure adafinal is");

      if With_Tasking or With_Finalization then

         --  Cannot use pragma Import C in the Java case, so use standard
         --  Ada constructs instead

         if not Hostparm.Java_VM then
            WBI ("");
            WBI ("      procedure do_finalize;");
            WBI ("      pragma Import");
            WBI ("        (C, do_finalize,");

            if With_Tasking then
               WBI
                 ("         " &
                  """system__tasking__stages__finalize_global_tasks"");");

            elsif With_Finalization then
               WBI
                 ("         " &
                  """system__finalization_implementation" &
                  "__finalize_global_list"");");
            end if;

            WBI ("");
         end if;

         WBI ("   begin");
         WBI ("      Elab_Final_Code := 1;");

         if not Hostparm.Java_VM then
            WBI ("      do_finalize;");
         else
            if With_Tasking then
               WBI ("      System.Tasking.Stages.Finalize_Global_Tasks;");
            else
               WBI ("      System.Finalization_Implementation." &
                    "Finalize_Global_List;");
            end if;
         end if;

      else
         WBI ("   begin");
         WBI ("      null;");
      end if;

      WBI ("      Elab_Final_Code := 0;");
      WBI ("   end adafinal;");
   end Gen_Adafinal_Ada;

   --------------------
   -- Gen_Adafinal_C --
   --------------------

   procedure Gen_Adafinal_C is
   begin
      WBI ("void adafinal () {");

      WBI ("   __gnat_elab_final_code = 1;");

      if With_Tasking then
         WBI ("   " &
              "system__tasking__stages__finalize_global_tasks ();");

      elsif With_Finalization then
         WBI ("   " &
              "system__finalization_implementation__finalize_global_list ();");
      end if;

      WBI ("   __gnat_elab_final_code = 0;");

      WBI ("}");
      WBI ("");
   end Gen_Adafinal_C;

   ------------------------
   -- Gen_Elab_Calls_Ada --
   ------------------------

   procedure Gen_Elab_Calls_Ada is
   begin
      WBI ("      Elab_Final_Code := 1;");

      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Decoded_Name_String_With_Brackets
           (Units.Table (Elab_Order.Table (E)).Uname);

         --  Generate elaboration call if elaboration needed, and a comment
         --  if no elaboration is required. Note that passive units are
         --  always excluded from elaboration.

         if Units.Table (Elab_Order.Table (E)).No_Elab then
            Set_String ("      -- ");
         else
            Set_String ("         ");
         end if;

         if Name_Buffer (Name_Len) = 's' then
            Name_Buffer (Name_Len - 1 .. Name_Len + 8) := "'elab_spec";
         else
            Name_Buffer (Name_Len - 1 .. Name_Len + 8) := "'elab_body";
         end if;

         Name_Len := Name_Len + 8;
         Set_Casing (Units.Table (Elab_Order.Table (E)).Icasing);

         for J in 1 .. Name_Len loop
            Set_Char (Name_Buffer (J));
         end loop;

         Set_Char (';');
         Write_Statement_Buffer;
      end loop;

      WBI ("      Elab_Final_Code := 0;");
   end Gen_Elab_Calls_Ada;

   ----------------------
   -- Gen_Elab_Calls_C --
   ----------------------

   procedure Gen_Elab_Calls_C is
   begin
      WBI ("   __gnat_elab_final_code = 1;");

      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);

         --  Generate elaboration call if elaboration needed, and a comment
         --  if no elaboration is required. Note that passive units are
         --  always excluded from elaboration.

         if Units.Table (Elab_Order.Table (E)).No_Elab then
            Set_String ("/* ");
         else
            Set_String ("   ");
         end if;

         Set_Unit_Name;
         Set_String ("___elab");
         Set_Char (Name_Buffer (Name_Len)); -- 's' or 'b' for spec/body
         Set_String (" ();");

         if Units.Table (Elab_Order.Table (E)).No_Elab then
            Set_String (" */");
         end if;

         Write_Statement_Buffer;
      end loop;

      WBI ("   __gnat_elab_final_code = 0;");
   end Gen_Elab_Calls_C;

   ----------------------
   -- Gen_Elab_Defs_C --
   ----------------------

   procedure Gen_Elab_Defs_C is
   begin
      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);

         --  Generate declaration of elaboration procedure if elaboration
         --  needed.   Note that passive units are always excluded.

         if not Units.Table (Elab_Order.Table (E)).No_Elab then
            Set_String ("extern void ");
            Set_Unit_Name;
            Set_String ("___elab");
            Set_Char (Name_Buffer (Name_Len)); -- 's' or 'b' for spec/body
            Set_String (" PARAMS ((void));");
            Write_Statement_Buffer;
         end if;

      end loop;

      WBI ("");
   end Gen_Elab_Defs_C;

   -----------------------------
   -- Gen_Exception_Table_Ada --
   -----------------------------

   procedure Gen_Exception_Table_Ada is
      Num  : Nat;
      Last : ALI_Id;

   begin
      if not Zero_Cost_Exceptions_Specified then
         WBI ("   begin");
         return;
      end if;

      --  The code we generate looks like

      --        procedure SDP_Table_Build
      --          (SDP_Addresses   : System.Address;
      --           SDP_Count       : Natural;
      --           Elab_Addresses  : System.Address;
      --           Elab_Addr_Count : Natural);
      --        pragma Import (C, SDP_Table_Build, "__gnat_SDP_Table_Build");
      --
      --        ST : aliased constant array (1 .. nnn) of System.Address := (
      --               unit_name_1'UET_Address,
      --               unit_name_2'UET_Address,
      --               ...
      --               unit_name_3'UET_Address,
      --
      --        EA : aliased constant array (1 .. eee) of System.Address := (
      --               adainit'Code_Address,
      --               adafinal'Code_Address,
      --               unit_name'elab[spec|body]'Address,
      --               unit_name'elab[spec|body]'Address,
      --               unit_name'elab[spec|body]'Address,
      --               unit_name'elab[spec|body]'Address);
      --
      --     begin
      --        SDP_Table_Build (ST'Address, nnn, EA'Address, eee);

      Num := 0;
      for A in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A).Unit_Exception_Table then
            Num := Num + 1;
            Last := A;
         end if;
      end loop;

      WBI ("      procedure SDP_Table_Build");
      WBI ("        (SDP_Addresses   : System.Address;");
      WBI ("         SDP_Count       : Natural;");
      WBI ("         Elab_Addresses  : System.Address;");
      WBI ("         Elab_Addr_Count : Natural);");
      WBI ("      " &
           "pragma Import (C, SDP_Table_Build, ""__gnat_SDP_Table_Build"");");

      WBI (" ");
      Set_String ("      ST : aliased constant array (1 .. ");
      Set_Int (Num);
      Set_String (") of System.Address := (");

      if Num = 1 then
         Set_String ("1 => A1);");
         Write_Statement_Buffer;

      else
         Write_Statement_Buffer;

         for A in ALIs.First .. ALIs.Last loop
            if ALIs.Table (A).Unit_Exception_Table then
               Get_Decoded_Name_String_With_Brackets
                 (Units.Table (ALIs.Table (A).First_Unit).Uname);
               Set_Casing (Mixed_Case);
               Set_String ("        ");
               Set_String (Name_Buffer (1 .. Name_Len - 2));
               Set_String ("'UET_Address");

               if A = Last then
                  Set_String (");");
               else
                  Set_Char (',');
               end if;

               Write_Statement_Buffer;
            end if;
         end loop;
      end if;

      WBI (" ");
      Set_String ("      EA : aliased constant array (1 .. ");
      Set_Int (Num_Elab_Calls + 2);
      Set_String (") of System.Address := (");
      Write_Statement_Buffer;
      WBI ("        adainit'Code_Address,");

      Set_String ("        adafinal'Code_Address");

      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Decoded_Name_String_With_Brackets
           (Units.Table (Elab_Order.Table (E)).Uname);

         if Units.Table (Elab_Order.Table (E)).No_Elab then
            null;

         else
            Set_Char (',');
            Write_Statement_Buffer;
            Set_String ("        ");

            if Name_Buffer (Name_Len) = 's' then
               Name_Buffer (Name_Len - 1 .. Name_Len + 16) :=
                                        "'elab_spec'address";
            else
               Name_Buffer (Name_Len - 1 .. Name_Len + 16) :=
                                        "'elab_body'address";
            end if;

            Name_Len := Name_Len + 16;
            Set_Casing (Units.Table (Elab_Order.Table (E)).Icasing);

            for J in 1 .. Name_Len loop
               Set_Char (Name_Buffer (J));
            end loop;
         end if;
      end loop;

      Set_String (");");
      Write_Statement_Buffer;

      WBI (" ");
      WBI ("   begin");

      Set_String ("      SDP_Table_Build (ST'Address, ");
      Set_Int (Num);
      Set_String (", EA'Address, ");
      Set_Int (Num_Elab_Calls + 2);
      Set_String (");");
      Write_Statement_Buffer;
   end Gen_Exception_Table_Ada;

   ---------------------------
   -- Gen_Exception_Table_C --
   ---------------------------

   procedure Gen_Exception_Table_C is
      Num  : Nat;
      Num2 : Nat;

   begin
      if not Zero_Cost_Exceptions_Specified then
         return;
      end if;

      --  The code we generate looks like

      --     extern void *__gnat_unitname1__SDP;
      --     extern void *__gnat_unitname2__SDP;
      --     ...
      --
      --     void **st[nnn] = {
      --       &__gnat_unitname1__SDP,
      --       &__gnat_unitname2__SDP,
      --       ...
      --       &__gnat_unitnamen__SDP};
      --
      --     extern void unitname1__elabb ();
      --     extern void unitname2__elabb ();
      --     ...
      --
      --     void (*ea[eee]) () = {
      --       adainit,
      --       adafinal,
      --       unitname1___elab[b,s],
      --       unitname2___elab[b,s],
      --       ...
      --       unitnamen___elab[b,s]};
      --
      --     __gnat_SDP_Table_Build (&st, nnn, &ea, eee);


      Num := 0;
      for A in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A).Unit_Exception_Table then
            Num := Num + 1;

            Set_String ("   extern void *__gnat_");
            Get_Name_String (Units.Table (ALIs.Table (A).First_Unit).Uname);
            Set_Unit_Name;
            Set_String ("__SDP");
            Set_Char (';');
            Write_Statement_Buffer;
         end if;
      end loop;

      WBI (" ");

      Set_String ("   void **st[");
      Set_Int (Num);
      Set_String ("] = {");
      Write_Statement_Buffer;

      Num2 := 0;
      for A in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A).Unit_Exception_Table then
            Num2 := Num2 + 1;

            Set_String ("     &__gnat_");
            Get_Name_String (Units.Table (ALIs.Table (A).First_Unit).Uname);
            Set_Unit_Name;
            Set_String ("__SDP");

            if Num = Num2 then
               Set_String ("};");
            else
               Set_Char (',');
            end if;

            Write_Statement_Buffer;
         end if;
      end loop;

      WBI ("");
      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);

         if Units.Table (Elab_Order.Table (E)).No_Elab then
            null;

         else
            Set_String ("   extern void ");
            Set_Unit_Name;
            Set_String ("___elab");
            Set_Char (Name_Buffer (Name_Len)); -- 's' or 'b' for spec/body
            Set_String (" ();");
            Write_Statement_Buffer;
         end if;
      end loop;

      WBI ("");
      Set_String ("   void (*ea[");
      Set_Int (Num_Elab_Calls + 2);
      Set_String ("]) () = {");
      Write_Statement_Buffer;

      WBI ("     adainit,");
      Set_String ("     adafinal");

      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);

         if Units.Table (Elab_Order.Table (E)).No_Elab then
            null;

         else
            Set_Char (',');
            Write_Statement_Buffer;
            Set_String ("     ");
            Set_Unit_Name;
            Set_String ("___elab");
            Set_Char (Name_Buffer (Name_Len)); -- 's' or 'b' for spec/body
         end if;
      end loop;

      Set_String ("};");
      Write_Statement_Buffer;

      WBI (" ");

      Set_String ("   __gnat_SDP_Table_Build (&st, ");
      Set_Int (Num);
      Set_String (", ea, ");
      Set_Int (Num_Elab_Calls + 2);
      Set_String (");");
      Write_Statement_Buffer;
   end Gen_Exception_Table_C;

   ------------------
   -- Gen_Main_Ada --
   ------------------

   procedure Gen_Main_Ada is
      Target         : constant String_Ptr := Target_Name;
      VxWorks_Target : Boolean;

   begin
      VxWorks_Target := Target (Target'Last - 7 .. Target'Last) = "vxworks/";
      WBI ("");
      Set_String ("   function ");
      Set_String (Get_Main_Name);

      if VxWorks_Target then
         Set_String (" return Integer is");
         Write_Statement_Buffer;

      else
         Write_Statement_Buffer;
         WBI ("     (argc : Integer;");
         WBI ("      argv : System.Address;");
         WBI ("      envp : System.Address)");
         WBI ("      return Integer");
         WBI ("   is");
      end if;

      --  Initialize and Finalize are not used in No_Run_Time mode

      if not No_Run_Time_Specified then
         WBI ("      procedure initialize;");
         WBI ("      pragma Import (C, initialize, ""__gnat_initialize"");");
         WBI ("");
         WBI ("      procedure finalize;");
         WBI ("      pragma Import (C, finalize, ""__gnat_finalize"");");
         WBI ("");
      end if;

      --  Deal with declarations for main program case

      if not No_Main_Subprogram then

         --  To call the main program, we declare it using a pragma Import
         --  Ada with the right link name.

         --  It might seem more obvious to "with" the main program, and call
         --  it in the normal Ada manner. We do not do this for three reasons:

         --    1. It is more efficient not to recompile the main program
         --    2. We are not entitled to assume the source is accessible
         --    3. We don't know what options to use to compile it

         --  It is really reason 3 that is most critical (indeed we used
         --  to generate the "with", but several regression tests failed).

         WBI ("");

         if ALIs.Table (ALIs.First).Main_Program = Func then
            WBI ("      Result : Integer;");
            WBI ("");
            WBI ("      function Ada_Main_Program return Integer;");

         else
            WBI ("      procedure Ada_Main_Program;");
         end if;

         Set_String ("      pragma Import (Ada, Ada_Main_Program, """);
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (""");");

         Write_Statement_Buffer;
         WBI ("");
      end if;

      WBI ("   begin");

      if VxWorks_Target then
         WBI ("      gnat_argc := 0;");
         WBI ("      gnat_argv := System.Null_Address;");
         WBI ("      gnat_envp := System.Null_Address;");

      else
         WBI ("      gnat_argc := argc;");
         WBI ("      gnat_argv := argv;");
         WBI ("      gnat_envp := envp;");
         WBI ("");
      end if;

      if not No_Run_Time_Specified then
         WBI ("      Initialize;");
      end if;

      WBI ("      adainit;");

      if not No_Main_Subprogram then
         WBI ("      Break_Start;");

         if ALIs.Table (ALIs.First).Main_Program = Proc then
            WBI ("      Ada_Main_Program;");
         else
            WBI ("      Result := Ada_Main_Program;");
         end if;
      end if;

      WBI ("      adafinal;");

      --  Finalize is only called if we have a run time

      if not No_Run_Time_Specified then
         WBI ("      Finalize;");
      end if;

      --  Return result

      if No_Main_Subprogram
        or else ALIs.Table (ALIs.First).Main_Program = Proc
      then
         WBI ("      return (gnat_exit_status);");
      else
         WBI ("      return (Result);");
      end if;

      WBI ("   end;");
   end Gen_Main_Ada;

   ----------------
   -- Gen_Main_C --
   ----------------

   procedure Gen_Main_C is

      Target         : constant String_Ptr := Target_Name;
      VxWorks_Target : Boolean;


   --  Start of processing for Gen_Main_C

   begin
      VxWorks_Target := Target (Target'Last - 7 .. Target'Last) = "vxworks/";

      if Bind_Alternate_Main_Name then
         Set_String ("int ");
         Set_String (Alternate_Main_Name.all);

         if VxWorks_Target then
            Set_String (" ()");
         else
            Set_String (" (argc, argv, envp)");
         end if;

         Write_Statement_Buffer;

      elsif VxWorks_Target then

         --  Get main program name

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         --  Remove the %b

         Set_String ("int ");
         Set_String (Name_Buffer (1 .. Name_Len - 2));
         Set_String (" ()");
         Write_Statement_Buffer;

      else
         WBI ("int main (argc, argv, envp)");
      end if;

      if VxWorks_Target then

         --  VxWorks doesn't have the notion of argc/argv

         WBI ("{");
         WBI ("   gnat_argc = 0;");
         WBI ("   gnat_argv = 0;");
         WBI ("   gnat_envp = 0;");

      else
         WBI ("    int argc;");
         WBI ("    char **argv;");
         WBI ("    char **envp;");
         WBI ("{");
         WBI ("   gnat_argc = argc;");
         WBI ("   gnat_argv = argv;");
         WBI ("   gnat_envp = envp;");
         WBI (" ");
      end if;

      --  The __gnat_initialize routine is used only if we have a run-time

      if not No_Run_Time_Specified then
         WBI
          ("   __gnat_initialize ();");
      end if;

      WBI ("   adainit ();");

      if not No_Main_Subprogram then

         WBI ("   __gnat_break_start ();");
         WBI (" ");

         --  Output main program name

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         --  Main program is procedure case

         if ALIs.Table (ALIs.First).Main_Program = Proc then
            Set_String ("   ");
            Set_Main_Program_Name;
            Set_String (" ();");
            Write_Statement_Buffer;

            --  Main program is function case

         else -- ALIs.Table (ALIs_First).Main_Program = Func
            Set_String ("   return (");
            Set_Main_Program_Name;
            Set_String (" ());");
            Write_Statement_Buffer;
         end if;

      end if;

      WBI (" ");
      WBI ("   adafinal ();");

      --  The finalize routine is used only if we have a run-time

      if not No_Run_Time_Specified then
         WBI
          ("   __gnat_finalize ();");
      end if;

      WBI ("   exit (gnat_exit_status);");
      WBI ("}");
   end Gen_Main_C;

   ------------------------------
   -- Gen_Object_Files_Options --
   ------------------------------

   procedure Gen_Object_Files_Options is
      Lgnat                     : Integer;
      File                      : File_Name_Type;
      Wrote_Info_IDENTIFICATION : Boolean := False;

      With_DEC                  : Boolean := False;
      --  Flag which indicates whether the program uses the DEC compatibility
      --  library (declib).

      Object_Library_Extension : constant String
        := Value (Object_Library_Extension_Ptr);

      File_Only_Name_Len : Integer;

      procedure Write_Linker_Option;
      --  Write binder info linker option, removing duplicated
      --  IDENTIFICATION options.

      procedure Write_Linker_Option is
         Start : Natural;
         Stop  : Natural;

      begin
         --  Loop through string, breaking at null's

         Start := 1;
         while Start < Name_Len loop

            --  Find null ending this section

            Stop := Start + 1;
            while Name_Buffer (Stop) /= ASCII.NUL loop
               Stop := Stop + 1;
            end loop;

            --  Process section if non-null

            if Stop > Start then

               --  Special processing for IDENTIFICATION, remove duplicates

               if (Stop - Start) > 28
                 and then Name_Buffer (Start .. Start + 27) =
                                                "--for-linker=IDENTIFICATION="
               then
                  if not Wrote_Info_IDENTIFICATION then
                     Write_Info_Ada_C
                       ("   --   ", "", Name_Buffer (Start .. Stop - 1));
                     Wrote_Info_IDENTIFICATION := True;
                  end if;

               else
                  --  Normal case, note that string is null-terminated

                  Write_Info_Ada_C
                    ("   --   ", "", Name_Buffer (Start .. Stop - 1));
               end if;
            end if;

            Start := Stop + 1;
         end loop;
      end Write_Linker_Option;

   --  Start of processing for Gen_Object_Files_Options

   begin
      WBI ("");
      Write_Info_Ada_C ("--", "/*", " BEGIN Object file/option list");

      for E in Elab_Order.First .. Elab_Order.Last loop

         --  If not spec that has an associated body, then generate a
         --  comment giving the name of the corresponding object file.

         if Units.Table (Elab_Order.Table (E)).Utype /= Is_Spec then
            Get_Name_String
              (ALIs.Table
                (Units.Table (Elab_Order.Table (E)).My_ALI).Ofile_Full_Name);

            --  If the presence of an object file is necessary or if it
            --  exists, then use it.

            if not Hostparm.Exclude_Missing_Objects
              or else
                GNAT.OS_Lib.Is_Regular_File (Name_Buffer (1 .. Name_Len))
            then
               Write_Info_Ada_C ("   --   ", "", Name_Buffer (1 .. Name_Len));
               if Output_Object_List then
                  Write_Str (Name_Buffer (1 .. Name_Len));
                  Write_Eol;
               end if;

               --  Don't link with the shared library on VMS if an internal
               --  filename object is seen. Multiply defined symbols will
               --  result.

               if Hostparm.OpenVMS
                 and then Is_Internal_File_Name
                  (ALIs.Table
                   (Units.Table (Elab_Order.Table (E)).My_ALI).Sfile)
               then
                  Opt.Shared_Libgnat := False;
               end if;

            end if;
         end if;
      end loop;

      --  Add a "-Ldir" for each directory in the object path. We skip this
      --  in No_Run_Time mode, where we want more precise control of exactly
      --  what goes into the resulting object file

      if not No_Run_Time_Specified then
         for J in 1 .. Nb_Dir_In_Obj_Search_Path loop
            declare
               Dir : String_Ptr := Dir_In_Obj_Search_Path (J);
               Str : String (1 .. Dir'Length + 2);

            begin
               Str (1 .. 2)        := "-L";
               Str (3 .. Str'Last) := Dir.all;
               Write_Info_Ada_C ("   --   ", "", Str);

               --  If the declib directory is searched, flag it so that
               --  -ldecgnat can be appended later.

               if Hostparm.OpenVMS and then Str'Last > 7 and then
                Str (Str'Last - 7 .. Str'Last) = "/declib/"
               then
                  With_DEC := True;
               end if;
            end;
         end loop;
      end if;

      --  Sort linker options

      Sort (Linker_Options.Last, Move_Linker_Option'Access,
                                    Lt_Linker_Option'Access);

      --  Write user linker options

      Lgnat := Linker_Options.Last + 1;

      for J in 1 .. Linker_Options.Last loop
         if not Linker_Options.Table (J).Internal_File then
            Get_Name_String (Linker_Options.Table (J).Name);
            Write_Linker_Option;
         else
            Lgnat := J;
            exit;
         end if;
      end loop;

      --  Add libgnat here since it references symbols in the thread
      --  library which is included as part of the linker options of the
      --  runtime. Again, we do not want this in No_Run_Time mode.

      if not (No_Run_Time_Specified or else Opt.No_Stdlib) then

         Name_Len := 0;
         Add_Str_To_Name_Buffer ("libgnat" & Object_Library_Extension);

         --  Save the length of the object library filename, to ease removal
         --  later if necessary

         File_Only_Name_Len := Name_Len;

         File := Find_File (Name_Enter, Library);

         if File = No_File then

            --  libgnat object library couldn't be found, switch to -lgnat

            if With_DEC then
               Write_Info_Ada_C ("   --   ", "", "-ldecgnat");
            end if;

            if With_GNARL then
               Write_Info_Ada_C ("   --   ", "", "-lgnarl");
            end if;

            Write_Info_Ada_C ("   --   ", "", "-lgnat");

         else
            Get_Name_String (File);

            if Opt.Shared_Libgnat then

               --  If supported, add the run path linker option to specify
               --  where the shared libgnat is located.
               --
               --  Note that Name_Buffer contains the full path for the
               --  libgnat object library  so we need to remove
               --  "libgnat" itself plus its extension.

               Output_Run_Path_Option : declare
                  Run_Path_Option : constant String :=
                                      Value (Run_Path_Option_Ptr);

               begin
                  if Run_Path_Option'Length /= 0 then
                     Write_Info_Ada_C ("   --   ", "",
                       Run_Path_Option &
                       Name_Buffer (1 .. Name_Len - File_Only_Name_Len));
                  end if;

               end Output_Run_Path_Option;

               if With_DEC then
                  Write_Info_Ada_C ("   --   ", "", "-ldecgnat");
               end if;

               if With_GNARL then
                  Write_Info_Ada_C ("   --   ", "", "-lgnarl");
               end if;

               Write_Info_Ada_C ("   --   ", "", "-lgnat");

            else
               --  Add the full path to ensure that the static libraries
               --  will be linked

               if With_DEC then
                  --  With_DEC is only True on VMS
                  --  Constant 7 = strlen ("declib/")

                  Write_Info_Ada_C ("   --   ", "",
                    Name_Buffer (1 .. Name_Len - File_Only_Name_Len - 7)
                    & "declib/libdecgnat" & Object_Library_Extension);
               end if;

               if With_GNARL then
                  Write_Info_Ada_C ("   --   ", "",
                    Name_Buffer (1 .. Name_Len - File_Only_Name_Len)
                    & "libgnarl" & Object_Library_Extension);
               end if;

               Write_Info_Ada_C ("   --   ", "",
                 Name_Buffer (1 .. Name_Len));
            end if;
         end if;
      end if;

      --  Write internal linker options

      for J in Lgnat .. Linker_Options.Last loop
         Get_Name_String (Linker_Options.Table (J).Name);
         Write_Linker_Option;
      end loop;

      if Ada_Bind_File then
         WBI ("-- END Object file/option list   ");
      else
         WBI ("   END Object file/option list */");
      end if;

   end Gen_Object_Files_Options;

   -------------------------
   -- Gen_Output_File_Ada --
   -------------------------

   procedure Gen_Output_File_Ada (Filename : String) is

      Bfiles : Name_Id;
      --  Name of generated bind file (spec)

      Bfileb : Name_Id;
      --  Name of generated bind file (body)

      Ada_Main : constant String := Get_Ada_Main_Name;
      --  Name to be used for generated Ada main program. See the body of
      --  function Get_Ada_Main_Name for details on the form of the name.

      Target         : constant String_Ptr := Target_Name;
      VxWorks_Target : Boolean;

   begin
      VxWorks_Target := Target (Target'Last - 7 .. Target'Last) = "vxworks/";

      --  Create spec first

      Create_Binder_Output (Filename, 's', Bfiles);

      if No_Run_Time_Specified then
         WBI ("pragma No_Run_Time;");
      end if;

      --  Generate with of System so we can reference System.Address, note
      --  that such a reference is safe even in No_Run_Time mode, since we
      --  do not need any run-time code for such a reference, and we output
      --  a pragma No_Run_Time for this compilation above.

      WBI ("with System;");

      Resolve_Binder_Options;

      --  Usually, adafinal is called using a pragma Import C. Since Import C
      --  doesn't have the same semantic for jgnat, use standard Ada

      if Hostparm.Java_VM then
         if With_Tasking then
            WBI ("with System.Tasking.Stages;");
         elsif With_Finalization then
            WBI ("with System.Finalization_Implementation;");
         end if;
      end if;

      WBI ("package " & Ada_Main & " is");

      --  Exported variable to track elaboration/finalization phase

      WBI ("");
      WBI ("   Elab_Final_Code : Integer := 0;");
      WBI ("   pragma Export (C, Elab_Final_Code, " &
           """__gnat_elab_final_code"");");

      --  Main program case

      if Bind_Main_Program then

         --  Generate argc/argv stuff

         WBI ("");
         WBI ("   gnat_argc : Integer;");
         WBI ("   gnat_argv : System.Address;");
         WBI ("   gnat_envp : System.Address;");

         --  If we have a run time present, these variables are in the
         --  runtime data area for easy access from the runtime

         if not No_Run_Time_Specified then
            WBI ("");
            WBI ("   pragma Import (C, gnat_argc);");
            WBI ("   pragma Import (C, gnat_argv);");
            WBI ("   pragma Import (C, gnat_envp);");
         end if;

         --  Define exit status. Again in normal mode, this is in the
         --  run-time library, and is initialized there, but in the no
         --  run time case, the variable is here and initialized here.

         WBI ("");

         if No_Run_Time_Specified then
            WBI ("   gnat_exit_status : Integer := 0;");
         else
            WBI ("   gnat_exit_status : Integer;");
            WBI ("   pragma Import (C, gnat_exit_status);");
         end if;
      end if;

      WBI ("");
      WBI ("   GNAT_Version : constant String :=");
      WBI ("                    ""GNAT Version: " &
                                Gnat_Version_String & """;");
      WBI ("   pragma Export (C, GNAT_Version, ""__gnat_version"");");

      WBI ("");
      WBI ("   procedure adafinal;");
      WBI ("   pragma Export (C, adafinal);");

      WBI ("");
      WBI ("   procedure adainit;");
      WBI ("   pragma Export (C, adainit);");

      if Bind_Main_Program then

         --  If we have a run time, then Break_Start is defined there, but
         --  if there is no run-time, Break_Start is defined in this file.

         WBI ("");
         WBI ("   procedure Break_Start;");

         if No_Run_Time_Specified then
            WBI ("   pragma Export (C, Break_Start, ""__gnat_break_start"");");
         else
            WBI ("   pragma Import (C, Break_Start, ""__gnat_break_start"");");
         end if;

         WBI ("");
         WBI ("   function " & Get_Main_Name);

         if not VxWorks_Target then
            WBI ("     (argc : Integer;");
            WBI ("      argv : System.Address;");
            WBI ("      envp : System.Address)");
         end if;

         WBI ("      return Integer;");
         WBI ("   pragma Export (C, " & Get_Main_Name & ", """ &
           Get_Main_Name & """);");
      end if;

      Gen_Versions_Ada;

      --  Spec is complete

      WBI ("");
      WBI ("end " & Ada_Main & ";");
      Close_Binder_Output;

      --  Prepare to write body

      Create_Binder_Output (Filename, 'b', Bfileb);

      --  Output Source_File_Name pragmas which look like

      --    pragma Source_File_Name (Ada_Main, Spec_File_Name => "sss");
      --    pragma Source_File_Name (Ada_Main, Body_File_Name => "bbb");

      --  where sss/bbb are the spec/body file names respectively

      Get_Name_String (Bfiles);
      Name_Buffer (Name_Len + 1 .. Name_Len + 3) := """);";

      WBI ("pragma Source_File_Name (" &
           Ada_Main &
           ", Spec_File_Name => """ &
           Name_Buffer (1 .. Name_Len + 3));

      Get_Name_String (Bfileb);
      Name_Buffer (Name_Len + 1 .. Name_Len + 3) := """);";

      WBI ("pragma Source_File_Name (" &
           Ada_Main &
           ", Body_File_Name => """ &
           Name_Buffer (1 .. Name_Len + 3));

      WBI ("");
      WBI ("package body " & Ada_Main & " is");

      Gen_Adainit_Ada;
      Gen_Adafinal_Ada;

      if Bind_Main_Program then

         --  In No_Run_Time mode, generate dummy body for Break_Start

         if No_Run_Time_Specified then
            WBI ("");
            WBI ("   procedure Break_Start is");
            WBI ("   begin");
            WBI ("      null;");
            WBI ("   end;");
         end if;

         --  Main is only present for Ada main case

         if Bind_Main_Program then
            Gen_Main_Ada;
         end if;
      end if;

      --  Output object file list and the Ada body is complete

      Gen_Object_Files_Options;

      WBI ("");
      WBI ("end " & Ada_Main & ";");

      Close_Binder_Output;
   end Gen_Output_File_Ada;

   -----------------------
   -- Gen_Output_File_C --
   -----------------------

   procedure Gen_Output_File_C (Filename : String) is

      Bfile : Name_Id;
      --  Name of generated bind file

   begin
      Create_Binder_Output (Filename, 'c', Bfile);


      WBI ("#ifdef __STDC__");
      WBI ("#define PARAMS(paramlist) paramlist");
      WBI ("#else");
      WBI ("#define PARAMS(paramlist) ()");
      WBI ("#endif");
      WBI ("");

      WBI ("extern void __gnat_set_globals ");
      WBI (" PARAMS ((int, int, int, int, int, int, ");
      WBI ("          void (*) PARAMS ((void)), int, int));");
      WBI ("extern void adafinal PARAMS ((void));");
      WBI ("extern void adainit PARAMS ((void));");

      if not No_Main_Subprogram then
         WBI ("extern int main PARAMS ((int, char **, char **));");
         WBI ("extern void __gnat_break_start PARAMS ((void));");
         Set_String ("extern ");

         if ALIs.Table (ALIs.First).Main_Program = Proc then
            Set_String ("void ");
         else
            Set_String ("int ");
         end if;

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (" PARAMS ((void));");
         Write_Statement_Buffer;
      end if;

      if not No_Run_Time_Specified then
         WBI ("extern void __gnat_initialize PARAMS ((void));");
         WBI ("extern void __gnat_finalize PARAMS ((void));");
      end if;

      WBI ("");

      Gen_Elab_Defs_C;


      --  Exported variable to track elaboration/finalization phase

      WBI ("int __gnat_elab_final_code = 0;");
      WBI ("");

      --  Write argv/argc stuff if main program case

      if Bind_Main_Program then

         --  In the normal case, these are in the runtime library

         if not No_Run_Time_Specified then
            WBI ("extern int gnat_argc;");
            WBI ("extern char **gnat_argv;");
            WBI ("extern char **gnat_envp;");
            WBI ("extern int gnat_exit_status;");

         --  In the No_Run_Time case, they are right in the binder file
         --  and we initialize gnat_exit_status in the declaration.

         else
            WBI ("int gnat_argc;");
            WBI ("char **gnat_argv;");
            WBI ("char **gnat_envp;");
            WBI ("int gnat_exit_status = 0;");
         end if;

         WBI ("");

      end if;

      Resolve_Binder_Options;

      --  In no run-time mode, the __gnat_break_start routine (for the
      --  debugger to get initial control) is defined in this file.

      if No_Run_Time_Specified then
         WBI ("");
         WBI ("void __gnat_break_start () {}");
      end if;

      WBI ("");
      WBI ("char __gnat_version[] = ""GNAT Version: " &
                                Gnat_Version_String & """;");



      --  We need adainit and adafinal for both cases

      Gen_Adafinal_C;
      Gen_Adainit_C;

      --  Main is only present for Ada main case

      if Bind_Main_Program then
         Gen_Main_C;
      end if;

      --  Versions and object files needed in both cases

      Gen_Versions_C;
      Gen_Object_Files_Options;

      --  C binder output is complete

      Close_Binder_Output;
   end Gen_Output_File_C;

   ---------------------
   -- Gen_Output_File --
   ---------------------

   procedure Gen_Output_File (Filename : String) is

      function Public_Version return Boolean;
      --  Return true if the version number contains a 'p'

      function Public_Version return Boolean is
      begin
         for J in Gnat_Version_String'Range loop
            if Gnat_Version_String (J) = 'p'  then
               return True;
            end if;
         end loop;

         return False;
      end Public_Version;

   --  Start of processing for Gen_Output_File

   begin
      --  Override Ada_Bind_File and Bind_Main_Program for Java since
      --  jgnat only supports Ada code, and the main program is already
      --  generated by the compiler.

      if Hostparm.Java_VM then
         Ada_Bind_File := True;
         Bind_Main_Program := False;
      end if;

      --  Override time slice value if -T switch is set

      if Time_Slice_Set then
         ALIs.Table (ALIs.First).Time_Slice_Value := Opt.Time_Slice_Value;
      end if;

      --  Count number of elaboration calls

      for E in Elab_Order.First .. Elab_Order.Last loop
         if Units.Table (Elab_Order.Table (E)).No_Elab then
            null;
         else
            Num_Elab_Calls := Num_Elab_Calls + 1;
         end if;
      end loop;

      --  Get the time stamp of the former bind for public version warning

      if Public_Version then
         Record_Time_From_Last_Bind;
      end if;

      --  Generate output file in appropriate language

      if Ada_Bind_File then
         Gen_Output_File_Ada (Filename);
      else
         Gen_Output_File_C (Filename);
      end if;

      --  Periodically issue a warning when the public version is used on
      --  big projects

      if Public_Version then
         Public_Version_Warning;
      end if;
   end Gen_Output_File;

   ----------------------------
   -- Public_Version_Warning --
   ----------------------------

   procedure Public_Version_Warning is

      Time : Int := Time_From_Last_Bind;

      --  Constants to help defining periods

      Always : constant := 0;
      Hour   : constant := 60;
      Day    : constant := 24 * Hour;
      Month  : constant := 30 * Day;

      Never  : constant := Integer'Last;
      --  Special value indicating no warnings should be given

      --  Constants defining when the warning is issued. Programs with more
      --  than Large Units will issue a warning every Period_Large amount of
      --  time. Smaller programs will generate a warning every Period_Small
      --  amount of time.

      Large : constant := 20;
      --  Threshold for considering a program small or large

      Period_Large : constant := Day;
      --  Periodic warning time for large programs

      Period_Small : constant := Never;
      --  Periodic warning time for small programs

      Nb_Unit : Int;

   begin
      --  Compute the number of units that are not GNAT internal files

      Nb_Unit := 0;
      for A in ALIs.First .. ALIs.Last loop
         if not Is_Internal_File_Name (ALIs.Table (A).Sfile) then
            Nb_Unit := Nb_Unit + 1;
         end if;
      end loop;

      --  Do not emit the message if the last message was emitted in the
      --  specified period taking into account the number of units.

      if Nb_Unit < Large and then Time <= Period_Small then
         return;

      elsif Time <= Period_Large then
         return;
      end if;

      Write_Eol;
      Write_Str ("IMPORTANT NOTICE:");
      Write_Eol;
      Write_Str ("    This version of GNAT is unsupported"
        &                        " and comes with absolutely no warranty.");
      Write_Eol;
      Write_Str ("    If you intend to evaluate or use GNAT for building "
        &                                       "commercial applications,");
      Write_Eol;
      Write_Str ("    please consult http://www.gnat.com/ for information");
      Write_Eol;
      Write_Str ("    on the GNAT Professional product line.");
      Write_Eol;
      Write_Eol;
   end Public_Version_Warning;

   ----------------------
   -- Gen_Versions_Ada --
   ----------------------

   --  This routine generates two sets of lines. The first set has the form:

   --    unnnnn : constant Integer := 16#hhhhhhhh#;

   --  The second set has the form

   --    pragma Export (C, unnnnn, unam);

   --  for each unit, where unam is the unit name suffixed by either B or
   --  S for body or spec, with dots replaced by double underscores, and
   --  hhhhhhhh is the version number, and nnnnn is a 5-digits serial number.

   procedure Gen_Versions_Ada is
      Ubuf : String (1 .. 6) := "u00000";

      procedure Increment_Ubuf;
      --  Little procedure to increment the serial number

      procedure Increment_Ubuf is
      begin
         for J in reverse Ubuf'Range loop
            Ubuf (J) := Character'Succ (Ubuf (J));
            exit when Ubuf (J) <= '9';
            Ubuf (J) := '0';
         end loop;
      end Increment_Ubuf;

   --  Start of processing for Gen_Versions_Ada

   begin
      WBI ("");

      for U in Units.First .. Units.Last loop
         Increment_Ubuf;
         WBI ("   " & Ubuf & " : constant Integer := 16#" &
              Units.Table (U).Version & "#;");
      end loop;

      WBI ("");
      Ubuf := "u00000";

      for U in Units.First .. Units.Last loop
         Increment_Ubuf;
         Set_String ("   pragma Export (C, ");
         Set_String (Ubuf);
         Set_String (", """);

         Get_Name_String (Units.Table (U).Uname);

         for K in 1 .. Name_Len loop
            if Name_Buffer (K) = '.' then
               Set_Char ('_');
               Set_Char ('_');

            elsif Name_Buffer (K) = '%' then
               exit;

            else
               Set_Char (Name_Buffer (K));
            end if;
         end loop;

         if Name_Buffer (Name_Len) = 's' then
            Set_Char ('S');
         else
            Set_Char ('B');
         end if;

         Set_String (""");");
         Write_Statement_Buffer;
      end loop;

   end Gen_Versions_Ada;

   --------------------
   -- Gen_Versions_C --
   --------------------

   --  This routine generates a line of the form:

   --    unsigned unam = 0xhhhhhhhh;

   --  for each unit, where unam is the unit name suffixed by either B or
   --  S for body or spec, with dots replaced by double underscores.

   procedure Gen_Versions_C is
   begin
      for U in Units.First .. Units.Last loop
         Set_String ("unsigned ");

         Get_Name_String (Units.Table (U).Uname);

         for K in 1 .. Name_Len loop
            if Name_Buffer (K) = '.' then
               Set_String ("__");

            elsif Name_Buffer (K) = '%' then
               exit;

            else
               Set_Char (Name_Buffer (K));
            end if;
         end loop;

         if Name_Buffer (Name_Len) = 's' then
            Set_Char ('S');
         else
            Set_Char ('B');
         end if;

         Set_String (" = 0x");
         Set_String (Units.Table (U).Version);
         Set_Char   (';');
         Write_Statement_Buffer;
      end loop;

   end Gen_Versions_C;

   -----------------------
   -- Get_Ada_Main_Name --
   -----------------------

   function Get_Ada_Main_Name return String is
      Name  : String := "ada_main_00";
      Nlen  : Natural;

   begin
      --  The main program generated by jgnat expects a package called
      --  ada_<main procedure>

      if Hostparm.Java_VM then
         --  Get main program name

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         --  Remove the %b

         return "ada_" & Name_Buffer (1 .. Name_Len - 2);
      end if;

      --  This loop tries the following possibilities in order

      --    Ada_Main
      --    Ada_Main_01
      --    Ada_Main_02
      --    ..
      --    Ada_Main_99

      for J in 0 .. 99 loop
         if J = 0 then
            Nlen := 8;
         else
            Nlen := 11;
            Name (11) := Character'Val (J mod 10 + Character'Pos ('0'));
            Name (10) := Character'Val (J /   10 + Character'Pos ('0'));
         end if;

         for K in ALIs.First .. ALIs.Last loop
            for L in ALIs.Table (K).First_Unit .. ALIs.Table (K).Last_Unit loop

               --  Get unit name, removing %b or %e at end

               Get_Name_String (Units.Table (L).Uname);
               Name_Len := Name_Len - 2;

               if Name_Buffer (1 .. Name_Len) = Name (1 .. Nlen) then
                  goto Continue;
               end if;
            end loop;
         end loop;

         Name (1) := 'A';
         Name (5) := 'M';
         return Name (1 .. Nlen);

      <<Continue>>
         null;
      end loop;

      --  If we fall through, just use a peculiar unlikely name

      return ("Qwertyuiop");
   end Get_Ada_Main_Name;

   -------------------
   -- Get_Main_Name --
   -------------------

   function Get_Main_Name return String is
      Target         : constant String_Ptr := Target_Name;
      VxWorks_Target : Boolean;

   begin
      VxWorks_Target := Target (Target'Last - 7 .. Target'Last) = "vxworks/";

      if Bind_Alternate_Main_Name then
         return Alternate_Main_Name.all;
      elsif VxWorks_Target then

         --  Get main program name

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         --  Remove the %b

         return Name_Buffer (1 .. Name_Len - 2);

      else
         return "main";
      end if;
   end Get_Main_Name;

   ----------------------
   -- Lt_Linker_Option --
   ----------------------

   function Lt_Linker_Option (Op1, Op2 : Natural) return Boolean is
   begin
      if Linker_Options.Table (Op1).Internal_File
           /=
         Linker_Options.Table (Op2).Internal_File
      then
         return Linker_Options.Table (Op1).Internal_File
                  <
                 Linker_Options.Table (Op2).Internal_File;
      else
         if Units.Table (Linker_Options.Table (Op1).Unit).Elab_Position
              /=
            Units.Table (Linker_Options.Table (Op2).Unit).Elab_Position
         then
            return Units.Table (Linker_Options.Table (Op1).Unit).Elab_Position
                     >
                   Units.Table (Linker_Options.Table (Op2).Unit).Elab_Position;

         else
            return Linker_Options.Table (Op1).Original_Pos
                     <
                   Linker_Options.Table (Op2).Original_Pos;
         end if;
      end if;
   end Lt_Linker_Option;

   ------------------------
   -- Move_Linker_Option --
   ------------------------

   procedure Move_Linker_Option (From : Natural; To : Natural) is
   begin
      Linker_Options.Table (To) := Linker_Options.Table (From);
   end Move_Linker_Option;

   --------------
   -- Set_Char --
   --------------

   procedure Set_Char (C : Character) is
   begin
      Last := Last + 1;
      Statement_Buffer (Last) := C;
   end Set_Char;

   -------------
   -- Set_Int --
   -------------

   procedure Set_Int (N : Int) is
   begin
      if N < 0 then
         Set_String ("-");
         Set_Int (-N);

      else
         if N > 9 then
            Set_Int (N / 10);
         end if;

         Last := Last + 1;
         Statement_Buffer (Last) :=
           Character'Val (N mod 10 + Character'Pos ('0'));
      end if;
   end Set_Int;

   ---------------------------
   -- Set_Main_Program_Name --
   ---------------------------

   procedure Set_Main_Program_Name is
   begin
      --  Note that name has %b on the end which we ignore

      --  First we output the initial _ada_ since we know that the main
      --  program is a library level subprogram.

      Set_String ("_ada_");

      --  Copy name, changing dots to double underscores

      for J in 1 .. Name_Len - 2 loop
         if Name_Buffer (J) = '.' then
            Set_String ("__");
         else
            Set_Char (Name_Buffer (J));
         end if;
      end loop;
   end Set_Main_Program_Name;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (S : String) is
   begin
      Statement_Buffer (Last + 1 .. Last + S'Length) := S;
      Last := Last + S'Length;
   end Set_String;

   -------------------
   -- Set_Unit_Name --
   -------------------

   procedure Set_Unit_Name is
   begin
      for J in 1 .. Name_Len - 2 loop
         if Name_Buffer (J) /= '.' then
            Set_Char (Name_Buffer (J));
         else
            Set_String ("__");
         end if;
      end loop;
   end Set_Unit_Name;

   ------------
   -- Tab_To --
   ------------

   procedure Tab_To (N : Natural) is
   begin
      while Last < N loop
         Set_Char (' ');
      end loop;
   end Tab_To;

   ----------------------
   -- Write_Info_Ada_C --
   ----------------------

   procedure Write_Info_Ada_C (Ada : String; C : String; Common : String) is
   begin
      if Ada_Bind_File then
         declare
            S : String (1 .. Ada'Length + Common'Length);

         begin
            S (1 .. Ada'Length) := Ada;
            S (Ada'Length + 1 .. S'Length) := Common;
            WBI (S);
         end;

      else
         declare
            S : String (1 .. C'Length + Common'Length);

         begin
            S (1 .. C'Length) := C;
            S (C'Length + 1 .. S'Length) := Common;
            WBI (S);
         end;
      end if;
   end Write_Info_Ada_C;

   ----------------------------
   -- Write_Statement_Buffer --
   ----------------------------

   procedure Write_Statement_Buffer is
   begin
      WBI (Statement_Buffer (1 .. Last));
      Last := 0;
   end Write_Statement_Buffer;

end Bindgen;
