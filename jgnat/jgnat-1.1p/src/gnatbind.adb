------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T B I N D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.56 $
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
with ALI.Util; use ALI.Util;
with Bcheck;   use Bcheck;
with Binde;    use Binde;
with Binderr;  use Binderr;
with Bindgen;  use Bindgen;
with Bindusg;
with Butil;    use Butil;
with Csets;    use Csets;
with Gnatvsn;  use Gnatvsn;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Switch;   use Switch;
with Types;    use Types;

procedure Gnatbind is

   Total_Errors : Nat := 0;
   --  Counts total errors in all files

   Total_Warnings : Nat := 0;
   --  Total warnings in all files

   Main_Lib_File : File_Name_Type;
   --  Current main library file

   Std_Lib_File : File_Name_Type;
   --  Standard library

   Text : Text_Buffer_Ptr;
   Id   : ALI_Id;

   Next_Arg : Positive;

   Output_Filename_Seen : Boolean := False;

   Output_Filename : String_Ptr := new String'("");

   procedure Scan_Bind_Arg (Argv : String);
   --  Scan and process binder specific arguments. Argv is a single argument.
   --  All the one character arguments are still handled by Switch. This
   --  routine handles -aO -aI and -I-.

   -------------------
   -- Scan_Bind_Arg --
   -------------------

   procedure Scan_Bind_Arg (Argv : String) is
   begin
      --  Now scan arguments that are specific to the binder and are not
      --  handled by the common circuitry in Switch.

      if Opt.Output_Filename_Present
        and then not Output_Filename_Seen
      then
         Output_Filename_Seen := True;

         if Argv'Length = 0
           or else (Argv'Length >= 1
                     and then (Argv (1) = Switch_Character
                                or else Argv (1) = '-'))
         then
            Fail ("Output filename missing after -o");

         else
            Output_Filename := new String'(Argv);
         end if;

      elsif Argv'Length >= 2
        and then (Argv (1) = Switch_Character
                   or else Argv (1) = '-')
      then
         if Argv (2 .. Argv'Last) = "I-" then
            Opt.Look_In_Primary_Dir := False;

         elsif Argv (2) = 'I' then
            Add_Src_Search_Dir (Argv (3 .. Argv'Last));
            Add_Lib_Search_Dir (Argv (3 .. Argv'Last));

         elsif Argv'Length >= 3
           and then Argv (2 .. 3) = "aI"
         then
            Add_Src_Search_Dir (Argv (4 .. Argv'Last));

         elsif Argv'Length >= 3
           and then Argv (2 .. 3) = "aO"
         then
            Add_Lib_Search_Dir (Argv (4 .. Argv'Last));

         elsif Argv (2 .. Argv'Last) = "nostdlib" then
            Opt.No_Stdlib := True;

         elsif Argv (2 .. Argv'Last) = "nostdinc" then
            Opt.No_Stdinc := True;

         elsif Argv (2 .. Argv'Last) = "static" then
            Opt.Shared_Libgnat := False;

         elsif Argv (2 .. Argv'Last) = "shared" then
            Opt.Shared_Libgnat := True;

         elsif Argv'Length >= 3 and then Argv (2) = 'M' then
            Opt.Bind_Alternate_Main_Name := True;
            Opt.Alternate_Main_Name := new String '(Argv (3 .. Argv'Last));

         --  All other options are single character and are handled
         --  by Scan_Switches.

         else
            Scan_Switches (Argv);
         end if;

      --  Not a switch, so must be a filename (if non-empty)

      elsif Argv'Length /= 0 then
         if Argv'Length > 4
           and then Argv (Argv'Last - 3 .. Argv'Last) = ".ali"
         then
            Set_Main_File_Name (Argv);
         else
            Set_Main_File_Name (Argv & ".ali");
         end if;
      end if;
   end Scan_Bind_Arg;

--  Start of processing for Gnatbind

begin
   Osint.Initialize (Binder);

   if Usage_Requested then
      Bindusg;
   end if;

   --  Set default for Shared_Libgnat option

   declare
      Shared_Libgnat_Default : Character;
      pragma Import (C, Shared_Libgnat_Default, "shared_libgnat_default");

      SHARED : constant Character := 'H';
      STATIC : constant Character := 'T';

   begin
      pragma Assert
        (Shared_Libgnat_Default = SHARED
         or else
        Shared_Libgnat_Default = STATIC);
      Shared_Libgnat := (Shared_Libgnat_Default = SHARED);
   end;

   --  Use low level argument routines to avoid dragging in the secondary stack

   Next_Arg := 1;
   Scan_Args : loop
      exit when Next_Arg >= Arg_Count;
      declare
         Next_Argv : String (1 .. Len_Arg (Next_Arg));

      begin
         Fill_Arg (Next_Argv'Address, Next_Arg);
         Scan_Bind_Arg (Next_Argv);
      end;
      Next_Arg := Next_Arg + 1;
   end loop Scan_Args;

   --  check that the Ada binder file specified has extension .adb and that
   --  the C binder file has extension .c

   if Opt.Output_Filename_Present
     and then Output_Filename_Seen
   then

      Check_Extensions : declare
         Length : constant Natural := Output_Filename'Length;
         Last   : constant Natural := Output_Filename'Last;
      begin

         if Ada_Bind_File then
            if Length <= 4
              or else Output_Filename (Last - 3 .. Last) /= ".adb"
            then
               Fail ("Output filename should have .adb extension");
            end if;

         else
            if Length <= 2
              or else Output_Filename (Last - 1 .. Last) /= ".c"
            then
               Fail ("Output filename should have .c extension");
            end if;
         end if;
      end Check_Extensions;
   end if;

   Osint.Add_Default_Search_Dirs;

   if Verbose_Mode then
      Write_Eol;
      Write_Str ("GNATBIND ");
      Write_Str (Gnat_Version_String);
      Write_Str (" Copyright 1995-2000 Free Software Foundation, Inc.");
      Write_Eol;
   end if;

   --  Output usage information if no files

   if not More_Lib_Files then
      Bindusg;
      Exit_Program (E_Fatal);
   end if;

   --  The block here is to catch the Unrecoverable_Error exception in the
   --  case where we exceed the maximum number of permissible errors or some
   --  other unrecoverable error occurs.

   begin
      --  Carry out package initializations. These are initializations which
      --  might logically be performed at elaboration time, but Namet at
      --  least can't be done that way (because it is used in the Compiler),
      --  and we decide to be consistent. Like elaboration, the order in
      --  which these calls are made is in some cases important.

      Csets.Initialize;
      Namet.Initialize;
      Initialize_Binderr;
      Initialize_ALI;
      Initialize_ALI_Source;

      if Verbose_Mode then
         Write_Eol;
      end if;

      --  Input ALI files

      while More_Lib_Files loop
         Main_Lib_File := Next_Main_Lib_File;

         if Verbose_Mode then
            if Check_Only then
               Write_Str ("Checking: ");
            else
               Write_Str ("Binding: ");
            end if;

            Write_Name (Main_Lib_File);
            Write_Eol;
         end if;

         Text := Read_Library_Info (Main_Lib_File, True);
         Id := Scan_ALI
                 (F => Main_Lib_File,
                  T => Text,
                  Ignore_ED => Full_Elaboration_Semantics
                                 or Pessimistic_Elab_Order,
                  Err       => False);
         Free (Text);
      end loop;

      --  Add System.Standard_Library to list to ensure that these files are
      --  included in the bind, even if not directly referenced from Ada code
      --  This is of course omitted in No_Run_Time mode

      if not No_Run_Time_Specified then
         Name_Buffer (1 .. 12) := "s-stalib.ali";
         Name_Len := 12;
         Std_Lib_File := Name_Find;
         Text := Read_Library_Info (Std_Lib_File, True);
         Id :=
           Scan_ALI
             (F => Std_Lib_File,
              T => Text,
              Ignore_ED => Full_Elaboration_Semantics
                             or Pessimistic_Elab_Order,
              Err       => False);
         Free (Text);
      end if;

      --  Acquire all information in ALI files that have been read in

      for Index in ALIs.First .. ALIs.Last loop
         Read_ALI (Index);
      end loop;

      --  Quit if some file needs compiling

      if No_Object_Specified then
         raise Unrecoverable_Error;
      end if;

      --  Build source file table from the ALI files we have read in

      Set_Source_Table;

      --  Check that main library file is a suitable main program

      if Bind_Main_Program
        and then ALIs.Table (ALIs.First).Main_Program = None
        and then not No_Main_Subprogram
      then
         Error_Msg_Name_1 := Main_Lib_File;
         Error_Msg ("% does not contain a unit that can be a main program");
      end if;

      --  Perform consistency and correctness checks

      Check_Duplicated_Subunits;
      Check_Versions;
      Check_Consistency;
      Check_Configuration_Consistency;

      --  Complete bind if no errors

      if Errors_Detected = 0 then
         Find_Elab_Order;

         if Errors_Detected = 0 then
            if Elab_Order_Output then
               Write_Eol;
               Write_Str ("ELABORATION ORDER");
               Write_Eol;

               for I in Elab_Order.First .. Elab_Order.Last loop
                  Write_Str ("   ");
                  Write_Unit_Name (Units.Table (Elab_Order.Table (I)).Uname);
                  Write_Eol;
               end loop;

               Write_Eol;
            end if;

            if not Check_Only then
               Gen_Output_File (Output_Filename.all);
            end if;
         end if;
      end if;

      Total_Errors := Total_Errors + Errors_Detected;
      Total_Warnings := Total_Warnings + Warnings_Detected;

   exception
      when Unrecoverable_Error =>
         Total_Errors := Total_Errors + Errors_Detected;
         Total_Warnings := Total_Warnings + Warnings_Detected;
   end;

   --  All done. Set proper exit status.

   Finalize_Binderr;
   Namet.Finalize;

   if Total_Errors > 0 then
      Exit_Program (E_Errors);
   elsif Total_Warnings > 0 then
      Exit_Program (E_Warnings);
   else
      Exit_Program (E_Success);
   end if;

end Gnatbind;
