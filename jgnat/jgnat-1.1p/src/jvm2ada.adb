------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J V M 2 A D A                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.19 $
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

with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Command_Line;
with J_Utils;
with JVM_Ada;
with Gnatvsn;

procedure Jvm2Ada is

   ----------------
   -- Local Data --
   ----------------

   Very_Verbose : Boolean := False;
   --  When this flag is set set the Verbose flag in J_Util.Command_Line to
   --  display all the .class files and directories which are skipped while
   --  processing a jar/zip file.

   --------------------
   -- Local Routines --
   --------------------

   procedure Process_Switches;
   --  Parse the switches on the command line

   procedure Write_Usage;
   --  Print program options

   package Jvm2Ada_Command_Line is
     new J_Utils.Command_Line
       (Verbose           => Very_Verbose,
        Write_Usage       => Write_Usage,
        Process_Switches  => Process_Switches,
        Process_Class     => JVM_Ada.Convert_To_Ada,
        Process_Directory => JVM_Ada.Convert_Directory_To_Ada);
   --  Generic instantiation that does the actual job of processing the
   --  command line of a jvm2ada invocation and calling the appropriate
   --  routines that transform JVM .class files into Ada specs.

   ----------------------
   -- Process_Switches --
   ----------------------

   procedure Process_Switches is
   begin
      loop
         case GNAT.Command_Line.Getopt ("I! L! k o: q s v V w") is
            when ASCII.Nul =>
               exit;

            when 'I' =>
               JVM_Ada.Search_Sources_In (GNAT.Command_Line.Parameter);

            when 'L' =>
               JVM_Ada.Search_Classes_In (GNAT.Command_Line.Parameter);

            when 'k' =>
               JVM_Ada.Keep_Original_Identifiers := True;

            when 'o' =>
               JVM_Ada.Output_Dir := new String'(GNAT.Command_Line.Parameter);

            when 'q' =>
               JVM_Ada.Quiet_Mode := True;

            when 's' =>
               JVM_Ada.Skip_Sun_Classes := False;

            when 'v' =>
               JVM_Ada.Verbose_Mode := True;

            when 'V' =>
               Very_Verbose         := True;
               JVM_Ada.Verbose_Mode := True;

            when 'w' =>
               JVM_Ada.Overwrite_Files := True;

            when others =>
               Write_Usage;
               return;
         end case;
      end loop;
   end Process_Switches;

   -----------------
   -- Write_Usage --
   -----------------

   procedure Write_Usage is
   begin
      Put_Line ("JVM2Ada " & Gnatvsn.Gnat_Version_String
                & " Copyright 1998-1999, Ada Core Technologies Inc.");
      Put_Line ("Usage: jvm2ada [switches] file [file ...]");
      New_Line;
      Put_Line ("  file   Can be one of the following:");
      Put_Line ("         . A .class file (you can omit the .class suffix)");
      Put      ("         . A zip archive (jvmlist applies to all .class");
      Put_Line (" files in the archive)");
      Put      ("         . A .class file inside a zip archive (e.g. ");
      Put_Line ("rt.jar/java/io/File.class)");
      New_Line;
      Put_Line ("Possible switches:");
      Put_Line ("  -Izip  Look for sources in archive ""zip""");
      Put_Line ("  -Lzip  Look for .class files in archive ""zip""");
      Put_Line ("  -k     Keep original identifiers");
      Put_Line ("  -o dir Output Ada specifications to directory ""dir""");
      Put_Line ("  -q     Quiet");
      Put_Line ("  -s     Do not ignore Sun classes if they are public");
      Put_Line ("  -v     Verbose");
      Put_Line ("  -V     Very verbose");
      Put_Line ("  -w     Overwrite existing filenames");
      New_Line;
   end Write_Usage;

--  Start of processing of Jvm2Ada
begin
   Jvm2Ada_Command_Line.Process;
end Jvm2Ada;
