------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               B C H E C K                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.34 $
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


with ALI;      use ALI;
with ALI.Util; use ALI.Util;
with Binderr;  use Binderr;
with Butil;    use Butil;
with Casing;   use Casing;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;
with Output;   use Output;
with Rident;   use Rident;
with Types;    use Types;

package body Bcheck is

   -------------------------------
   -- Check_Duplicated_Subunits --
   -------------------------------

   procedure Check_Duplicated_Subunits is
   begin
      for J in Sdep.First .. Sdep.Last loop
         if Sdep.Table (J).Subunit_Name /= No_Name then
            Get_Decoded_Name_String (Sdep.Table (J).Subunit_Name);
            Name_Len := Name_Len + 2;
            Name_Buffer (Name_Len - 1) := '%';

            --  See if there is a body or spec with the same name

            for K in Boolean loop
               if K then
                  Name_Buffer (Name_Len) := 'b';
               else
                  Name_Buffer (Name_Len) := 's';
               end if;

               declare
                  Info : constant Int := Get_Name_Table_Info (Name_Find);

               begin
                  if Info /= 0 then
                     Set_Standard_Error;
                     Write_Str ("error: subunit """);
                     Write_Name_Decoded (Sdep.Table (J).Subunit_Name);
                     Write_Str (""" in file """);
                     Write_Name_Decoded (Sdep.Table (J).Sfile);
                     Write_Char ('"');
                     Write_Eol;
                     Write_Str ("       has same name as unit """);
                     Write_Unit_Name (Units.Table (Unit_Id (Info)).Uname);
                     Write_Str (""" found in file """);
                     Write_Name_Decoded (Units.Table (Unit_Id (Info)).Sfile);
                     Write_Char ('"');
                     Write_Eol;
                     Write_Str ("       this is not allowed within a single "
                                & "partition (RM 10.2(19))");
                     Write_Eol;
                     Osint.Exit_Program (Osint.E_Fatal);
                  end if;
               end;
            end loop;
         end if;
      end loop;
   end Check_Duplicated_Subunits;

   --------------------
   -- Check_Versions --
   --------------------

   procedure Check_Versions is
      VL : constant Natural := ALIs.Table (ALIs.First).Ver_Len;

   begin
      for A in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A).Ver_Len /= VL
           or else ALIs.Table (A).Ver          (1 .. VL) /=
                   ALIs.Table (ALIs.First).Ver (1 .. VL)
         then
            Error_Msg_Name_1 := ALIs.Table (A).Sfile;
            Error_Msg_Name_2 := ALIs.Table (ALIs.First).Sfile;

            if Tolerate_Consistency_Errors then
               Error_Msg
                 ("?% and % compiled with different GNAT versions");
            else
               Error_Msg
                 ("% and % compiled with different GNAT versions");
            end if;
         end if;
      end loop;
   end Check_Versions;

   -------------------------------------
   -- Check_Configuration_Consistency --
   -------------------------------------

   procedure Check_Configuration_Consistency is
      P1, P2 : Character;

   begin
      --  Check consistent floating format

      if Float_Format_Specified /= ' ' then
         Fouter : for A1 in ALIs.First .. ALIs.Last loop
            P1 := ALIs.Table (A1).Float_Format;

            for A2 in ALIs.First .. ALIs.Last loop
               P2 := ALIs.Table (A2).Float_Format;

               if P1 /= P2 then
                  Error_Msg_Name_1 := ALIs.Table (A1).Sfile;
                  Error_Msg_Name_2 := ALIs.Table (A2).Sfile;

                  if Tolerate_Consistency_Errors then
                     Error_Msg
                       ("?% and % compiled with different " &
                        "floating-point representations");
                  else
                     Error_Msg
                       ("% and % compiled with different " &
                        "floating-point representations");
                  end if;

                  exit Fouter;
               end if;
            end loop;
         end loop Fouter;
      end if;

      --  Check consistent Normalize_Scalars settings

      if Normalize_Scalars_Specified and No_Normalize_Scalars_Specified then
         if Tolerate_Consistency_Errors then
            Error_Msg
              ("?some but not all files compiled with Normalize_Scalars");
         else
            Error_Msg
              ("some but not all files compiled with Normalize_Scalars");
         end if;

         Write_Eol;
         Write_Str ("files compiled with Normalize_Scalars");
         Write_Eol;

         for A1 in ALIs.First .. ALIs.Last loop
            if ALIs.Table (A1).Normalize_Scalars then
               Write_Str ("  ");
               Write_Name (ALIs.Table (A1).Sfile);
               Write_Eol;
            end if;
         end loop;

         Write_Eol;
         Write_Str ("files compiled without Normalize_Scalars");
         Write_Eol;

         for A1 in ALIs.First .. ALIs.Last loop
            if not ALIs.Table (A1).Normalize_Scalars then
               Write_Str ("  ");
               Write_Name (ALIs.Table (A1).Sfile);
               Write_Eol;
            end if;
         end loop;
      end if;

      --  Check consistent queuing policy

      if Queuing_Policy_Specified /= ' ' then
         Qouter : for A1 in ALIs.First .. ALIs.Last loop
            P1 := ALIs.Table (A1).Queuing_Policy;

            if P1 /= ' ' then
               for A2 in ALIs.First .. ALIs.Last loop
                  P2 := ALIs.Table (A2).Queuing_Policy;

                  if P2 /= ' ' then
                     if P1 /= P2 then
                        Error_Msg_Name_1 := ALIs.Table (A1).Sfile;
                        Error_Msg_Name_2 := ALIs.Table (A2).Sfile;

                        if Tolerate_Consistency_Errors then
                           Error_Msg
                             ("?% and % compiled with different " &
                              "queuing policies");
                        else
                           Error_Msg
                             ("% and % compiled with different " &
                              "queuing policies");
                        end if;

                        exit Qouter;
                     end if;
                  end if;
               end loop;
            end if;
         end loop Qouter;
      end if;

      --  Check consistent locking policy

      --  Note: this is unnecessary code as long as only one locking policy
      --  is defined, but it is in place so that if locking policies are
      --  added then they will be checked for consistency.

      if Locking_Policy_Specified /= ' ' then
         Louter : for A1 in ALIs.First .. ALIs.Last loop
            P1 := ALIs.Table (A1).Locking_Policy;

            if P1 /= ' ' then
               for A2 in ALIs.First .. ALIs.Last loop
                  P2 := ALIs.Table (A2).Locking_Policy;

                  if P2 /= ' ' then
                     if P1 /= P2 then
                        Error_Msg_Name_1 := ALIs.Table (A1).Sfile;
                        Error_Msg_Name_2 := ALIs.Table (A2).Sfile;

                        if Tolerate_Consistency_Errors then
                           Error_Msg
                             ("?% and % compiled with different " &
                              "locking policies");
                        else
                           Error_Msg
                             ("% and % compiled with different " &
                              "locking policies");
                        end if;

                        exit Louter;
                     end if;
                  end if;
               end loop;
            end if;
         end loop Louter;
      end if;

      --  Check consistent zero cost exception handling

      if Zero_Cost_Exceptions_Specified then
         for A1 in ALIs.First + 1 .. ALIs.Last loop
            if ALIs.Table (A1).Zero_Cost_Exceptions /=
               ALIs.Table (ALIs.First).Zero_Cost_Exceptions
            then
               Error_Msg_Name_1 := ALIs.Table (A1).Sfile;
               Error_Msg_Name_2 := ALIs.Table (ALIs.First).Sfile;

               if Tolerate_Consistency_Errors then
                  Error_Msg
                    ("?% and % compiled with different " &
                     "exception handling mechanism");
               else
                  Error_Msg
                    ("% and % compiled with different " &
                     "exception handling mechanism");
               end if;
            end if;
         end loop;
      end if;

      --  Check for restrictions violations

      for J in Partition_Restrictions loop
         for A1 in ALIs.First .. ALIs.Last loop
            if ALIs.Table (A1).Restrictions (J) = 'r' then
               for A2 in ALIs.First .. ALIs.Last loop

                  --  Check for violation, but do not count standard units

                  if ALIs.Table (A2).Restrictions (J) = 'v' then
                     Get_Name_String (ALIs.Table (A2).Sfile);

                     if Name_Len < 3
                       or else Name_Buffer (2) /= '-'
                       or else (Name_Buffer (1) /= 's'
                                  and then
                                Name_Buffer (1) /= 'a'
                                  and then
                                Name_Buffer (1) /= 'i'
                                  and then
                                Name_Buffer (1) /= 'g')
                     then
                        declare
                           M1 : constant String := "% has Restriction (";

                           M2 : String :=
                                 M1 & "?????????????????????????????????????";

                           S : String := Restriction_Id'Image (J);

                        begin
                           Name_Buffer (1 .. S'Length) := S;
                           Name_Len := S'Length;
                           Set_Casing
                             (Units.Table
                               (ALIs.Table (A1).First_Unit).Icasing);

                           M2 (M1'Length + 1 .. M1'Length + S'Length) :=
                             Name_Buffer (1 .. S'Length);
                           M2 (M1'Length + S'Length + 1) := ')';

                           Error_Msg_Name_1 := ALIs.Table (A1).Sfile;
                           Error_Msg (M2 (1 .. M1'Length + S'Length + 1));
                           Error_Msg_Name_1 := ALIs.Table (A2).Sfile;
                           Error_Msg ("but file % violates this restriction");
                        end;
                     end if;
                  end if;
               end loop;
            end if;
         end loop;
      end loop;
   end Check_Configuration_Consistency;

   -----------------------
   -- Check_Consistency --
   -----------------------

   procedure Check_Consistency is
      Src : Source_Id;
      --  Source file Id for this Sdep entry

   begin
      --  First, we go through the source table to see if there are any cases
      --  in which we should go after source files and compute checksums of
      --  the source files. We need to do this for any file for which we have
      --  mismatching time stamps and (so far) matching checksums.

      for S in Source.First .. Source.Last loop

         --  If all time stamps for a file match, then there is nothing to
         --  do, since we will not be checking checksums in that case anyway

         if Source.Table (S).All_Timestamps_Match then
            null;

         --  If we did not find the source file, then we can't compute its
         --  checksum anyway. Note that when we have a time stamp mismatch,
         --  we try to find the source file unconditionally (i.e. if
         --  Check_Source_Files is False).

         elsif not Source.Table (S).Source_Found then
            null;

         --  If we already have non-matching or missing checksums, then no
         --  need to try going after source file, since we won't trust the
         --  checksums in any case.

         elsif not Source.Table (S).All_Checksums_Match then
            null;

         --  Now we have the case where we have time stamp mismatches, and
         --  the source file is around, but so far all checksums match. This
         --  is the case where we need to compute the checksum from the source
         --  file, since otherwise we would ignore the time stamp mismatches,
         --  and that is wrong if the checksum of the source does not agree
         --  with the checksums in the ALI files.

         elsif Check_Source_Files then
            if Source.Table (S).Checksum /=
               Get_File_Checksum (Source.Table (S).Sfile)
            then
               Source.Table (S).All_Checksums_Match := False;
            end if;
         end if;
      end loop;

      --  Loop through ALI files

      ALIs_Loop : for A in ALIs.First .. ALIs.Last loop

         --  Loop through Sdep entries in one ALI file

         Sdep_Loop : for D in
           ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep
         loop
            Src := Source_Id (Get_Name_Table_Info (Sdep.Table (D).Sfile));

            --  If the time stamps match, or all checksums match, then we
            --  are OK, otherwise we have a definite error.

            if Sdep.Table (D).Stamp /= Source.Table (Src).Stamp
              and then not Source.Table (Src).All_Checksums_Match
            then
               Error_Msg_Name_1 := ALIs.Table (A).Sfile;
               Error_Msg_Name_2 := Sdep.Table (D).Sfile;

               --  Two styles of message, depending on whether or not
               --  the updated file is the one that must be recompiled

               if Error_Msg_Name_1 = Error_Msg_Name_2 then
                  if Tolerate_Consistency_Errors then
                     Error_Msg
                        ("?% has been modified and should be recompiled");
                  else
                     Error_Msg
                       ("% has been modified and must be recompiled");
                  end if;

               else
                  if Tolerate_Consistency_Errors then
                     Error_Msg
                       ("?% should be recompiled (% has been modified)");
                  else
                     Error_Msg
                       ("% must be recompiled (% has been modified)");
                  end if;
               end if;

               if (not Tolerate_Consistency_Errors) and Verbose_Mode then
                  declare
                     Msg : constant String := "file % has time stamp ";
                     Buf : String (1 .. Msg'Length + Time_Stamp_Length);

                  begin
                     Buf (1 .. Msg'Length) := Msg;
                     Buf (Msg'Length + 1 .. Buf'Length) :=
                       String (Source.Table (Src).Stamp);
                     Error_Msg_Name_1 := ALIs.Table (A).Sfile;
                     Error_Msg (Buf);

                     Buf (Msg'Length + 1 .. Buf'Length) :=
                       String (Sdep.Table (D).Stamp);
                     Error_Msg_Name_1 := Sdep.Table (D).Sfile;
                     Error_Msg (Buf);
                  end;
               end if;

               --  Exit from the loop through Sdep entries once we find one
               --  that does not match.

               exit Sdep_Loop;
            end if;

         end loop Sdep_Loop;
      end loop ALIs_Loop;
   end Check_Consistency;

end Bcheck;
