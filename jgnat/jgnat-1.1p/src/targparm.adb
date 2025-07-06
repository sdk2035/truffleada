------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                             T A R G P A R M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.7 $
--                                                                          --
--          Copyright (C) 1999-2000 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with Namet;    use Namet;
with Output;   use Output;
with Sinput;   use Sinput;
with Sinput.L; use Sinput.L;
with Types;    use Types;

package body Targparm is

   DEN : constant Source_Buffer := "Denorm";
   DSP : constant Source_Buffer := "Functions_Return_By_DSP";
   LSI : constant Source_Buffer := "Long_Shifts_Inlined";
   MOV : constant Source_Buffer := "Machine_Overflows";
   MRN : constant Source_Buffer := "Machine_Rounds";
   SCD : constant Source_Buffer := "Stack_Check_Default";
   SCP : constant Source_Buffer := "Stack_Check_Probes";
   SNZ : constant Source_Buffer := "Signed_Zeros";
   VMS : constant Source_Buffer := "OpenVMS";
   ZCE : constant Source_Buffer := "Zero_Cost_Exceptions";

   ---------------------------
   -- Get_Target_Parameters --
   ---------------------------

   procedure Get_Target_Parameters is
      use ASCII;

      S : Source_File_Index;
      T : Source_Buffer_Ptr;
      P : Source_Ptr;
      Z : Source_Ptr;

      function Get_Boolean return Boolean;
      --  Read True/False from current definition line in text and return it

      function Get_Boolean return Boolean is
      begin
         while T (P) /= ':' or else T (P + 1) /= '=' loop
            P := P + 1;
         end loop;

         P := P + 2;

         while T (P) = ' ' loop
            P := P + 1;
         end loop;

         return T (P) = 'T';
      end Get_Boolean;

   --  Start of processing for Get_Target_Parameters

   begin
      Name_Buffer (1 .. 10) := "system.ads";
      Name_Len := 10;
      S := Load_Source_File (Name_Find);

      if S = No_Source_File then
         Write_Line ("fatal error, run-time library not installed correctly");
         Write_Line ("cannot locate file system.ads");
         raise Unrecoverable_Error;
      end if;

      P := Source_First (S);
      Z := Source_Last  (S);
      T := Source_Text  (S);

      while T (P .. P + 10) /= "end System;" loop

         if T (P + 3 .. P + 2 + DEN'Length) = DEN then
            Denorm_On_Target := Get_Boolean;

         elsif T (P + 3 .. P + 2 + DSP'Length) = DSP then
            Functions_Return_By_DSP_On_Target := Get_Boolean;

         elsif T (P + 3 .. P + 2 + LSI'Length) = LSI then
            Long_Shifts_Inlined_On_Target := Get_Boolean;

         elsif T (P + 3 .. P + 2 + MOV'Length) = MOV then
            Machine_Overflows_On_Target := Get_Boolean;

         elsif T (P + 3 .. P + 2 + MRN'Length) = MRN then
            Machine_Rounds_On_Target := Get_Boolean;

         elsif T (P + 3 .. P + 2 + SCD'Length) = SCD then
            Stack_Check_Default_On_Target := Get_Boolean;

         elsif T (P + 3 .. P + 2 + SCP'Length) = SCP then
            Stack_Check_Probes_On_Target := Get_Boolean;

         elsif T (P + 3 .. P + 2 + SNZ'Length) = SNZ then
            Signed_Zeros_On_Target := Get_Boolean;

         elsif T (P + 3 .. P + 2 + VMS'Length) = VMS then
            OpenVMS_On_Target := Get_Boolean;

         elsif T (P + 3 .. P + 2 + ZCE'Length) = ZCE then
            Zero_Cost_Exceptions_On_Target := Get_Boolean;

         end if;

         while T (P) /= CR and then T (P) /= LF loop
            P := P + 1;
            exit when P >= Z;
         end loop;

         while T (P) = CR or else T (P) = LF loop
            P := P + 1;
            exit when P >= Z;
         end loop;

         if P >= Z then
            Write_Line ("fatal error, system.ads not formatted correctly");
            raise Unrecoverable_Error;
         end if;
      end loop;
   end Get_Target_Parameters;

end Targparm;
