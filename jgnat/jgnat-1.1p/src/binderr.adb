------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              B I N D E R R                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.20 $                             --
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

with Butil;   use Butil;
with Namet;   use Namet;
with Opt;     use Opt;
with Output;  use Output;

package body Binderr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Error_Msg_Output (Msg : String; Info : Boolean := False);
   --  Output given message, with insertions, to current message output file.
   --  The second argument is True for an info message, false for a normal
   --  warning or error message.

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg (Msg : String) is
   begin
      if Msg (Msg'First) = '?' then
         if Warning_Mode = Suppress then
            return;
         end if;

         if Warning_Mode = Treat_As_Error then
            Errors_Detected := Errors_Detected + 1;
         else
            Warnings_Detected := Warnings_Detected + 1;
         end if;

      else
         Errors_Detected := Errors_Detected + 1;
      end if;

      if Brief_Output or else (not Verbose_Mode) then
         Set_Standard_Error;
         Error_Msg_Output (Msg);
         Set_Standard_Output;
      end if;

      if Verbose_Mode then
         if Errors_Detected + Warnings_Detected = 0 then
            Write_Eol;
         end if;

         Error_Msg_Output (Msg);
      end if;

      if Warnings_Detected + Errors_Detected > Maximum_Errors then
         raise Unrecoverable_Error;
      end if;

   end Error_Msg;

   --------------------
   -- Error_Msg_Info --
   --------------------

   procedure Error_Msg_Info (Msg : String) is
   begin
      if Brief_Output or else (not Verbose_Mode) then
         Set_Standard_Error;
         Error_Msg_Output (Msg, True);
         Set_Standard_Output;
      end if;

      if Verbose_Mode then
         Error_Msg_Output (Msg, True);
      end if;

   end Error_Msg_Info;

   ----------------------
   -- Error_Msg_Output --
   ----------------------

   procedure Error_Msg_Output (Msg : String; Info : Boolean := False) is
      Use_Second_Name : Boolean := False;

   begin
      if Warnings_Detected + Errors_Detected > Maximum_Errors then
         Write_Str ("error: maximum errors exceeded");
         Write_Eol;
         return;
      end if;

      if Msg (Msg'First) = '?' then
         Write_Str ("warning: ");
      elsif Info then
         if not Info_Prefix_Suppress then
            Write_Str ("info:  ");
         end if;
      else
         Write_Str ("error: ");
      end if;

      for I in Msg'Range loop
         if Msg (I) = '%' then

            if Use_Second_Name then
               Get_Decoded_Name_String (Error_Msg_Name_2);
            else
               Use_Second_Name := True;
               Get_Decoded_Name_String (Error_Msg_Name_1);
            end if;

            Write_Char ('"');
            Write_Str (Name_Buffer (1 .. Name_Len));
            Write_Char ('"');

         elsif Msg (I) = '&' then
            Write_Char ('"');

            if Use_Second_Name then
               Write_Unit_Name (Error_Msg_Name_2);
            else
               Use_Second_Name := True;
               Write_Unit_Name (Error_Msg_Name_1);
            end if;

            Write_Char ('"');

         elsif Msg (I) /= '?' then
            Write_Char (Msg (I));
         end if;
      end loop;

      Write_Eol;
   end Error_Msg_Output;

   ----------------------
   -- Finalize_Binderr --
   ----------------------

   procedure Finalize_Binderr is
   begin
      --  Message giving number of errors detected (verbose mode only)

      if Verbose_Mode then
         Write_Eol;

         if Errors_Detected = 0 then
            Write_Str ("No errors");

         elsif Errors_Detected = 1 then
            Write_Str ("1 error");

         else
            Write_Int (Errors_Detected);
            Write_Str (" errors");
         end if;

         if Warnings_Detected = 1 then
            Write_Str (", 1 warning");

         elsif Warnings_Detected > 1 then
            Write_Str (", ");
            Write_Int (Warnings_Detected);
            Write_Str (" warnings");
         end if;

         Write_Eol;
      end if;
   end Finalize_Binderr;

   ------------------------
   -- Initialize_Binderr --
   ------------------------

   procedure Initialize_Binderr is
   begin
      Errors_Detected := 0;
      Warnings_Detected := 0;
   end Initialize_Binderr;

end Binderr;
