------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              B I N D E R R                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.12 $                             --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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

--  This package contains the routines to output error messages for the binder
--  and also the routines for handling fatal error conditions in the binder.

with Types; use Types;

package Binderr is

   Errors_Detected : Int;
   --  Number of errors detected so far

   Warnings_Detected : Int;
   --  Number of warnings detected

   Info_Prefix_Suppress : Boolean := False;
   --  If set to True, the normal "info: " header before messages generated
   --  by Error_Msg_Info will be omitted.

   ---------------------------------------------------------
   -- Error Message Text and Message Insertion Characters --
   ---------------------------------------------------------

   --  Error message text strings are composed of letters, digits and the
   --  special characters space, comma, period, colon and semicolon,
   --  apostrophe and parentheses. Special insertion characters can also
   --  appear which cause the error message circuit to modify the given
   --  string as follows:

   --    Insertion character % (Percent: insert file name from Names table)
   --      The character % is replaced by the text for the file name specified
   --      by the Name_Id value stored in Error_Msg_Name_1. The name is always
   --      enclosed in quotes. A second % may appear in a single message in
   --      which case it is similarly replaced by the name which is specified
   --      by the Name_Id value stored in Error_Msg_Name_2.

   --    Insertion character & (Ampersand: insert unit name from Names table)
   --      The character & is replaced by the text for the unit name specified
   --      by the Name_Id value stored in Error_Msg_Name_1. The name is always
   --      enclosed in quotes. A second & may appear in a single message in
   --      which case it is similarly replaced by the name which is specified
   --      by the Name_Id value stored in Error_Msg_Name_2.

   --    Insertion character ? (Question mark: warning message)
   --      The character ?, which must be the first character in the message
   --      string, signals a warning message instead of an error message.

   -----------------------------------------------------
   -- Global Values Used for Error Message Insertions --
   -----------------------------------------------------

   --  The following global variables are essentially additional parameters
   --  passed to the error message routine for insertion sequences described
   --  above. The reason these are passed globally is that the insertion
   --  mechanism is essentially an untyped one in which the appropriate
   --  variables are set dependingon the specific insertion characters used.

   Error_Msg_Name_1 : Name_Id;
   Error_Msg_Name_2 : Name_Id;
   --  Name_Id values for % insertion characters in message

   ------------------------------
   -- Error Output Subprograms --
   ------------------------------

   procedure Error_Msg (Msg : String);
   --  Output specified error message to standard error or standard output
   --  as governed by the brief and verbose switches, and update error
   --  counts appropriately

   procedure Error_Msg_Info (Msg : String);
   --  Output information line. Indentical in effect to Error_Msg, except
   --  that the prefix is info: instead of error: and the error count is
   --  not incremented. The prefix may be suppressed by setting the global
   --  variable Info_Prefix_Suppress to True.

   procedure Finalize_Binderr;
   --  Finalize error output for one file

   procedure Initialize_Binderr;
   --  Initialize error output for one file

end Binderr;
