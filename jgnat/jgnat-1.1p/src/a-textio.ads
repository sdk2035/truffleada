------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          A D A . T E X T _ I O                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.50 $
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

--  Note: the generic subpackages of Text_IO (Integer_IO, Float_IO, Fixed_IO,
--  Modular_IO, Decimal_IO and Enumeration_IO) appear as private children in
--  GNAT. These children are with'ed automatically if they are referenced, so
--  this rearrangement is invisible to user programs, but has the advantage
--  that only the needed parts of Text_IO are processed and loaded.

with Ada.IO_Exceptions;
with Ada.Streams;
with System;
with System.File_Control_Block;

package Ada.Text_IO is
pragma Elaborate_Body (Text_IO);

   type File_Type is limited private;
   type File_Mode is (In_File, Out_File, Append_File);

   --  The following representation clause allows the use of unchecked
   --  conversion for rapid translation between the File_Mode type
   --  used in this package and System.File_IO.

   for File_Mode use
     (In_File     => 0,  -- System.FIle_IO.File_Mode'Pos (In_File)
      Out_File    => 2,  -- System.File_IO.File_Mode'Pos (Out_File)
      Append_File => 3); -- System.File_IO.File_Mode'Pos (Append_File)

   type Count is range 0 .. Natural'Last;
   --  The value of Count'Last must be large enough so that the assumption
   --  enough so that the assumption that the Line, Column and Page
   --  counts can never exceed this value is a valid assumption.

   subtype Positive_Count is Count range 1 .. Count'Last;

   Unbounded : constant Count := 0;
   --  Line and page length

   subtype Field is Integer range 0 .. 255;
   --  Note: if for any reason, there is a need to increase this value,
   --  then it will be necessary to change the corresponding value in
   --  System.Img_Real in file s-imgrea.adb.

   subtype Number_Base is Integer range 2 .. 16;

   type Type_Set is (Lower_Case, Upper_Case);

   ---------------------
   -- File Management --
   ---------------------

   procedure Create
     (File : in out File_Type;
      Mode : in File_Mode := Out_File;
      Name : in String := "";
      Form : in String := "");

   procedure Open
     (File : in out File_Type;
      Mode : in File_Mode;
      Name : in String;
      Form : in String := "");

   procedure Close  (File : in out File_Type);
   procedure Delete (File : in out File_Type);
   procedure Reset  (File : in out File_Type; Mode : in File_Mode);
   procedure Reset  (File : in out File_Type);

   function Mode (File : in File_Type) return File_Mode;
   function Name (File : in File_Type) return String;
   function Form (File : in File_Type) return String;

   function Is_Open (File : in File_Type) return Boolean;

   ------------------------------------------------------
   -- Control of default input, output and error files --
   ------------------------------------------------------

   procedure Set_Input  (File : in File_Type);
   procedure Set_Output (File : in File_Type);
   procedure Set_Error  (File : in File_Type);

   function Standard_Input  return File_Type;
   function Standard_Output return File_Type;
   function Standard_Error  return File_Type;

   function Current_Input  return File_Type;
   function Current_Output return File_Type;
   function Current_Error  return File_Type;

   type File_Access is access constant File_Type;

   function Standard_Input  return File_Access;
   function Standard_Output return File_Access;
   function Standard_Error  return File_Access;

   function Current_Input  return File_Access;
   function Current_Output return File_Access;
   function Current_Error  return File_Access;

   --------------------
   -- Buffer control --
   --------------------

   --  Note: The parameter file is IN OUT in the RM, but this is clearly
   --  an oversight, and was intended to be IN, see AI95-00057.

   procedure Flush (File : in File_Type);
   procedure Flush;

   --------------------------------------------
   -- Specification of line and page lengths --
   --------------------------------------------

   procedure Set_Line_Length (File : in File_Type; To : in Count);
   procedure Set_Line_Length (To : in Count);

   procedure Set_Page_Length (File : in File_Type; To : in Count);
   procedure Set_Page_Length (To : in Count);

   function Line_Length (File : in File_Type) return Count;
   function Line_Length return Count;

   function Page_Length (File : in File_Type) return Count;
   function Page_Length return Count;

   ------------------------------------
   -- Column, Line, and Page Control --
   ------------------------------------

   procedure New_Line (File : in File_Type; Spacing : in Positive_Count := 1);
   procedure New_Line (Spacing : in Positive_Count := 1);

   procedure Skip_Line (File : in File_Type; Spacing : in Positive_Count := 1);
   procedure Skip_Line (Spacing : in Positive_Count := 1);

   function End_Of_Line (File : in File_Type) return Boolean;
   function End_Of_Line return Boolean;

   procedure New_Page (File : in File_Type);
   procedure New_Page;

   procedure Skip_Page (File : in File_Type);
   procedure Skip_Page;

   function End_Of_Page (File : in File_Type) return Boolean;
   function End_Of_Page return Boolean;

   function End_Of_File (File : in File_Type) return Boolean;
   function End_Of_File return Boolean;

   procedure Set_Col (File : in File_Type;  To : in Positive_Count);
   procedure Set_Col (To : in Positive_Count);

   procedure Set_Line (File : in File_Type; To : in Positive_Count);
   procedure Set_Line (To : in Positive_Count);

   function Col (File : in File_Type) return Positive_Count;
   function Col return Positive_Count;

   function Line (File : in File_Type) return Positive_Count;
   function Line return Positive_Count;

   function Page (File : in File_Type) return Positive_Count;
   function Page return Positive_Count;

   -----------------------------
   -- Characters Input-Output --
   -----------------------------

   procedure Get (File : in File_Type; Item : out Character);
   procedure Get (Item : out Character);
   procedure Put (File : in File_Type; Item : in Character);
   procedure Put (Item : in Character);

   procedure Look_Ahead
     (File        : in File_Type;
      Item        : out Character;
      End_Of_Line : out Boolean);

   procedure Look_Ahead
     (Item        : out Character;
      End_Of_Line : out Boolean);

   procedure Get_Immediate
     (File : in File_Type;
      Item : out Character);

   procedure Get_Immediate
     (Item : out Character);

   procedure Get_Immediate
     (File      : in File_Type;
      Item      : out Character;
      Available : out Boolean);

   procedure Get_Immediate
     (Item      : out Character;
      Available : out Boolean);

   --------------------------
   -- Strings Input-Output --
   --------------------------

   procedure Get (File : in File_Type; Item : out String);
   procedure Get (Item : out String);
   procedure Put (File : in File_Type; Item : in String);
   procedure Put (Item : in String);

   procedure Get_Line
     (File : in File_Type;
      Item : out String;
      Last : out Natural);

   procedure Get_Line
     (Item : out String;
      Last : out Natural);

   procedure Put_Line
     (File : in File_Type;
      Item : in String);

   procedure Put_Line
     (Item : in String);

   --  Exceptions

   Status_Error : exception renames IO_Exceptions.Status_Error;
   Mode_Error   : exception renames IO_Exceptions.Mode_Error;
   Name_Error   : exception renames IO_Exceptions.Name_Error;
   Use_Error    : exception renames IO_Exceptions.Use_Error;
   Device_Error : exception renames IO_Exceptions.Device_Error;
   End_Error    : exception renames IO_Exceptions.End_Error;
   Data_Error   : exception renames IO_Exceptions.Data_Error;
   Layout_Error : exception renames IO_Exceptions.Layout_Error;

private
   -----------------------------------
   -- Handling of Format Characters --
   -----------------------------------

   --  Line marks are represented by the single character ASCII.LF (16#0A#).
   --  In DOS and similar systems, underlying file translation takes care
   --  of translating this to and from the standard CR/LF sequences used in
   --  these operating systems to mark the end of a line. On output there is
   --  always a line mark at the end of the last line, but on input, this
   --  line mark can be omitted, and is implied by the end of file.

   --  Page marks are represented by the single character ASCII.FF (16#0C#),
   --  The page mark at the end of the file may be omitted, and is normally
   --  omitted on output unless an explicit New_Page call is made before
   --  closing the file. No page mark is added when a file is appended to,
   --  so, in accordance with the permission in (RM A.10.2(4)), there may
   --  or may not be a page mark separating preexising text in the file
   --  from the new text to be written.

   --  A file mark is marked by the physical end of file. In DOS translation
   --  mode on input, an EOF character (SUB = 16#1A#) gets translated to the
   --  physical end of file, so in effect this character is recognized as
   --  marking the end of file in DOS and similar systems.

   LM : constant := Character'Pos (ASCII.LF);
   --  Used as line mark

   PM : constant := Character'Pos (ASCII.FF);
   --  Used as page mark, except at end of file where it is implied

   --------------------------------
   -- Text_IO File Control Block --
   --------------------------------

   package FCB renames System.File_Control_Block;

   type Text_AFCB;
   type File_Type is access all Text_AFCB;

   type Text_AFCB is new FCB.AFCB with record
      Page        : Count := 1;
      Line        : Count := 1;
      Col         : Count := 1;
      Line_Length : Count := 0;
      Page_Length : Count := 0;

      Self : aliased File_Type;
      --  Set to point to the containing Text_AFCB block. This is used to
      --  implement the Current_{Error,Input,Ouput} functions which return
      --  a File_Access, the file access value returned is a pointer to
      --  the Self field of the corresponding file.

      Before_LM : Boolean := False;
      --  This flag is used to deal with the anomolies introduced by the
      --  peculiar definition of End_Of_File and End_Of_Page in Ada. These
      --  functions require looking ahead more than one character. Since
      --  there is no convenient way of backing up more than one character,
      --  what we do is to leave ourselves positioned past the LM, but set
      --  this flag, so that we know that from an Ada point of view we are
      --  in front of the LM, not after it. A bit of a kludge, but it works!

      Before_LM_PM : Boolean := False;
      --  This flag similarly handles the case of being physically positioned
      --  after a LM-PM sequence when logically we are before the LM-PM. This
      --  flag can only be set if Before_LM is also set.

   end record;

   function AFCB_Allocate (Control_Block : Text_AFCB) return FCB.AFCB_Ptr;

   procedure AFCB_Close (File : access Text_AFCB);
   procedure AFCB_Free  (File : access Text_AFCB);

   procedure Read
     (File : in out Text_AFCB;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);
   --  Read operation used when Text_IO file is treated directly as Stream

   procedure Write
     (File : in out Text_AFCB;
      Item : in Ada.Streams.Stream_Element_Array);
   --  Write operation used when Text_IO file is treated directly as Stream

   ------------------------
   -- The Standard Files --
   ------------------------

   Null_Str : aliased constant String := "";
   --  Used as name and form of standard files

   Standard_Err_AFCB : aliased Text_AFCB;
   Standard_In_AFCB  : aliased Text_AFCB;
   Standard_Out_AFCB : aliased Text_AFCB;

   Standard_Err : aliased File_Type := Standard_Err_AFCB'Access;
   Standard_In  : aliased File_Type := Standard_In_AFCB'Access;
   Standard_Out : aliased File_Type := Standard_Out_AFCB'Access;
   --  Standard files

   Current_In   : aliased File_Type := Standard_In;
   Current_Out  : aliased File_Type := Standard_Out;
   Current_Err  : aliased File_Type := Standard_Err;
   --  Current files

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  These subprograms are in the private part of the spec so that they can
   --  be shared by the routines in the body of Ada.Text_IO.Wide_Text_IO.

   --  Note: we use Integer in these declarations instead of the more accurate
   --  Interfaces.C_Streams.int, because we do not want to drag in the spec of
   --  this interfaces package with the spec of Ada.Text_IO, and we know that
   --  in fact these types are identical

   function Getc (File : File_Type) return Integer;
   --  Gets next character from file, which has already been checked for
   --  being in read status, and returns the character read if no error
   --  occurs. The result is EOF if the end of file was read.

   function Nextc (File : File_Type) return Integer;
   --  Returns next character from file without skipping past it (i.e. it
   --  is a combination of Getc followed by an Ungetc).

   procedure Putc (ch : Integer; File : File_Type);
   --  Outputs the given character to the file, which has already been
   --  checked for being in output status. Device_Error is raised if the
   --  character cannot be written.

   procedure Terminate_Line (File : File_Type);
   --  If the file is in Write_File or Append_File mode, and the current
   --  line is not terminated, then a line terminator is written using
   --  New_Line. Note that there is no Terminate_Page routine, because
   --  the page mark at the end of the file is implied if necessary.

   procedure Ungetc (ch : Integer; File : File_Type);
   --  Pushes back character into stream, using ungetc. The caller has
   --  checked that the file is in read status. Device_Error is raised
   --  if the character cannot be pushed back. An attempt to push back
   --  and end of file character (EOF) is ignored.

end Ada.Text_IO;
