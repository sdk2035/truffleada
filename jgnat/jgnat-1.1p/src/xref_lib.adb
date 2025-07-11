------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              X R E F _ L I B                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.40 $
--                                                                          --
--          Copyright (C) 1998-2000 Free Software Foundation, Inc.          --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.IO_Aux;               use GNAT.IO_Aux;
with Output;                    use Output;

package body Xref_Lib is

   ---------------------
   -- Local Variables --
   ---------------------

   D    : constant Character := 'D';
   X    : constant Character := 'X';
   W    : constant Character := 'W';
   Dot  : constant Character := '.';
   Pipe : constant Character := '|';
   --  First character on xref lines in the .ali file

   EOF : constant Character := ASCII.SUB;
   --  Special character to signal end of file. Not required in input file,
   --  but should be properly treated if present. See also Read_File.

   No_Xref_Information : exception;
   --  Exception raised when there is no cross-referencing information in
   --  the .ali files

   subtype File_Offset is Natural;

   function End_Of_Line_Index (File : ALI_File) return Integer;
   --  Returns the index of the last character of the current_line

   procedure Read_File
     (FD       : File_Descriptor;
      Contents : out String_Access;
      Success  : out Boolean);
   --  Reads file associated with FS into the newly allocated
   --  string Contents. An EOF character will be added to the
   --  returned Contents to simplify parsing.
   --  [VMS] Success is true iff the number of bytes read is less than or
   --   equal to the file size.
   --  [Other] Success is true iff the number of bytes read is equal to
   --   the file size.

   procedure Parse_EOL (Source : access String; Ptr : in out Positive);
   --  On return Source (Ptr) is the first character of the next line
   --  or EOF. Source.all must be terminated by EOF.

   procedure Parse_Identifier_Info
     (Pattern       : Search_Pattern;
      File          : in out ALI_File;
      Local_Symbols : Boolean;
      Der_Info      : Boolean := False;
      Type_Tree     : Boolean := False;
      Wide_Search   : Boolean := True);
   --  Output the file and the line where the identifier was referenced,
   --  If Local_Symbols is False then only the publicly visible symbols
   --  will be processed

   procedure Parse_Token
     (Source    : access String;
      Ptr       : in out Positive;
      Token_Ptr : out Positive);
   --  Skips any separators and stores the start of the token in Token_Ptr.
   --  Then stores the position of the next separator in Ptr.
   --  On return Source (Token_Ptr .. Ptr - 1) is the token.
   --  Separators are space and ASCII.HT.
   --  Parse_Token will never skip to the next line.

   procedure Parse_Number
     (Source : access String;
      Ptr    : in out Positive;
      Number : out Natural);
   --  Skips any separators and parses Source upto the first character that
   --  is not a decimal digit. Returns value of parsed digits or 0 if none.

   procedure Parse_X_Filename (File : in out ALI_File);
   --  Reads and processes "X..." lines in the ALI file
   --  and updates the File.X_File information.

   ----------------
   -- Add_Entity --
   ----------------

   procedure Add_Entity
     (Pattern : in out Search_Pattern;
      Entity  : in String;
      Glob    : in Boolean := False)
   is
      File_Start   : Natural;
      Line_Start   : Natural;
      Col_Start    : Natural;
      Line_Num     : Natural := 0;
      Col_Num      : Natural := 0;
      File_Ref     : File_Reference := Empty_File;
      File_Existed : Boolean;
      Has_Pattern  : Boolean := False;

   begin
      --  Find the end of the first item in Entity (pattern or file?)
      --  If there is no ':', we only have a pattern

      File_Start := Index (Entity, ":");
      if File_Start = 0 then

         --  If the regular expression is invalid, just consider it as a string
         begin
            Pattern.Entity := Compile (Entity, Glob, False);
         exception
            when Error_In_Regexp =>
               --  The basic idea is to insert a \ before every character
               declare
                  Tmp_Regexp : String (1 .. 2 * Entity'Length);
                  Index      : Positive := 1;
               begin
                  for J in Entity'Range loop
                     Tmp_Regexp (Index) := '\';
                     Tmp_Regexp (Index + 1) := Entity (J);
                     Index := Index + 2;
                  end loop;
                  Pattern.Entity := Compile (Tmp_Regexp, True, False);
               end;
         end;

         Set_Default_Match (True);
         return;
      end if;

      --  If there is a dot in the pattern, then it is a file name

      if (Glob and then
             Index (Entity (Entity'First .. File_Start - 1), ".") /= 0)
               or else
                (not Glob
                   and then Index (Entity (Entity'First .. File_Start - 1),
                                   "\.") /= 0)
      then
         Pattern.Entity := Compile (".*", False);
         File_Start     := Entity'First;

      else
         --  If the regular expression is invalid,
         --  just consider it as a string

         begin
            Pattern.Entity :=
              Compile (Entity (Entity'First .. File_Start - 1), Glob, False);

         exception
            when Error_In_Regexp =>

               --  The basic idea is to insert a \ before every character

               declare
                  Tmp_Regexp : String (1 .. 2 * (File_Start - Entity'First));
                  Index      : Positive := 1;

               begin
                  for J in Entity'First .. File_Start - 1 loop
                     Tmp_Regexp (Index) := '\';
                     Tmp_Regexp (Index + 1) := Entity (J);
                     Index := Index + 2;
                  end loop;

                  Pattern.Entity := Compile (Tmp_Regexp, True, False);
               end;
         end;

         File_Start     := File_Start + 1;
         Has_Pattern    := True;
      end if;

      --  Parse the file name

      Line_Start := Index (Entity (File_Start .. Entity'Last), ":");

      --  Check if it was a disk:\directory item (for NT and OS/2)

      if File_Start = Line_Start - 1
        and then Line_Start < Entity'Last
        and then Entity (Line_Start + 1) = '\'
      then
         Line_Start := Index (Entity (Line_Start + 1 .. Entity'Last), ":");
      end if;

      if Line_Start = 0 then
         Line_Start := Entity'Length + 1;

      elsif Line_Start /= Entity'Last then
         Col_Start := Index (Entity (Line_Start + 1 .. Entity'Last), ":");

         if Col_Start = 0 then
            Col_Start := Entity'Last + 1;
         end if;

         if Col_Start > Line_Start + 2 then
            begin
               Line_Num := Natural'Value
                 (Entity (Line_Start + 1 .. Col_Start - 1));
            exception
               when Constraint_Error =>
                  raise Invalid_Argument;
            end;
         end if;

         if Col_Start < Entity'Last then
            begin
               Col_Num := Natural'Value (Entity
                                         (Col_Start + 1 .. Entity'Last));
            exception
               when Constraint_Error =>  raise Invalid_Argument;
            end;
         end if;
      end if;

      Add_File (Entity (File_Start .. Line_Start - 1),
                File_Existed,
                File_Ref,
                Visited => True);
      Add_Line (File_Ref, Line_Num, Col_Num);
      Add_File
        (ALI_File_Name (Entity (File_Start .. Line_Start - 1)),
         File_Existed, File_Ref,
         Visited => False,
         Emit_Warning => True);
   end Add_Entity;

   --------------
   -- Add_File --
   --------------

   procedure Add_File (File : String) is
      File_Ref     : File_Reference := Empty_File;
      File_Existed : Boolean;
      Iterator     : Expansion_Iterator;

      procedure Add_File_Internal (File : String);
      --  Do the actual addition of the file

      -----------------------
      -- Add_File_Internal --
      -----------------------

      procedure Add_File_Internal (File : String) is
      begin
         --  Case where we have an ALI file, accept it even though this is
         --  not official usage, since the intention is obvious

         if Tail (File, 4) = ".ali" then
            Add_File
              (File,
               File_Existed,
               File_Ref,
               Visited => False,
               Emit_Warning => True);

         --  Normal non-ali file case

         else
            Add_File
              (File,
               File_Existed,
               File_Ref,
               Visited => True);

            Add_File
              (ALI_File_Name (File),
               File_Existed,
               File_Ref,
               Visited => False,
               Emit_Warning => True);
         end if;
      end Add_File_Internal;

   begin
      --  Check if we need to do the expansion

      if Ada.Strings.Fixed.Index (File, "*") /= 0
        or else Ada.Strings.Fixed.Index (File, "?") /= 0
      then

         Start_Expansion (Iterator, File);

         loop
            declare
               S : constant String := Expansion (Iterator);

            begin
               exit when S'Length = 0;
               Add_File_Internal (S);
            end;
         end loop;

      else
         Add_File_Internal (File);
      end if;
   end Add_File;

   -----------------------
   -- Current_Xref_File --
   -----------------------

   function Current_Xref_File
     (File : ALI_File)
      return File_Reference
   is
   begin
      return File.X_File;
   end Current_Xref_File;

   --------------------------
   -- Default_Project_File --
   --------------------------

   function Default_Project_File
     (Dir_Name : String)
      return     String
   is
      My_Dir  : Dir_Type;
      Dir_Ent : File_Name_String;
      Last    : Natural;

   begin
      Open (My_Dir, Dir_Name);

      loop
         Read (My_Dir, Dir_Ent, Last);
         exit when Last = 0;

         if Tail (Dir_Ent (1 .. Last), 4) = ".adp" then
            --  The first project file found is the good one.
            Close (My_Dir);
            return Dir_Ent (1 .. Last);
         end if;
      end loop;

      Close (My_Dir);
      return String'(1 .. 0 => ' ');

   exception
      when Directory_Error => return String'(1 .. 0 => ' ');
   end Default_Project_File;

   -----------------------
   -- End_Of_Line_Index --
   -----------------------

   function End_Of_Line_Index (File : ALI_File) return Integer is
      Index : Integer := File.Current_Line;
   begin
      while Index <= File.Buffer'Last
        and then File.Buffer (Index) /= ASCII.LF
      loop
         Index := Index + 1;
      end loop;
      return Index;
   end End_Of_Line_Index;

   ---------------
   -- File_Name --
   ---------------

   function File_Name
     (File : ALI_File;
      Num  : Positive)
      return File_Reference
   is
   begin
      return File.Dep (Num);
   end File_Name;

   --------------------
   -- Find_ALI_Files --
   --------------------

   procedure Find_ALI_Files is
      My_Dir       : Rec_DIR;
      Dir_Ent      : File_Name_String;
      Last         : Natural;
      File_Existed : Boolean;
      File_Ref     : File_Reference;

      function Open_Next_Dir return Boolean;
      --  Tries to open the next object directory, and return False if
      --  the directory cannot be opened.

      -------------------
      -- Open_Next_Dir --
      -------------------

      function Open_Next_Dir return Boolean is
      begin
         --  Until we are able to open a new directory

         loop
            declare
               Obj_Dir : constant String := Next_Obj_Dir;
            begin
               --  If there was no more Obj_Dir line

               if Obj_Dir'Length = 0 then
                  return False;
               end if;

               Open (My_Dir.Dir, Obj_Dir);
               exit;

            exception
               --  Could not open the directory
               when Directory_Error => null;
            end;
         end loop;

         return True;
      end Open_Next_Dir;

   --  Start of processing for Find_ALI_Files

   begin
      if Open_Next_Dir then
         loop
            Read (My_Dir.Dir, Dir_Ent, Last);

            if Last = 0 then
               Close (My_Dir.Dir);

               if not Open_Next_Dir then
                  return;
               end if;

            elsif Last > 4 and then Dir_Ent (Last - 3 .. Last) = ".ali" then
               Add_File (Dir_Ent (1 .. Last), File_Existed, File_Ref,
                  Visited => False);
               Set_Directory (File_Ref, Current_Obj_Dir);
            end if;
         end loop;
      end if;
   end Find_ALI_Files;

   -------------------
   -- Get_Full_Type --
   -------------------

   function Get_Full_Type (Abbrev : Character) return String is
   begin
      case Abbrev is
         when 'A' => return "Array";
         when 'C' => return "Class-wide";
         when 'D' => return "Decimal";
         when 'E' => return "Enum";
         when 'F' => return "Float";
         when 'G' => return "Generic";
         when 'I' => return "Integer";
         when 'L' => return "Block";
         when 'M' => return "Modular";
         when 'N' => return "Number";
         when 'O' => return "Fixed";
         when 'P' => return "Access";
         when 'R' => return "Record";
         when 'S' => return "String";
         when 'T' => return "Task";
         when 'U' => return "Unit";
         when 'W' => return "Protected";
         when 'X' => return "Exception";
         when 'Y' => return "Entry";

         --  There is not enough information in the ALI to show the full
         --  type for private or incomplete type declarations, so print
         --  the word "Private" in those cases. (See 5711-005)

         when '+' => return "Private";

         --  The above should be the only possibilities, but for a
         --  tool like this we don't want to bomb if we find something
         --  else, so just return ??? when we have an unknown Abbrev value

         when others =>
            return "???";
      end case;
   end Get_Full_Type;

   ------------------
   -- Symbol_Match --
   ------------------

   function Match
     (Pattern : Search_Pattern;
      Symbol  : String)
      return    Boolean
   is
   begin
      --  Get the entity name

      return Match (Symbol, Pattern.Entity);
   end Match;


   ----------------------
   -- Parse_X_Filename --
   ----------------------

   procedure Parse_X_Filename
     (File       : in out ALI_File)
   is
      Ali         : String_Access renames File.Buffer;
      Ptr         : Positive renames File.Current_Line;
      File_Nr     : Natural;

   begin
      while Ali (Ptr) = X loop
         --  The current line is the start of a new Xref file section,
         --  which format looks like:
         --     " X 1 debug.ads"

         --  Skip the X and read the file number for the new X_File

         Ptr := Ptr + 1;
         Parse_Number (Ali, Ptr, File_Nr);

         if File_Nr > 0 then
            File.X_File := File.Dep (File_Nr);
         end if;

         Parse_EOL (Ali, Ptr);
      end loop;

   end Parse_X_Filename;

   ----------
   -- Open --
   ----------

   procedure Open
     (Name         : String;
      File         : out ALI_File;
      Dependencies : Boolean := False)
   is
      Name_0           : constant String := Name & ASCII.NUL;
      Num_Dependencies : Natural := 0;
      File_Existed     : Boolean;
      File_Ref         : File_Reference;
      FD               : File_Descriptor;
      Success          : Boolean := False;

      Ali              : String_Access renames File.Buffer;
      Token,
      Ptr              : Positive;

   begin
      if File.Buffer /= null then
         Free (File.Buffer);
      end if;

      FD := Open_Read (Name_0'Address, Binary);

      if FD = Invalid_FD then
         raise No_Xref_Information;
      end if;

      Read_File (FD, Ali, Success);
      Close (FD);

      Ptr := Ali'First;

      --  Read all the lines possibly processing with-clauses and dependency
      --  information and exit on finding the first Xref line.
      --  A fall-through of the loop means that there is no xref information
      --  which is an error condition.

      while Ali (Ptr) /= EOF loop

         if Ali (Ptr) = D then
            --  Found dependency information. Format looks like:
            --     "D debug.ads             19990120020048 5A7C8C19"

            --  Skip the D and parse the filename

            Ptr := Ptr + 1;
            Parse_Token (Ali, Ptr, Token);

            Num_Dependencies := Num_Dependencies + 1;

            Add_File
              (Ali (Token .. Ptr - 1),
               File_Existed,
               File.Dep (Num_Dependencies));

         elsif Dependencies and then Ali (Ptr) = W then
            --  Found with-clause information. Format looks like:
            --     "W debug%s               debug.adb               debug.ali"

            --  Skip the W and parse the the .ali filename (3rd token)

            Parse_Token (Ali, Ptr, Token);
            Parse_Token (Ali, Ptr, Token);
            Parse_Token (Ali, Ptr, Token);

            Add_File
              (Ali (Token .. Ptr - 1),
               File_Existed, File_Ref,
               Visited => False);

         elsif Ali (Ptr) = X then
            --  Found a cross-referencing line - stop processing

            File.Current_Line := Ptr;
            File.Xref_Line    := Ptr;
            return;
         end if;

         Parse_EOL (Ali, Ptr);
      end loop;

      raise No_Xref_Information;
   end Open;

   ---------------
   -- Parse_EOL --
   ---------------

   procedure Parse_EOL (Source : access String; Ptr : in out Positive) is
   begin
      --  Skip to end of line

      while Source (Ptr) /= ASCII.CR and then Source (Ptr) /= ASCII.LF
        and then Source (Ptr) /= EOF
      loop
         Ptr := Ptr + 1;
      end loop;

      if Source (Ptr) /= EOF then
         Ptr := Ptr + 1;      -- skip CR or LF
      end if;

      --  Skip past CR/LF or LF/CR combination

      if (Source (Ptr) = ASCII.CR or else Source (Ptr) = ASCII.LF)
         and then Source (Ptr) /= Source (Ptr - 1)
      then
         Ptr := Ptr + 1;
      end if;
   end Parse_EOL;

   ------------------
   -- Parse_Number --
   ------------------

   procedure Parse_Number
     (Source    : access String;
      Ptr       : in out Positive;
      Number    : out Natural)
   is
   begin
      --  Skip separators

      while Source (Ptr) = ' ' or else Source (Ptr) = ASCII.HT loop
         Ptr := Ptr + 1;
      end loop;

      Number := 0;
      while Source (Ptr) in '0' .. '9' loop
         Number := 10 * Number
           + (Character'Pos (Source (Ptr)) - Character'Pos ('0'));
         Ptr := Ptr + 1;
      end loop;
   end Parse_Number;

   -----------------
   -- Parse_Token --
   -----------------

   procedure Parse_Token
     (Source    : access String;
      Ptr       : in out Positive;
      Token_Ptr : out Positive)
   is
      In_Quotes : Boolean := False;

   begin
      --  Skip separators

      while Source (Ptr) = ' ' or else Source (Ptr) = ASCII.HT loop
         Ptr := Ptr + 1;
      end loop;

      Token_Ptr := Ptr;

      --  Find end-of-token

      while (In_Quotes or else
               not (Source (Ptr) = ' '
                     or else Source (Ptr) = ASCII.HT
                     or else Source (Ptr) = '<'))
        and then Source (Ptr) >= ' '
      loop
         if Source (Ptr) = '"' then
            In_Quotes := not In_Quotes;
         end if;

         Ptr := Ptr + 1;
      end loop;
   end Parse_Token;

   --------------------
   -- Print_Gnatfind --
   --------------------

   procedure Print_Gnatfind
     (References     : in Boolean;
      Full_Path_Name : in Boolean)
   is

      Decl : Declaration_Reference := First_Declaration;
      Ref1 : Reference;
      Ref2 : Reference;

      procedure Print_Ref
        (Ref : Reference;
         Msg : String := "      ");
      --  Print a reference, according to the extended tag of the output

      ---------------
      -- Print_Ref --
      ---------------

      procedure Print_Ref
        (Ref : Reference;
         Msg : String := "      ")
      is
         Buffer : constant String := Get_File (Ref, Full_Path_Name)
           & ":" & Get_Line (Ref)
           & ":" & Get_Column (Ref)
           & ": ";
         Num_Blanks : Integer
           := Longest_File_Name + 10 - Buffer'Length;
      begin
         Num_Blanks := Integer'Max (0, Num_Blanks);
         Write_Line
           (Buffer
            & String'(1 .. Num_Blanks => ' ')
            & Msg & " " & Get_Symbol (Decl));
         if Get_Source_Line (Ref)'Length /= 0 then
            Write_Line ("   " & Get_Source_Line (Ref));
         end if;
      end Print_Ref;

   --  Start of processing for Print_Gnatfind

   begin
      while Decl /= Empty_Declaration loop
         if Match (Decl) then

            --  Output the declaration

            declare
               Parent : constant Declaration_Reference := Get_Parent (Decl);
               Buffer : constant String
                 := Get_File (Decl, Full_Path_Name)
                 & ":" & Get_Line (Decl)
                 & ":" & Get_Column (Decl)
                 & ": ";
               Num_Blanks : Integer
                 := Longest_File_Name + 10 - Buffer'Length;
            begin
               Num_Blanks := Integer'Max (0, Num_Blanks);
               Write_Line
                 (Buffer & String'(1 .. Num_Blanks => ' ')
                  & "(spec) " & Get_Symbol (Decl));

               if Parent /= Empty_Declaration then
                  Write_Line (Buffer & String'(1 .. Num_Blanks => ' ')
                              & "   derived from " & Get_Symbol (Parent)
                              & " (" & Get_File (Parent)
                              & ':' & Get_Line (Parent)
                              & ':' & Get_Column (Parent) & ')');
               end if;
            end;

            if Get_Source_Line (Decl)'Length /= 0 then
               Write_Line ("   " & Get_Source_Line (Decl));
            end if;

            --  Output the body (sorted)

            Ref1 := First_Body (Decl);
            while Ref1 /= Empty_Reference loop
               Print_Ref (Ref1, "(body)");
               Ref1 := Next (Ref1);
            end loop;

            if References then
               Ref1 := First_Modif (Decl);
               Ref2 := First_Reference (Decl);
               while Ref1 /= Empty_Reference
                       or else
                     Ref2 /= Empty_Reference
               loop
                  if Compare (Ref1, Ref2) = LessThan then
                     Print_Ref (Ref1);
                     Ref1 := Next (Ref1);
                  else
                     Print_Ref (Ref2);
                     Ref2 := Next (Ref2);
                  end if;
               end loop;
            end if;
         end if;

         Decl := Next (Decl);
      end loop;
   end Print_Gnatfind;

   --------------
   -- Print_Vi --
   --------------

   procedure Print_Vi (Full_Path_Name : in Boolean) is

      Tab  : constant Character := ASCII.HT;
      Decl : Declaration_Reference := First_Declaration;
      Ref  : Reference;

   begin
      while Decl /= Empty_Declaration loop
         Write_Line (Get_Symbol (Decl) & Tab
                            & Get_File (Decl, Full_Path_Name) & Tab
                            & Get_Line (Decl));

         --  Print the body if any

         Ref := First_Body (Decl);

         if Ref /= Empty_Reference then
            Write_Line (Get_Symbol (Decl) & Tab
                               & Get_File (Ref, Full_Path_Name)
                               & Tab
                               & Get_Line (Ref));
         end if;

         --  Print the modifications

         Ref := First_Modif (Decl);

         while Ref /= Empty_Reference loop
            Write_Line (Get_Symbol (Decl) & Tab
                               & Get_File (Ref, Full_Path_Name)
                               & Tab
                               & Get_Line (Ref));
            Ref := Next (Ref);
         end loop;

         Decl := Next (Decl);
      end loop;
   end Print_Vi;

   ----------------
   -- Print_Xref --
   ----------------

   procedure Print_Xref (Full_Path_Name : in Boolean) is

      Decl   : Declaration_Reference := First_Declaration;
      Ref    : Reference;
      File   : File_Reference;

      Margin : constant := 10;
      --  Column where file names start

      procedure New_Line80;
      --  Go to start of new line

      procedure Print80 (S : in String);
      --  Print the text, respecting the 80 columns rule.

      procedure PrintRef (Line, Column : String);
      --  The beginning of the output is aligned on a column multiple of 9

      procedure New_Line80 is
      begin
         Write_Eol;
         Write_Str (String'(1 .. Margin - 1 => ' '));
      end New_Line80;

      procedure Print80 (S : in String) is
         Align : Natural := Margin - (Integer (Column) mod Margin);
      begin
         if Align = Margin then
            Align := 0;
         end if;

         Write_Str (String'(1 .. Align => ' ') & S);
      end Print80;

      procedure PrintRef (Line, Column : String) is
         Line_Align : constant Integer := 4 - Line'Length;

         S : constant String := String'(1 .. Line_Align => ' ')
                                  & Line & ':' & Column;

         Align : Natural := Margin - (Integer (Output.Column) mod Margin);

      begin
         if Align = Margin then
            Align := 0;
         end if;

         if Integer (Output.Column) + Align + S'Length > 79 then
            New_Line80;
            Align := 0;
         end if;

         Write_Str (String'(1 .. Align => ' ') & S);
      end PrintRef;

   --  Start of processing for Print_Xref

   begin
      while Decl /= Empty_Declaration loop
         Write_Str (Get_Symbol (Decl));
         Write_Str (String'(1 .. 62 - Integer (Column) => ' '));
         Write_Line
           ("Type: " & Get_Full_Type (Get_Type (Decl)(1)));

         Write_Parent_Info : declare
            Parent : constant Declaration_Reference := Get_Parent (Decl);
         begin
            if Parent /= Empty_Declaration then
               Write_Str ("  Ptype: ");
               Print80 (Get_File (Parent));
               PrintRef (Get_Line (Parent), Get_Column (Parent));
               Print80 ("  " & Get_Symbol (Parent));
               Write_Eol;
            end if;
         end Write_Parent_Info;

         Write_Str ("  Decl:  ");
         Print80 (Get_File (Decl, Full_Path_Name) & ' ');
         PrintRef (Get_Line (Decl), Get_Column (Decl));

         --  Print the body if any

         Ref := First_Body (Decl);

         if Ref /= Empty_Reference then
            Write_Eol;
            Write_Str ("  Body:  ");
            Print80 (Get_File (Ref, Full_Path_Name) & ' ');
            PrintRef (Get_Line (Ref), Get_Column (Ref));
         end if;

         --  Print the modifications if any

         Ref := First_Modif (Decl);

         if Ref /= Empty_Reference then
            Write_Eol;
            Write_Str ("  Modi:  ");
         end if;

         File := Empty_File;

         while Ref /= Empty_Reference loop
            if Get_File_Ref (Ref) /= File then
               if File /= Empty_File then
                  New_Line80;
               end if;

               File := Get_File_Ref (Ref);
               Write_Str
                 (Get_File (Ref, Full_Path_Name) & ' ');
               PrintRef (Get_Line (Ref), Get_Column (Ref));

            else
               PrintRef (Get_Line (Ref), Get_Column (Ref));
            end if;

            Ref := Next (Ref);
         end loop;

         --  Print the references

         Ref := First_Reference (Decl);

         if Ref /= Empty_Reference then
            Write_Eol;
            Write_Str ("  Ref:   ");
         end if;

         File := Empty_File;

         while Ref /= Empty_Reference loop
            if Get_File_Ref (Ref) /= File then
               if File /= Empty_File then
                  New_Line80;
               end if;

               File := Get_File_Ref (Ref);
               Write_Str
                 (Get_File (Ref, Full_Path_Name) & ' ');
               PrintRef (Get_Line (Ref), Get_Column (Ref));

            else
               PrintRef (Get_Line (Ref), Get_Column (Ref));
            end if;

            Ref := Next (Ref);
         end loop;

         Write_Eol;
         Decl := Next (Decl);
      end loop;
   end Print_Xref;

   ------------------
   -- Print_Unused --
   ------------------

   procedure Print_Unused (Full_Path_Name : in Boolean) is

      Decl : Declaration_Reference := First_Declaration;
      Ref  : Reference;

   begin
      while Decl /= Empty_Declaration loop
         if First_Modif (Decl) = Empty_Reference
           and then First_Reference (Decl) = Empty_Reference
         then
            Write_Str (Get_Symbol (Decl)
                      & " "
                      & Get_Type (Decl)
                      & " "
                      & Get_File (Decl, Full_Path_Name)
                      & ':'
                      & Get_Line (Decl)
                      & ':'
                      & Get_Column (Decl));

            --  Print the body if any

            Ref := First_Body (Decl);

            if Ref /= Empty_Reference then
               Write_Line (' '
                          & Get_File (Ref, Full_Path_Name)
                          & ':' & Get_Line (Ref)
                          & ':' & Get_Column (Ref));
            else
               Write_Eol;
            end if;
         end if;

         Decl := Next (Decl);
      end loop;
   end Print_Unused;


   ---------------------------
   -- Parse_Identifier_Info --
   ---------------------------

   procedure Parse_Identifier_Info
     (Pattern       : Search_Pattern;
      File          : in out ALI_File;
      Local_Symbols : Boolean;
      Der_Info      : Boolean := False;
      Type_Tree     : Boolean := False;
      Wide_Search   : Boolean := True)
   is
      Ptr      : Positive renames File.Current_Line;
      Ali      : String_Access renames File.Buffer;

      E_Line   : Natural;   --  Line number of current entity
      E_Col    : Natural;   --  Column number of current entity
      E_Type   : Character; --  Type of current entity
      E_Name   : Positive;  --  Pointer to begin of entity name
      E_Global : Boolean;   --  True iff entity is global

      R_Line   : Natural;   --  Line number of current reference
      R_Col    : Natural;   --  Column number of current reference
      R_Type   : Character; --  Type of current reference

      Decl_Ref : Declaration_Reference;
      File_Ref : File_Reference := Current_Xref_File (File);

      function Get_Symbol_Name (Eun, Line, Col : Natural) return String;
      --  Returns the symbol name for the entity defined at the specified
      --  line and column in the dependent unit number Eun. For this we need
      --  to parse the ali file again because the parent entity is not in
      --  the declaration table if it did not match the search pattern.

      ---------------------
      -- Get_Symbol_Name --
      ---------------------

      function Get_Symbol_Name (Eun, Line, Col : Natural) return String is
         Ptr    : Positive := 1;
         E_Eun  : Positive;   --  Unit number of current entity
         E_Line : Natural;    --  Line number of current entity
         E_Col  : Natural;    --  Column number of current entity
         E_Name : Positive;   --  Pointer to begin of entity name
         E_Type : Character;  --  Type of current entity

         procedure Skip_Line;
         --  skip current line and continuation line

         procedure Skip_Line is
         begin
            loop
               Parse_EOL (Ali, Ptr);
               exit when Ali (Ptr) /= '.';
            end loop;
         end Skip_Line;

      --  Start of processing for Get_Symbol_Name

      begin
         --  Look for the X lines corresponding to unit Eun

         loop
            if Ali (Ptr) = 'X' then
               Ptr := Ptr + 1;
               Parse_Number (Ali, Ptr, E_Eun);
               exit when E_Eun = Eun;
            end if;

            Skip_Line;
         end loop;

         --  Here we are in the right Ali section, we now look for the entity
         --  declared at position (Line, Col).

         loop
            Parse_Number (Ali, Ptr, E_Line);
            E_Type := Ali (Ptr);
            Ptr := Ptr + 1;
            Parse_Number (Ali, Ptr, E_Col);
            Ptr := Ptr + 1;

            if Line = E_Line and then Col = E_Col then
               Parse_Token (Ali, Ptr, E_Name);
               return Ali (E_Name .. Ptr - 1);
            end if;

            Skip_Line;
         end loop;

         --  We were not able to find the symbol, this should not happend but
         --  since we don't want to stop here we return a string of three
         --  question marks as the symbol name.

         return "???";
      end Get_Symbol_Name;

   --  Start of processing for Parse_Identifier_Info

   begin
      --  The identifier info looks like:
      --     "38U9*Debug 12|36r6 36r19"

      --  Extract the line, column and entity name information

      Parse_Number (Ali, Ptr, E_Line);

      if Ali (Ptr) > ' ' then
         E_Type := Ali (Ptr);
         Ptr := Ptr + 1;
      end if;

      Parse_Number (Ali, Ptr, E_Col);

      E_Global := False;
      if Ali (Ptr) >= ' ' then
         E_Global := (Ali (Ptr) = '*');
         Ptr := Ptr + 1;
      end if;

      Parse_Token (Ali, Ptr, E_Name);

      --  Exit if the symbol does not match
      --  or if we have a local symbol and we do not want it

      if (not Local_Symbols and not E_Global)
                or else
         not Match (Pattern, Ali (E_Name .. Ptr - 1))
                or else
              (E_Name >= Ptr)
      then
         --  Skip rest of this line and all continuation lines
         loop
            Parse_EOL (Ali, Ptr);
            exit when Ali (Ptr) /= '.';
         end loop;
         return;
      end if;

      --  Insert the declaration in the table

      Decl_Ref := Add_Declaration
        (File.X_File, Ali (E_Name .. Ptr - 1), E_Line, E_Col, E_Type);

      if Ali (Ptr) = '<' then
         --  Here we have a type derivation information. The format is
         --  <3|12I45> which means that the current entity is derived from the
         --  type defined in unit number 3, line 12 column 45. The pipe and
         --  unit number is optional. It is specified only if the parent type
         --  is not defined in the current unit.

         Ptr := Ptr + 1;

         Parse_Derived_Info : declare
            P_Line     : Natural;          --  parent entity line
            P_Column   : Natural;          --  parent entity column
            P_Type     : Character;        --  parent entity type
            P_Eun      : Positive;         --  parent entity file number

         begin
            Parse_Number (Ali, Ptr, P_Line);

            --  If we have a pipe then the first number was the unit number

            if Ali (Ptr) = '|' then
               P_Eun := P_Line;
               Ptr := Ptr + 1;

               --  Now we have the line number

               Parse_Number (Ali, Ptr, P_Line);
            else
               --  We don't have a unit number specified, so we set P_Eun to
               --  the current unit.

               for K in File.Dep'Range loop
                  P_Eun := K;
                  exit when File.Dep (K) = File_Ref;
               end loop;
            end if;

            --  Then parse the type and column number

            P_Type := Ali (Ptr);
            Ptr := Ptr + 1;
            Parse_Number (Ali, Ptr, P_Column);

            --  skip '>'

            Ptr := Ptr + 1;

            --  The derived info is needed only is the derived info mode is on
            --  or if we want to output the type hierarchy

            if Der_Info or else Type_Tree then
               Add_Parent
                 (Decl_Ref,
                  Get_Symbol_Name (P_Eun, P_Line, P_Column),
                  P_Line,
                  P_Column,
                  File.Dep (P_Eun));
            end if;

            if Type_Tree then
               Search_Parent_Tree : declare
                  Pattern         : Search_Pattern;  --  Parent type pattern
                  File_Pos_Backup : Positive;
               begin
                  Add_Entity (Pattern,
                              Get_Symbol_Name (P_Eun, P_Line, P_Column)
                              & ':' & Get_File (File.Dep (P_Eun))
                              & ':' & Get_Line (Get_Parent (Decl_Ref))
                              & ':' & Get_Column (Get_Parent (Decl_Ref)),
                              False);

                  --  No default match is needed to look for the parent type
                  --  since we are using the fully qualified symbol name:
                  --  symbol:file:line:column

                  Set_Default_Match (False);

                  --  The parent type is defined in the same unit as the
                  --  derived type. So we want to revisit the unit.

                  File_Pos_Backup   := File.Current_Line;

                  if File.Dep (P_Eun) = File_Ref then

                     --  set file pointer at the start of the xref lines

                     File.Current_Line := File.Xref_Line;

                     Revisit_ALI_File : declare
                        File_Existed : Boolean;
                        File_Ref     : File_Reference;
                     begin
                        Add_File (ALI_File_Name (Get_File (File.Dep (P_Eun))),
                                  File_Existed,
                                  File_Ref,
                                  Visited => False);
                        Set_Unvisited (File_Ref);
                     end Revisit_ALI_File;
                  end if;

                  Search (Pattern,
                          Local_Symbols, False, False, Der_Info, Type_Tree);

                  File.Current_Line := File_Pos_Backup;

                  --  in this mode there is no need to parse the remaining of
                  --  the lines.

                  return;
               end Search_Parent_Tree;
            end if;
         end Parse_Derived_Info;
      end if;

      --  To find the body, we will have to parse the file too

      if Wide_Search then
         declare
            File_Existed : Boolean;
            File_Ref     : File_Reference;
            File_Name    : constant String := Get_File (File.X_File);
         begin
            Add_File (ALI_File_Name (File_Name),
               File_Existed, File_Ref, False);
         end;
      end if;

      --  Parse references to this entity.
      --  Ptr points to next reference with leading blanks

      loop
         --  Process references on current line

         while Ali (Ptr) = ' ' or Ali (Ptr) = ASCII.HT loop

            --  For every reference read the line, type and column,
            --  optionally preceded by a file number and a pipe symbol.

            Parse_Number (Ali, Ptr, R_Line);

            if Ali (Ptr) = Pipe then
               Ptr := Ptr + 1;
               File_Ref := File_Name (File, R_Line);

               Parse_Number (Ali, Ptr, R_Line);
            end if;

            if Ali (Ptr) > ' ' then
               R_Type := Ali (Ptr);
               Ptr := Ptr + 1;
            end if;

            Parse_Number (Ali, Ptr, R_Col);

            --  Insert the reference or body in the table

            Add_Reference (Decl_Ref, File_Ref, R_Line, R_Col, R_Type);

         end loop;

         Parse_EOL (Ali, Ptr);

         --   Loop until new line is no continuation line

         exit when Ali (Ptr) /= '.';
         Ptr := Ptr + 1;
      end loop;
   end Parse_Identifier_Info;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_File
     (FD       : File_Descriptor;
      Contents : out String_Access;
      Success  : out Boolean)
   is
      Length : constant File_Offset := File_Offset (File_Length (FD));
      --  Include room for EOF char

      Buffer : String (1 .. Length + 1);

      This_Read : Integer;
      Read_Ptr  : File_Offset := 1;

   begin

      loop
         This_Read := Read (FD,
           A => Buffer (Read_Ptr)'Address,
           N => Length + 1 - Read_Ptr);
         Read_Ptr := Read_Ptr + Integer'Max (This_Read, 0);
         exit when This_Read <= 0;
      end loop;

      Buffer (Read_Ptr) := EOF;
      Contents := new String'(Buffer (1 .. Read_Ptr));

      --  Things aren't simple on VMS due to the plethora of file types
      --  and organizations. It seems clear that there shouldn't be more
      --  bytes read than are contained in the file though.

      if Hostparm.OpenVMS then
         Success := Read_Ptr <= Length + 1;
      else
         Success := Read_Ptr = Length + 1;
      end if;
   end Read_File;

   ------------
   -- Search --
   ------------

   procedure Search
     (Pattern       : Search_Pattern;
      Local_Symbols : Boolean;
      Wide_Search   : Boolean;
      Read_Only     : Boolean;
      Der_Info      : Boolean;
      Type_Tree     : Boolean)
   is
      ALIfile    : ALI_File;
      File_Ref   : File_Reference;

   begin
      --  If we want all the .ali files, then find them

      if Wide_Search then
         Find_ALI_Files;
      end if;

      loop
         --  Get the next unread ali file

         File_Ref := Next_Unvisited_File;

         exit when File_Ref = Empty_File;

         if not File_Exists (Get_File (File_Ref, With_Dir => True)) then

            if Get_Emit_Warning (File_Ref) then
               Set_Standard_Error;
               Write_Line
                 ("warning : file "
                  & Get_File (File_Ref, With_Dir => True)
                  & " not found");
               Set_Standard_Output;
            end if;

         elsif Read_Only
           or else Is_Writable_File (Get_File (File_Ref, With_Dir => True))
         then
            begin
               Open (Get_File (File_Ref, With_Dir => True), ALIfile);
               while ALIfile.Buffer (ALIfile.Current_Line) /= EOF loop
                  Parse_X_Filename (ALIfile);
                  Parse_Identifier_Info (Pattern, ALIfile, Local_Symbols,
                     Der_Info, Type_Tree, Wide_Search);
               end loop;

            exception
               when No_Xref_Information   =>
                  if Get_Emit_Warning (File_Ref) then
                     Set_Standard_Error;
                     Write_Line
                       ("warning : No cross-referencing information in  "
                        & Get_File (File_Ref, With_Dir => True));
                     Set_Standard_Output;
                  end if;
            end;
         end if;
      end loop;
   end Search;

   -----------------
   -- Search_Xref --
   -----------------

   procedure Search_Xref
     (Pattern       : Search_Pattern;
      Local_Symbols : Boolean;
      Read_Only     : Boolean;
      Der_Info      : Boolean)
   is

      ALIfile    : ALI_File;
      File_Ref   : File_Reference;

   begin
      loop
         --  Find the next unvisited file

         File_Ref := Next_Unvisited_File;
         exit when File_Ref = Empty_File;

         --  Search the object directories for the .ali file

         if Read_Only
           or else Is_Writable_File (Get_File (File_Ref, With_Dir => True))
         then
            begin
               Open (Get_File (File_Ref, With_Dir => True), ALIfile, True);

               while ALIfile.Buffer (ALIfile.Current_Line) /= EOF loop
                  Parse_X_Filename (ALIfile);
                  Parse_Identifier_Info (Pattern, ALIfile, Local_Symbols,
                                         Der_Info);
               end loop;

            exception
               when No_Xref_Information =>  null;
            end;
         end if;
      end loop;
   end Search_Xref;

end Xref_Lib;
