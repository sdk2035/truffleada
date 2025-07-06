------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            G N A T . D I R E C T O R Y _ O P E R A T I O N S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.9 $
--                                                                          --
--            Copyright (C) 1998-2000 Ada Core Technologies, Inc.           --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Unchecked_Deallocation;
with Unchecked_Conversion;
with System;  use System;

package body GNAT.Directory_Operations is

   type Dir_Type_Value is new System.Address;

   procedure Free is new
     Unchecked_Deallocation (Dir_Type_Value, Dir_Type);

   ----------------
   -- Change_Dir --
   ----------------

   procedure Change_Dir (Dir_Name : in Dir_Name_Str) is
      C_Dir_Name : String := Dir_Name & ASCII.NUL;

      function chdir (Dir_Name : String) return Integer;
      pragma Import (C, chdir, "chdir");

   begin
      if chdir (C_Dir_Name) /= 0 then
         raise Directory_Error;
      end if;
   end Change_Dir;

   ----------------
   -- Create_Dir --
   ----------------

   procedure Make_Dir (Dir_Name : in Dir_Name_Str) is
      C_Dir_Name : String := Dir_Name & ASCII.NUL;

      function mkdir (Dir_Name : String) return Integer;
      pragma Import (C, mkdir, "__gnat_mkdir");

   begin
      if mkdir (C_Dir_Name) /= 0 then
         raise Directory_Error;
      end if;
   end Make_Dir;

   -----------
   -- Close --
   -----------

   procedure Close (Dir : in out Dir_Type) is

      function closedir (Directory : System.Address) return Integer;
      pragma Import (C, closedir, "closedir");

      Discard : Integer;

   begin
      if not Is_Open (Dir) then
         raise Directory_Error;
      end if;

      Discard := closedir (System.Address (Dir.all));
      Free (Dir);
   end Close;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir return Dir_Name_Str is
      Max_Path : Integer;
      pragma Import (C, Max_Path, "max_path_len");

      Current_Dir : String (1 .. Max_Path + 1);
      Last        : Natural;

   begin
      Get_Current_Dir (Current_Dir, Last);
      return Current_Dir (1 .. Last);
   end Get_Current_Dir;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   procedure Get_Current_Dir (Dir : out Dir_Name_Str; Last : out Natural) is
      Max_Path : Integer;
      pragma Import (C, Max_Path, "max_path_len");

      Path_Len : Natural := Max_Path;

      Buffer   : String (Dir'First .. Dir'First + Max_Path + 1);

      procedure Local_Get_Current_Dir
        (Dir    : System.Address;
         Length : System.Address);
      pragma Import (C, Local_Get_Current_Dir, "Get_Current_Dir");

   begin
      Local_Get_Current_Dir (Buffer'Address, Path_Len'Address);

      if Dir'Length > Path_Len then
         Last := Dir'First + Path_Len - 1;
      else
         Last := Dir'Last;
      end if;

      Dir (Buffer'First .. Last) := Buffer (Buffer'First .. Last);
   end Get_Current_Dir;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (Dir : in Dir_Type) return Boolean is
   begin
      return Dir /= Null_Dir
        and then System.Address (Dir.all) /= System.Null_Address;
   end Is_Open;

   ----------
   -- Open --
   ----------

   procedure Open
     (Dir      : out Dir_Type;
      Dir_Name : in  Dir_Name_Str)
   is
      C_File_Name : String := Dir_Name & ASCII.NUL;

      function opendir
        (File_Name : String)
         return      Dir_Type_Value;
      pragma Import (C, opendir, "opendir");

   begin
      Dir := new Dir_Type_Value'(opendir (C_File_Name));

      if not Is_Open (Dir) then
         Free (Dir);
         Dir := Null_Dir;
         raise Directory_Error;
      end if;
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (Dir  : in out Dir_Type;
      Str  : out String;
      Last : out Natural)
   is
      Filename_Addr : Address;
      Filename_Len  : Integer;

      Buffer : array (0 .. 1024) of Character;
      --  1024 is the value of FILENAME_MAX in stdio.h

      function readdir_gnat
         (Directory : System.Address;
          Buffer    : System.Address)
          return      System.Address;
      pragma Import (C, readdir_gnat, "readdir_gnat");

      function strlen (S : Address) return Integer;
      pragma Import (C, strlen, "strlen");

   begin
      if not Is_Open (Dir) then
         raise Directory_Error;
      end if;

      Filename_Addr :=
        readdir_gnat (System.Address (Dir.all), Buffer'Address);

      if Filename_Addr = System.Null_Address then
         Last := 0;
         return;
      end if;

      Filename_Len  := strlen (Filename_Addr);

      if Str'Length > Filename_Len then
         Last := Str'First + Filename_Len - 1;
      else
         Last := Str'Last;
      end if;

      declare
         subtype Path_String is String (1 .. Filename_Len);
         type    Path_String_Access is access Path_String;

         function Address_To_Access is new
           Unchecked_Conversion
             (Source => Address,
              Target => Path_String_Access);

         Path_Access : Path_String_Access :=
                         Address_To_Access (Filename_Addr);

      begin
         for J in Str'First .. Last loop
            Str (J) := Path_Access (J - Str'First + 1);
         end loop;
      end;
   end Read;

   -------------------------
   -- Read_Is_Thread_Sage --
   -------------------------

   function Read_Is_Thread_Safe return Boolean is

      function readdir_is_thread_safe return Integer;
      pragma Import (C, readdir_is_thread_safe, "readdir_is_thread_safe");

   begin
      return (readdir_is_thread_safe /= 0);
   end Read_Is_Thread_Safe;


end GNAT.Directory_Operations;
