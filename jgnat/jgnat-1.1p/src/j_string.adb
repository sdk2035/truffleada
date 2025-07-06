------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J _ S T R I N G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.5 $                             --
--                                                                          --
--           Copyright (C) 1998-1999 Ada Core Technologies, Inc.            --
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

with Osint;   use Osint;
with Sinput;  use Sinput;
with Stringt; use Stringt;
with Types;   use Types;
with Namet;   use Namet;

package body J_String is

   ----------
   -- Name --
   ----------

   function Name (Name : String) return Name_Id is
   begin
      for J in 1 .. Name'Length loop
         Name_Buffer (J) := Name (Name'First + (J - 1));
      end loop;

      Name_Len := Name'Length;
      return Name_Find;
   end Name;

   -----------------
   -- Name_String --
   -----------------

   function Name_String (Name : Name_Id) return String is
   begin
      pragma Assert (Name /= No_Name);

      Get_Name_String (Name);

      return Name_Buffer (1 .. Name_Len);
   end Name_String;

   ------------
   -- Str_Id --
   ------------

   function Str_Id (S : String) return String_Id is
   begin
      for J in 1 .. S'Length loop
         Name_Buffer (J) := S (S'First + (J - 1));
      end loop;

      Name_Len := S'Length;
      return String_From_Name_Buffer;
   end Str_Id;

   ---------
   -- Str --
   ---------

   function Str (Str_Id : String_Id) return String is
   begin
      --  ??? pragma Assert (Str_Id /= No_String);
      if Str_Id = No_String then
         return "";
      end if;

      String_To_Name_Buffer (Str_Id);

      return Name_Buffer (1 .. Name_Len);
   end Str;

   -----------------
   -- Source_Name --
   -----------------

   function Source_Name (Sloc : Source_Ptr) return Name_Id is
   begin
      if Sloc = No_Location or Sloc = Standard_Location then
         return No_Name;
      else
         return Strip_Directory
                  (Debug_Source_Name (Get_Source_File_Index (Sloc)));
      end if;
   end Source_Name;

end J_String;
