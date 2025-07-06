------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 I N T E R F A C E S . C _ S T R E A M S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.9 $
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- JGNAT -  The GNAT Ada 95 tool chain for the Java (TM) Virtual Machine is --
--          maintained by Ada Core Technologies, Inc. - http://www.gnat.com --
--                                                                          --
------------------------------------------------------------------------------

package body Interfaces.C_Streams is

   ------------
   -- fread --
   ------------

   function fread
     (buffer : voids;
      size   : size_t;
      count  : size_t;
      stream : FILEs)
      return   size_t
   is
      function C_fread
        (buffer : voids;
         size   : size_t;
         count  : size_t;
         stream : FILEs)
         return   size_t;
      pragma Import (C, C_fread, "fread");

   begin
      return C_fread (buffer, size, count, stream);
   end fread;

   ------------
   -- fread --
   ------------

   function fread
     (buffer : voids;
      index  : size_t;
      size   : size_t;
      count  : size_t;
      stream : FILEs)
      return   size_t
   is
      function C_fread
        (buffer : voids;
         index  : size_t;
         size   : size_t;
         count  : size_t;
         stream : FILEs)
         return   size_t;
      pragma Import (C, C_fread, "fread");
   begin
      return C_fread (buffer, index, size, count, stream);
   end fread;

   ------------
   -- fwrite --
   ------------

   function fwrite
     (buffer : voids;
      size   : size_t;
      count  : size_t;
      stream : FILEs)
      return   size_t
   is
      function C_fwrite
        (buffer : voids;
         size   : size_t;
         count  : size_t;
         stream : FILEs)
         return   size_t;
      pragma Import (C, C_fwrite, "fwrite");

   begin
      return C_fwrite (buffer, size, count, stream);
   end fwrite;

   -------------
   -- setvbuf --
   -------------

   function setvbuf
     (stream : FILEs;
      buffer : chars;
      mode   : int;
      size   : size_t)
      return   int
   is
      function C_setvbuf
        (stream : FILEs;
         buffer : chars;
         mode   : int;
         size   : size_t)
         return   int;
      pragma Import (C, C_setvbuf, "setvbuf");

   begin
      return C_setvbuf (stream, buffer, mode, size);
   end setvbuf;

end Interfaces.C_Streams;
