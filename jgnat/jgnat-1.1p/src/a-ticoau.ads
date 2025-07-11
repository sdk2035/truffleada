------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--              A D A . T E X T _ I O . C O M P L E X _ A U X               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.2 $                              --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
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

--  This package contains the routines for Ada.Text_IO.Complex_IO that are
--  shared among separate instantiations of this package. The routines in
--  this package are identical semantically to those in Complex_IO itself,
--  except that the generic parameter Complex has been replaced by separate
--  real and imaginary values of type Long_Long_Float, and default parameters
--  have been removed because they are supplied explicitly by the calls from
--  within the generic template.

package Ada.Text_IO.Complex_Aux is

   procedure Get
     (File  : in  File_Type;
      ItemR : out Long_Long_Float;
      ItemI : out Long_Long_Float;
      Width : Field);

   procedure Put
     (File  : File_Type;
      ItemR : Long_Long_Float;
      ItemI : Long_Long_Float;
      Fore  : Field;
      Aft   : Field;
      Exp   : Field);

   procedure Gets
     (From  : String;
      ItemR : out Long_Long_Float;
      ItemI : out Long_Long_Float;
      Last  : out Positive);

   procedure Puts
     (To    : out String;
      ItemR : Long_Long_Float;
      ItemI : Long_Long_Float;
      Aft   : Field;
      Exp   : Field);

end Ada.Text_IO.Complex_Aux;
