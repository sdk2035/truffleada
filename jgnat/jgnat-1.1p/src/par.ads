------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  P A R                                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.14 $                             --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  The Par function and its subunits contains all the parsing routines
--  for the top down recursive descent parser that constructs the parse tree

with Types; use Types;

function Par (Configuration_Pragmas : Boolean) return List_Id;
--  Top level parsing routine. There are two cases:
--
--  If Configuration_Pragmas is False, Par parses a compilation unit in the
--  current source file and sets the Cunit, Cunit_Entity and Unit_Name fields
--  of the units table entry for Current_Source_Unit. On return the parse tree
--  is complete, and decorated with any required implicit label declarations.
--  The value returned in this case is always No_List.
--
--  If Configuration_Pragmas is True, Par parses a list of configuration
--  pragmas from the current source file, and returns the list of pragmas.
