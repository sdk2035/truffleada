------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 8                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.6 $
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

--  Expand routines for chapter 8 constructs

with Types; use Types;

package Exp_Ch8 is
   procedure Expand_N_Exception_Renaming_Declaration  (N : Node_Id);
   procedure Expand_N_Object_Renaming_Declaration     (N : Node_Id);
   procedure Expand_N_Package_Renaming_Declaration    (N : Node_Id);
   procedure Expand_N_Subprogram_Renaming_Declaration (N : Node_Id);
end Exp_Ch8;
