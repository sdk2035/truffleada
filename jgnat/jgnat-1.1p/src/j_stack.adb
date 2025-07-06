------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J _ S T A C K                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.2 $                             --
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

package body J_Stack is

   Stack   : array (Natural range 1 .. Max_Depth) of Element_Type;
   Stk_Top : Natural := 0;

   ----------
   -- Push --
   ----------

   procedure Push (Elmt : Element_Type) is
   begin
      pragma Assert (Stk_Top < Stack'Last);
      Stk_Top := Stk_Top + 1;
      Stack (Stk_Top) := Elmt;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop is
   begin
      pragma Assert (Stk_Top >= Stack'First);
      Stk_Top := Stk_Top - 1;
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop return Element_Type is
   begin
      pragma Assert (Stk_Top >= Stack'First);
      Stk_Top := Stk_Top - 1;
      return Stack (Stk_Top + 1);
   end Pop;

   ---------
   -- Top --
   ---------

   function Top return Element_Type is
   begin
      pragma Assert (Stk_Top >= Stack'First);
      return Stack (Stk_Top);
   end Top;

   -----------
   -- Empty --
   -----------

   function Empty return Boolean is
   begin
      return Stk_Top = 0;
   end Empty;

end J_Stack;
