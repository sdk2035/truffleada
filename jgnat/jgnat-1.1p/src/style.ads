------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S T Y L E                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.9 $                              --
--                                                                          --
--          Copyright (C) 1992-1998 Free Software Foundation, Inc.          --
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

--  This package collects all the routines used for style checking, as
--  activated by the relevant command line option. These are gathered in
--  a separate package so that they can more easily be customized. Calls
--  to these subprograms are only made if Opt.Style_Check is set True.

with Types; use Types;

package Style is

   procedure Body_With_No_Spec (N : Node_Id);
   --  Called where N is a subprogram body node for a subprogram body
   --  for which no spec was given, i.e. a body acting as its own spec.

   procedure Check_Abs_Not;
   --  Called after scanning an ABS or NOT operator to check spacing

   procedure Check_Arrow;
   --  Called after scanning out an arrow to check spacing

   procedure Check_Attribute_Name (Reserved : Boolean);
   --  The current token is an attribute designator. Check that it is
   --  capitalized in an appropriate manner. Reserved is set if the
   --  attribute designator is a reserved word (access, digits, delta
   --  or range) to allow differing rules for the two cases.

   procedure Check_Box;
   --  Called after scanning out a box to check spacing

   procedure Check_Binary_Operator;
   --  Called after scanning out a binary operator other than a plus, minus
   --  or exponentiation operator. Intended for checking spacing rules.

   procedure Check_Exponentiation_Operator;
   --  Called after scanning out an exponentiation operator. Intended for
   --  checking spacing rules.

   procedure Check_Colon;
   --  Called after scanning out colon to check spacing

   procedure Check_Colon_Equal;
   --  Called after scanning out colon equal to check spacing

   procedure Check_Comma;
   --  Called after scanning out comma to check spacing

   procedure Check_Comment;
   --  Called with Scan_Ptr pointing to the first minus sign of a comment.
   --  Intended for checking any specific rules for comment placement/format.

   procedure Check_Dot_Dot;
   --  Called after scanning out dot dot to check spacing

   procedure Check_HT;
   --  Called with Scan_Ptr pointing to a horizontal tab character

   procedure Check_Identifier (Ref : Node_Id; Def : Node_Id);
   --  Check style of identifier occurrence. Ref is an N_Identifier node whose
   --  spelling is to be checked against the Chars spelling in identifier node
   --  Def (which may be either an N_Identifier, or N_Defining_Identifier node)

   procedure Check_Indentation;
   --  Called at the start of a new statement or declaration, with Token_Ptr
   --  pointing to the first token of the statement or declaration. The check
   --  is that the starting column is appropriate to the indentation rules if
   --  Token_Ptr is the first token on the line.

   procedure Check_Left_Paren;
   --  Called after scanning out a left parenthesis to check spacing.

   procedure Check_Line_Terminator (Len : Int);
   --  Called with Scan_Ptr pointing to the first line terminator terminating
   --  the current line, used to check for appropriate line terminator and
   --  to check the line length (Len is the length of the current line).
   --  Note that the terminator may be the EOF character.

   procedure Check_Pragma_Name;
   --  The current token is a pragma identifier. Check that it is spelled
   --  properly (i.e. with an appropriate casing convention).

   procedure Check_Right_Paren;
   --  Called after scanning out a right parenthesis to check spacing.

   procedure Check_Semicolon;
   --  Called after scanning out a semicolon to check spacing

   procedure Check_Then (If_Loc : Source_Ptr);
   --  Called to check that THEN and IF keywords are appropriately positioned.
   --  The parameters show the first characters of the two keywords. This
   --  procedure is called only if THEN appears at the start of a line with
   --  Token_Ptr pointing to the THEN keyword.

   procedure Check_Unary_Plus_Or_Minus;
   --  Called after scanning a unary plus or minus to check spacing

   procedure Check_Vertical_Bar;
   --  Called after scanning a vertical bar to check spacing

   procedure No_End (Name : Node_Id);
   --  Called if an END is encountered where a name is allowed but not present.
   --  The parameter is the node whose name is the name that is permitted in
   --  the END line, and the scan pointer is positioned so that if an error
   --  message is to be generated in this situation, it should be generated
   --  using Error_Msg_SP.

   procedure Non_Lower_Case_Keyword;
   --  Called if a reserved keyword is scanned which is not spelled in all
   --  lower case letters. On entry Token_Ptr points to the keyword token.
   --  This is not used for keywords appearing as attribute designators,
   --  where instead Check_Attribute_Name (True) is called.

   function RM_Column_Check return Boolean;
   pragma Inline (RM_Column_Check);
   --  Determines whether style checking is active and the RM column check
   --  mode is set requiring checking of RM format layout.

end Style;
