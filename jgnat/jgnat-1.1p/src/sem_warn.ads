------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ W A R N                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.3 $
--                                                                          --
--          Copyright (C) 1999-2000 Free Software Foundation, Inc.          --
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

--  This package contains the routines used to deal with issuing warnings
--  about uses of uninitialized variables and unused with's. It also has
--  some unrelated routines related to the generation of warnings.

with Types; use Types;

package Sem_Warn is

   ------------------------------------------
   -- Routines to Handle Unused References --
   ------------------------------------------

   procedure Check_References (E : Entity_Id; Anod : Node_Id := Empty);
   --  Called at the end of processing a declarative region. The entity E
   --  is the entity for the scope. All entities declared in the region,
   --  as indicated by First_Entity and the entity chain, are checked to
   --  see if they are variables for which warnings need to be posted for
   --  either no assignments, or a use before an assignment or no references
   --  at all. The Anod node is present for the case of an accept statement,
   --  and references the accept statement. This is used to place the warning
   --  messages in the right place.

   procedure Check_Unset_Reference (N : Node_Id);
   --  N is the node for an expression which occurs in a reference position,
   --  e.g. as the right side of an assignment. This procedure checks to see
   --  if the node is a reference to a variable entity where the entity has
   --  Not_Assigned set. If so, the Unset_Reference field is set if it is not
   --  the first occurrence. No warning is posted, instead warnings will be
   --  posted later by Check_References. The reason we do things that
   --  way is that if there are no assignments anywhere, we prefer to flag
   --  the entity, rather than a reference to it. Note that for the purposes
   --  of this routine, a type conversion or qualified expression whose
   --  expression is an entity is also processed. The reason that we do not
   --  process these at the point of occurrence is that both these constructs
   --  can occur in non-reference positions (e.g. as out parameters).

   procedure Check_Unused_Withs (Spec_Unit : Unit_Number_Type := No_Unit);
   --  This routine performs two kinds of checks. It checks that all with'ed
   --  units are referenced, and that at least one entity of each with'ed
   --  unit is referenced (the latter check catches units that are only
   --  referenced in a use or package renaming statement). Appropriate
   --  warning messages are generated if either of these situations is
   --  detected.
   --
   --  A special case arises when a package body or a subprogram body with
   --  a separate spec is being compiled. In this case, a with may appear
   --  on the spec, but be needed only by the body. This still generates
   --  a warning, but the text is different (the with is not redundant,
   --  it is misplaced).
   --
   --  This special case is implemented by making an initial call to this
   --  procedure with Spec_Unit set to the unit number of the separate spec.
   --  This call does not generate any warning messages, but instead may
   --  result in flags being set in the N_With_Clause node that record that
   --  there was no use in the spec.
   --
   --  The main call (made after all units have been analyzed, with Spec_Unit
   --  set to the default value of No_Unit) generates the required warnings
   --  using the flags set by the initial call where appropriate to specialize
   --  the text of the warning messages.

   ----------------------------------------
   -- Routines to Deal with Conditionals --
   ----------------------------------------

   --  These routines provide the necessary interfacing information to
   --  correctly handle references in conditional structures (if/then/end-if,
   --  or case/when/end-case). The issue here is that if a variable is only
   --  set in some but not all branches of a conditional, then it is not
   --  considered as being set by the conditional as a whole.

   procedure Start_Unit;
   --  Mark start of new unit to be analyzed, deals with fact that a call to
   --  Rtsfind may cause new unit to be analyzed in middle of conditional.

   procedure End_Unit;
   --  Mark end of unit corresponding to previous call to Start_Unit

   procedure Start_Conditional (If_Stmt : Boolean);
   --  Mark start of a new conditional structure (an if-elsif-else-endif
   --  or a case-when-end-case structure). If_Stmt is True for the IF
   --  statement case, and False for the CASE statement case.

   procedure Start_Branch (Loc : Source_Ptr);
   --  Start processing of one branch of conditional previously marked by
   --  a call to Start_Conditional (i.e. start of then/elsif/else statements
   --  or set of statements after a when condition). The Loc value is the
   --  source pointer to be used in warning messages concerning variables
   --  not properly initialized in this branch. A branch is terminated by
   --  either another Start_Branch or End_Conditional call.

   procedure End_Conditional;
   --  Terminate conditional started by previous Start_Conditional statement.

   ---------------------
   -- Output Routines --
   ---------------------

   procedure Output_Unreferenced_Messages;
   --  Warnings about unreferenced entities are collected till the end of
   --  the compilation process (see Check_Unset_Reference for further
   --  details). This procedure outputs waiting warnings, if any.

   ----------------------------
   -- Other Warning Routines --
   ----------------------------

   procedure Warn_On_Known_Condition (C : Node_Id);
   --  C is a node for a boolean expression resluting from a relational
   --  or membership operation. If the expression has a compile time known
   --  value, then a warning is output if all the following conditions hold:
   --
   --    1. Original expression comes from source. We don't want to generate
   --       warnings for internally generated conditionals.
   --
   --    2. As noted above, the expression is a relational or membership
   --       test, we don't want to generate warnings for boolean variables
   --       since this is typical of conditional compilation in Ada.
   --
   --    3. The expression appears in a statement, rather than a declaration.
   --       In practice, most occurrences in declarations are legitimate
   --       conditionalizations, but occurrences in statements are often
   --       errors for which the warning is useful.
   --
   --    4. The expression does not occur within an instantiation. A non-
   --       static expression in a generic may become constant because of
   --       the attributes of the actuals, and we do not want to warn on
   --       these legitimate constant foldings.
   --
   --  If all these conditions are met, the warning is issued noting that
   --  the result of the test is always false or always true as appropriate.

end Sem_Warn;
