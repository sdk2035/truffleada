------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C A T                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.12 $                             --
--                                                                          --
--          Copyright (C) 1992-1999 Free Software Foundation, Inc.          --
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

--  This unit contains the routines used for checking for conformance with
--  the semantic restrictions required for the categorization pragmas:
--
--    Preelaborate
--    Pure,
--    Remote_Call_Interface
--    Remote_Types
--    Shared_Passive
--
--  Note that we treat Preelaborate as a categorization pragma, even though
--  strictly, according to RM E.2(2,3), the term does not apply in this case.

with Types; use Types;

package Sem_Cat is

   function In_Preelaborated_Unit return Boolean;
   --  Determines if the current scope is within a preelaborated compilation
   --  unit, that is one to which one of the pragmas Preelaborate, Pure,
   --  Shared_Passive, Remote_Types, or inside a unit other than a package
   --  body with pragma Remote_Call_Interface.

   function In_Pure_Unit return Boolean;
   pragma Inline (In_Pure_Unit);
   --  Determines if the current scope is within pure compilation unit,
   --  that is, one to which the pragmas Pure is applied.

   function In_Subprogram_Task_Protected_Unit return Boolean;
   --  Determines if the current scope is within a subprogram, task
   --  or protected unit. Used to validate if the library unit is Pure
   --  (RM 10.2.1(16)).

   procedure Set_Categorization_From_Pragmas (N : Node_Id);
   --  Since validation of categorization dependency is done during analyze
   --  so categorization flags from following pragmas should be set before
   --  validation begin. N is the N_Compilation_Unit node.

   procedure Validate_Access_Type_Declaration (T : Entity_Id; N : Node_Id);
   --  Validate all constraints against declaration of access types in
   --  categorized library units. Usually this is a violation in Pure unit,
   --  Shared_Passive unit. N is the declaration node.

   procedure Validate_Ancestor_Part (N : Node_Id);
   --  Checks that a type given as the ancestor in an extension aggregate
   --  satisfies the restriction of 10.2.1(9).

   procedure Validate_Categorization_Dependency (N : Node_Id; E : Entity_Id);
   --  There are restrictions on lib unit that semantically depends on other
   --  units (RM E.2(5), 10.2.1(11). This procedure checks the restrictions
   --  on categorizations. N is the current unit node, and E is the current
   --  library unit entity.

   procedure Validate_Controlled_Object (E : Entity_Id);
   --  Given an entity for a library level controlled object, check that it is
   --  not in a preelaborated unit (prohibited by RM 10.2.1(9)).

   procedure Validate_Null_Statement_Sequence (N : Node_Id);
   --  Given N, a package body node, check that the handled statement sequence
   --  contains no statements other than null_statement. This from
   --  RM 10.2.1(6).

   procedure Validate_Object_Declaration
     (N   : Node_Id;
      Id  : Entity_Id;
      E   : Node_Id;
      Odf : Node_Id;
      T   : Entity_Id);
   --  Validate all the constraints in a preelaborable lib unit against
   --  an object declaration, including the creation of task object, etc.

   procedure Validate_RCI_Declarations (P : Entity_Id);
   --  Apply semantic checks given in  E2.3(10-14).

   procedure Validate_RCI_Subprogram_Declaration (N : Node_Id);
   --  Check for RCI unit subprogram declarations with respect to
   --  in-lined subprogram and subprogram with access parameter or
   --  limited type parameter without Read and Write.

   procedure Validate_Remote_Access_To_Class_Wide_Type (N : Node_Id);
   --  Checks that Storage_Pool and Storage_Size attribute references are
   --  not applied to remote access-to-class-wide types. And the expected
   --  type for an allocator shall not be a remote access-to-class-wide
   --  type. And a remote access-to-class-wide type shall not be an actual
   --  parameter for a generic formal access type. RM E.2.3(22).

   procedure Validate_Remote_Access_To_Subprogram_Type (N : Node_Id);
   --  Checks that a remote access to subprogram type does not have a
   --  parameter of an access type. This is not strictly forbidden at this
   --  time, but this is useless, as such a RAS type will not be usable
   --  per E.2.2(12) and E.2.3(14).

   procedure Validate_RT_RAT_Component (N : Node_Id);
   --  Given N, the package library unit declaration node, we should check
   --  against RM:9.95 E.2.2(8): the full view of a type declared in the
   --  visible part of a Remote Types unit has a part that is of a non-remote
   --  access type which has no read/write.

   procedure Validate_Remote_Type_Type_Conversion (N : Node_Id);
   --  Check for remote-type type conversion constraints. First, a value of
   --  a remote access-to-subprogram type can be converted only to another
   --  type conformant remote access-to-subprogram type. Secondly, a value
   --  of a remote access-to-class-wide type can be converted only to another
   --  remote access-to-class-wide type (RM E.2.3(17,20)).

   procedure Validate_SP_Access_Object_Type_Decl (T : Entity_Id);
   --  Check validity of declaration if shared passive unit. It should not
   --  contain the declaration of an access-to-object type whose designated
   --  type is a class-wide type ,task type or protected type. E.2.1(7).
   --  T is the entity of the declared type.

   procedure Validate_Static_Object_Name (N : Node_Id);
   --  In the elaboration code of a preelaborated library unit, check
   --  that we do not have the evaluation of a primary that is a name of
   --  an object, unless the name is a static expression (RM 10.2.1(8)).
   --  Non-static constant and variable are the targets, generic parameters
   --  are not included because the generic declaration and body are
   --  preelaborable.

end Sem_Cat;
