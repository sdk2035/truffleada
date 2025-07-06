------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . X R E F                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.20 $
--                                                                          --
--          Copyright (C) 1998-2000, Free Software Foundation, Inc.         --
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

--  This package contains for collecting and outputting cross-reference
--  information.

with Einfo; use Einfo;
with Types; use Types;

package Lib.Xref is

   -------------------------------------------------------
   -- Format of Cross-Reference Information in ALI File --
   -------------------------------------------------------

   --  Cross-reference sections follow the dependency section (D lines) in
   --  an ALI file, so that they need not be read by gnatbind, gnatmake etc.
   --
   --  A cross reference section has a header of the form
   --
   --     X  dependency-number  file name
   --
   --        This header precedes xref information (entities/references from
   --        the unit, identified by dependency number and file name. The
   --        dependency number is the index into the generated D lines and
   --        is ones origin (i.e. 2 = reference to second generated D line).
   --
   --  The lines following the header look like
   --
   --     line type col level  entity ptype  ref  ref  ref
   --
   --        line is the line number of the referenced entity. It starts
   --        in column one.
   --
   --        type is a single letter identifying the type of the entity.
   --        See next section (Cross-Reference Entity Identifiers) for a
   --        full list of the characters used).
   --
   --        col is the column number of the referenced entity
   --
   --        level is a single character that separates the col and
   --        entity fields. It is an asterisk for a top level library
   --        entity that is publicly visible, and space otherwise.
   --
   --        entity is the name of the referenced entity, with casing in
   --        the canical casing for the source file where it is defined.
   --
   --        ptype is the parent's entity reference. This part is optional (it
   --        is only set for derived types) and has the following format:
   --
   --        < file | line type col >
   --
   --        file is the dependency number of the file containing the
   --        declaration of the parent type. This number and the following
   --        vertical bar are omitted if the parent type is defined in the
   --        same file as the derived type. The line, type, col are defined
   --        as previously described, and give the location of the parent
   --        type declaration in the referenced file.
   --
   --     There may be zero or more ref entries on each line
   --
   --        file | line type col
   --
   --           file is the dependency number of the file with the reference.
   --           It and the following vertical bar are omitted if the file is
   --           the same as the previous ref, and the refs for the current
   --           file are first (and do not need a bar).
   --
   --           type is one of
   --              r = reference
   --              m = modification
   --              b = body entity
   --              c = completion of private or incomplete type
   --              x = type extension
   --
   --           b is used for spec entities that are repeated in a body,
   --           including the unit (subprogram, package, task, protected
   --           body, protected entry) name itself, and in the case of a
   --           subprogram, the formals. This letter is also used for the
   --           occurrence of entry names in accept statements. Such entities
   --           are not considered to be definitions for cross-referencing
   --           purposes, but rather are considered to be references to the
   --           corresponding spec entities, marked with this special type.
   --
   --           c is similarly used to mark the completion of a private or
   --           incomplete type. Again, the completion is not regarded as
   --           a separate definition, but rather a reference to the initial
   --           declaration, marked with this special type.
   --
   --           x is used to identify the reference as the entity from which
   --           a tagged type is extended. This allows immediate access to
   --           the parent of a tagged type.
   --
   --     Examples:
   --
   --        44B5*Flag_Type 5r23 6m45 3|9r35 11r56
   --
   --           This line gives references for the publicly visible Boolean
   --           type Flag_Type declared on line 44, column 5. There are four
   --           references
   --
   --              a reference on line 5, column 23 of the current file
   --
   --              a modification on line 6, column 45 of the current file
   --
   --              a reference on line 9, column 35 of unit number 3
   --
   --              a reference on line 11, column 56 of unit number 3
   --
   --        2U13 p3 5b13 8r4
   --
   --           This line gives references for the non-publicly visible
   --           program unit (e.g. procedure) p3 declared on line 2,
   --           column 13. There are two reference:
   --
   --              the corresponding body entity at line 5, column 13,
   --              of the current file.
   --
   --              a reference (e.g. a call) at line 8 column 4 of the
   --              of the current file.
   --
   --        16I9*My_Type<2|4I9> 18r8
   --
   --           This line gives references for the publicly visible Integer
   --           derived type My_Type declared on line 16, column 9. It also
   --           gives references to the parent type declared in the unit
   --           number 2 on line 4, column 9. There is one reference:
   --
   --              a reference (e.g. a variable declaration) at line 18 column
   --              4 of the current file.
   --
   --  Continuation lines are used if the reference list gets too long,
   --  a continuation line starts with a period, and then has references
   --  continuing from the previous line. The references are sorted first
   --  by unit, then by position in the source.

   --  Note on handling of generic entities. The cross-reference is oriented
   --  towards source references, so the entities in a generic instantiation
   --  are not considered distinct from the entities in the template. All
   --  definitions and references from generic instantiations are suppressed,
   --  since they will be generated from the template. Any references to
   --  entities in a generic instantiation from outside the instantiation
   --  are considered to be references to the original template entity.

   ----------------------------------------
   -- Cross-Reference Entity Identifiers --
   ----------------------------------------

   --  In the cross-reference section of the ali file, entity types are
   --  identified by a single letter, indicating the entity type. The
   --  following table indicates the letter. A space for an entry is
   --  used for entities that do not appear in the cross-reference table.

   --  For objects, the character * appears in this table. In the xref
   --  listing, this character is replaced by the lower case letter that
   --  corresponds to the type of the object. For example, if a variable
   --  is of Boolean type, then, since the type is represented by an
   --  upper case B, the object would be represented by a lower case b.

   --  For private types, the character + appears in the table. In this
   --  case the type from the full view is used.

   Xref_Entity_Letters : array (Entity_Kind) of Character := (
      E_Void                             => ' ',
      E_Variable                         => '*',
      E_Component                        => '*',
      E_Constant                         => '*',
      E_Discriminant                     => '*',

      E_Loop_Parameter                   => '*',
      E_In_Parameter                     => '*',
      E_Out_Parameter                    => '*',
      E_In_Out_Parameter                 => '*',
      E_Generic_In_Out_Parameter         => '*',

      E_Generic_In_Parameter             => '*',
      E_Named_Integer                    => 'N',
      E_Named_Real                       => 'N',
      E_Enumeration_Type                 => 'E',
      E_Enumeration_Subtype              => 'E',

      E_Signed_Integer_Type              => 'I',
      E_Signed_Integer_Subtype           => 'I',
      E_Modular_Integer_Type             => 'M',
      E_Modular_Integer_Subtype          => 'M',
      E_Ordinary_Fixed_Point_Type        => 'O',

      E_Ordinary_Fixed_Point_Subtype     => 'O',
      E_Decimal_Fixed_Point_Type         => 'D',
      E_Decimal_Fixed_Point_Subtype      => 'D',
      E_Floating_Point_Type              => 'F',
      E_Floating_Point_Subtype           => 'F',

      E_Access_Type                      => 'P',
      E_Access_Subtype                   => 'P',
      E_Access_Attribute_Type            => 'P',
      E_Allocator_Type                   => ' ',
      E_General_Access_Type              => 'P',

      E_Access_Subprogram_Type           => 'P',
      E_Access_Protected_Subprogram_Type => 'P',
      E_Anonymous_Access_Type            => ' ',
      E_Array_Type                       => 'A',
      E_Array_Subtype                    => 'A',

      E_String_Type                      => 'S',
      E_String_Subtype                   => 'S',
      E_String_Literal_Subtype           => ' ',
      E_Enum_Table_Type                  => ' ',
      E_Class_Wide_Type                  => 'C',

      E_Class_Wide_Subtype               => 'C',
      E_Record_Type                      => 'R',
      E_Record_Subtype                   => 'R',
      E_Record_Type_With_Private         => 'R',
      E_Record_Subtype_With_Private      => 'R',

      E_Private_Type                     => '+',
      E_Private_Subtype                  => '+',
      E_Limited_Private_Type             => '+',
      E_Limited_Private_Subtype          => '+',
      E_Incomplete_Type                  => '+',

      E_Task_Type                        => 'T',
      E_Task_Subtype                     => 'T',
      E_Protected_Type                   => 'W',
      E_Protected_Subtype                => 'W',
      E_Exception_Type                   => ' ',

      E_Subprogram_Type                  => ' ',
      E_Enumeration_Literal              => 'N',
      E_Function                         => 'U',
      E_Operator                         => 'U',
      E_Procedure                        => 'U',

      E_Entry                            => 'Y',
      E_Entry_Family                     => 'Y',
      E_Block                            => 'L',
      E_Entry_Index_Parameter            => '*',
      E_Exception                        => 'X',

      E_Generic_Function                 => 'G',
      E_Generic_Package                  => 'G',
      E_Generic_Procedure                => 'G',
      E_Label                            => 'L',
      E_Loop                             => 'L',

      E_Package                          => 'U',
      E_Package_Body                     => ' ',
      E_Protected_Object                 => ' ',
      E_Protected_Body                   => ' ',
      E_Task_Body                        => ' ',

      E_Subprogram_Body                  => ' ');

   -----------------
   -- Subprograms --
   -----------------

   procedure Generate_Definition (E : Entity_Id);
   --  Records the definition of an entity

   procedure Generate_Operator_Reference (N : Node_Id);
   --  Node N is an operator node, whose entity has been set. If this entity
   --  is a user defined operator (i.e. an operator not defined in package
   --  Standard), then a reference to the operator is recorded at node N.

   procedure Generate_Reference
     (E       : Entity_Id;
      N       : Node_Id;
      Typ     : Character := 'r';
      Set_Ref : Boolean   := True);
   --  This procedure is called to record a reference. N is the location
   --  of the reference and E is the referenced entity. Typ is one of:
   --
   --    'b'  body entity (e.g. body formal referencing spec formal)
   --    'c'  completion of incomplete or private type (see below)
   --    'r'  standard reference
   --    'm'  modification
   --    'x'  type extension
   --    ' '  dummy reference (see below)
   --
   --  Note: all references to incomplete or private types are to the
   --  original (incomplete or private type) declaration. The full
   --  declaration is treated as a reference with type 'c'.
   --
   --  The call has no effect if N is not in the extended main source unit.
   --  If N is in the extended main source unit, then the Is_Referenced
   --  flag of E is set. In addition, if appropriate, a cross-reference
   --  entry is made. The entry is made if:
   --
   --    cross-reference collection is enabled
   --    both entity and reference come from source
   --    the entity is one for which xrefs are appropriate
   --    the type letter is non-blank
   --    the node N is an identifier, defining identifier, or expanded name
   --
   --  If all these conditions are met, then a cross-reference entry is
   --  made for later output when Output_References is called.
   --
   --  Note: the dummy entry is for the convenience of some callers, who
   --  find it easier to pass a space to suppress the entry than to do a
   --  specific test.
   --
   --  The parameter Set_Ref is normally True, and indicates that in
   --  addition to generating a cross-reference, the Referenced flag
   --  of the specified entity should be set. If this parameter is
   --  False, then setting of the Referenced flag is inhibited.

   procedure Output_References;
   --  Output references to the current ali file

end Lib.Xref;
