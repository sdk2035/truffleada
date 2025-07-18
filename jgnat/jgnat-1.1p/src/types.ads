------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                T Y P E S                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.81 $
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

with Unchecked_Deallocation;

package Types is
pragma Preelaborate (Types);

--  This package contains host independent type definitions which are used
--  in more than one unit in the compiler. They are gathered here for easy
--  reference, though in some cases the full description is found in the
--  relevant module which implements the definition. The main reason that
--  they are not in their "natural" specs is that this would cause a lot of
--  inter-spec dependencies, and in particular some awkward circular
--  dependencies would have to be dealt with.

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file a-types.h

--  Note: the declarations in this package reflect an expectation that the
--  host machine has an efficient integer base type with a range at least
--  32 bits 2s-complement. If there are any machines for which this is not
--  a correct assumption, a significant number of changes will be required!

   -------------------------------
   -- General Use Integer Types --
   -------------------------------

   type Int is range -2 ** 31 .. +2 ** 31 - 1;
   --  Signed 32-bit integer

   subtype Nat is Int range 0 .. Int'Last;
   --  Non-negative Int values

   subtype Pos is Int range 1 .. Int'Last;
   --  Positive Int values

   type Word is mod 2 ** 32;
   --  Unsigned 32-bit integer

   type Short is range -32768 .. +32767;
   for Short'Size use 16;
   --  16-bit signed integer

   type Byte is mod 2 ** 8;
   for Byte'Size use 8;
   --  8-bit unsigned integer

   type size_t is mod 2 ** Standard'Address_Size;
   --  Memory size value, for use in calls to C routines

   --------------------------------------
   -- 8-Bit Character and String Types --
   --------------------------------------

   --  We use Standard.Character and Standard.String freely, since we are
   --  compiling ourselves, and we properly implement the required 8-bit
   --  character code as required in Ada 95. This section defines a few
   --  general use constants and subtypes.

   EOF : constant Character := ASCII.SUB;
   --  The character SUB (16#1A#) is used in DOS and other systems derived
   --  from DOS (OS/2, NT etc) to signal the end of a text file. Internally
   --  all source files are ended by an EOF character, even on Unix systems.
   --  An EOF character acts as the end of file only as the last character
   --  of a source buffer, in any other position, it is treated as a blank
   --  if it appears between tokens, and as an illegal character otherwise.
   --  This makes life easier dealing with files that originated from DOS,
   --  including concatenated files with interspersed EOF characters.

   subtype Graphic_Character is Character range ' ' .. '~';
   --  Graphic characters, as defined in ARM

   subtype Line_Terminator is Character range ASCII.LF .. ASCII.CR;
   --  Line terminator characters (LF, VT, FF, CR)

   subtype Upper_Half_Character is
     Character range Character'Val (16#80#) .. Character'Val (16#FF#);
   --  Characters with the upper bit set

   type Character_Ptr is access all Character;
   type String_Ptr    is access all String;
   --  Standard character and string pointers

   procedure Free is new Unchecked_Deallocation (String, String_Ptr);
   --  Procedure for freeing dynamically allocated String values

   subtype Word_Hex_String is String (1 .. 8);
   --  Type used to represent Word value as 8 hex digits, with upper case
   --  letters for the alphabetic cases.

   function Get_Hex_String (W : Word) return Word_Hex_String;
   --  Convert word value to 8-character hex string

   -----------------------------------------
   -- Types Used for Text Buffer Handling --
   -----------------------------------------

   --  We can't use type String for text buffers, since we must use the
   --  standard 32-bit integer as an index value, since we count on all
   --  index values being the same size.

   type Text_Ptr is new Int;
   --  Type used for subscripts in text buffer

   type Text_Buffer is array (Text_Ptr range <>) of Character;
   --  Text buffer used to hold source file or library information file

   type Text_Buffer_Ptr is access all Text_Buffer;
   --  Text buffers for input files are allocated dynamically and this type
   --  is used to reference these text buffers.

   procedure Free is new Unchecked_Deallocation (Text_Buffer, Text_Buffer_Ptr);
   --  Procedure for freeing dynamically allocated text buffers

   ------------------------------------------
   -- Types Used for Source Input Handling --
   ------------------------------------------

   type Logical_Line_Number is range 0 .. Int'Last;
   for Logical_Line_Number'Size use 32;
   --  Line number type, used for storing logical line numbers (i.e. line
   --  numbers that include the line offset from pragma Source_Reference)
   --  Logical line number zero is reserved for the line containing the
   --  Source_Reference pragma at the start of the file.

   No_Line_Number : constant Logical_Line_Number := 0;
   --  Special value used to indicate no line number

   type Column_Number is range 0 .. 32767;
   for Column_Number'Size use 16;
   --  Column number (assume that 2**15 is large enough, see declaration
   --  of Hostparm.Max_Line_Length)

   No_Column_Number : constant Column_Number := 0;
   --  Special value used to indicate no column number

   subtype Source_Buffer is Text_Buffer;
   --  Type used to store text of a source file . The buffer for the main
   --  source (the source specified on the command line) has a lower bound
   --  starting at zero. Subsequent subsidiary sources have lower bounds
   --  which are one greater than the previous upper bound.

   subtype Big_Source_Buffer is Text_Buffer (0 .. Text_Ptr'Last);
   --  This is a virtual type used as the designated type of the access
   --  type Source_Buffer_Ptr, see Osint.Read_Source_File for details.

   type Source_Buffer_Ptr is access all Big_Source_Buffer;
   --  Pointer to source buffer. We use virtual origin addressing for
   --  source buffers, with thin pointers. The pointer points to a virtual
   --  instance of type Big_Source_Buffer, where the actual type is in fact
   --  of type Source_Buffer. The address is adjusted so that the virtual
   --  origin addressing works correctly. See Osint.Read_Source_Buffer for
   --  further details.

   subtype Source_Ptr is Text_Ptr;
   --  Type used to represent a source location, which is a subscript of a
   --  character in the source buffer. As noted above, diffferent source
   --  buffers have different ranges, so it is possible to tell from a
   --  Source_Ptr value which source it refers to. Note that negative numbers
   --  are allowed to accomodate the following special values.

   No_Location : constant Source_Ptr := -1;
   --  Value used to indicate no source position set in a node

   Standard_Location : constant Source_Ptr := -2;
   --  Used for all nodes in the representation of package Standard other
   --  than nodes representing the contents of Standard.ASCII. Note that
   --  testing for <= Standard_Location tests for both Standard_Location
   --  and for Standard_ASCII_Location.

   Standard_ASCII_Location : constant Source_Ptr := -3;
   --  Used for all nodes in the presentation of package Standard.ASCII

   First_Source_Ptr : constant Source_Ptr := 0;
   --  Starting source pointer index value for first source program

   -------------------------------------
   -- Range Definitions for Tree Data --
   -------------------------------------

   --  The tree has fields that can hold any of the following types:

   --    Pointers to other tree nodes (type Node_Id)
   --    List pointers (type List_Id)
   --    Element list pointers (type Elist_Id)
   --    Names (type Name_Id)
   --    Strings (type String_Id)
   --    Universal integers (type Uint)
   --    Universal reals (type Ureal)
   --    Character codes (type Char_Code stored with a bias)

   --  In most contexts, the strongly typed interface determines which of
   --  these types is present. However, there are some situations (involving
   --  untyped traversals of the tree), where it is convenient to be easily
   --  able to distinguish these values. The underlying representation in all
   --  cases is an integer type Union_Id, and we ensure that the range of
   --  the various possible values for each of the above types is disjoint
   --  so that this distinction is possible.

   type Union_Id is new Int;
   --  The type in the tree for a union of possible ID values

   --  Note: it is also helpful for debugging purposes to make these ranges
   --  distinct. If a bug leads to misidentification of a value, then it will
   --  typically result in an out of range value and a Constraint_Error.

   List_Low_Bound : constant := -100_000_000;
   --  The List_Id values are subscripts into an array of list headers which
   --  has List_Low_Bound as its lower bound. This value is chosen so that all
   --  List_Id values are negative, and the value zero is in the range of both
   --  List_Id and Node_Id values (see further description below).

   List_High_Bound : constant := 0;
   --  Maximum List_Id subscript value. This allows up to 100 million list
   --  Id values, which is in practice infinite, and there is no need to
   --  check the range. The range overlaps the node range by one element
   --  (with value zero), which is used both for the Empty node, and for
   --  indicating no list. The fact that the same value is used is convenient
   --  because it means that the default value of Empty applies to both nodes
   --  and lists, and also is more efficient to test for.

   Node_Low_Bound : constant := 0;
   --  The tree Id values start at zero, because we use zero for Empty (to
   --  allow a zero test for Empty). Actual tree node subscripts start at 0
   --  since Empty is a legitimate node value.

   Node_High_Bound : constant := 099_999_999;
   --  Maximum number of nodes that can be allocated is 100 million, which
   --  is in practice infinite, and there is no need to check the range.

   Elist_Low_Bound : constant := 100_000_000;
   --  The Elist_Id values are subscripts into an array of elist headers which
   --  has Elist_Low_Bound as its lower bound.

   Elist_High_Bound : constant := 199_999_999;
   --  Maximum Elist_Id subscript value. This allows up to 100 million Elists,
   --  which is in practice infinite and there is no need to check the range.

   Elmt_Low_Bound : constant := 200_000_000;
   --  Low bound of element Id values. The use of these values is internal to
   --  the Elists package, but the definition of the range is included here
   --  since it must be disjoint from other Id values. The Elmt_Id values are
   --  subscripts into an array of list elements which has this as lower bound.

   Elmt_High_Bound : constant := 299_999_999;
   --  Upper bound of Elmt_Id values. This allows up to 100 million element
   --  list members, which is in practice infinite (no range check needed).

   Names_Low_Bound : constant := 300_000_000;
   --  Low bound for name Id values

   Names_High_Bound : constant := 399_999_999;
   --  Maximum number of names that can be allocated is 100 million, which is
   --  in practice infinite and there is no need to check the range.

   Strings_Low_Bound : constant := 400_000_000;
   --  Low bound for string Id values

   Strings_High_Bound : constant := 499_999_999;
   --  Maximum number of strings that can be allocated is 100 million, which
   --  is in practice infinite and there is no need to check the range.

   Uint_Low_Bound : constant := 500_000_000;
   --  Low bound for Uint values.

   Uint_High_Bound : constant := 599_999_999;
   --  Maximum number of Uint values stored is 100_000_000 which is in
   --  practice infinite so that no check is required.

   Ureal_Low_Bound : constant := 600_000_000;
   --  Low bound for Ureal values.

   Ureal_High_Bound : constant := 699_999_999;
   --  Maximum number of Ureal values stored is 100_000_000 which is in
   --  practice infinite so that no check is required.

   Char_Code_Bias : constant := 700_000_000;
   --  A bias value added to character code values stored in the tree which
   --  ensures that they have different values from any of the above types.

   --  The following subtype definitions are used to provide convenient names
   --  for membership tests on Int values to see what data type range they
   --  lie in. Such tests appear only in the lowest level packages.

   subtype List_Range      is Union_Id
     range List_Low_Bound   .. List_High_Bound;

   subtype Node_Range      is Union_Id
     range Node_Low_Bound   .. Node_High_Bound;

   subtype Elist_Range     is Union_Id
     range Elist_Low_Bound  .. Elist_High_Bound;

   subtype Elmt_Range      is Union_Id
     range Elmt_Low_Bound   .. Elmt_High_Bound;

   subtype Names_Range     is Union_Id
     range Names_Low_Bound   .. Names_High_Bound;

   subtype Strings_Range   is Union_Id
     range Strings_Low_Bound .. Strings_High_Bound;

   subtype Uint_Range      is Union_Id
     range Uint_Low_Bound    .. Uint_High_Bound;

   subtype Ureal_Range     is Union_Id
     range Ureal_Low_Bound    .. Ureal_High_Bound;

   subtype Char_Code_Range is Union_Id
     range Char_Code_Bias    .. Char_Code_Bias + 2**16 - 1;

   -----------------------------
   -- Types for Namet Package --
   -----------------------------

   --  Name_Id values are used to identify entries in the names table. Except
   --  for the special values No_Name, and Error_Name, they are subscript
   --  values for the Names table defined in package Namet.

   --  Note that with only a few exceptions, which are clearly documented, the
   --  type Name_Id should be regarded as a private type. In particular it is
   --  never appropriate to perform arithmetic operations using this type.

   type Name_Id is range Names_Low_Bound .. Names_High_Bound;
   for Name_Id'Size use 32;
   --  Type used to identify entries in the names table

   No_Name : constant Name_Id := Names_Low_Bound;
   --  The special Name_Id value No_Name is used in the parser to indicate
   --  a situation where no name is present (e.g. on a loop or block).

   Error_Name : constant Name_Id := Names_Low_Bound +  1;
   --  The special Name_Id value Error_Name is used in the parser to
   --  indicate that some kind of error was encountered in scanning out
   --  the relevant name, so it does not have a representable label.

   First_Name_Id : constant Name_Id := Names_Low_Bound + 2;
   --  Subscript of first entry in names table

   ----------------------------
   -- Types for Atree Package --
   ----------------------------

   --  Node_Id values are used to identify nodes in the tree. They are
   --  subscripts into the Node table declared in package Tree. Note that
   --  the special values Empty and Error are subscripts into this table,
   --  See package Atree for further details.

   type Node_Id is range Node_Low_Bound .. Node_High_Bound;
   --  Type used to identify nodes in the tree

   subtype Entity_Id is Node_Id;
   --  A synonym for node types, used in the entity package to refer to
   --  nodes that are entities (i.e. nodes with an Nkind of N_Defining_xxx)
   --  All such nodes are extended nodes and these are the only extended
   --  nodes, so that in practice entity and extended nodes are synonymous.

   subtype Node_Or_Entity_Id is Node_Id;
   --  A synonym for node types, used in cases where a given value may be used
   --  to represent either a node or an entity. We like to minimize such uses
   --  for obvious reasons of logical type consistency, but where such uses
   --  occur, they should be documented by use of this type.

   Empty : constant Node_Id := Node_Low_Bound;
   --  Used to indicate null node. A node is actually allocated with this
   --  Id value, so that Nkind (Empty) = N_Empty. Note that Node_Low_Bound
   --  is zero, so Empty = No_List = zero.

   Empty_List_Or_Node : constant := 0;
   --  This constant is used in situations (e.g. initializing empty fields)
   --  where the value set will be used to represent either an empty node
   --  or a non-existent list, depending on the context.

   Error : constant Node_Id := Node_Low_Bound + 1;
   --  Used to indicate that there was an error in the source program. A node
   --  is actually allocated at this address, so that Nkind (Error) = N_Error.

   Empty_Or_Error : constant Node_Id := Error;
   --  Since Empty and Error are the first two Node_Id values, the test for
   --  N <= Error tests to see if N is Empty or Error. This definition provides
   --  convenient self-documentation for such tests.

   First_Node_Id  : constant Node_Id := Node_Low_Bound;
   --  Subscript of first allocated node. Note that Empty and Error are both
   --  allocated nodes, whose Nkind fields can be accessed without error.

   ------------------------------
   -- Types for Nlists Package --
   ------------------------------

   --  List_Id values are used to identify node lists in the tree. They are
   --  subscripts into the Lists table declared in package Tree. Note that
   --  the special value Error_List is a subscript in this table, but the
   --  value No_List is *not* a valid subscript, and any attempt to apply
   --  list operations to No_List will cause a (detected) error.

   type List_Id is range List_Low_Bound .. List_High_Bound;
   --  Type used to identify a node list

   No_List : constant List_Id := List_High_Bound;
   --  Used to indicate absence of a list. Note that the value is zero, which
   --  is the same as Empty, which is helpful in intializing nodes where a
   --  value of zero can represent either an empty node or an empty list.

   Error_List : constant List_Id := List_Low_Bound;
   --  Used to indicate that there was an error in the source program in a
   --  context which would normally require a list. This node appears to be
   --  an empty list to the list operations (a null list is actually allocated
   --  which has this Id value).

   First_List_Id : constant List_Id := Error_List;
   --  Subscript of first allocated list header

   ------------------------------
   -- Types for Elists Package --
   ------------------------------

   --  Element list Id values are used to identify element lists stored in
   --  the tree (see package Tree for further details). They are formed by
   --  adding a bias (Element_List_Bias) to subscript values in the same
   --  array that is used for node list headers.

   type Elist_Id is range Elist_Low_Bound .. Elist_High_Bound;
   --  Type used to identify an element list (Elist header table subscript)

   No_Elist : constant Elist_Id := Elist_Low_Bound;
   --  Used to indicate absense of an element list. Note that this is not
   --  an actual Elist header, so element list operations on this value
   --  are not valid.

   First_Elist_Id : constant Elist_Id := No_Elist + 1;
   --  Subscript of first allocated Elist header.

   --  Element Id values are used to identify individual elements of an
   --  element list (see package Elists for further details).

   type Elmt_Id is range Elmt_Low_Bound .. Elmt_High_Bound;
   --  Type used to identify an element list

   No_Elmt : constant Elmt_Id := Elmt_Low_Bound;
   --  Used to represent empty element

   First_Elmt_Id : constant Elmt_Id := No_Elmt + 1;
   --  Subscript of first allocated Elmt table entry

   -------------------------------
   -- Types for Stringt Package --
   -------------------------------

   --  String_Id values are used to identify entries in the strings table.
   --  They are subscripts into the strings table defined in package Strings.

   --  Note that with only a few exceptions, which are clearly documented, the
   --  type String_Id should be regarded as a private type. In particular it is
   --  never appropriate to perform arithmetic operations using this type.

   type String_Id is range Strings_Low_Bound .. Strings_High_Bound;
   --  Type used to identify entries in the strings table

   No_String : constant String_Id := Strings_Low_Bound;
   --  Used to indicate missing string Id. Note that the value zero is used
   --  to indicate a missing data value for all the Int types in this section.

   First_String_Id : constant String_Id := No_String + 1;
   --  First subscript allocated in string table

   -------------------------
   -- Character Code Type --
   -------------------------

   --  The type Char is used for character data internally in the compiler,
   --  but character codes in the source are represented by the Char_Code
   --  type. Each character literal in the source is interpreted as being one
   --  of the 2**16 possible Wide_Character codes, and a unique integer value
   --  is assigned, corresponding to the POS value in the Wide_Character type.
   --  String literals are similarly interpreted as a sequence of such codes.

   --  Note: when character code values are stored in the tree, they are stored
   --  by adding a bias value (Char_Code_Bias) that results in values that can
   --  be distinguished from other types of values stored in the tree.

   type Char_Code is mod 2 ** 16;
   for Char_Code'Size use 16;

   function Get_Char_Code (C : Character) return Char_Code;
   pragma Inline (Get_Char_Code);
   --  Function to obtain internal character code from source character. For
   --  the moment, the internal character code is simply the Pos value of the
   --  input source character, but we provide this interface for possible
   --  later support of alternative character sets.

   function In_Character_Range (C : Char_Code) return Boolean;
   pragma Inline (In_Character_Range);
   --  Determines if the given character code is in range of type Character,
   --  and if so, returns True. If not, returns False.

   function Get_Character (C : Char_Code) return Character;
   pragma Inline (Get_Character);
   --  For a character C that is in character range (see above function), this
   --  function returns the corresponding Character value. It is an error to
   --  call Get_Char if C is not in character range

   ---------------------------------------
   -- Types used for Library Management --
   ---------------------------------------

   type Unit_Number_Type is new Int;
   --  Unit number. The main source is unit 0, and subsidiary sources have
   --  non-zero numbers starting with 1. Unit numbers are used to index the
   --  file table in Lib.

   Main_Unit : constant Unit_Number_Type := 0;
   --  Unit number value for main unit

   No_Unit : constant Unit_Number_Type := -1;
   --  Special value used to signal no unit

   type Source_File_Index is new Nat;
   --  Type used to index the source file table (see package Sinput)

   No_Source_File : constant Source_File_Index := 0;
   --  Value used to indicate no source file present

   subtype File_Name_Type is Name_Id;
   --  File names are stored in the names table and this synonym is used to
   --  indicate that a Name_Id value is being used to hold a simple file
   --  name (which does not include any directory information).

   No_File : constant File_Name_Type := File_Name_Type (No_Name);
   --  Constant used to indicate no file found

   subtype Unit_Name_Type is Name_Id;
   --  Unit names are stored in the names table and this synonym is used to
   --  indicate that a Name_Id value is being used to hold a unit name.

   -----------------------------------
   -- Representation of Time Stamps --
   -----------------------------------

   --  All compiled units are marked with a time stamp which is derived from
   --  the source file (we assume that the host system has the concept of a
   --  file time stamp which is modified when a file is modified). These
   --  time stamps are used to ensure consistency of the set of units that
   --  constitutes a library. Time stamps are 12 character strings with
   --  with the following format:

   --     YYYYMMDDHHMMSS

   --       YYYY   year
   --       MM     month (2 digits 01-12)
   --       DD     day (2 digits 01-31)
   --       HH     hour (2 digits 00-23)
   --       MM     minutes (2 digits 00-59)
   --       SS     seconds (2 digits 00-59)

   --  In the case of Unix systems (and other systems which keep the time in
   --  GMT), the time stamp is the GMT time of the file, not the local time.
   --  This solves problems in using libraries across networks with clients
   --  spread across multiple time-zones.

   Time_Stamp_Length : constant := 14;
   --  Length of time stamp value

   subtype Time_Stamp_Index is Natural range 1 .. Time_Stamp_Length;
   type Time_Stamp_Type is new String (Time_Stamp_Index);
   --  Type used to represent time stamp

   Empty_Time_Stamp : constant Time_Stamp_Type := (others => ' ');
   --  Type used to represent an empty or missing time stamp. Looks less
   --  than any real time stamp if two time stamps are compared. Note that
   --  although this is not a private type, clients should not rely on the
   --  exact way in which this string is represented, and instead should
   --  use the subprograms below.

   function "="  (Left, Right : Time_Stamp_Type) return Boolean;
   function "<=" (Left, Right : Time_Stamp_Type) return Boolean;
   function ">=" (Left, Right : Time_Stamp_Type) return Boolean;
   function "<"  (Left, Right : Time_Stamp_Type) return Boolean;
   function ">"  (Left, Right : Time_Stamp_Type) return Boolean;
   --  Comparison functions on time stamps. Note that two time stamps
   --  are defined as being equal if they have the same day/month/year
   --  and the hour/minutes/seconds values are within 2 seconds of one
   --  another. This deals with rounding effects in library file time
   --  stamps caused by copying operations during installation. We have
   --  particularly noticed that WinNT seems susceptible to such changes.
   --  Note: the Empty_Time_Stamp value looks equal to itself, and less
   --  than any non-empty time stamp value.

   procedure Split_Time_Stamp
     (TS      : Time_Stamp_Type;
      Year    : out Nat;
      Month   : out Nat;
      Day     : out Nat;
      Hour    : out Nat;
      Minutes : out Nat;
      Seconds : out Nat);
   --  Given a time stamp, decompose it into its components

   procedure Make_Time_Stamp
     (Year    : Nat;
      Month   : Nat;
      Day     : Nat;
      Hour    : Nat;
      Minutes : Nat;
      Seconds : Nat;
      TS      : out Time_Stamp_Type);
   --  Given the components of a time stamp, initialize the value

   -----------------------------------------------
   -- Types used for Pragma Suppress Management --
   -----------------------------------------------

   --  The following record contains an entry for each recognized check name
   --  for pragma Suppress. It is used to represent current settings of scope
   --  based suppress actions from pragma Suppress or command line settings.

   type Suppress_Record is record
      Access_Checks        : Boolean;
      Accessibility_Checks : Boolean;
      Discriminant_Checks  : Boolean;
      Division_Checks      : Boolean;
      Elaboration_Checks   : Boolean;
      Index_Checks         : Boolean;
      Length_Checks        : Boolean;
      Overflow_Checks      : Boolean;
      Range_Checks         : Boolean;
      Storage_Checks       : Boolean;
      Tag_Checks           : Boolean;
   end record;

   --  To add a new check type to GNAT, the following steps are required:

   --    1.  Add an appropriate entry to the above record type
   --    2.  Add an entry to Snames spec and body for the new name
   --    3.  Add an entry to the definition of Check_Id in the Snames spec
   --    4.  Add a new entity flag definition in Einfo for the check
   --    5.  Add a new function to Sem.Util to handle the new check test
   --    6.  Add appropriate processing for pragma Suppress in Sem.Prag
   --    7.  Add a branch to the case statement in Sem.Ch8.Pop_Scope
   --    8.  Add a new Do_xxx_Check flag to Sinfo (if required)
   --    9.  Add appropriate checks for the new test

   -----------------------------------
   -- Global Exception Declarations --
   -----------------------------------

   --  This section contains declarations of exceptions that are used
   --  throughout the compiler.

   Unrecoverable_Error : exception;
   --  This exception is raised to immediately terminate the compilation
   --  of the current source program. Used in situations where things are
   --  bad enough that it doesn't seem worth continuing (e.g. max errors
   --  reached, or a required file is not found). Also raised when the
   --  compiler finds itself in trouble after an error (see Comperr).

   ---------------------------------
   -- Parameter Mechanism Control --
   ---------------------------------

   --  Function and parameter entities have a field that records the
   --  passing mechanism. See specification of Sem_Mech for full details.
   --  The following subtype is used to represent values of this type:

   subtype Mechanism_Type is Int range -10 .. Int'Last;
   --  Type used to represent a mechanism value. This is a subtype rather
   --  than a type to avoid some annoying processing problems with certain
   --  routines in Einfo (processing them to create the corresponding C).

end Types;
