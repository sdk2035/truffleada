------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M _ A D A                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.45 $                             --
--                                                                          --
--           Copyright (C) 1998-2000 Ada Core Technologies, Inc.            --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Strings.Fixed;           use Ada.Strings.Fixed;
with Ada.Text_IO;
with GNAT.IO_Aux;
with GNAT.Spitbol;
with GNAT.Spitbol.Table_Integer;
with J_Basics;
with J_List;
with J_Types;                     use J_Types;
with J_Utils;
with J_Zip;
with JVM_File;                    use JVM_File;
with Osint;

package body JVM_Ada is

   ------------------------
   -- Local Types & Data --
   ------------------------

   JVM_Byte     : constant Character := 'B';
   JVM_Char     : constant Character := 'C';
   JVM_Double   : constant Character := 'D';
   JVM_Float    : constant Character := 'F';
   JVM_Int      : constant Character := 'I';
   JVM_Long     : constant Character := 'J';
   JVM_Short    : constant Character := 'S';
   JVM_Boolean  : constant Character := 'Z';
   JVM_Void     : constant Character := 'V';
   JVM_Class    : constant Character := 'L';
   JVM_Array    : constant Character := '[';

   ------------------------------------
   -- Global Class File Symbol Table --
   ------------------------------------

   --  The following types are used to implement a symbol table where we keep
   --  cetain critical informations concerning JVM classes. This information is
   --  needed, for instance, to determine whether a JVM class is a JVM
   --  exception or not.

   package String_List is new J_List (String);
   procedure Sort is new String_List.Sort ("<");
   --  Implements a list of strings

   type Class_Info is record
      Super_Class_Name : String_Ptr;
      --  The name of the class this class derives from

      Implements : String_List.List;
      --  List of all the public interfaces this class implements in sorted
      --  order.  Needed so that we can output all the required Ada
      --  discriminants for this class.

      Is_Public : Boolean;
      --  Set if this class is public

      Is_Exception : Boolean;
      --  Set to YES if this class derives directly or indericetly form class
      --  java.lang.Throwable.
   end record;

   Unknown_Class : constant Class_Info
     := (Super_Class_Name => new String'("java/lang/Object"),
         Implements       => String_List.Empty_List,
         Is_Public        => True,
         Is_Exception     => False);

   function Do_Nothing (Info : Class_Info) return String;
   --  Returns string "Do nothing", needed for the following generic
   --  instantioation.

   package Symbol_Table_Pkg is
      new GNAT.Spitbol.Table (Class_Info, Unknown_Class, Do_Nothing);

   Symbol_Table : Symbol_Table_Pkg.Table (2048);
   --  This is the symbol table that contains all the classes already parsed by
   --  jvm2ada. This is used to avoid redundant parsing of the classes.

   function Enter_Info (CF : Class_File) return Class_Info;
   --  Enters the information relative to CF in Symbol_Table. If an
   --  entry for CF was already present that entry is update.

   procedure Enter_Info (CF : Class_File);
   --  Same as above but the information entred is not returned

   function Get_Info (Name : String) return Class_Info;
   --  Returns the information known about the class whose full name is
   --  Name. Name is of the form "java/lang/Object".  If this class has never
   --  been looked at before, the class it is parsed and the information is
   --  stored in the Symbol_Table.

   ----------------------
   -- Used Names Table --
   ----------------------

   package Name_Table renames GNAT.Spitbol.Table_Integer;
   Used_Names : Name_Table.Table (100);
   --  Table of names already in use in the Ada package spec currently being
   --  generated. This table is used to avoid name clashing, since Ada is
   --  case insensitive whereas Java is not. if a string S is present in the
   --  Used_Names table, the name S is in use. The nunmer associated with it
   --  gives the number of times the name is in use in the Ada spec. If this
   --  number is 1 then a "_K" must be appended at the end of the new
   --  instance of this name. Otherwise if this number is nnn then a "_Knnn"
   --  must be appended.

   --------------------------
   -- Search File Handling --
   --------------------------

   --  When an archive is specified via a -I or -L switch the whole archive is
   --  kept in memory, uncompressed. When the archive is read, its content is
   --  parsed and the archive directory is kept so that looking for a file in
   --  the archived is fast.

   package Archive_Search is new J_List (J_Utils.Archive_Directory_Access);

   Classes_Search_List : Archive_Search.List := Archive_Search.Empty_List;
   --  List of archives we must look in to locate .class files

   Sources_Search_List : Archive_Search.List := Archive_Search.Empty_List;
   --  List of archives we must look in to locate source files

   generic
      The_Search_List : in out Archive_Search.List;
   procedure Generic_Search_In (Zip : String);
   --  Generic version of Search_Classes_In and Search_Sources_In.

   ----------------------------
   -- File Location Routines --
   ----------------------------

   --  All file location routines return a File_Id given below

   type File_Id is record
      Stream : Stream_Of_U1_Ptr;
      --  The zip archive containing the file

      Info   : J_Zip.File_Info;
      --  The location of the file within the zip archive
   end record;

   No_File : constant File_Id := (null, (1, 0, 1, 0, False, False));

   function Find_Class_File
     (File          : String;
      Ignore_Errors : Boolean := False;
      Ignore_Casing : Boolean := False)
     return File_Id;
   --  Locate class file File (which is expected to be of the form
   --  "java/lang/Object.class") and returns its File_Id. File is searched in:
   --
   --    (1) If we are currently processing a zip archive look there first
   --    (2) Then look in each of the archives specified by the -Lzip
   --        switches (i.e. look through the Classes_Search_List)
   --
   --  If no class file is found, then a warning message is printed if
   --  Ignore_Errors is False and No_File is returned. If Ignore_Casing is
   --  set then we ignore casing differences in file names when looking for
   --  a file.

   function Find_Source_File
     (File          : String;
      Ignore_Errors : Boolean := not Verbose_Mode)
     return File_Id;
   --  Tries to locate source file File by looking in each of the locations
   --  specified by the -Izip switches (i.e. look through the
   --  Sources_Search_List). If no class file is found, then a warning
   --  message is printed when Ignore_Errors is False and No_File is returned.

   generic
      The_Search_List         : Archive_Search.List;
      --  The archive search list to look into

      Look_In_Current_Archive : Boolean;
      --  If this is set look in the current zip archive being processed if any

   function Generic_Find_File
     (File          : String;
      Ignore_Errors : Boolean;
      Ignore_Casing : Boolean)
     return File_Id;
   --  Generic version of Find_Class_File & Find_Source_File

   -----------------------------
   -- Pretty Printing Package --
   -----------------------------

   package Pretty_Print is
      function Open_File (Class_Name : String) return Boolean;
      --  Open a new output file to store the specifications corresponding to
      --  Class_Name. Returns False if the file could not be open.

      procedure Close_File;
      --  Close the output file

      function Current_Column return Ada.Text_IO.Count;
      --  Returns the current column in the output file

      procedure Incr_Indent (Step : Integer);
      --  Adds or subtracts 3 * Step to the current indentation level.  The
      --  new indentation will be used after the line currently being output
      --  has been terminated with by a Print_Line.

      procedure Set_Tmp_Indent (Value : Ada.Text_IO.Count);
      --  Set the indentation at absolue value Value.

      procedure Print (S : String);
      --  Print S to the file opened with Open_File

      procedure Print_Line (S : String := "");
      --  Print S to the file opened with Open_File and add a New_Line at the
      --  end.

   private
      Output_File         : Ada.Text_IO.File_Type;
      Current_Indentation : Natural := 1;
   end Pretty_Print;

   --  Convenient renamings

   procedure P (S : String)
     renames Pretty_Print.Print;

   procedure PL (S : String := "")
     renames Pretty_Print.Print_Line;

   --------------------------
   -- Identifiers handling --
   --------------------------

   function Is_Ada_Keyword (Identifier : String) return Boolean;
   --  Return true if Identifier is an Ada keyword

   function Ada_Identifier (Name : String) return String;
   --  Given a Java identifier it turns it into an Ada one.

   function Get_Identifier (Entity : String;
                            Short  : Boolean := False)
                            return   String;
   --  Returns the Ada type name corresponding to the JVM class Entity. If
   --  short is True, ignore the leading package name (e.g. in
   --  java/lang/Object, just process Object).

   type Ada_Type_Format is (Regular_Type, Parameter_Type, Use_As_Name);
   --  The format of the string returned by the Ada_Type function. See below.

   function Ada_Type
     (D      : String;
      Format : Ada_Type_Format := Regular_Type)
     return String;
   --  Returns a string which is the Ada type equivalent for the JVM type
   --  descriptor D. If Format = Regular_Type then the string returned can be
   --  used in a regular Ada context except for a parameter type. If Format =
   --  Parameter_Type then the string returned can be used as a parameter
   --  type. Finally if Format = Use_As_Name then the string returned is the
   --  Ada type that can be used for appending to a parameter identifier name
   --  (e.g. P1_Int or P3_Object or P2_Object_Arr).

   --------------------
   -- Print Routines --
   --------------------

   procedure Print_With (CF : JVM_File.Class_File);
   --  Print the with statements for the class file. We print with statements
   --  for 'extends' and 'implements' clauses in the JVM class and in all
   --  other cases we print 'with type' statements.

   procedure Print_Obj_Declaration (CF          : Class_File;
                                    Pragma_List : in out String_List.List);
   --  Print the 'type Typ is ... ' declaration. Pragma_List is the
   --  list current list of pragmas to add to the private part of the
   --  Ada package spec.

   procedure Print_Fields (CF            : Class_File;
                           Static_Fields : Boolean;
                           Pragma_List   : in out String_List.List);
   --  Process the fields of the Java class file CF. If Static_Fields is
   --  True, only static fields from the Java file will be printed, otherwise
   --  only the instance fields will be printed. Pragma_List is the list of
   --  pragmas to append to the private part of the Ada package spec.

   procedure Print_Methods (CF          : Class_File;
                            Pragma_List : in out String_List.List);
   --  Process the methods of the class file CF. Class_Name is CF's name as
   --  it appears in CF. Pragma_List is the list of pragmas to append in the
   --  private part of the generated Ada package spec.

   ---------------
   -- Utilities --
   ---------------

   function To_String (T : Utf8.Table) return String
     renames JVM_File.To_String;

   function Get_String (CF : Class_File; K : CP_Index_Class) return String;
   --  Given a constant pool entry K, which is the index of a class entry
   --  in constant pool T, return the name of that class as a string.

   function Get_Public_Super_Class (CF : Class_File) return String;
   --  Returns the first public superclass of CF in the format "java/lang/Void"

   function Access_Rights_OK (Flags : Access_Mask) return Boolean;
   --  Returns True if the accesss Flags indicates that the field or the method
   --  should be converted (this is the case for public and protected items
   --  only).

   procedure Check_Visibility
     (CF                      : Class_File;
      Is_Public               : out Boolean;
      Has_Nested_Public_Class : out Boolean);
   --  Given CF, a class file, this routine sets Is_Public to True if the CF
   --  is a bona fide public class. That is:
   --    (a) CF is marked as public
   --    (b) If CF is an inner class there must be an Inner_Classes attribute
   --        in CF and the public flag must be set in there.
   --    (c) CF is not one of Sun's implementation classes (unless swithc -s
   --        is set).
   --  Flag Has_Nested_Public_Classes is set iff CF has nested public
   --  classes. The side effect of this routine is to enter in the Used_Names
   --  table all the names of the public inner class of CF.

   function Is_Public_Class (CF : Class_File) return Boolean;
   --  Returns True if the CF is a bona fide public class as defined above.

   function Is_Public_Member (M : Member_Info; T : CP.Table) return Boolean;
   --  Returns True if the Field or Method is public. A member is public if its
   --  access flag is either public or protected and if all its class types are
   --  public.

   function Dottify (Entity : String) return String;
   --  Replaces all "/" with "."

   procedure Warning (Msg : String);
   --  Print a warning message if not is quiet mode

   procedure Debug_Msg (Msg : String);
   --  Print a debugging message if debug mode is on

   ----------------------
   -- Access_Rights_OK --
   ----------------------

   function Access_Rights_OK (Flags : Access_Mask) return Boolean is
   begin
      return Is_Set (Flags, ACC_Public) or else Is_Set (Flags, ACC_Protected);
   end Access_Rights_OK;

   --------------------
   -- Ada_Identifier --
   --------------------

   function Ada_Identifier (Name : String) return String is
      function Ada_Identifier_Rec (Name : String) return String;

      function Ada_Identifier_Rec
        (Name : String)
        return String
      is
      begin
         if Name'Length = 0 then
            return "";

            --  A single underscore is mapped into a U

         elsif Name = "_" then
            return "U";

            --  Replace leading underscores with a U_

         elsif Name (Name'First) = '_' then
            return
              "U_" & Ada_Identifier_Rec (Name (Name'First + 1 .. Name'Last));
         end if;

         --  Replace multiple "_" with "U_"

         for K in Name'Range loop
            if Name (K) = '_' then
               if K = Name'Last then
                  return Name & "U";
               elsif Name (K + 1) = '_' then
                  return Name (Name'First .. K)
                    & Ada_Identifier_Rec (Name (K + 1 .. Name'Last));
               end if;
            end if;
         end loop;

         return Name;
      end Ada_Identifier_Rec;

   begin
      if Keep_Original_Identifiers then
         return Name;
      elsif Is_Ada_Keyword (Name) then
         return Name & "_K";
      else
         return Ada_Identifier_Rec (Name);
      end if;
   end Ada_Identifier;

   ---------------
   --  Ada_Type --
   ---------------

   function Ada_Type
     (D      : String;
      Format : Ada_Type_Format := Regular_Type)
     return String
   is
      function Dimension_To_String (D : U1) return String;
      --  If D = 1 returns the empty string otherwise it returns the string
      --  image of D preceded with a "_".

      function Scalar_Prefix return String;
      --  Returns the proper prefix for the scalar type

      -------------------------
      -- Dimension_To_String --
      -------------------------

      function Dimension_To_String (D : U1) return String is
      begin
         if D = 1 then
            return "";
         else
            return "_" & Image (D);
         end if;
      end Dimension_To_String;

      -------------------
      -- Scalar_Prefix --
      -------------------

      function Scalar_Prefix return String is
      begin
         if Format /= Use_As_Name then
            return "Java.";
         else
            return "";
         end if;
      end Scalar_Prefix;

   begin
      case D (D'First) is
         when JVM_Class  =>
            for K in D'Range loop
               if D (K) = ';' then
                  declare
                     Java_Type : constant String := D (D'First + 1 .. K - 1);
                  begin
                     if Format = Parameter_Type then
                        return
                          "access "
                          & Get_Identifier (Java_Type) & ".Typ'Class";
                     elsif Format = Regular_Type then
                        return Get_Identifier (Java_Type) & ".Ref";
                     else
                        return Get_Identifier (Java_Type, Short => True);
                     end if;
                  end;
               end if;
            end loop;
            pragma Assert (False);
            return "";  --  This return should never be taken

         when JVM_Array =>
            declare
               Dim : U1 := 0;
               Pos : Natural := D'First;
            begin
               while D (Pos) = JVM_Array loop
                  Pos := Pos + 1;
                  Dim := Dim + 1;
               end loop;

               --  Is this is an array of class references

               if D (Pos) = JVM_Class then
                  if Format /= Use_As_Name then
                     declare
                        S : constant String := Ada_Type (D (Pos .. D'Last));
                     begin
                        return
                          S (S'First .. S'Last - 4) & ".Arr"
                          & Dimension_To_String (Dim);
                     end;

                  else
                     return
                       Ada_Type (D (Pos .. D'Last), Use_As_Name) & "_Arr"
                       & Dimension_To_String (Dim);
                  end if;

               --  Otherwise we have a scalar type array

               else
                  if Dim > 1 then
                     return
                       Ada_Type (D (Pos .. Pos), Format) & "_Arr_"
                       & Image (Dim);
                  else
                     return Ada_Type (D (Pos .. Pos), Format) & "_Arr";
                  end if;
               end if;
            end;

         when JVM_Byte    =>
            return Scalar_Prefix & "Byte";

         when JVM_Char    =>
            return Scalar_Prefix & "Char";

         when JVM_Double  =>
            return Scalar_Prefix & "Double";

         when JVM_Float   =>
            return Scalar_Prefix & "Float";

         when JVM_Int     =>
            return Scalar_Prefix & "Int";

         when JVM_Long    =>
            return Scalar_Prefix & "Long";

         when JVM_Short   =>
            return Scalar_Prefix & "Short";

         when JVM_Boolean =>
            return Scalar_Prefix & "Boolean";

         when others =>
            Osint.Fail ("Invalid type descriptor : " & D);
            return "";
      end case;
   end Ada_Type;

   ----------------------
   -- Check_Visibility --
   ----------------------

   procedure Check_Visibility
     (CF                      : Class_File;
      Is_Public               : out Boolean;
      Has_Nested_Public_Class : out Boolean)
   is
      use Class_Attribute;
      use Inner_Class;

      Class_Name : constant String := Get_String (CF, CF.This_Class);
      CA         : Class_Attribute_Info;
      ICI        : Inner_Class_Info;

      Is_A_Public_Nested_Class : Boolean := False;

   begin
      Is_Public               := False;
      Has_Nested_Public_Class := False;

      --  Skip SUN implementation classes if their mapping has not been
      --  requested.

      if Skip_Sun_Classes
        and then (Head (Class_Name, 4) = "sun/"
                  or else Head (Class_Name, 5) = "sunw/"
                  or else Head (Class_Name, 8) = "com/sun/")
      then
         return;
      end if;

      Is_Public := Is_Set (CF.Access_Flags, ACC_Public);

      --  Now look at the class attributes and check if this is a public
      --  nested class and whether it has any public nested classes.

      for J in 0 .. Last (CF.Attributes) loop
         CA := Get (CF.Attributes, J);

         if CA.Kind = Attr_Inner_Classes then
            for K in 0 .. Last (CA.Classes) loop
               ICI := Get (CA.Classes, K);

               --  Is this a public nested class ?

               if Is_Public
                 and then ICI.Inner_Class_Info_Index = CF.This_Class
               then
                  --  If there is no name this was an anonymous class

                  if ICI.Inner_Name_Index = 0 then
                     Is_Public := False;
                  elsif Is_Set (ICI.Inner_Class_Access_Flags, ACC_Public) then
                     Is_A_Public_Nested_Class := True;
                  end if;

               elsif ICI.Outer_Class_Info_Index = CF.This_Class then
                  --  If the nested class is anonymous nothing to do

                  if ICI.Inner_Name_Index = 0 then
                     null;
                  elsif Is_Set (ICI.Inner_Class_Access_Flags, ACC_Public) then
                     Has_Nested_Public_Class := True;
                     declare
                        IC_Name : constant String :=
                          To_String
                            (J_Basics.Get_Utf8
                               (CF.Constant_Pool, ICI.Inner_Name_Index));
                     begin
                        Name_Table.Set (Used_Names, To_Upper (IC_Name), 1);
                     end;
                  end if;
               end if;
            end loop;
         end if;
      end loop;

      --  If the class name has a $ in it, it is a nested class. If no nested
      --  class attribute was found then this class is not public.

      if Is_Public
        and then Ada.Strings.Fixed.Index (Class_Name, "$") /= 0
        and then not Is_A_Public_Nested_Class
      then
         Is_Public := False;
      end if;
   end Check_Visibility;

   --------------------
   -- Convert_To_Ada --
   --------------------

   procedure Convert_To_Ada (Bytes : Stream_Of_U1) is
      use Class_Attribute;
      use Inner_Class;
      use String_List;

      CF : constant Class_File := JVM_File.Read (Bytes, Check => True);
      Class_Name : constant String := Get_String (CF, CF.This_Class);

      Pragma_List : String_List.List;
      --  List of the pragmas to generate in the private part of the Ada
      --  package spec being generated.

      Iter : String_List.List_Iterator;

      Ada_Package : String := Get_Identifier (Class_Name);

      Is_Public             : Boolean;
      Contains_Public_Class : Boolean;

   begin
      Check_Visibility (CF, Is_Public, Contains_Public_Class);

      if not Is_Public then
         --  If this is not a public class, but it contains a public nested
         --  class generate an empty package spec so that the nested class has
         --  a proper Ada package parent (e.g. java.awt.font.TextLine and
         --  java.awt.font.TextLine.TextLineMetrics).

         if Contains_Public_Class then
            if Pretty_Print.Open_File (Ada_Package) then
               if Verbose_Mode then
                  Ada.Text_IO.Put_Line ("  " & Class_Name & ".class");
               end if;

               PL ("pragma Extensions_Allowed (On);");
               PL;
               P ("package " & Ada_Package & " is");
               Pretty_Print.Incr_Indent (1);
               PL;
               PL ("pragma Preelaborate;");
               Pretty_Print.Incr_Indent (-1);
               PL ("end " & Ada_Package & ";");
               PL ("pragma Import (Java, " & Ada_Package & ", """
                   & Dottify (Class_Name) & """);");
               PL ("pragma Extensions_Allowed (Off);");
               Pretty_Print.Close_File;
            end if;
         end if;

         Name_Table.Clear (Used_Names);
         return;
      end if;

      --  If we can not create the output file, just print a warning and exit

      if not Pretty_Print.Open_File (Ada_Package) then
         Name_Table.Clear (Used_Names);
         return;
      elsif Verbose_Mode then
         Ada.Text_IO.Put_Line ("  " & Class_Name & ".class");
      end if;

      --  Grab the following nams since these are already in use in the
      --  generated specs.

      Name_Table.Set   (Used_Names, "STANDARD", 1);
      Name_Table.Set   (Used_Names, "TYP", 1);
      Name_Table.Set   (Used_Names, "REF", 1);
      Name_Table.Set   (Used_Names, "ARR", 1);
      Name_Table.Set   (Used_Names, "ARR_2", 1);
      Name_Table.Set   (Used_Names, "ARR_3", 1);

      --  Update the Symbol_Table information with data about CF

      Enter_Info (CF);

      --  Allow the extensions when interfacing to the java API

      PL ("pragma Extensions_Allowed (On);");

      --  Print the list of required with_statements and with_type_statements

      Print_With (CF);

      --  Print the definition of the class

      PL;
      P ("package " & Ada_Package & " is");
      Pretty_Print.Incr_Indent (1);
      PL;
      PL ("pragma Preelaborate;");
      PL;

      --  Print the incomplete declaration for types

      PL ("-----------------------");
      PL ("-- Type Declarations --");
      PL ("-----------------------");
      PL;

      if Is_Set (CF.Access_Flags, ACC_Final) then
         PL ("--  final class");
         PL;
      end if;

      if Class_Name /= "java/lang/Object" then
         PL ("type Typ;");
         String_List.Append ("pragma Convention (Java, Typ);", Pragma_List);
      else
         PL ("type Typ is tagged limited null record;");
         PL ("pragma Convention (Java, Typ);");
      end if;

      PL ("type Ref is access all Typ'Class;");

      --  Print the array declarations

      PL;
      PL ("type Arr_Obj is array (Natural range <>) of Ref;");
      PL ("type Arr     is access all Arr_Obj;");
      PL;
      PL ("type Arr_2_Obj is array (Natural range <>) of Arr;");
      PL ("type Arr_2     is access all Arr_2_Obj;");
      PL;
      PL ("type Arr_3_Obj is array (Natural range <>) of Arr_2;");
      PL ("type Arr_3     is access all Arr_3_Obj;");
      PL;

      --  Complete type declaration

      if Class_Name /= "java/lang/Object" then
         Print_Obj_Declaration (CF, Pragma_List);
      end if;

      if Class_Name = "java/lang/String" then
         PL;
         PL ("---------------------------------------------------");
         PL ("-- Java String to Ada String Conversion Routines --");
         PL ("---------------------------------------------------");
         PL;
         PL ("type String_Access is access all Standard.String;");
         PL ("function ""+"" (S : Ref) return String_Access;");
         PL ("function ""+"" (S : Standard.String) return Ref;");
         String_List.Append
           ("pragma Import (Java, ""+"", "
            & """jgnat.adalib.GNAT_libc.to_string"");",
            Pragma_List);
      end if;

      Print_Methods (CF, Pragma_List);
      Print_Fields (CF, Static_Fields => True, Pragma_List => Pragma_List);

      Pretty_Print.Incr_Indent (-1);
      PL;
      PL ("private");
      Pretty_Print.Incr_Indent (1);
      PL;

      --  Print the pragmas associated with the current class and clean up
      --  the list of pragmas.

      Associate (Pragma_List, Iter);
      while not Is_Last (Iter) loop
         PL (Get (Iter));
         Next (Iter);
      end loop;
      Clean (Pragma_List);

      --  Terminate the package declaration

      Pretty_Print.Incr_Indent (-1);
      PL;
      PL ("end " & Ada_Package & ";");
      PL ("pragma Import (Java, " & Ada_Package
          & ", """ & Dottify (Class_Name) & """);");
      PL ("pragma Extensions_Allowed (Off);");

      Pretty_Print.Close_File;

      --  Clear the table of names already in use for the next use;

      Name_Table.Clear (Used_Names);
   end Convert_To_Ada;

   ------------------------------
   -- Convert_Directory_To_Ada --
   ------------------------------

   procedure Convert_Directory_To_Ada (Name : String) is
      Name_L   : constant String := To_Lower (Name);
      Ada_Name : constant String := Get_Identifier (Name);

   begin
      --  JGNAT already contains a package Java in its library

      if Name_L = "java" then
         return;
      end if;

      --  Skip the Sun packages if not requested

      if Skip_Sun_Classes
        and then (Name_L = "sun"
                  or else Head (Name_L, 4) = "sun/"
                  or else Head (Name_L, 4) = "sun\"

                  or else Name_L = "sunw"
                  or else Head (Name_L, 5) = "sunw/"
                  or else Head (Name_L, 5) = "sunw\"

                  or else Name_L = "com/sun"
                  or else Name_L = "com\sun"
                  or else Head (Name_L, 8) = "com/sun/"
                  or else Head (Name_L, 8) = "com\sun\")
      then
         return;
      end if;

      --  To be a Java package the directory name must contain only letters,
      --  digits or underscores

      if not Is_Letter (Name (Name'First)) then
         return;
      end if;

      for J in Name'Range loop
         if not (Is_Letter (Name (J))
                 or else Is_Digit (Name (J))
                 or else Name (J) = '_'
                 or else Name (J) = '/'
                 or else Name (J) = '\')
         then
            return;
         end if;
      end loop;

      if not Quiet_Mode then
         Ada.Text_IO.Put_Line ("Processing: " & Name & " ...");
      end if;

      --  Do nothing if there is already a class with the same (case
      --  insensitive) name, since it's that class that will generate the
      --  corresponding Ada package spec. This occurs for instance in the
      --  Java API with directory java/awt/font and class java/awt/Font.class

      if Find_Class_File
           (Name & ".class",
            Ignore_Errors => True,
            Ignore_Casing => True) /= No_File
      then
         return;
      end if;

      --  Otherwise create a very simple Ada package spec corresponding to
      --  the Java package.

      if not Pretty_Print.Open_File (Name) then
         return;
      end if;

      PL ("pragma Extensions_Allowed (On);");
      Pretty_Print.Incr_Indent (1);
      PL ("package " & Ada_Name & " is");
      PL ("pragma Preelaborate;");
      Pretty_Print.Incr_Indent (-1);
      PL;
      PL ("end " & Ada_Name & ";");
      PL ("pragma Import (Java, " & Ada_Name & ", """
                               & Dottify (Name) & """);");
      PL ("pragma Extensions_Allowed (Off);");
      Pretty_Print.Close_File;
   end Convert_Directory_To_Ada;

   ---------------
   -- Debug_Msg --
   ---------------

   procedure Debug_Msg (Msg : String) is
   begin
      if Verbose_Mode then
         Ada.Text_IO.Put_Line ("      " & Msg);
      end if;
   end Debug_Msg;

   ----------------
   -- Do_Nothing --
   ----------------

   function Do_Nothing (Info : Class_Info) return String is
   begin
      return "Nothing to do";
   end Do_Nothing;

   -------------
   -- Dottify --
   -------------

   function Dottify (Entity : String) return String is
      Tmp : String := Entity;

   begin
      for J in Tmp'Range loop
         if Tmp (J) = '/' then
            Tmp (J) := '.';
         end if;
      end loop;
      return Tmp;
   end Dottify;

   ----------------
   -- Enter_Info --
   ----------------

   pragma Warnings (Off);
   procedure Enter_Info (CF : Class_File) is
      Ignore : constant Class_Info := Enter_Info (CF);
   begin
      null;
   end Enter_Info;
   pragma Warnings (On);

   function Enter_Info (CF : Class_File) return Class_Info is
      use Class_Index;
      use Class_Attribute;
      use String_List;

      Class_Name       : constant String := Get_String (CF, CF.This_Class);
      Super_Class_Name : String_Ptr;
      Implements       : String_List.List;
      Is_Exception     : Boolean;
      Info             : Class_Info;

   begin
      --  Debug_Msg (Class_Name & ": added to Symbol Table");

      --  Get the name of the super class

      Super_Class_Name := new String'(Get_String (CF, CF.Super_Class));

      --  First copy the list of interfaces the superclass implements ad then
      --  add the new interfaces this class implements.

      Copy (Get_Info (Super_Class_Name.all).Implements, Implements);

      for K in 0 .. Last (CF.Interfaces) loop
         declare
            Interf_Index : constant CP_Index := Get (CF.Interfaces, K);
            Interf_Name  : constant String   := Get_String (CF, Interf_Index);
         begin
            --  Ignore non-public interfaces

            if Get_Info (Interf_Name).Is_Public then
               String_List.Append_If_Uniq (Interf_Name, Implements);
            end if;
         end;
      end loop;

      Sort (Implements);

      --  Figure out whether this class is a java exception.  There are
      --  only two cases where we know for sure whether or not we have an
      --  exception. In all other cases we must look at the parent class.

      Is_Exception := Get_Info (Super_Class_Name.all).Is_Exception;

      Info := (Super_Class_Name => Super_Class_Name,
               Implements       => Implements,
               Is_Public        => Is_Public_Class (CF),
               Is_Exception     => Is_Exception);

      Symbol_Table_Pkg.Set (Symbol_Table, Class_Name, Info);
      return Info;
   end Enter_Info;

   -----------------------
   -- Generic_Find_File --
   -----------------------

   function Generic_Find_File
     (File          : String;
      Ignore_Errors : Boolean;
      Ignore_Casing : Boolean)
     return File_Id
   is
      use J_Basics;
      use Archive_Search;

      subtype Zip_Data is J_Utils.Archive_Directory_Access;

      function Find (A_File : String; Zip : Zip_Data) return File_Id;
      --  Look for File in the zip archive Zip. If it notthere return No_File,
      --  otherwise return the proper File_Id.

      function Find (A_File : String; Zip : Zip_Data) return File_Id is
      begin
         if Zip.Stream = null then
            return No_File;
         end if;

         for J in Zip.Archive'Range loop
            declare
               F    : J_Zip.File_Info renames Zip.Archive (J);
               Name : constant String :=
                 To_String (Zip.Stream (F.Name_First .. F.Name_Last));
            begin
               if Name = A_File
                 or else
                 (Ignore_Casing and then To_Upper (Name) = To_Upper (A_File))
               then
                  return (Stream => Zip.Stream, Info => F);
               end if;
            end;
         end loop;

         return No_File;
      end Find;

      Iterator : Archive_Search.List_Iterator;
      F_Id     : File_Id;

   begin --  of Generic_Find_File
      if Look_In_Current_Archive then
         F_Id := Find (File, J_Utils.Get_Current_Archive);
         if F_Id /= No_File then
            return F_Id;
         end if;
      end if;

      Associate (The_Search_List, Iterator);

      while not Is_Last (Iterator) loop
         F_Id := Find (File, Get (Iterator));
         if F_Id /= No_File then
            return F_Id;
         end if;
         Next (Iterator);
      end loop;

      if not Ignore_Errors then
         Warning ("WARNING: Can't find: " & File);
      end if;

      return No_File;
   end Generic_Find_File;

   ---------------------
   -- Find_Class_File --
   ---------------------

   function Find_Class_File
     (File          : String;
      Ignore_Errors : Boolean := False;
      Ignore_Casing : Boolean := False)
     return File_Id
   is
      function Find_Class_File_Instance is
         new Generic_Find_File (The_Search_List         => Classes_Search_List,
                                Look_In_Current_Archive => True);
   begin
      return Find_Class_File_Instance (File, Ignore_Errors, Ignore_Casing);
   end Find_Class_File;

   ----------------------
   -- Find_Source_File --
   ----------------------

   function Find_Source_File
     (File          : String;
      Ignore_Errors : Boolean := not Verbose_Mode)
     return File_Id
   is
      function Find_Source_File_Instance is
         new Generic_Find_File (The_Search_List         => Sources_Search_List,
                                Look_In_Current_Archive => False);
   begin
      return Find_Source_File_Instance (File, Ignore_Errors, False);
   end Find_Source_File;

   -----------------------
   -- Generic_Search_In --
   -----------------------

   procedure Generic_Search_In (Zip : String) is
      Bytes : Stream_Of_U1_Ptr;
      --  Contains the bytes of the input archive or null if we have a
      --  directory.

   begin
      --  Make sure we have an uncompressed zip archive

      Bytes := J_Basics.Get_Stream_Of_U1 (Zip);

      declare
         use J_Zip;
         Archive : J_Utils.Archive_Directory_Access :=
           (Stream  => Bytes,
            Archive => new Archive_Directory' (Get_Archive_Dir (Bytes.all)));
      begin
         Archive_Search.Append (Archive, The_Search_List);
      end;

   exception
      when J_Zip.Bad_Zip_Archive =>
         Osint.Fail (Zip & " is not a zip archive");

      when J_Zip.Compressed_Zip_Archive =>
         Osint.Fail
           ("Compressed archive: ",
            Zip, " jvm2ada can only handle uncompressed archives.");
   end Generic_Search_In;

   --------------------
   -- Get_Identifier --
   --------------------

   function Get_Identifier (Entity : String;
                            Short  : Boolean := False)
                            return   String
   is
      S : String := Entity;
   begin
      if S'Length = 0 then
         return "";
      end if;

      for J in reverse S'Range loop
         if S (J) = '/' or S (J) = '$' then
            declare
               Id : constant String := Ada_Identifier (S (J + 1 .. S'Last));
            begin
               if Short then
                  return Id;
               else
                  return Get_Identifier (S (S'First .. J - 1)) & '.' & Id;
               end if;
            end;
         end if;
      end loop;

      return Ada_Identifier (S);
   end Get_Identifier;

   --------------
   -- Get_Info --
   --------------

   function Get_Info (Name : String) return Class_Info is
      use Class_Index;

      File : File_Id;
      CF   : Class_File;

   begin
      --  Debug_Msg ("    -> " & Name & ": Symbol Table lookup ... ");

      if Name = "" then
         return Unknown_Class;

      --  If the class info is already in the symbol table then return it.
      --  The entries for "java/lang/Object" and "java/lang/Throwable" have
      --  been entered in the Symbol_Table when this package is elaborated
      --  (see the begin-end section at the end of this package body).

      elsif Symbol_Table_Pkg.Present (Symbol_Table, Name) then
         return Symbol_Table_Pkg.Get (Symbol_Table, Name);
      end if;

      --  Otherwise, the class hasn't been looked at yet, so load it and
      --  parse it to get the information we need.

      File := Find_Class_File (Name & ".class");

      if File = No_File then
         return Unknown_Class;
      end if;

      CF := Read (File.Stream (File.Info.First .. File.Info.Last), True);

      return Enter_Info (CF);
   end Get_Info;

   ----------------------------
   -- Get_Public_Super_Class --
   ----------------------------

   function Get_Public_Super_Class (CF : Class_File) return String is
      use type Ada.Text_IO.Count;
      use String_List;

      Class_Name : constant String := Get_String (CF, CF.This_Class);
      Info       : constant Class_Info := Get_Info (Class_Name);

      Super_Info : Class_Info;
      Super_Name : String_Ptr;

   begin
      Super_Name := Info.Super_Class_Name;
      loop
         Super_Info := Get_Info (Super_Name.all);
         if Super_Info.Is_Public then
            return Super_Name.all;
         end if;
         Super_Name := Super_Info.Super_Class_Name;
      end loop;
      return "";
   end Get_Public_Super_Class;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (CF : Class_File; K : CP_Index_Class) return String is
      T          : constant CP.Table   := CF.Constant_Pool;
      Class_Info : CP_Info;
      Class_Name : Utf8.Table;
   begin
      if K = CP_Empty then
         return "";
      end if;

      Class_Info := JVM_File.CP.Get (T, K);
      Class_Name := J_Basics.Get_Utf8 (T, Class_Info.Class_Name_Index);
      return To_String (Class_Name);
   end Get_String;

   ---------------------
   --  Is_Ada_Keyword --
   ---------------------

   function Is_Ada_Keyword (Identifier : String) return Boolean is
      Str : String := To_Upper (Identifier);
   begin
      if Str'Length < 2 or else Str'Length > 9 then
         return False;
      end if;

      case Str (Str'First) is

         when 'A' =>
            case Str (Str'First + 1) is
               when 'B' => return False
                 or else Str = "ABORT"
                 or else Str = "ABS"
                 or else Str = "ABSTRACT";
               when 'C' => return False
                 or else Str = "ACCEPT"
                 or else Str = "ACCESS";
               when 'L' => return False
                 or else Str = "ALL"
                 or else Str = "ALIASED";
               when 'N' => return False
                 or else Str = "AND";
               when 'R' => return False
                 or else Str = "ARRAY";
               when 'T' => return False
                 or else Str = "AT";
               when others => return False;
            end case;

         when 'B' => return False
           or else Str = "BEGIN"
           or else Str = "BODY";
         when 'C' => return False
           or else Str = "CASE"
           or else Str = "CONSTANT";
         when 'D' =>
            case Str (Str'First + 1) is
               when 'E' => return False
                 or else Str = "DECLARE"
                 or else Str = "DELAY"
                 or else Str = "DELTA";
               when 'I' => return Str = "DIGITS";
               when 'O' => return Str = "DO";
               when others => return False;
            end case;
         when 'E' =>
            case Str (Str'First + 1) is
               when 'L' => return False
                 or else Str = "ELSE"
                 or else Str = "ELSIF";
               when 'N' => return False
                 or else Str = "END"
                 or else Str = "ENTRY";
               when 'X' => return False
                 or else Str = "EXCEPTION"
                 or else Str = "EXIT";
               when others => return False;
            end case;
         when 'F' => return False
           or else Str = "FOR"
           or else Str = "FUNCTION";
         when 'G' => return False
           or else Str = "GENERIC"
           or else Str = "GOTO";
         when 'I' => return False
           or else Str = "IF"
           or else Str = "IN"
           or else Str = "IS";
         when 'L' => return False
           or else Str = "LIMITED"
           or else Str = "LOOP";
         when 'M' => return False
           or else Str = "MOD";
         when 'N' => return False
           or else Str = "NEW"
           or else Str = "NOT"
           or else Str = "NULL";
         when 'O' => return False
           or else Str = "OF"
           or else Str = "OR"
           or else Str = "OTHERS"
           or else Str = "OUT";
         when 'P' =>
            case Str (Str'First + 1) is
               when 'A' => return False
                 or else Str = "PACKAGE";
               when 'R' => return False
                 or else Str = "PRAGMA"
                 or else Str = "PRIVATE"
                 or else Str = "PROCEDURE"
                 or else Str = "PROTECTED";
               when others => return False;
            end case;
         when 'R' =>
            case Str (Str'First + 1) is
               when 'A' => return False
                 or else Str = "RAISE"
                 or else Str = "RANGE";
               when 'E' => return False
                 or else Str = "RECORD"
                 or else Str = "REM"
                 or else Str = "RENAMES"
                 or else Str = "RETURN"
                 or else Str = "REVERSE"
                 or else Str = "REQUEUE";
               when others => return False;
            end case;
         when 'S' => return False
           or else Str = "SELECT"
           or else Str = "SEPARATE"
           or else Str = "SUBTYPE";
         when 'T' => return False
           or else Str = "TASK"
           or else Str = "TERMINATE"
           or else Str = "THEN"
           or else Str = "TYPE"
           or else Str = "TAGGED";
         when 'U' => return False
           or else Str = "UNTIL";
         when 'W' => return False
           or else Str = "WHEN"
           or else Str = "WHILE"
           or else Str = "WITH";
         when 'X' => return False
           or else Str = "XOR";
         when others => return False;
      end case;
   end Is_Ada_Keyword;

   ---------------------
   -- Is_Public_Class --
   ---------------------

   pragma Warnings (Off);
   function Is_Public_Class (CF : Class_File) return Boolean is
      Is_Public : Boolean;
      Ignore    : Boolean;
   begin
      Check_Visibility (CF, Is_Public, Ignore);
      return Is_Public;
   end Is_Public_Class;
   pragma Warnings (On);

   ----------------------
   -- Is_Public_Member --
   ----------------------

   function Is_Public_Member (M : Member_Info; T : CP.Table) return Boolean is
      use Member_Attribute;

      Descriptor : constant String :=
        To_String (J_Basics.Get_Utf8 (T, M.Descriptor_Index));

      K : Natural;
      Start_Class : Natural;

   begin
      if not Access_Rights_OK (M.Access_Flags) then
         return False;
      end if;

      --  If the Member is Deprecated or Syntetic skip it

      for J in 0 .. Last (M.Attributes) loop
         case Get (M.Attributes, J).Kind is
            when Attr_Deprecated
              |  Attr_Synthetic  =>
               return False;
            when others =>
               null;
         end case;
      end loop;

      K := Descriptor'First;
      while K < Descriptor'Last loop
         case Descriptor (K) is
            when JVM_Class  =>
               K := K + 1;
               Start_Class := K;

               while Descriptor (K) /= ';' loop
                  K := K + 1;
                  if K > Descriptor'Last then
                     Osint.Fail ("Bad type descriptor: " & Descriptor);
                  end if;
               end loop;
               if
                 not Get_Info (Descriptor (Start_Class .. K - 1)).Is_Public
               then
                  return False;
               end if;

            when others =>
               K := K + 1;
         end case;
      end loop;

      return True;
   end Is_Public_Member;

   ------------------
   -- Pretty_Print --
   ------------------

   package body Pretty_Print is

      ----------------
      -- Close_File --
      ----------------

      procedure Close_File is
      begin
         Ada.Text_IO.Close (Output_File);
      end Close_File;

      --------------------
      -- Current_Column --
      --------------------

      function Current_Column return Ada.Text_IO.Count is
      begin
         return Ada.Text_IO.Col (Output_File);
      end Current_Column;

      -----------------
      -- Incr_Indent --
      -----------------

      procedure Incr_Indent (Step : Integer) is
      begin
         Current_Indentation := Current_Indentation + Step * 3;
      end Incr_Indent;

      ----------------
      -- Set_Tmp_Indent --
      ----------------

      procedure Set_Tmp_Indent (Value : Ada.Text_IO.Count) is
      begin
         Ada.Text_IO.Set_Col
           (Output_File, Ada.Text_IO.Positive_Count (Value));
      end Set_Tmp_Indent;

      ---------------
      -- Open_File --
      ---------------

      function Open_File (Class_Name : String) return Boolean is
         use Ada.Characters.Handling;
         use Ada.Text_IO;

         File : String := String (Class_Name);
         Len  : Natural := File'Length;

      begin
         --  Convert the Class_Name into a file name
         for Index in File'Range loop
            if File (Index) = '.'
              or else File (Index) = '/'
              or else File (Index) = '\'
              or else File (Index) = '$'
            then
               File (Index) := '-';
            end if;
         end loop;

         --  Convert the package name to a file name
         for K in File'Range loop
            if Osint.Is_Directory_Separator (File (K)) then
               File (K) := '-';
            else
               File (K) := To_Lower (File (K));
            end if;
         end loop;

         --  If there is already an open output file (from a previous class)
         --  we just close it.

         if Ada.Text_IO.Is_Open (Output_File) then
            Close_File;
         end if;

         declare
            S : String := Output_Dir.all
              & File (File'First .. File'First + Len - 1) & ".ads";
         begin
            --  Do not allow overwritting a file, unless the user specified
            --  it is ok to do so

            if GNAT.IO_Aux.File_Exists (S) and not Overwrite_Files then
               Put_Line
                 ("*** Can't create " & S & ": file exists - skipping "
                  & Class_Name);
               return False;
            else
               declare
               begin
                  Ada.Text_IO.Create (Output_File, Name => S);
               exception
                  when others =>
                     Put_Line
                       ("*** Can't create " & S & ": file error - skipping "
                        & Class_Name);
                     return False;
               end;
            end if;
         end;
         return True;
      end Open_File;

      -----------
      -- Print --
      -----------

      procedure Print (S : String) is
      begin
         Ada.Text_IO.Put (Output_File, S);
      end Print;

      ----------------
      -- Print_Line --
      ----------------

      procedure Print_Line (S : String := "") is
      begin
         Ada.Text_IO.Put_Line (Output_File, String (S));
         Ada.Text_IO.Set_Col
           (Output_File, Ada.Text_IO.Positive_Count (Current_Indentation));
      end Print_Line;

   end Pretty_Print;

   ------------------
   -- Print_Fields --
   ------------------

   procedure Print_Fields (CF            : Class_File;
                           Static_Fields : Boolean;
                           Pragma_List   : in out String_List.List)
   is
      use J_Basics;
      use Member_Attribute;

      function Field_Name (F_Name : String) return String;
      --  Given the original field name F_Name returns the Ada identifier to
      --  use.

      function Is_Constant_Field (F : Member_Info) return Boolean;
      --  Returns True if field F is a constant.

      ----------------
      -- Field_Name --
      ----------------

      function Field_Name (F_Name : String) return String is
         Ada_Name   : constant String := Ada_Identifier (F_Name);
         Ada_Name_U : constant String := To_Upper (Ada_Name);
         Nb_Clashes : Integer;

      begin
         if not Static_Fields or else Keep_Original_Identifiers then
            return Ada_Name;
         end if;

         Nb_Clashes := Name_Table.Get (Used_Names, Ada_Name_U);

         if Nb_Clashes < 1 then
            Name_Table.Set (Used_Names, Ada_Name_U, 1);
            return Ada_Name;

         elsif Nb_Clashes = 1 then
            Name_Table.Set (Used_Names, Ada_Name_U, 2);
            return Ada_Name & "_K";

         else
            Name_Table.Set (Used_Names, Ada_Name_U, Nb_Clashes + 1);
            return Ada_Name & "_K" & Image (U4 (Nb_Clashes));
         end if;
      end Field_Name;

      -----------------------
      -- Is_Constant_Field --
      -----------------------

      function Is_Constant_Field (F : in Member_Info) return Boolean is
      begin
         for K in 0 .. Member_Attribute.Last (F.Attributes) loop
            if Get (F.Attributes, K).Kind = Attr_Constant_Value then
               return True;
            end if;
         end loop;
         return False;
      end Is_Constant_Field;

      T : constant CP.Table := CF.Constant_Pool;
      F : Member_Info;

      Field_Printed : Boolean := False;
      --  Set to True if we printed at least one field declaration

   --  Beginning of Print_Fields

   begin
      for K in 0 .. Member.Last (CF.Fields) loop
         F := Member.Get (CF.Fields, K);

         if Is_Set (F.Access_Flags, ACC_Static) = Static_Fields
           and then Is_Public_Member (F, T)
         then
            if Field_Printed then
               PL;
            else
               if Static_Fields then
                  PL;
                  PL ("---------------------------");
                  PL ("-- Variable Declarations --");
                  PL ("---------------------------");
                  PL;
               else
                  Pretty_Print.Incr_Indent (1);
                  PL;
                  PL ("with record");
                  Pretty_Print.Incr_Indent (1);
                  PL;
                  PL ("------------------------");
                  PL ("-- Field Declarations --");
                  PL ("------------------------");
                  PL;
               end if;
            end if;

            Field_Printed := True;

            declare
               F_Name       : constant String :=
                 To_String (Get_Utf8 (T, F.Name_Index));
               Ada_F_Name   : constant String := Field_Name (F_Name);
               F_Descriptor : constant String :=
                 To_String (Get_Utf8 (T, F.Descriptor_Index));

               Is_Protected : constant Boolean :=
                 Is_Set (F.Access_Flags, ACC_Protected);
               Is_Final     : constant Boolean :=
                 Is_Set (F.Access_Flags, ACC_Final);

            begin
               if Is_Protected or else Is_Final then
                  P ("--");

                  if Is_Protected then
                     P ("  protected");
                  end if;

                  if Is_Final then
                     P ("  final");
                  end if;

                  PL;
               end if;

               P (Ada_F_Name);
               P (" : ");

               if Is_Constant_Field (F) and then Static_Fields then
                  P ("constant ");
               end if;

               P (Ada_Type (F_Descriptor));
               PL (";");

               if not Static_Fields then
                  PL ("pragma Import (Java, " & Ada_F_Name & ", """
                      & F_Name & """);");
               else
                  String_List.Append_If_Uniq
                    ("pragma Import (Java, " & Ada_F_Name & ", """
                     & F_Name & """);",
                     Pragma_List);
               end if;
            end;
         end if;
      end loop;

      if not Static_Fields then
         if not Field_Printed then
            Pretty_Print.Incr_Indent (1);
            PL;
            P ("with null record;");
            Pretty_Print.Incr_Indent (-1);
            PL;
         else
            Pretty_Print.Incr_Indent (-1);
            PL;
            Pretty_Print.Incr_Indent (-1);
            PL ("end record;");
         end if;
      end if;
   end Print_Fields;

   -------------------
   -- Print_Methods --
   -------------------

   procedure Print_Methods (CF          : Class_File;
                            Pragma_List : in out String_List.List)
   is
      use Member_Attribute;
      use J_Basics;
      use JVM_File;

      Class_Name : constant String   := Get_String (CF, CF.This_Class);
      T          : constant CP.Table := CF.Constant_Pool;

      Constructor_Printed : Boolean := False;
      Method_Printed      : Boolean := False;

      function Next_Declaration_Pos (Descriptor : String) return Natural;
      --  Descriptor is a method descriptor or a the tail of a mathod
      --  descriptor.  This routine returns the first position in Descriptor of
      --  the next valid type descriptor. If we have reached the end of the
      --  method descriptor 0 is returned.

      procedure Process_Method (F : Access_Mask; Name : String; D : String);
      --  Process the method whose flags are given in F, whose name is Name and
      --  whose descriptor is D.

      --------------------------
      -- Next_Declaration_Pos --
      --------------------------

      function Next_Declaration_Pos (Descriptor : String) return Natural is
         Pos : Natural := Descriptor'First;
      begin
         case Descriptor (Pos) is
            when JVM_Class =>
               while Descriptor (Pos) /= ';' loop
                  Pos := Pos + 1;
               end loop;
               return Pos + 1;

            when JVM_Array =>
               while Descriptor (Pos) = JVM_Array loop
                  Pos := Pos + 1;
               end loop;
               return
                 Next_Declaration_Pos (Descriptor (Pos .. Descriptor'Last));

            when ')' =>
               return 0;

            when others =>
               return Pos + 1;
         end case;
      end Next_Declaration_Pos;

      --------------------
      -- Process_Method --
      --------------------

      procedure Process_Method (F : Access_Mask; Name : String; D : String) is
         Is_Procedure    : constant Boolean := D (D'Last) = 'V';
         Is_Constructor  : constant Boolean := Name (Name'First) = '<';

         Is_Static       : constant Boolean := Is_Set (F, ACC_Static);
         Is_Protected    : constant Boolean := Is_Set (F, ACC_Protected);
         Is_Final        : constant Boolean := Is_Set (F, ACC_Final);
         Is_Synchronized : constant Boolean := Is_Set (F, ACC_Synchronized);
         Is_Abstract     : constant Boolean :=
           Is_Set (CF.Access_Flags, ACC_Interface) or Is_Set (F, ACC_Abstract);

         Has_Parameters : constant Boolean :=
           D (D'First + 1) /= ')' or not Is_Static;

         Current_Pos : Natural := D'First + 1;
         End_Pos     : Natural;
         Param       : U2 := 1;
         Start_Col   : Ada.Text_IO.Positive_Count := 1;

         Need_Semicolon : Boolean := False;

         Subprog_Name     : constant String := Ada_Identifier (Name);
         Constructor_Name : constant String :=
           "new_" & Get_Identifier (Class_Name, Short => True);

      begin
         if Is_Constructor then
            if not Constructor_Printed then
               PL;
               PL ("------------------------------");
               PL ("-- Constructor Declarations --");
               PL ("------------------------------");
               Constructor_Printed := True;
            end if;

         elsif not Method_Printed then
            PL;
            PL ("-------------------------");
            PL ("-- Method Declarations --");
            PL ("-------------------------");
            Method_Printed := True;
         end if;
         PL;

         if Is_Final or else Is_Protected or else Is_Synchronized then
            P ("--");

            if Is_Final then
               P ("  final");
            end if;

            if Is_Protected then
               P ("  protected");
            end if;

            if Is_Synchronized then
               P ("  synchronized");
            end if;

            PL;
         end if;

         if Is_Constructor then
            P ("function " & Constructor_Name & " (");
            Name_Table.Set (Used_Names, To_Upper (Constructor_Name), 1);

         else
            if Is_Procedure then
               P ("procedure " & Subprog_Name);
            else
               P ("function "  & Subprog_Name);
            end if;
            Name_Table.Set (Used_Names, To_Upper (Subprog_Name), 1);

            --  For non-static methods, the first argument is 'This'

            if Has_Parameters then
               P (" (");
            end if;
         end if;

         Start_Col := Pretty_Print.Current_Column;

         if not Is_Static and then not Is_Constructor then
            P ("This : access Typ");
            Need_Semicolon := True;
         end if;

         --  Print the arguments

         while D (Current_Pos) /= ')' loop
            End_Pos := Next_Declaration_Pos (D (Current_Pos .. D'Last));

            if Need_Semicolon then
               PL ("; ");
               Pretty_Print.Set_Tmp_Indent (Start_Col);
            end if;

            P ("P" & Image (Param) & "_"
               & Ada_Type (D (Current_Pos .. End_Pos - 1),
                           Format => Use_As_Name)
               & " : ");
            P (Ada_Type (D (Current_Pos .. End_Pos - 1),
                         Format => Parameter_Type));

            Param := Param + 1;
            Current_Pos := End_Pos;

            Need_Semicolon := True;
         end loop;

         --  If we had a constructor

         if Is_Constructor then
            if Need_Semicolon then
               PL ("; ");
               Pretty_Print.Set_Tmp_Indent (Start_Col);
            end if;

            PL ("This : Ref := null)");
            Pretty_Print.Set_Tmp_Indent (Start_Col);
            PL ("return Ref;");

            String_List.Append_If_Uniq
              ("pragma Java_Constructor (" & Constructor_Name & ");",
               Pragma_List);

         else
            if Has_Parameters or else not Is_Static then
               P (")");
            end if;

            if not Is_Procedure then
               PL;
               Pretty_Print.Set_Tmp_Indent (Start_Col);
               P ("return " & Ada_Type (D (Current_Pos + 1 .. D'Last)));
            end if;

            --  Special case for abstract procedures and methods

            if Is_Abstract then
               P (" is abstract");
            end if;

            PL (";");

            String_List.Append_If_Uniq
              ("pragma Import (Java, " & Subprog_Name & ", """ & Name & """);",
               Pragma_List);
         end if;
      end Process_Method;

      M : Member_Info;

   --  Beginning of Print_Methods

   begin
      for K in 0 .. Member.Last (CF.Methods) loop
         M := Member.Get (CF.Methods, K);

         if Is_Public_Member (M, T) then
            declare
               Name : constant String :=
                 To_String (Get_Utf8 (T, M.Name_Index));
            begin
               if Name /= "<clinit>" then
                  Process_Method
                    (M.Access_Flags,
                     Name,
                     To_String (Get_Utf8 (T, M.Descriptor_Index)));
               end if;
            end;
         end if;
      end loop;
   end Print_Methods;

   ---------------------------
   -- Print_Obj_Declaration --
   ---------------------------

   procedure Print_Obj_Declaration (CF           : Class_File;
                                    Pragma_List  : in out String_List.List)
   is
      use type Ada.Text_IO.Count;
      use String_List;

      Class_Name : constant String := Get_String (CF, CF.This_Class);
      Info       : constant Class_Info := Get_Info (Class_Name);

      Super_Name : constant String     := Get_Public_Super_Class (CF);
      Super_Info : constant Class_Info := Get_Info (Super_Name);

      Iter       : String_List.List_Iterator;

      Is_Interface      : constant Boolean :=
        Is_Set (CF.Access_Flags, ACC_Interface);

      Has_Discriminants : constant Boolean :=
        Is_Interface or else Info.Implements /= Empty_List;

      Start_Col : Ada.Text_IO.Count;

   begin
      P ("type Typ ");

      --  Print the discriminants associated with the interface CF implements

      if Has_Discriminants then
         P ("(");
         Start_Col := Pretty_Print.Current_Column;
         Associate (Info.Implements, Iter);

         if Is_Interface then
            P ("Self : access java.lang.Object.Typ'Class");
         else
            P (Get_Identifier (Get (Iter), Short => True) & "_I"
               & " : " & Get_Identifier (Get (Iter)) & ".Ref");
            Next (Iter);
         end if;

         while not Is_Last (Iter) loop
            PL (";");
            Pretty_Print.Set_Tmp_Indent (Start_Col);
            P (Get_Identifier (Get (Iter), Short => True) & "_I"
               & " : " & Get_Identifier (Get (Iter)) & ".Ref");
            Next (Iter);
         end loop;

         Pretty_Print.Incr_Indent (1);
         PL (")");
      end if;

      P ("is ");

      if Has_Discriminants then
         Pretty_Print.Incr_Indent (-1);
      end if;

      if Is_Set (CF.Access_Flags, ACC_Abstract) then
         P ("abstract ");
      end if;

      --  Print the super class info

      P ("new " & Get_Identifier (Super_Name) & ".Typ ");

      --  Constrain the parent type with the discriminants that correspond to
      --  the interfaces the parent class implements if any.

      if Super_Info.Implements /= Empty_List then
         P ("(");
         Start_Col := Pretty_Print.Current_Column;
         Associate (Super_Info.Implements, Iter);

         P (Get_Identifier (Get (Iter), Short => True) & "_I");
         Next (Iter);

         while not Is_Last (Iter) loop
            PL (",");
            Pretty_Print.Set_Tmp_Indent (Start_Col);
            P (Get_Identifier (Get (Iter), Short => True) & "_I");
            Next (Iter);
         end loop;

         P (")");
      end if;

      Print_Fields (CF, Static_Fields => False, Pragma_List => Pragma_List);

      if Is_Interface then
         PL ("pragma Java_Interface (Typ);");
         PL;
      end if;

      if Get_Info (Class_Name).Is_Exception then
         PL ("---------------------------");
         PL ("-- Exception Declaration --");
         PL ("---------------------------");
         PL;
         PL ("Except : Exception;");
         PL;
         Append_If_Uniq
           ("pragma Import (Java, Except, """ & Dottify (Class_Name) & """);",
            Pragma_List);
      end if;
   end Print_Obj_Declaration;

   ----------------
   -- Print_With --
   ----------------

   procedure Print_With (CF : Class_File) is
      Class_Name : constant String   := Get_String (CF, CF.This_Class);
      T          : constant CP.Table := CF.Constant_Pool;

      With_List : String_List.List;
      --  This is the list of all the 'with' statements to emit.  The strings
      --  in this list should include everything to be printed except for the
      --  initial "with " and final ";".

      procedure Print_With_Recursive (CF : Class_File);
      --  Collect the 'with' statements for CF interfaces and super class
      --  and call the procedure below for each of its public members.

      procedure Print_With_Descriptor (CF : Class_File; M : Member_Info);
      --  Collect the with-type statements for field or method M. M's
      --  descriptor is parsed for all the class names that appear in it, and
      --  the required with-type are emitted. We need to generate a with-type
      --  access clause for field types and function return types, while for
      --  parameters we need with-type tagged clause.

      ---------------------------
      -- Print_With_Descriptor --
      ---------------------------

      procedure Print_With_Descriptor (CF : Class_File; M : Member_Info) is
         D : constant Utf8.Table := J_Basics.Get_Utf8 (T, M.Descriptor_Index);
         W : constant String     := To_String (D);

         Index : Natural := W'First;
         Save  : Natural;

         Array_Dim : U1 := 0;

         Is_Param_Type : Boolean := False;
         --  True when processing parameter types

      begin
         while Index <= W'Last loop
            case W (Index) is
               when '(' | ')' =>
                  Is_Param_Type := W (Index) = '(';
                  Array_Dim := 0;
                  Index     := Index + 1;

               when JVM_Array =>
                  Array_Dim := 0;
                  loop
                     Array_Dim := Array_Dim + 1;
                     Index := Index + 1;
                     exit when W (Index) /= JVM_Array;
                  end loop;

               when JVM_Class =>
                  Save := Index + 1;
                  while W (Index) /= ';' loop
                     Index := Index + 1;
                  end loop;
                  Index := Index + 1;

                  declare
                     Id     : constant String := W (Save .. Index - 2);
                     Ada_Id : constant String := Get_Identifier (Id);
                  begin
                     if Id /= Class_Name then
                        if Array_Dim = 0 then
                           if not Is_Param_Type then
                              String_List.Append_If_Uniq
                                ("type " & Ada_Id & ".Ref is access",
                                 With_List);
                           else
                              String_List.Append_If_Uniq
                                ("type " & Ada_Id & ".Typ is tagged",
                                 With_List);
                           end if;

                        elsif Array_Dim = 1 then
                           String_List.Append_If_Uniq
                             ("type " & Ada_Id & ".Arr is access",
                              With_List);

                        else
                           String_List.Append_If_Uniq
                             ("type " & Ada_Id & ".Arr_"
                              & Image (Array_Dim) & " is access",
                              With_List);
                        end if;
                     end if;
                     Array_Dim := 0;
                  end;

               when others =>
                  Array_Dim := 0;
                  Index     := Index + 1;
            end case;
         end loop;
      end Print_With_Descriptor;

      --------------------------
      -- Print_With_Recursive --
      --------------------------

      procedure Print_With_Recursive (CF : Class_File) is
         use String_List;

         Iter : String_List.List_Iterator;

      begin
         --  Print a with statement for the parent class except when there is
         --  no parent package.

         if Class_Name /= "java/lang/Object" then
            Append (Get_Identifier (Get_Public_Super_Class (CF)), With_List);
         end if;

         --  Then print a with-type is access clause for every public interface
         --  the current class or its parents extends (non public interfaces
         --  have not been added to the Implements list collected by Get_Info).

         Associate (Get_Info (Class_Name).Implements, Iter);
         while not Is_Last (Iter) loop
            Append_If_Uniq
              ("type " & Get_Identifier (Get (Iter)) & ".Ref is access",
               With_List);
            Next (Iter);
         end loop;

         --  Print the with_type statements for the fields

         for K in 0 .. Member.Last (CF.Fields) loop
            declare
               Field : constant Member_Info := Member.Get (CF.Fields, K);
            begin
               if Is_Public_Member (Field, T) then
                  Print_With_Descriptor (CF, Field);
               end if;
            end;
         end loop;

         --  Print the with_type statements for the methods

         for K in 0 .. Member.Last (CF.Methods) loop
            declare
               Method : constant Member_Info := Member.Get (CF.Methods, K);
            begin
               if Is_Public_Member (Method, T) then
                  Print_With_Descriptor (CF, Method);
               end if;
            end;
         end loop;
      end Print_With_Recursive;

      Iter : String_List.List_Iterator;

   --  Beginning of Print_With

   begin
      PL ("with Java; use Java;");

      Print_With_Recursive (CF);
      Sort (With_List);

      String_List.Associate (With_List, Iter);
      while not String_List.Is_Last (Iter) loop
         PL ("with " & String_List.Get (Iter) & ";");
         String_List.Next (Iter);
      end loop;

      String_List.Clean (With_List);
   end Print_With;

   -----------------------
   -- Search_Classes_In --
   -----------------------

   procedure Search_Classes_In (Zip : String) is
      procedure Search_Classes_Instance is
         new Generic_Search_In (The_Search_List => Classes_Search_List);
   begin
      Search_Classes_Instance (Zip);
   end Search_Classes_In;

   -----------------------
   -- Search_Sources_In --
   -----------------------

   procedure Search_Sources_In (Zip : String) is
      procedure Search_Sources_Instance is
         new Generic_Search_In (The_Search_List => Sources_Search_List);
   begin
      Search_Sources_Instance (Zip);
   end Search_Sources_In;

   -------------
   -- Warning --
   -------------

   procedure Warning (Msg : String) is
   begin
      if not Quiet_Mode then
         Ada.Text_IO.Put_Line ("*** " & Msg);
      end if;
   end Warning;

begin
   Name_Table.Clear (Used_Names);

   --  Add entries for "java/lang/Object" and "java/lang/Throwable" to
   --  the Symbol_Table.

   declare
      Serializable : String_List.List;
   begin

      Symbol_Table_Pkg.Set
        (Symbol_Table,
         Name  => "java/lang/Object",
         Value => (Super_Class_Name => new String'(""),
                   Implements       => String_List.Empty_List,
                   Is_Public        => True,
                   Is_Exception     => False));

      String_List.Append ("java/io/Serializable", Serializable);

      Symbol_Table_Pkg.Set
        (Symbol_Table,
         Name  => "java/lang/Throwable",
         Value => (Super_Class_Name => new String'("java/lang/Object"),
                   Implements       => Serializable,
                   Is_Public        => True,
                   Is_Exception     => True));
   end;
end JVM_Ada;
