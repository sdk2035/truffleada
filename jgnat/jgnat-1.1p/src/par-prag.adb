------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . P R A G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.128 $
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

--  Generally the parser checks the basic syntax of pragmas, but does not
--  do specialized syntax checks for individual pragmas, these are deferred
--  to semantic analysis time (see unit Sem_Prag). There are some pragmas
--  which require recognition and either partial or complete processing
--  during parsing, and this unit performs this required processing.

with Osint;   use Osint;
with Stringt; use Stringt;
with Stylesw; use Stylesw;
with Uintp;   use Uintp;
with Uname;   use Uname;

separate (Par)

function Prag (Pragma_Node : Node_Id; Semi : Source_Ptr) return Node_Id is
   Pragma_Name : constant Name_Id    := Chars (Pragma_Node);
   Pragma_Sloc : constant Source_Ptr := Sloc (Pragma_Node);
   Arg_Count   : Nat;
   Arg_Node    : Node_Id;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Arg1 return Node_Id;
   function Arg2 return Node_Id;
   function Arg3 return Node_Id;
   --  Obtain specified Pragma_Argument_Association. It is allowable to call
   --  the routine for the argument one past the last present argument, but
   --  that is the only case in which a non-present argument can be referenced.

   procedure Check_Arg_Count (Required : Int);
   --  Check argument count for pragma = Required.
   --  If not give error and raise Error_Resync.

   procedure Check_Arg_Is_String_Literal (Arg : Node_Id);
   --  Check the expression of the specified argument to make sure that it
   --  is a string literal. If not give error and raise Error_Resync.

   procedure Check_Arg_Is_On_Or_Off (Arg : Node_Id);
   --  Check the expression of the specified argument to make sure that it
   --  is an identifier which is either ON or OFF, and if not, then issue
   --  an error message and raise Error_Resync.

   procedure Check_No_Identifier (Arg : Node_Id);
   --  Checks that the given argument does not have an identifier. If an
   --  identifier is present, then an error message is issued, and
   --  Error_Resync is raised.

   procedure Check_Optional_Identifier (Arg : Node_Id; Id : Name_Id);
   --  Checks if the given argument has an identifier, and if so, requires
   --  it to match the given identifier name. If there is a non-matching
   --  identifier, then an error message is given and Error_Resync raised.

   ----------
   -- Arg1 --
   ----------

   function Arg1 return Node_Id is
   begin
      return First (Pragma_Argument_Associations (Pragma_Node));
   end Arg1;

   ----------
   -- Arg2 --
   ----------

   function Arg2 return Node_Id is
   begin
      return Next (Arg1);
   end Arg2;

   ----------
   -- Arg3 --
   ----------

   function Arg3 return Node_Id is
   begin
      return Next (Arg2);
   end Arg3;

   ---------------------
   -- Check_Arg_Count --
   ---------------------

   procedure Check_Arg_Count (Required : Int) is
   begin
      if Arg_Count /= Required then
         Error_Msg ("wrong number of arguments for pragma%", Pragma_Sloc);
         raise Error_Resync;
      end if;
   end Check_Arg_Count;

   ----------------------------
   -- Check_Arg_Is_On_Or_Off --
   ----------------------------

   procedure Check_Arg_Is_On_Or_Off (Arg : Node_Id) is
      Argx : constant Node_Id := Expression (Arg);

   begin
      if Nkind (Expression (Arg)) /= N_Identifier
        or else (Chars (Argx) /= Name_On
                   and then
                 Chars (Argx) /= Name_Off)
      then
         Error_Msg_Name_2 := Name_On;
         Error_Msg_Name_3 := Name_Off;

         Error_Msg
           ("argument for pragma% must be% or%", Sloc (Argx));
         raise Error_Resync;
      end if;
   end Check_Arg_Is_On_Or_Off;

   ---------------------------------
   -- Check_Arg_Is_String_Literal --
   ---------------------------------

   procedure Check_Arg_Is_String_Literal (Arg : Node_Id) is
   begin
      if Nkind (Expression (Arg)) /= N_String_Literal then
         Error_Msg
           ("argument for pragma% must be string literal",
             Sloc (Expression (Arg)));
         raise Error_Resync;
      end if;
   end Check_Arg_Is_String_Literal;

   -------------------------
   -- Check_No_Identifier --
   -------------------------

   procedure Check_No_Identifier (Arg : Node_Id) is
   begin
      if Chars (Arg) /= No_Name then
         Error_Msg_N ("pragma% does not permit named arguments", Arg);
         raise Error_Resync;
      end if;
   end Check_No_Identifier;

   -------------------------------
   -- Check_Optional_Identifier --
   -------------------------------

   procedure Check_Optional_Identifier (Arg : Node_Id; Id : Name_Id) is
   begin
      if Present (Arg) and then Chars (Arg) /= No_Name then
         if Chars (Arg) /= Id then
            Error_Msg_Name_2 := Id;
            Error_Msg_N ("pragma% argument expects identifier%", Arg);
            raise Error_Resync;
         end if;
      end if;
   end Check_Optional_Identifier;

   ----------
   -- Prag --
   ----------

begin
   Error_Msg_Name_1 := Pragma_Name;

   --  Count number of arguments. This loop also checks if any of the arguments
   --  are Error, indicating a syntax error as they were parsed. If so, we
   --  simply return, because we get into trouble with cascaded errors if we
   --  try to perform our error checks on junk arguments.

   Arg_Count := 0;

   if Present (Pragma_Argument_Associations (Pragma_Node)) then
      Arg_Node := Arg1;

      while Arg_Node /= Empty loop
         Arg_Count := Arg_Count + 1;

         if Expression (Arg_Node) = Error then
            return Error;
         end if;

         Next (Arg_Node);
      end loop;
   end if;

   --  Remaining processing is pragma dependent

   case Get_Pragma_Id (Pragma_Name) is

      ------------
      -- Ada_83 --
      ------------

      --  This pragma must be processed at parse time, since we want to set
      --  the Ada 83 and Ada 95 switches properly at parse time to recognize
      --  Ada 83 syntax or Ada 95 syntax as appropriate.

      when Pragma_Ada_83 =>
         Ada_83 := True;
         Ada_95 := False;

      ------------
      -- Ada_95 --
      ------------

      --  This pragma must be processed at parse time, since we want to set
      --  the Ada 83 and Ada_95 switches properly at parse time to recognize
      --  Ada 83 syntax or Ada 95 syntax as appropriate.

      when Pragma_Ada_95 =>
         Ada_83 := False;
         Ada_95 := True;

      -----------
      -- Debug --
      -----------

      --  pragma Debug (PROCEDURE_CALL_STATEMENT);

      --  This has to be processed by the parser because of the very peculiar
      --  form of the second parameter, which is syntactically from a formal
      --  point of view a function call (since it must be an expression), but
      --  semantically we treat it as a procedure call (which has exactly the
      --  same syntactic form, so that's why we can get away with this!)

      when Pragma_Debug =>
         Check_Arg_Count (1);
         Check_No_Identifier (Arg1);

         declare
            Expr : constant Node_Id := New_Copy (Expression (Arg1));

         begin
            if Nkind (Expr) /= N_Indexed_Component
              and then Nkind (Expr) /= N_Function_Call
              and then Nkind (Expr) /= N_Identifier
              and then Nkind (Expr) /= N_Selected_Component
            then
               Error_Msg
                 ("argument of pragma% is not procedure call", Sloc (Expr));
               raise Error_Resync;
            else
               Set_Debug_Statement
                 (Pragma_Node, P_Statement_Name (Expr));
            end if;
         end;

      -------------------------------
      -- Extensions_Allowed (GNAT) --
      -------------------------------

      --  pragma Extensions_Allowed (Off | On)

      --  The processing for pragma Extensions_Allowed must be done at
      --  parse time, since extensions mode may affect what is accepted.

      when Pragma_Extensions_Allowed =>
         Check_Arg_Count (1);
         Check_No_Identifier (Arg1);
         Check_Arg_Is_On_Or_Off (Arg1);
         Opt.Extensions_Allowed := (Chars (Expression (Arg1)) = Name_On);

      ----------------
      -- List (2.8) --
      ----------------

      --  pragma List (Off | On)

      --  The processing for pragma List must be done at parse time,
      --  since a listing can be generated in parse only mode.

      when Pragma_List =>
         Check_Arg_Count (1);
         Check_No_Identifier (Arg1);
         Check_Arg_Is_On_Or_Off (Arg1);

         --  We unconditionally make a List_On entry for the pragma, so that
         --  in the List (Off) case, the pragma will print even in a region
         --  of code with listing turned off (this is required!)

         List_Pragmas.Increment_Last;
         List_Pragmas.Table (List_Pragmas.Last) :=
           (Ptyp => List_On, Ploc => Sloc (Pragma_Node));

         --  Now generate the list off entry for pragma List (Off)

         if Chars (Expression (Arg1)) = Name_Off then
            List_Pragmas.Increment_Last;
            List_Pragmas.Table (List_Pragmas.Last) :=
              (Ptyp => List_Off, Ploc => Semi);
         end if;

      ----------------
      -- Page (2.8) --
      ----------------

      --  pragma Page;

      --  Processing for this pragma must be done at parse time, since a
      --  listing can be generated in parse only mode with semantics off.

      when Pragma_Page =>
         Check_Arg_Count (0);
         List_Pragmas.Increment_Last;
         List_Pragmas.Table (List_Pragmas.Last) := (Page, Semi);

      -----------------------------
      -- Source_File_Name (GNAT) --
      -----------------------------

      --  pragma Source_File_Name (
      --    [UNIT_NAME                        =>] unit_NAME,
      --    [BODY_FILE_NAME | SPEC_FILE_NAME] =>  STRING_LITERAL);

      --    or

      --  pragma Source_File_Name (
      --    [UNIT_NAME        =>] unit_NAME,
      --    BODY_FILE_NAME    =>  STRING_LITERAL;
      --    SUBUNIT_FILE_NAME =>  STRING_LITERAL);

      --  Note: we process this during parsing, since we need to have the
      --  source file names set well before the semantic analysis starts,
      --  since we load the spec and with'ed packages before analysis.

      when Pragma_Source_File_Name => Source_File_Name : declare
         Unam : Unit_Name_Type;

         function Get_Fname (Arg : Node_Id) return Name_Id;
         --  Process file name from 2nd or 3rd argument

         function Get_Fname (Arg : Node_Id) return Name_Id is
         begin
            String_To_Name_Buffer (Strval (Expression (Arg)));

            for J in 1 .. Name_Len loop
               if Is_Directory_Separator (Name_Buffer (J)) then
                  Error_Msg
                    ("directory separator character not allowed",
                     Sloc (Expression (Arg)) + Source_Ptr (J));
               end if;
            end loop;

            return Name_Find;
         end Get_Fname;

      --  Start of processing for Source_File_Name

      begin
         if Arg_Count /= 3 then
            Check_Arg_Count (2);
         end if;

         Check_Optional_Identifier (Arg1, Name_Unit_Name);

         if Nkind (Expression (Arg1)) = N_Identifier
           or else
             (Nkind (Expression (Arg1)) = N_Selected_Component
               and then
              Nkind (Selector_Name (Expression (Arg1))) = N_Identifier)
         then
            Unam := Get_Unit_Name (Expression (Arg1));
         else
            Error_Msg_N
              ("pragma% argument must be unit name", Expression (Arg1));
            return Pragma_Node;
         end if;

         Check_Arg_Is_String_Literal (Arg2);

         if Chars (Arg2) = Name_Spec_File_Name then
            Check_Arg_Count (2);
            Set_File_Name (Get_Spec_Name (Unam), Get_Fname (Arg2));

         elsif Chars (Arg2) = Name_Body_File_Name then
            if Arg_Count = 2 then
               Set_File_Name (Unam, Get_Fname (Arg2));

            else
               Check_Arg_Is_String_Literal (Arg3);

               if Chars (Arg3) /= Name_Subunit_File_Name then
                  Error_Msg_N
                    ("pragma% 3rd argument must have identifier " &
                     "Subunit_File_Name", Arg3);
                  return Pragma_Node;

               else
                  Set_File_Name (Unam, Get_Fname (Arg2), Get_Fname (Arg3));
               end if;
            end if;

         else
            Error_Msg_N ("pragma% argument has incorrect identifier", Arg2);
            return Pragma_Node;
         end if;

      end Source_File_Name;

      -----------------------------
      -- Source_Reference (GNAT) --
      -----------------------------

      --  pragma Source_Reference
      --    (INTEGER_LITERAL [, STRING_LITERAL] );

      --  Processing for this pragma must be done at parse time, since error
      --  messages needing the proper line numbers can be generated in parse
      --  only mode with semantic checking turned off, and indeed we usually
      --  turn off semantic checking anyway if any parse errors are found.

      when Pragma_Source_Reference =>
         if Arg_Count /= 1 then
            Check_Arg_Count (2);
            Check_No_Identifier (Arg2);
         end if;

         Check_No_Identifier (Arg1);

         if Arg_Count = 2 then
            Check_Arg_Is_String_Literal (Arg2);

            declare
               S : constant String_Id := Strval (Expression (Arg2));
               C : Char_Code;

            begin
               Name_Len := 0;

               for J in 1 .. String_Length (S) loop
                  C := Get_String_Char (S, J);
                  Name_Len := Name_Len + 1;
                  Name_Buffer (Name_Len) := Get_Character (C);
               end loop;

               Set_Reference_Name (Current_Source_File, Name_Find);
               Set_Full_Ref_Name  (Current_Source_File, Name_Find);

               if not Debug_Generated_Code then
                  Set_Debug_Source_Name (Current_Source_File, Name_Find);
               end if;
            end;
         end if;

         if Nkind (Expression (Arg1)) /= N_Integer_Literal then
            Error_Msg
              ("argument for pragma% must be integer literal",
                Sloc (Expression (Arg1)));
            raise Error_Resync;

         --  OK, this source reference pragma is effective, however, we
         --  ignore it if it is not in the first unit in the multiple unit
         --  case. This is because the only purpose in this case is to
         --  provide source pragmas for subsequent use by gnatchop.

         else
            if Num_Library_Units = 1 then
               Set_Line_Offset
                 (Current_Source_File,
                  UI_To_Int (Intval (Expression (Arg1))) - 2);
               Set_Has_Line_Offset
                 (Current_Source_File, True);
            end if;
         end if;

      -------------------------
      -- Style_Checks (GNAT) --
      -------------------------

      --  pragma Style_Checks (On | Off | ALL_CHECKS | STRING_LITERAL);

      --  This is processed by the parser since some of the style
      --  checks take place during source scanning and parsing.

      when Pragma_Style_Checks => Style_Checks : declare
         A  : constant Node_Id   := Expression (Arg1);
         S  : String_Id;
         C  : Char_Code;
         OK : Boolean := True;

      begin
         --  Two argument case is only for semantics

         if Arg_Count = 2 then
            null;

         else
            Check_Arg_Count (1);
            Check_No_Identifier (Arg1);

            if Nkind (A) = N_String_Literal then
               S   := Strval (A);

               declare
                  Slen    : Natural := Natural (String_Length (S));
                  Options : String (1 .. Slen);
                  J       : Natural;
                  Ptr     : Natural;

               begin
                  J := 1;
                  loop
                     C := Get_String_Char (S, Int (J));

                     if not In_Character_Range (C) then
                        OK := False;
                        Ptr := J;
                        exit;

                     else
                        Options (J) := Get_Character (C);
                     end if;

                     if J = Slen then
                        Set_Style_Check_Options (Options, OK, Ptr);
                        exit;

                     else
                        J := J + 1;
                     end if;
                  end loop;

                  if not OK then
                     Error_Msg
                       ("invalid style check option",
                        Sloc (Expression (Arg1)) + Source_Ptr (Ptr));
                     raise Error_Resync;
                  end if;
               end;

            elsif Nkind (A) /= N_Identifier then
               OK := False;

            elsif Chars (A) = Name_All_Checks then
               Stylesw.Set_Default_Style_Check_Options;

            elsif Chars (A) = Name_On then
               Style_Check := True;

            elsif Chars (A) = Name_Off then
               Style_Check := False;

            else
               OK := False;
            end if;

            if not OK then
               Error_Msg ("incorrect argument for pragma%", Sloc (A));
               raise Error_Resync;
            end if;
         end if;
      end Style_Checks;

      ---------------------
      -- Warnings (GNAT) --
      ---------------------

      --  pragma Warnings (On | Off, [LOCAL_NAME])

      --  The one argument case is processed by the parser, since it may
      --  control parser warnings as well as semantic warnings, and in any
      --  case we want to be absolutely sure that the range in the warnings
      --  table is set well before any semantic analysis is performed.

      when Pragma_Warnings =>
         if Arg_Count = 1 then
            Check_No_Identifier (Arg1);
            Check_Arg_Is_On_Or_Off (Arg1);

            if Chars (Expression (Arg1)) = Name_On then
               Set_Warnings_Mode_On (Pragma_Sloc);
            else
               Set_Warnings_Mode_Off (Pragma_Sloc);
            end if;
         end if;

      -----------------------
      -- All Other Pragmas --
      -----------------------

      --  For all other pragmas, checking and processing is handled
      --  entirely in Sem_Prag, and no further checking is done by Par.

      when Pragma_Abort_Defer              |
           Pragma_AST_Entry                |
           Pragma_All_Calls_Remote         |
           Pragma_Annotate                 |
           Pragma_Assert                   |
           Pragma_Asynchronous             |
           Pragma_Atomic                   |
           Pragma_Atomic_Components        |
           Pragma_Attach_Handler           |
           Pragma_CPP_Class                |
           Pragma_CPP_Constructor          |
           Pragma_CPP_Destructor           |
           Pragma_CPP_Virtual              |
           Pragma_CPP_Vtable               |
           Pragma_C_Pass_By_Copy           |
           Pragma_Comment                  |
           Pragma_Common_Object            |
           Pragma_Complex_Representation   |
           Pragma_Component_Alignment      |
           Pragma_Controlled               |
           Pragma_Convention               |
           Pragma_Discard_Names            |
           Pragma_Eliminate                |
           Pragma_Elaborate                |
           Pragma_Elaborate_All            |
           Pragma_Elaborate_Body           |
           Pragma_Export                   |
           Pragma_Export_Exception         |
           Pragma_Export_Function          |
           Pragma_Export_Object            |
           Pragma_Export_Procedure         |
           Pragma_Export_Valued_Procedure  |
           Pragma_Extend_System            |
           Pragma_External_Name_Casing     |
           Pragma_Finalize_Storage_Only    |
           Pragma_Float_Representation     |
           Pragma_Ident                    |
           Pragma_Import                   |
           Pragma_Import_Exception         |
           Pragma_Import_Function          |
           Pragma_Import_Object            |
           Pragma_Import_Procedure         |
           Pragma_Import_Valued_Procedure  |
           Pragma_Inline                   |
           Pragma_Inline_Always            |
           Pragma_Inline_Generic           |
           Pragma_Inspection_Point         |
           Pragma_Interface                |
           Pragma_Interface_Name           |
           Pragma_Interrupt_Handler        |
           Pragma_Interrupt_Priority       |
           Pragma_Java_Constructor         |
           Pragma_Java_Interface           |
           Pragma_Linker_Alias             |
           Pragma_Linker_Options           |
           Pragma_Linker_Section           |
           Pragma_Locking_Policy           |
           Pragma_Long_Float               |
           Pragma_Machine_Attribute        |
           Pragma_Main                     |
           Pragma_Main_Storage             |
           Pragma_Memory_Size              |
           Pragma_No_Return                |
           Pragma_No_Run_Time              |
           Pragma_Normalize_Scalars        |
           Pragma_Optimize                 |
           Pragma_Pack                     |
           Pragma_Passive                  |
           Pragma_Polling                  |
           Pragma_Preelaborate             |
           Pragma_Priority                 |
           Pragma_Propagate_Exceptions     |
           Pragma_Psect_Object             |
           Pragma_Pure                     |
           Pragma_Pure_Function            |
           Pragma_Queuing_Policy           |
           Pragma_Remote_Call_Interface    |
           Pragma_Remote_Types             |
           Pragma_Restrictions             |
           Pragma_Restricted_Run_Time      |
           Pragma_Ravenscar                |
           Pragma_Reviewable               |
           Pragma_Share_Generic            |
           Pragma_Shared                   |
           Pragma_Shared_Passive           |
           Pragma_Storage_Size             |
           Pragma_Storage_Unit             |
           Pragma_Stream_Convert           |
           Pragma_Subtitle                 |
           Pragma_Suppress                 |
           Pragma_Suppress_All             |
           Pragma_Suppress_Debug_Info      |
           Pragma_Suppress_Initialization  |
           Pragma_System_Name              |
           Pragma_Task_Dispatching_Policy  |
           Pragma_Task_Info                |
           Pragma_Task_Name                |
           Pragma_Task_Storage             |
           Pragma_Time_Slice               |
           Pragma_Title                    |
           Pragma_Unchecked_Union          |
           Pragma_Unimplemented_Unit       |
           Pragma_Unreserve_All_Interrupts |
           Pragma_Unsuppress               |
           Pragma_Use_VADS_Size            |
           Pragma_Volatile                 |
           Pragma_Volatile_Components      |
           Pragma_Weak_External            =>
         null;

   end case;

   return Pragma_Node;

   --------------------
   -- Error Handling --
   --------------------

exception
   when Error_Resync =>
      return Error;

end Prag;
