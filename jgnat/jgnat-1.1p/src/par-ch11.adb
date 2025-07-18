------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . C H 1 1                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.21 $                             --
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

with Sinfo.CN; use Sinfo.CN;

separate (Par)
package body Ch11 is

   --  Local functions, used only in this chapter

   function P_Exception_Handler  return Node_Id;
   function P_Exception_Choice   return Node_Id;

   ---------------------------------
   -- 11.1  Exception Declaration --
   ---------------------------------

   --  Parsed by P_Identifier_Declaration (3.3.1)

   ------------------------------------------
   -- 11.2  Handled Sequence Of Statements --
   ------------------------------------------

   --  HANDLED_SEQUENCE_OF_STATEMENTS ::=
   --      SEQUENCE_OF_STATEMENTS
   --    [exception
   --      EXCEPTION_HANDLER
   --      {EXCEPTION_HANDLER}]

   --  Error_Recovery : Cannot raise Error_Resync

   function P_Handled_Sequence_Of_Statements return Node_Id is
      Handled_Stmt_Seq_Node : Node_Id;

   begin
      Handled_Stmt_Seq_Node :=
        New_Node (N_Handled_Sequence_Of_Statements, Token_Ptr);
      Set_Statements
        (Handled_Stmt_Seq_Node, P_Sequence_Of_Statements (SS_Extm_Sreq));

      if Token = Tok_Exception then
         Scan; -- past EXCEPTION
         Set_Exception_Handlers
           (Handled_Stmt_Seq_Node, Parse_Exception_Handlers);
      end if;

      return Handled_Stmt_Seq_Node;
   end P_Handled_Sequence_Of_Statements;

   -----------------------------
   -- 11.2  Exception Handler --
   -----------------------------

   --  EXCEPTION_HANDLER ::=
   --    when [CHOICE_PARAMETER_SPECIFICATION :]
   --      EXCEPTION_CHOICE {| EXCEPTION_CHOICE} =>
   --        SEQUENCE_OF_STATEMENTS

   --  CHOICE_PARAMETER_SPECIFICATION ::= DEFINING_IDENTIFIER

   --  Error recovery: cannot raise Error_Resync

   function P_Exception_Handler return Node_Id is
      Scan_State        : Saved_Scan_State;
      Handler_Node      : Node_Id;
      Choice_Param_Node : Node_Id;

   begin
      Handler_Node := New_Node (N_Exception_Handler, Token_Ptr);
      T_When;

      --  Test for possible choice parameter present

      if Token = Tok_Identifier then
         Choice_Param_Node := Token_Node;
         Save_Scan_State (Scan_State); -- at identifier
         Scan; -- past identifier

         if Token = Tok_Colon then
            if Ada_83 then
               Error_Msg_SP ("(Ada 83) choice parameter not allowed!");
            end if;

            Scan; -- past :
            Change_Identifier_To_Defining_Identifier (Choice_Param_Node);
            Set_Choice_Parameter (Handler_Node, Choice_Param_Node);

         elsif Token = Tok_Others then
            Error_Msg_AP ("missing "":""");
            Change_Identifier_To_Defining_Identifier (Choice_Param_Node);
            Set_Choice_Parameter (Handler_Node, Choice_Param_Node);

         else
            Restore_Scan_State (Scan_State); -- to identifier
         end if;
      end if;

      --  Loop through exception choices

      Set_Exception_Choices (Handler_Node, New_List);

      loop
         Append (P_Exception_Choice, Exception_Choices (Handler_Node));
         exit when Token /= Tok_Vertical_Bar;
         Scan; -- past vertical bar
      end loop;

      TF_Arrow;
      Set_Statements (Handler_Node, P_Sequence_Of_Statements (SS_Sreq_Whtm));
      return Handler_Node;
   end P_Exception_Handler;

   ------------------------------------------
   -- 11.2  Choice Parameter Specification --
   ------------------------------------------

   --  Parsed by P_Exception_Handler (11.2)

   ----------------------------
   -- 11.2  Exception Choice --
   ----------------------------

   --  EXCEPTION_CHOICE ::= exception_NAME | others

   --  Error recovery: cannot raise Error_Resync. If an error occurs, then the
   --  scan pointer is advanced to the next arrow or vertical bar or semicolon.

   function P_Exception_Choice return Node_Id is
   begin

      if Token = Tok_Others then
         Scan; -- past OTHERS
         return New_Node (N_Others_Choice, Prev_Token_Ptr);

      else
         return P_Name; -- exception name
      end if;

   exception
      when Error_Resync =>
         Resync_Choice;
         return Error;
   end P_Exception_Choice;

   ---------------------------
   -- 11.3  Raise Statement --
   ---------------------------

   --  RAISE_STATEMENT ::= raise [exception_NAME];

   --  The caller has verified that the initial token is RAISE

   --  Error recovery: can raise Error_Resync

   function P_Raise_Statement return Node_Id is
      Raise_Node : Node_Id;

   begin
      Raise_Node := New_Node (N_Raise_Statement, Token_Ptr);
      Scan; -- past RAISE

      if Token /= Tok_Semicolon then
         Set_Name (Raise_Node, P_Name);
      end if;

      TF_Semicolon;
      return Raise_Node;
   end P_Raise_Statement;

   ------------------------------
   -- Parse_Exception_Handlers --
   ------------------------------

   --  This routine scans out a list of exception handlers appearing in a
   --  construct as:

   --    exception
   --      EXCEPTION_HANDLER {EXCEPTION_HANDLER}

   --  The caller has scanned out the EXCEPTION keyword

   --  Control returns after scanning the last exception handler, presumably
   --  at the keyword END, but this is not checked in this routine.

   --  Error recovery: cannot raise Error_Resync

   function Parse_Exception_Handlers return List_Id is
      Handler       : Node_Id;
      Handlers_List : List_Id;
      Pragmas_List  : List_Id;

   begin
      Handlers_List := New_List;
      P_Pragmas_Opt (Handlers_List);

      if Token = Tok_End then
         Error_Msg_SC ("must have at least one exception handler!");

      else
         loop
            Handler := P_Exception_Handler;
            Pragmas_List := No_List;
            Append (Handler, Handlers_List);

            --  Note: no need to check for pragmas here. Although the
            --  syntax officially allows them in this position, they
            --  will have been swallowed up as part of the statement
            --  sequence of the handler we just scanned out.

            exit when Token /= Tok_When;
         end loop;
      end if;

      return Handlers_List;
   end Parse_Exception_Handlers;

end Ch11;
