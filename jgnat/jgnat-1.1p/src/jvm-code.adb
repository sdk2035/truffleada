------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             J V M . C O D E                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.13 $
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

with JVM.Info; use JVM.Info;
with JVM_File; use JVM_File;
with Namet;    use Namet;
with Output;   use Output;
with GNAT.Table;
with Unchecked_Deallocation;

package body JVM.Code is

   package Code_Table is new GNAT.Table (
     Table_Component_Type => Instruction,
     Table_Index_Type     => Instr_Id,
     Table_Low_Bound      => Instr_Id'First,
     Table_Initial        => 10_000,
     Table_Increment      => 100);

   Next_Code_Index : Instr_Id := Null_Instr + 1;
   --  The table index of the next available instruction slot

   Free_List : Instr_Id := Null_Instr;
   --  A list of freed Instruction records that can be reused

   Empty_Instr : constant Instruction :=
     (Op => Xxxunusedxxx, Next => Null_Instr);
   --  The default value of an Instruction record allocated by New_Instr


   package Handler_Table is new GNAT.Table (
     Table_Component_Type => Handler_Entry,
     Table_Index_Type     => Handler_Id,
     Table_Low_Bound      => Handler_Id'First,
     Table_Initial        => 10_000,
     Table_Increment      => 100);

   Next_Handler_Index : Handler_Id := Null_Handler + 1;
   --  The table index of the next available handler entry slot

   Free_Handler_List : Handler_Id := Null_Handler;
   --  A list of freed Handler_Entry records that can be reused


   -------------------------------------
   -- Instruction Sequence Operations --
   -------------------------------------

   --------------------
   -- Start_Sequence --
   --------------------

   procedure Start_Sequence (Seq : in out Code_Sequence) is
   begin
      pragma Assert (Seq.First = Null_Instr);

      Seq := Empty_Sequence;
   end Start_Sequence;

   -------------------
   -- Free_Sequence --
   -------------------

   procedure Free_Sequence (Seq : in out Code_Sequence) is
   begin
      if Seq.Last /= Null_Instr then
         Code_Table.Table (Seq.Last).Next := Free_List;
         Free_List := Seq.First;
         Seq := Empty_Sequence;
      end if;
   end Free_Sequence;

   -----------
   -- First --
   -----------

   function First (Seq : Code_Sequence) return Instr_Id is
   begin
      return Seq.First;
   end First;

   ----------
   -- Last --
   ----------

   function Last (Seq : Code_Sequence) return Instr_Id is
   begin
      return Seq.Last;
   end Last;

   ------------
   -- Append --
   ------------

   procedure Append (Seq : in out Code_Sequence; Instr : Instruction) is
   begin
      Append (Seq, New_Instr);
      Code_Table.Table (Seq.Last) := Instr;
   end Append;

   ------------
   -- Insert --
   ------------

   procedure Insert (New_Instr : Instr_Id; After : Instr_Id) is
   begin
      pragma Assert (Code_Table.Table (New_Instr).Next = Null_Instr);

      Code_Table.Table (After).Next := New_Instr;
   end Insert;

   ------------
   -- Append --
   ------------

   procedure Append (Seq : in out Code_Sequence; Id : Instr_Id) is
   begin
      pragma Assert (Code_Table.Table (Id).Next = Null_Instr);

      if Seq.First = Null_Instr then
         Seq.First := Id;
      else
         Code_Table.Table (Seq.Last).Next := Id;
      end if;
      Seq.Last := Id;
   end Append;

   ------------
   -- Attach --
   ------------

   procedure Attach (First_Seq, Second_Seq : in out Code_Sequence) is
   begin
      pragma Assert (First_Seq.First /= Second_Seq.First);

      if First_Seq.First = Null_Instr then
         First_Seq := Second_Seq;
      else
         Code_Table.Table (First_Seq.Last).Next := Second_Seq.First;
         First_Seq.Last := Second_Seq.Last;
      end if;
      Second_Seq := Empty_Sequence;
   end Attach;

   -------------
   -- Prepend --
   -------------

   procedure Prepend (First_Seq, Second_Seq : in out Code_Sequence) is
   begin
      pragma Assert (First_Seq.First /= Second_Seq.First);

      if Second_Seq.First = Null_Instr then
         Second_Seq := First_Seq;

      elsif First_Seq.First = Null_Instr then
         return;

      else
         Code_Table.Table (First_Seq.Last).Next := Second_Seq.First;
         Second_Seq.First := First_Seq.First;
      end if;
      First_Seq := Empty_Sequence;
   end Prepend;

   ---------------
   -- New_Instr --
   ---------------

   function New_Instr return Instr_Id is
      Freed_Id : Instr_Id := Free_List;

   begin
      if Freed_Id /= Null_Instr then
         Free_List := Code_Table.Table (Freed_Id).Next;
      else
         Freed_Id := Next_Code_Index;
         Next_Code_Index := Next_Code_Index + 1;
         Code_Table.Set_Last (Next_Code_Index);
      end if;

      Code_Table.Table (Freed_Id) := Empty_Instr;
      return Freed_Id;
   end New_Instr;

   ---------------
   -- New_Instr --
   ----------------

   function New_Instr (Instr : Instruction) return Instr_Id is
      New_Instr_Id : Instr_Id := New_Instr;
   begin
      Code_Table.Table (New_Instr_Id) := Instr;
      return New_Instr_Id;
   end New_Instr;

   ---------
   -- Get --
   ---------

   function Get (Id : Instr_Id) return Instruction is
   begin
      return Code_Table.Table (Id);
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get (Id : Instr_Id; Instr : out Instruction) is
   begin
      Instr := Code_Table.Table (Id);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (Id : Instr_Id; Instr : Instruction) is
      Save_Next : constant Instr_Id := Code_Table.Table (Id).Next;

   begin
      Code_Table.Table (Id) := Instr;
      Code_Table.Table (Id).Next := Save_Next;
   end Put;


   -----------------------------------
   -- Switch Instruction Operations --
   -----------------------------------

   -----------------------
   -- Start_Switch_List --
   -----------------------

   procedure Start_Switch_List (List : in out Switch_List) is
   begin
      pragma Assert (List = null);

      List := new Switch_List_Record;
   end Start_Switch_List;

   ----------------------
   -- Free_Switch_List --
   ----------------------

   procedure Free_Switch_List (List : in out Switch_List) is
   begin
      null;  -- TBD ???
   end Free_Switch_List;

   ---------------------
   -- Add_Switch_Pair --
   ---------------------

   procedure Add_Switch_Pair
     (List         : in out Switch_List;
      Match_Value  : Int_32;
      Switch_Label : Label_Id)
   is
      New_Pair  : constant Switch_Pair_Id := new Switch_Pair;
      Curr_Pair : Switch_Pair_Id;
      Prev_Pair : Switch_Pair_Id;

   begin
      pragma Assert (List /= null);

      New_Pair.Match_Value := Match_Value;
      New_Pair.Switch_Lbl  := Switch_Label;

      --  This is the first list element

      if List.First_Pair = Null_Switch_Pair then
         List.First_Pair    := New_Pair;
         List.Last_Pair     := New_Pair;
         New_Pair.Next_Pair := Null_Switch_Pair;

      --  Add the new pair to the end of the list

      elsif Match_Value > List.Last_Pair.Match_Value then
         List.Last_Pair.Next_Pair := New_Pair;
         New_Pair.Next_Pair       := Null_Switch_Pair;
         List.Last_Pair           := New_Pair;

      --  Add the new pair to the beginning of the list

      elsif Match_Value < List.First_Pair.Match_Value then
         New_Pair.Next_Pair := List.First_Pair;
         List.First_Pair    := New_Pair;

      --  Insert the new pair in front of the first element with
      --  a greater match value.

      else
         Curr_Pair := List.First_Pair;
         while Match_Value > Curr_Pair.Match_Value loop
            Prev_Pair := Curr_Pair;
            Curr_Pair := Curr_Pair.Next_Pair;
         end loop;

         pragma Assert (Match_Value /= Curr_Pair.Match_Value);

         Prev_Pair.Next_Pair := New_Pair;
         New_Pair.Next_Pair  := Curr_Pair;
      end if;

      List.Pair_Count := List.Pair_Count + 1;
   end Add_Switch_Pair;

   -----------------------
   -- Switch_Pair_Count --
   -----------------------

   function Switch_Pair_Count (List : Switch_List) return U4 is
   begin
      return List.Pair_Count;
   end Switch_Pair_Count;

   ----------------
   -- First_Pair --
   ----------------

   function First_Pair (List : Switch_List) return Switch_Pair_Id is
   begin
      return List.First_Pair;
   end First_Pair;

   ---------------
   -- Last_Pair --
   ---------------

   function Last_Pair (List : Switch_List) return Switch_Pair_Id is
   begin
      return List.Last_Pair;
   end Last_Pair;

   ---------------
   -- Next_Pair --
   ---------------

   function Next_Pair (Pair : Switch_Pair_Id) return Switch_Pair_Id is
   begin
      pragma Assert (Pair /= Null_Switch_Pair);

      return Pair.Next_Pair;
   end Next_Pair;

   -----------------
   -- Match_Value --
   -----------------

   function Match_Value (Pair : Switch_Pair_Id) return Int_32 is
   begin
      pragma Assert (Pair /= Null_Switch_Pair);

      return Pair.Match_Value;
   end Match_Value;

   ------------------
   -- Switch_Label --
   ------------------

   function Match_Label (Pair : Switch_Pair_Id) return Label_Id is
   begin
      pragma Assert (Pair /= Null_Switch_Pair);

      return Pair.Switch_Lbl;
   end Match_Label;


   ----------------------------------
   -- Exception Handler Operations --
   ----------------------------------

   --------------------
   -- Start_Sequence --
   --------------------

   procedure Start_Sequence (Seq : in out Handler_Sequence) is
   begin
      pragma Assert (Seq.First = Null_Handler);

      Seq := (First => Null_Handler, Last => Null_Handler);
   end Start_Sequence;

   -------------------
   -- Free_Sequence --
   -------------------

   procedure Free_Sequence (Seq : in out Handler_Sequence) is
   begin
      if Seq.Last /= Null_Handler then
         Handler_Table.Table (Seq.Last).Next := Free_Handler_List;
         Free_Handler_List := Seq.First;
         Seq := (First => Null_Handler, Last => Null_Handler);
      end if;
   end Free_Sequence;

   -----------------------
   -- New_Handler_Entry --
   -----------------------

   function New_Handler_Entry
     (Exc_Class   : Pool_Id;
      Start_Lbl   : Label_Id;
      End_Lbl     : Label_Id;
      Handler_Lbl : Label_Id)
      return        Handler_Id
   is
      Freed_Handler : Handler_Id := Free_Handler_List;

   begin
      if Freed_Handler /= Null_Handler then
         Free_Handler_List := Handler_Table.Table (Freed_Handler).Next;
      else
         Freed_Handler := Next_Handler_Index;
         Next_Handler_Index := Next_Handler_Index + 1;
         Handler_Table.Set_Last (Next_Handler_Index);
      end if;

      Handler_Table.Table (Freed_Handler)
        := (Exc_Class, Start_Lbl, End_Lbl, Handler_Lbl, Null_Handler);

      return Freed_Handler;
   end New_Handler_Entry;

   ------------
   -- Append --
   ------------

   procedure Append (Seq : in out Handler_Sequence; Handler : Handler_Id) is
   begin
      pragma Assert (Handler_Table.Table (Handler).Next = Null_Handler);

      if Seq.First = Null_Handler then
         Seq.First := Handler;
      else
         Handler_Table.Table (Seq.Last).Next := Handler;
      end if;
      Seq.Last := Handler;
   end Append;

   -----------
   -- First --
   -----------

   function First (Seq : Handler_Sequence) return Handler_Id is
   begin
      return Seq.First;
   end First;

   ----------
   -- Next --
   ----------

   function Next (Id : Handler_Id) return Handler_Id is
   begin
      return Handler_Table.Table (Id).Next;
   end Next;

   ----------
   -- Last --
   ----------

   function Last (Seq : Handler_Sequence) return Handler_Id is
   begin
      return Seq.Last;
   end Last;

   ---------
   -- Get --
   ---------

   function Get (Id : Handler_Id) return Handler_Entry is
   begin
      return Handler_Table.Table (Id);
   end Get;


   ------------------------------
   -- Operand Stack Operations --
   ------------------------------

   ---------------
   -- New_Stack --
   ---------------

   function New_Stack (Max_Elements : Stack_Range) return Op_Stack_Id is
   begin
      return new Operand_Stack (Max_Elements);
   end New_Stack;

   ----------------
   -- Free_Stack --
   ----------------

   procedure Free_Stack (Stack : in out Op_Stack_Id) is
      procedure Free is
        new Unchecked_Deallocation (Operand_Stack, Op_Stack_Id);
   begin
      Free (Stack);
   end Free_Stack;

   ---------------
   -- Max_Depth --
   ---------------

   function Max_Depth (Stack : Op_Stack_Id) return Depth_Range is
   begin
      return Stack.Max_Depth;
   end Max_Depth;

   -------------------
   -- Set_Max_Depth --
   -------------------

   procedure Set_Max_Depth (Stack : Op_Stack_Id; Depth : Depth_Range) is
   begin
      pragma Assert (Depth >= Stack.Max_Depth);

      Stack.Max_Depth := Depth;
   end Set_Max_Depth;

   ----------
   -- Push --
   ----------

   procedure Push (Stack : Op_Stack_Id; Typ : Type_Id) is
   begin
      pragma Assert (Stack.Top < Stack.Max);

      Stack.Top := Stack.Top + 1;
      Stack.Stack (Stack.Top).Jtype := Typ;

      Stack.Curr_Depth := Stack.Curr_Depth + Depth_Range (Word_Size (Typ));
      if Stack.Curr_Depth > Stack.Max_Depth then
         Stack.Max_Depth := Stack.Curr_Depth;
      end if;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Stack : Op_Stack_Id; Count : Stack_Range) is
   begin
      pragma Assert (Stack.Top - Count >= Empty_Stack_Index);

      pragma Assert (Stack.Mark_Level = Empty_Stack_Index
             or else Stack.Top - Count >= Stack.Mark_Level);

      for T in 0 .. Count - 1 loop
         Stack.Curr_Depth := Stack.Curr_Depth
           - Depth_Range (Word_Size (Stack.Stack (Stack.Top - T).Jtype));
      end loop;

      Stack.Top := Stack.Top - Count;
   end Pop;

   ---------
   -- Pop --
   ---------

   function Pop (Stack : Op_Stack_Id) return Type_Id is
   begin
      pragma Assert (Stack.Top > Empty_Stack_Index);

      pragma Assert (Stack.Mark_Level = Empty_Stack_Index
             or else Stack.Top - 1 >= Stack.Mark_Level);

      Stack.Curr_Depth := Stack.Curr_Depth
        - Depth_Range (Word_Size (Stack.Stack (Stack.Top).Jtype));

      Stack.Top := Stack.Top - 1;
      return Stack.Stack (Stack.Top + 1).Jtype;
   end Pop;

   ---------
   -- Top --
   ---------

   function Top (Stack : Op_Stack_Id) return Type_Id is
   begin
      pragma Assert (Stack.Top > Empty_Stack_Index);

      return Stack.Stack (Stack.Top).Jtype;
   end Top;

   -----------------
   -- Next_To_Top --
   -----------------

   function Next_To_Top (Stack : Op_Stack_Id) return Type_Id is
   begin
      pragma Assert (Stack.Top >= 2);

      return Stack.Stack (Stack.Top - 1).Jtype;
   end Next_To_Top;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Stack : Op_Stack_Id) return Boolean is
   begin
      return Stack.Top = Empty_Stack_Index;
   end Is_Empty;

   ----------
   -- Mark --
   ----------

   procedure Mark (Stack : Op_Stack_Id) is
   begin
      pragma Assert (Stack.Top /= Empty_Stack_Index);

      Stack.Mark_Level := Stack.Top;
      Stack.Stack (Stack.Top).Mark_Count
        := Stack.Stack (Stack.Top).Mark_Count + 1;
   end Mark;

   ------------
   -- Marked --
   ------------

   function Marked (Stack : Op_Stack_Id) return Boolean is
   begin
      return Stack.Mark_Level > Empty_Stack_Index;
   end Marked;

   -------------
   -- Release --
   -------------

   procedure Release (Stack : Op_Stack_Id) is
      Pop_Count   : constant Stack_Range := Stack.Top - Stack.Mark_Level;
      Stack_Index : Stack_Range;

   begin
      pragma Assert (Stack.Mark_Level >= Empty_Stack_Index
            and then Stack.Mark_Level <= Stack.Top);

      --  Check that the stack below the released stack elements is
      --  consistent with the released operand types (each of the
      --  popped types must be mirrored by another type in the same
      --  relative position below the mark point). Note that we
      --  only check type kinds here, which is adequate for most
      --  purposes and avoids problems with cases of N_Conditional_Ops
      --  where there might be compatible but differing scalar types
      --  on the stack (e.g., byte vs. int).

      for Index in Stack.Mark_Level + 1 .. Stack.Top loop
         pragma Assert
           (Type_Kind (Stack.Stack (Index).Jtype)
              = Type_Kind (Stack.Stack (Index - Pop_Count).Jtype));
         null;
      end loop;

      Pop (Stack, Pop_Count);
      pragma Assert (Stack.Top = Stack.Mark_Level);

      Stack.Stack (Stack.Top).Mark_Count
        := Stack.Stack (Stack.Top).Mark_Count - 1;

      --  If there is no mark at the current Mark_Level, then
      --  look down the stack to see if any other stack entries
      --  have marks, and reset the Mark_Level to indicate the
      --  nearest mark (if none, then Mark_Level will be set to
      --  Empty_Stack_Index).

      if Stack.Stack (Stack.Top).Mark_Count = 0 then
         Stack.Mark_Level := Empty_Stack_Index;

         Stack_Index := Stack.Top - 1;
         while Stack_Index > Empty_Stack_Index loop
            if Stack.Stack (Stack_Index).Mark_Count > 0 then
               Stack.Mark_Level := Stack_Index;
               exit;
            end if;
            Stack_Index := Stack_Index - 1;
         end loop;
      end if;
   end Release;

   -----------
   -- Reset --
   -----------

   procedure Reset (Stack : Op_Stack_Id) is
   begin
      Stack.Top := Empty_Stack_Index;
      Stack.Curr_Depth := 0;
      Stack.Mark_Level := Empty_Stack_Index;
   end Reset;

   -----------------
   -- Print_Stack --
   -----------------

   procedure Print_Stack (Stack : Op_Stack_Id) is
   begin
      Write_Str ("======(Top of Stack)======");
      Write_Eol;
      if Is_Empty (Stack) then
         Write_Str (" *** <stack is empty> ***");
         Write_Eol;
      else
         for Index in reverse Empty_Stack_Index + 1 .. Stack.Top loop
            Write_Str ("[" & Index'Img & " ]: ");
            if Name (Stack.Stack (Index).Jtype) = No_Name then
               Write_Str ("<unknown type>");
            else
               Write_Name (Name (Stack.Stack (Index).Jtype));
               Write_Str (" (type_id =" & Stack.Stack (Index).Jtype'Img & ")");
            end if;
            Write_Eol;
         end loop;
      end if;
      Write_Str ("======(Stack Bottom)======");
      Write_Eol;
      Write_Eol;
   end Print_Stack;

begin
   Code_Table.Set_Last (Next_Code_Index);
end JVM.Code;
