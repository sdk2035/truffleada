------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               I N L I N E                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.45 $
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Expander; use Expander;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch11; use Exp_Ch11;
with Exp_Tss;  use Exp_Tss;
with Fname;    use Fname;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch10; use Sem_Ch10;
with Sem_Ch12; use Sem_Ch12;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Uname;    use Uname;

package body Inline is

   --------------------
   -- Inlined Bodies --
   --------------------

   --  Inlined functions are actually placed in line by the backend if the
   --  corresponding bodies are available (i.e. compiled). Whenever we find
   --  a call to an inlined subprogram, we add the name of the enclosing
   --  compilation unit to a worklist. After all compilation, and after
   --  expansion of generic bodies, we traverse the list of pending bodies
   --  and compile them as well.

   package Inlined_Bodies is new Table.Table (
     Table_Component_Type => Entity_Id,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Inlined_Bodies_Initial,
     Table_Increment      => Alloc.Inlined_Bodies_Increment,
     Table_Name           => "Inlined_Bodies");

   -----------------------
   -- Inline Processing --
   -----------------------

   --  For each call to an inlined subprogram, we make entries in a table
   --  that stores caller and callee, and indicates a prerequisite from
   --  one to the other. We also record the compilation unit that contains
   --  the callee. After analyzing the bodies of all such compilation units,
   --  we produce a list of subprograms in  topological order, for use by the
   --  back-end. If P2 is a prerequisite of P1, then P1 calls P2, and for
   --  proper inlining the back-end must analyze the body of P2 before that of
   --  P1. The code below guarantees that the transitive closure of inlined
   --  subprograms called from the main compilation unit is made available to
   --  the code generator.

   Last_Inlined : Entity_Id := Empty;

   --  For each entry in the table we keep a list of successors in topological
   --  order, i.e. callers of the current subprogram.

   type Subp_Index is new Nat;
   No_Subp : constant Subp_Index := 0;

   --  The subprogram entities are hashed into the Inlined table.

   Num_Hash_Headers : constant := 512;

   Hash_Headers : array (Subp_Index range 0 .. Num_Hash_Headers - 1)
                                                          of Subp_Index;

   type Succ_Index is new Nat;
   No_Succ : constant Succ_Index := 0;

   type Succ_Info is record
      Subp : Subp_Index;
      Next : Succ_Index;
   end record;

   --  The following table stores list elements for the successor lists.
   --  These lists cannot be chained directly through entries in the Inlined
   --  table, because a given subprogram can appear in several such lists.

   package Successors is new Table.Table (
      Table_Component_Type => Succ_Info,
      Table_Index_Type     => Succ_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => Alloc.Successors_Initial,
      Table_Increment      => Alloc.Successors_Increment,
      Table_Name           => "Successors");

   type Subp_Info is record
      Name        : Entity_Id  := Empty;
      First_Succ  : Succ_Index := No_Succ;
      Count       : Integer    := 0;
      Listed      : Boolean    := False;
      Main_Call   : Boolean    := False;
      Next        : Subp_Index := No_Subp;
      Next_Nopred : Subp_Index := No_Subp;
   end record;

   package Inlined is new Table.Table (
      Table_Component_Type => Subp_Info,
      Table_Index_Type     => Subp_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => Alloc.Inlined_Initial,
      Table_Increment      => Alloc.Inlined_Increment,
      Table_Name           => "Inlined");

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Scope_In_Main_Unit (Scop : Entity_Id) return Boolean;
   --  Return True if Scop is in the main unit or its spec.

   procedure Add_Call (Called : Entity_Id; Caller : Entity_Id := Empty);
   --  Make two entries in Inlined table, for an inlined subprogram being
   --  called, and for the inlined subprogram that contains the call. If
   --  the call is in the main compilation unit, Caller is Empty.

   function Add_Subp (E : Entity_Id) return Subp_Index;
   --  Make entry in Inlined table for subprogram E, or return table index
   --  that already holds E.

   function Has_Initialized_Type (E : Entity_Id) return Boolean;
   --  If a candidate for inlining contains type declarations for types with
   --  non-trivial initialization procedures, they are not worth inlining.

   function Is_Nested (E : Entity_Id) return Boolean;
   --  If the function is nested inside some other function, it will
   --  always be compiled if that function is, so don't add it to the
   --  inline list. We cannot compile a nested function outside the
   --  scope of the containing function anyway. This is also the case if
   --  the function is defined in a task body.

   procedure Add_Inlined_Subprogram (Index : Subp_Index);
   --  Add subprogram to Inlined List once all of its predecessors have been
   --  placed on the list. Decrement the count of all its successors, and
   --  add them to list (recursively) if count drops to zero.

   ------------------------------
   -- Deferred Cleanup Actions --
   ------------------------------

   --  The cleanup actions for scopes that contain instantiations is delayed
   --  until after expansion of those instantiations, because they may
   --  contain finalizable objects or tasks that affect the cleanup code.
   --  A scope that contains instantiations only needs to be finalized once,
   --  even if it contains more than one instance. We keep a list of scopes
   --  that must still be finalized, and call cleanup_actions after all the
   --  instantiations have been completed.

   To_Clean : Elist_Id;

   procedure Add_Scope_To_Clean (Inst : Entity_Id);
   --  Build set of scopes on which cleanup actions must be performed.

   procedure Cleanup_Scopes;
   --  Complete cleanup actions on scopes that need it.

   --------------
   -- Add_Call --
   --------------

   procedure Add_Call (Called : Entity_Id; Caller : Entity_Id := Empty) is
      P1 : Subp_Index := Add_Subp (Called);
      P2 : Subp_Index;
      J  : Succ_Index;

   begin
      if Present (Caller) then
         P2 := Add_Subp (Caller);

         --  Add P2 to the list of successors of P1, if not already there.
         --  Note that P2 may contain more than one call to P1, and only
         --  one needs to be recorded.

         J := Inlined.Table (P1).First_Succ;

         while J /= No_Succ loop

            if Successors.Table (J).Subp = P2 then
               return;
            end if;

            J := Successors.Table (J).Next;
         end loop;

         --  On exit, make a successor entry for P2.

         Successors.Increment_Last;
         Successors.Table (Successors.Last).Subp := P2;
         Successors.Table (Successors.Last).Next :=
                             Inlined.Table (P1).First_Succ;
         Inlined.Table (P1).First_Succ := Successors.Last;

         Inlined.Table (P2).Count := Inlined.Table (P2).Count + 1;

      else
         Inlined.Table (P1).Main_Call := True;
      end if;
   end Add_Call;

   ----------------------
   -- Add_Inlined_Body --
   ----------------------

   procedure Add_Inlined_Body (N : Node_Id; E : Entity_Id) is
      Pack : Entity_Id;
      Comp_Unit : Node_Id;

      function Must_Inline return Boolean;
      --  Inlining is only done if the call statement N is in the main unit,
      --  or within the body of another inlined subprogram.

      function Must_Inline return Boolean is
         Scop : Entity_Id := Current_Scope;
         Comp : Node_Id;

      begin
         --  Check if call is in main unit.

         while Scope (Scop) /= Standard_Standard
           and then not Is_Child_Unit (Scop)
         loop
            Scop := Scope (Scop);
         end loop;

         Comp := Parent (Scop);

         while Nkind (Comp) /= N_Compilation_Unit loop
            Comp := Parent (Comp);
         end loop;

         if (Comp = Cunit (Main_Unit)
           or else Comp = Library_Unit (Cunit (Main_Unit)))
         then
            Add_Call (E);
            return True;
         end if;

         --  Call is not in main unit. See if it's in some inlined
         --  subprogram.

         Scop := Current_Scope;
         while Scope (Scop) /= Standard_Standard
           and then not Is_Child_Unit (Scop)
         loop
            if Is_Overloadable (Scop)
              and then Is_Inlined (Scop)
            then
               Add_Call (E, Scop);
               return True;
            end if;

            Scop := Scope (Scop);
         end loop;

         return False;

      end Must_Inline;

   --  Start of processing for Add_Inlined_Body

   begin
      --  Find unit containing E, and add to list of inlined bodies if needed.
      --  If the body is already present, no need to load any other unit. This
      --  is the case for an initialization procedure, which appears in the
      --  package declaration that contains the type. It is also the case if
      --  the body has already been analyzed.

      --  Library-level functions must be handled specially, because there is
      --  no enclosing package to retrieve. In this case, it is the body of
      --  the function that will have to be loaded.

      if not Is_Abstract (E) and then not Is_Nested (E)
        and then Convention (E) /= Convention_Protected
      then
         Pack := Scope (E);

         if Must_Inline
           and then Ekind (Pack) = E_Package
         then
            Set_Is_Called (E);
            Comp_Unit := Parent (Pack);

            if Pack = Standard_Standard then

               --  Library-level inlined function. Add function iself to
               --  list of needed units.

               Inlined_Bodies.Increment_Last;
               Inlined_Bodies.Table (Inlined_Bodies.Last) := E;

            elsif not Is_Inlined (Pack)
              and then not Has_Completion (E)
              and then not Scope_In_Main_Unit (Pack)
            then
               Set_Is_Inlined (Pack);
               Inlined_Bodies.Increment_Last;
               Inlined_Bodies.Table (Inlined_Bodies.Last) := Pack;
            end if;
         end if;
      end if;
   end Add_Inlined_Body;

   ------------------------
   -- Add_Scope_To_Clean --
   ------------------------

   procedure Add_Scope_To_Clean (Inst : Entity_Id) is
      Elmt : Elmt_Id;
      Scop : Entity_Id := Enclosing_Dynamic_Scope (Inst);

   begin
      --  If the instance appears in a library-level package declaration,
      --  all finalization is global, and nothing needs doing here.

      if Scop = Standard_Standard then
         return;
      end if;

      Elmt := First_Elmt (To_Clean);

      while Present (Elmt) loop

         if Node (Elmt) = Scop then
            return;
         end if;

         Elmt := Next_Elmt (Elmt);
      end loop;

      Append_Elmt (Scop, To_Clean);
   end Add_Scope_To_Clean;

   --------------
   -- Add_Subp --
   --------------

   function Add_Subp (E : Entity_Id) return Subp_Index is
      Index : Subp_Index := Subp_Index (E) mod Num_Hash_Headers;
      J     : Subp_Index;

      procedure New_Entry;
      --  Initialize entry in Inlined table.

      procedure New_Entry is
      begin
         Inlined.Increment_Last;
         Inlined.Table (Inlined.Last).Name        := E;
         Inlined.Table (Inlined.Last).First_Succ  := No_Succ;
         Inlined.Table (Inlined.Last).Count       := 0;
         Inlined.Table (Inlined.Last).Listed      := False;
         Inlined.Table (Inlined.Last).Main_Call   := False;
         Inlined.Table (Inlined.Last).Next        := No_Subp;
         Inlined.Table (Inlined.Last).Next_Nopred := No_Subp;
      end New_Entry;

   --  Start of processing for Add_Subp

   begin
      if Hash_Headers (Index) = No_Subp then
         New_Entry;
         Hash_Headers (Index) := Inlined.Last;
         return Inlined.Last;

      else
         J := Hash_Headers (Index);

         while J /= No_Subp loop

            if Inlined.Table (J).Name = E then
               return J;
            else
               Index := J;
               J := Inlined.Table (J).Next;
            end if;
         end loop;

         --  On exit, subprogram was not found. Enter in table. Index is
         --  the current last entry on the hash chain.

         New_Entry;
         Inlined.Table (Index).Next := Inlined.Last;
         return Inlined.Last;
      end if;
   end Add_Subp;

   ----------------------------
   -- Analyze_Inlined_Bodies --
   ----------------------------

   procedure Analyze_Inlined_Bodies is
      Comp_Unit : Node_Id;
      J         : Int;
      Pack      : Entity_Id;
      S         : Succ_Index;

   begin
      Analyzing_Inlined_Bodies := False;

      if Errors_Detected = 0 then
         New_Scope (Standard_Standard);

         J := 0;
         while J <= Inlined_Bodies.Last
           and then Errors_Detected = 0
         loop
            Pack := Inlined_Bodies.Table (J);

            while Present (Pack)
              and then Scope (Pack) /= Standard_Standard
              and then not Is_Child_Unit (Pack)
            loop
               Pack := Scope (Pack);
            end loop;

            Comp_Unit := Parent (Pack);

            while Present (Comp_Unit)
              and then Nkind (Comp_Unit) /= N_Compilation_Unit
            loop
               Comp_Unit := Parent (Comp_Unit);
            end loop;

            if Present (Comp_Unit)
              and then Comp_Unit /= Cunit (Main_Unit)
              and then Body_Required (Comp_Unit)
            then
               declare
                  Bname : constant Unit_Name_Type :=
                            Get_Body_Name (Get_Unit_Name (Unit (Comp_Unit)));

                  OK : Boolean;

               begin
                  if not Is_Loaded (Bname) then
                     Load_Needed_Body (Comp_Unit, OK);

                     if not OK then
                        Error_Msg_Unit_1 := Bname;
                        Error_Msg_N
                          ("one or more inlined subprograms accessed in $!",
                           Comp_Unit);
                        Error_Msg_Name_1 :=
                          Get_File_Name (Bname, Subunit => False);
                        Error_Msg_N ("\but file{ was not found!", Comp_Unit);
                        raise Unrecoverable_Error;
                     end if;
                  end if;
               end;
            end if;

            J := J + 1;
         end loop;

         --  The analysis of required bodies may have produced additional
         --  generic instantiations. To obtain further inlining, we perform
         --  another round of generic body instantiations. Establishing a
         --  fully recursive loop between inlining and generic instantiations
         --  is unlikely to yield more than this one additional pass.

         Instantiate_Bodies;

         --  The list of inlined subprograms is an overestimate, because
         --  it includes inlined functions called from functions that are
         --  compiled as part of an inlined package, but are not themselves
         --  called. An accurate computation of just those subprograms that
         --  are needed requires that we perform a transitive closure over
         --  the call graph, starting from calls in the main program. Here
         --  we do one step of the inverse transitive closure, and reset
         --  the Is_Called flag on subprograms all of whose callers are not.

         for Index in Inlined.First .. Inlined.Last loop
            S := Inlined.Table (Index).First_Succ;

            if S /= No_Succ
              and then not Inlined.Table (Index).Main_Call
            then
               Set_Is_Called (Inlined.Table (Index).Name, False);

               while S /= No_Succ loop

                  if Is_Called
                    (Inlined.Table (Successors.Table (S).Subp).Name)
                   or else Inlined.Table (Successors.Table (S).Subp).Main_Call
                  then
                     Set_Is_Called (Inlined.Table (Index).Name);
                     exit;
                  end if;

                  S := Successors.Table (S).Next;
               end loop;
            end if;
         end loop;

         --  Now that the units are compiled, chain the subprograms within
         --  that are called and inlined. Produce list of inlined subprograms
         --  sorted in  topological order. Start with all subprograms that
         --  have no prerequisites, i.e. inlined subprograms that do not call
         --  other inlined subprograms.

         for Index in Inlined.First .. Inlined.Last loop

            if Is_Called (Inlined.Table (Index).Name)
              and then Inlined.Table (Index).Count = 0
              and then not Inlined.Table (Index).Listed
            then
               Add_Inlined_Subprogram (Index);
            end if;
         end loop;

         --  Because Add_Inlined_Subprogram treats recursively nodes that have
         --  no prerequisites left, at the end of the loop all subprograms
         --  must have been listed. If there are any unlisted subprograms
         --  left, there must be some recursive chains that cannot be inlined.

         for Index in Inlined.First .. Inlined.Last loop
            if Is_Called (Inlined.Table (Index).Name)
              and then Inlined.Table (Index).Count /= 0
              and then not Is_Predefined_File_Name
                (Unit_File_Name
                  (Get_Source_Unit (Inlined.Table (Index).Name)))
            then
               Error_Msg_N
                 ("& cannot be inlined?", Inlined.Table (Index).Name);
               --  A warning on the first one might be sufficient.
            end if;
         end loop;

         Pop_Scope;
      end if;
   end Analyze_Inlined_Bodies;

   --------------------
   -- Cleanup_Scopes --
   --------------------

   procedure Cleanup_Scopes is
      Elmt : Elmt_Id;
      Decl : Node_Id;
      Scop : Entity_Id;

   begin
      Elmt := First_Elmt (To_Clean);

      while Present (Elmt) loop
         Scop := Node (Elmt);

         if Ekind (Scop) = E_Entry then
            Scop := Protected_Body_Subprogram (Scop);
         end if;

         if Ekind (Scop) = E_Block then
            Decl := Block_Node (Scop);

         else
            Decl := Unit_Declaration_Node (Scop);

            if Nkind (Decl) = N_Subprogram_Declaration
              or else Nkind (Decl) = N_Task_Type_Declaration
              or else Nkind (Decl) = N_Subprogram_Body_Stub
            then
               Decl := Unit_Declaration_Node (Corresponding_Body (Decl));
            end if;
         end if;

         New_Scope (Scop);
         Expand_Cleanup_Actions (Decl);
         End_Scope;

         Elmt := Next_Elmt (Elmt);
      end loop;
   end Cleanup_Scopes;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Analyzing_Inlined_Bodies := False;
      Pending_Descriptor.Init;
      Pending_Instantiations.Init;
      Inlined_Bodies.Init;
      Successors.Init;
      Inlined.Init;

      for J in Hash_Headers'Range loop
         Hash_Headers (J) := No_Subp;
      end loop;
   end Initialize;

   ------------------------
   -- Instantiate_Bodies --
   ------------------------

   --  Generic bodies contain all the non-local references, so an
   --  instantiation does not need any more context than Standard
   --  itself, even if the instantiation appears in an inner scope.
   --  Generic associations have verified that the contract model is
   --  satisfied, so that any error that may occur in the analysis of
   --  the body is an internal error.

   procedure Instantiate_Bodies is
      J    : Int;
      Info : Pending_Body_Info;

   begin
      if Errors_Detected = 0 then
         Expander_Active := True;
         New_Scope (Standard_Standard);
         To_Clean := New_Elmt_List;

         if Is_Generic_Unit (Cunit_Entity (Main_Unit)) then
            Start_Generic;
         end if;

         --  A body instantiation may generate additional instantiations, so
         --  the following loop must scan to the end of a possibly expanding
         --  set (that's why we can't simply use a FOR loop here).

         J := 0;

         while J <= Pending_Instantiations.Last
           and then Errors_Detected = 0
         loop

            Info := Pending_Instantiations.Table (J);

            --  If the  instantiation node is absent, it has been removed
            --  as part of unreachable code.

            if No (Info.Inst_Node) then
               null;

            elsif Nkind (Info. Act_Decl) = N_Package_Declaration then
               Instantiate_Package_Body (Info);
               Add_Scope_To_Clean (Defining_Entity (Info.Act_Decl));

            else
               Instantiate_Subprogram_Body (Info);
            end if;

            J := J + 1;
         end loop;

         --  Reset the table of instantiations. Additional instantiations
         --  may be added through inlining, when additional bodies are
         --  analyzed.

         Pending_Instantiations.Init;

         --  We can now complete the cleanup actions of scopes that contain
         --  pending instantiations (skipped for generic units, since we
         --  never need any cleanups in generic units).
         --  pending instantiations.

         if Expander_Active
           and then not Is_Generic_Unit (Main_Unit_Entity)
         then
            Cleanup_Scopes;

            --  Also generate subprogram descriptors that were delayed

            for J in Pending_Descriptor.First .. Pending_Descriptor.Last loop
               declare
                  Ent : constant Entity_Id := Pending_Descriptor.Table (J);

               begin
                  if Is_Subprogram (Ent) then
                     Generate_Subprogram_Descriptor_For_Subprogram
                       (Get_Subprogram_Body (Ent), Ent);

                  elsif Ekind (Ent) = E_Package then
                     Generate_Subprogram_Descriptor_For_Package
                       (Parent (Declaration_Node (Ent)), Ent);

                  elsif Ekind (Ent) = E_Package_Body then
                     Generate_Subprogram_Descriptor_For_Package
                       (Declaration_Node (Ent), Ent);
                  end if;
               end;
            end loop;

         elsif Is_Generic_Unit (Cunit_Entity (Main_Unit)) then
            End_Generic;
         end if;

         Pop_Scope;
      end if;
   end Instantiate_Bodies;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Pending_Instantiations.Locked := True;
      Inlined_Bodies.Locked := True;
      Successors.Locked := True;
      Inlined.Locked := True;
      Pending_Instantiations.Release;
      Inlined_Bodies.Release;
      Successors.Release;
      Inlined.Release;
   end Lock;

   --------------------------
   -- Has_Initialized_Type --
   --------------------------

   function Has_Initialized_Type (E : Entity_Id) return Boolean is
      E_Body : constant Node_Id := Get_Subprogram_Body (E);
      Decl   : Node_Id;

   begin
      if No (E_Body) then        --  imported subprogram
         return False;

      else
         Decl := First (Declarations (E_Body));

         while Present (Decl) loop

            if Nkind (Decl) = N_Full_Type_Declaration
              and then Present (Init_Proc (Defining_Identifier (Decl)))
            then
               return True;
            end if;

            Next (Decl);
         end loop;
      end if;

      return False;
   end Has_Initialized_Type;

   -----------------
   --  Is_Nested  --
   -----------------

   function Is_Nested (E : Entity_Id) return Boolean is
      Scop : Entity_Id := Scope (E);

   begin
      while Scop /= Standard_Standard loop
         if Ekind (Scop) in Subprogram_Kind then
            return True;

         elsif Ekind (Scop) = E_Task_Type then
            return True;
         end if;

         Scop := Scope (Scop);
      end loop;

      return False;
   end Is_Nested;

   ----------------------------
   -- Add_Inlined_Subprogram --
   ----------------------------

   procedure Add_Inlined_Subprogram (Index : Subp_Index) is
      E    : constant Entity_Id := Inlined.Table (Index).Name;
      Succ : Succ_Index;
      Subp : Subp_Index;

   begin
      --  Insert the current subprogram in the list of inlined subprograms

      if not Scope_In_Main_Unit (E)
        and then Is_Inlined (E)
        and then not Is_Nested (E)
        and then not Has_Initialized_Type (E)
      then
         if No (Last_Inlined) then
            Set_First_Inlined_Subprogram (Cunit (Main_Unit), E);
         else
            Set_Next_Inlined_Subprogram (Last_Inlined, E);
         end if;

         Last_Inlined := E;
      end if;

      Inlined.Table (Index).Listed := True;
      Succ := Inlined.Table (Index).First_Succ;

      while Succ /= No_Succ loop
         Subp := Successors.Table (Succ).Subp;
         Inlined.Table (Subp).Count := Inlined.Table (Subp).Count - 1;

         if Inlined.Table (Subp).Count = 0 then
            Add_Inlined_Subprogram (Subp);
         end if;

         Succ := Successors.Table (Succ).Next;
      end loop;

   end Add_Inlined_Subprogram;

   --------------------------
   -- Remove_Dead_Instance --
   --------------------------

   procedure Remove_Dead_Instance (N : Node_Id) is
      J    : Int;

   begin
      J := 0;

      while J <= Pending_Instantiations.Last loop

         if Pending_Instantiations.Table (J).Inst_Node = N then
            Pending_Instantiations.Table (J).Inst_Node := Empty;
            return;
         end if;

         J := J + 1;
      end loop;
   end Remove_Dead_Instance;

   ------------------------
   -- Scope_In_Main_Unit --
   ------------------------

   function Scope_In_Main_Unit (Scop : Entity_Id) return Boolean is
      Comp : Node_Id;
      S    : Entity_Id := Scop;

   begin

      while Scope (S) /= Standard_Standard
        and then not Is_Child_Unit (S)
      loop
         S := Scope (S);
      end loop;

      Comp := Parent (S);

      while Present (Comp)
        and then Nkind (Comp) /= N_Compilation_Unit
      loop
         Comp := Parent (Comp);
      end loop;

      return
        Comp = Cunit (Main_Unit)
          or else Comp = Library_Unit (Cunit (Main_Unit));
   end Scope_In_Main_Unit;

end Inline;
