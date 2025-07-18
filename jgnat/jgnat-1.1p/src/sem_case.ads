------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C A S E                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $                              --
--                                                                          --
--            Copyright (C) 1996 Free Software Foundation, Inc.             --
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

with Types; use Types;

--  Package containing all the routines to proces a list of discrete choices.
--  Such lists can occur in 3 different constructs: case statements, array
--  aggregates and record variants. We have factorized what used to be 3 very
--  similar sets of routines here. If you didn't figure it out already Choi
--  in the package name stands for Choices.

package Sem_Case is

   type Choice_Bounds is record
     Lo   : Node_Id;
     Hi   : Node_Id;
     Node : Node_Id;
   end record;

   type Choice_Table_Type is array (Pos range <>) of Choice_Bounds;
   --  Table type used to sort the choices present in a case statement,
   --  array aggregate or record variant.

   procedure No_OP (C : Node_Id);
   --  The no-operation routine. Does absolutely nothing. Can be used
   --  in the following generic for the parameter Proces_Empty_Choice.

   generic
      with function Get_Alternatives (N : Node_Id) return List_Id;
      --  Function needed to get to the actual list of case statement
      --  alternatives, or array aggregate component associations or
      --  record variants from which we can then access the actual lists
      --  of discrete choices. N is the node for the original construct
      --  ie a case statement, an array aggregate or a record variant.

      with function Get_Choices (A : Node_Id) return List_Id;
      --  Given a case statement alternative, array aggregate component
      --  association or record variant A we need different access functions
      --  to get to the actual list of discrete choices.

      with procedure Process_Empty_Choice (Choice : Node_Id);
      --  Processing to carry out for an empty Choice.

      with procedure Process_Non_Static_Choice (Choice : Node_Id);
      --  Processing to carry out for a non static Choice.

      with procedure Process_Associated_Node (A : Node_Id);
      --  Associated to each case alternative, aggregate component
      --  association or record variant A there is a node or list of nodes
      --  that need semantic processing. This routine implements that
      --  processing.

   package Generic_Choices_Processing is

      function Number_Of_Choices (N : Node_Id) return Nat;
      --  Iterates through the choices of N, (N can be a case statement,
      --  array aggregate or record variant), counting all the Choice nodes
      --  except for the Others choice.

      procedure Analyze_Choices
        (N              : Node_Id;
         Subtyp         : Entity_Id;
         Choice_Table   : in out Choice_Table_Type;
         Last_Choice    : out Nat;
         Raises_CE      : out Boolean;
         Others_Present : out Boolean);
      --  From a case statement, array aggregate or record variant N, this
      --  routine analyzes the corresponding list of discrete choices.
      --  Subtyp is the subtype of the discrete choices. The type against
      --  which the discrete choices must be resolved is its base type.
      --
      --  On entry Choice_Table must be big enough to contain all the
      --  discrete choices encountered.
      --
      --  On exit Choice_Table contains all the static and non empty
      --  discrete choices in sorted order. Last_Choice gives the position
      --  of the last valid choice in Choice_Table, Choice_Table'First
      --  contains the first. We can have Last_Choice < Choice_Table'Last
      --  for one (or several) of the following reasons:
      --
      --    (a) The list of choices contained a non static choice
      --
      --    (b) The list of choices contained an empty choice
      --        (something like "1 .. 0 => ")
      --
      --    (c) One of the bounds of a discrete choice contains an
      --        error or raises constraint error.
      --
      --  In one of the bounds of a discrete choice raises a constraint
      --  error the flag Raise_CE is set.
      --
      --  Finally Others_Present is set to True if an Others choice is
      --  present in the list of choices.

   end Generic_Choices_Processing;

end Sem_Case;
