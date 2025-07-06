------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              J V M . D B G                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                             $Revision: 1.8 $                             --
--                                                                          --
--           Copyright (C) 1998-1999 Ada Core Technologies, Inc.            --
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

with Atree;    use Atree;
with Debug;    use Debug;
with JVM.Info; use JVM.Info;
with J_Basics; use J_Basics;
with J_String; use J_String;
with J_Types;  use J_Types;
with Namet;    use Namet;
with Output;   use Output;
with Sinput;   use Sinput;

package body JVM.Dbg is

   --  The following declarations are state variables used by
   --  Print_Source_Line for source line output.

   Source_Name_Id : Name_Id;
   Src_Stream_Ptr : Stream_Of_U1_Ptr;

   type Line_Table_Ptr is access Line_Table;

   Src_Lines      : Line_Table_Ptr;
   Last_Line_Num  : Nat_32 := 0;

   -----------------------------
   -- Init_Source_Line_Output --
   -----------------------------

   procedure Init_Source_Line_Output (Node : Node_Id) is
   begin
      if Debug_Flag_JJ then
         --  Set up the source file stream and line table for use
         --  by Print_Source_Line when debugging.

         Source_Name_Id
           := Debug_Source_Name (Get_Source_File_Index (Sloc (Node)));
         Src_Stream_Ptr := Get_Stream_Of_U1 (Name_String (Source_Name_Id));
         Src_Lines := new Line_Table'(Get_Line_Table (Src_Stream_Ptr.all));
      end if;
   end Init_Source_Line_Output;

   -----------
   -- Print --
   -----------

   procedure Print (S : String) is
   begin
      Write_Str (S);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (N : Name_Id) is
   begin
      if N = No_Name then
         Write_Str ("<no name>");
      else
         Write_Name (N);
      end if;
   end Print;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (S : String := "") is
   begin
      Write_Str (S);
      Write_Eol;
   end Print_Line;

   -----------------------
   -- Print_Source_Line --
   -----------------------

   procedure Print_Source_Line (Node : Node_Id) is
      Line_Num : constant Nat_32 := Nat_32 (Get_Line_Number (Sloc (Node)));

   begin
      if Debug_Flag_JJ
        and then Line_Num > 0
        and then Line_Num /= Last_Line_Num
      then
         Last_Line_Num := Line_Num;

         Write_Location (Sloc (Node));
         Write_Str (":  ");
         Write_Str
           (To_String
             (Src_Stream_Ptr
               (Src_Lines (Line_Num).First .. Src_Lines (Line_Num).Last)));
         Write_Eol;
      end if;
   end Print_Source_Line;

   -----------------
   -- Print_Class --
   -----------------

   procedure Print_Class (C : Class_Id) is
   begin
      Print (">>> Class: ");      Print (Name (C));
      Print ("[class_id =");     Print (C'Img);
      Print ("], superclass = "); Print (Name (Superclass (C)));

      if Is_Open (C) then
         Print (" (Is_Open)");
      else
         Print (" (not Is_Open)");
      end if;

      Print_Line;

      null;  --  other stuff TBD ???...
   end Print_Class;

   ----------------
   -- Print_Type --
   ----------------

   procedure Print_Type (T : Type_Id) is
   begin
      Print (">>> Type: ");      Print (Name (T));
      Print ("; type_id =");    Print (T'Img);
      Print_Line;

      null;  --  other stuff TBD... ???
   end Print_Type;

   -----------------
   -- Print_Field --
   -----------------

   procedure Print_Field (F : Field_Id) is
   begin
      Print (">>> Field: ");  Print (Name (F));
      Print (", class = ");   Print (Name (Class (F)));
      Print ("[class_id ="); Print (Class (F)'Img);
      Print ("], type = ");   Print (Name (Type_Of (F)));
      Print (" [type_id = "); Print (Name (Type_Of (F))); Print ("]");
      Print_Line;
   end Print_Field;

   ------------------
   -- Print_Method --
   ------------------

   procedure Print_Method (M : Method_Id) is
   begin
      Print (">>> Method: ");   Print (Name (M));
      Print ("; method_id ="); Print (M'Img);

      if Is_Open (M) then
         Print (" (Is_Open)");
      else
         Print (" (not Is_Open)");
      end if;

      Print (" [class_id =");   Print (Class (M)'Img);   Print ("]");

      Print_Line;

      null;  --  other stuff TBD ???...
   end Print_Method;

   -----------------
   -- Print_Jcode --
   -----------------

   procedure Print_Jcode (M : Method_Id) is
   begin
      null;  --  TBD ???
   end Print_Jcode;

   ---------------------
   -- Print_Local_Var --
   ---------------------

   procedure Print_Local_Var (L : Local_Var_Id) is
   begin
      Print (">>> Local: ");   Print (Name (L));
      Print (" [local_id = "); Print (L'Img);
      Print ("], type = ");    Print (Name (Type_Of (L)));
      Print (" [type_id = ");  Print (Type_Of (L)'Img);  Print ("]");
      Print_Line;

      null;  --  other stuff TBD ???...
   end Print_Local_Var;

   procedure PC (C : Class_Id) is
   begin
      Print_Class (C);
   end PC;

   procedure PT (T : Type_Id) is
   begin
      Print_Type (T);
   end PT;

   procedure PF (F : Field_Id) is
   begin
      Print_Field (F);
   end PF;

   procedure PM (M : Method_Id) is
   begin
      Print_Method (M);
   end PM;

   procedure PJ (M : Method_Id) is
   begin
      Print_Jcode (M);
   end PJ;

   procedure PL (L : Local_Var_Id) is
   begin
      Print_Local_Var (L);
   end PL;

end JVM.Dbg;
