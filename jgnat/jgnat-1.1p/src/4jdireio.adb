------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                        A D A . D I R E C T _ I O                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--              Copyright (C) 2000 Ada Core Technologies, Inc.              --
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
-- JGNAT -  The GNAT Ada 95 tool chain for the Java (TM) Virtual Machine is --
--          maintained by Ada Core Technologies, Inc. - http://www.gnat.com --
--                                                                          --
------------------------------------------------------------------------------

--  This is the JGNAT-specific version of the Ada.Direct_IO body

--  This is the generic template for Direct_IO, i.e. the code that gets
--  duplicated. We absolutely minimize this code by either calling routines
--  in System.File_IO (for common file functions), in System.Direct_IO
--  (for specialized Direct_IO functions), or in the Ada-specific Java
--  class Object_File.

with Interfaces.C_Streams; use Interfaces.C_Streams;
with System;               use System;
with System.File_Control_Block;
with System.File_IO;
with System.Direct_IO;
with Unchecked_Conversion;

use type System.Direct_IO.Count;

package body Ada.Direct_IO is

   package FCB renames System.File_Control_Block;
   package FIO renames System.File_IO;
   package DIO renames System.Direct_IO;

   subtype AP      is FCB.AFCB_Ptr;
   subtype FP      is DIO.File_Type;
   subtype DCount  is DIO.Count;
   subtype DPCount is DIO.Positive_Count;

   function To_FCB is new Unchecked_Conversion (File_Mode, FCB.File_Mode);
   function To_DIO is new Unchecked_Conversion (FCB.File_Mode, File_Mode);

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      FIO.Close (AP (File));
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Type;
      Mode : in File_Mode := Inout_File;
      Name : in String := "";
      Form : in String := "")
   is
   begin
      DIO.Create (FP (File), To_FCB (Mode), Name, Form);
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : in out File_Type) is
   begin
      FIO.Delete (AP (File));
   end Delete;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : in File_Type) return Boolean is
      function Object_End_Of_File
        (Stream : Interfaces.C_Streams.FILEs)
           return Boolean;
      pragma Import
        (Java, Object_End_Of_File, "jgnat.adalib.Object_File.object_eof");

   begin
      FIO.Check_Read_Status (AP (File));

      return Object_End_Of_File (File.Stream);
   end End_Of_File;

   ----------
   -- Form --
   ----------

   function Form (File : in File_Type) return String is
   begin
      return FIO.Form (AP (File));
   end Form;

   -----------
   -- Index --
   -----------

   function Index (File : in File_Type) return Positive_Count is
   begin
      return Positive_Count (DIO.Index (FP (File)));
   end Index;

   -------------
   -- Is_Open --
   -------------

   function Is_Open (File : in File_Type) return Boolean is
   begin
      return FIO.Is_Open (AP (File));
   end Is_Open;

   ----------
   -- Mode --
   ----------

   function Mode (File : in File_Type) return File_Mode is
   begin
      return To_DIO (FIO.Mode (AP (File)));
   end Mode;

   ----------
   -- Name --
   ----------

   function Name (File : in File_Type) return String is
   begin
      return FIO.Name (AP (File));
   end Name;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Mode : in File_Mode;
      Name : in String;
      Form : in String := "")
   is
   begin
      DIO.Open (FP (File), To_FCB (Mode), Name, Form);
   end Open;

   ----------
   -- Read --
   ----------

   procedure Read
     (File : in File_Type;
      Item : out Element_Type;
      From : in Positive_Count)
   is
      function Read_Element (Stream : Interfaces.C_Streams.FILEs)
        return System.Address;
      pragma Import
        (Java, Read_Element, "jgnat.adalib.Object_File.read_element");

      type Elt_Access is access Element_Type;

      function Addr_To_Elt is
        new Unchecked_Conversion (System.Address, Elt_Access);

   begin
      FIO.Check_Read_Status (AP (File));

      Set_Index (File, From);
      Item := Addr_To_Elt (Read_Element (File.Stream)).all;
      Set_Index (File, From + 1);
   end Read;

   procedure Read (File : in File_Type; Item : out Element_Type) is
   begin
      Read (File, Item, Index (File));
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in out File_Type; Mode : in File_Mode) is
   begin
      DIO.Reset (FP (File), To_FCB (Mode));
   end Reset;

   procedure Reset (File : in out File_Type) is
   begin
      DIO.Reset (FP (File));
   end Reset;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index (File : in File_Type; To : in Positive_Count) is
      procedure Set_File_Index
        (Stream : Interfaces.C_Streams.FILEs;
         Index  : Positive_Count);
      pragma Import
        (Java, Set_File_Index, "jgnat.adalib.Object_File.set_file_index");

   begin
      DIO.Set_Index (FP (File), DPCount (To));

      Set_File_Index (File.Stream, To);
   end Set_Index;

   ----------
   -- Size --
   ----------

   function Size (File : in File_Type) return Count is
      function File_Size (Stream : Interfaces.C_Streams.FILEs)
        return Count;
      pragma Import (Java, File_Size, "jgnat.adalib.Object_File.file_size");

   begin
      FIO.Check_File_Open (AP (File));
      File.Last_Op := DIO.Op_Other;

      return File_Size (File.Stream);
   end Size;

   -----------
   -- Write --
   -----------

   procedure Write
     (File : in File_Type;
      Item : in Element_Type;
      To   : in Positive_Count)
   is
      procedure Write_Element
        (Stream : Interfaces.C_Streams.FILEs; Item : System.Address);
      pragma Import
        (Java, Write_Element, "jgnat.adalib.Object_File.write_element");

      type Elt_Access is access Element_Type;

      function Elt_To_Addr is
        new Unchecked_Conversion (Elt_Access, System.Address);

   begin
      FIO.Check_Write_Status (AP (File));

      Set_Index (File, To);
      Write_Element (File.Stream, Elt_To_Addr (new Element_Type'(Item)));
      Set_Index (File, To + 1);
   end Write;

   procedure Write (File : in File_Type; Item : in Element_Type) is
   begin
      Write (File, Item, Index (File));
   end Write;

end Ada.Direct_IO;
