------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                 S Y S T E M . S E Q U E N T I A L _ I O                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.7 $                              --
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

with System.File_IO;
with Unchecked_Deallocation;

package body System.Sequential_IO is

   subtype AP is FCB.AFCB_Ptr;

   package FIO renames System.File_IO;

   -------------------
   -- AFCB_Allocate --
   -------------------

   function AFCB_Allocate
     (Control_Block : Sequential_AFCB)
      return          FCB.AFCB_Ptr
   is
   begin
      return new Sequential_AFCB;
   end AFCB_Allocate;

   ----------------
   -- AFCB_Close --
   ----------------

   --  No special processing required for Sequential_IO close

   procedure AFCB_Close (File : access Sequential_AFCB) is
   begin
      null;
   end AFCB_Close;

   ---------------
   -- AFCB_Free --
   ---------------

   procedure AFCB_Free (File : access Sequential_AFCB) is

      type FCB_Ptr is access all Sequential_AFCB;

      FT : FCB_Ptr := FCB_Ptr (File);

      procedure Free is new
        Unchecked_Deallocation (Sequential_AFCB, FCB_Ptr);

   begin
      Free (FT);
   end AFCB_Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (File : in out File_Type;
      Mode : in FCB.File_Mode := FCB.Out_File;
      Name : in String := "";
      Form : in String := "")
   is
      File_Control_Block : Sequential_AFCB;

   begin
      FIO.Open (File_Ptr  => AP (File),
                Dummy_FCB => File_Control_Block,
                Mode     => Mode,
                Name     => Name,
                Form     => Form,
                Amethod  => 'Q',
                Creat    => True,
                Text     => False);
   end Create;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Mode : in FCB.File_Mode;
      Name : in String;
      Form : in String := "")
   is
      File_Control_Block : Sequential_AFCB;

   begin
      FIO.Open (File_Ptr  => AP (File),
                Dummy_FCB => File_Control_Block,
                Mode      => Mode,
                Name      => Name,
                Form      => Form,
                Amethod   => 'Q',
                Creat     => False,
                Text      => False);
   end Open;

   ----------
   -- Read --
   ----------

   --  Not used, since Sequential_IO files are not used as streams

   procedure Read
     (File : in out Sequential_AFCB;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      raise Program_Error;
   end Read;

   -----------
   -- Write --
   -----------

   --  Not used, since Sequential_IO files are not used as streams

   procedure Write
     (File : in out Sequential_AFCB;
      Item : in Ada.Streams.Stream_Element_Array)
   is
   begin
      raise Program_Error;
   end Write;

end System.Sequential_IO;
