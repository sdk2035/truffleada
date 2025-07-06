------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.7 $                              --
--                                                                          --
--              Copyright (C) 1999-2000 Ada Core Technologies, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

--  This is the JGNAT specific version of Ada.Exceptions body

package body Ada.Exceptions is

   subtype EO  is Exception_Occurrence;
   subtype EOA is Exception_Occurrence_Access;

   type String_Access is access all String;

   ----------------------------
   -- Allocate_Machine_State --
   ----------------------------

   function Allocate_Machine_State return System.Address is
   begin
      return System.Null_Address;
   end Allocate_Machine_State;

   ------------------------------
   -- Deallocate_Machine_State --
   ------------------------------

   procedure Deallocate_Machine_State (M : in out System.Address) is
   begin
      M := System.Null_Address;
   end Deallocate_Machine_State;

   ---------------------------
   -- Exception_Information --
   ---------------------------

   function Exception_Information (X : EO) return String is
      function E_Info (X : EO) return String_Access;
      pragma Import (Java, E_Info, "jgnat.adalib.GNAT_libc.e_information");
   begin
      return E_Info (X).all;
   end Exception_Information;

   -----------------------
   -- Exception_Message --
   -----------------------

   function Exception_Message (X : EO) return String is
      function E_Message (X : EO) return String_Access;
      pragma Import (Java, E_Message, "jgnat.adalib.GNAT_libc.e_message");
   begin
      return E_Message (X).all;
   end Exception_Message;

   --------------------
   -- Exception_Name --
   --------------------

   function Exception_Name (X : Exception_Id) return String is
      function Ada_Name (X : Exception_Id) return String_Access;
      pragma Import (Java, Ada_Name, "jgnat.adalib.GNAT_libc.ada_name");
   begin
      return Ada_Name (X).all;
   end Exception_Name;

   --------------------
   -- Exception_Name --
   --------------------

   function Exception_Name (X : EO) return String is
   begin
      return Exception_Name (Exception_Identity (X));
   end Exception_Name;

   ----------
   -- Poll --
   ----------

   procedure Poll is separate;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception (E : Exception_Id; Message : String := "") is
      function Create_EO (E : Exception_Id; M : String) return EO;
      pragma Import (Java, Create_EO, "jgnat.adalib.GNAT_libc.create_EO");
      --  Creates an EO corresponding to E containing Message.
   begin
      if E /= Null_Id then
         Reraise_Occurrence (Create_EO (E, Message));
      end if;
   end Raise_Exception;

   ------------------------
   -- Reraise_Occurrence --
   ------------------------

   procedure Reraise_Occurrence (X : EO) is
   begin
      if X /= Null_Occurrence then
         Reraise_Occurrence_No_Defer (X);
      end if;
   end Reraise_Occurrence;

   ---------------------
   -- Save_Occurrence --
   ---------------------

   procedure Save_Occurrence (Target : out EO; Source : EO) is
   begin
      Target := Source;
   end Save_Occurrence;

   function Save_Occurrence (Source : EO) return EOA is
      Target : EOA := new EO;
   begin
      Save_Occurrence (Target.all, Source);
      return Target;
   end Save_Occurrence;

end Ada.Exceptions;
