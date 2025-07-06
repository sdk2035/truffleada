------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      G N A T . E X C E P T I O N S                       --
--                                                                          --
--                                 S p e c                                  --
--                              (JVM Version)                               --
--                                                                          --
--                            $Revision: 1.2 $
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides an interface for raising predefined exceptions
--  with an exception message. It can be used from Pure units.

with System;

package GNAT.Exceptions is
pragma Pure (Exceptions);

   type Exception_Type is private;
   --  Type used to specify which exception to raise.

   CE : constant Exception_Type;  -- Constraint_Error
   PE : constant Exception_Type;  -- Program_Error
   SE : constant Exception_Type;  -- Storage_Error
   TE : constant Exception_Type;  -- Tasking_Error
   --  One of these constants is used in the call to specify the exception

   procedure Raise_Exception (E : Exception_Type; Message : String);
   pragma Import (C, Raise_Exception, "__gnat_raise_exception");
   pragma No_Return (Raise_Exception);
   --  Raise specified exception with specified message

private
   --  Really Exception_Type is Exception_Id, but Exception_Id can't be
   --  used directly since it is declared in the non-pure unit Ada.Exceptions,

   --  Exception_Id is in fact simply a pointer to the type Exception_Data
   --  declared in System.Standard_Library (which is also non-pure). So what
   --  we do is to define it here as a derivation of System.Address, and
   --  then Import the definitions from Standard_Library.

   type Exception_Type is new System.Address;

   pragma Import (C, CE, "constraint_error");
   pragma Import (C, PE, "program_error");
   pragma Import (C, SE, "storage_error");
   pragma Import (C, TE, "tasking_error");
   --  References to the exception structures in the standard library

end GNAT.Exceptions;
