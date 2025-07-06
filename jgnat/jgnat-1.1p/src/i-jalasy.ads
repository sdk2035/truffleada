------------------------------------------------------------------------------
--                                                                          --
--                        JGNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          I N T E R F A C E S . J A V A . L A N G . S Y S T E M           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $
--                                                                          --
--              Copyright (C) 1999 Ada Core Technologies, Inc.              --
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
-- JGNAT - The GNAT Ada 95 toolchain for the Java Virtual Machine is        --
--         maintained by Ada Core Technologies, Inc. - http://www.gnat.com  --
--                                                                          --
------------------------------------------------------------------------------

--  This child package corresponds to the Java class java.lang.System.
--  Please consult package Interfaces.Java for an explanation of the
--  mapping between a Java Class and an Ada package.

package Interfaces.Java.Lang.System is
   pragma Preelaborate;

   function Current_Time_Millis return long;

private
   pragma Import (Java, Current_Time_Millis, "currentTimeMillis");
end Interfaces.Java.Lang.System;

pragma Import (Java, Interfaces.Java.Lang.System, "java.lang.System");
