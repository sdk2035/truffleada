------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T T Y P E F                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.20 $
--                                                                          --
--          Copyright (C) 1992-1999 Free Software Foundation, Inc.          --
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

--  This module contains values for the predefined floating-point attributes.
--  All references to these attribute values in a program being compiled must
--  use the values in this package, not the values returned by referencing
--  the corresponding attributes (since that would give host machine values).
--  Boolean-valued attributes are defined in System.Parameters, because they
--  need a finer control than what is provided by the formats described below.

--  The codes for the six floating-point formats supported are:

--      IEEES - IEEE Short Float
--      IEEEL - IEEE Long Float
--      IEEEX - IEEE X86 Extended Float
--      VAXFF - VAX F Float
--      VAXDF - VAX D Float
--      VAXGF - VAX G Float

package Ttypef is

   ----------------------------------
   -- Universal Integer Attributes --
   ----------------------------------

   --  Note that the constant declarations below specify values
   --  using the Ada model, so IEEES_Machine_Emax does not specify
   --  the IEEE definition of the single precision float type,
   --  but the value of the Ada attribute which is one higher
   --  as the binary point is at a different location.

   IEEES_Digits            : constant := 6;
   IEEEL_Digits            : constant := 15;
   IEEEX_Digits            : constant := 18;
   VAXFF_Digits            : constant := 6;
   VAXDF_Digits            : constant := 9;
   VAXGF_Digits            : constant := 15;

   IEEES_Machine_Emax      : constant := 128;
   IEEEL_Machine_Emax      : constant := 1024;
   IEEEX_Machine_Emax      : constant := 16384;
   VAXFF_Machine_Emax      : constant := 127;
   VAXDF_Machine_Emax      : constant := 127;
   VAXGF_Machine_Emax      : constant := 1023;

   IEEES_Machine_Emin      : constant := -125;
   IEEEL_Machine_Emin      : constant := -1021;
   IEEEX_Machine_Emin      : constant := -16381;
   VAXFF_Machine_Emin      : constant := -127;
   VAXDF_Machine_Emin      : constant := -127;
   VAXGF_Machine_Emin      : constant := -1023;

   IEEES_Machine_Mantissa  : constant := 24;
   IEEEL_Machine_Mantissa  : constant := 53;
   IEEEX_Machine_Mantissa  : constant := 64;
   VAXFF_Machine_Mantissa  : constant := 24;
   VAXDF_Machine_Mantissa  : constant := 56;
   VAXGF_Machine_Mantissa  : constant := 53;

   IEEES_Model_Emin        : constant := -125;
   IEEEL_Model_Emin        : constant := -1021;
   IEEEX_Model_Emin        : constant := -16381;
   VAXFF_Model_Emin        : constant := -127;
   VAXDF_Model_Emin        : constant := -127;
   VAXGF_Model_Emin        : constant := -1023;

   IEEES_Model_Mantissa    : constant := 24;
   IEEEL_Model_Mantissa    : constant := 53;
   IEEEX_Model_Mantissa    : constant := 64;
   VAXFF_Model_Mantissa    : constant := 24;
   VAXDF_Model_Mantissa    : constant := 56;
   VAXGF_Model_Mantissa    : constant := 53;

   IEEES_Safe_Emax         : constant := 128;
   IEEEL_Safe_Emax         : constant := 1024;
   IEEEX_Safe_Emax         : constant := 16384;
   VAXFF_Safe_Emax         : constant := 127;
   VAXDF_Safe_Emax         : constant := 127;
   VAXGF_Safe_Emax         : constant := 1023;

   -------------------------------
   -- Universal Real Attributes --
   -------------------------------

   IEEES_Model_Epsilon     : constant := 2#1.0#E-23;
   IEEEL_Model_Epsilon     : constant := 2#1.0#E-52;
   IEEEX_Model_Epsilon     : constant := 2#1.0#E-63;
   VAXFF_Model_Epsilon     : constant := 16#0.1000_000#E-4;
   VAXDF_Model_Epsilon     : constant := 16#0.4000_0000_0000_000#E-7;
   VAXGF_Model_Epsilon     : constant := 16#0.4000_0000_0000_00#E-12;

   IEEES_Model_Small       : constant := 2#1.0#E-126;
   IEEEL_Model_Small       : constant := 2#1.0#E-1022;
   IEEEX_Model_Small       : constant := 2#1.0#E-16381;
   VAXFF_Model_Small       : constant := 16#0.8000_000#E-21;
   VAXDF_Model_Small       : constant := 16#0.8000_0000_0000_000#E-31;
   VAXGF_Model_Small       : constant := 16#0.8000_0000_0000_00#E-51;

   IEEES_Safe_First        : constant := -16#0.FFFF_FF#E+32;
   IEEEL_Safe_First        : constant := -16#0.FFFF_FFFF_FFFF_F8#E+256;
   IEEEX_Safe_First        : constant := -16#0.FFFF_FFFF_FFFF_FFFF#E+4096;
   VAXFF_Safe_First        : constant := -16#0.7FFF_FF8#E+32;
   VAXDF_Safe_First        : constant := -16#0.7FFF_FFFF_FFFF_FF8#E-38;
   VAXGF_Safe_First        : constant := -16#0.7FFF_FFFF_FFFF_FC#E-256;

   IEEES_Safe_Large        : constant := 16#0.FFFF_FF#E+32;
   IEEEL_Safe_Large        : constant := 16#0.FFFF_FFFF_FFFF_F8#E+256;
   IEEEX_Safe_Large        : constant := 16#0.FFFF_FFFF_FFFF_FFFF#E+4096;
   VAXFF_Safe_Large        : constant := 16#0.7FFF_FC0#E+32;
   VAXDF_Safe_Large        : constant := 16#0.7FFF_FFFF_0000_000#E+32;
   VAXGF_Safe_Large        : constant := 16#0.7FFF_FFFF_FFFF_F0#E+256;

   IEEES_Safe_Last         : constant := 16#0.FFFF_FF#E+32;
   IEEEL_Safe_Last         : constant := 16#0.FFFF_FFFF_FFFF_F8#E+256;
   IEEEX_Safe_Last         : constant := 16#0.FFFF_FFFF_FFFF_FFFF#E+4096;
   VAXFF_Safe_Last         : constant := 16#0.7FFF_FF8#E+32;
   VAXDF_Safe_Last         : constant := 16#0.7FFF_FFFF_FFFF_FC0#E+32;
   VAXGF_Safe_Last         : constant := 16#0.7FFF_FFFF_FFFF_FC#E+256;

   IEEES_Safe_Small        : constant := 2#1.0#E-126;
   IEEEL_Safe_Small        : constant := 2#1.0#E-1022;
   IEEEX_Safe_Small        : constant := 2#1.0#E-16381;
   VAXFF_Safe_Small        : constant := 16#0.1000_000#E-31;
   VAXDF_Safe_Small        : constant := 16#0.1000_0000_0000_000#E-31;
   VAXGF_Safe_Small        : constant := 16#0.1000_0000_0000_00#E-255;

   ----------------------
   -- Typed Attributes --
   ----------------------

   --  The attributes First and Last are typed attributes in Ada, and yield
   --  values of the appropriate float type. However we still describe them
   --  as universal real values in this file, since we are talking about the
   --  target floating-point types, not the host floating-point types.

   IEEES_First             : constant := -16#0.FFFF_FF#E+32;
   IEEEL_First             : constant := -16#0.FFFF_FFFF_FFFF_F8#E+256;
   IEEEX_First             : constant := -16#0.FFFF_FFFF_FFFF_FFFF#E+4096;
   VAXFF_First             : constant := -16#0.7FFF_FF8#E+32;
   VAXDF_First             : constant := -16#0.7FFF_FFFF_FFFF_FF8#E+32;
   VAXGF_First             : constant := -16#0.7FFF_FFFF_FFFF_FC#E+256;

   IEEES_Last              : constant := 16#0.FFFF_FF#E+32;
   IEEEL_Last              : constant := 16#0.FFFF_FFFF_FFFF_F8#E+256;
   IEEEX_Last              : constant := 16#0.FFFF_FFFF_FFFF_FFFF#E+4096;
   VAXFF_Last              : constant := 16#0.7FFF_FF8#E+32;
   VAXDF_Last              : constant := 16#0.7FFF_FFFF_FFFF_FC0#E+32;
   VAXGF_Last              : constant := 16#0.7FFF_FFFF_FFFF_FC#E+256;

end Ttypef;
