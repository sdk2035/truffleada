------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                         A D A . R E A L _ T I M E                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.32 $                             --
--                                                                          --
--            Copyright (C) 1991-1999, Florida State University             --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

with System.Task_Primitives.Operations;
--  used for Clock

package body Ada.Real_Time is

   -----------
   -- Clock --
   -----------

   function Clock return Time is
   begin
      return Time (System.Task_Primitives.Operations.Clock);
   end Clock;

   ---------
   -- "+" --
   ---------

   --  Note that Constraint_Error may be propagated

   function "+" (Left : Time; Right : Time_Span) return Time is
   begin
      return Time (Duration (Left) + Duration (Right));
   end "+";

   function "+" (Left : Time_Span; Right : Time) return Time is
   begin
      return Time (Duration (Left) + Duration (Right));
   end "+";

   function "+" (Left, Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Duration (Left) + Duration (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   --  Note that Constraint_Error may be propagated

   function "-" (Left : Time; Right : Time_Span) return Time is
   begin
      return Time (Duration (Left) - Duration (Right));
   end "-";

   function "-" (Left, Right : Time) return Time_Span is
   begin
      return Time_Span (Duration (Left) - Duration (Right));
   end "-";

   function "-" (Left, Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Duration (Left) - Duration (Right));
   end "-";

   function "-" (Right : Time_Span) return Time_Span is
   begin
      return Time_Span_Zero - Right;
   end "-";

   ---------
   -- "/" --
   ---------

   --  Note that Constraint_Error may be propagated

   function "/" (Left, Right : Time_Span) return Integer is
   begin
      return Integer (Duration (Left) / Duration (Right));
   end "/";

   function "/" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Time_Span (Duration (Left) / Right);
   end "/";

   ---------
   -- "*" --
   ---------

   --  Note that Constraint_Error may be propagated

   function "*" (Left : Time_Span; Right : Integer) return Time_Span is
   begin
      return Time_Span (Duration (Left) * Right);
   end "*";

   function "*" (Left : Integer; Right : Time_Span) return Time_Span is
   begin
      return Time_Span (Left * Duration (Right));
   end "*";

   -----------------
   -- To_Duration --
   -----------------

   function To_Duration (TS : Time_Span) return Duration is
   begin
      return Duration (TS);
   end To_Duration;

   ------------------
   -- To_Time_Span --
   ------------------

   function To_Time_Span (D : Duration) return Time_Span is
   begin
      return Time_Span (D);
   end To_Time_Span;

   -----------------
   -- Nanoseconds --
   -----------------

   function Nanoseconds (NS : Integer) return Time_Span is
   begin
      return Time_Span_Unit * NS;
   end Nanoseconds;

   ------------------
   -- Microseconds --
   ------------------

   function Microseconds (US : Integer) return Time_Span is
   begin
      return Time_Span_Unit * US * 1_000;
   end Microseconds;

   -------------------
   --  Milliseconds --
   -------------------

   function Milliseconds (MS : Integer) return Time_Span is
   begin
      return Time_Span_Unit * MS * 1_000_000;
   end Milliseconds;

   -----------
   -- Split --
   -----------

   procedure Split (T : Time; SC : out Seconds_Count; TS : out Time_Span) is
   begin
      --  Extract the integer part of T

      if T = 0.0 then
         SC := 0;
      else
         SC := Seconds_Count (Time_Span'(T - 0.5));
      end if;

      --  Since we loose precision when converting a time value to float,
      --  we need to add this check

      if Time (SC) > T then
         SC := SC - 1;
      end if;

      TS := T - Time (SC);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of (SC : Seconds_Count; TS : Time_Span) return Time is
   begin
      return Time (SC) + TS;
   end Time_Of;

end Ada.Real_Time;
