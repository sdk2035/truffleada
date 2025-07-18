------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--               A D A . S T R I N G S . W I D E _ F I X E D                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.17 $                             --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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


with Ada.Strings.Wide_Maps; use Ada.Strings.Wide_Maps;
with Ada.Strings.Wide_Search;

package body Ada.Strings.Wide_Fixed is

   ------------------------
   -- Search Subprograms --
   ------------------------

   function Index
     (Source  : in Wide_String;
      Pattern : in Wide_String;
      Going   : in Direction := Forward;
      Mapping : in Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return    Natural
   renames Ada.Strings.Wide_Search.Index;

   function Index
     (Source  : in Wide_String;
      Pattern : in Wide_String;
      Going   : in Direction := Forward;
      Mapping : in Wide_Maps.Wide_Character_Mapping_Function)
      return    Natural
   renames Ada.Strings.Wide_Search.Index;

   function Index
     (Source : in Wide_String;
      Set    : in Wide_Maps.Wide_Character_Set;
      Test   : in Membership := Inside;
      Going  : in Direction  := Forward)
      return   Natural
   renames Ada.Strings.Wide_Search.Index;

   function Index_Non_Blank
     (Source : in Wide_String;
      Going  : in Direction := Forward)
      return   Natural
   renames Ada.Strings.Wide_Search.Index_Non_Blank;

   function Count
     (Source  : in Wide_String;
      Pattern : in Wide_String;
      Mapping : in Wide_Maps.Wide_Character_Mapping := Wide_Maps.Identity)
      return    Natural
   renames Ada.Strings.Wide_Search.Count;

   function Count
     (Source   : in Wide_String;
      Pattern  : in Wide_String;
      Mapping  : in Wide_Maps.Wide_Character_Mapping_Function)
      return     Natural
   renames Ada.Strings.Wide_Search.Count;

   function Count
     (Source : in Wide_String;
      Set    : in Wide_Maps.Wide_Character_Set)
      return   Natural
   renames Ada.Strings.Wide_Search.Count;

   procedure Find_Token
     (Source : in Wide_String;
      Set    : in Wide_Maps.Wide_Character_Set;
      Test   : in Membership;
      First  : out Positive;
      Last   : out Natural)
   renames Ada.Strings.Wide_Search.Find_Token;

   ---------
   -- "*" --
   ---------

   function "*"
     (Left  : in Natural;
      Right : in Wide_Character)
      return  Wide_String
   is
      Result : Wide_String (1 .. Left);

   begin
      for J in Result'Range loop
         Result (J) := Right;
      end loop;

      return Result;
   end "*";

   function "*"
     (Left  : in Natural;
      Right : in Wide_String)
      return  Wide_String
   is
      Result : Wide_String (1 .. Left * Right'Length);
      Ptr    : Integer := 1;

   begin
      for J in 1 .. Left loop
         Result (Ptr .. Ptr + Right'Length - 1) := Right;
         Ptr := Ptr + Right'Length;
      end loop;

      return Result;
   end "*";

   ------------
   -- Delete --
   ------------

   function Delete
     (Source  : in Wide_String;
      From    : in Positive;
      Through : in Natural)
      return    Wide_String
   is
   begin
      if From not in Source'Range
        or else Through > Source'Last
      then
         raise Index_Error;

      elsif From > Through then
         return Source;

      else
         declare
            Result : constant Wide_String :=
                       Source (Source'First .. From - 1) &
                       Source (Through + 1 .. Source'Last);
         begin
            return Result;
         end;
      end if;
   end Delete;

   procedure Delete
     (Source  : in out Wide_String;
      From    : in Positive;
      Through : in Natural;
      Justify : in Alignment := Left;
      Pad     : in Wide_Character := Wide_Space)
   is
   begin
      Move (Source  => Delete (Source, From, Through),
            Target  => Source,
            Justify => Justify,
            Pad     => Pad);
   end Delete;

   ----------
   -- Head --
   ----------

   function Head
     (Source : in Wide_String;
      Count  : in Natural;
      Pad    : in Wide_Character := Wide_Space)
      return   Wide_String
   is
      Result : Wide_String (1 .. Count);

   begin
      if Count <= Source'Length then
         Result := Source (Source'First .. Source'First + Count - 1);

      else
         Result (1 .. Source'Length) := Source;

         for J in Source'Length + 1 .. Count loop
            Result (J) := Pad;
         end loop;
      end if;

      return Result;
   end Head;

   procedure Head
     (Source  : in out Wide_String;
      Count   : in Natural;
      Justify : in Alignment := Left;
      Pad     : in Wide_Character := Ada.Strings.Wide_Space)
   is
   begin
      Move (Source  => Head (Source, Count, Pad),
            Target  => Source,
            Drop    => Error,
            Justify => Justify,
            Pad     => Pad);
   end Head;

   ------------
   -- Insert --
   ------------

   function Insert
     (Source   : in Wide_String;
      Before   : in Positive;
      New_Item : in Wide_String)
      return     Wide_String
   is
      Result : Wide_String (1 .. Source'Length + New_Item'Length);

   begin
      if Before < Source'First or else Before > Source'Last + 1 then
         raise Index_Error;
      end if;

      Result := Source (Source'First .. Before - 1) & New_Item &
                Source (Before .. Source'Last);
      return Result;
   end Insert;

   procedure Insert
     (Source   : in out Wide_String;
      Before   : in Positive;
      New_Item : in Wide_String;
      Drop     : in Truncation := Error)
   is
   begin
      Move (Source => Insert (Source, Before, New_Item),
            Target => Source,
            Drop   => Drop);
   end Insert;

   ----------
   -- Move --
   ----------

   procedure Move
     (Source  : in  Wide_String;
      Target  : out Wide_String;
      Drop    : in  Truncation := Error;
      Justify : in  Alignment  := Left;
      Pad     : in  Wide_Character  := Wide_Space)
   is
      Sfirst  : constant Integer := Source'First;
      Slast   : constant Integer := Source'Last;
      Slength : constant Integer := Source'Length;

      Tfirst  : constant Integer := Target'First;
      Tlast   : constant Integer := Target'Last;
      Tlength : constant Integer := Target'Length;

      function Is_Padding (Item : Wide_String) return Boolean;
      --  Determinbe if all characters in Item are pad characters

      function Is_Padding (Item : Wide_String) return Boolean is
      begin
         for J in Item'Range loop
            if Item (J) /= Pad then
               return False;
            end if;
         end loop;

         return True;
      end Is_Padding;

   --  Start of processing for Move

   begin
      if Slength = Tlength then
         Target := Source;

      elsif Slength > Tlength then

         case Drop is
            when Left =>
               Target := Source (Slast - Tlength + 1 .. Slast);

            when Right =>
               Target := Source (Sfirst .. Sfirst + Tlength - 1);

            when Error =>
               case Justify is
                  when Left =>
                     if Is_Padding (Source (Sfirst + Tlength .. Slast)) then
                        Target :=
                          Source (Sfirst .. Sfirst + Target'Length - 1);
                     else
                        raise Length_Error;
                     end if;

                  when Right =>
                     if Is_Padding (Source (Sfirst .. Slast - Tlength)) then
                        Target := Source (Slast - Tlength + 1 .. Slast);
                     else
                        raise Length_Error;
                     end if;

                  when Center =>
                     raise Length_Error;
               end case;

         end case;

      --  Source'Length < Target'Length

      else
         case Justify is
            when Left =>
               Target (Tfirst .. Tfirst + Slength - 1) := Source;

               for J in Tfirst + Slength .. Tlast loop
                  Target (J) := Pad;
               end loop;

            when Right =>
               for J in Tfirst .. Tlast - Slength loop
                  Target (J) := Pad;
               end loop;

               Target (Tlast - Slength + 1 .. Tlast) := Source;

            when Center =>
               declare
                  Front_Pad   : constant Integer := (Tlength - Slength) / 2;
                  Tfirst_Fpad : constant Integer := Tfirst + Front_Pad;

               begin
                  for J in Tfirst .. Tfirst_Fpad - 1 loop
                     Target (J) := Pad;
                  end loop;

                  Target (Tfirst_Fpad .. Tfirst_Fpad + Slength - 1) := Source;

                  for J in Tfirst_Fpad + Slength .. Tlast loop
                     Target (J) := Pad;
                  end loop;
               end;
         end case;
      end if;
   end Move;

   ---------------
   -- Overwrite --
   ---------------

   function Overwrite
     (Source   : in Wide_String;
      Position : in Positive;
      New_Item : in Wide_String)
      return     Wide_String
   is
   begin
      if Position not in Source'First .. Source'Last + 1 then
         raise Index_Error;
      else
         declare
            Result_Length : Natural :=
                Natural'Max (Source'Length,
                             Position - Source'First + New_Item'Length);
            Result : Wide_String (1 .. Result_Length);

         begin
            Result := Source (Source'First .. Position - 1) & New_Item &
                     Source (Position + New_Item'Length .. Source'Last);
            return Result;
         end;
      end if;
   end Overwrite;

   procedure Overwrite
     (Source   : in out Wide_String;
      Position : in Positive;
      New_Item : in Wide_String;
      Drop     : in Truncation := Right)
   is
   begin
      Move (Source => Overwrite (Source, Position, New_Item),
            Target => Source,
            Drop   => Drop);
   end Overwrite;

   -------------------
   -- Replace_Slice --
   -------------------

   function Replace_Slice
     (Source   : in Wide_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in Wide_String)
      return     Wide_String
   is
      Result_Length : Natural;

   begin
      if Low > Source'Last + 1 or else High < Source'First - 1 then
         raise Index_Error;
      else
         Result_Length :=
           Source'Length - Natural'Max (High - Low + 1, 0) + By'Length;

         declare
            Result : Wide_String (1 .. Result_Length);

         begin
            if High >= Low then
               Result :=
                  Source (Source'First .. Low - 1) & By &
                  Source (High + 1 .. Source'Last);
            else
               Result := Source (Source'First .. Low - 1) & By &
                         Source (Low .. Source'Last);
            end if;

            return Result;
         end;
      end if;
   end Replace_Slice;

   procedure Replace_Slice
     (Source   : in out Wide_String;
      Low      : in Positive;
      High     : in Natural;
      By       : in Wide_String;
      Drop     : in Truncation := Error;
      Justify  : in Alignment  := Left;
      Pad      : in Wide_Character  := Wide_Space)
   is
   begin
      Move (Replace_Slice (Source, Low, High, By), Source, Drop, Justify, Pad);
   end Replace_Slice;

   ----------
   -- Tail --
   ----------

   function Tail
     (Source : in Wide_String;
      Count  : in Natural;
      Pad    : in Wide_Character := Wide_Space)
      return   Wide_String
   is
      Result : Wide_String (1 .. Count);

   begin
      if Count < Source'Length then
         Result := Source (Source'Last - Count + 1 .. Source'Last);

      --  Pad on left

      else
         for J in 1 .. Count - Source'Length loop
            Result (J) := Pad;
         end loop;

         Result (Count - Source'Length + 1 .. Count) := Source;
      end if;

      return Result;
   end Tail;

   procedure Tail
     (Source  : in out Wide_String;
      Count   : in Natural;
      Justify : in Alignment := Left;
      Pad     : in Wide_Character := Ada.Strings.Wide_Space)
   is
   begin
      Move (Source  => Tail (Source, Count, Pad),
            Target  => Source,
            Drop    => Error,
            Justify => Justify,
            Pad     => Pad);
   end Tail;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Source  : in Wide_String;
      Mapping : in Wide_Maps.Wide_Character_Mapping)
      return    Wide_String
   is
      Result : Wide_String (1 .. Source'Length);

   begin
      for J in Source'Range loop
         Result (J - (Source'First - 1)) := Value (Mapping, Source (J));
      end loop;

      return Result;
   end Translate;

   procedure Translate
     (Source  : in out Wide_String;
      Mapping : in Wide_Maps.Wide_Character_Mapping)
   is
   begin
      for J in Source'Range loop
         Source (J) := Value (Mapping, Source (J));
      end loop;
   end Translate;

   function Translate
     (Source  : in Wide_String;
      Mapping : in Wide_Maps.Wide_Character_Mapping_Function)
      return    Wide_String
   is
      Result : Wide_String (1 .. Source'Length);

   begin
      for J in Source'Range loop
         Result (J - (Source'First - 1)) := Mapping (Source (J));
      end loop;

      return Result;
   end Translate;

   procedure Translate
     (Source  : in out Wide_String;
      Mapping : in Wide_Maps.Wide_Character_Mapping_Function)
   is
   begin
      for J in Source'Range loop
         Source (J) := Mapping (Source (J));
      end loop;
   end Translate;

   ----------
   -- Trim --
   ----------

   function Trim
     (Source : in Wide_String;
      Side   : in Trim_End)
      return   Wide_String
   is
      Low  : Natural := Source'First;
      High : Natural := Source'Last;

   begin
      if Side = Left or else Side = Both then
         while Low <= High and then Source (Low) = Wide_Space loop
            Low := Low + 1;
         end loop;
      end if;

      if Side = Right or else Side = Both then
         while High >= Low and then Source (High) = Wide_Space loop
            High := High - 1;
         end loop;
      end if;

      --  All blanks case

      if Low > High then
         return "";

      --  At least one non-blank

      else
         declare
            Result : Wide_String (1 .. High - Low + 1) := Source (Low .. High);

         begin
            return Result;
         end;
      end if;
   end Trim;

   procedure Trim
     (Source  : in out Wide_String;
      Side    : in Trim_End;
      Justify : in Alignment      := Left;
      Pad     : in Wide_Character := Wide_Space)
   is
   begin
      Move (Source  => Trim (Source, Side),
            Target  => Source,
            Justify => Justify,
            Pad     => Pad);
   end Trim;

   function Trim
      (Source : in Wide_String;
       Left   : in Wide_Maps.Wide_Character_Set;
       Right  : in Wide_Maps.Wide_Character_Set)
       return   Wide_String
   is
      Low  : Natural := Source'First;
      High : Natural := Source'Last;

   begin
      while Low <= High and then Is_In (Source (Low), Left) loop
         Low := Low + 1;
      end loop;

      while High >= Low and then Is_In (Source (High), Right) loop
         High := High - 1;
      end loop;

      --  Case where source comprises only characters in the sets

      if Low > High then
         return "";
      else
         declare
            subtype WS is Wide_String (1 .. High - Low + 1);

         begin
            return WS (Source (Low .. High));
         end;
      end if;
   end Trim;

   procedure Trim
      (Source  : in out Wide_String;
       Left    : in Wide_Maps.Wide_Character_Set;
       Right   : in Wide_Maps.Wide_Character_Set;
       Justify : in Alignment      := Strings.Left;
       Pad     : in Wide_Character := Wide_Space)
   is
   begin
      Move (Source  => Trim (Source, Left, Right),
            Target  => Source,
            Justify => Justify,
            Pad     => Pad);
   end Trim;

end Ada.Strings.Wide_Fixed;
