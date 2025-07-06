------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D B U G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.39 $
--                                                                          --
--          Copyright (C) 1996-2000 Free Software Foundation, Inc.          --
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

with Alloc;    use Alloc;
with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Output;   use Output;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Table;
with Urealp;   use Urealp;

package body Exp_Dbug is

   --  The following table is used to queue up the entities passed as
   --  arguments to Qualify_Entity_Names for later processing when
   --  Qualify_All_Entity_Names is called.

   package Name_Qualify_Units is new Table.Table (
     Table_Component_Type => Node_Id,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.Name_Qualify_Units_Initial,
     Table_Increment      => Alloc.Name_Qualify_Units_Increment,
     Table_Name           => "Name_Qualify_Units");

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Add_Uint_To_Buffer (U : Uint);
   --  Add image of universal integer to Name_Buffer, updating Name_Len

   procedure Add_Real_To_Buffer (U : Ureal);
   --  Add nnn_ddd to Name_Buffer, where nnn and ddd are integer values of
   --  the normalized numerator and denominator of the given real value.

   function Bounds_Match_Size (E : Entity_Id) return  Boolean;
   --  Determine whether the bounds of E match the size of the type. This is
   --  used to determine whether encoding is required for a discrete type.

   procedure Prepend_String_To_Buffer (S : String);
   --  Prepend given string to the contents of the string buffer, updating
   --  the value in Name_Len (i.e. string is added at start of buffer).

   procedure Prepend_Uint_To_Buffer (U : Uint);
   --  Prepend image of universal integer to Name_Buffer, updating Name_Len

   procedure Qualify_Entity_Name (Ent : Entity_Id);
   --  If not already done, replaces the Chars field of the given entity
   --  with the appropriate fully qualified name.

   procedure Strip_BNPE_Suffix (Suffix_Found : in out Boolean);
   --  Given an qualified entity name in Name_Buffer, remove any plain X or
   --  X{nb} qualification suffix. The contents of Name_Buffer is not changed
   --  but Name_Len may be adjusted on return to remove the suffix. If a
   --  suffix is found and stripped, then Suffix_Found is set to True. If
   --  no suffix is found, then Suffix_Found is not modified.

   ------------------------
   -- Add_Real_To_Buffer --
   ------------------------

   procedure Add_Real_To_Buffer (U : Ureal) is
   begin
      Add_Uint_To_Buffer (Norm_Num (U));
      Add_Str_To_Name_Buffer ("_");
      Add_Uint_To_Buffer (Norm_Den (U));
   end Add_Real_To_Buffer;

   ------------------------
   -- Add_Uint_To_Buffer --
   ------------------------

   procedure Add_Uint_To_Buffer (U : Uint) is
   begin
      if U < 0 then
         Add_Uint_To_Buffer (-U);
         Add_Char_To_Name_Buffer ('m');
      else
         UI_Image (U, Decimal);
         Add_Str_To_Name_Buffer (UI_Image_Buffer (1 .. UI_Image_Length));
      end if;
   end Add_Uint_To_Buffer;

   -----------------------
   -- Bounds_Match_Size --
   -----------------------

   function Bounds_Match_Size (E : Entity_Id) return Boolean is
      Siz : Uint;

   begin
      if not Is_OK_Static_Subtype (E) then
         return False;

      elsif Is_Integer_Type (E)
        and then Subtypes_Statically_Match (E, Base_Type (E))
      then
         return True;

      --  Here we check if the static bounds match the natural size, which
      --  is the size passed through with the debugging information. This
      --  is the Esize rounded up to 8, 16, 32 or 64 as appropriate.

      else
         if Esize (E) <= 8 then
            Siz := Uint_8;
         elsif Esize (E) <= 16 then
            Siz := Uint_16;
         elsif Esize (E) <= 32 then
            Siz := Uint_32;
         else
            Siz := Uint_64;
         end if;

         if Is_Modular_Integer_Type (E) or else Is_Enumeration_Type (E) then
            return
              Expr_Rep_Value (Type_Low_Bound (E)) = 0
                and then
              2 ** Siz - Expr_Rep_Value (Type_High_Bound (E)) = 1;

         else
            return
              Expr_Rep_Value (Type_Low_Bound (E)) + 2 ** (Siz - 1) = 0
                and then
              2 ** (Siz - 1) - Expr_Rep_Value (Type_High_Bound (E)) = 1;
         end if;
      end if;
   end Bounds_Match_Size;

   --------------------------------
   -- Debug_Renaming_Declaration --
   --------------------------------

   function Debug_Renaming_Declaration (N : Node_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (N);
      Ent : constant Node_Id    := Defining_Entity (N);
      Nam : constant Node_Id    := Name (N);
      Rnm : Name_Id;
      Ren : Node_Id;
      Lit : Entity_Id;
      Typ : Entity_Id;
      Res : Node_Id;
      Def : Entity_Id;

      function Output_Subscript (N : Node_Id; S : String) return Boolean;
      --  Outputs a single subscript value as ?nnn (subscript is compile
      --  time known value with value nnn) or as ?e (subscript is local
      --  constant with name e), where S supplies the proper string to
      --  use for ?. Returns False if the subscript is not of an appropriate
      --  type to output in one of these two forms. The result is prepended
      --  to the name stored in Name_Buffer.

      function Output_Subscript (N : Node_Id; S : String) return Boolean is
      begin
         if Compile_Time_Known_Value (N) then
            Prepend_Uint_To_Buffer (Expr_Value (N));

         elsif Nkind (N) = N_Identifier
           and then Scope (Entity (N)) = Scope (Ent)
           and then Ekind (Entity (N)) = E_Constant
         then
            Prepend_String_To_Buffer (Get_Name_String (Chars (Entity (N))));

         else
            return False;
         end if;

         Prepend_String_To_Buffer (S);
         return True;
      end Output_Subscript;

   --  Start of processing for Debug_Renaming_Declaration

   begin
      if not Comes_From_Source (N) then
         return Empty;
      end if;

      --  Prepare entity name for type declaration

      Get_Name_String (Chars (Ent));

      case Nkind (N) is
         when N_Object_Renaming_Declaration =>
            Add_Str_To_Name_Buffer ("___XR");

         when N_Exception_Renaming_Declaration =>
            Add_Str_To_Name_Buffer ("___XRE");

         when N_Package_Renaming_Declaration =>
            Add_Str_To_Name_Buffer ("___XRP");

         when others =>
            return Empty;
      end case;

      Rnm := Name_Find;

      --  Get renamed entity and compute suffix

      Name_Len := 0;
      Ren := Nam;
      loop
         case Nkind (Ren) is

            when N_Identifier =>
               exit;

            when N_Expanded_Name =>

               --  The entity field for an N_Expanded_Name is on the
               --  expanded name node itself, so we are done here too.

               exit;

            when N_Selected_Component =>
               Prepend_String_To_Buffer
                 (Get_Name_String (Chars (Selector_Name (Ren))));
               Prepend_String_To_Buffer ("XR");
               Ren := Prefix (Ren);

            when N_Indexed_Component =>
               declare
                  X : Node_Id := Last (Expressions (Ren));

               begin
                  while Present (X) loop
                     if not Output_Subscript (X, "XS") then
                        Set_Materialize_Entity (Ent);
                        return Empty;
                     end if;

                     Prev (X);
                  end loop;
               end;

               Ren := Prefix (Ren);

            when N_Slice =>

               Typ := Etype (First_Index (Etype (Nam)));

               if not Output_Subscript (Type_High_Bound (Typ), "XS") then
                  Set_Materialize_Entity (Ent);
                  return Empty;
               end if;

               if not Output_Subscript (Type_Low_Bound  (Typ), "XL") then
                  Set_Materialize_Entity (Ent);
                  return Empty;
               end if;

               Ren := Prefix (Ren);

            when N_Explicit_Dereference =>
               Prepend_String_To_Buffer ("XA");
               Ren := Prefix (Ren);

            --  For now, anything else simply results in no translation

            when others =>
               Set_Materialize_Entity (Ent);
               return Empty;
         end case;
      end loop;

      Prepend_String_To_Buffer ("___XE");

      --  For now, the literal name contains only the suffix. The Entity_Id
      --  value for the name is used to create a link from this literal name
      --  to the renamed entity using the Debug_Renaming_Link field. Then the
      --  Qualify_Entity_Name procedure uses this link to create the proper
      --  fully qualified name.

      --  The reason we do things this way is that we really need to copy the
      --  qualification of the renamed entity, and it is really much easier to
      --  do this after the renamed entity has itself been fully qualified.

      Lit := Make_Defining_Identifier (Loc, Chars => Name_Enter);
      Set_Debug_Renaming_Link (Lit, Entity (Ren));

      --  Return the appropriate enumeration type

      Def := Make_Defining_Identifier (Loc, Chars => Rnm);
      Res :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Def,
          Type_Definition =>
            Make_Enumeration_Type_Definition (Loc,
              Literals => New_List (Lit)));

      Set_Needs_Debug_Info (Def);
      Set_Needs_Debug_Info (Lit);

      Set_Discard_Names (Defining_Identifier (Res));
      return Res;

   --  If we get an exception, just figure it is a case that we cannot
   --  successfully handle using our current approach, since this is
   --  only for debugging, no need to take the compilation with us!

   exception
      when others =>
         return Make_Null_Statement (Loc);

   end Debug_Renaming_Declaration;

   ----------------------
   -- Get_Encoded_Name --
   ----------------------

   --  Note: see spec for details on encodings

   procedure Get_Encoded_Name (E : Entity_Id) is
      Has_Suffix : Boolean;

   begin
      if not Is_Type (E) then
         Get_Name_String (Chars (E));
         Name_Buffer (Name_Len + 1) := ASCII.Nul;
         return;
      end if;

      Has_Suffix := True;

      --  Fixed-point case

      if Is_Fixed_Point_Type (E) then
         Get_External_Name_With_Suffix (E, "XF_");
         Add_Real_To_Buffer (Delta_Value (E));

         if Small_Value (E) /= Delta_Value (E) then
            Add_Str_To_Name_Buffer ("_");
            Add_Real_To_Buffer (Small_Value (E));
         end if;

      --  Vax floating-point case

      elsif Vax_Float (E) then

         if Digits_Value (Base_Type (E)) = 6 then
            Get_External_Name_With_Suffix (E, "XFF");

         elsif Digits_Value (Base_Type (E)) = 9 then
            Get_External_Name_With_Suffix (E, "XFF");

         elsif Digits_Value (Base_Type (E)) = 15 then
            Get_External_Name_With_Suffix (E, "XFG");

         else
            pragma Assert (False);
            raise Program_Error;
         end if;

      --  Discrete case where bounds do not match size

      elsif Is_Discrete_Type (E)
        and then not Bounds_Match_Size (E)
      then
         if Has_Biased_Representation (E) then
            Get_External_Name_With_Suffix (E, "XB");
         else
            Get_External_Name_With_Suffix (E, "XD");
         end if;

         declare
            Lo : constant Node_Id := Type_Low_Bound (E);
            Hi : constant Node_Id := Type_High_Bound (E);

            Lo_Stat : constant Boolean := Is_OK_Static_Expression (Lo);
            Hi_Stat : constant Boolean := Is_OK_Static_Expression (Hi);

            Lo_Discr : constant Boolean :=
                         Nkind (Lo) = N_Identifier
                           and then
                         Ekind (Entity (Lo)) = E_Discriminant;

            Hi_Discr : constant Boolean :=
                         Nkind (Hi) = N_Identifier
                           and then
                         Ekind (Entity (Hi)) = E_Discriminant;

            Lo_Encode : constant Boolean := Lo_Stat or Lo_Discr;
            Hi_Encode : constant Boolean := Hi_Stat or Hi_Discr;

         begin
            if Lo_Encode or Hi_Encode then
               if Lo_Encode then
                  if Hi_Encode then
                     Add_Str_To_Name_Buffer ("LU_");
                  else
                     Add_Str_To_Name_Buffer ("L_");
                  end if;
               else
                  Add_Str_To_Name_Buffer ("U_");
               end if;

               if Lo_Stat then
                  Add_Uint_To_Buffer (Expr_Rep_Value (Lo));
               elsif Lo_Discr then
                  Get_Name_String_And_Append (Chars (Entity (Lo)));
               end if;

               if Lo_Encode and Hi_Encode then
                  Add_Str_To_Name_Buffer ("__");
               end if;

               if Hi_Stat then
                  Add_Uint_To_Buffer (Expr_Rep_Value (Hi));
               elsif Hi_Discr then
                  Get_Name_String_And_Append (Chars (Entity (Hi)));
               end if;
            end if;
         end;

      --  For all other cases, the encoded name is the normal type name

      else
         Has_Suffix := False;
         Get_External_Name (E, Has_Suffix);
      end if;

      if Debug_Flag_B and then Has_Suffix then
         Write_Str ("**** type ");
         Write_Name (Chars (E));
         Write_Str (" is encoded as ");
         Write_Str (Name_Buffer (1 .. Name_Len));
         Write_Eol;
      end if;

      Name_Buffer (Name_Len + 1) := ASCII.NUL;
   end Get_Encoded_Name;

   -------------------
   -- Get_Entity_Id --
   -------------------

   function Get_Entity_Id (External_Name : String) return Entity_Id is
   begin
      return Empty;
   end Get_Entity_Id;

   -----------------------
   -- Get_External_Name --
   -----------------------

   procedure Get_External_Name (Entity : Entity_Id; Has_Suffix : Boolean)
   is
      E    : Entity_Id := Entity;
      Kind : Entity_Kind;

   begin
      Name_Len := 0;

      --  If this is a child unit, we want the child

      if Nkind (E) = N_Defining_Program_Unit_Name then
         E := Defining_Identifier (Entity);
      end if;

      Kind := Ekind (E);

      --  Case of interface name being used

      if (Kind = E_Procedure or Kind = E_Function
        or Kind = E_Constant or Kind = E_Variable
        or Kind = E_Exception)
        and then Present (Interface_Name (E))
        and then No (Address_Clause (E))
        and then not Has_Suffix
      then
         if Convention (E) = Convention_Stdcall
            and then Ekind (E) = E_Variable
         then
            Add_Str_To_Name_Buffer ("_imp__");
         end if;

         Add_String_To_Name_Buffer (Strval (Interface_Name (E)));


      --  All other cases besides the interface name case

      else
         --  If this is a library level subprogram (i.e. a subprogram that is a
         --  compilation unit other than a subunit), then we prepend _ada_ to
         --  ensure distinctions required as described in the spec.
         --  Check explicitly for child units, because those are not flagged
         --  as Compilation_Units by lib. Should they be ???

         if Is_Subprogram (E)
           and then (Is_Compilation_Unit (E) or Is_Child_Unit (E))
           and then not Has_Suffix
         then
            Add_Str_To_Name_Buffer ("_ada_");
         end if;

         --  If the entity is a subprogram instance that is not a compilation
         --  unit, generate the name of the original Ada entity, which is the
         --  one gdb needs.

         if Is_Generic_Instance (E)
           and then Is_Subprogram (E)
           and then not Is_Compilation_Unit (Scope (E))
         then
            E := Related_Instance (Scope (E));
         end if;

         Get_Qualified_Name_And_Append (E);

         if Has_Homonym (E) then
            declare
               H  : Entity_Id := Homonym (E);
               Nr : Nat := 1;

            begin
               while Present (H) loop
                  if (Scope (H) = Scope (E)) then
                     Nr := Nr + 1;
                  end if;

                  H := Homonym (H);
               end loop;

               if Nr > 1 then
                  Add_Char_To_Name_Buffer ('$');
                  Add_Nat_To_Name_Buffer (Nr);
               end if;
            end;
         end if;
      end if;

      Name_Buffer (Name_Len + 1) := ASCII.Nul;
   end Get_External_Name;

   -----------------------------------
   -- Get_External_Name_With_Suffix --
   -----------------------------------

   procedure Get_External_Name_With_Suffix
     (Entity : Entity_Id;
      Suffix : String)
   is
      Has_Suffix : constant Boolean := (Suffix /= "");
   begin
      Get_External_Name (Entity, Has_Suffix);

      if Has_Suffix then
         Add_Str_To_Name_Buffer ("___");
         Add_Str_To_Name_Buffer (Suffix);

         Name_Buffer (Name_Len + 1) := ASCII.Nul;
      end if;
   end Get_External_Name_With_Suffix;

   -----------------------------------
   -- Get_Qualified_Name_And_Append --
   -----------------------------------

   procedure Get_Qualified_Name_And_Append (Entity : Entity_Id) is
   begin
      --  If the entity is a compilation unit, its scope is Standard, there is
      --  no outer scope, and the name remains unqualified. If the front end
      --  has already computed a qualified name, then it is also the case that
      --  no further qualification is required

      if Present (Scope (Scope (Entity)))
        and then not Has_Qualified_Name (Entity)
      then
         Get_Qualified_Name_And_Append (Scope (Entity));
         Add_Str_To_Name_Buffer ("__");
      end if;

      Get_Name_String_And_Append (Chars (Entity));
   end Get_Qualified_Name_And_Append;

   --------------------------
   -- Get_Variant_Encoding --
   --------------------------

   procedure Get_Variant_Encoding (V : Node_Id) is
      Choice : Node_Id;

      procedure Choice_Val (Typ : Character; Choice : Node_Id);
      --  Output encoded value for a single choice value. Typ is the key
      --  character ('S', 'F', or 'T') that precedes the choice value.

      ----------------
      -- Choice_Val --
      ----------------

      procedure Choice_Val (Typ : Character; Choice : Node_Id) is
      begin
         Add_Char_To_Name_Buffer (Typ);

         if Nkind (Choice) = N_Integer_Literal then
            Add_Uint_To_Buffer (Intval (Choice));

         --  Character literal with no entity present (this is the case
         --  Standard.Character or Standard.Wide_Character as root type)

         elsif Nkind (Choice) = N_Character_Literal
           and then No (Entity (Choice))
         then
            Add_Uint_To_Buffer
              (UI_From_Int (Int (Char_Literal_Value (Choice))));

         else
            declare
               Ent : constant Entity_Id := Entity (Choice);

            begin
               if Ekind (Ent) = E_Enumeration_Literal then
                  Add_Uint_To_Buffer (Enumeration_Rep (Ent));

               else
                  pragma Assert (Ekind (Ent) = E_Constant);
                  Choice_Val (Typ, Constant_Value (Ent));
               end if;
            end;
         end if;
      end Choice_Val;

   --  Start of processing for Get_Variant_Encoding

   begin
      Name_Len := 0;

      Choice := First (Discrete_Choices (V));
      while Present (Choice) loop
         if Nkind (Choice) = N_Others_Choice then
            Add_Char_To_Name_Buffer ('O');

         elsif Nkind (Choice) = N_Range then
            Choice_Val ('R', Low_Bound (Choice));
            Choice_Val ('T', High_Bound (Choice));

         elsif Is_Entity_Name (Choice)
           and then Is_Type (Entity (Choice))
         then
            Choice_Val ('R', Type_Low_Bound (Entity (Choice)));
            Choice_Val ('T', Type_High_Bound (Entity (Choice)));

         elsif Nkind (Choice) = N_Subtype_Indication then
            declare
               Rang : constant Node_Id :=
                        Range_Expression (Constraint (Choice));
            begin
               Choice_Val ('R', Low_Bound (Rang));
               Choice_Val ('T', High_Bound (Rang));
            end;

         else
            Choice_Val ('S', Choice);
         end if;

         Next (Choice);
      end loop;

      Name_Buffer (Name_Len + 1) := ASCII.NUL;

      if Debug_Flag_B then
         declare
            VP : constant Node_Id := Parent (V);    -- Variant_Part
            CL : constant Node_Id := Parent (VP);   -- Component_List
            RD : constant Node_Id := Parent (CL);   -- Record_Definition
            FT : constant Node_Id := Parent (RD);   -- Full_Type_Declaration

         begin
            Write_Str ("**** variant for type ");
            Write_Name (Chars (Defining_Identifier (FT)));
            Write_Str (" is encoded as ");
            Write_Str (Name_Buffer (1 .. Name_Len));
            Write_Eol;
         end;
      end if;
   end Get_Variant_Encoding;

   ---------------------------------
   -- Make_Packed_Array_Type_Name --
   ---------------------------------

   function Make_Packed_Array_Type_Name
     (Typ   : Entity_Id;
      Csize : Uint)
      return  Name_Id
   is
   begin
      Get_Name_String (Chars (Typ));
      Add_Str_To_Name_Buffer ("___XP");
      Add_Uint_To_Buffer (Csize);
      return Name_Find;
   end Make_Packed_Array_Type_Name;

   ------------------------------
   -- Prepend_String_To_Buffer --
   ------------------------------

   procedure Prepend_String_To_Buffer (S : String) is
      N : constant Integer := S'Length;

   begin
      Name_Buffer (1 + N .. Name_Len + N) := Name_Buffer (1 .. Name_Len);
      Name_Buffer (1 .. N) := S;
      Name_Len := Name_Len + N;
   end Prepend_String_To_Buffer;

   ----------------------------
   -- Prepend_Uint_To_Buffer --
   ----------------------------

   procedure Prepend_Uint_To_Buffer (U : Uint) is
   begin
      if U < 0 then
         Prepend_String_To_Buffer ("m");
         Prepend_Uint_To_Buffer (-U);
      else
         UI_Image (U, Decimal);
         Prepend_String_To_Buffer (UI_Image_Buffer (1 .. UI_Image_Length));
      end if;
   end Prepend_Uint_To_Buffer;

   -------------------------
   -- Qualify_Entity_Name --
   -------------------------

   procedure Qualify_Entity_Name (Ent : Entity_Id) is

      Subprogram_Name : String (1 .. Name_Buffer'Length);
      Subprogram_Len  : Natural := 0;
      --  Used to accumulate fully qualified name of subprogram

      function Is_BNPE (S : Entity_Id) return Boolean;
      --  Determines if S is a BNPE, i.e. Body-Nested Package Entity, which
      --  is defined to be a package which is immediately nested within a
      --  package body.

      function Qualify_Needed (S : Entity_Id) return Boolean;
      --  Given a scope, determines if the scope is to be included in the
      --  fully qualified name, True if so, False if not.

      procedure Qualify_Subprogram_Name (E : Entity_Id);
      --  Used to qualify subprogram name, where full qualification up
      --  to Standard is always used. Name is set in Subprogram_Name
      --  with the length in Subprogram_Len. Note that this routine
      --  does not prepend the _ada_ string required for library
      --  subprograms (this is done in the back end).

      procedure Set_BNPE_Suffix (E : Entity_Id);
      --  Recursive routine to append the BNPE qualification suffix. Works
      --  from right to left with E being the current entity in the list.
      --  The result does NOT have the trailing n's and trailing b stripped.
      --  The caller must do this required stripping.

      procedure Set_Entity_Name (E : Entity_Id);
      --  Internal recursive routine that does most of the work. This routine
      --  leaves the result sitting in Name_Buffer and Name_Len.

      BNPE_Suffix_Needed : Boolean := False;
      --  Set true if a body-nested package entity suffix is required

      -------------
      -- Is_BNPE --
      -------------

      function Is_BNPE (S : Entity_Id) return Boolean is
      begin
         return
           Ekind (S) = E_Package
             and then Is_Package_Body_Entity (S);
      end Is_BNPE;

      --------------------
      -- Qualify_Needed --
      --------------------

      function Qualify_Needed (S : Entity_Id) return Boolean is
      begin
         return
           S /= Standard_Standard
             and then
               (Is_Subprogram (Ent)
                  or else
                Ekind (Ent) = E_Subprogram_Body
                  or else
                   (Ekind (S) /= E_Block
                      and then not Is_Dynamic_Scope (S)));
      end Qualify_Needed;

      -----------------------------
      -- Qualify_Subprogram_Name --
      -----------------------------

      procedure Qualify_Subprogram_Name (E : Entity_Id) is
      begin
         if Scope (E) /= Standard_Standard then
            Qualify_Subprogram_Name (Scope (E));
            Subprogram_Name (Subprogram_Len + 1) := '_';
            Subprogram_Name (Subprogram_Len + 2) := '_';
            Subprogram_Len := Subprogram_Len + 2;
         end if;

         if Has_Qualified_Name (E) then
            Get_Unqualified_Name_String (Chars (E));
         else
            Get_Name_String (Chars (E));
         end if;

         Subprogram_Name (Subprogram_Len + 1 .. Subprogram_Len + Name_Len) :=
           Name_Buffer (1 .. Name_Len);
         Subprogram_Len := Subprogram_Len + Name_Len;

         if Is_BNPE (E) then
            BNPE_Suffix_Needed := True;
         end if;
      end Qualify_Subprogram_Name;

      ---------------------
      -- Set_BNPE_Suffix --
      ---------------------

      procedure Set_BNPE_Suffix (E : Entity_Id) is
         S : constant Entity_Id := Scope (E);

      begin
         if Qualify_Needed (S) then
            Set_BNPE_Suffix (S);

            if Is_BNPE (E) then
               Add_Char_To_Name_Buffer ('b');
            else
               Add_Char_To_Name_Buffer ('n');
            end if;

         else
            Add_Char_To_Name_Buffer ('X');
         end if;

      end Set_BNPE_Suffix;

      ---------------------
      -- Set_Entity_Name --
      ---------------------

      procedure Set_Entity_Name (E : Entity_Id) is
         S : constant Entity_Id := Scope (E);

      begin
         --  If we reach an already qualified name, just take the encoding
         --  except that we strip the package body suffixes, since these
         --  will be separately put on later.

         if Has_Qualified_Name (E) then
            Get_Name_String_And_Append (Chars (E));
            Strip_BNPE_Suffix (BNPE_Suffix_Needed);

         --  Case where upper level name is not encoded yet

         else
            --  Recurse if further qualification required

            if Qualify_Needed (S) then
               Set_Entity_Name (S);
               Add_Str_To_Name_Buffer ("__");
            end if;

            --  Otherwise get name and note if it is a NPBE

            Get_Name_String_And_Append (Chars (E));

            if Is_BNPE (E) then
               BNPE_Suffix_Needed := True;
            end if;
         end if;
      end Set_Entity_Name;

   --  Start of processing for Qualify_Entity_Name

   begin
      if Has_Qualified_Name (Ent) then
         return;

      --  Here is where we create the proper link for renaming

      elsif Ekind (Ent) = E_Enumeration_Literal
        and then Present (Debug_Renaming_Link (Ent))
      then
         Set_Entity_Name (Debug_Renaming_Link (Ent));
         Get_Name_String (Chars (Ent));
         Prepend_String_To_Buffer
           (Get_Name_String (Chars (Debug_Renaming_Link (Ent))));
         Set_Chars (Ent, Name_Enter);
         Set_Has_Qualified_Name (Ent);
         return;

      elsif Is_Subprogram (Ent)
        or else Ekind (Ent) = E_Subprogram_Body
      then
         Qualify_Subprogram_Name (Ent);
         Name_Len := Subprogram_Len;
         Name_Buffer (1 .. Name_Len) := Subprogram_Name (1 .. Name_Len);

      elsif Qualify_Needed (Scope (Ent)) then
         Name_Len := 0;
         Set_Entity_Name (Ent);

      else
         Set_Has_Qualified_Name (Ent);
         return;
      end if;

      --  Fall through with a fully qualified name in Name_Buffer/Name_Len

      --  Add body-nested package suffix if required

      if BNPE_Suffix_Needed then
         Set_BNPE_Suffix (Ent);

         --  Strip trailing n's and last trailing b as required. note that
         --  we know there is at least one b, or no suffix would be generated.

         while Name_Buffer (Name_Len) = 'n' loop
            Name_Len := Name_Len - 1;
         end loop;

         Name_Len := Name_Len - 1;
      end if;

      Set_Chars (Ent, Name_Enter);
      Set_Has_Qualified_Name (Ent);
   end Qualify_Entity_Name;

   ------------------------------
   -- Qualify_All_Entity_Names --
   ------------------------------

   procedure Qualify_All_Entity_Names is
      E   : Entity_Id;
      Ent : Entity_Id;

   begin
      for J in Name_Qualify_Units.First .. Name_Qualify_Units.Last loop
         E := Defining_Entity (Name_Qualify_Units.Table (J));
         Qualify_Entity_Name (E);

         Ent := First_Entity (E);
         while Present (Ent) loop
            Qualify_Entity_Name (Ent);
            Next_Entity (Ent);

            --  There are odd cases where Last_Entity (E) = E. This happens
            --  in the case of renaming of packages. This test avoids getting
            --  stuck in such cases.

            exit when Ent = E;
         end loop;
      end loop;
   end Qualify_All_Entity_Names;

   --------------------------
   -- Qualify_Entity_Names --
   --------------------------

   procedure Qualify_Entity_Names (N : Node_Id) is
   begin
      Name_Qualify_Units.Append (N);
   end Qualify_Entity_Names;

   --------------------------------
   -- Save_Unitname_And_Use_List --
   --------------------------------

   procedure Save_Unitname_And_Use_List
     (Main_Unit_Node : Node_Id;
      Main_Kind      : Node_Kind)
   is
      INITIAL_NAME_LENGTH : constant := 1024;

      Item       : Node_Id;
      Pack_Name  : Node_Id;

      Unit_Spec  : Node_Id := 0;
      Unit_Body  : Node_Id := 0;

      Main_Name : String_Id;
      --  Fully qualified name of Main Unit

      Unit_Name : String_Id;
      --  Name of unit specified in a Use clause

      Spec_Unit_Index : Source_File_Index;
      Spec_File_Name  : File_Name_Type := No_File;

      Body_Unit_Index : Source_File_Index;
      Body_File_Name : File_Name_Type := No_File;

      type String_Ptr is access all String;

      Spec_File_Name_Str : String_Ptr;
      Body_File_Name_Str : String_Ptr;

      type Label is record
        Label_Name  : String_Ptr;
        Name_Length : Integer;
        Pos         : Integer;
      end record;

      Spec_Label : Label;
      Body_Label : Label;

      procedure Initialize  (L : out Label);
      --  Initialize label

      procedure Append      (L : in out Label; Ch : Character);
      --  Append character to label

      procedure Append      (L : in out Label; Str : String);
      --  Append string to label

      procedure Append_Name (L : in out Label; Unit_Name : String_Id);
      --  Append name to label

      function  Sufficient_Space
        (L         : Label;
         Unit_Name : String_Id)
         return      Boolean;
      --  Does sufficient space exist to append another name?

      procedure Initialize (L : out Label) is
      begin
         L.Name_Length := INITIAL_NAME_LENGTH;
         L.Pos := 0;
         L.Label_Name := new String (1 .. L.Name_Length);
      end Initialize;

      procedure Append (L : in out Label; Str : String) is
      begin
         L.Label_Name (L.Pos + 1 .. L.Pos + Str'Length) := Str;
         L.Pos := L.Pos + Str'Length;
      end Append;

      procedure Append (L : in out Label; Ch : Character) is
      begin
         L.Pos := L.Pos + 1;
         L.Label_Name (L.Pos) := Ch;
      end Append;

      procedure Append_Name (L : in out Label; Unit_Name : String_Id) is
         Char         : Char_Code;
         Upper_Offset : constant := Character'Pos ('a') - Character'Pos ('A');

      begin
         for J in 1 .. String_Length (Unit_Name) loop
            Char := Get_String_Char (Unit_Name, J);

            if Character'Val (Char) = '.' then
               Append (L, "__");
            elsif Character'Val (Char) in 'A' .. 'Z' then
               Append (L, Character'Val (Char + Upper_Offset));
            elsif Char /= 0 then
               Append (L, Character'Val (Char));
            end if;
         end loop;
      end Append_Name;

      function  Sufficient_Space
        (L         : Label;
         Unit_Name : String_Id)
         return      Boolean
      is
         Len : Integer := Integer (String_Length (Unit_Name)) + 1;

      begin
         for J in 1 .. String_Length (Unit_Name) loop
            if Character'Val (Get_String_Char (Unit_Name, J)) = '.' then
               Len := Len + 1;
            end if;
         end loop;

         return L.Pos + Len < L.Name_Length;
      end Sufficient_Space;

   --  Start of processing for Save_Unitname_And_Use_List

   begin
      Initialize (Spec_Label);
      Initialize (Body_Label);

      case Main_Kind is
         when N_Package_Declaration =>
            Main_Name := Full_Qualified_Name
              (Defining_Unit_Name (Specification (Unit (Main_Unit_Node))));
            Unit_Spec := Main_Unit_Node;
            Append (Spec_Label, "_LPS__");
            Append (Body_Label, "_LPB__");

         when N_Package_Body =>
            Unit_Spec := Corresponding_Spec (Unit (Main_Unit_Node));
            Unit_Body := Main_Unit_Node;
            Main_Name := Full_Qualified_Name (Unit_Spec);
            Append (Spec_Label, "_LPS__");
            Append (Body_Label, "_LPB__");

         when N_Subprogram_Body =>
            Unit_Body := Main_Unit_Node;

            if Present (Corresponding_Spec (Unit (Main_Unit_Node))) then
               Unit_Spec := Corresponding_Spec (Unit (Main_Unit_Node));
               Main_Name := Full_Qualified_Name
                 (Corresponding_Spec (Unit (Main_Unit_Node)));
            else
               Main_Name := Full_Qualified_Name
                 (Defining_Unit_Name (Specification (Unit (Main_Unit_Node))));
            end if;

            Append (Spec_Label, "_LSS__");
            Append (Body_Label, "_LSB__");

         when others =>
            return;
      end case;

      Append_Name (Spec_Label, Main_Name);
      Append_Name (Body_Label, Main_Name);

      --  If we have a body, process it first

      if Present (Unit_Body) then

         Item := First (Context_Items (Unit_Body));

         while Present (Item) loop
            if Nkind (Item) = N_Use_Package_Clause then
               Pack_Name := First (Names (Item));
               while Present (Pack_Name) loop
                  Unit_Name := Full_Qualified_Name (Entity (Pack_Name));

                  if Sufficient_Space (Body_Label, Unit_Name) then
                     Append (Body_Label, '$');
                     Append_Name (Body_Label, Unit_Name);
                  end if;

                  Pack_Name := Next (Pack_Name);
               end loop;
            end if;

            Item := Next (Item);
         end loop;
      end if;

      while Present (Unit_Spec) and then
        Nkind (Unit_Spec) /= N_Compilation_Unit
      loop
         Unit_Spec := Parent (Unit_Spec);
      end loop;

      if Present (Unit_Spec) then

         Item := First (Context_Items (Unit_Spec));

         while Present (Item) loop
            if Nkind (Item) = N_Use_Package_Clause then
               Pack_Name := First (Names (Item));
               while Present (Pack_Name) loop
                  Unit_Name := Full_Qualified_Name (Entity (Pack_Name));

                  if Sufficient_Space (Spec_Label, Unit_Name) then
                     Append (Spec_Label, '$');
                     Append_Name (Spec_Label, Unit_Name);
                  end if;

                  if Sufficient_Space (Body_Label, Unit_Name) then
                     Append (Body_Label, '$');
                     Append_Name (Body_Label, Unit_Name);
                  end if;

                  Pack_Name := Next (Pack_Name);
               end loop;
            end if;

            Item := Next (Item);
         end loop;
      end if;

      if Present (Unit_Spec) then
         Append (Spec_Label, Character'Val (0));
         Spec_Unit_Index := Source_Index (Get_Cunit_Unit_Number (Unit_Spec));
         Spec_File_Name := Full_File_Name (Spec_Unit_Index);
         Get_Name_String (Spec_File_Name);
         Spec_File_Name_Str := new String (1 .. Name_Len + 1);
         Spec_File_Name_Str (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
         Spec_File_Name_Str (Name_Len + 1) := Character'Val (0);
         Spec_Filename := Spec_File_Name_Str (1)'Unrestricted_Access;
         Spec_Context_List :=
           Spec_Label.Label_Name.all (1)'Unrestricted_Access;
      end if;

      if Present (Unit_Body) then
         Append (Body_Label, Character'Val (0));
         Body_Unit_Index := Source_Index (Get_Cunit_Unit_Number (Unit_Body));
         Body_File_Name := Full_File_Name (Body_Unit_Index);
         Get_Name_String (Body_File_Name);
         Body_File_Name_Str := new String (1 .. Name_Len + 1);
         Body_File_Name_Str (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
         Body_File_Name_Str (Name_Len + 1) := Character'Val (0);
         Body_Filename := Body_File_Name_Str (1)'Unrestricted_Access;
         Body_Context_List :=
           Body_Label.Label_Name.all (1)'Unrestricted_Access;
      end if;

   end Save_Unitname_And_Use_List;

   -----------------------
   -- Strip_BNPE_Suffix --
   -----------------------

   procedure Strip_BNPE_Suffix (Suffix_Found : in out Boolean) is
   begin
      for J in reverse 2 .. Name_Len loop
         if Name_Buffer (J) = 'X' then
            Name_Len := J - 1;
            Suffix_Found := True;
            exit;
         end if;

         exit when Name_Buffer (J) /= 'b' and then Name_Buffer (J) /= 'n';
      end loop;
   end Strip_BNPE_Suffix;

end Exp_Dbug;
