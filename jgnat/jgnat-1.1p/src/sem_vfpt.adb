------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ V F P T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.9 $
--                                                                          --
--          Copyright (C) 1997-1999, Free Software Foundation, Inc.         --
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

with CStand;   use CStand;
with Einfo;    use Einfo;
with Opt;      use Opt;
with Stand;    use Stand;
with Targparm; use Targparm;
with Ttypef;   use Ttypef;
with Uintp;    use Uintp;

pragma Elaborate_All (Uintp);

package body Sem_VFpt is

   T_Digits : constant Uint := UI_From_Int (IEEEL_Digits);
   --  Digits for IEEE formats

   -----------------
   -- Set_D_Float --
   -----------------

   procedure Set_D_Float (E : Entity_Id) is
   begin
      Init_Esize        (Base_Type (E), 64);
      Init_Alignment    (Base_Type (E));
      Init_Digits_Value (Base_Type (E), VAXDF_Digits);
      Set_Vax_Float     (Base_Type (E), True);
      Set_Float_Bounds  (Base_Type (E));

      Init_Esize        (E, 64);
      Init_Alignment    (E);
      Init_Digits_Value (E, VAXDF_Digits);
      Set_Scalar_Range  (E, Scalar_Range (Base_Type (E)));
   end Set_D_Float;

   -----------------
   -- Set_F_Float --
   -----------------

   procedure Set_F_Float (E : Entity_Id) is
   begin
      Init_Esize        (Base_Type (E), 32);
      Init_Alignment    (Base_Type (E));
      Init_Digits_Value (Base_Type (E), VAXFF_Digits);
      Set_Vax_Float     (Base_Type (E), True);
      Set_Float_Bounds  (Base_Type (E));

      Init_Esize        (E, 32);
      Init_Alignment    (E);
      Init_Digits_Value (E, VAXFF_Digits);
      Set_Scalar_Range  (E, Scalar_Range (Base_Type (E)));
   end Set_F_Float;

   -----------------
   -- Set_G_Float --
   -----------------

   procedure Set_G_Float (E : Entity_Id) is
   begin
      Init_Esize        (Base_Type (E), 64);
      Init_Alignment    (Base_Type (E));
      Init_Digits_Value (Base_Type (E), VAXGF_Digits);
      Set_Vax_Float     (Base_Type (E), True);
      Set_Float_Bounds  (Base_Type (E));

      Init_Esize        (E, 64);
      Init_Alignment    (E);
      Init_Digits_Value (E, VAXGF_Digits);
      Set_Scalar_Range  (E, Scalar_Range (Base_Type (E)));
   end Set_G_Float;

   -------------------
   -- Set_IEEE_Long --
   -------------------

   procedure Set_IEEE_Long (E : Entity_Id) is
   begin
      Init_Esize        (Base_Type (E), 64);
      Init_Alignment    (Base_Type (E));
      Init_Digits_Value (Base_Type (E), IEEEL_Digits);
      Set_Vax_Float     (Base_Type (E), False);
      Set_Float_Bounds  (Base_Type (E));

      Init_Esize        (E, 64);
      Init_Alignment    (E);
      Init_Digits_Value (E, IEEEL_Digits);
      Set_Scalar_Range  (E, Scalar_Range (Base_Type (E)));
   end Set_IEEE_Long;

   --------------------
   -- Set_IEEE_Short --
   --------------------

   procedure Set_IEEE_Short (E : Entity_Id) is
   begin
      Init_Esize        (Base_Type (E), 32);
      Init_Alignment    (Base_Type (E));
      Init_Digits_Value (Base_Type (E), IEEES_Digits);
      Set_Vax_Float     (Base_Type (E), False);
      Set_Float_Bounds  (Base_Type (E));

      Init_Esize        (E, 32);
      Init_Alignment    (E);
      Init_Digits_Value (E, IEEES_Digits);
      Set_Scalar_Range  (E, Scalar_Range (Base_Type (E)));
   end Set_IEEE_Short;

   ------------------------------
   -- Set_Standard_Fpt_Formats --
   ------------------------------

   procedure Set_Standard_Fpt_Formats is
   begin
      --  IEEE case

      if Opt.Float_Format = 'I' then
         Set_IEEE_Short (Standard_Float);
         Set_IEEE_Long  (Standard_Long_Float);
         Set_IEEE_Long  (Standard_Long_Long_Float);

      --  Vax float case

      else
         Set_F_Float (Standard_Float);

         if Opt.Float_Format_Long = 'D' then
            Set_D_Float (Standard_Long_Float);
         else
            Set_G_Float (Standard_Long_Float);
         end if;

         --  Note: Long_Long_Float gets set only in the real VMS case,
         --  because this gives better results for testing out the use
         --  of VAX float on non-VMS environments with the -gnatdm switch.

         if OpenVMS_On_Target then
            Set_G_Float (Standard_Long_Long_Float);
         end if;
      end if;
   end Set_Standard_Fpt_Formats;

end Sem_VFpt;
