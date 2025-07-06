------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                        S Y S T E M . T A S K I N G                       --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.36 $
--                                                                          --
--             Copyright (C) 1991-2000 Florida State University             --
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
--  used for Self

with Unchecked_Deallocation;
--  To recover from failure of ATCB initialization.

with System.Storage_Elements;
--  Needed for initializing Stack_Info.Size

with System.Parameters;
--  Used for Adjust_Storage_Size

package body System.Tasking is

   package STPO renames System.Task_Primitives.Operations;

   procedure Free is new
     Unchecked_Deallocation (Ada_Task_Control_Block, Task_ID);

   ----------
   -- Self --
   ----------

   function Self return Task_ID renames STPO.Self;

   ---------------------
   -- Initialize_ATCB --
   ---------------------

   --  Call this only with abort deferred and holding All_Tasks_L.

   procedure Initialize_ATCB
     (Self_ID          : Task_ID;
      Task_Entry_Point : Task_Procedure_Access;
      Task_Arg         : System.Address;
      Parent           : Task_ID;
      Elaborated       : Access_Boolean;
      Base_Priority    : System.Any_Priority;
      Task_Info        : System.Task_Info.Task_Info_Type;
      Stack_Size       : System.Parameters.Size_Type;
      Master_of_Task   : Master_Level;
      T                : in out Task_ID;
      Success          : out Boolean) is
   begin
      --  Initialize T.LL

      STPO.Initialize_TCB (T, Success);

      if not Success then
         Free (T);
         return;
      end if;

      T.Common.Elaborated := Elaborated;
      T.Common.Parent := Parent;
      T.Common.Task_Entry_Point := Task_Entry_Point;
      T.Common.Task_Arg := Task_Arg;
      T.Common.Base_Priority := Base_Priority;
      T.Common.Activator := Self_ID;
      T.Task_Info := Task_Info;

      T.Master_of_Task := Master_of_Task;
      T.Master_Within := T.Master_of_Task + 1;

      if T.Common.Parent = null then
         --  For the environment task, the adjusted stack size is
         --  meaningless. For example, an unspecified Stack_Size means
         --  that the stack size is determined by the environment, or
         --  can grow dynamically. The Stack_Checking algorithm
         --  therefore needs to use the requested size, or 0 in
         --  case of an unknown size.

         T.Common.Compiler_Data.Pri_Stack_Info.Size :=
            Storage_Elements.Storage_Offset (Stack_Size);

      else
         T.Common.Compiler_Data.Pri_Stack_Info.Size :=
           Storage_Elements.Storage_Offset
             (Parameters.Adjust_Storage_Size (Stack_Size));
      end if;

      for J in 1 .. T.Entry_Num loop
         T.Entry_Queues (J).Head := null;
         T.Entry_Queues (J).Tail := null;
      end loop;

      for L in T.Entry_Calls'Range loop
         T.Entry_Calls (L).Self := T;
         T.Entry_Calls (L).Level := L;
      end loop;

      --  Link the task into the list of all tasks.

      T.Common.All_Tasks_Link := All_Tasks_List;
      All_Tasks_List := T;
   end Initialize_ATCB;

   --------------
   -- Init_RTS --
   --------------

   Main_Task_Control_Block : aliased Ada_Task_Control_Block (0);
   --  We declare a global variable to avoid allocating dynamic memory that
   --  will never be freed, so that gnatmem output looks clean.

   Main_Task_Image : aliased String := "main_task";
   --  ditto

   Main_Priority : Priority;
   pragma Import (C, Main_Priority, "__gl_main_priority");

begin
   ----------------------------
   -- Tasking Initialization --
   ----------------------------

   --  This block constitutes the first part of the initialization of the
   --  GNARL. This includes creating data structures to make the initial thread
   --  into the environment task. The last part of the initialization is done
   --  in System.Tasking[.Restricted].Initialization
   --  All the initializations used to be in Tasking.Initialization, but this
   --  is no longer possible with the run time simplification (including
   --  optimized PO and the restricted run time) since one cannot rely on
   --  System.Tasking.Initialization being present, as was done before.

   declare
      T             : Task_ID;
      Success       : Boolean;
      Base_Priority : Any_Priority;

   begin
      if Main_Priority = Unspecified_Priority then
         Base_Priority := Default_Priority;
      else
         Base_Priority := Main_Priority;
      end if;

      Success := True;
      T := Main_Task_Control_Block'Access;
      Initialize_ATCB (T, null, Null_Address, Null_Task, null, Base_Priority,
        Task_Info.Unspecified_Task_Info, 0, Environment_Task_Level, T,
        Success);
      pragma Assert (Success);

      --  As a special exception, the environment task doesn't have an
      --  Activator.

      T.Common.Activator := null;

      STPO.Initialize (T);
      STPO.Set_Priority (T, T.Common.Base_Priority);

      T.Common.State := Runnable;

      T.Awake_Count := 1;
      T.Alive_Count := 1;

      T.Master_Within := Library_Task_Level;
      --  Normally, a task starts out with internal master nesting level
      --  one larger than external master nesting level. It is incremented
      --  to one by Enter_Master, which is called in the task body only if
      --  the compiler thinks the task may have dependent tasks. There is no
      --  corresponding call to Enter_Master for the environment task, so we
      --  would need to increment it to 2 here.  Instead, we set it to 3.
      --  By doing this we reserve the level 2 for server tasks of the runtime
      --  system. The environment task does not need to wait for these server

      T.Common.Task_Image := Main_Task_Image'Unrestricted_Access;
   end;
end System.Tasking;
