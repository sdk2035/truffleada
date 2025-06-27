/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                             S - T R A C E B                              */
/*                                                                          */
/*                          C Implementation File                           */
/*                                                                          */
/*                            $Revision: 1.3 $
/*                                                                          */
/*              Copyright (C) 2000 Ada Core Technologies, Inc.              */
/*                                                                          */
/* GNAT is free software;  you can  redistribute it  and/or modify it under */
/* terms of the  GNU General Public License as published  by the Free Soft- */
/* ware  Foundation;  either version 2,  or (at your option) any later ver- */
/* sion.  GNAT is distributed in the hope that it will be useful, but WITH- */
/* OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License */
/* for  more details.  You should have  received  a copy of the GNU General */
/* Public License  distributed with GNAT;  see file COPYING.  If not, write */
/* to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, */
/* MA 02111-1307, USA.                                                      */
/*                                                                          */
/* As a  special  exception,  if you  link  this file  with other  files to */
/* produce an executable,  this file does not by itself cause the resulting */
/* executable to be covered by the GNU General Public License. This except- */
/* ion does not  however invalidate  any other reasons  why the  executable */
/* file might be covered by the  GNU Public License.                        */
/*                                                                          */
/* GNAT was originally developed  by the GNAT team at  New York University. */
/* It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). */
/*                                                                          */
/****************************************************************************/

/* This file contains low level support for stack unwinding using GCC intrinsic
   functions.
   It has been tested on the following configurations:
   HPPA/HP-UX
   PowerPC/AiX
   PowerPC/VxWorks
   Alpha/Tru64
   Alpha/VxWorks
   Sparc/Solaris
   i386/Linux
   i386/Solaris
   i386/NT
   i386/OS2
   i386/LynxOS
*/

#include <sys/types.h>
#include <stddef.h>

#ifndef CURRENT_STACK_FRAME
# define CURRENT_STACK_FRAME  ({ char __csf; &__csf; })
#endif

#if defined (__hppa)
struct layout
{
  void *return_address;
  void *pad[4];
  struct layout *next;
};

#define FRAME_LEVEL 1
#define FRAME_OFFSET -20
#define SKIP_FRAME 1
#define PC_ADJUST -3
#define STOP_FRAME (!current->return_address)

#elif defined (_AIX)
struct layout
{
  struct layout *next;
  void *pad;
  void *return_address;
};

#define FRAME_LEVEL 1
#define FRAME_OFFSET 0
#define SKIP_FRAME 2
#define PC_ADJUST 0
#define STOP_FRAME ((void*)current < top_stack)

#elif defined (_ARCH_PPC) && defined (__vxworks)
struct layout
{
  struct layout *next;
  void *return_address;
};

#define FRAME_LEVEL 1
#define FRAME_OFFSET 0
#define SKIP_FRAME 2
#define PC_ADJUST 0
#define STOP_FRAME (!current->return_address)

#elif defined (__alpha) || defined (__alpha_vxworks)
struct layout
{
  void *return_address;
  struct layout *next;
};

#define FRAME_LEVEL 0
#define FRAME_OFFSET 0
#define SKIP_FRAME 2
#define PC_ADJUST -8
#define STOP_FRAME (current < 0xffff)

#elif defined (sun) && defined (sparc)
struct layout
{
  struct layout *next;
  void *return_address;
};

#define FRAME_LEVEL 1
#define FRAME_OFFSET (14*4)
#define SKIP_FRAME 1
#define PC_ADJUST 8
#define STOP_FRAME \
  (!current->return_address || !current->next || (void*)current < top_stack)

#elif defined (i386)
struct layout
{
  struct layout *next;
  void *return_address;
};

#define FRAME_LEVEL 0
#define FRAME_OFFSET 0
#define SKIP_FRAME 1
#define PC_ADJUST -2
#define STOP_FRAME \
  (!current->return_address || !current->next || (void*)current < top_stack)

#endif

#if !defined (FRAME_LEVEL)
int
__gnat_backtrace (array, size, exclude_min, exclude_max)
     void **array;
     int size;
     void *exclude_min;
     void *exclude_max;
{
  return 0;
}

#else
int
__gnat_backtrace (array, size, exclude_min, exclude_max)
     void **array;
     int size;
     void *exclude_min;
     void *exclude_max;
{
  struct layout *current;
  void *top_frame;
  void *top_stack;
  void *ret;
  int cnt = 0;

  top_frame = __builtin_frame_address (FRAME_LEVEL);
  top_stack = CURRENT_STACK_FRAME;
  current = (struct layout *) ((size_t)top_frame + FRAME_OFFSET);

  /* We skip the call to this function, it makes no sense to record it.  */
  while (cnt < SKIP_FRAME)
    {
      current = (struct layout *) ((size_t)current->next + FRAME_OFFSET);
      cnt++;
    }

  cnt = 0;
  while (cnt < size)
    {
      if (STOP_FRAME)
        break;

      if (current->return_address < exclude_min ||
	  current->return_address > exclude_max)
        array[cnt++] = current->return_address + PC_ADJUST;

      current = (struct layout *) ((size_t)current->next + FRAME_OFFSET);
    }
  return cnt;
}
#endif
