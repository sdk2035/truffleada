
/*****************************************************************************/
/*                                                                           */
/*                          GNAT COMPILER COMPONENTS                         */
/*                                                                           */
/*                               j c o n f i g                               */
/*                                                                           */
/*                             $Revision: 1.4 $                              */
/*                                                                           */
/*           Copyright (C) 1998-1999 Ada Core Technologies, Inc.             */
/*                                                                           */
/*  GNAT is free software;  you can  redistribute it  and/or modify it under */
/*  terms of the  GNU General Public License as published  by the Free Soft- */
/*  ware  Foundation;  either version 2,  or (at your option) any later ver- */
/*  sion.  GNAT is distributed in the hope that it will be useful, but WITH- */
/*  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY */
/*  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License */
/*  for  more details.  You should have  received  a copy of the GNU General */
/*  Public License  distributed with GNAT;  see file COPYING.  If not, write */
/*  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, */
/*  MA 02111-1307, USA.                                                      */
/*                                                                           */
/*  JGNAT - The GNAT Ada 95 toolchain for the Java Virtual Machine is        */
/*          maintained by Ada Core Technologies, Inc. - http://www.gnat.com  */
/*                                                                           */
/*****************************************************************************/

/* This is the "config.h" file that needs to be used when compiling the C
   sources that are part of the GNAT library if these are needed by the
   build of JGNAT. */

/* Microsoft Windows */
#if defined (WINNT)

#define EXECUTABLE_SUFFIX ".exe"
#define PATH_SEPARATOR ';'
#define DIR_SEPARATOR  '\\'

#endif
