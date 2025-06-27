/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                             A - A D A I N T                              */
/*                                                                          */
/*                            $Revision: 1.18 $                             */
/*                                                                          */
/*                              C Header File                               */
/*                                                                          */
/*          Copyright (C) 1992-1998 Free Software Foundation, Inc.          */
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

/* Add prototype support.  */
#ifndef PROTO
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define PROTO(ARGS) ARGS
#else
#define PROTO(ARGS) ()
#endif
#endif

#include <dirent.h>

extern void   to_gm_time  \
              PROTO ((time_t *, int *, int *, int *, int *, int *, int *));
extern int    Get_Maximum_File_Name_Length         PROTO ((void));
extern char   Get_Switch_Character                 PROTO ((void));
extern int    Get_Switches_Case_Sensitive          PROTO ((void));
extern int    Get_File_Names_Case_Sensitive        PROTO ((void));
extern char   Get_Default_Identifier_Character_Set PROTO ((void));
extern void   Get_Object_Suffix                    PROTO ((char *));
extern void   Get_Executable_Suffix                PROTO ((char *));
extern int    Try_Lock                             PROTO ((char *, char *));
extern int    open_new                             PROTO ((char *, int));
extern int    open_read                            PROTO ((char *, int));
extern int    open_rw                              PROTO ((char *, int));
extern int    open_create                          PROTO ((char *, int));
extern int    open_append                          PROTO ((char *, int));
extern long   file_length                          PROTO ((int));
extern char*  readdir_gnat                         PROTO ((DIR*, char*));
extern int    readdir_is_thread_safe               PROTO ((void));
extern time_t file_time_name                       PROTO ((char *));
extern time_t file_time_fd                         PROTO ((int));
extern void   get_env_value_ptr \
              PROTO ((char *, int *, char **));
extern int    file_exists                          PROTO ((char *));
extern int    is_regular_file                      PROTO ((char *));
extern int    portable_spawn                       PROTO ((char *[]));
extern int    portable_no_block_spawn              PROTO ((char *[]));
extern int    portable_wait                        PROTO ((int *));
extern char   *locate_exec                         PROTO ((char *, char *));
extern char   *locate_regular_file                 PROTO ((char *, char *));
extern void   maybe_glob_args                      PROTO ((int *, char ***));
extern char   *to_canonical_dir_spec               PROTO ((char *, int));
extern char   *to_canonical_file_spec              PROTO ((char *));
extern char   *to_host_dir_spec                    PROTO ((char *, int));
extern char   *to_host_file_spec                   PROTO ((char *));


/* portable definition of strdup which is not available on all systems */
#define xstrdup(s)  strcpy ((char *) malloc (strlen(s)+1), s)
