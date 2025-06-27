/****************************************************************************/
/*                                                                          */
/*                         GNAT COMPILER COMPONENTS                         */
/*                                                                          */
/*                             A - A D A I N T                              */
/*                                                                          */
/*                            $Revision: 1.136 $
/*                                                                          */
/*                          C Implementation File                           */
/*                                                                          */
/*          Copyright (C) 1992-2000, Free Software Foundation, Inc.         */
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

/*  This file contains those routines named by Import pragmas in packages   */
/*  in the GNAT hierarchy (especially GNAT.OS_Lib) and in package Osint.    */
/*  Many of the subprograms in OS_Lib import standard library calls         */
/*  directly. This file contains all other routines.                        */

#ifdef __vxworks
/* No need to redefine exit here */
#ifdef exit
#undef exit
#endif
#include "vxWorks.h"

#if defined (__mips_vxworks)
#include "cacheLib.h"
#endif /* __mips_vxworks */

#endif /* VxWorks */

#include "config.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>

#if defined (__EMX__) || defined (MSDOS) || defined (_WIN32)
#include <process.h>
#include <string.h>
#endif

#if defined (_WIN32)
#include <dir.h>
#endif

#include "a-adaint.h"

/* Define symbols O_BINARY and O_TEXT as harmless zeroes if they are not
   defined in the current system. On DOS-like systems these flags control
   whether the file is opened/created in text-translation mode (CR/LF in
   external file mapped to LF in internal file), but in Unix-like systems,
   no text translation is required, so these flags have no effect.
*/


#if defined (__EMX__)
#include <os2.h>
#endif

#if defined (MSDOS)
#include <dos.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef O_TEXT
#define O_TEXT 0
#endif

extern char *getenv ();

#ifndef EXECUTABLE_SUFFIX
#define EXECUTABLE_SUFFIX ""
#endif

#ifndef OBJECT_SUFFIX
#define OBJECT_SUFFIX ".o"
#endif

#ifndef PATH_SEPARATOR
#define PATH_SEPARATOR ':'
#endif

#ifndef DIR_SEPARATOR
#define DIR_SEPARATOR '/'
#endif

/* Export DIR_SEPARATOR as Dir_Separator, used in g-locfil.adb */

extern char Dir_Separator;
char Dir_Separator = DIR_SEPARATOR;

/* The GNAT_LIBRARY_TEMPLATE contains a list of expressions that define
   the base filenames that libraries specified with -lsomelib options
   may have. This is used by GNATMAKE to check whether an executable
   is up-to-date or not. The syntax is

     library_template ::= { pattern ; } pattern NUL
     pattern          ::= [ prefix ] * [ postfix ]

   These should only specify names of static libraries as it makes
   no sense to determine at link time if dynamic-link libraries are
   up to date or not. Any libraries that are not found are supposed
   to be up-to-date:

     * if they are needed but not present, the link
       will fail,

     * otherwise they are libraries in the system paths and so
       they are considered part of the system and not checked
       for that reason.

   ??? This should be part of a GNAT host-specific compiler
       file instead of being included in all user applications
       as well. This is only a temporary work-around for 3.11b.
*/
#ifndef GNAT_LIBRARY_TEMPLATE
#if defined(__EMX__)
   #define GNAT_LIBRARY_TEMPLATE "*.a"
#elif defined(VMS)
   #define GNAT_LIBRARY_TEMPLATE "*.olb"
#else
   #define GNAT_LIBRARY_TEMPLATE "lib*.a"
#endif
#endif
char *_gnat_library_template = GNAT_LIBRARY_TEMPLATE;
/* The following macro HAVE_READDIR_R should be defined if the
   system provides the routine readdir_r */
#undef HAVE_READDIR_R


void
to_gm_time (p_time, p_year, p_month, p_day, p_hours, p_mins, p_secs)
     time_t *p_time;
     int *p_year, *p_month, *p_day, *p_hours, *p_mins, *p_secs;
{
  struct tm *res;
  time_t time = *p_time;

#ifdef _WIN32
  /* On Windows systems, the time is sometimes rounded up to the nearest
     even second, so if the number of seconds is odd, increment it.  */
  if (time & 1)
    time++;
#endif

  res = gmtime (&time);

  if (res) {
    *p_year = res->tm_year;
    *p_month = res->tm_mon;
    *p_day = res->tm_mday;
    *p_hours = res->tm_hour;
    *p_mins = res->tm_min;
    *p_secs = res->tm_sec;
  } else {
    *p_year = *p_month = *p_day = *p_hours = *p_mins = *p_secs = 0;
  }
}

/* Try to lock a file, return 1 if success */

#if defined (__vxworks) || defined (MSDOS) || defined (_WIN32)

/* Version that does not use link() */

int
Try_Lock (dir, file)
     char *dir;
     char *file;
{
  char full_path [256];
  int fd;

  sprintf (full_path, "%s%c%s", dir, DIR_SEPARATOR, file);
  fd = open (full_path, O_CREAT | O_EXCL, 0600);
  if (fd < 0) {
    return 0;
  }
  close (fd);
  return 1;
}

#elif defined (__EMX__) || defined (VMS)

/* More cases that do not use link() */
/* identical code, to solve too long line problem ??? */

int
Try_Lock (dir, file)
     char *dir;
     char *file;
{
  char full_path [256];
  int fd;

  sprintf (full_path, "%s%c%s", dir, DIR_SEPARATOR, file);
  fd = open (full_path, O_CREAT | O_EXCL, 0600);
  if (fd < 0) {
    return 0;
  }
  close (fd);
  return 1;
}


#else

/* Version using link(), more secure over NFS */

int
Try_Lock (dir, file)
     char *dir;
     char *file;
{
  char full_path [256];
  char temp_file [256];
  struct stat stat_result;
  int fd;

  sprintf (full_path, "%s%c%s", dir, DIR_SEPARATOR, file);
  sprintf (temp_file, "%s-%d-%d", dir, getpid(), getppid ());

  /* Create the temporary file and write the process number */
  fd = open (temp_file, O_CREAT | O_WRONLY, 0600);
  if (fd < 0) {
    return 0;
  }
  close (fd);

  /* Link it with the new file */
  link (temp_file, full_path);

  /* Count the references on the old one. If we have a count of two, then
   * the link did succeed. Remove the temporary file before returning */
  stat (temp_file, &stat_result);
  unlink (temp_file);
  return stat_result.st_nlink == 2;
}

#endif

/* Return the maximum file name length.  */

int
Get_Maximum_File_Name_Length ()
{
#if defined(MSDOS)
  return 8;
#elif defined (VMS)
  if (getenv ("GNAT$EXTENDED_FILE_SPECIFICATIONS"))
    return -1;
  else
    return 39;
#else
  return -1;
#endif
}

/* Return the default switch character.  */

char
Get_Switch_Character ()
{
  /* Under MSDOS, the switch character is not normally a hyphen, but this is
     the convention DJGPP uses. Similarly under OS2, the switch character is
     not normally a hypen, but this is the convention EMX uses.
   */
  return '-';
}

/* Return nonzero if file names are case sensitive.  */

int
Get_File_Names_Case_Sensitive ()
{
#if defined (__EMX__) || defined (MSDOS) || defined (VMS) || defined(WINNT)
  return 0;
#else
  return 1;
#endif
}

char
Get_Default_Identifier_Character_Set ()
{
#if defined (__EMX__) || defined (MSDOS)
  return 'p';
#else
  return '1';
#endif
}

char
Get_Dirsep_Char ()
{
  return DIR_SEPARATOR;
}

/* Return the path separator.   */

char
Get_Pathsep_Char ()
{
  return PATH_SEPARATOR;
}

/* Return the current working directory */
void Get_Current_Dir (char *dir, int *length)
{
   getcwd (dir, *length);

   *length = strlen (dir);

#ifndef VMS
   dir [*length] = DIR_SEPARATOR;
   ++(*length);
   dir [*length] = '\0';
#endif
}

/* Return the suffix for object files */
void
get_object_suffix_ptr (len, value)
     int *len;
     char **value;
{
  *value = OBJECT_SUFFIX;
  if (!*value)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

/* Return the suffix for executable files */
void
get_executable_suffix_ptr (len, value)
     int *len;
     char **value;
{
  *value = EXECUTABLE_SUFFIX;
  if (!*value)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

/* Return the suffix for debuggable files. Usually this is the same as the
   executable extension. */
void
get_debuggable_suffix_ptr (len, value)
     int *len;
     char **value;
{
#ifndef MSDOS
  *value = EXECUTABLE_SUFFIX;
#else
  /* On DOS, the extensionless COFF file is what gdb likes. */
  *value = "";
#endif
  if (!*value)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

int
open_read (path, fmode)
     char *path;
     int fmode;
{
  int fd;
  int o_fmode = O_BINARY;

  if (fmode)
    o_fmode = O_TEXT;

#if defined(VMS)
  /* Optional arguments mbc,deq,fop increase read performance */
  fd = open (path, O_RDONLY | o_fmode, 0444,
             "mbc=16", "deq=64", "fop=tef");
#elif defined(__vxworks)
  fd = open (path, O_RDONLY | o_fmode, 0444);
#else
  fd = open (path, O_RDONLY | o_fmode);
#endif

  return fd < 0 ? -1 : fd;
}


#if defined (__EMX__)
#define PERM (S_IREAD | S_IWRITE)
#else
#define PERM (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)
#endif

int
open_rw (path, fmode)
     char *path;
     int  fmode;
{
  int fd;
  int o_fmode = O_BINARY;

  if (fmode)
    o_fmode = O_TEXT;

#if defined(VMS)
  fd = open (path, O_RDWR | o_fmode, PERM,
             "mbc=16", "deq=64", "fop=tef");
#else
  fd = open (path, O_RDWR | o_fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

int
open_create (path, fmode)
     char *path;
     int  fmode;
{
  int fd;
  int o_fmode = O_BINARY;

  if (fmode)
    o_fmode = O_TEXT;

#if defined(VMS)
  fd = open (path, O_WRONLY | O_CREAT | O_TRUNC | o_fmode, PERM,
             "mbc=16", "deq=64", "fop=tef");
#else
  fd = open (path, O_WRONLY | O_CREAT | O_TRUNC | o_fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

int
open_append (path, fmode)
     char *path;
     int  fmode;
{
  int fd;
  int o_fmode = O_BINARY;

  if (fmode)
    o_fmode = O_TEXT;

#if defined(VMS)
  fd = open (path, O_WRONLY | O_CREAT | O_APPEND | o_fmode, PERM,
             "mbc=16", "deq=64", "fop=tef");
#else
  fd = open (path, O_WRONLY | O_CREAT | O_APPEND | o_fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

/*  Open a new file.  Return error (-1) if the file already exists. */

int
open_new (path, fmode)
     char *path;
     int fmode;
{
  int fd;
  int o_fmode = O_BINARY;

  if (fmode)
    o_fmode = O_TEXT;

#if defined(VMS)
  fd = open (path, O_WRONLY | O_CREAT | O_EXCL | o_fmode, PERM,
             "mbc=16", "deq=64", "fop=tef");
#else
  fd = open (path, O_WRONLY | O_CREAT | O_EXCL | o_fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

/* Open a new temp file.  Return error (-1) if the file already exists.
   Special options for VMS allow the file to be shared between parent
   and child processes, however they really slow down output.
   Used in gnatchop. */

int
open_new_temp (path, fmode)
     char *path;
     int fmode;
{
  int fd;
  int o_fmode = O_BINARY;

  if (fmode)
    o_fmode = O_TEXT;

#if defined(VMS)
  fd = open (path, O_WRONLY | O_CREAT | O_EXCL | o_fmode, PERM,
             "rfm=stmlf", "ctx=rec", "rat=none", "shr=del,get,put,upd",
             "mbc=16", "deq=64", "fop=tef");
#else
  fd = open (path, O_WRONLY | O_CREAT | O_EXCL | o_fmode, PERM);
#endif

  return fd < 0 ? -1 : fd;
}

int
__gnat_mkdir (char *dir_name)
{
#if defined (_WIN32)
  return mkdir (dir_name);
#elif defined (__vxworks)
  return mkdir (dir_name);
#else
  return mkdir (dir_name, S_IRWXU | S_IRWXG | S_IRWXO);
#endif
}

/* Return the number of bytes in the specified file. */

long
file_length (fd)
     int fd;
{
  int ret;
  struct stat statbuf;

  ret = fstat (fd, &statbuf);
  if (ret || !S_ISREG (statbuf.st_mode))
    return 0;

  return (statbuf.st_size);
}

/* create a temporary filename and put it in string pointed to 
   by tmp_filename */

void
__gnat_tmp_name (char *tmp_filename)
{
#ifdef __MINGW32__
  {
    char *pname;

    /* tempnam try to create a temporary file in directory pointed to by TMP
       environment variable, in c:\temp if TMP is not set, and in directory
       specified by P_tmpdir in stdio.h if c:\temp does not exist. The
       filename will be created with the prefix "gnat-" */

    pname = (char *)tempnam ("c:\\temp", "gnat-");
    strcpy (tmp_filename, pname);
    free (pname);
  }
#else
  tmpnam (tmp_filename);
#endif
}

/* Read the next entry in a directory
   The returned string points somewhere in the buffer */

char *
readdir_gnat (dirp, buffer)
     DIR *dirp;
     char* buffer;
{
  /* if possible, try to use the thread-safe version */
#ifdef HAVE_READDIR_R
  if (readdir_r (dirp, buffer) != NULL)
    return (((struct dirent*)buffer)->d_name);
  else
    return NULL;

#else

  struct dirent* dirent = readdir (dirp);
  if (dirent != NULL)
    {
      strcpy (buffer, dirent->d_name);
      return buffer;
    }
  else
    return NULL;

#endif
}

/* returns 1 if readdir is thread safe, 0 otherwise */

int
readdir_is_thread_safe ()
{
#ifdef HAVE_READDIR_R
  return 1;
#else
  return 0;
#endif
}


/* Return a GNAT time stamp given a file name.  */

time_t
file_time_name (name)
     char *name;
{
  struct stat statbuf;

#if defined (__EMX__) || defined (MSDOS)
  int fd = open (name, O_RDONLY | O_BINARY);
  time_t ret = file_time_fd (fd);
  close (fd);
  return ret;

#else

  int ret = stat (name, &statbuf);
#ifdef VMS
  /* VMS has file versioning */
  return statbuf.st_ctime;
#else
  return statbuf.st_mtime;
#endif
#endif
}

/* Return a GNAT time stamp given a file descriptor.  */

time_t
file_time_fd (fd)
     int fd;
{
  /* The following workaround code is due to the fact that under EMX and DJGPP
     fstat attempts to convert time values to GMT rather than keep the actual
     OS timestamp of the file. By using the OS2/DOS functions directly the GNAT
     timestamp are independent of this behavior, which is desired to facilitate
     the distribution of GNAT compiled libraries. */

#if defined (__EMX__) || defined (MSDOS)
#ifdef __EMX__

  FILESTATUS fs;
  int ret = DosQueryFileInfo (fd, 1, (unsigned char *) &fs,
                                sizeof (FILESTATUS));

  unsigned file_year  = fs.fdateLastWrite.year;
  unsigned file_month = fs.fdateLastWrite.month;
  unsigned file_day   = fs.fdateLastWrite.day;
  unsigned file_hour  = fs.ftimeLastWrite.hours;
  unsigned file_min   = fs.ftimeLastWrite.minutes;
  unsigned file_tsec  = fs.ftimeLastWrite.twosecs;

#else
  struct ftime fs;
  int ret = getftime (fd, &fs);

  unsigned file_year  = fs.ft_year;
  unsigned file_month = fs.ft_month;
  unsigned file_day   = fs.ft_day;
  unsigned file_hour  = fs.ft_hour;
  unsigned file_min   = fs.ft_min;
  unsigned file_tsec  = fs.ft_tsec;
#endif

  /* Calculate the seconds since epoch from the time components. First count
     the whole days passed.  The value for years returned by the DOS and OS2
     functions count years from 1980, so to compensate for the UNIX epoch which
     begins in 1970 start with 10 years worth of days and add days for each
     four year period since then. */

  time_t tot_secs;
  int cum_days [12] = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
  int days_passed = 3652 + (file_year / 4) * 1461;
  int years_since_leap = file_year % 4;
  if      (years_since_leap == 1) days_passed += 366;
  else if (years_since_leap == 2) days_passed += 731;
  else if (years_since_leap == 3) days_passed += 1096;
  if (file_year > 20) days_passed -= 1;
  days_passed += cum_days [file_month - 1];
  if (years_since_leap == 0 && file_year != 20
      && file_month > 2) days_passed++;
  days_passed += file_day - 1;

  /* OK - have whole days.  Multiply -- then add in other parts. */
  tot_secs  = days_passed               * 86400;
  tot_secs += file_hour   * 3600;
  tot_secs += file_min * 60;
  tot_secs += file_tsec * 2;

  return tot_secs;

#else
  struct stat statbuf;
  int ret = fstat (fd, &statbuf);
#ifdef VMS
  /* VMS has file versioning */
  return statbuf.st_ctime;
#else
  return statbuf.st_mtime;
#endif
#endif
}

void
get_env_value_ptr (name, len, value)
     char *name;
     int *len;
     char **value;
{
  *value = getenv (name);
  if (!*value)
    *len = 0;
  else
    *len = strlen (*value);

  return;
}

#ifdef _WIN32

#include "stdarg.h"
#include <windows.h>

#endif

/* Get the list of installed standard libraries from the
   HKEY_LOCAL_MACHINE\SOFTWARE\Ada Core Technologies\GNAT\Standard Libraries
   key.  */

char *
Get_Libraries_From_Registry ()
{
  char *result = "";

#if defined (_WIN32) && ! defined (__vxworks)

  HKEY reg_key;
  DWORD name_size, value_size;
  char name[256];
  char value[256];
  DWORD type;
  DWORD index;
  LONG res;

  /* First open the key.  */
  res = RegOpenKeyExA (HKEY_LOCAL_MACHINE, "SOFTWARE", 0, KEY_READ, &reg_key);

  if (res == ERROR_SUCCESS)
    res = RegOpenKeyExA (reg_key, "Ada Core Technologies", 0,
                         KEY_READ, &reg_key);

  if (res == ERROR_SUCCESS)
    res = RegOpenKeyExA (reg_key, "GNAT", 0, KEY_READ, &reg_key);

  if (res == ERROR_SUCCESS)
    res = RegOpenKeyExA (reg_key, "Standard Libraries", 0, KEY_READ, &reg_key);

  /* If the key exists, read out all the values in it and concatenate them
     into a path.  */
  for (index = 0; res == ERROR_SUCCESS; index++)
    {
      value_size = name_size = 256;
      res = RegEnumValue (reg_key, index, name, &name_size, 0,
                          &type, value, &value_size);

      if (res == ERROR_SUCCESS && type == REG_SZ)
        {
          char *old_result = result;

          result = (char *) malloc (strlen (old_result) + value_size + 2);
          strcpy (result, old_result);
          strcat (result, value);
          strcat (result, ";");
        }
    }

  /* Remove the trailing ";".  */
  if (result[0] != 0)
    result[strlen (result) - 1] = 0;

#endif
  return result;
}

int
__gnat_file_exists (name)
     char *name;
{
  struct stat statbuf;

  return !stat (name, &statbuf);
}

int
is_regular_file (name)
     char *name;
{
  int ret;
  struct stat statbuf;

  ret = stat (name, &statbuf);
  return (!ret && S_ISREG (statbuf.st_mode));
}

int
is_directory (name)
     char *name;
{
  int ret;
  struct stat statbuf;

  ret = stat (name, &statbuf);
  return (!ret && S_ISDIR (statbuf.st_mode));
}

int
is_writable_file (name)
     char *name;
{
  int ret;
  int mode;
  struct stat statbuf;

  ret = stat (name, &statbuf);
  mode = statbuf.st_mode & S_IWUSR;
  return (!ret && mode);
}

#ifdef VMS
/* Defined in VMS header files */
#define fork() (decc$$alloc_vfork_blocks() >= 0 ? \
               LIB$GET_CURRENT_INVO_CONTEXT (decc$$get_vfork_jmpbuf()) : -1)
#endif

#if defined (sun) && defined (__SVR4)
/* Using fork() on Solaris will duplicate all the threads. fork1() which   */
/* duplicates only the active thread must be used instead, or spawning     */
/* subprocess from a program with tasking will lead into numerous problems */
#define fork() fork1()
#endif

int
portable_spawn (args)
    char *args[];
{
  int status;
  int finished;
  int pid;

#if defined (__EMX__) || defined (MSDOS) || defined (_WIN32)
  if (spawnvp (P_WAIT, args [0], args) != 0)
    return (4);
#elif defined(__vxworks)  /* Mods for VxWorks */
  pid = sp(args[0], args);  /* Spawn process and save pid */
  if (pid == -1)
    return (4);
  while (taskIdVerify(pid) >= 0) {};  /* Wait until spawned task is complete
                                         then continue                       */
  return 0;
#else
  pid = fork ();
  if (pid == -1)
    return (4);
  if (pid == 0) {
    /* The child */
    if (execv (args [0], args) != 0)
      _exit (1);
  }

  /* The parent */
#ifdef VMS
  /* Wait doesn't do the right thing on VMS */
  finished = waitpid (-1, &status, 0);
#else
  finished = wait (&status);
#endif
  if (finished != pid || status & 0xffff)
    return 4;
#endif
  return 0;
}

/* WIN32 code to implement a wait call that wait for any child process */
#ifdef _WIN32

#include <errno.h> /* for ECHILD */

/* synchronization code to be thread safe */

CRITICAL_SECTION plist_cs;

void
__plist_init (void)
{
  InitializeCriticalSection (&plist_cs);
}

void
plist_enter (void)
{
  EnterCriticalSection (&plist_cs);
}

void
plist_leave (void)
{
  LeaveCriticalSection (&plist_cs);
}

typedef struct _process_list {
  HANDLE h;
  struct _process_list *next;
} Process_List;

Process_List *PLIST = NULL;

int plist_length = 0;

void
add_handle (HANDLE h)
{
  Process_List *pl;

  pl = (Process_List *) malloc (sizeof (Process_List));

  /* -------------------- critical section -------------------- */
  plist_enter();
  pl->h = h;
  pl->next = PLIST;
  PLIST = pl;
  ++plist_length;
  plist_leave();
  /* -------------------- critical section -------------------- */
}

void remove_handle (HANDLE h)
{
  Process_List *pl, *prev;

  /* -------------------- critical section -------------------- */
  plist_enter();
  pl = PLIST;
  while (pl)
    {
      if (pl->h == h)
        {
          if (pl == PLIST)
            {
              PLIST = pl->next;
            }
          else
            {
              prev->next = pl->next;
            }
          free (pl);
          break;
        }
      else
        {
          prev = pl;
          pl = pl->next;
        }
    }
  --plist_length;
  plist_leave();
  /* -------------------- critical section -------------------- */
}

int
win32_no_block_spawn (char *command, char *args[])
{
  BOOL result;
  STARTUPINFO SI;
  PROCESS_INFORMATION PI;
  SECURITY_ATTRIBUTES SA;

  char full_command [2000];
  int k;

  /* startup info */
  SI.cb          = sizeof (STARTUPINFO);
  SI.lpReserved  = NULL;
  SI.lpReserved2 = NULL;
  SI.lpDesktop   = NULL;
  SI.cbReserved2 = 0;
  SI.lpTitle     = NULL;
  SI.dwFlags     = 0;
  SI.wShowWindow = SW_HIDE;

  /* security attributes */
  SA.nLength = sizeof (SECURITY_ATTRIBUTES);
  SA.bInheritHandle = TRUE;
  SA.lpSecurityDescriptor = NULL;

  /* prepare the command string */
  strcpy (full_command, command);
  strcat (full_command, " ");

  k = 1;
  while (args[k]) {
    strcat (full_command, args[k]);
    strcat (full_command, " ");
    k++;
  }

  result = CreateProcess (NULL,
                          (char *)full_command,
                          &SA,
                          NULL,
                          TRUE,
                          NORMAL_PRIORITY_CLASS,
                          NULL,
                          NULL,
                          &SI,
                          &PI);
  if (result == TRUE) {
    add_handle (PI.hProcess);
    CloseHandle (PI.hThread);
    return (int)PI.hProcess;
  } else
    return -1;
}

int
win32_wait (int *status)
{
  DWORD exitcode;
  HANDLE *hl;
  HANDLE h;
  DWORD res;
  int k;
  Process_List *pl;

  if (plist_length == 0) {
    errno = ECHILD;
    return -1;
  }

  hl = (HANDLE *) malloc (sizeof (HANDLE) * plist_length);

  k = 0;
  /* -------------------- critical section -------------------- */
  plist_enter();
  pl = PLIST;
  while (pl) {
    hl[k++] = pl->h;
    pl = pl->next;
  }
  plist_leave();
  /* -------------------- critical section -------------------- */

  res = WaitForMultipleObjects (plist_length, hl, FALSE, INFINITE);

  h = hl [res - WAIT_OBJECT_0];
  free (hl);

  remove_handle (h);

  GetExitCodeProcess (h, &exitcode);
  CloseHandle (h);

  *status = (int)exitcode;
  return (int)h;
}

#endif

int
portable_no_block_spawn (args)
    char *args[];
{
  int pid = 0;

#if defined (__EMX__) || defined (MSDOS)
  /* ??? For PC machines I (Franco) don't know the system calls to
     implement this routine. So I'll fake it as follows. This routine
     will behave exactly like the blocking portable_spawn and will
     systematically return a pid of 0 unless the spawned task did not
     complete successfully, in which case we return a pid of -1.  To
     synchronize with this the portable_wait below systematically
     returns a pid of 0 and reports that the subprocess terminated
     successfully. */

  if (spawnvp (P_WAIT, args [0], args) != 0)
    return (-1);

#elif defined (_WIN32)

  pid = win32_no_block_spawn (args[0], args);
  return pid;

#elif defined (__vxworks) /* Mods for VxWorks */
  pid = sp(args[0], args);  /* Spawn task and then return (no waiting) */
  if (pid == -1)
    return (4);
  return pid;

#else
  pid = fork ();

  if (pid == 0) {
    /* The child */
    if (execv (args [0], args) != 0)
      _exit (1);
  }
#endif

  return pid;
}

int
portable_wait (process_status)
    int *process_status;
{
  int status = 0;
  int pid    = 0;

#if defined (_WIN32)

  pid = win32_wait (&status);

#elif defined (__EMX__) || defined (MSDOS)
  /* ??? See corresponding comment in portable_no_block_spawn. */

#elif defined (__vxworks)
  /* Not sure what to do here, so do same as __EMX__ case,
     i.e., nothing but return zero                         */
#else
#ifdef VMS
  /* Wait doesn't do the right thing on VMS */
  pid    = waitpid (-1, &status, 0);
#else
  pid    = wait (&status);
#endif
  status = status & 0xffff;
#endif

  *process_status = status;
  return pid;
}

void
os_exit (status)
     int status;
{
#ifdef VMS
  /* Exit without changing 0 to 1 */
  __posix_exit (status);
#else
  exit (status);
#endif
}

/* Locate a regular file, give a Path value */

char *
locate_regular_file (file_name, path_val)
     char *file_name;
     char *path_val;
{
  int len;
  char *ptr;

  /* Handle absolute pathnames. */
  for (ptr = file_name; *ptr && *ptr != '/' && *ptr != DIR_SEPARATOR; ptr++)
    ;

  if (*ptr != 0
#if defined(__EMX__) || defined(MSDOS) || defined(WINNT)
      || isalpha (file_name [0]) && file_name [1] == ':'
#endif
     )
    {
      if (is_regular_file (file_name))
        return xstrdup (file_name);

      return 0;
    }

  if (path_val == 0)
    return 0;

  {
    /* The result has to be smaller than path_val + file_name.  */
    char *file_path = alloca (strlen (path_val) + strlen (file_name) + 2);
    char pathsep_char = Get_Pathsep_Char ();

    for (;;)
      {
        for (; *path_val == pathsep_char ; path_val++)
          ;

      if (*path_val == 0)
        return 0;

      for (ptr = file_path; *path_val && *path_val != pathsep_char; )
        *ptr++ = *path_val++;

      ptr--;
      if (*ptr != '/' && *ptr != DIR_SEPARATOR)
        *++ptr = DIR_SEPARATOR;

      strcpy (++ptr, file_name);

      if (is_regular_file (file_path))
        return xstrdup (file_path);
      }
  }

  return 0;
}

/* Locate an executable given a Path argument. This routine is only used by
   gnatbl and should not be used otherwise.  Use locate_exec_on_path
   instead. */

char *
locate_exec (exec_name, path_val)
     char *exec_name;
     char *path_val;
{
  if (!strstr (exec_name, EXECUTABLE_SUFFIX))
    {
      char *full_exec_name
        = alloca (strlen(exec_name) + strlen(EXECUTABLE_SUFFIX) + 1);

      strcpy (full_exec_name, exec_name);
      strcat (full_exec_name, EXECUTABLE_SUFFIX);
      return locate_regular_file (full_exec_name, path_val);
    }
  else
    return locate_regular_file (exec_name, path_val);
}

/* Locate an executable using the Systems default PATH */

char *
locate_exec_on_path (exec_name)
     char *exec_name;
{
#ifdef VMS
  /* Warning: getenv only retrieves the first directory in VAXC$PATH */
  char *path_val = to_canonical_dir_spec (getenv ("VAXC$PATH"), 0);
#else
  char *path_val = getenv ("PATH");
#endif
  char *apath_val = alloca (strlen (path_val) + 1);

  strcpy (apath_val, path_val);
  return locate_exec (exec_name, apath_val);
}

#ifdef VMS

/* These functions are used to translate to and from VMS and Unix syntax
   file, directory and path specifications. */

#define MAXNAMES 256
#define NEW_CANONICAL_FILELIST_INCREMENT 64

static char new_canonical_dirspec [255];
static char new_canonical_filespec [255];
static char new_canonical_pathspec [MAXNAMES*255];
static unsigned new_canonical_filelist_index;
static unsigned new_canonical_filelist_in_use;
static unsigned new_canonical_filelist_allocated;
static char **new_canonical_filelist;
static char new_host_dirspec [255];
static char new_host_filespec [255];

/* Routine is called repeatedly by decc$from_vms() via
   to_canonical_file_list_init() until it returns 0 or the expansion runs
   out. */
static int
wildcard_translate_unix (name)
     char *name;
{
  char *ver;
  char buff [256];

  strcpy (buff, name);
  ver = strrchr (buff, '.');

  /* Chop off the version */
  if (ver)
    *ver = 0;

  /* Dynamically extend the allocation by the increment */
  if (new_canonical_filelist_in_use == new_canonical_filelist_allocated)
    {
      new_canonical_filelist_allocated += NEW_CANONICAL_FILELIST_INCREMENT;
      new_canonical_filelist = (char **) realloc
	(new_canonical_filelist,
	 new_canonical_filelist_allocated * sizeof (char *));
    }

  new_canonical_filelist[new_canonical_filelist_in_use++] = xstrdup (buff);

  return 1;
}

/* Translate a wildcard VMS file spec into a list of Unix file
   specs. First do full translation and copy the results into a list (_init),
   then return them one at a time (_next). If onlydirs set, only expand
   directory files. */
int
to_canonical_file_list_init (filespec, onlydirs)
     char *filespec;
     int onlydirs;
{
  int len;
  char buff [256];

  len = strlen (filespec);
  strcpy (buff, filespec);

  /* Only look for directories */
  if (onlydirs && !strstr (&buff [len-5], "*.dir"))
    strcat (buff, "*.dir");

  decc$from_vms(buff, wildcard_translate_unix, 1);

  /* Remove the .dir extension */
  if (onlydirs)
    {
      int i;
      char *ext;

      for (i=0; i<new_canonical_filelist_in_use; i++)
	{
	  ext = strstr (new_canonical_filelist [i], ".dir");
	  if (ext)
	    *ext = 0;
	}
    }

  return new_canonical_filelist_in_use;
}

/* Return the next filespec in the list */
char *
to_canonical_file_list_next ()
{
  return new_canonical_filelist [new_canonical_filelist_index++];
}

/* Free up storage used in the wildcard expansion */
void
to_canonical_file_list_free ()
{
  int i;

   for (i=0; i<new_canonical_filelist_in_use; i++)
     free (new_canonical_filelist [i]);

  free (new_canonical_filelist);

  new_canonical_filelist_in_use = 0;
  new_canonical_filelist_allocated = 0;
  new_canonical_filelist_index = 0;
  new_canonical_filelist = 0;
}

/* Translate a VMS syntax directory specification in to Unix syntax.
   If prefixflag is set, append an underscore "/". If no indicators
   of VMS syntax found, return input string. Also translate a dirname
   that contains no slashes, in case it's a logical name. */
char *
to_canonical_dir_spec (dirspec,prefixflag)
     char *dirspec;
     int prefixflag;
{
  int len;

  strcpy (new_canonical_dirspec, "");
  if (strlen (dirspec))
    {
      char *dirspec1;

      if (strchr (dirspec, ']') || strchr (dirspec, ':'))
        strcpy (new_canonical_dirspec, (char *)decc$translate_vms (dirspec));
      else if (!strchr (dirspec, '/') && (dirspec1 = getenv (dirspec)))
        strcpy (new_canonical_dirspec, (char *)decc$translate_vms (dirspec1));
      else
        strcpy (new_canonical_dirspec, dirspec);
    }

  len = strlen (new_canonical_dirspec);
  if (prefixflag && new_canonical_dirspec [len-1] != '/')
    strcat (new_canonical_dirspec, "/");

  return new_canonical_dirspec;

}

/* Translate a VMS syntax file specification into Unix syntax.
   If no indicators of VMS syntax found, return input string. */
char *
to_canonical_file_spec (filespec)
     char *filespec;
{
  strcpy (new_canonical_filespec, "");
  if (strchr (filespec, ']') || strchr (filespec, ':'))
    strcpy (new_canonical_filespec, (char *)decc$translate_vms (filespec));
  else
    strcpy (new_canonical_filespec, filespec);

  return new_canonical_filespec;
}

/* Translate a VMS syntax path specification into Unix syntax.
   If no indicators of VMS syntax found, return input string. */
char *
to_canonical_path_spec (pathspec)
     char *pathspec;
{
  char *curr, *next, buff [256];

  if (pathspec == 0)
    return pathspec;

  /* If there are /'s, assume it's a Unix path spec and return */
  if (strchr (pathspec, '/'))
    return pathspec;

  new_canonical_pathspec [0] = 0;
  curr = pathspec;

  for (;;)
    {
      next = strchr (curr, ',');
      if (next == 0)
        next = strchr (curr, 0);
      strncpy (buff, curr, next - curr);
      buff [next - curr] = 0;

      /* Check for wildcards and expand if present */
      if (strchr (buff, '*') || strchr (buff, '%') || strstr (buff, "..."))
        {
          int i, dirs;

          dirs = to_canonical_file_list_init (buff, 1);
          for (i=0; i<dirs; i++)
            {
              char *next_dir;

              next_dir = to_canonical_file_list_next ();
              strcat (new_canonical_pathspec, next_dir);

              /* Don't append the separator after the last expansion */
              if (i+1 < dirs)
                strcat (new_canonical_pathspec, ":");
            }

	  to_canonical_file_list_free ();
        }
      else
        {
          strcat (new_canonical_pathspec, to_canonical_dir_spec (buff, 0));
        }
      if (*next == 0)
        break;
      strcat (new_canonical_pathspec, ":");
      curr = next + 1;
    }
  return new_canonical_pathspec;
}

static char filename_buff [256];

static int
translate_unix (name, type)
     char *name;
     int type;
{
  strcpy (filename_buff, name);
  return 0;
}

/* Translate a Unix syntax directory specification into VMS syntax.
   The prefixflag has no effect, but is kept for symmetry with
   to_canonical_dir_spec.
   If indicators of VMS syntax found, return input string. */
char *
to_host_dir_spec (dirspec,prefixflag)
     char *dirspec;
     int prefixflag;
{
  int len = strlen (dirspec);

  strcpy (new_host_dirspec, dirspec);

  if (strchr (new_host_dirspec, ']') || strchr (new_host_dirspec, ':'))
    return new_host_dirspec;

  while (len > 1 && new_host_dirspec [len-1] == '/')
    {
      new_host_dirspec [len-1] = 0;
      len--;
    }

  decc$to_vms (new_host_dirspec, translate_unix, 1, 2);
  strcpy (new_host_dirspec, filename_buff);

  return new_host_dirspec;

}

/* Translate a Unix syntax file specification into VMS syntax.
   If indicators of VMS syntax found, return input string. */
char *
to_host_file_spec (filespec)
     char *filespec;
{
  strcpy (new_host_filespec, "");
  if (strchr (filespec, ']') || strchr (filespec, ':'))
    strcpy (new_host_filespec, filespec);
  else
    {
      decc$to_vms (filespec, translate_unix, 1, 1);
      strcpy (new_host_filespec, filename_buff);
    }

  return new_host_filespec;
}

void
adjust_os_resource_limits ()
{
  SYS$ADJWSL (131072, 0);
}

#else

/* Dummy functions for Osint import for non-VMS systems */

int
to_canonical_file_list_init (dirspec)
     char *dirspec;
{
  return 0;
}

char *
to_canonical_file_list_next ()
{
  return "";
}

void
to_canonical_file_list_free () {;}

char *
to_canonical_dir_spec (dirspec, prefixflag)
     char *dirspec;
     int prefixflag;
{
  return dirspec;
}

char *
to_canonical_file_spec (filespec)
     char *filespec;
{
  return filespec;
}

char *
to_canonical_path_spec (pathspec)
     char *pathspec;
{
  return pathspec;
}

char *
to_host_dir_spec (dirspec, prefixflag)
     char *dirspec;
     int prefixflag;
{
  return dirspec;
}

char *
to_host_file_spec (filespec)
        char *filespec;
{
  return filespec;
}

void
adjust_os_resource_limits () {;}

#endif

/* for EMX, we cannot include dummy in libgcc, since it is too difficult
   to coordinate this with the EMX distribution. Consequently, we put the
   definition of dummy() which is used for exception handling, here */

#if defined (__EMX__)
void __dummy () {}
#endif


#if defined (__mips_vxworks)
int _flush_cache() {
   CACHE_USER_FLUSH (0, ENTIRE_CACHE);
}
#endif

#if defined (CROSS_COMPILE) || \
(!(defined (sparc) && defined (sun) && defined (__SVR4)) \
   && !defined (linux) \
   && !defined (sgi) \
   && !(defined (__alpha__)  && defined (__osf__)) \
   && !defined (__MINGW32__))
/* Dummy function to satisfy g-trasym.o.
   Currently only Solaris, Linux & Windows provides a non-dummy version of 
   this procedure in libaddr2line.a */

void
convert_addresses (addrs, n_addr, buf, len)
     void *addrs;
     int n_addr;
     void *buf;
     int *len;
{
  *len = 0;
}
#endif
