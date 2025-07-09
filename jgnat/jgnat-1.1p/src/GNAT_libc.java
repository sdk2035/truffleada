//////////////////////////////////////////////////////////////////////////////
//                                                                          //
//                         GNAT COMPILER COMPONENTS                         //
//                                                                          //
//                            G N A T _ l i b c                             //
//                                                                          //
//                            $Revision: 1.25 $
//                                                                          //
//          Copyright (C) 1998-2000 Ada Core Technologies, Inc.             //
//                                                                          //
// GNAT is free software;  you can  redistribute it  and/or modify it under //
// terms of the  GNU General Public License as published  by the Free Soft- //
// ware  Foundation;  either version 2,  or (at your option) any later ver- //
// sion.  GNAT is distributed in the hope that it will be useful, but WITH- //
// OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY //
// or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License //
// for  more details.  You should have  received  a copy of the GNU General //
// Public License  distributed with GNAT;  see file COPYING.  If not, write //
// to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, //
// MA 02111-1307, USA.                                                      //
//                                                                          //
// JGNAT - The GNAT Ada 95 toolchain for the Java Virtual Machine is        //
//         maintained by Ada Core Technologies, Inc. - http://www.gnat.com  //
//                                                                          //
//////////////////////////////////////////////////////////////////////////////

//  This Java class is part of the JGNAT library.

//  The GNAT library sources have a number of "pragma Import (C, ..)" sprinkled
//  throughout. Some of these C routines are part of the C library, others are
//  part of the C sources of GNAT.  The purpose of this Java class is to give a
//  Java-compatible definition for all the C routines used in the GNAT sources
//  which are part of the JGNAT library. 

//  The actual mapping between the "pragma Import (C, ...)" routines and the
//  methods in this class is performed by the JGNAT compiler:  whenever an Ada
//  subprogram, say <routine>, is imported from C, for instance:
//
//             pragma Import (C, <routine>, <C_routine>);
//
//  the JGNAT compiler transforms all Ada calls to <routine> into calls to a
//  public static method jgnat.adalib.GNAT_libc.<C_routine> with the
//  appropriate parameter signature.

package jgnat.adalib;


import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.PushbackReader;
import java.io.RandomAccessFile;
import java.io.Reader;

import java.lang.Character;
import java.lang.Class;
import java.lang.String;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import java.security.AccessControlException;

import java.util.Date;
import java.util.GregorianCalendar;

import jgnat.adalib.constraint_error;
import jgnat.adalib.program_error;
import jgnat.adalib.storage_error;
import jgnat.adalib.tasking_error;
import jgnat.adalib.Object_File;

public class GNAT_libc {

   ///////////////////////////////////////////////////////
   ///////////////////////////////////////////////////////
   ////                                               ////
   ////  General Purpose Routines Used in this Class  ////
   ////                                               ////     
   ///////////////////////////////////////////////////////
   ///////////////////////////////////////////////////////

   //  Throws an exception if `b' is false

   private static void assert (boolean b) 
   {
      if (! b)
	 raise ("assert failure");
   }
   
   //  Copy String `name' into a byte array `buffer' which is already allocated
   //  and add zero after the last character of name.

   private static void copy (String name, byte buffer []) 
   {
      int k;

      for (k = 0; k < name.length (); k++)
	 buffer [k] = (byte) name.charAt (k);

      buffer [k] = 0;
   }

   //  size_t strlen (const char *)

   private static int strlen (byte name []) 
   {
      int zero_pos;

      for (zero_pos = 0; zero_pos < name.length; zero_pos++) {
	 if (name [zero_pos] == 0)
	    return zero_pos;
      }
      raise ("No null terminating zero found");
      return -1;
   }

   //  Throws a java.lang.Error with meassage `msg'

   private static void raise (String msg) 
   {
      throw new java.lang.Error (msg);
   }
   
   //  `name' is a null terminated byte array. Returns a File object
   //  corresponding to `name'. If `name' is not null terminated then 
   //  an exception is thrown.

   private static File to_File (byte name []) 
   {
      return new File (new String (name, 0, strlen (name)));
   }

   //  Returns 1 if `b' is true, 0 otherwise

   private static int to_int (boolean b) 
   {
      return b ? 1 : 0;
   }


   ////////////////////////////////////////////////////////////////////////
   ////////////////////////////////////////////////////////////////////////
   ////                                                                ////
   ////  General Purpose Routines Used in Several JGNAT Library Units  ////
   ////                                                                ////
   ////////////////////////////////////////////////////////////////////////
   ////////////////////////////////////////////////////////////////////////

   //  The following routines are used to convert Ada strings into Java Strings

   public static String to_string (byte ada_string []) {
      return new String (ada_string);
   }

   public static byte [] to_string (String java_string) {
      return java_string.getBytes ();
   }

   //  Given a java.lang.Class object, the following returns an array of bytes
   //  containing the fully qualified name of that class. Because of the way
   //  JGNAT maps Ada tagged (and untagged) records and exceptions to Java
   //  classes this routine is use to implement Ada.Tags.Expanded_Name and
   //  Ada.Exceptions.Exception_Name. 

   //  More specifically if the fully qualified name of an Ada tagged type or
   //  exception D is X.Y.Z.D, where X, Y and Z are all packages, then JGNAT
   //  maps D into class x$y$z$d.class. If, on the other hand, D is declared
   //  inside an inner subprogram Z, then JGNAT adds a `__nnn', where `nnn' is
   //  some unique number, to the end of inner subprogram Z in case Z is
   //  overloaded. For instance if X is a package, Y a procedure declared
   //  inside X and Z an inner procedure of Y which is overloaded with another
   //  procdeure nested inside Y, JGNAT will generate file x$y$z__2$d.class.
   //  After retrieving the external_tag of a java class, the code below
   //  replaces all `$' with `.', removes all `__nnn' and converts the final
   //  string to upper case letters.

   public static byte [] ada_name (Object t) {
      //  First get the actual external tag
      byte s [] = external_tag (t);
      byte r [] = new byte [s.length];

      int r_pos = 0;

      for (int k = 0; k < s.length; k++) {
	 if ((s [k] == (byte) '_') && (s [k + 1] == (byte) '_')) {
	    k = k + 2;
	    while ((s [k] >= (byte) '0') && (s [k] <= (byte) '9'))
	       k++;
	    k--;
	 }
	 else {
	    if (s [k] == (byte) '$')
	       r [r_pos] = (byte) '.';
	    else 
	       r [r_pos] = (byte) Character.toUpperCase ((char) s [k]);
	    r_pos++;
	 }
      }

      return r;
   }

   //  Given a JGNAT System.Address (that is a reference to an Object) this
   //  routine returns the hash code integer associated with this reference.
   //  Typically this is the address of the Object, but this is not guaranteed
   //  by the Java API. 

    public static int hash_code (Object address) {
       return address.hashCode ();
    }


   /////////////////////////////////////
   /////////////////////////////////////
   ////                             ////
   ////  Entities in libc <math.h>  ////
   ////                             ////
   /////////////////////////////////////
   /////////////////////////////////////

   public static double sin  (double a) { return Math.sin (a); }
   public static double cos  (double a) { return Math.cos (a); }
   public static double tan  (double a) { return Math.tan (a); }
   public static double asin (double a) { return Math.asin (a); }
   public static double acos (double a) { return Math.acos (a); }
   public static double atan (double a) { return Math.atan (a); }
   public static double exp  (double a) { return Math.exp (a); }
   public static double log  (double a) { return Math.log (a); }
   public static double sqrt (double a) { return Math.sqrt (a); }
   public static double pow  (double a, double b) { return Math.pow (a, b); }

   public static double sinh (double a) 
   {
      return (Math.exp (a) - Math.exp (-a)) * 0.5;
   }
   
   public static double cosh (double a) 
   {
      return (Math.exp (a) + Math.exp (-a)) * 0.5;
   }
   
   public static double tanh (double a) 
   {
      return (Math.exp (a) - Math.exp (-a))/(Math.exp (a) + Math.exp (-a));
   }


   ///////////////////////////////////////
   ///////////////////////////////////////
   ////                               ////
   ////  Entities in libc <stdlib.h>  ////
   ////                               ////
   ///////////////////////////////////////
   ///////////////////////////////////////

   private static final int EOF      = -1;
   private static final int _IOFBF   = 0;
   private static final int _IOLBF   = 1;
   private static final int _IONBF   = 2;
   private static final int SEEK_CUR = 1;
   private static final int SEEK_END = 2;
   private static final int SEEK_SET = 0;
   private static final int L_tmpnam = 256;

   private static PushbackReader stdin = null;

   //  We protect the initialization of stdin as a solution to avoiding
   //  an exception for applets that need GNAT_libc (the constructor
   //  new FileReader will raise java.security.AccessControlException).
   //  A better solution may be needed since currently applets will be
   //  prevented from performing any Ada text input, not just text input
   //  from disk files (but maybe that's okay). ???

   static {
      try {
         stdin =
            new PushbackReader 
                  (new LineNumberReader (new FileReader (FileDescriptor.in)));
      }
      catch (Throwable e) {
         // Ignore the exception and continue...
      }
   }

   //  WARNING: The following is NOT a general implementation of the C routines
   //  in <stdlib.h>. The following implementation takes advantage of the way
   //  these routines are used in the GNAT sources.

   //  With this implementation the user can create and package an Ada applet
   //  on a UNIX workstation and run it on a Windows platform (which is the
   //  whole point of the JVM & the Java API).  The difference in text file
   //  formats between UNIX and Windows are hidden in the implementation of the
   //  Java API.

   //  void clearerr (FILE *stream)

   public static void clearerr (Object stream) 
   {
      //  Every call in the GNAT sources to ferror looks (in spirit) like
      //
      //     if ferror (stream) /= then
      //        raise Device_Error;
      //
      //  Because this functionality is not directly available in the Java API
      //  the C routines that are used in the GNAT sources and that may cause
      //  ferror to return a non zero value (fgetc, getc_immediate, 
      //  getc_imediate_nowait, and fread) are implemented as follows:
      //
      //        getc (...) {
      //           try {
      //             ... // try to get the byte
      //           }
      //           catch (IOException e) {
      //              throw new ada$io_exceptions$device_error ();
      //           }
      //        }
      //
      //  All this means that our implementation of ferror can safely return 0.
      // As a result this routine does noting.
   }

   //  int fclose (FILE *stream)

   public static int fclose (Object stream) 
   {
      try {
	 if (stream instanceof PushbackReader) {
	    ((PushbackReader) stream).close ();
	 }
	 else if (stream instanceof PrintStream) {
	    ((PrintStream) stream).close ();
	 }
	 else if (stream instanceof RandomAccessFile) {
	    ((RandomAccessFile) stream).close ();
	 }
	 else if (stream instanceof Object_File) {
	    ((Object_File) stream).close ();
	 }
	 else
	    assert (false);
	 
	 return 0;
      }
      catch (IOException e) {
	 return EOF;
      }
   }
   
   //  FILE *fdopen (int handle, const char *mode)
   //  UNIMPLEMENTED - not needed by the JGNAT library

   //  int feof (FILE *stream)

   public static int feof (Object stream) { 
      int c = ungetc (fgetc (stream), stream);
      return to_int (c == EOF);
   }

   //  int ferror (FILE *stream)

   public static int ferror (Object stream) { 
      //  See comment inside method `clearerr' in this class
      return 0;
   }

   //  int fflush (FILE *stream)

   public static int fflush (Object stream) 
   {
       if (stream instanceof PrintStream)
	   ((PrintStream) stream).flush ();
       else if (stream instanceof RandomAccessFile)
	   ; //  do nothing
       else if (stream instanceof Object_File)
	   ; //  do nothing
       else
	   assert (false);
	 
       return 0;
   }

   //  int fgetc (FILE *stream)

   public static int fgetc (Object stream) {
      try {
	 if (stream instanceof PushbackReader) {
	    return ((PushbackReader) stream).read ();
	 }
	 else if (stream instanceof RandomAccessFile) {
	    return ((RandomAccessFile) stream).read ();
	 }
	 else {
	    assert (false);
	    return 0;
	 }
      }
      catch (IOException e) {
	 throw new ada$io_exceptions$device_error ();
      }
   }

   //  char *fgets (char *s, int n, FILE *stream)
   //  UNIMPLEMENTED - not needed by the JGNAT library

   //  int fileno (FILE *stream)

   public static int fileno (Object stream) {
      if (stream == stdin)
	 return 0;
      else if (stream == System.out)
	 return 1;
      else if (stream == System.err)
	 return 2;
      else
	 //  ??? for now always associate the samae handle with a stream
	 return 99;
   }
   
   //  Special method to allow the top level file I/O pacakges
   //  to communicate the class of file API to be used by fopen
   //  (e.g., RandomAccessFile, Object_File). This avoids making
   //  JGNAT-specific changes to the lower-level file support
   //  packages such as System.File_IO, etc (it would be nice
   //  if the file classification could be simply passed to
   //  fopen as part of the mode info).

   public static byte file_class = (byte) '0';

   public static void set_file_class (byte fclass) {
      file_class = fclass;
   }

   //  FILE *fopen (const char *filename, const char *mode)

   public static Object fopen (Object FILENAME, Object MODE) 
     throws java.io.IOException, java.io.StreamCorruptedException,
            java.lang.ClassNotFoundException
   {
      byte filename [] = (byte []) FILENAME;
      byte mode []     = (byte []) MODE;

      int end = strlen (mode) - 1;

      final boolean update      = (mode [1]   == (byte) '+');
      final boolean text_mode   = (mode [end] == (byte) 't');
      final boolean binary_mode = (mode [end] == (byte) 'b');

      assert (text_mode || binary_mode);

      final boolean read_only    = (mode [0] == (byte) 'r') && !update;
      final boolean write_only   = (mode [0] == (byte) 'w') && !update;
      final boolean read_update  = (mode [0] == (byte) 'r') && update;
      final boolean write_update = (mode [0] == (byte) 'w') && update;

      File file = to_File (filename);

      if (text_mode) {
	 //  When opening a text file in read mode we must provide "ungetc"
	 //  capabilities (this is provided by the PushBackReader). 
         //  Furthermore, we must provide the ability of reading end-of-lines
	 //  in a portable manner (this is provided by the LineNumberReader).

         //  Ensure that the file_class is reset, in case it was set
         //  to 'S' by freopen.

         file_class = (byte) '0';

	 if (read_only) {
	    try {
	       return
		  new PushbackReader 
		     (new LineNumberReader (new FileReader (file)));
	    }
	    catch (FileNotFoundException e) { return null; }
	 }

	 //  When opening a text file in append mode we must first open it as a
	 //  random file and position the file head to the end before creating
	 //  a regular PrintStream. This is because the implementation of fseek
	 //  in this class is not capable of handling the SEEK_END case for
	 //  text streams.

	 else if (read_update) {
	    try {
	       RandomAccessFile r = new RandomAccessFile (file, "rw");
	       r.seek (r.length());
	       return new PrintStream (new FileOutputStream (r.getFD()));
	    }
	    catch (IOException e) { 
	       return null; 
	    }
	 }

	 //  When opening a text file in read mode we must create it as a
	 //  PrintStream to be compatible with the way Standard.out is created.

	 else if (write_only || write_update) {
	    try { return new PrintStream (new FileOutputStream (file)); }
	    catch (IOException e) { 
	       return null; 
	    }
	 }

	 else 
	    assert (false);
      }

      else if (binary_mode && file_class == (byte) 'S') {
         String java_mode = (read_only) ? "r" : "rw";

         file_class = (byte) '0';

         try { 
            return new RandomAccessFile (file, java_mode); 
         }
         catch (IOException e) { 
            return null; 
         }
      }

      //  Open an Object_IO file (supports Sequential_IO and Direct_IO files)

      else {
         try {
            return new Object_File (file, mode [0] == (byte) 'w', update);
         }
	 catch (IOException e) { 
	    return null; 
	 }
      }

      return null;
   }

   //  int fputc (int c, FILE *stream)

   public static int fputc (int c, Object stream) {
      try {
	 if (stream instanceof PrintStream) {
	    if (c == 10)
	       ((PrintStream) stream).println ();
	    else
	       ((PrintStream) stream).print ((char) c);
	 }
	 else if (stream instanceof RandomAccessFile) {
	    ((RandomAccessFile) stream).write (c);
	 }
	 else
	    assert (false);
	 
	 return c;
      }
      catch (IOException e) {
	 throw new ada$io_exceptions$device_error ();
      }
   }

   //  int fputs (const char *s, FILE *stream)
   //  UNIMPLEMENTED - not needed by the JGNAT library

   //  size_t fread (void *ptr, size_t size, size_t nobj, FILE *stream)

   public static int fread (Object ptr, int size, int nobj, Object stream)
   {
      //  GNAT sources use a size of 1
      assert (size == 1);
      
      try {
	 if (ptr instanceof Int) {
	    assert (nobj == 4);
	    ((Int) ptr).all = ((RandomAccessFile) stream).readInt ();
	 }
	 else if (ptr instanceof Lng) {
	    assert (nobj == 8);
	    ((Lng) ptr).all = ((RandomAccessFile) stream).readLong ();
	 }
	 else if (ptr instanceof Flt) {
	    assert (nobj == 4);
	    ((Flt) ptr).all = ((RandomAccessFile) stream).readFloat ();
	 }
	 else if (ptr instanceof Dbl) {
	    assert (nobj == 8);
	    ((Dbl) ptr).all = ((RandomAccessFile) stream).readDouble ();
	 }
	 else {
            return fread (ptr, 0, 1, nobj, stream);
	 }
	 return nobj;
      }
      catch (IOException e) {
	 throw new ada$io_exceptions$device_error ();
      }
   }
   
   //  This function does not exist in the C library but has been introduced
   //  because the GNAT sources take the address an element within the array
   //  and pass that address to fread to indicate that the array should be
   //  filled from that element on. This is not possible in the JVM and the
   //  following routine works exactly like fread except that it puts elements
   //  in buffer `ptr' at starting index `index'.

   public static int fread (Object ptr, 
			    int index, 
			    int size, int nobj, Object stream)
   {
      byte buf [] = (byte []) ptr;
      int  nb_bytes_read = 0;
      
      //  GNAT sources use a size of 1
      assert (size == 1);
      
      try {
	 if (stream instanceof PushbackReader) {
	    PushbackReader text_stream = (PushbackReader) stream;
	    int k;
	    int c;
	    
	    for (k = 0; k < nobj; k++) {
	       c = text_stream.read ();

	       if (c != EOF)
		  buf [index + k] = (byte) c;
	       else
		  return nb_bytes_read;

               nb_bytes_read++;
	    }

            return nb_bytes_read;
	 }
	 else if (stream instanceof RandomAccessFile) {
	    return ((RandomAccessFile) stream).read (buf, index, nobj);
	 }
	 else
	    assert (false);
      }
      catch (IOException e) {
	 throw new ada$io_exceptions$device_error ();
      }
      return 0;
   }
   
   //  FILE *freopen (const char *filename, const char *mode, FILE *stream)

   public static Object freopen (Object filename, Object mode, Object stream) 
     throws java.io.IOException, java.io.StreamCorruptedException,
            java.lang.ClassNotFoundException
   {
      //  In the implementation of freopen we do NOT associate the returned
      //  stream with the input stream `stream' because GNAT sources do not
      //  need this and it is too much work to do so.

      fclose (stream);

      //  This is a kludge to ensure that Reset will work properly
      //  for stream files. The file could actually be a text file
      //  (opened for read/update), but fopen will check for the
      //  text file case first, so there won't be any problem with
      //  setting the file class to 'S' in that case.

      if (stream instanceof RandomAccessFile) {
         set_file_class ((byte) 'S');
      }

      return fopen (filename, mode);
   }

   //  int fseek (FILE *stream, long offset, int origin)

   public static int fseek (Object stream, long offset, int origin) 
   {
      boolean stream_is_binary = (stream instanceof RandomAccessFile)
                              || (stream instanceof Object_File);

      //  The following assertions state the assumptions made in the following
      //  code given the current use of fseek from the GNAT sources.

      //  Calls to fseek with SEEK_CUR have not been implemented

      assert (origin != SEEK_CUR);

      //  Calls to fseek with SEEK_END must have offset == 0 and cannot be from
      //  text streams in read only mode (i.e. PushbachReader streams).

      assert ((origin != SEEK_END) || (offset == 0L));
      assert ((origin != SEEK_END) || ! (stream instanceof PushbackReader));

      //  Calls to fseek with SEEK_SET can only be done for a file opened
      //  in binary mode (i.e., a RandomAccessFile or Object_File).

      assert ((origin != SEEK_SET) || stream_is_binary);
      
      try { 
	 if (origin == SEEK_SET) {
	    if (stream instanceof RandomAccessFile) {
	       ((RandomAccessFile) stream).seek (offset);
            }
	    else if (stream instanceof Object_File) {
	       Object_File.set_file_index
                 ((Object_File) stream, offset + 1);
            }
	 }
	 else if (origin == SEEK_END) {
	    if (stream_is_binary) {
	       if (stream instanceof RandomAccessFile) {
                  RandomAccessFile s = (RandomAccessFile) stream;
                  s.seek (s.length ());
               }
	       else if (stream instanceof Object_File) {
                  Object_File s = (Object_File) stream;
                  Object_File.set_file_index
                    (s, Object_File.file_size (s) + 1);
               }
	    }
	    //  There is nothing to do for text streams that you can write into
	    //  since for these fseek is only called in append mode and the
	    //  actual positioning at the end of the file in append mode is
	    //  done by fopen.
	 }
	 return 0;
      }
      catch (IOException e) { 
	 return 1; 
      }
   }
   
   //  long ftell (FILE *stream)

   public static long ftell (Object stream) 
   {
      try {
	 if (stream instanceof RandomAccessFile)
	    return ((RandomAccessFile) stream).getFilePointer ();
	 else
	    assert (false);
      }
      catch (IOException e) {
	 throw new ada$io_exceptions$device_error ();
      }
      return -1L;
   }

   //  size_t fwrite (const void *ptr, size_t size, size_t nobj, FILE *stream)

   public static int fwrite (Object ptr, int size, int nobj, Object stream) {
      //  GNAT sources use either a size of 1 or an nboj of 1

      assert ((size == 1) || (nobj == 1));
      
      try {
	 if (ptr instanceof Int) {
	    assert ((size == 4) || (nobj == 4));
	    ((RandomAccessFile) stream).writeInt (((Int) ptr).all);
	 }
	 else if (ptr instanceof Lng) {
	    assert ((size == 8) || (nobj == 8));
	    ((RandomAccessFile) stream).writeLong (((Lng) ptr).all);
	 }
	 else if (ptr instanceof Flt) {
	    assert ((size == 4) || (nobj == 4));
	    ((RandomAccessFile) stream).writeFloat (((Flt) ptr).all);
	 }
	 else if (ptr instanceof Dbl) {
	    assert ((size == 8) || (nobj == 8));
	    ((RandomAccessFile) stream).writeDouble (((Dbl) ptr).all);
	 }
	 else {
	    byte buf [] = (byte []) ptr;
      
	    if (stream instanceof PrintStream) {
	       PrintStream text_stream = (PrintStream) stream;
	       int c;
	       int length = (size == 1) ? nobj : size;
		  
	       for (int k = 0; k < length; k++) {
		  c = 255 & buf [k];

		  if (c == 10)
		     text_stream.println ();
		  else
		     text_stream.print ((char) c);
	       }
	    }
	    else if (stream instanceof RandomAccessFile) {
	       ((RandomAccessFile) stream).write (buf, 0, size);
	    }
	    else {
	       assert (false);
            }
	 }
	 return nobj;
      }
      catch (IOException e) {
	 return 0;
      }
   }

   //  void getc_immediate_nowait 
   //         (FILE *stream, int *ch, int *end_of_file, int *avail)

   public static void getc_immediate_nowait 
                        (Object stream, Int ch, Int end_of_file, Int avail) {
      if (stream instanceof PushbackReader)
	 try {
	    if (! ((PushbackReader) stream).ready ()) {
	       end_of_file.all = 0;
	       avail.all = 0;
	       return;
	    }
	 }
	 catch (IOException e) {
	    throw new ada$io_exceptions$device_error ();
	 }

      avail.all = 1;
      getc_immediate (stream, ch, end_of_file);
   }

   //  void getc_immediate (FILE *stream, int *ch, int *end_of_file)

   public static void getc_immediate (Object stream, Int ch, Int end_of_file) {
      ch.all = fgetc (stream);
      end_of_file.all = to_int (ch.all == EOF);
   }
   
   //  int isatty (int handle)
   //  UNIMPLEMENTED - not needed by the JGNAT library

   //  char *mktemp (char *buf)
   //  UNIMPLEMENTED - not needed by the JGNAT library

   //  void rewind (FILE *stream)

   public static void rewind (Object stream) 
   {
      fseek (stream, 0L, SEEK_SET);
   }

   //  int setvbuf (FILE *stream, char *buf, int mode, size_t size)

   public static int setvbuf (Object stream, Object buf, int mode, int size) 
   {
      assert (buf == null);

      //  System.out & System.err are unbuffered by default
      if ((stream == System.out) || (stream == System.err)) {
	 assert (mode == _IONBF);
      }

      //  Input & output text streams are buffered by default
      else if (mode == _IONBF) {
	 assert (! (stream instanceof Reader));
	 assert (! (stream instanceof PrintStream));
      }

      //  Random Acces Files are unbuffered by default
      else if ((mode == _IOLBF) || (mode == _IOFBF)) {
	 assert (! (stream instanceof RandomAccessFile));
      }

      else {
	 assert (false);
      }

      return 0;
   }

   //  FILE *tmpfile (void)
   //  UNIMPLEMENTED - not needed by the JGNAT library
   
   //  char *tmpnam (char s[L_tmpnam])

   static public void tmpnam (Object s) 
   {
      byte buf [] = (byte []) s;
      
      try {
	 copy (File.createTempFile ("JGNAT-", null).getCanonicalPath (), buf);
      }
      catch (IOException e) {
	 buf [0] = 0;
      }
   }

   //  FILE *tmpfile (void)
   
   //  char *__gnat_tmp_name (char s[L_tmpnam])

   static public void __gnat_tmp_name (Object s) 
   {
      tmpnam (s);
   }

   //  int ungetc (int c, FILE *stream)

   public static int ungetc (int c, Object stream) 
   {
      if (c == EOF)
	 return EOF;

      try {
	 if (stream instanceof PushbackReader) {
	    ((PushbackReader) stream).unread (c);
	 }

	 //  For a binary stream (i.e.  a RandomAccessFile) ungetc is used as
	 //  follows in the GNAT sources:
	 //       ungetc (fgetc (stream), Stream) 
	 //  which allows us to implement ungetc by simply seking back one
	 //  character.
	 else if (stream instanceof RandomAccessFile) {
	    RandomAccessFile r = (RandomAccessFile) stream;
	    r.seek (r.getFilePointer () - 1);
	 }

	 return c;
      }
      catch (IOException e) {
	 raise ("ungetc failed");
	 return EOF;
      }
   }

   //  int unlink (char *)

   public static int unlink (Object filename) 
   {
      try {
	 return to_File ((byte []) filename).delete () ? 0 : EOF;
      }
      catch (Exception e) {
	 return EOF;
      }
   }


   /////////////////////////////////////
   /////////////////////////////////////
   ////                             ////
   ////  Entities in libc <time.h>  ////
   ////                             ////
   /////////////////////////////////////
   /////////////////////////////////////
   
   //  void __gnat_localtime_r (const time_t *seconds, tm *tp)
   
   public static void __gnat_localtime_r (Lng seconds, ada$calendar$tm tp) {
      GregorianCalendar cal = new GregorianCalendar ();
      cal.setTime (new Date (1000 * seconds.all));
      
      tp.tm_sec  = cal.get (cal.SECOND);
      tp.tm_min  = cal.get (cal.MINUTE);
      tp.tm_hour = cal.get (cal.HOUR_OF_DAY);
      tp.tm_mday = cal.get (cal.DAY_OF_MONTH);
      tp.tm_mon  = cal.get (cal.MONTH);
      tp.tm_year = cal.get (cal.YEAR) - 1900;
      tp.tm_wday = cal.get (cal.DAY_OF_WEEK) - 1;
      tp.tm_yday = cal.get (cal.DAY_OF_YEAR) - 1;
   }
   
   //  time_t mktime (struct tm *tp)
   
   public static long mktime (ada$calendar$tm tp) {
      GregorianCalendar cal =
	 new GregorianCalendar
	    (tp.tm_year + 1900,
	     tp.tm_mon,
	     tp.tm_mday,
	     tp.tm_hour,
	     tp.tm_min,
	     tp.tm_sec);

      return (cal.getTime().getTime ())/1000;
   }
   

   //////////////////////////////////
   //////////////////////////////////
   ////                          ////
   ////  Entities in a-adaint.c  ////
   ////                          ////
   //////////////////////////////////
   //////////////////////////////////

   public static int __gnat_file_exists (Object name) {
      try {
	 return to_int (to_File ((byte []) name).exists ());
      }
      catch (SecurityException e) {}

      return 0;
   }


   //////////////////////////////////
   //////////////////////////////////
   ////                          ////
   ////  Entities in a-cstrea.c  ////
   ////                          ////
   //////////////////////////////////
   //////////////////////////////////
   
   public static final int c_constant_eof      = EOF;
   public static final int c_constant_iofbf    = _IOFBF;
   public static final int c_constant_iolbf    = _IOLBF;
   public static final int c_constant_ionbf    = _IONBF;
   public static final int c_constant_seek_cur = SEEK_CUR;
   public static final int c_constant_seek_end = SEEK_END;
   public static final int c_constant_seek_set = SEEK_SET;
   public static final int c_constant_l_tmpnam = L_tmpnam;

   public static int max_path_len = 2048;

   public static void full_name (Object name, Object buffer) {
      byte buff [] = (byte []) buffer;

      try {
	 copy
           (to_File ((byte []) name).getCanonicalPath (), (byte []) buffer);
      }
      catch (IOException e) {
	 buff [0] = 0;
      }
   }

   public static int is_regular_file_fd (int handle) {
      if (handle <= 2)
	 return 0;
      else
	 //  ??? for now always assume we are dealing with regular files
	 return 1;
   }

   public static int feof__   (Object stream) { return feof (stream); }
   public static int ferror__ (Object stream) { return ferror (stream); }
   public static int fileno__ (Object stream) { return fileno (stream); }
   
   public static Object c_constant_stdin  () { return stdin; }
   public static Object c_constant_stderr () { return System.err; }
   public static Object c_constant_stdout () { return System.out; }


   ////////////////////////////////////
   ////////////////////////////////////
   ////                            ////
   ////  Entities in a-except.adb  ////
   ////                            ////
   ////////////////////////////////////
   ////////////////////////////////////

   //  function Create_EO (E : Exception_Id; M : String) return EO;

   private static Class constructor_param_type [] = { ("hello").getClass () };
   public static Object create_EO (Object e, byte msg []) {
      try {
	 Constructor c = ((Class) e).getConstructor (constructor_param_type);
	 Object param [] = { new String (msg) };
	 return c.newInstance (param);
      }
      catch (NoSuchMethodException e1) {
	  System.out.println (e.toString ());
	  raise ("The String constructor for this exception does not exist");
      }
      catch (SecurityException e2) {
	  System.out.println (e.toString ());
	  raise ("Not allowed to call the String constructor");
      }
      catch (InstantiationException e3) {
	  System.out.println (e.toString ());
	  raise ("The class for this exception is abstract");
      }
      catch (IllegalAccessException e4) {
	  System.out.println (e.toString ());
	  raise ("Call the String constructor forbidden");
      }
      catch (IllegalArgumentException e5) {
	  System.out.println (e.toString ());
	  raise ("Wrong number of parameters fro the exception constructor");
      }
      catch (InvocationTargetException e6) {
	  System.out.println (e.toString ());
	  raise ("Exception raised during call to the constructor");
      }
      return null;
   }

   // function Exception_Identity (X : EO) return Exception_Id;

   public static Object exception_identity (Object x) {
      return x.getClass ();
   }

   //  function E_Information (X : EO) return String_Access;

   public static byte [] e_information (Object x) {
      ByteArrayOutputStream the_info = new ByteArrayOutputStream ();
      PrintWriter buf = new PrintWriter (the_info);

      ((Throwable) x).printStackTrace (buf);
      return the_info.toByteArray();
   }

   //  function E_Message (X : EO) return String_Access;

   public static byte [] e_message (Object x) {
      return to_string (((Throwable) x).getMessage ());
   }

   //  procedure Reraise_Occurrence_No_Defer (X : EO);

   public static void reraise_occurrence_no_defer (Object e)
      throws Throwable
   {
      throw (Throwable) e;
   }

   public static void __gnat_raise_with_msg (Object e)
      throws Throwable
   {
      //  The call to get_current_excep returns a reference to
      //  an aliased Exception_Occurrence in the TSD. Since we
      //  represent EOs as System.Address, this will actually
      //  be a reference to an object of type jgnat.adalib.Acc,
      //  and we have to dereference it to get the occurrence.

      //  Excep : Exception_Occurrence := Get_Current_Excep.all;
      Object excep = system$soft_links.get_current_excep.Invoke ();
      Throwable EO = (Throwable) ((jgnat.adalib.Acc) excep).all;

      //  If EO is null, then there is no current exception, so
      //  pass a null string as the message.

      if (EO == null)
         throw (Throwable) create_EO (e, to_string (""));
      else
         throw (Throwable) create_EO (e, e_message (EO));
   }

   public static void __gnat_reraise ()
      throws Throwable
   {
      //  The call to get_current_excep returns a reference to
      //  an aliased Exception_Occurrence in the TSD. Since we
      //  represent EOs as System.Address, this will actually
      //  be a reference to an object of type jgnat.adalib.Acc,
      //  and we have to dereference it to get the occurrence.

      //  Excep : Exception_Occurrence := Get_Current_Excep.all;
      Object excep = system$soft_links.get_current_excep.Invoke ();
      Throwable EO = (Throwable) ((jgnat.adalib.Acc) excep).all;

      throw EO;
   }

   public static void __gnat_raise_exception (Object e, byte msg [])
      throws Throwable
   {
      throw (Throwable) create_EO (e, msg);
   }

   public static Throwable c_e = new jgnat.adalib.constraint_error ();
   public static Throwable p_e = new jgnat.adalib.program_error ();
   public static Throwable s_e = new jgnat.adalib.storage_error ();
   public static Throwable t_e = new jgnat.adalib.tasking_error ();

   public static Object constraint_error = exception_identity (c_e);
   public static Object program_error    = exception_identity (p_e);
   public static Object storage_error    = exception_identity (s_e);
   public static Object tasking_error    = exception_identity (t_e);

   ////////////////////////////////
   ////////////////////////////////
   ////                        ////
   ////  Entities in a-init.c  ////
   ////                        ////
   ////////////////////////////////
   ////////////////////////////////

   public static int  __gl_main_priority            = -1;
   public static int  __gl_time_slice_val           = -1;
   public static byte __gl_wc_encoding              = (byte) 'n';
   public static byte __gl_locking_policy           = (byte) ' ';
   public static byte __gl_queuing_policy           = (byte) ' ';
   public static byte __gl_task_dispatching_policy  = (byte) ' ';
   public static Object __gl_adafinal_ptr           = null;
   public static int  __gl_unreserve_all_interrupts = 0;
   public static int  __gl_exception_tracebacks     = 0;

   public static void
      __gnat_set_globals (int     main_priority,
			  int     time_slice_val,
			  byte    wc_encoding,
			  byte    locking_policy,
			  byte    queuing_policy,
			  byte    task_dispatching_policy,
			  Object  adafinal_ptr,
			  boolean unreserve_all_interrupts,
			  boolean exception_tracebacks) {
      __gl_main_priority            = main_priority;
      __gl_time_slice_val           = time_slice_val;
      __gl_wc_encoding              = wc_encoding;
      __gl_locking_policy           = locking_policy;
      __gl_queuing_policy           = queuing_policy;
      __gl_task_dispatching_policy  = task_dispatching_policy;
      __gl_adafinal_ptr             = adafinal_ptr;
      __gl_unreserve_all_interrupts = to_int (unreserve_all_interrupts);
      __gl_exception_tracebacks     = to_int (exception_tracebacks);
   }


   //////////////////////////////////
   //////////////////////////////////
   ////                          ////
   ////  Entities in a-sysdep.c  ////
   ////                          ////
   //////////////////////////////////
   //////////////////////////////////
   
   public static boolean text_translation_required = true;
   
   //  For now these functions do nothing, like in a-sysdep.c.
   //  Must be fixed when these routines are also fixed in a-sysdep.c. ???
   
   public static void set_binary_mode (int handle) {}
   public static void set_text_mode   (int handle) {}


   //////////////////////////////////
   //////////////////////////////////
   ////                          ////
   ////  Entities in a-tags.adb  ////
   ////                          ////
   //////////////////////////////////
   //////////////////////////////////

   //  function External_Tag (T : Tag) return String;

   public static byte [] external_tag (Object t) {
      String s = ((Class) t).getName ();

      //  Method getName returns a string with a "class " or "interface "
      //  prefix which we have to strip before returning the actual name of
      //  the JVM class. So we just look for the first " " and return the
      //  name after that.

      int space_pos = s.indexOf (' ');
      return to_string (s.substring (space_pos + 1));
   }

   //  function Internal_Tag (External : String) return Tag;

   public static Object internal_tag (byte external []) 
      throws ClassNotFoundException
   {
      return Class.forName (new String (external));
   }

   ////////////////////////////////
   ////////////////////////////////
   ////                        ////
   ////  Entities in a-argv.c  ////
   ////                        ////
   ////////////////////////////////
   ////////////////////////////////

   public static String gnat_argv [] = null;
   public static String command_name = null;

   public static int arg_count ()
   {
      //  We return length+1 to compensate for Ada.Command_Line.Argument_Count,
      //  which subtracts one because of the presence of the command name
      //  argument on non-JVM systems.

      if (gnat_argv == null)
         return 1;
      else
         return gnat_argv.length + 1;
   }

   public static void fill_arg (Object arg, int arg_num)
   {
      byte arg_string [] = (byte []) arg;
      int  k;

      if ((arg_num == 0) && (command_name != null))
         {
            for (k = 0; k < command_name.length (); k++)
               arg_string [k] = (byte) command_name.charAt (k);

            return;
         }

      if ((gnat_argv == null) || (arg_num < 1) || (arg_num > gnat_argv.length))
	 throw new jgnat.adalib.constraint_error ();

      else
         for (k = 0; k < gnat_argv [arg_num - 1].length (); k++)
            arg_string [k] = (byte) gnat_argv [arg_num - 1].charAt (k);
   }

   public static int len_arg (int arg_num)
   {
      if (arg_num == 0)
         if (command_name != null)
            return command_name.length ();
         else
            return 0;
      else
         return gnat_argv [arg_num - 1].length ();
   }

   public static void set_gnat_exit_status (int code)
   {
      //  Do nothing
   }

}
