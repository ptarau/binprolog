Welcome to BinProlog Open Source Edition!

First, uncompress using unzip the source files (or use git
to extract them from the repository).

The main directories in this distribution are:

-- TESTED recently on OS X Mountain Lion

+---src ---------------------> BinProlog sources and makefile
+---bin----------------------> executable
+---lib----------------------> *.so *.a libraries
+---doc ---------------------> documentation
+---progs -------------------> sample programs
+---library -----------------> Prolog libraries

-- UNTESTED recently
+---c_inter------------------> high performance C interface
+---pl2c --------------------> Prolog to C translator
+---csocks ------------------> simple C socket interface
+---j_inter------------------> simple JNI Java interface

Look in directory "bin" for ready to run executables. Just copy the 
executable bp (on OS x, Linux) and bp.exe (on Windows) somewhere 
on your path.

The directory "doc" (or "bp_doc" in some distributions) contains the
documentation in PostScript and HTML form. The new API description
is in file help.html. You can regenerate it by just typing "help" in BinProlog.

The documentation has not been recently revised - some things might be outdated.

WITH THE EXCEPTION of the "make" process in src, no recent testing 
has been performed on the following BinProlog EXTENSIONS.

Please read the README.txt files in various directories before staring 
to work with them. 

On a win32 or win64 machine make sure cl.exe is in the path - i.e. run something like
vc32.bat or similar, based on your Visual C installation.

BinProlog's C-interface tools are in directory "c_inter". Type make of
winmake.bat to recompile the files and link with the binary libraries
provided in directory "lib".

Tools for generation of standalone executables, through compilation to C
are available in directory "pl2c".

Header files and static libraries are available in directory "lib" - allowing
to use the C-interface or generate C-code without need to recompile the sources.

Just go in directory src and type make all (for gcc) or makeall.bat 
(for cl.exe - the Visual C compiler). 

The directory "csocks" contains tools for building standalone C-based
socket based client, server and a remote toplevel components 
based on BinProlog's modular and portable socket package.

Enjoy,

Paul Tarau
Nov 12, 2012

