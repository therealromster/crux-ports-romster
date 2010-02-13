#
#  acrux @ progetto crux italia - http://www.crux-it.org/
#
#
SHELL = /bin/sh
#  The machine (platform) identifier to append to the library names
PLAT = _linux
#  Modify the FORTRAN and OPTS definitions to refer to the
#  compiler and desired compiler options for your machine.  NOOPT
#  refers to the compiler options desired when NO OPTIMIZATION is
#  selected.  Define LOADER and LOADOPTS to refer to the loader and 
#  desired load options for your machine.
FORTRAN  = g77
#OPTS     = -funroll-all-loops -fno-f2c -O3 -march=i686 -ffast-math
OPTS     = -funroll-all-loops -O3 -march=i686 -ffast-math $(CFLAGS)
DRVOPTS  = $(OPTS)
NOOPT    = 
LOADER   = g77
LOADOPTS = $(OPTS)
#  The archiver and the flag(s) to use when building archive (library)
#  If you system has no ranlib, set RANLIB = echo.
ARCH     = ar
ARCHFLAGS= cr
RANLIB   = ranlib
#  The location of the libraries to which you will link.  (The 
#  machine-specific, optimized BLAS library should be used whenever
#  possible.)
BLASLIB      = ../../blas$(PLAT).a
LAPACKLIB    = lapack$(PLAT).a
TMGLIB       = tmglib$(PLAT).a
EIGSRCLIB    = eigsrc$(PLAT).a
LINSRCLIB    = linsrc$(PLAT).a
