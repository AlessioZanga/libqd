#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
define([QD_PATCH_VERSION], 142)
AC_PREREQ(2.59)
AC_INIT(qd, 2.1.QD_PATCH_VERSION, yozo@cs.berkeley.edu)
AC_CONFIG_SRCDIR([src/qd.cpp])

PACKAGE="qd"
MAJOR_VERSION="2"
MINOR_VERSION="1"
PATCH_VERSION="QD_PATCH_VERSION"
VERSION=$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION
AC_DEFINE_UNQUOTED([MAJOR_VERSION],$MAJOR_VERSION,[qd major version number])
AC_DEFINE_UNQUOTED([MINOR_VERSION],$MINOR_VERSION,[qd minor version number])
AC_DEFINE_UNQUOTED([PATCH_VERSION],$PATCH_VERSION,[qd patch number (sub minor version)])
echo "configuring $PACKAGE $VERSION..."

AC_CONFIG_AUX_DIR(config)
AM_INIT_AUTOMAKE([nostdinc])
AM_CONFIG_HEADER([config.h])
AM_CONFIG_HEADER([include/qd/qd_config.h])

AC_CANONICAL_HOST

# Checks for arguments
# --enable-inline
AC_ARG_ENABLE(enable_inline, AC_HELP_STRING(--enable-inline, [inline commonly used functions.  [[default=yes]]]))
AC_MSG_CHECKING([if commonly used function is to be inlined])
if test "$enable_inline" != "no"; then
  enable_inline="yes"
  AC_DEFINE([QD_INLINE], [1], [Define to 1 to inline commonly used functions.])
fi
AC_MSG_RESULT($enable_inline)

# --enable-ieee-add
AC_ARG_ENABLE(enable_ieee_add, AC_HELP_STRING(--enable-ieee-add, [use addition that satisfies IEEE-style error bound instead of Cray-style error bound.  [[default=no]]]))
AC_MSG_CHECKING([if addition with IEEE-style error bound is to be used])
if test "$enable_ieee_add" = "yes"; then
  AC_DEFINE([QD_IEEE_ADD], [1], [Define to 1 to use additions with IEEE-style error bounds.])
else
  enable_ieee_add="no"
fi
AC_MSG_RESULT($enable_ieee_add)

# --enable-sloppy-mul
AC_ARG_ENABLE(enable_sloppy_mul, AC_HELP_STRING(--enable-sloppy-mul, [use fast but slightly inaccurate multiplication.  [[default=yes]]]))
AC_MSG_CHECKING([if sloppy multiplication is to be used])
if test "$enable_sloppy_mul" != "no"; then
  enable_sloppy_mul="yes"
  AC_DEFINE([QD_SLOPPY_MUL], [1], [Define to 1 to use sloppy multiplication (which is faster by slightly inaccurate).])
fi
AC_MSG_RESULT($enable_sloppy_mul)

# --enable-sloppy-div
AC_ARG_ENABLE(enable_sloppy_div, AC_HELP_STRING(--enable-sloppy-div, [use fast but slightly inaccurate division.  [[default=yes]]]))
AC_MSG_CHECKING([if sloppy division is to be used])
if test "$enable_sloppy_div" != "no"; then
  enable_sloppy_div="yes"
  AC_DEFINE([QD_SLOPPY_DIV], [1], [Define to 1 to use sloppy division (which is faster by slightly inaccurate).])
fi
AC_MSG_RESULT($enable_sloppy_div)

# --enable-fma
AC_ARG_ENABLE(enable_fma, AC_HELP_STRING(--enable-fma, [assume the compiler generates  fused multiply-accumulate instructions.  Use this option with care.  [[default=no]]]))
AC_MSG_CHECKING([if multiply-accumulate instruction is to be assumed])
if test "$enable_fma" = "yes"; then
  AC_DEFINE([QD_HAS_FMA], [1], [Define to 1 if your compiler automatically generates fused multiply-accumulate instructions.  Use this with care.])
else
  enable_fma="no"
fi
AC_MSG_RESULT($enable_fma)

# --enable-debug
AC_ARG_ENABLE(enable_debug, AC_HELP_STRING(--enable-debug, [enable debugging code. [[default=no]]]))
AC_MSG_CHECKING([if debugging code is to be enabled])
if test "$enable_debug" = "yes"; then
  AC_DEFINE([QD_DEBUG], [1], [Define to 1 to enable debugging code.])
else
  enable_debug="no"
fi
AC_MSG_RESULT($enable_debug)

# Checks for programs.
AC_LANG(C++)
if test "$CXXFLAGS" = ""; then
  if test "$enable_debug" = "yes"; then
    CXXFLAGS="-O"
  else
    CXXFLAGS="-O2"
  fi
fi
AC_PROG_CXX([xlC_r xlC icpc icc ecc cxx aCC CC cc++ cl FCC KCC RCC c++ g++ gpp])
if test "$CC" = ""; then
  CC="$CXX";
fi
AC_PROG_CC

CXX_PROG_NAME=`basename $CXX`
case $host in
  *-*-aix*)
    case "$CXX_PROG_NAME" in
      xlC*) CXXFLAGS="$CXXFLAGS -qstrict" ;;
    esac
  ;;
  i?86-*-* | k?*-*-* | athlon-*-*)
    case "$CXX_PROG_NAME" in
      icc|icpc) 
        CXXFLAGS="$CXXFLAGS -mp -wd1572 -wd981 -wd1418 -wd1419"
      ;;
    esac
  ;;
  ia64-*-*)
    case "$CXX_PROG_NAME" in
      icc|icpc|ecc) 
        CXXFLAGS="$CXXFLAGS -mp -wd1572 -wd981 -wd1418 -wd1419"
      ;;
    esac
  ;;
  alpha*-*-*)
    case "$CXX_PROG_NAME" in
      cxx) 
        CXXFLAGS="$CXXFLAGS -std strict_ansi -tweak"
      ;;
    esac
  ;;
esac
if test "$GCC" = "yes"; then
  CXXFLAGS="$CXXFLAGS -Wall"
fi
if test "$enable_debug" = "yes"; then
  CXXFLAGS="$CXXFLAGS -g"
fi

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([sys/time.h ieeefp.h])

CONFIGURE_ARGS=$ac_configure_args
AC_SUBST(CONFIGURE_ARGS)

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_HEADER_TIME
AC_STRUCT_TM

# Check for x86 FPU fix
AC_MSG_CHECKING([whether x86 FPU control code is needed])
x86_fix="no"
case "$host" in
  i?86-*-* | k?*-*-* | athlon-*-* | x86_64-*-*)
    AC_DEFINE([X86], [1], [Whether to use x86 fpu fix.])
    x86_fix="yes"
  ;;
esac
AC_MSG_RESULT($x86_fix)
if test "$x86_fix" = "yes"; then
  AC_CHECK_HEADERS([fpu_control.h])
fi

# Checks for library functions.
AC_CHECK_FUNCS([gettimeofday isnan finite copysign])

# check for C++ std includes
AC_DEFINE([QD_HAVE_STD], [1], [Define to 1 if your compiler have the C++ standard include files.])

# Check for fortran-90 compiler
if test "$FCFLAGS" = ""; then
  if test "$enable_debug" = "yes"; then
    FCFLAGS="-O"
  else
    FCFLAGS="-O2"
  fi
fi
AC_ARG_ENABLE(enable_fortran, AC_HELP_STRING(--enable-fortran, [build Fortran 77/90 interfaces [[default=auto]]]))
if test "$enable_fortran" != "no"; then
  AC_LANG_PUSH(Fortran)
  AC_PROG_FC([xlf95 ifort ifc efc f95 fort pgf95 lf95 gfortran xlf90 pgf90 epcf90 xlf f90])
  if test "$FC" != ""; then
    enable_fortran="yes"
    AC_DEFINE([HAVE_FORTRAN], [1], [Define to 1 if Fortran interface is to be compiled.])
    AC_SUBST(FC, "$FC")

    AC_FC_SRCEXT([f90])

    dnl add FCFLAGS_f90 to FCFLAGS until automake supports ac_fc_srcext.
    FCFLAGS="$FCFLAGS $FCFLAGS_f90"

    AC_FC_LIBRARY_LDFLAGS
    AC_FC_FREEFORM

    FC_PROG_NAME=`basename $FC`

    ACX_FC_ETIME
    AC_SUBST(ETIME, $acx_fc_etime)
    ACX_CXX_FC_LIB

    MODINC="-I"
    case $host in
      i?86-*-* | k?*-*-* | athlon-*-*)
        case "$FC_PROG_NAME" in
          ifc|ifort) FCFLAGS="$FCFLAGS -mp" ;;
        esac
      ;;
      *-*-aix*)
        case "$FC_PROG_NAME" in
          xlf*) FCFLAGS="$FCFLAGS -qstrict" ;;
        esac
      ;;
      *-sun-solaris*)
        case "$FC_PROG_NAME" in
          f90|f95) MODINC="-moddir=" ;;
        esac
    esac
    AC_SUBST(MODINC, $MODINC)

    AC_FC_WRAPPERS
    AC_FC_MAIN
  else
    enable_fortran="no"
  fi
  if test "$enable_debug" = "yes"; then
    FCFLAGS="$FCFLAGS -g"
  fi
  AC_LANG_POP(Fortran)
fi
AM_CONDITIONAL(HAVE_FORTRAN, [test "$enable_fortran" = "yes"])
AM_CONDITIONAL(QD_DEBUG, [test "$enable_debug" = "yes"])

# Add libraries
LIBS="$LIBS -lm"

# libtool stuff
AC_DISABLE_SHARED
AC_PROG_LIBTOOL

# Output
AC_CONFIG_FILES([Makefile config/Makefile src/Makefile include/Makefile 
                 tests/Makefile fortran/Makefile docs/Makefile qd-config 
                 fortran/second.f90])
AC_OUTPUT
chmod +x qd-config

echo
echo "Using "
echo "  C++ Compiler      = $CXX"
echo "  C++ Flags         = $CXXFLAGS $DEFS"
if test "$enable_fortran" = "yes"; then
  echo "  F90 Compiler      = $FC"
  echo "  F90 Flags         = $FCFLAGS $FCFLAGS_f90"
  echo "  F90 Libs          = $FCLIBS"
fi
echo "  enable fortran    = $enable_fortran"
echo "  enable inline     = $enable_inline"
echo "  enable fma        = $enable_fma"
echo "  enable x86 fix    = $x86_fix"
echo "  enable ieee add   = $enable_ieee_add"
echo "  enable sloppy mul = $enable_sloppy_mul"
echo "  enable sloppy div = $enable_sloppy_div"

