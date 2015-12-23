AC_INIT([Eden modules for parallel Haskell], [1.2.0.0], [eden@informatik.uni-marburg.de], [edenmodules])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([edenmodules.cabal])

# figure out GHC to use
AC_ARG_WITH([compiler],
            [AC_HELP_STRING([--with-compiler=ARG],
	                    [Use path ARG for GHC (we only support GHC!)])],
	    [WithGhc="$withval"],
            [if test "$HC" = ""; then
	       AC_PATH_PROG([GHC], [ghc])
	     fi
	     WithGhc="$GHC"])

AC_SUBST([WithGhc])

# we require it to be really GHC
AS_IF([test "$WithGhc" == ""],
      AC_MSG_ERROR([GHC is required for this library.]))

# GHC should be callable
AS_IF([$WithGhc --version],[],
      AC_MSG_ERROR([Your GHC does not work!]))

AC_MSG_CHECKING([for Eden extensions to the GHC runtime system])
# create Haskell which imports primitives
rm -f conftest.*; 
cat > conftest.hs << EOF
{-#  OPTIONS_GHC -XMagicHash #-}
module Edentest where
import GHC.Base(expectData#, connectToPort#, sendData# )
test s = sendData# s
EOF
#try to compile this, using WithGhc:
AS_IF( $WithGhc -c conftest.hs 2>&AS_MESSAGE_LOG_FD ,
      [AC_MSG_RESULT([found]); eden=YES],
      [AC_MSG_RESULT([not found]); eden=NO ]
)

# Build the package as a simulation if the runtime does not support Eden
if test "$eden" = YES
  then AC_MSG_RESULT([Parallel build enabled])
       NOT_PARALLEL=
  else AC_MSG_RESULT([Parallel build disabled-using simulation])
       NOT_PARALLEL=-DNOT_PARALLEL
fi
AC_SUBST([NOT_PARALLEL])

# this creates the buildinfo file from buildinfo.in
AC_CONFIG_FILES([edenmodules.buildinfo])
AC_OUTPUT
