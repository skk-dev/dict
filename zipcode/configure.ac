dnl Process this file with autoconf to produce a configure script.
AC_INIT()

AC_ARG_WITH(temp_dir,
  [  --with-temp-dir=DIR     directory where temporary files will go],
  temp_dir="$withval", temp_dir=".work")

AC_SUBST(temp_dir)

dnl Checks for programs.
AC_PATH_PROG(rm, rm)
AC_PATH_PROG(cp, cp)
AC_PATH_PROG(mkdir, mkdir)
AC_PATH_PROG(diff, diff)
AC_PATH_PROG(cat, cat)
AC_PATH_PROG(wget, wget)
AC_PATH_PROG(lha, lha)
AC_PATH_PROG(nkf, nkf)
AC_PATH_PROG(skkdic_expr2, skkdic-expr2)

if test x$ac_cv_path_skkdic_expr2 = x; then
   AC_PATH_PROG(skkdic_expr, skkdic-expr)
   AC_PATH_PROG(skkdic_sort, skkdic-sort)
   if test x$ac_cv_path_skkdic_expr = x || test x$ac_cv_path_skkdic_sort = x; then
      echo " "
      echo "Error: SKK Tools are not found." >&2 && exit 1
   fi
fi

AC_ARG_WITH(emacs,
  [  --with-emacs=PROG       file name of Emacs],
  emacs="$withval", emacs="")

if test x$emacs != x ; then
   AC_PATH_PROG(emacs, $emacs)
else
   AC_PATH_PROG(emacs, emacs)
fi

if test x$ac_cv_path_emacs = x; then
   echo " "
   echo "Error: Emacs is not found." >&2 && exit 1
fi

dnl Checks for libraries.

dnl Checks for header files.

dnl Checks for typedefs, structures, and compiler characteristics.

dnl Checks for library functions.

AC_OUTPUT(Makefile)
