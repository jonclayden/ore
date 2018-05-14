dnl Process this file with autoconf to produce a configure script.
AC_INIT(onigmo, 6.1.3)

AC_CONFIG_HEADER(src/onig/config.h)

dnl default value for STATISTICS
STATISTICS=""
AC_ARG_WITH(statistics,
	[  --with-statistics       take matching time statistical data],
	[ STATISTICS=-DONIG_DEBUG_STATISTICS ])
AC_SUBST(STATISTICS)

dnl check for COMBINATION_EXPLOSION
AC_ARG_ENABLE(combination-explosion-check,
	[  --enable-combination-explosion-check   enable combination explosion check],
	[comb_expl_check=$enableval])
if test "${comb_expl_check}" = yes; then
    AC_DEFINE(USE_COMBINATION_EXPLOSION_CHECK,1,[Define if combination explosion check])
fi

dnl check for CRNL_AS_LINE_TERMINATOR
AC_ARG_ENABLE(crnl-as-line-terminator,
	[  --enable-crnl-as-line-terminator   enable CR+NL as line terminator],
	[crnl_as_line_terminator=$enableval])
if test "${crnl_as_line_terminator}" = yes; then
    AC_DEFINE(USE_CRNL_AS_LINE_TERMINATOR,1,[Define if enable CR+NL as line terminator])
fi

dnl check for TOKEN_THREADED_VM
AC_ARG_ENABLE(token-threaded-vm,
    [  --enable-token-threaded-vm   enable token-threaded VM (requires GCC)],
    [token_threaded_vm=$enableval])
if test ! "${token_threaded_vm}" = yes; then
    AC_DEFINE(USE_TOKEN_THREADED_VM,0,[Define if token-threaded VM required])
fi

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(sys/time.h unistd.h sys/times.h stdint.h)

dnl Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_SIZEOF(int, 4)
AC_CHECK_SIZEOF(short, 2)
AC_CHECK_SIZEOF(long, 4)
AC_CHECK_SIZEOF([void*])
AC_CHECK_SIZEOF([long long])
AC_C_CONST
AC_HEADER_TIME

dnl Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_MEMCMP


AC_OUTPUT
