dnl
dnl Autoconf script for records
dnl

AC_PREREQ(2.53)

dnl Remember to update the version
AC_INIT(records, 1.5.0)
AM_INIT_AUTOMAKE

dnl for cygwin windows environment
AC_PATH_PROGS(CYGPATH, cygpath)
AC_PATH_PROGS(PERL, perl perl5)

AC_ARG_ENABLE(xemacs,
		AC_HELP_STRING([--enable-xemacs],
				[Use xemacs for compilation]),
		[ test "$enableval" != "no" && EMACS=xemacs ])

AM_PATH_LISPDIR

dnl AC_CONFIG_FILES(Makefile records.el records-vars.el)
dnl AC_CONFIG_FILES(recordsadmin, [chmod +x recordsadmin])

AC_CONFIG_FILES(Makefile records.el)
AC_OUTPUT
