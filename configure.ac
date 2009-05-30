dnl
dnl Autoconf script for records
dnl

dnl Need this version
AC_PREREQ(2.54)

dnl Remember to update the version
AC_INIT([records], [1.5.1],[http://launchpad.net/~emacs+records],[records])

dnl Initialize automake, requiring version 1.9 or better,
dnl using moder tar format and generate both bzip tarballs,
dnl zip archive and gzip tarballs
AM_INIT_AUTOMAKE([tar-ustar dist-bzip2 dist-zip 1.9])

dnl for cygwin windows environment
AC_PATH_PROGS(CYGPATH, cygpath)
AC_PATH_PROGS(PERL, perl perl5)
AC_CHECK_PROGS(EGREP, egrep grep)
AC_ARG_ENABLE(xemacs,
		AC_HELP_STRING([--enable-xemacs],
				[Use xemacs for compilation]),
		[ test "$enableval" != "no" && EMACS=xemacs ])

AM_PATH_LISPDIR
AC_PATH_INFO_DIR
dnl AC_CHECK_ELISP(mailcrypt)
dnl AC_CONFIG_FILES(Makefile records.el records-vars.el)
dnl AC_CONFIG_FILES(recordsadmin, [chmod +x recordsadmin])
AC_SUBST(pkgdatadir)
AC_CONFIG_FILES(Makefile src/Makefile doc/Makefile src/records.el src/records-version.el src/records-vars.el)
AC_CONFIG_FILES(src/recordsadmin, [chmod +x recordsadmin])
AC_OUTPUT
