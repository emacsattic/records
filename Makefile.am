##############################################################################
#
#  File:		Makefile.am
#  Description:		Automake input file for records.el package
#
##############################################################################

bin_SCRIPTS = recordsadmin
info_TEXINFOS = records.texi
pkgdata_DATA = records-templ.tex

MAILCRYPT =					\
	mailcrypt/FSF-timer.el			\
	mailcrypt/expect.el			\
	mailcrypt/mailcrypt.el			\
	mailcrypt/mc-gpg.el			\
	mailcrypt/mc-pgp.el			\
	mailcrypt/mc-pgp5.el			\
	mailcrypt/mc-remail.el			\
	mailcrypt/mc-setversion.el		\
	mailcrypt/mc-toplev.el

# list of Lisp files to be installed in lispdir
lisp_LISP =					\
	$(MAILCRYPT)				\
	records-autoloads.el			\
	records-dindex.el			\
	records-index.el			\
	records-search.el			\
	records-tex.el				\
	records-util.el				\
	records-vars.el				\
	records.el

# list of Lisp files that will not be installed
noinst_LISP = records-load.el $(MAILCRYPT)

# need to explicitly set this so records-load is not byte-compiled
ELCFILES =					\
	records-autoloads.elc			\
	records-dindex.elc			\
	records-index.elc			\
	records-search.elc			\
	records-tex.elc				\
	records-util.elc			\
	records-vars.elc			\
	records.elc

# list of things to distribute that are not picked up automatically
EXTRA_DIST =					\
	FAQ					\
	INSTALL-UNIX				\
	INSTALL-NT				\
	$(MAILCRYPT)				\
	records-templ.tex			\
	records-autoloads.el			\
	records-dindex.el			\
	records-index.el			\
	records-search.el			\
	records-tex.el				\
	records-util.el				\
	records-load.el

# files to remove in 'make clean'
CLEANFILES =					\
	records.aux				\
	records.cp				\
	records.cps				\
	records.dvi				\
	records.fn				\
	records.pg				\
	records.ky				\
	records.kys				\
	records.toc				\
	records.tp				\
	records.vr				\
	records.vrs				\
	records.log

# files to remove in 'make distclean' (+CLEANFILES)
DISTCLEANFILES =				\
	*~					\
	records-vars.el				\
	recordsadmin				\
	records.ps				\
	Makefile				\
	config.status				\
	config.cache				\
	config.log

# files to remove in 'make maintainer-clean' (+DISTCLEANFILES)
MAINTAINERCLEANFILES =				\
	*.tar.gz				\
	Makefile.in				\
	records.info				\
	aclocal.m4				\
	configure

records-vars.el: records-vars.el.in
	rm -f $@
	sed -e 's,@pkgdatadir\@,$(pkgdatadir),' $< > $@

recordsadmin: recordsadmin.in
	rm -f $@
	sed -e 's,@lispdir\@,$(lispdir),' \
		-e 's,@CYGPATH\@,$(CYGPATH),' \
		-e 's,@PERL\@,$(PERL),' $< > $@