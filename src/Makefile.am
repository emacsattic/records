#ACLOCAL_AMFLAGS = -I m4
AUTOMAKE_OPTIONS = 1.9

##############################################################################
#
#  File:		Makefile.am
#  Description:		Automake input file for records.el package
#
##############################################################################

bin_SCRIPTS = recordsadmin
#info_TEXINFOS = records.texi
pkgdata_DATA = records-templ.tex
#SUBDIRS = src doc
my_lisp_sources = mailcrypt/FSF-timer.el mailcrypt/expect.el 	\
mailcrypt/mailcrypt.el mailcrypt/mc-gpg.el			\
mailcrypt/mc-pgp.el mailcrypt/mc-pgp5.el			\
mailcrypt/mc-remail.el mailcrypt/mc-setversion.el		\
mailcrypt/mc-toplev.el 	records-autoloads.el			\
records-dindex.el records-index.el records-search.el		\
records-tex.el records-util.el records-vars.el			\
records.el

dist_lisp_LISP = $(my_lisp_sources) records-autoloads.el records-version.el
EXTRA_DIST = records-pkg.el.in
MAINTAINERCLEANFILES = records-autoloads.el

# list of Lisp files that will not be installed
noinst_LISP = records-load.el $(MAILCRYPT)


# # list of things to distribute that are not picked up automatically
# EXTRA_DIST =					\
# 	FAQ					\
# 	INSTALL-UNIX				\
# 	INSTALL-NT				\
# 	$(MAILCRYPT)				\
# 	records-templ.tex			\
# 	records-autoloads.el			\
# 	records-dindex.el			\
# 	records-index.el			\
# 	records-search.el			\
# 	records-tex.el				\
# 	records-util.el				\
# 	records-load.el
# 
# files to remove in 'make clean'
# CLEANFILES =					\
# 	*.elc					\
# 	records.aux				\
# 	records.cp				\
# 	records.cps				\
# 	records.dvi				\
# 	records.fn				\
# 	records.pg				\
# 	records.ky				\
# 	records.kys				\
# 	records.toc				\
# 	records.tp				\
# 	records.vr				\
# 	records.vrs				\
# 	records.log

CLEANFILES = records-pkg.el

# # files to remove in 'make distclean' (+CLEANFILES)
# DISTCLEANFILES =				\
# 	*~					\
# 	records-vars.el				\
# 	recordsadmin				\
# 	records.ps				\
# 	Makefile				\
# 	config.status				\
# 	config.cache				\
# 	config.log

# # files to remove in 'make maintainer-clean' (+DISTCLEANFILES)
# MAINTAINERCLEANFILES =				\
# 	*.tar.gz				\
# 	Makefile.in				\
# 	records.info				\
# 	aclocal.m4				\
# 	configure

# records-vars.el: records-vars.el.in
# 	rm -f $@
# 	sed -e 's,@pkgdatadir\@,$(pkgdatadir),' $< > $@

# recordsadmin: recordsadmin.in
# 	rm -f $@
# 	sed -e 's,@lispdir\@,$(lispdir),' \
# 		-e 's,@CYGPATH\@,$(CYGPATH),' \
# 		-e 's,@PERL\@,$(PERL),' $< > $@
