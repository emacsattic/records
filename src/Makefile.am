
bin_SCRIPTS = recordsadmin
my_lisp_sources = records-macro.el	\
records-dindex.el records-index.el records-search.el		\
records-tex.el records-util.el records-vars.el			\
records.el

dist_lisp_LISP = $(my_lisp_sources) records-autoloads.el records-version.el
dist_pkgdata_DATA = records-templ.tex
EXTRA_DIST = records-pkg.el.in

# list of Lisp files that will not be installed
# noinst_LISP = records-load.el

CLEANFILES = records-pkg.el

MAINTAINERCLEANFILES = Makefile.in records-autoloads.el

records-autoloads.el: $(my_lisp_sources)
	test -s $@ || cp -f $(abs_builddir)/records-autoloads.stub $@; touch -t 197001030000 $@
	@$(EMACS) -Q --batch --eval "(setq generated-autoload-file \"$(abs_builddir)/$@\")" \
	 -f batch-update-autoloads $(abs_builddir)
