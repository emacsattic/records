## Emacs Makefile for RECORDS
# @configure_input@

# Copyright (C) 2009 by Xavier Maillard <xma@gnu.org>

# Author: Xavier Maillard
# Keywords: processes

# This file is not part of GNU Emacs.

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.

# This program is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the GNU General Public License for more details.

# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
# MA 02111-1307 USA

# require 1.9
AUTOMAKE_OPTIONS = 1.9 gnu

SUBDIRS = src doc

EXTRA_DIST = FAQ INSTALL-NT INSTALL-UNIX bootstrap debian

MAINTAINERCLEANfILES = Makefile.in aclocal.m4 \
	config.guess config.sub configure \
	install-sh missing

# Our special targets
autoloads: src/Makefile
	(cd src ; $(MAKE) records-autoloads.el)

docs: src/Makefile.in
	(cd doc ; $(MAKE) info html)

# Courtesy of Tramp authors. Thank you.
release:
	emacs -nw ChangeLog --eval \
	"(progn (add-change-log-entry nil nil nil t) \
	(backward-delete-char 2) \
	(insert \"Version @PACKAGE_VERSION@ released.\") \
	(save-buffer) (kill-emacs))";
