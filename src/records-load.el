;; records-load.el

;; Copyright (C) 1996 by Ashvin Goel

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(defmacro cons-unique (mem list)
  "Add mem to list if it does not exist in it."
  `(setq ,list
	   (if (member ,mem ,list) list (cons ,mem ,list))))

(eval-when-compile (cons-unique (expand-file-name ".") load-path))
(eval-when-compile (cons-unique (expand-file-name "./mailcrypt") load-path))
