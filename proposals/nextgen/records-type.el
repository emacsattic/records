;;; records-type.el --- record type information

;; $Id: records-type.el,v 1.2 2001/05/15 14:01:39 burtonator Exp $

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (burton@openprivacy.org)
;; Copyright (C) 1996 by Ashvin Goel

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Ability to set the type of items within 

;;; History:
;; 

;;; TODO:
;;
;; - we can't just let anyone arbitrarily set the 'type' metainfo value.  Make
;;   this a denied value in that package.

;;; Code:

(defvar records-type-values '("rss" "log" "definition" "bug" "rfe") 
  "Known values for the type of a record.")

(defun records-type-set(value)
  "Set the type for the current record: "
  (interactive
   (list
    (completing-read "Type: " (records-get-collection records-type-values))))

  (records-format-require-version "1.0.1")

  ;;set the new metainfo  
  (records-metainfo-set "type" value))

(defun records-type-get()
  "Get the type for the current record or nil if it isn't set."

  (records-format-require-version "1.0.1")

  (records-metainfo-get "type"))

(provide 'records-type)

;;; records-type.el ends here
