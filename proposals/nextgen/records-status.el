;;; records-status.el --- record status information

;; $Id: records-status.el,v 1.2 2001/05/14 06:09:21 burtonator Exp $

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
;; Ability to set the status of items within 

;;; History:
;; 

;;; TODO:
;;
;; - we can't just let anyone arbitrarily set the 'status' metainfo value.  Make
;;   this a denied value in that package.

;;; Code:

(defvar records-status-values '("todo" "completed" "working") 
  "Known values for the status of a record.")

(defun records-status-set(value)
  "Set the status for the current record: "
  (interactive
   (list
    (completing-read "Status: " (records-get-collection records-status-values))))

  (records-format-require-version "1.0.1")

  ;;set the new metainfo  
  (records-metainfo-set "status" value))

(defun records-status-get()
  "Get the status for the current record or nil if it isn't set."

  (records-metainfo-get "status"))

(provide 'records-status)

;;; records-status.el ends here
