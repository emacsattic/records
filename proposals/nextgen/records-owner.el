;;; records-status.el --- record status information

;; $Id: records-owner.el,v 1.2 2002/04/05 19:37:53 burtonator Exp $

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


;;; History:
;; 

;;; Code:
(defcustom records-owner-name "unknown"
  "Full name of the owner of this set of records."
  :type 'string
  :group 'records-owner)

(defcustom records-owner-email "unknown"
  "Email address of the records owner."
  :type 'string
  :group 'records-owner)

(provide 'records-owner)

;;; records-owner.el ends here
