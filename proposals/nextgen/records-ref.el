;; $Id: records-ref.el,v 1.1 2001/05/26 23:13:34 burtonator Exp $

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (burton@openprivacy.org)

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: 
;; Version: 1.0.0

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
;; Contains code for generating record URIs

;;; Format:
;;
;; Record URI based on date:
;;
;;     record:///date/14052001/tag/2/subject/Records
;;
;; Record URI based on subject:
;;
;;     record:///subject/Records
;;
;;     - This could show metainfo for the "Records" subject
;;
;; Records URI based on query:
;;
;;     record://query/subject/Records
;;
;;     - Runs a query on the "Records" subject.


(defun records-ref-get-record-uri(date tag subject)
  "Get a URI capable of jumping to a "

  (let(uri)

    (setq uri (concat "record:///date/" date "/subject/" subject))
    
    (if (and tag
             (not (string-equal tag "")))
        (setq uri (concat uri "/tag/" tag)))

    uri))

(defun records-ref-goto-record-uri(uri)
  "Given a URI obtained by `records-ref-get-record-uri', jump to it."

  (let(subject date tag)
  
    ;;use records-goto-record
  
    ))


(provide 'records-ref)
