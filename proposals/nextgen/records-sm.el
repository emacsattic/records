;; $Id: records-sm.el,v 1.1 2001/05/26 23:13:34 burtonator Exp $

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
;; The records-sm package keeps "Subject MetaInfo" so that applications can keep
;; track of data regarding subjects.  This is used so that we can keep track of
;; descriptions, 


(defun records-sm-set-image(subject image)
  "Set the image for the given subject"
  (interactive
   (let(subject url default-url)

     (setq subject (records-util-completing-read-subject))

     (setq default-url (cdr (assoc subject
                                   records-rss-images-subjects-alist)))

     (if (null default-url)
         (setq default-url records-rss-images-default))
     
     (setq url (read-string "Image URL: " default-url))

     (list subject url)))

  ;;delete the current entry
  (setq records-rss-images-subjects-alist
        (delete (assoc subject records-rss-images-subjects-alist)
                records-rss-images-subjects-alist))

  (add-to-list 'records-rss-images-subjects-alist (cons subject image))

  (customize-save-variable 'records-rss-images-subjects-alist
                           records-rss-images-subjects-alist))

(provide 'records-sm)
