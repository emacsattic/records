;;; records-diary.el --- 

;; $Id: records-diary.el,v 1.2 2002/05/27 19:12:26 burtonator Exp $

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

;; This package allows the user to create Diary entries and export these to RSS
;; files.  Internally this uses the records-rss package and
;; records-rss-export-record
;; 

;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; Code:

(defvar records-diary-index-file (concat records-rss-export-directory "/diary.rss")
  "File used for RSS index output.")

(defun records-diary-export-current-buffer()
  "Export all diary entries within the current buffer"
  (interactive)
  
  (let((records-rss-index-file records-diary-index-file))

    ;;find the current diary record.
    (when (equal major-mode 'records-mode)

      ;;
      (save-excursion
        (beginning-of-buffer)
        (while (re-search-forward "^\\* \\(.*\\)$" nil t)

          (let((subject nil)
               (buffer nil))

            (setq subject (match-string 1))

            ;;export this diary entry...

            (when (string-equal subject "Diary")

              (let(record-link subject title url description created buffer)

                (setq created (records-metainfo-get "created"))

                (setq title (format "Diary entry for %s" created))

                (setq url "http://relativity.yi.org/rss/diary.shtml")

                (setq record-link (records-metainfo-get "link"))
              
                (setq description (records-format-get-body))

                (setq subject "Diary")

                (setq buffer (find-file-noselect records-rss-index-file))
              
                (records-rss-init-buffer buffer)

                (records-rss-export-record record-link
                                           subject
                                           title
                                           url
                                           created
                                           description
                                           buffer)

                (records-rss-save)))))))))

(add-hook 'after-save-hook (lambda()
                            (records-diary-export-current-buffer)))
(provide 'records-diary)

;;; records-diary.el ends here