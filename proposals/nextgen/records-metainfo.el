;;; records-metainfo.el --- metainfo for records

;; $Id: records-metainfo.el,v 1.1 2001/05/13 02:08:18 burtonator Exp $

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
;; Code for adding metainfo to certain records.

;;; History:
;; 

;;; TODO:

;; - metainfo can be added twice for the same topic.
;;
;; - metainfo can not be deleted :(
;;
;; - when metainfo is updated based on metainfo that is already there, don't
;; delete it but instead update it inline.  If we do change it then we should
;; modify it inline.  This is a major problem because we don't want all the
;; metainfo readjusted evertime we update something.

;;; Code:

(defvar records-metainfo-categories '("url" "title" "file" "status")
  "Known/basic metainfo entries.")

(defun records-metainfo-set(category value)
  "Add metainfo to the current record."
  (interactive
   (list
    (records-metainfo-get-category)
    (read-string "Value: ")))


  ;;now add this as metainfo
  (save-excursion

    (records-goto-subject)

;;    (records-metainfo-delete category)
    
    ;;now insert this metainfo
    
    (let((inhibit-read-only t)
         start
         end)

      (save-excursion
        ;; use the end of the metainfo "-- " value
        (assert (re-search-forward records-format-header-end-string nil t)
                nil "Unable to find previous record.")

        (setq start (match-beginning 0)))
      
      ;;update the record inline if it already exists.
      (if (re-search-forward (concat "^" category ": .*$")  start t)
          (replace-match (concat category ": " value))
        ;;else add it with a new value.
        
        (goto-char start)
        
        (insert (format "%s: " category))
        (insert (format "%s" value))
        (insert "\n")
        
        (setq end (point))
        
        ;;make the newly added text read-only
        
        (put-text-property start end 'read-only t)))))

(defun records-metainfo-delete(category)
  "Delete the given record or do nothing."
  (interactive
   (list
    (records-metainfo-get-category)))
  
  (save-excursion
    
    (records-goto-subject)

    (let(max)

      (setq max (records-format-get-header-end-point))

      (if (re-search-forward (concat "^" category ": .*$") max t)
          (let(start end)

            (setq start (match-beginning 0))

            ;;need to add 1 to the end for \n
            (setq end (1+ (match-end 0)))

            (let((inhibit-read-only t))
              (delete-region start end)))))))
  
(defun records-metainfo-get-category()
  "Use completion to get a category from `records-metainfo-categories'."

  (let (category)
    
    (setq category (completing-read "Category: "
                                    (records-get-collection records-metainfo-categories)))

    ;;if category is a new one they we should add it to the known categories

    (if (not (member category records-metainfo-categories))
        (add-to-list 'records-metainfo-categories category))
    
    category))

(defun records-metainfo-get(category)
  "Get the value of the given category within this record or nil if it does not
exist."

  (save-excursion
    
    (records-goto-subject)

    (let(max)

      (setq max (records-format-get-header-end-point))

      (if (re-search-forward (concat "\\(^" category ": \\)\\(.*$\\)" ) max t)

          (buffer-substring-no-properties (match-beginning 2)
                                          (match-end 2))

        nil))))

(provide 'records-metainfo)

;;; records-metainfo.el ends here
