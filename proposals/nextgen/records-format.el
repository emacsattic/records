;;; records-format.el --- functions used for parsing and formating records file.

;; $Id: records-format.el,v 1.4 2001/05/15 14:01:39 burtonator Exp $

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

;;; TODO:
;;
;; - make the records file header read-only

;;; Code:
(defvar records-format-header-end-string "-- " "String used to end a record header.")

(defvar records-format-header-end-regexp "^-- $" "Regexp used to find the end a record header.")

;;; Version history;
;;
;; 1.0.0 - init
;;
;; 1.0.1 - added records-format-header-end-string as the end of header metainfo
;;         marker "-- "
;;
(defvar records-format-version-number "1.0.1" "Version number of this records files format.")

(defun records-format-get-header-end-point()
  "Get the end of the subject header info.  See `records-format-header-end-regexp'"

  (save-excursion
    (records-goto-subject)

    (assert (re-search-forward records-format-header-end-regexp nil t)
            nil "Could not find header end")

    (match-end 0)))

(defun records-format-tag-file()
  "Tag the current file with format information."

  (save-excursion
    (beginning-of-buffer)
    
    ;;make sure that this hasn't already been tagged.
    
    (if (not (re-search-forward "^// records-format-version-number" nil t))
        (progn 
          
          (insert (format "// records-owner-name: %s\n" records-owner-name))
          (insert (format "// records-owner-email: <%s>\n" records-owner-email))
          (insert (format "// records-format-version-number: %s\n" records-format-version-number))
          (insert "\n")))))

(defun records-format-new-file()
  "Invoked when a new records file is created."

  (records-format-tag-file)

  (end-of-buffer))

(defun records-format-get-version()
  "Return the format version number from the current file."

  (save-excursion

    (beginning-of-buffer)

    (if (re-search-forward "\\(^// records-format-version-number: \\)\\(.*$\\)" nil t)
        (progn
          ;;version number found.
          (match-string 2))
      ;;return no version
      "0.0.0")))
  
(defun records-format-require-version(necessary-version)
  "Require that the given version is greater than or equal to the current format
version in the current records file.  "

  (let(current-version)

    (setq current-version (records-format-get-version))

    ;;FIXME: Speed improvement.  We need to cache this variable as a buffer-local
    ;;variable so that it isn't computed everytime.
    
    (if (string-lessp current-version necessary-version)
        (throw 'error (format "The current version is %s but %s is required"
                              current-version
                              necessary-version)))))

(defun records-format-read-only-remove-from-record()
  "Remove the read-only properties from the current record"

  (save-excursion

    (records-goto-subject)
    
    (let(begin end)

      (setq begin (point))

      (setq end (records-format-get-header-end-point))

      (let((inhibit-read-only t))

        (set-text-properties begin end nil)))))

(defun records-format-read-only-header()
  "Make the header of this records file read-only."
  (interactive)
  
  (save-excursion
    (beginning-of-buffer)

    (goto-char (records-format-end-of-header))
    
    (put-text-property (point-min) (match-end 0) 'read-only t)))

(defun records-format-end-of-header()
  "Return the point at the end of the header"

  (records-format-require-version "1.0.1")
  
  (save-excursion
    (beginning-of-buffer)

    (assert (re-search-forward "^// records-format-version-number: .*" nil t)
            nil "Could not find end of header")

    (1+ (match-end 0))))

(defun records-format-get-body()
  "Get the body of the current record as a string."
  
  (save-excursion

    (records-goto-subject)
    
    (let(start end body)

      (assert (re-search-forward records-format-header-end-regexp nil t)
              nil "Could not find end of header")

      (setq start (1+ (match-end 0)))

      (if (re-search-forward (records-subject-regexp) nil t)
          (setq end (match-beginning 0))
        (setq end (point-max)))

      ;;search for the first non-space character braced by the end and then
      ;;adjust start so that it contains no beginning spaces \t or \n

      (goto-char start)
      (if (re-search-forward "[^ \t\n]" end t)
          (setq start (match-beginning 0)))
      
      (setq body (buffer-substring-no-properties start end))

      body)))

(defun records-format-get-header()
  "Get the header of the current record as a string."

  (save-excursion
    (let(start end)

      (records-goto-subject)
      
      (forward-line 1)
      (beginning-of-line)
      
      (setq start (point))
      
      (assert (re-search-forward records-format-header-end-regexp nil t)
              nil "Could not find end of header")
      
      (setq end (match-beginning 0))
      
      (setq header (buffer-substring-no-properties start end))

      header)))
  
  

(provide 'records-format)

;;; records-format.el ends here
