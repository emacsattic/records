;;; records-hs.el --- Ability to hide/show certain records.

;; $Id: records-hs.el,v 1.1 2001/05/13 02:08:18 burtonator Exp $

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

;; Note that if we set a text property we have to inhibit-read-only.
;;

;;; TODO
;;
;; - need a way to keep hidden records persistent so that they don't ever show
;; up again unless I ask for them
;;
;; - When showing a hidden subject throw an error if it doesn't exist.

(defun records-hs-show-all()
  "Show all records in the current buffer"
  (interactive)

  (records-hs-show-region (point-min) (point-max)))

(defun records-hs-hide-all()
  "Show all records in the current buffer"
  (interactive)

  ;;find the end of the header.
  (save-excursion
    (let(start end)
      (setq start (records-format-end-of-header))
      
      (setq end (point-max))
      
      (records-hs-hide-region start end)))

  (beginning-of-buffer))

(defun records-hs-hide-completed()
  "Hide all records marked completed."
  (interactive)
  
  (save-excursion
    (beginning-of-buffer)

    (while (re-search-forward "^status: completed" nil t)
      
      (records-hs-hide-current)
      (records-goto-down-record))))
  
(defun records-hs-hide-current()
  "Hide the current record."
  (interactive)
  
  (save-excursion
    (records-goto-subject)

    (let(start end)

      (setq start (point))

      (records-goto-down-record)

      (forward-line -1)

      (end-of-line)

      (setq end (1+ (point)))

      (records-hs-hide-region start end))))

(defun records-hs-show-current()
  "Show the current record."
  (interactive)

  (records-hs--current-impl 'show))

(defun records-hs-hide-subject(subject)
  "Prompt for a subject and then hide it."
  (interactive
   (list
    (records-util-completing-read-subject)))

  (save-excursion
    (beginning-of-buffer)

    (while (re-search-forward (records-subject-regexp subject) nil t)

      (forward-line 1)
      
      (records-hs-hide-current)
      (records-goto-down-record))))

(defun records-hs-show-subject(subject)
  "Prompt for a subject and then show it."
  (interactive
   (list
    (records-util-completing-read-subject)))

  (records-hs--subject-impl subject 'show))

  
(defun records-hs-show-region(start end)
  
  (let((inhibit-read-only t))
    (put-text-property start end 'invisible nil)
    (put-text-property start end 'intangible nil))

  ;;need to rerun this because of lazy font-lock
  (font-lock-fontify-buffer))

(defun records-hs-hide-region(start end)
  
  (let((inhibit-read-only t))
    (put-text-property start end 'invisible t)
    (put-text-property start end 'intangible t))

  ;;need to rerun this because of lazy font-lock
  (font-lock-fontify-buffer))


;;; BEGIN IMPL FUNCTIONS
;;
;; provide certain toggle implementations of functions.  These are all generic
;; searches but are given a value of hide|show so what logic can be centralized
;; but the type of operation is separated from it.

(defun records-hs--subject-impl(subject operation)
  "Search for a subject and hide|show it based on the given `operation'."

  (save-excursion
    (beginning-of-buffer)

    (let((search-invisible t))
      (while (re-search-forward (records-subject-regexp subject) nil t)

      (message "FIXME (debug): matched on subject: %s" (match-string 0))

        (if (equal operation 'hide)
            (records-hs-hide-current)
          (records-hs-show-current))
        
        (records-goto-down-record)))))


(defun records-hs--current-impl(operation)
  "Hide or show the current record"

  (records-format-require-version "1.0.1")
  
  (let((search-invisible t)
       start end)
    
    ;;WARNING: since some of this may be invisible we have to use match data.
    
    
    ;;goto the beginning of the subject if necessary
    (let((regexp "^\\* .*"))
      
      ;;PROBLEM: WE ARE GOING OVER OUR SUBJECT!!!

      ;;just show where we are
      
      (message "FIXME (debug): the current line is: %s" (buffer-substring (point-at-bol) (point-at-eol)))
      
      (if (not (looking-at regexp))
          
          (progn
            (assert (re-search-backward regexp nil t)
                    nil "Could not find the beginning of the subject.")
            
            (message "FIXME (debug): found subject: %s" (match-string 0)
                     ))
            
      )))

       (setq start (match-beginning 0))

      ;;find out where the next record ends

      ;;we need to goto the end of this header.
      (re-search-forward records-format-header-end-regexp nil t)

      (if (re-search-forward records-format-header-end-regexp nil t)
          (progn 
            (re-search-backward "^\\* " nil t)
            (setq end (match-beginning 0)))

        (message "FIXME (debug): using max poitn")
        (setq end (point-max)))

;;       ;;now goto this header.
      
;;       (message "FIXME (debug): start == %s, end == %s" start end)

       (if (equal operation 'hide)
           (records-hs-hide-region start end)
         (records-hs-show-region start end)))
  
;;; END IMPL FUNCTIONS

(provide 'records-hs)
