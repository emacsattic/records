;;; records-calendar.el --- Calendar integration for records.

;; $Id: records-calendar.el,v 1.1 2001/05/13 02:08:18 burtonator Exp $

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
;; Calendar integration for records.

;;; History:
;; 

;;; Code:
(defvar records-calendar-overlay nil "Overlay used to highlight the current date.")

(defvar records-calendar-buffer-name "*Calendar*" "Name of buffer used with the calendar.")

(defvar records-calendar-show-auto t "If true show the calendar when we enter records.")

(defun records-show-calendar-auto()
  "If the variable `records-calendar-show-auto' is true then the calendar will
be shown. "

  (if records-calendar-show-auto
      (records-show-calendar)))
  

(defun records-show-calendar ()
  "Show the calendar date in the current record file."
  (interactive)

  (let* ((date (records-file-to-date))
     (ndate (records-normalize-date date))
     ;; convert normalized date to calendar date
     ;; the day and month are interchanged
     (cdate (list (nth 1 ndate) (nth 0 ndate) (nth 2 ndate))))
    (eval-when-compile (require 'calendar))
    (calendar)
    (calendar-goto-date cdate))

  (records-highlight-calendar)
  
  (other-window -1))

(defun records-highlight-calendar()

  (save-excursion

    (set-buffer records-calendar-buffer-name)
    
    ;;delete any overlays that already exist.
    (if records-calendar-overlay
        (delete-overlay records-calendar-overlay))

    (let(start end start-bound end-bound)

      (setq start-bound (- 5 (point)))
      (setq end-bound (+ 5 (point)))

      
      (assert (re-search-backward " " start-bound t)
              nil "Could not find the beginning of the date.")

      (setq start (match-beginning 0))

      (setq end (+ start 3))
      
      (setq records-calendar-overlay (make-extent start end))

      (set-extent-property records-calendar-overlay 'face 'modeline)

      (set-extent-property records-calendar-overlay 'buffer (current-buffer))
      
      (set-extent-property records-calendar-overlay 'window (selected-window)))))

(provide 'records-calendar)

;;; records-calendar.el ends here
