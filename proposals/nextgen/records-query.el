;;; FILE.el --- ADD BASIC DESCRIPTION

;; $Id: records-query.el,v 1.8 2001/07/09 04:41:32 burtonator Exp $

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

;;; Features:

;; - Only query within a certain date range by default.
;;
;; - Use a full elisp widgetized buffer.
;;
;; - Specify all or specific subjects via a pulldown.
;;
;; - Sort by date... this will probably be the default.
;;
;; - Ability to query on metainfo.
;;   - specifiy multiple metainfo records.
;;
;; - Subject regexp
;;
;; - dedicated buffer for results:
;;    - should have:
;;        - Created:
;;        - Subject:
;;        - Link:
;;        - Excerpt
;; 

;;; TODO:
;;
;; - Don't use functions that change windows when running queries.
;;
;; - Change the default number of days.
;;
;; - Remove any buffers created by query so that we don't pollute the buffer
;;   list.
;;
;; - We should make links "optionally" hidden.
;; 

;;; History:
;;
;; - Sat May 12 2001 09:05 PM (burton@relativity.yi.org): Maybe get rid of the
;; <link> in the buffer or hide it.  This should be very easy to do..
;;
;; - Sat May 12 2001 09:06 PM (burton@relativity.yi.org): Setup key bindings.
;;


;;; Code:
(defvar records-query-results-buffer-name "*records-query-results*"
  "Buffer name to use for records query results.")

(defvar records-query-default-days 14
  "Number of days by default to run this query on.")


(defvar records-query-width-created 30)

(defvar records-query-width-subject 20)

(defvar records-query-width-link 40)

(define-derived-mode records-query-result-mode fundamental-mode "Result"
  "Major mode for query results."

  
  (toggle-read-only 1)
  (font-lock-mode 1)

  (beginning-of-buffer)
  (forward-line 2))

(defun records-query-exec(subject-regexp header-regexp body-regexp &optional num-days)
  "Run a query across available records and.  The `subject-regexp' param is a
regexp used across the records subject.  The `header-regexp' is a regexp used
across the headers of the a record.  The `body-regexp' param is a regexp used
across the records body.  The `num-days' param is the number of days to run this
query across.  "
  (interactive
   (list
    (read-string "Subject regexp")
    (read-string "Header regexp")
    (read-string "Body regexp")))

  ;;FIXME: this method needs to be shortened.  It should NOT be longer than the screen.  

  ;;FIXME: report the number of days and number of hits this query found after
  ;;it is executed.
  
  (if (null num-days)
      (setq num-days records-query-default-days))

  (records-query-setup-results-buffer)

  (let(subject header body current-day-index date ndate hit-count)
    (catch 'error

      ;;init the hit-count.  This should be the number of records found by this
      ;;query.
      (setq hit-count 0)
      
      ;;date should be a raw date (12052001), ndate should be normalized.  We
      ;;need to make sure these are always synched up.

      (setq date (records-todays-date))
      
      (setq ndate (records-normalize-date date))
      
      (save-window-excursion
        
        ;;start off at 0 and keep going until we hit num-days.
        (setq current-day-index 0)
        
        ;;go to the end of this buffer.
        ;;(records-goto-today)

        ;;the first date should start with today
        
        (set-buffer (find-file-noselect (records-util-get-filename date)))
        
        (end-of-buffer)
        
        (while (< current-day-index num-days)
          (while (re-search-backward "\\(^\\* \\)\\(.*$\\)" nil t)
            
            (let(link created excerpt)

              (setq subject (match-string 2))
              (records-remove-text-properties subject)
              
              (setq link (records-metainfo-get "link"))

              (setq created (records-metainfo-get "created"))

              ;;get the header
              (setq header (records-format-get-header))
              
              ;;now get the body

              (setq body (records-format-get-body))
              
              ;;try to get the title and use that as the excerpt if it is provided
              (setq excerpt (records-metainfo-get "title"))

              ;;now if the excerpt is still null this means there is no title so we should probably guess something.
              (if (null excerpt)
                  (setq excerpt (records-query-get-excerpt body)))
              
              ;;see if we have a match
              (if (records-query-exec--match subject body header subject-regexp
                                             body-regexp header-regexp)
                  (setq hit-count (1+ hit-count)))))

          (setq current-day-index (1+ current-day-index))

          ;;recompute the dates
          (setq ndate (records-add-date ndate -1))
          (setq date (records-denormalize-date ndate))

          (set-buffer (find-file-noselect (records-util-get-filename date)))
          
          ;; need to require format version 1.0.1 in all records files.
          (records-format-require-version "1.0.1")
          
          (end-of-buffer))))
    
    (pop-to-buffer records-query-results-buffer-name)
    (records-query-result-mode)
    
    (message "Query found %i records over the last %i days." hit-count records-query-default-days)))

(defun records-query-exec--match(subject
                                 body
                                 header
                                 subject-regexp
                                 body-regexp
                                 header-regexp)
  "Try to match on the subject, body or header with the given regexps.  If a
match is found add it to the results.  Return true if a hit was found."


  (let((found nil))

    (if (and subject-regexp
             (string-match subject-regexp subject))
        (progn
          (setq found t)
          (records-query-add-result created subject link excerpt)))
    
    (if (and header-regexp
             (string-match header-regexp header))
        (progn
          (setq found t)
          (records-query-add-result created subject link excerpt)))
    
    (if (and body-regexp
             (string-match body-regexp body))
        (progn
          (setq found t)
          (records-query-add-result created subject link excerpt)))

    found))
  
(defun records-query-get-excerpt(string)
  "From a string get an excerpt."

  (let(end max-length excerpt )

    (setq max-length 100)
    
     (if (< (length string)
            max-length)
         (setq end (length string))
       (setq end max-length))

    (setq excerpt (substring string 0 end))

    (replace-in-string excerpt "\n" " ")
    ;;(replace-in-string excerpt "^[ ]*" " ")
    ))

(defun records-query-add-result(created subject link excerpt)
  "Add the given result to the result buffer and format correctly."
  
  (save-excursion
    (set-buffer records-query-results-buffer-name)
    (end-of-buffer)

    ;;format the columns correctly.
    (records-query-insert-column created records-query-width-created)
    (records-query-insert-column subject records-query-width-subject)
    (records-query-insert-column (concat "<" link ">") records-query-width-link t)
    (records-query-insert-column excerpt)
    (insert "\n")))

(defun records-query-setup-results-buffer()
  "Setup the results buffer for any results found."
  
  (save-excursion
    (set-buffer (get-buffer-create records-query-results-buffer-name))
    (toggle-read-only -1)
    
    (erase-buffer)

    ;; now add the header
    (records-query-insert-column "Created" records-query-width-created)
    (records-query-insert-column "Subject" records-query-width-subject)
    ;;(records-query-insert-column "Link" records-query-width-link)
    (records-query-insert-column "Excerpt")
    (insert "\n")

    ;;now insert a bar

    (insert (make-string (+ records-query-width-created
                            records-query-width-subject
                            (length "Excerpt"))
                         ?-))
    
    (insert "\n")))

(defun records-query-insert-column(value &optional width hidden)
  "Insert a formated column into the buffer so that enough spaces are to the
right of `value' to equal `width'."

  (let(start end)

    (setq start (point))
    
    (insert value)
    
    ;;FIXME: this will break if the length of the value is greater than the
    ;;width.  If this happens just insert three spaces.
    (if (and width
             (< (length value) width))
        (insert (make-string (- width (length value) ) ? ))
      (insert "   "))

    (setq end (point))

    (if hidden
        (progn
          (put-text-property start end 'invisible t)
          (put-text-property start end 'intangible t)))))

(defun records-query-results-goto-record()
  "Goto the record on the current line of the results buffer."
  (interactive)


  (let(link base date tag subject)
    (save-excursion
      ;;get the link from the current buffer..
      
      (beginning-of-line)
      
      (assert (re-search-forward "<\\.\\./\\.\\./\\([0-9]+/[0-9]+\\)/\\([0-9]+\\)#\\([0-9]*\\)\\* \\([A-Za-z ]+\\)>" nil t)
              nil "Could not find link on the current line")
      
      (setq link (match-string 0))

      (setq base (match-string 1))

      ;;subject

      ;;date
      (setq date (match-string 2))

      ;;tag
      (setq tag (match-string 3))

      (setq subject (match-string 4))
      
      (records-remove-text-properties link)
      (records-remove-text-properties base))

    (delete-other-windows)
    
    (split-window)
    
    (other-window 1)
    
    (records-goto-record subject date tag)))

;;; specific 'on' queries.

(defun records-query-on-status-todo-working()
  "Query on the todo|working status."
  (interactive)

  (records-query-exec nil "^status: \\(working\\|todo\\)$" nil))

(defun records-query-on-status-completed()
  "Query on the completed status."
  (interactive)
  
  (records-query-exec nil "^status: completed$" nil))

(defun records-query-on-subject(subject)
  "Query on the subject."
  (interactive
   (list
    (records-util-completing-read-subject)))
   
  (records-query-exec subject nil nil))

(defun records-query-on-header(header)
  "Query on the header."
  (interactive
   (list
    (read-string "Header (regexp): ")))

  (records-query-exec nil header nil))

(defun records-query-on-body(body)
  "Query on the body."
  (interactive
   (list
    (read-string "Body (regexp): ")))

  (records-query-exec nil nil body))

(defun records-query-on-type-rss()
  "Query on the body."
  (interactive)

  (records-query-exec nil "^type: rss$" nil))

(defun records-query-set-length(length)
  "Use completion to set the amount of days to run a query across."
  (interactive
   (list 
    (string-to-number (completing-read "Over how many days do you want to run queries? " (list (list "14" 1)
                                                                                               (list "28" 2) 
                                                                                               (list "56" 3)
                                                                                               (list "112" 4))
                                       nil nil (number-to-string records-query-default-days)))))

  (setq records-query-default-days length))


(define-key records-query-result-mode-map  [return] 'records-query-results-goto-record)

;;add keywords for this mode

(font-lock-add-keywords 'records-query-result-mode  '(("^-------+$"
                                                       (0 'font-lock-comment-face append))))

(font-lock-add-keywords 'records-query-result-mode  '(("^Created "
                                                       (0 'font-lock-constant-face append))))

(font-lock-add-keywords 'records-query-result-mode  '((" Subject "
                                                       (0 'font-lock-constant-face append))))

(font-lock-add-keywords 'records-query-result-mode  '((" Link "
                                                       (0 'font-lock-constant-face append))))


(font-lock-add-keywords 'records-query-result-mode  '((" Excerpt$"
                                                       (0 'font-lock-constant-face append))))

(provide 'records-query)

;;; records-query.el ends here
