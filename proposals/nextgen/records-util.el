;;; records-util.el --- util

;; $Id: records-util.el,v 1.4 2002/04/10 23:56:29 burtonator Exp $

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

(defvar records-created-date-format-string
  "%a %h %d %Y %I:%M %p"
  "Format string used when stamping a new record.")

(defun records-create-todo ()
  "Create a records todo entry in the current record."
  (interactive)

  (save-excursion ;; make sure that we are on a subject
    (records-goto-subject))

  (if (not (bolp))
      (insert "\n"))

  (let (cur-point)
    (insert records-todo-begin-move-regexp "// created on " 
            (records-timestamp) "\n")
    (setq cur-point (point))
    (insert "\n" records-todo-end-regexp)
    (goto-char cur-point)))

(defun records-create-supplemental ()
  "Create a records supplemental entry in the current record."
  (interactive)

  (records-create-marked-region "SUPPLEMENTAL"))
  
(defun records-create-attachment ()
  "Create a records supplemental entry in the current record."
  (interactive)

  (records-create-marked-region "ATTACHMENT"))

(defun records-create-marked-region (name)
  "Used to create supplemental, attachment regions"
  (interactive)
  
  (save-excursion ;; make sure that we are on a subject
    (records-goto-subject))

  (if (not (bolp))
      (insert "\n"))

  (let (cur-point)
    (insert name ": " "// created on "
            (records-timestamp) "\n\n")

    (setq cur-point (point))
    (insert "\n\n" "END_" name "\n")
    (goto-char cur-point)))

(defun records-get-todo (&optional date)
  "Insert the previous record files todo's into the date file.
See the records-todo-.*day variables on when it is automatically invoked."
  (interactive)
  (if date
      (records-goto-record nil date "" nil t nil nil nil)
    (setq date (records-file-to-date)))
  (save-excursion
    (let* ((new-buf (current-buffer))
       (prev-date (records-goto-prev-record-file 1 t t))
       (cur-buf (current-buffer))) ;; this is the prev-date buffer
      (if (null prev-date)
      () ;; nothing to do
    (goto-char (point-min))
    (while (records-goto-down-record nil t) ;; record exists
      ;; start the magic
      (let* ((subject (nth 0 (records-subject-tag t))) ;; first record
                 (point-pair (records-record-region))
                 (bon-point (first point-pair))
                 (eon-point (second point-pair))
         bt-point et-point move subject-inserted)
            (goto-char bon-point)
        ;; process all the todo's in the current record
        (while (re-search-forward records-todo-begin-regexp eon-point 'end)
          ;; do the copy/move thing for the current todo
          (setq bt-point (match-beginning 0))
          (setq move (match-beginning 2))
          ;; goto end of todo
          (if (re-search-forward 
                   (concat "^" records-todo-end-regexp ".*\n") 
                                     eon-point 'end)
          (setq et-point (match-end 0))
        (setq et-point (point)))
          ;; for move, save the regions in the old file
          (if move (setq records-todo-move-regions 
                 (cons (list bt-point et-point)
                   records-todo-move-regions)))
          ;; now copy the region to the new file
          (save-excursion
        (set-buffer new-buf)
        (goto-char (point-max))
        (if (not subject-inserted)
            (progn (records-insert-record subject) 
               (setq subject-inserted t)))
        (insert-buffer-substring cur-buf bt-point et-point)
        ;; insert an extra newline - this is useful for empty records
        (insert "\n")))))
    ;; end of record processing. 
        ;; for todo-move, remove regions from old file
    (let ((prev-modified (buffer-modified-p)))
      (while records-todo-move-regions
        (goto-char (car (car records-todo-move-regions)))
        (apply 'delete-region (car records-todo-move-regions))
        ;; do the records-todo-delete-empty-record
        (if (and records-todo-delete-empty-record (records-body-empty-p))
        (records-delete-record nil t))
        (setq records-todo-move-regions
          (cdr records-todo-move-regions)))
      (if (or prev-modified (not (buffer-modified-p)))
              () ;; don't do anything
            (save-buffer)
            (records-delete-empty-record-file)
            ))))))

(autoload 'mc-encrypt-generic "mc-toplev" nil t)

(defun records-user-name ()
  "The user name of the records user."
  (if (not (boundp 'mc-default-scheme))
      (eval-when-compile (require 'mailcrypt)))
  (let ((user (cdr (assoc 'user-id (funcall mc-default-scheme)))))
    (cond ((boundp 'mc-ripem-user-id)
           mc-ripem-user-id)
          ((not (null user)) user)
          (t (user-full-name)))))

(defun records-encrypt-record (arg)
  "Encrypt the current record for the current user.
With prefix arg, start the encryption from point to the end of record.
Records encryption requires the mailcrypt and mc-pgp (or mc-pgp5) packages."
  (interactive "P")
  (if (not (boundp 'mc-default-scheme))
      (eval-when-compile (require 'mailcrypt)))
  (save-excursion
    (let ((point-pair (records-record-region t))
          start end)
      (if arg (setq start (point))
        (setq start (first point-pair)))
      (setq end (second point-pair))
      (goto-char start)
      ;; sanity check
      (if (or (looking-at 
               (cdr (assoc 'msg-begin-line (funcall mc-default-scheme))))
          (looking-at 
               (cdr (assoc 'signed-begin-line (funcall mc-default-scheme)))))
      (error "records-encrypt-record: record is already encrypted."))
      (mc-encrypt-generic (records-user-name) nil 
                          start end (records-user-name) nil)
      )))

(defun records-decrypt-record ()
  "Decrypt the current record.
Records decryption requires the mailcrypt and mc-pgp (or mc-pgp5) packages."
  (interactive)
  (if (not (boundp 'mc-default-scheme))
      (eval-when-compile (require 'mailcrypt)))
  (save-excursion
    (let ((point-pair (records-record-region t)))
      (goto-char (first point-pair))
      (if (not (re-search-forward
                (concat "\\(" 
                        (cdr (assoc 'msg-begin-line
                                    (funcall mc-default-scheme)))
                        "\\|" 
                        (cdr (assoc 'signed-begin-line
                                    (funcall mc-default-scheme)))
                        "\\)") (mark) t))
          (error "records-decrypt-record: record is not encrypted."))
      (funcall (cdr (assoc 'decryption-func (funcall mc-default-scheme)))
               (match-beginning 0) (second point-pair)))))

(defun records-concatenate-records (num)
  "Concatenate the current record with the records on the same subject written
in the last NUM days. Output these records in the records output buffer (see 
records-output-buffer). Without prefix arg, prompts for number of days.
An empty string will output the current record only. A negative number
will output all the past records on the subject!!"
  (interactive
   (list
    (if current-prefix-arg (int-to-string current-prefix-arg)
      (read-from-minibuffer "Concat records in last N days (default 1): "))))
  (let* ((date (records-file-to-date))
     (subject-tag (records-subject-tag t))
     (subject (nth 0 subject-tag))
     (tag (nth 1 subject-tag))
     (arg (string-to-int num))
     (first-ndate (records-add-date (records-normalize-date date)
                      (if (= arg 0) -1 (- arg))))
     cur-buf point-pair bon-point eon-point prev-date-tag)

    (if (< arg 0)
    (setq first-ndate '(0 0 0)))
    ;; erase output buffer if needed
    ;; print subject
    (save-excursion
      (set-buffer (get-buffer-create records-output-buffer))
      (if records-erase-output-buffer
      (erase-buffer)
    (goto-char (point-max)))
      (insert (records-subject-on-concat subject)))
    ;; start with current record
    (save-excursion
      (while ;; do-while loop 
      (progn
        ;; get the current records's buffer, beg-point and end-point.
        (setq point-pair (records-record-region t))
        (setq cur-buf (buffer-name))
        (setq bon-point (first point-pair))
        (setq eon-point (second point-pair))
            (goto-char bon-point)
        ;; insert the current record into records-output-buffer
        (save-excursion
          (set-buffer (get-buffer records-output-buffer))
          (goto-char (point-max))
          (insert (records-date-on-concat (concat date (records-tag tag))))
          (insert-buffer-substring cur-buf bon-point eon-point))
        ;; goto the previous record
        (setq prev-date-tag (records-goto-prev-record 1 subject date tag t t))
        (setq date (nth 0 prev-date-tag))
        (setq tag (nth 1 prev-date-tag))
        ;; check if this record should be copied
        (and prev-date-tag 
         (records-ndate-lessp first-ndate 
                    (records-normalize-date date))))))
    ;; display/select
    (if records-select-buffer-on-concat
    (pop-to-buffer (get-buffer records-output-buffer))
      (display-buffer (get-buffer records-output-buffer)))))
    
(defun records-concatenate-record-files (num)
  "Concatenate all the records in the records files of the last NUM days. All
the records of a subject are collected together. Output these records in the
records output buffer (see records-output-buffer). Without prefix arg, prompts
for number of days. An empty string will output the records of the current
file."
  (interactive
   (list
    (if current-prefix-arg (int-to-string current-prefix-arg)
      (read-from-minibuffer "Concat records files in last N days (default 1): "
                ))))
  (let* ((date (records-file-to-date))
     (arg (string-to-int num))
     (first-ndate (records-add-date (records-normalize-date date)
                      (if (= arg 0) -1 (- arg))))
     records-subject-list)
    ;; erase output buffer if needed
    (save-excursion
      (set-buffer (get-buffer-create records-output-buffer))
      (if records-erase-output-buffer
      (erase-buffer)
    (goto-char (point-max))))
    (save-excursion
      (while ;; loop thru. all files
      (progn ;; do-while loop 
        ;; goto the beginning of the file
        (goto-char (point-min))
        ;; loop thru. all records in a file
        (while (records-goto-down-record nil t) 
          (let* ((subject (nth 0 (records-subject-tag t)))
             (tag  (nth 1 (records-subject-tag t)))
                     (point-pair (records-record-region t))
             (bon-point (first point-pair))
             (eon-point (second point-pair))
             subject-mark omark record)
        ;; get subject-mark
        (setq subject-mark (assoc subject records-subject-list))
        (if subject-mark
            ()
          (save-excursion
            (set-buffer (get-buffer records-output-buffer))
            ;; make a new marker
            (setq omark (point-max-marker))
            (goto-char omark)
            ;; insert subject header 
            (insert-before-markers (records-subject-on-concat subject))
            (goto-char omark)
            (insert "\n")) ;; this does a lot of the trick for markers
          ;; add subject and new marker to list
          (setq subject-mark (list subject omark))
          (setq records-subject-list
            (append records-subject-list (list subject-mark))))
        (setq record (buffer-substring bon-point eon-point))
        (save-excursion
          (set-buffer (get-buffer records-output-buffer))
          (goto-char (nth 1 subject-mark))
          (insert-before-markers 
           (records-date-on-concat (concat date (records-tag tag))))
          (insert-before-markers record))
        (goto-char eon-point)))
        (setq date (records-goto-prev-record-file 1 t))
        ;; check if this record should be copied
        (and date (records-ndate-lessp first-ndate 
                     (records-normalize-date date))))))
    ;; clean up records-subject-list
    (while records-subject-list
      (set-marker (nth 1 (car records-subject-list)) nil)
      (setq records-subject-list (cdr records-subject-list)))
    ;; display/select
    (if records-select-buffer-on-concat
    (pop-to-buffer (get-buffer records-output-buffer))
      (display-buffer (get-buffer records-output-buffer)))))

;;;###autoload
(defun records-calendar-to-record ()
  "Goto the record file corresponding to the calendar date."
  (interactive)
  (let* ((cdate (calendar-cursor-to-date))
     (ndate (list (nth 1 cdate) (nth 0 cdate) (nth 2 cdate)))
     (date (records-denormalize-date ndate)))
    (records-goto-record nil date nil nil 'other)))

;;;###autoload
(defun records-insert-record-region (beg end)
  "Insert the region in the current buffer into today's record.
Prompts for subject."
  (interactive "r")
  (let ((record-body (buffer-substring beg end)))
    (records-goto-today)
    (goto-char (point-max))
    (records-insert-record nil record-body)))

;;;###autoload
(defun records-insert-record-buffer ()
  "Insert the current buffer into today's record.
Prompts for subject."
  (interactive)
  (records-insert-record-region (point-min) (point-max)))

;; bind the following to some simple key
;;      From Jody Klymak (jklymak@apl.washington.edu)
;; 01/10/2000 Support for url and gnus message id 
;;      From Rob Mihram 
(defun records-insert-link (&optional comment-string)
  "Writes the current buffer file name, url or message id
at the end of today's record and inserts a comment."
  (interactive "scomment: ")
  (require 'url)
  (save-excursion
    (let ((flink 
           (cond ((not (null buffer-file-name));; 1. normal file 
                  buffer-file-name)
                 ((not (null url-current-object));; 2. url page
                  (concat (aref url-current-object 0) "://" 
                          (aref url-current-object 3)
                          (aref url-current-object 5)))
                 ((eq major-mode 'gnus-summary-mode);; 3. gnus page
                  (progn
                    ;; silence byte compilation
                    (eval-when-compile (require 'mail-utils))
                    (eval-when-compile (require 'gnus))
                    (defvar gnus-current-headers)
                    (mail-strip-quoted-names 
                     (mail-header-message-id gnus-current-headers))))
                 (t (error "Couldn't create link.")))))
      (message "%s" flink)
       ;;; now we need to visit the buffer records-goto-today
      (if (one-window-p t) 
      (split-window))
      (other-window 1)
      (records-goto-today)
      (goto-char (point-max))
      (insert "link: " flink "\n")
      (insert "" comment-string "\n")
      (other-window -1))))

(defun records-ispell-current-record()
  "Run ispell over the current record."
  (interactive)

  (let(list start end)

    (setq list (records-record-region))

    (setq start (elt list 0))

    (setq end (elt list 1))

    (ispell-region start end)))

(defun records-timestamp()
  "Generate a timestamp for "
  
  (format-time-string records-created-date-format-string))

(defun records-get-collection(original-list)
  "Given a list of strings, get it as a collection for use with completion."

  (let(collection i value)

    (setq collection '())

    (setq i 0)
    
    (while (< i (length original-list))

      (setq value (elt original-list i))

      (add-to-list 'collection (list value (1+ i)))
      
      (setq i (1+ i)))

    collection))

(defun records-util-browse-url()
  "Browse the URL of this record.  It must have a metainfo entry of 'url'."
  (interactive)

  (let(url)

    (setq url (records-metainfo-get "url"))

    (if url
        (browse-url url)
      (error "Record does not have a metainfo entry of 'url'"))))
  
(defun records-util-find-file()
  "Browse the URL of this record.  It must have a metainfo entry of 'url'."
  (interactive)

  (let(file)

    (setq file (records-metainfo-get "file"))

    (if file
        (find-file file)
      (error "Record does not have a metainfo entry of 'file'"))))

(defun records-util-completing-read-subject()
  "Use completion to read in a subject."

  ;; initializes records-subject-table if required
  (records-index-buffer)  
  
  (completing-read "Subject: " 
                          records-subject-table))
  
(defun records-util-get-filename(date)
  "Given a date, get the filename which holds its records."

  ;;if ndate is a list assume it is an ndate

  (if (listp date)
      (setq date (concat (elt date 2) (elt date 1) (elt date 3))))
  
  (concat (records-directory-path date t) "/" date))

(defun records-util-delete-whitespace-forward(&optional no-newline)
  "Delete all whitespace forward of the current point. "
  (interactive)
  (save-excursion
    
    (let(current-point regexp)
      (if no-newline
          (setq regexp "[^ \t]")
        (setq regexp "[^ \n\t]"))
      (setq current-point (point))
      ;;delete any extra spaces, eols or tabs
      (if (re-search-forward regexp nil t)
          (if (> (match-end 0) current-point)
              (delete-region current-point (1- (match-end 0))))))))

(provide 'records-util)
