;;;
;;; notes-index.el
;;;
;;; $Id: records-index.el,v 1.1 1996/11/20 02:34:07 asgoel Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

; $Log: records-index.el,v $
; Revision 1.1  1996/11/20 02:34:07  asgoel
; Initial revision
;

(defmacro notes-index-subject-regexp (&optional subject)
  "Regexp matching an index note subject."
  ( `(if (, subject)
	 (concat "^\\(" (, subject) "\\): ")
       "^\\(.*\\): ")))

(defun notes-index-buffer ()
  "Initialize the notes index buffer from the index file.
Create the notes directory and index file if needed."
  (if (and notes-index-buffer
	   (get-buffer notes-index-buffer))
      ()
    (if (file-exists-p (expand-file-name notes-index-file))
	()
      ;; see if notes directory exists
      (if (file-directory-p (expand-file-name notes-directory))
	  ()
	;; create notes directory 
	(make-directory (expand-file-name notes-directory) t))
      ;; initialize notes index file
      (write-region "-*- notes-index -*-" nil notes-index-file))
    ;; now get the index file
    (setq notes-index-buffer (buffer-name 
			      (find-file-noselect notes-index-file)))))

(defun notes-index-goto-subject (subject &optional no-error)
  "Goto the beginning of subject in the index buffer.
If no-error is specified and the subject is not found, 
place point at the beginning of the next subject."
  (notes-index-buffer)
  (set-buffer notes-index-buffer)
  (goto-char (point-min))
  (if (re-search-forward (notes-index-subject-regexp subject) (point-max) t)
      (goto-char (match-beginning 0))
    ;; search linearly until we get the next subject
    ;; really a 'do' loop
    (if (null no-error)
	(error 
	 (concat "notes-index-goto-subject: subject " subject " not found")))
    (while (let (match)
	     (next-line 1)
	     (beginning-of-line)
	     (setq match (looking-at (notes-index-subject-regexp)))
	     (and (string-lessp 
		  (buffer-substring-no-properties
		   (match-beginning 1)
		   (match-end 1)) subject)
		 match)))))

(defun notes-index-goto-date-tag (date tag &optional no-error)
  "Goto the (date, tag) in the index file and return (date, tag).
It is assumed that point is at the beginning of the notes index subject.
If (date, tag) does not exist, raise error unless no-error is true, 
in which case, leave point in the space between dates and return nil."
  (let ((date-tag (concat date (if tag (concat "#" tag))))
	curr-date-tag ndate)
    ;; first check if (date, tag) exists
    (if (re-search-forward date-tag (point-eoln) t)
	;; found
	(progn
	  (goto-char (match-beginning 0))
	  (list date tag))
      ;; (date, tag) not found
      (if (null no-error)
	  (error "notes-index-goto-date-tag: " date " " tag " not found."))
      ;; search linearly and place point between dates
      ;; TODO: figure out which date to place point on exactly
      (setq ndate (notes-normalize-date date))
      (setq curr-date-tag (notes-index-goto-next-date-tag))
      (while (and curr-date-tag
		  (notes-ndate-lessp 
		   (notes-normalize-date (nth 0 curr-date-tag)) ndate))
	(setq curr-date-tag (notes-index-next-date-tag)))
      (if curr-date-tag
	  (backward-char 1))
      nil)))

(defun notes-index-goto-prev-date-tag (&optional date tag no-error)
  "Goto the (date, tag) previous to the argument (date, tag) 
in the index buffer. Return the previous (date, tag),
or nil if the previous (date, tag) don't exist."
  (if date
      (notes-index-goto-date-tag date tag no-error))
  (if (re-search-backward notes-date-tag-regexp (point-boln) t)
      (progn
	(list 
	 ;; date
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 (if (match-beginning 3)
	     ;; tag
	     (buffer-substring-no-properties (match-beginning 3) 
					     (match-end 3))
	   ;; null tag
	   nil))
	)))

(defun notes-index-goto-next-date-tag (&optional date tag no-error)
  "Goto the (date, tag) next to the argument (date, tag)
in the index buffer. Return the next (date, tag),
or nil if the next (date, tag) don't exist."
  (if date
      (notes-index-goto-date-tag date tag no-error))
  (if (looking-at (notes-index-subject-regexp))
      (goto-char (1- (match-end 0))))
  (if (looking-at notes-date-tag-regexp)
      (goto-char (match-end 0)))
  (if (re-search-forward notes-date-tag-regexp (point-eoln) t)
      (progn 
	(goto-char (match-beginning 0))
	(list 
	 ;; date
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 (if (match-beginning 3)
	     ;; tag
	     (buffer-substring-no-properties (match-beginning 3) 
					     (match-end 3))
	   ;; null tag
	   nil)))))

(defun notes-index-insert-subject (subject)
  "Insert a note subject into the notes index."
  (if (looking-at (notes-index-subject-regexp subject))
      ;; no insertion required
      ()
    ;; ask for confirmation
    (if (y-or-n-p (concat "Insert subject: " subject " "))
	(save-excursion (insert (concat subject ": \n"))
			(intern subject notes-subject-table)))))

(defun notes-index-delete-subject (subject)
  (beginning-of-line)
  (if (notes-index-next-date-tag)
      ;; other guys exist. don't do anything
      ()
    ;; make sure that we are removing the correct subject
    (if (not (looking-at (notes-index-subject-regexp subject)))
	(error "notes-index-delete-subject: bad subject")
      ;; ask for confirmation
      (if (y-or-n-p (concat "Delete subject: " subject))
	  ;; the 1+ is for the newline
	  (progn
	    (delete-region (point) (1+ (point-eoln)))
	    (unintern subject notes-subject-table))))))

(defun notes-index-insert-note (subject date tag)
  "Insert a note into the notes index."
  (save-excursion
    (notes-index-goto-subject subject t)
    (notes-index-insert-subject subject)
    (if (notes-index-goto-date-tag date tag t)
	(error (concat "notes-index-insert-note: " date " " tag
		       " already exists."))
      ;; now insert
      (forward-char 1)
      (insert (concat date (if tag (concat "#" tag)) " ")))))

(defun notes-index-delete-note (subject date tag)
  "Delete a note from the notes index."
  (save-excursion
    (notes-index-goto-subject subject)
    (notes-index-goto-date-tag date tag)
    ;; the 1+ is for the extra space
    (delete-region (match-beginning 0) (1+ (match-end 0)))
    (notes-index-delete-subject subject)))


(provide 'notes-index)
