;;;
;;; notes.el
;;;
;;; $Id: records.el,v 1.1 1996/11/20 02:34:01 asgoel Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

; $Log: records.el,v $
; Revision 1.1  1996/11/20 02:34:01  asgoel
; Initial revision
;

(require 'notes-misc)
(require 'notes-index)

(defvar notes-subject-table (make-vector 127 0)
  "List of subjects for notes subject completion.
Reloaded by loading the notes-index file.")

(defun notes-read-subject (&optional subject)
  "Read the notes subject to be inserted from the minibuffer.
Completion is possible."
  (interactive (list (completing-read "Notes subject: " notes-subject-table)))
  subject)

(defun notes-make-tag (prev-date-exists next-date-exists prev-tag next-tag)
  "Returns a new tag that depends on the previous and next tags
when a note is being inserted.
The tag is a null value or an integer value (negative or positive) 
converted to a string."
  (let ((prev-int-tag (if (null prev-tag) 0 (string-to-int prev-tag)))
	(next-int-tag (if (null next-tag) 0 (string-to-int next-tag))))
    (cond ((and (null prev-date-exists)
		(null next-date-exists)) nil)
	  ((null next-date-exists)
	   (int-to-string 
	    (notes-mirror-bits 
	     (/ (+ (notes-mirror-bits prev-int-tag)
		   (notes-mirror-bits 1)) 2))))
	  ((null prev-date-exists)
	   (int-to-string 
	    (notes-mirror-bits 
	     (/ (+ (notes-mirror-bits next-int-tag)
		   (notes-mirror-bits -1)) 2))))
	  (t (int-to-string 
	      (notes-mirror-bits 
	       (/ (+ (notes-mirror-bits prev-int-tag)
		     (notes-mirror-bits next-int-tag)) 2)))))))

(defun notes-date-equalp (date-a date-b)
  "Are two dates equal?"
  (equal date-a date-b))

(defun notes-ndate-equalp (ndate-a ndate-b)
  "Are two normalized dates equal?"
  (equal ndate-a ndate-b))

(defun notes-ndate-lessp (ndate-a ndate-b)
  "Returns T if ndate-a is less than ndate-b."
  (or (< (nth 2 ndate-a) (nth 2 ndate-b))
      (< (nth 1 ndate-a) (nth 1 ndate-b))
      (< (nth 0 ndate-a) (nth 0 ndate-b))))

(defmacro notes-subject-regexp (&optional subject)
  "Regexp matching the beginning of a note."
  (` (if (, subject)
	 (concat "^\\* \\(" (, subject) "\\)\n\\-+$")
       "^\\* \\(.*\\)\n\\-+$")))

(defun notes-make-link (subject date tag)
  "Make a notes link."
  (concat "link: <"
	  (notes-directory-path date)
	  date "#" (if tag (concat tag) "")
	  "* " subject ">"))

(defun notes-goto-note (subject date tag &optional no-error)
  "Goto the note with subject and tag on date."
  (let ((file (concat (notes-directory-path date t) date)))
    (find-file file)
    (goto-char (point-min))
    (if (re-search-forward 
	 (concat "link: <.*" date "#" tag "\\* " subject ">") (point-max) t)
	;; found
	(notes-goto-subject)
      ;; goto subject if possible
      (if (notes-goto-down-note subject)
	  (if (null no-error)
	      (error (concat "tag: " tag " not found.")))
	(if (null no-error)
	    (error (concat "subject tag: " subject " " tag " not found.")))))))

(defun notes-goto-subject ()
  "Goto the subject on the current note and return the subject."
  (if (looking-at (notes-subject-regexp))
      ()
    (if (notes-goto-up-note) 
	()
      (error "notes-goto-subject: no subject")))
  (buffer-substring-no-properties (match-beginning 1) (match-end 1)))

(defun notes-subject-tag ()
  "Returns the subject and tag of the note where point is located."
  (save-excursion
    (let ((subject (notes-goto-subject))
	  tag)
      (next-line 2)
      (if (re-search-forward notes-tag-regexp (point-eoln) t)
	  (setq tag (buffer-substring-no-properties 
		     (match-beginning 1) (match-end 1))))
      ;; TODO: should not be required
      (if (equal tag "")
	  (setq tag nil))
      (list subject tag))))

(defun notes-mark-subject ()
  "Put mark at the end of subject in this note and point at beginning.
The note marked is the one that contains point or follows point."
  (if (null (notes-goto-subject))
      (error "notes-mark-subject: no subject"))
  (let ((pt (point)))
    (next-line 2)
    (beginning-of-line)
    (if (looking-at "link: <.*>")
	(progn
	  (next-line 1)
	  (beginning-of-line)))
    (push-mark)
    (goto-char pt)))

(defun notes-make-note (subject date tag &optional note-body)
  "Make a basic note with it's link name." 
  (if (not (bolp))
      (insert "\n"))
  (insert "* " subject "\n")
  (insert-char ?- (+ (length subject) 2))
  (insert (concat "\n" (notes-make-link subject date tag) "\n")))

(defun notes-free-note (&optional keep-body)
  "Remove the current note. 
With arg., keep the body and remove the subject only."
  (save-excursion
    (let* ((pt-mark (if keep-body (notes-mark-subject) (notes-mark-note) ))
	   (pt (nth 0 pt-mark))
	   (mark (nth 1 pt-mark)))
      (kill-region pt mark))))

(defun notes-find-prev-note (subject date tag &optional point no-error)
  "Find the previous note on subject starting from date and tag.
If point is specified, the tag is not used."
  (save-excursion (notes-goto-prev-note subject date tag point no-error)))

(defun notes-find-next-note (subject date tag &optional point no-error)
  "Find the next note on subject starting from date and tag.
If point is specified, the tag is not used."
  (save-excursion (notes-goto-next-note subject date tag point no-error)))

(defun notes-mark-note ()
  "Put mark at end of this node and point at beginning.
The note marked is the one that contains point or follows point."
  (interactive)
  (notes-goto-down-note)
  (push-mark)
  (notes-goto-up-note))

(defun notes-goto-up-note (&optional subject)
  "Go to the beginning of the current note.
If the point is currently at the beginning of a note, go to the note above.
If subject is specified, go up to the beginning of a note with subject."
  (interactive)
  (re-search-backward (notes-subject-regexp subject) (point-min) 'start))

(defun notes-goto-down-note (&optional subject)
  "Go to the beginning of the next note. 
If subject is specified, go down to the beginning of a note with subject."
  (interactive)
  (let ((regexp (notes-subject-regexp subject)))
    (if (looking-at regexp)
	(goto-char (match-end 0)))
    ;; find next note and leave cursor at section beginning
    (if (re-search-forward regexp (point-max) 'end)
	(goto-char (match-beginning  0)))
    ))

(defun notes-goto-prev-note (&optional subject date tag point no-error)
  "Find the previous note on subject starting from date and tag.
If point is specified, goto the previous note on subject 
starting from the current point (tag is ignored)."
  (interactive)
  (if (and point (notes-goto-up-note subject))
      ;; return the date and tag of current subject
      (list date (nth 1 (notes-subject-tag)))
    ;; goto the default
    (if (and subject date)
	()
      ;; initialize subject date and tag
      (let ((subject-tag (notes-subject-tag)))
	(setq subject (nth 0 subject-tag))
	(setq tag (nth 1 subject-tag))
	(setq date (notes-file-to-date))))
    (if (notes-index-goto-subject subject no-error)
	(let ((prev-date-tag 
	       (notes-index-goto-prev-date-tag date tag no-error)))
	  (if (null prev-date-tag)
	      nil
	    (notes-goto-note subject (nth 0 prev-date-tag)
			     (nth 1 prev-date-tag))
	    prev-date-tag))
      nil)))

(defun notes-goto-next-note (&optional subject date tag point no-error)
  "Find the next note on subject starting from date and tag.
If point is specified, goto the next note on subject 
starting from the current point (tag is ignored)."
  (interactive)
  (if (and point (notes-goto-down-note subject))
      ;; return the date and tag of current subject
      (list date (nth 1 (notes-subject-tag)))
    ;; goto the default
    (if (and subject date)
	()
      ;; initialize subject date and tag
      (let ((subject-tag (notes-subject-tag)))
	(setq subject (nth 0 subject-tag))
	(setq tag (nth 1 subject-tag))
	(setq date (notes-file-to-date))))
    (if (notes-index-goto-subject subject no-error)
	(let ((next-date-tag
	       (notes-index-goto-next-date-tag date tag no-error)))
	  (if (null next-date-tag)
	      nil
	    (notes-goto-note subject (nth 0 next-date-tag)
			     (nth 1 next-date-tag))
	    next-date-tag))
      nil)))

(defun notes-insert-note(&optional note-body)
  "Insert a new note for the current date. Asks for the subject."
  (interactive)
  (let* ((subject (call-interactively 'notes-read-subject))
	 (date (notes-file-to-date))
	 (tag nil)
	 ;; find previous date and tag
	 (prev-date-tag (notes-find-prev-note subject date tag (point) t))
	 ;; find next date and tag
	 (next-date-tag (notes-find-next-note subject date tag (point) t))
	 (prev-date (nth 0 prev-date-tag))
	 (prev-tag (nth 1 prev-date-tag)) 
	 (next-date (nth 0 next-date-tag))
	 (next-tag (nth 1 next-date-tag)))

    ;; make the current tag
    (setq tag (notes-make-tag (notes-date-equalp prev-date date)
			      (notes-date-equalp next-date date)
			      prev-tag next-tag))

    ;; add a notes index entry 
    (notes-index-insert-note subject date tag)

    ;; now make the note body
    (notes-make-note subject date tag note-body)))

(defun notes-delete-note (&optional keep-body)
  "Delete the current note for the current date.
With arg, removes the subject only."
  (interactive "P")
  (let* ((date (notes-file-to-date))
	 (subject-tag (notes-subject-tag))
	 (subject (nth 0 subject-tag))
	 (tag (nth 1 subject-tag)))
    
    ;; remove the note subject and optionally the body
    (notes-free-note keep-body)

    ;; remove the notes index entry
    (notes-index-delete-note subject date tag)
    ))
	
(defun notes-rename-note ()
  "Renames the subject of the current note for the current date."
  (interactive "P")
  (notes-delete-note 'keep-body)
  (notes-insert-note))

