;;;
;;; notes-index.el
;;;
;;; $Id: records-index.el,v 1.9 1996/12/18 16:36:25 asgoel Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

; $Log: records-index.el,v $
; Revision 1.9  1996/12/18 16:36:25  asgoel
; Added autoloads.
; Removed date index functions to separate file.
;
; Revision 1.8  1996/12/15  04:53:41  asgoel
; 1. Removed some todo's.
; 2. Added documentation about return values of functions.
; 3. Added notes-tag macro.
;
; Revision 1.7  1996/12/13  19:58:17  asgoel
; Fixed error messages.
; Added support for notes-goto-last-note.
;
; Revision 1.6  1996/12/10  01:35:30  asgoel
; Fix in notes-index-parse-buffer.
;
; Revision 1.5  1996/12/05  21:23:02  asgoel
; Added date index code.
; Added optional 'modified' parameter to notes-index-buffer,
;   notes-index-goto-subject.
; Added optional 'buf' parameter to notes-index-save-buffer.
; Fixed loading order of notes and notes-index.
;
; Revision 1.4  1996/11/26  02:00:40  asgoel
; Move notes-subject table to notes.el
;
; Revision 1.3  1996/11/22  12:44:00  asgoel
; A working version that supports the old functionality + auto insertion,
; deletion and renaming of notes.
; However it can not be tested since I have to write a perl script
; to change my notes files to the new format. In the process, a script
; that can regenerate the index should also be written.
;
; Revision 1.2  1996/11/21  03:05:27  asgoel
; The first working version.
;
; Revision 1.1  1996/11/20  02:34:07  asgoel
; Initial revision
;

(defvar notes-index-use-font-lock t
  "* Enable notes index fontification.")

(defvar notes-index-font-lock-keywords
  '(("^\\(.*\\): " 1 notes-bold-face))
  "* Font-lock keywords for notes index mode.")

(defvar notes-index-buffer nil
  "The name of the index buffer.
Initialized when the notes index file is loaded.")

(defvar notes-index-mode-map nil
  "Key map for notes index mode.")

(if notes-index-mode-map
    nil
  (setq notes-index-mode-map (make-sparse-keymap))
  (define-key notes-index-mode-map "\r" 'notes-index-goto-link)
  (define-key notes-index-mode-map "\C-c\C-j" 'notes-index-goto-link)
  (define-key notes-index-mode-map [mouse-2] 
    'notes-index-goto-mouse-link))

;;;###autoload
(defun notes-index-mode ()
  "Notes-index-mode with mouse support.
Key bindings are:
\\{notes-index-mode-map}"
  (interactive)
  ;; make read only for user
  (setq buffer-read-only t)
  (notes-index-parse-buffer)
  (setq major-mode 'notes-index-mode mode-name "notes-index")
  (use-local-map notes-index-mode-map)
  (if notes-index-use-font-lock
      (progn
	(require 'font-lock)
	(make-local-variable 'font-lock-no-comments)
	(setq font-lock-no-comments t)
	(make-local-variable 'font-lock-keywords)
	(setq font-lock-keywords notes-index-font-lock-keywords)
	(font-lock-mode 1)))
  (run-hooks 'notes-index-mode-hooks)
  )

(defmacro notes-index-subject-regexp (&optional subject)
  "Regexp matching a subject in the notes index file."
  ( `(if (, subject)
	 (concat "^\\(" (, subject) "\\): ")
       "^\\(.*\\): ")))

(defun notes-index-parse-buffer ()
  "Parses the index buffer for notes subject completion."
  (if (null notes-index-buffer)
      (setq notes-index-buffer (buffer-name (current-buffer))))
  (goto-char (point-min))
  ;; really a 'do' loop
  (while (progn
	   (forward-line 1)
	   (beginning-of-line)
	   (and (looking-at (notes-index-subject-regexp))
		(intern (buffer-substring-no-properties
			 (match-beginning 1) (match-end 1))
			notes-subject-table)
		))))

(defun notes-index-buffer (&optional modified)
  "Initialize the notes index buffer from the index file.
If needed, create the notes directory and index file.
If modified is t, check the file modification time since being visited."
  (if (and notes-index-buffer
	   (get-buffer notes-index-buffer))
      (if (and modified
	       (not (verify-visited-file-modtime
		     (get-buffer notes-index-buffer))))
	  ;; revert buffer since it has been modified under you.
	  (save-excursion
	    (set-buffer notes-index-buffer)
	    (revert-buffer)))
    ;; get the index file
    (if (file-exists-p (expand-file-name notes-index-file))
	()
      ;; see if notes directory exists
      (if (not (file-directory-p (expand-file-name notes-directory)))
	  ;; create notes directory 
	  (make-directory (expand-file-name notes-directory) t))
      ;; initialize notes index file
      (write-region "-*- notes-index -*-" nil notes-index-file))
    ;; now get the index file
    (setq notes-index-buffer (buffer-name 
			      (find-file-noselect notes-index-file)))))

(defun notes-index-save-buffer (&optional buf)
  "Save the notes index buffer."
  ;; TODO: if this function is a no-op, things will still work.
  ;; the index buffers will be read-only, but not saved: modeline "--%*-" 
  ;; If indexes are changed frequently enough, this function ought to just
  ;; mark the index buffers so that they are eventually saved ...
  (let ((buf (if buf buf notes-index-buffer)))
    (if (and buf
	     (get-buffer buf))
	(save-excursion
	  (set-buffer buf)
	  ;; no message would be better
	  (save-buffer)))))

(defun notes-index-goto-subject (subject &optional switch no-error modified)
  "Goto the beginning of subject in the index buffer.
If switch is t, switch to the notes index buffer.
If no-error is t and the subject is not found, then
place point at the beginning of the next subject."
  (notes-index-buffer modified)
  (if switch
      (switch-to-buffer notes-index-buffer t)
    (set-buffer notes-index-buffer))
  (goto-char (point-min))
  (if (re-search-forward (notes-index-subject-regexp subject) (point-max) t)
      (goto-char (match-beginning 0))
    (if (null no-error)
	(error 
	 (concat "notes-index-goto-subject: subject " subject " not found.")))
    ;; search linearly until we get the next subject
    (while (let (match) ;; a do-while loop
	     (forward-line 1)
	     (beginning-of-line)
	     (setq match (looking-at (notes-index-subject-regexp)))
	     (and match 
		  (string-lessp 
		  (buffer-substring-no-properties
		   (match-beginning 1)
		   (match-end 1)) subject))))))

(defun notes-index-goto-date-tag (date tag &optional no-error)
  "Goto the (date, tag) in the index file.
Function assumes that point is at the beginning of the notes index subject.
If no-error is nil, raise error if (date, tag) doesn't exist.
if no-error is t, return nil if (date, tag) doesn't exist and 
place point on the smallest (date, tag) pair greater than (date, tag)."
  ;; first check if (date, tag) exists
  (if (re-search-forward (concat date (notes-tag tag)) (point-eoln) t)
      (progn ;; found
	(goto-char (match-beginning 0))
	(list date tag))
    ;; (date, tag) not found
    (if (null no-error)
	(error "notes-index-goto-date-tag: " date " " tag " not found."))
    ;; search linearly and place point on next date
    (let ((ndate (notes-normalize-date date)))
      (while ;; a do-while loop
	  (let* ((curr-date-tag (notes-index-goto-next-date-tag))
		 (curr-ndate
		  (if curr-date-tag 
		      (notes-normalize-date (nth 0 curr-date-tag)))))
	    (and curr-date-tag
		 (or (notes-ndate-lessp curr-ndate ndate)
		     (and (notes-ndate-equalp curr-ndate ndate)
			  (string-lessp (nth 1 curr-date-tag) tag))))
	    )))))

(defun notes-index-goto-prev-date-tag (&optional arg)
  "Goto the arg^th previous (date, tag) in the index buffer. 
Return the previous (date, tag) 
or nil if the previous (date, tag) doesn't exist."
  (if (re-search-backward notes-date-tag-regexp (point-boln) t arg)
      (progn
	(list 
	 ;; date
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 (if (match-beginning 3)
	     ;; tag
	     (buffer-substring-no-properties (match-beginning 3) 
					     (match-end 3))
	   ;; empty tag
	   ""))
	)))

(defun notes-index-goto-next-date-tag (&optional arg)
  "Goto the arg^th next (date, tag) in the index buffer.
Return the next (date, tag),
or nil if the next (date, tag) doesn't exist."
  (if (looking-at (notes-index-subject-regexp))
      ;; go to the front of the subject 
      (goto-char (match-end 0))
    ;; or else go to the next date
    (if (looking-at notes-date-tag-regexp)
	(goto-char (match-end 0))))
  (if (re-search-forward notes-date-tag-regexp (point-eoln) t arg)
      (progn 
	(goto-char (match-beginning 0))
	(list 
	 ;; date
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 (if (match-beginning 3)
	     ;; tag
	     (buffer-substring-no-properties (match-beginning 3) 
					     (match-end 3))
	   ;; empty tag
	   "")))))

(defun notes-index-goto-relative-date-tag(&optional arg date tag)
  "Invoke notes-index-goto-prev-date-tag or notes-index-goto-next-date-tag
depending on whether arg is negative or positive.
Returns the new (date, tag)."
  (let ((date-tag (if date (notes-index-goto-date-tag date tag t))))
    (cond ((null arg) date-tag)
	  ((= arg 0) date-tag)
	  ((< arg 0)
	   (notes-index-goto-prev-date-tag (- arg)))
	  ((> arg 0)
	   (notes-index-goto-next-date-tag arg)))))

(defun notes-index-goto-link()
  "Go to the link under point in the notes index file."
  (interactive)
  (save-excursion
    (skip-chars-backward "0-9#" (point-boln))
    (let (subject date tag)
    (if (not (looking-at notes-date-tag-regexp))
	;; didn't find a date, tag
	(error "notes-index-goto-link: invalid link.")
      ;; found date and tag. get subject
      (setq date (buffer-substring-no-properties (match-beginning 1)
						 (match-end 1)))
      (setq tag 
	    (if (match-beginning 3) 
		(buffer-substring-no-properties (match-beginning 3) 
						(match-end 3))
	      ""))
      (beginning-of-line)
      (if (not (looking-at (notes-index-subject-regexp)))
	  (error "notes-index-goto-link: subject not found."))
      (setq subject (buffer-substring-no-properties (match-beginning 1)
						    (match-end 1)))
      (notes-goto-note subject date tag t)
      ))))

(defun notes-index-goto-mouse-link(e)
  (interactive "e")
  (mouse-set-point e)
  (notes-index-goto-link))

(defun notes-index-insert-subject (subject)
  "Insert a note subject into the notes index."
  (if (looking-at (notes-index-subject-regexp subject))
      ;; no insertion required
      ()
    (save-excursion (insert (concat subject ": \n"))
		    (intern subject notes-subject-table))))

(defun notes-index-delete-subject (subject)
  (beginning-of-line)
  (if (notes-index-goto-next-date-tag)
      ;; other guys exist. don't do anything
      ()
    ;; make sure that we are removing the correct subject
    (beginning-of-line)
    (if (not (looking-at (notes-index-subject-regexp subject)))
	(error "notes-index-delete-subject: invalide subject.")
      ;; ask for confirmation
      (if (y-or-n-p (concat "Delete subject: " subject " "))
	  ;; the 1+ is for the newline
	  (progn
	    (delete-region (point) (1+ (point-eoln)))
	    (unintern subject notes-subject-table))))))

(defun notes-index-insert-note (subject date tag)
  "Insert a note into the notes index."
  (save-excursion
    (notes-index-goto-subject subject nil t t)
    (setq buffer-read-only nil)
    (notes-index-insert-subject subject)
    (if (notes-index-goto-date-tag date tag t)
	(error (concat "notes-index-insert-note: Note " date " " tag 
		       " exists already."))
      ;; now insert
      (insert (concat date (notes-tag tag) " "))
      (notes-index-save-buffer)
      (setq buffer-read-only t)
      )))

(defun notes-index-delete-note (subject date tag)
  "Delete a note from the notes index."
  (save-excursion
    (notes-index-goto-subject subject nil nil t)
    (notes-index-goto-date-tag date tag)
    ;; the 1+ is for the extra space
    (setq buffer-read-only nil)
    (delete-region (match-beginning 0) (1+ (match-end 0)))
    (notes-index-delete-subject subject)
    (notes-index-save-buffer)
    (setq buffer-read-only t)
    ))

(put 'notes-index 'mode-class 'special)
  
(provide 'notes-index)
(if (not (featurep 'notes))
    (require 'notes))

