;;;
;;; notes-util.el
;;;
;;; $Id: records-util.el,v 1.1 1996/12/17 22:37:29 asgoel Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

; $Log: records-util.el,v $
; Revision 1.1  1996/12/17 22:37:29  asgoel
; Initial revision
;

(defun notes-todo (&optional date)
  "Insert the previous note files todo's into the date file.
See the notes-todo-.*day variables on when it is automatically invoked."
  (interactive)
  (if (null date)
      (setq date (notes-file-to-date)))
  (save-excursion
    (let* ((date-buf (current-buffer))
	   (prev-date (notes-goto-prev-note-file 1 t))
	   (cur-buf (current-buffer)))
      (if (null prev-date)
	  () ;; nothing to do
	(goto-char (point-min))
	(while (notes-goto-down-note nil t) ;; note exists
	  ;; start the magic
	  (let* ((subject (nth 0 (notes-subject-tag t))) ;; first note
		 (bon-point (notes-mark-note))
		 (eon-point (mark))
		 bt-point et-point move subject-inserted)
	    ;; process all the todo's in the current note
	    (while (re-search-forward notes-todo-begin-regexp eon-point 'end)
	      ;; do the copy/move thing for the current todo
	      (setq bt-point (match-beginning 0))
	      (setq move (match-beginning 2))
	      ;; goto end of todo
	      (if (re-search-forward notes-todo-end-regexp eon-point 'end)
		  (setq et-point (match-end 0))
		(setq et-point (point)))
	      ;; for move, save the regions in the old file
	      (if move (setq notes-todo-move-regions 
			     (cons (list bt-point et-point)
				   notes-todo-move-regions)))
	      ;; now copy the region to the new file
	      (save-excursion
		(set-buffer date-buf)
		(goto-char (point-max))
		(if (not subject-inserted)
		    (progn (notes-insert-note subject) 
			   (setq subject-inserted t)))
		(insert-buffer-substring cur-buf bt-point et-point)
		;; insert an extra newline - this is useful for empty notes
		(insert "\n")))))
	;; end of note processing. for todo-move, remove regions from old file
	(let ((modified (buffer-modified-p)))
	  (while notes-todo-move-regions
	    (goto-char (car (car notes-todo-move-regions)))
	    (apply 'delete-region (car notes-todo-move-regions))
	    ;; do the notes-todo-delete-empty-note
	    (if (and notes-todo-delete-empty-note (notes-body-empty-p))
		(notes-delete-note nil t))
	    (setq notes-todo-move-regions
		  (cdr notes-todo-move-regions)))
	  (and (not modified) (buffer-modified-p) (save-buffer)))
	))))

(defun notes-user-name ()
  "The user name of the notes user."
  (cond ((boundp 'mc-ripem-user-id)
	 mc-ripem-user-id)
	((boundp 'mc-pgp-user-id)
	 mc-pgp-user-id)
	(t (user-full-name))))

(defun notes-encrypt-note (arg)
  "Encrypt the current note for the current user.
With prefix arg, start the encryption from point to the end of note.
Notes encryption requires the mailcrypt and mc-pgp packages."
  (interactive "P")
  (if (not (fboundp 'mc-pgp-encrypt-region))
      (load "mc-pgp"))
  (save-excursion
    (let ((start (point)))
      (notes-mark-note t)
      (if arg
	  (goto-char start)
	(setq start (point)))
      ;; sanity check
      (if (or (looking-at mc-pgp-msg-begin-line)
	      (looking-at mc-pgp-signed-begin-line))
	  (error "notes-encrypt-note: note is already encrypted."))
      (mc-pgp-encrypt-region (list (notes-user-name)) start (mark)
			  (notes-user-name) nil))))

(defun notes-decrypt-note ()
  "Decrypt the current note.
Notes decryption requires the mailcrypt and mc-pgp packages."
  (interactive)
  (if (not (fboundp 'mc-pgp-decrypt-region))
      (load "mc-pgp"))
  (save-excursion
    (notes-mark-note t)
    (if (not (re-search-forward
	      (concat "\\(" mc-pgp-msg-begin-line "\\|" 
		      mc-pgp-signed-begin-line "\\)") (mark) t))
	(error "notes-decrypt-note: note is not encrypted."))
    (mc-pgp-decrypt-region (match-beginning 0) (mark))))

(defun notes-concatenate-notes (num)
  "Concatenate the current note with the notes on the same subject written
in the last NUM days. Output these notes in the notes output buffer (see 
notes-output-buffer). Without prefix arg, prompts for number of days.
An empty string will output the current note only. A negative number
will output all the past notes on the subject!!"
  (interactive
   (list
    (if current-prefix-arg (int-to-string current-prefix-arg)
      (read-from-minibuffer "Concat notes in last N days (default 1): "))))
  (let* ((date (notes-file-to-date))
	 (subject-tag (notes-subject-tag t))
	 (subject (nth 0 subject-tag))
	 (tag (nth 1 subject-tag))
	 (arg (string-to-int num))
	 (first-ndate (notes-add-date (notes-normalize-date date)
				      (if (= arg 0) -1 (- arg))))
	 cur-buf bon-point eon-point prev-date-tag)

    (if (< arg 0)
	(setq first-ndate '(0 0 0)))
    ;; erase output buffer if needed
    ;; print subject
    (save-excursion
      (set-buffer (get-buffer-create notes-output-buffer))
      (if notes-erase-output-buffer
	  (erase-buffer)
	(goto-char (point-max)))
      (insert (notes-subject-on-concat subject)))
    ;; start with current note
    (save-excursion
      (while ;; do-while loop 
	  (progn
	    ;; get the current notes's buffer, beg-point and end-point.
	    (notes-mark-note t)
	    (setq cur-buf (buffer-name))
	    (setq bon-point (point))
	    (setq eon-point (mark))
	    ;; insert the current note into notes-output-buffer
	    (save-excursion
	      (set-buffer (get-buffer notes-output-buffer))
	      (goto-char (point-max))
	      (insert (notes-date-on-concat (concat date (notes-tag tag))))
	      (insert-buffer-substring cur-buf bon-point eon-point))
	    ;; goto the previous note
	    (setq prev-date-tag (notes-goto-prev-note 1 subject date tag t t))
	    (setq date (nth 0 prev-date-tag))
	    (setq tag (nth 1 prev-date-tag))
	    ;; check if this note should be copied
	    (and prev-date-tag 
		 (notes-ndate-lessp first-ndate 
				    (notes-normalize-date date))))))
    ;; display/select
    (if notes-select-buffer-on-concat
	(pop-to-buffer (get-buffer notes-output-buffer))
      (display-buffer (get-buffer notes-output-buffer)))))
    
(defun notes-concatenate-note-files (num)
  "Concatenate all the notes in the notes files of the last NUM days.
All the notes of a subject are collected together. Output these notes in the
notes output buffer (see notes-output-buffer). Without prefix arg, prompts
for number of days. An empty string will output the notes of the current file."
  (interactive
   (list
    (if current-prefix-arg (int-to-string current-prefix-arg)
      (read-from-minibuffer "Concat notes in last N days (default 1): "))))
  (let* ((date (notes-file-to-date))
	 (arg (string-to-int num))
	 (first-ndate (notes-add-date (notes-normalize-date date)
				      (if (= arg 0) -1 (- arg))))
	 notes-subject-list)
    ;; erase output buffer if needed
    (save-excursion
      (set-buffer (get-buffer-create notes-output-buffer))
      (if notes-erase-output-buffer
	  (erase-buffer)
	(goto-char (point-max))))
    (save-excursion
      (while ;; loop thru. all files
	  (progn ;; do-while loop 
	    ;; goto the beginning of the file
	    (goto-char (point-min))
	    ;; loop thru. all notes in a file
	    (while (notes-goto-down-note nil t) 
	      (let* ((subject (nth 0 (notes-subject-tag t)))
		     (tag  (nth 1 (notes-subject-tag t)))
		     (bon-point (notes-mark-note t))
		     (eon-point (mark))
		     subject-mark omark note)
		;; get subject-mark
		(setq subject-mark (assoc subject notes-subject-list))
		(if subject-mark
		    ()
		  (save-excursion
		    (set-buffer (get-buffer notes-output-buffer))
		    ;; make a new marker
		    (setq omark (point-max-marker))
		    (goto-char omark)
		    ;; insert subject header 
		    (insert-before-markers (notes-subject-on-concat subject))
		    (goto-char omark)
		    (insert "\n")) ;; this does a lot of the trick for markers
		  ;; add subject and new marker to list
		  (setq subject-mark (list subject omark))
		  (setq notes-subject-list
			(append notes-subject-list (list subject-mark))))
		(setq note (buffer-substring bon-point eon-point))
		(save-excursion
		  (set-buffer (get-buffer notes-output-buffer))
		  (goto-char (nth 1 subject-mark))
		  (insert-before-markers 
		   (notes-date-on-concat (concat date (notes-tag tag))))
		  (insert-before-markers note))
		(goto-char eon-point)))
	    (setq date (notes-goto-prev-note-file 1 t))
	    ;; check if this note should be copied
	    (and date (notes-ndate-lessp first-ndate 
					 (notes-normalize-date date))))))
    ;; clean up notes-subject-list
    (while notes-subject-list
      (set-marker (nth 1 (car notes-subject-list)) nil)
      (setq notes-subject-list (cdr notes-subject-list)))
    ;; display/select
    (if notes-select-buffer-on-concat
	(pop-to-buffer (get-buffer notes-output-buffer))
      (display-buffer (get-buffer notes-output-buffer)))))

(provide 'notes-util)
