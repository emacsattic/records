;;;
;;; notes-dindex.el
;;;
;;; $Id: records-dindex.el,v 1.1 1996/12/17 18:59:40 asgoel Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

; $Log: records-dindex.el,v $
; Revision 1.1  1996/12/17 18:59:40  asgoel
; Initial revision
;

(defvar notes-dindex-buffer nil
  "The name of the date-index buffer.
Initialized when the notes date index file is loaded.")

(defun notes-dindex-buffer (&optional modified)
  "Initialize the notes date-index buffer from the date-index file.
If needed, create the date-index file.
If modified is t, check the file modification time since being visited."
  (if (and notes-dindex-buffer
	   (get-buffer notes-dindex-buffer))
      (if (and modified
	       (not (verify-visited-file-modtime 
		     (get-buffer notes-dindex-buffer))))
	  ;; revert buffer since it has been modified under you.
	  (save-excursion
	    (set-buffer notes-dindex-buffer)
	    (revert-buffer)))
    ;; get the dindex file
    (setq notes-dindex-buffer (buffer-name 
			       (find-file-noselect notes-dindex-file)))
    (save-excursion 
      (set-buffer notes-dindex-buffer)
      (setq buffer-read-only t))))

(defun notes-dindex-save-buffer ()
  "Save the notes date-index buffer."
  (notes-index-save-buffer notes-dindex-buffer))

(defun notes-dindex-goto-date (date &optional no-error modified)
  "Goto the date in the date-index file.
If no-error is nil, raise error if date doesn't exist.
if no-error is t, return nil if (date, tag) doesn't exist and 
place point on the smallest date greater than the argument date."
  (notes-dindex-buffer modified)
  (set-buffer notes-dindex-buffer)
  (goto-char (point-max))  
  ;; first check if date exists
  ;; start from end since we assume that
  ;; the last dates are most frequently used. 
  (if (re-search-backward (notes-date-count-regexp date) (point-min) t)
      ;; found
      (progn
	(goto-char (match-beginning 0))
	(list 
	 ;; date
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 ;; count
	 (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
	)
    ;; date not found
    (if (null no-error)
	(error "notes-dindex-goto-date: date " date " not found."))
    ;; search linearly and place point on next date
    ;; search is done from end because we assume that
    ;; the last dates are most frequently used. 
    (let ((ndate (notes-normalize-date date)))
      (while ;; a do-while loop
	  (let* ((curr-date-count (notes-dindex-goto-prev-date))
		 (curr-ndate
		  (if curr-date-count
		      (notes-normalize-date (nth 0 curr-date-count)))))
	    (and curr-date-count
		 (notes-ndate-lessp ndate curr-ndate))))
      (notes-dindex-goto-next-date)
      nil)))

(defun notes-dindex-goto-prev-date (&optional arg)
  "Goto the arg^th previous date in the date-index buffer. 
Return the previous date or nil if the previous date doesn't exist."
  (if (re-search-backward (notes-date-count-regexp) (point-min) t arg)
      ;; found
      (progn
	(list 
	 ;; date
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 ;; count
	 (buffer-substring-no-properties (match-beginning 2) (match-end 2))))))

(defun notes-dindex-goto-next-date (&optional arg)
  "Goto the arg^th next date in the date-index buffer. 
Return the next date or nil if the next date doesn't exist."
    ;; or else go to the next date
  (if (looking-at (notes-date-count-regexp))
      (goto-char (match-end 0)))
  (if (re-search-forward (notes-date-count-regexp) (point-max) t arg)
      (progn 
	(goto-char (match-beginning 0))
	(list 
	 ;; date
	 (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	 ;; count
	 (buffer-substring-no-properties (match-beginning 2) (match-end 2))))))

(defun notes-dindex-goto-relative-date(&optional arg date)
  "Invoke notes-dindex-goto-prev-date or notes-dindex-goto-next-date
depending on whether arg is negative or positive."
  (let ((date-count (if date (notes-dindex-goto-date date t))))
    (cond ((null arg) date-count)
	  ((= arg 0) date-count)
	  ((< arg 0)
	   (notes-dindex-goto-prev-date (- arg)))
	  ((> arg 0)
	   (notes-dindex-goto-next-date arg)))))

(defun notes-dindex-insert-note (date)
  "Insert a date into the notes date-index."
  (save-excursion
    (let ((date-count (notes-dindex-goto-date date t t)))
      (setq buffer-read-only nil)
      (if date-count
	  ;; date already exists, bump count
	  (progn
	    (goto-char (match-beginning 2))
	    (delete-region (match-beginning 2) (match-end 2))
	    (insert (int-to-string (1+ (string-to-int (nth 1 date-count))))))
	;; insert date + zero count
	(insert (concat date "#1 ")))
      (notes-dindex-save-buffer)
      (setq buffer-read-only t))))

(defun notes-dindex-delete-note (date)
  "Delete a date from the notes date-index."
  (save-excursion
    (let* ((date-count (notes-dindex-goto-date date nil t))
	   (count (string-to-int (nth 1 date-count))))
      (setq buffer-read-only nil)
      (if (> count 1)
	  ;; decrement count
	  (progn
	    (goto-char (match-beginning 2))
	    (delete-region (match-beginning 2) (match-end 2))
	    (insert (int-to-string (1- count))))
	;; remove date
	(delete-region (match-beginning 0) (match-end 0)))
      (notes-dindex-save-buffer)
      (setq buffer-read-only t))))

(provide 'notes-dindex)
