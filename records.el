;;;
;;; notes.el
;;;
;;; $Id: records.el,v 1.3 1996/11/22 12:43:49 asgoel Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

; $Log: records.el,v $
; Revision 1.3  1996/11/22 12:43:49  asgoel
; A working version that supports the old functionality + auto insertion,
; deletion and renaming of notes.
; However it can not be tested since I have to write a perl script
; to change my notes files to the new format. In the process, a script
; that can regenerate the index should also be written.
;
; Revision 1.2  1996/11/21  03:05:47  asgoel
; The first working version.
;
; Revision 1.1  1996/11/20  02:34:01  asgoel
; Initial revision
;

(require 'notes-index)

(defvar notes-directory (concat (getenv "HOME") "/notes/")
  "* Directory under which all notes are stored.")

(defvar notes-directory-structure 1
  "* The directory structure for notes files. Its values can be 
0 => all notes are stored in the variable notes-directory. 
1 => notes are stored by year.
2 => notes are stored by year and month.")

(defvar notes-use-font-lock t
  "* Enable notes fontification.")

(defvar notes-bold-face 'bold
  "* Face to use for notes-index-mode and notes-mode subjects.
The default face is copied from 'bold.")

(defvar notes-font-lock-keywords
  '(("^\\(\\* .*\\)\n\\-+$" 1 notes-bold-face)
    ("^\\* .*\n\\(\\-+\\)$" 1 notes-bold-face))
  "* Font-lock keywords for notes mode. This is experimental.
If it is too slow or incorrect, then a simpler expression can be used.")

(defvar notes-file-date-order '((day 2) (month 2) (year 4))
  "* A notes file name is composed of a day, month and year.
This variable determines the order of the day, month and year 
in a file name and their lengths.")

(defvar notes-date-order '((day 0) (month 1) (year 2))
  "Internal variable.")

(defvar notes-file-date '((0 0) (0 0) (0 0))
  "Internal variable calculated from notes-file-date-order.")

(defvar notes-file-date-length 0
  "Length of a notes file-name.
 Internal variable calculated from notes-file-date-order.") 

;; sets notes-file-date and notes-file-date-length
(mapcar 
 '(lambda (x)
    (setcar (nthcdr (nth 1 (assq (car x) notes-date-order))
		    notes-file-date)
	    (list notes-file-date-length 
		  (+ notes-file-date-length (nth 1 x))))
    (setq notes-file-date-length
	  (+ notes-file-date-length (nth 1 x))))
 notes-file-date-order)

(defvar notes-date-regexp (concat 
			   "\\("
			   (let ((i 0) regexp)
			     (while (< i notes-file-date-length)
			       (setq regexp (concat regexp "[0-9]"))
			       (setq i (1+ i))) regexp)
			   "\\)")
  "Regexp matching a notes date.")

(defvar notes-tag-regexp "#\\([0-9]+\\|\\)"
  "Regexp matching a notes tag.")

(defvar notes-date-tag-regexp 
  (concat notes-date-regexp "\\(\\|" notes-tag-regexp "\\)\\s-")
  "Regexp matching links in a note index.")

(defun point-boln ()
  "Return the boln as a position."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun point-eoln ()
  "Return the eoln as a position."
  (save-excursion
    (end-of-line)
    (point)))

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

(defun notes-file-to-date (&optional file-name)
  "Get the date associated with file-name.
If file-name is not specified, the current buffers file-name is used."
    (if file-name
	()
      ;; get the file-name of the current buffer
      (if (null buffer-file-name)
	  (error "notes-file-to-date: buffer has no associated file."))
      (setq file-name (file-name-nondirectory buffer-file-name)))
    ;; check that lenght of file name is meaningful
    (if (= (length file-name) notes-file-date-length)
	file-name
      (error (concat "notes-file-to-date: bad file-name " file-name))))

(defun notes-dmy-length(dmy)
  "Lenght of day, month or year in a date.
dmy should be 'day, 'month or 'year."
  (- (nth 1 (nth (nth 1 (assq dmy notes-date-order)) notes-file-date))
     (nth 0 (nth (nth 1 (assq dmy notes-date-order)) notes-file-date))))

(defun notes-denormalize-date (ndate)
  "Get the file name associated with  date.
The ndate is normalized and in (day month year) format."
  (let ((cdate ndate)
	(date (make-string notes-file-date-length ? )))
    (if (= (notes-dmy-length 'year) 2)
	;; convert to 2 digit year
	(if (> (nth 2 cdate) 95)
	    (setcar (nthcdr 2 cdate) (- (nth 2 cdate) 1900))
	  (setcar (nthcdr 2 cdate) (- (nth 2 cdate) 2000))))
    ;; now denormalize
    (let ((i 0))
      (mapcar
       '(lambda (x)
	  ;; this is kinda gross
	  (let* ((start (nth 0 x))
		 (len (- (nth 1 x) start)))
	    (setq date (concat 
			(substring date 0 start)
			(format (concat "%0" len "d") (nth i ndate))
			(substring date (+ start len))))
	    (setq i (1+ i))))
       notes-file-date))
    date))

(defun notes-normalize-date (date)
  "Returns date in (day month year) format with year in four digits"
  (let ((ndate '(0 0 0))
	(i 0))
    (mapcar
     '(lambda (x)
	(setcar (nthcdr i ndate) (string-to-int (apply 'substring date x)))
	(setq i (1+ i)))
     notes-file-date)
    (if (= (notes-dmy-length 'year) 2)
	;; convert to four digit year
	(if (> (nth 2 ndate) 95)
	    (setcar (nthcdr 2 ndate) (+ (nth 2 ndate) 1900))
	  (setcar (nthcdr 2 ndate) (+ (nth 2 ndate) 2000))))
    (copy-sequence ndate)))

(defun notes-directory-path (date &optional absolute)
  "Get the relative directory path to a notes file.
With absolute set, get the absolute path."
  (cond ((= notes-directory-structure 0) (if absolute notes-directory ""))
	((= notes-directory-structure 1) 
	 (concat (if absolute notes-directory "../")
		 (apply 'substring date (nth 2 notes-file-date))
		 "/"))
	((= notes-directory-structure 2)
	 (concat (if absolute notes-directory "../../")
		 (apply 'substring date (nth 1 notes-file-date))
		 "/" (format "%02d" (nth 1 date)) "/"))
	(t (error "notes-directory-path: bad value"))))

(defun notes-read-subject (&optional subject)
  "Read the notes subject to be inserted from the minibuffer.
Completion is possible."
  (interactive (list (completing-read "Notes subject: " notes-subject-table)))
  subject)

(defmacro notes-subject-regexp (&optional subject)
  "Regexp matching the beginning of a note."
  (` (if (, subject)
	 (concat "^\\* \\(" (, subject) "\\)\n\\-+$")
       "^\\* \\(.*\\)\n\\-+$")))

(defun notes-make-link (subject date tag)
  "Make a notes link."
  (concat "link: <"
	  (notes-directory-path date)
	  date "#" tag "* " subject ">"))

(defun notes-goto-subject ()
  "Goto the subject on the current note and return the subject."
  (beginning-of-line)
  (if (looking-at "^\\s-*-+\\s-*$")
      (progn 
	(previous-line 1)
	(beginning-of-line)))
  (if (looking-at (notes-subject-regexp))
      ()
    (if (notes-goto-up-note) 
	()
      (error "notes-goto-subject: no subject")))
  (buffer-substring-no-properties (match-beginning 1) (match-end 1)))

(defun notes-subject-tag (&optional no-str)
  "Returns subject#tag of the note where point is located.
If no-str is t, return (subject, tag)."
  (save-excursion
    (let ((subject (notes-goto-subject))
	  tag)
      (next-line 2)
      (if (re-search-forward notes-tag-regexp (point-eoln) t)
	  (setq tag (buffer-substring-no-properties 
		     (match-beginning 1) (match-end 1))))
      (if no-str (list subject tag)
	(concat subject (if (> (length tag) 0) (concat "#" tag) ""))))))

(defun notes-mark-note ()
  "Put mark at end of this node and point at beginning.
The note marked is the one that contains point or follows point."
  (interactive)
  (notes-goto-down-note)
  (push-mark)
  (notes-goto-up-note) 
  ;; return value
  (point))

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
    ;; also return value
    (goto-char pt)))

(defun notes-link ()
  "Returns the notes link of the note around the current point."
  (save-excursion
    (if (null (notes-goto-subject))
	(error "notes-link: no subject"))
    (next-line 2)
    (beginning-of-line)
    (if (looking-at "link: \\(<.*>\\)")
	(buffer-substring-no-properties (match-beginning 1) (match-end 1)))))

(defun notes-link-as-kill ()
  "Put the notes link of the note around the current point in the kill ring."
  (interactive)
  (kill-new (notes-link)))

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
    (if keep-body (notes-mark-subject) (notes-mark-note))
    (kill-region (point) (mark))))

(defun notes-underline-line ()
  "Underline the current line to the length of the line."
  ;; check to see if current line is already underlined
  ;; remove that underlining first.
  (interactive)
  (save-excursion
    (forward-line 1)
    (beginning-of-line)
    (if (looking-at "^\\s-*-+\\s-*$")
	(kill-line 1)))
  ;; now underline the line
  (save-excursion
    (let ((bol (progn (beginning-of-line) (point)))
	  (bospaces (progn (skip-chars-forward " \t") (point)))
	  (eol (progn (end-of-line) (point))))
      (insert "\n" (buffer-substring bol bospaces))
      (insert-char ?- (- eol bospaces)))))

(defun notes-goto-link ()
  "Goto the link around point."
  (interactive)
  (if (not (or (looking-at "<")
	       (re-search-backward "<" (point-boln) t)))
      ;; not a link I know about
      (error "notes-goto-link: not on a link")
    ;; try to figure out a link
    (if (looking-at (concat "<.*/\\([0-9]+\\)" 
			    notes-tag-regexp "\\* \\(.*\\)>"))
	;; found a link
	(let ((subject (buffer-substring-no-properties (match-beginning 3)
						       (match-end 3)))
	      (date (buffer-substring-no-properties (match-beginning 1)
						       (match-end 1)))
	      (tag (buffer-substring-no-properties (match-beginning 2)
						       (match-end 2))))
	(notes-goto-note subject date tag))
      (error "notes-goto-link: not on a link"))))

(defun notes-goto-mouse-link (e)
  "Goto the link where mouse is clicked."
  (interactive "e")
  (mouse-set-point e)
  (notes-goto-link))

(defun notes-goto-note (subject date tag &optional no-error)
  "Goto the note on date with subject and tag.
If subject is nil, goto date only."
  (let ((file (concat (notes-directory-path date t) date)))
    (find-file file)
    (goto-char (point-min))
    (if (null subject)
	;; this is for going to a specific day and not a note
	nil
      (if (re-search-forward 
	   (concat "link: <.*" date "#" tag "\\* " subject ">") 
	   (point-max) t)
	  ;; found
	  (notes-goto-subject)
	(if no-error
	    nil
	  (error (concat "subject tag: " subject " " tag " not found.")))))))

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

(defun notes-goto-index(&optional arg subject date tag no-error)
  "If arg is nil or zero, goto the index on date and tag.
With positive arg, go to the index arg-times next to date and tag.
With negative arg, go to the index arg-times previous to date and tag."
  (interactive "P")
  (if (not (and subject date))
      ;; initialize subject date and tag
      (let ((subject-tag (notes-subject-tag t)))
	(setq subject (nth 0 subject-tag))
	(setq tag (nth 1 subject-tag))
	(setq date (notes-file-to-date))))
  (if (notes-index-goto-subject subject (interactive-p) no-error)
      (notes-index-goto-relative-date-tag arg date tag)))
  
(defun notes-goto-relative-day(&optional arg)
  "With positive arg, go arg days ahead of current note's date. 
With negative arg, go arg days behind current note's date."
  (interactive "p")
  (let ((ndate (notes-normalize-date (notes-file-to-date)))
	new-ndate new-date)
    ;; bump ndate
    (setcar ndate (+ arg (car ndate)))
    ;; encode and decode ndate
    (setq new-ndate (nthcdr 3 (decode-time (apply 'encode-time 0 0 0 ndate))))
    (setcdr (nthcdr 2 new-ndate) nil)
    ;; denormalize the date to get the file name
    (setq new-date (notes-denormalize-date new-ndate))
    (notes-goto-note nil new-date "")
  ))

(defun notes-goto-prev-day(&optional arg)
  "Go to the notes file of the previous day.
With arg. go that many days back."
  (interactive "P")
  (notes-goto-relative-day (if arg (- arg) -1)))

(defun notes-goto-next-day(&optional arg)
  "Go to the notes file of the next day.
With arg. go that many days forward."
  (interactive "P")
  (notes-goto-relative-day (if arg arg 1)))

(defun notes-goto-today ()
  "Go to the notes file of today."
  (interactive)
  (let ((ndate (nthcdr 3 (decode-time)))
	date)
    (setcdr (nthcdr 2 ndate) nil)
    ;; denormalize the date to get the file name
    (setq date (notes-denormalize-date ndate))
    (notes-goto-note nil date "")
    ))

(defun notes-goto-relative-note (&optional arg subject date tag no-error)
  "If arg is nil or zero, goto the note on subject date and tag.
With positive arg, goto the note arg-times next to date and tag.
With negative arg, goto the note arg-times previous to date and tag."
  (interactive "P")
  (if (not (and subject date))
      ;; initialize subject date and tag
      (let ((subject-tag (notes-subject-tag t)))
	(setq subject (nth 0 subject-tag))
	(setq tag (nth 1 subject-tag))
	(setq date (notes-file-to-date))))
  (let ((date-tag 
	 (save-excursion
	   (notes-goto-index arg subject date tag no-error))))
    (if date-tag
	;; goto the note
	(notes-goto-note subject (nth 0 date-tag) (nth 1 date-tag) no-error)
      (if (null no-error)
	  (error (concat "notes-goto-relative-note: "
			 subject " " date " not found"))))
    ;; return value
    date-tag))

(defun notes-goto-prev-note (&optional arg subject date tag no-error)
  "Find the previous note on subject starting from date and tag."
  (interactive "P")
  (notes-goto-relative-note (if arg (- arg) -1) subject date tag no-error))

(defun notes-goto-next-note (&optional arg subject date tag no-error)
  "Find the next note on subject starting from date and tag."
  (interactive "P")
  (notes-goto-relative-note (if arg arg 1) subject date tag no-error))
  
(defun notes-insert-note(&optional note-body)
  "Insert a new note for the current date. Asks for the subject."
  (interactive)
  (let* ((subject (call-interactively 'notes-read-subject))
	 (date (notes-file-to-date))
	 (tag ""))
    ;; we don't currently allow a note insertion 
    ;; if another note with the same subject exists below this note.
    (save-excursion
      (if (notes-goto-down-note subject)
	  (error "notes-insert-note: can't insert out-of-order note")))
    ;; check if another note with same subject exists above 
    ;; to get a new tag value
    (save-excursion
      (if (notes-goto-up-note subject)
	  ;; get tag
	  (setq tag (int-to-string (1+ (string-to-int 
					(nth 1 (notes-subject-tag t))))))))

    ;; add a notes index entry 
    (notes-index-insert-note subject date tag)

    ;; now make the note body
    (notes-make-note subject date tag note-body)))

(defun notes-delete-note (&optional keep-body)
  "Delete the current note for the current date.
With arg, removes the subject only."
  (interactive "P")
  (let* ((date (notes-file-to-date))
	 (subject-tag (notes-subject-tag t))
	 (subject (nth 0 subject-tag))
	 (tag (nth 1 subject-tag)))
    
    ;; remove the note subject and optionally the body
    (notes-free-note keep-body)

    ;; remove the notes index entry
    (notes-index-delete-note subject date tag)
    ))
	
(defun notes-rename-note ()
  "Renames the subject of the current note for the current date."
  (interactive)
  (notes-delete-note 'keep-body)
  (notes-insert-note))

(define-derived-mode notes-mode text-mode "Notes"
  "Enable notes-mode for a buffer.
The key-bindings of this mode are:
\\{notes-mode-map}"

  ;; key-bindings
  (define-key notes-mode-map "\C-c\C-i" 'notes-insert-note)
  (define-key notes-mode-map "\C-c\C-d" 'notes-delete-note)
  (define-key notes-mode-map "\C-c\C-r" 'notes-rename-note)

  (define-key notes-mode-map "\M-\C-a" 'notes-goto-up-note)
  (define-key notes-mode-map "\M-\C-e" 'notes-goto-down-note)

  (define-key notes-mode-map "\C-c\C-p" 'notes-goto-prev-note)
  (define-key notes-mode-map "\C-c\C-n" 'notes-goto-next-note)

  (define-key notes-mode-map "\C-c\C-y" 'notes-goto-prev-day) ;; yesterday
  (define-key notes-mode-map "\C-c\C-t" 'notes-goto-next-day) ;; tomorrow
  (define-key notes-mode-map "\C-c\C-j" 'notes-goto-index) ;; jump!!

  (define-key notes-mode-map "\C-c\C-l" 'notes-goto-last-note)

  (define-key notes-mode-map "\C-c\C-g" 'notes-goto-link)
  (define-key notes-mode-map [M-S-mouse-1] 'notes-goto-mouse-link)

  (define-key notes-mode-map "\C-c\C-k" 'notes-link-as-kill)
  ;; TODO: broken binding
  (define-key notes-mode-map "\C-c\C-\\-" 'notes-underline-line)

  ;; imenu stuff 
  (make-variable-buffer-local 'imenu-prev-index-position-function)
  (make-variable-buffer-local 'imenu-extract-index-name-function)
  (setq imenu-prev-index-position-function 'notes-goto-up-note)
  (setq imenu-extract-index-name-function 'notes-subject-tag)

  (if notes-use-font-lock
      (progn
	(require 'font-lock)
	(make-local-variable 'font-lock-no-comments)
	(setq font-lock-no-comments t)
	(make-local-variable 'font-lock-keywords)
	(setq font-lock-keywords notes-font-lock-keywords)
	(font-lock-mode 1)))

  (run-hooks 'notes-mode-hooks))

(run-hooks 'notes-load-hooks)
(provide 'notes)
