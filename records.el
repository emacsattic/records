;;;
;;; notes.el
;;;
;;; $Id: records.el,v 1.11 1996/12/16 16:55:20 asgoel Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

; $Log: records.el,v $
; Revision 1.11  1996/12/16 16:55:20  asgoel
; Added more notes-concatenate variables for customization.
;
; Revision 1.10  1996/12/15  05:02:06  asgoel
; 1. Major: Added notes encryption and  decryption.
;    func: notes-encrypt-note
;          notes-decrypt-note
; 2. Major: Implemented concatenation of subjects.
;    func: notes-concatenate-notes
;          notes-concatenate-note-files
;    vars: notes-output-buffer
; 	 notes-select-buffer-on-concat
;          notes-erase-output-buffer
; 3. Fixed most todo's.
; 4. Fixed notes-ndate-lessp.
; 5. Added notes-add-date.
; 6. Added striping of file:// and file://localhost.
; 7. Identical note files are not put in the history consecutively.
; 8. Added documentation about function return values.
; 9. Changed name from notes-goto-prev/next/relative-date to
;    notes-goto-prev/next/relative-note-file
;
; Revision 1.9  1996/12/13  19:58:26  asgoel
; Fixed error messages.
; Added support for notes-goto-last-note.
;
; Revision 1.8  1996/12/11  21:17:52  asgoel
; Added notes menu.
;
; Revision 1.7  1996/12/11  01:55:27  asgoel
; Added notes-body-empty-p
; Added removal of empty notes after todo moves
;
; Revision 1.6  1996/12/10  01:34:31  asgoel
; Made notes-initialize interactive (binding : C-c C-z).
; notes-directory does not have to have a slash following it.
;
; Revision 1.5  1996/12/05  21:02:40  asgoel
; Added initialization from notes-init-file.
; Added date index code for going to the previous notes file.
; Added todo functionality.
; Fixed a normalize year bug.
; Added subject completion code before notes-read-subject is called.
; Added note subject fontification and read-only text.
; Added no-switch, directory and todo parameters to notes-goto-note.
; Added directory creation support in notes-goto-note.
; Added on-next parameter to notes-goto-down-note.
; Added no-switch and todo parameters to notes-goto-relative-day.
; Added no-switch parameter to notes-goto-prev/next-day.
; Added optional subject parameter to notes-insert-note.
; Added key bindings for notes-goto-prev/next-date.
; Fixed loading order of notes and notes-index.
;
; Revision 1.4  1996/11/26  02:01:01  asgoel
; Move notes-subject-table to notes.el
;
; Revision 1.3  1996/11/22  12:43:49  asgoel
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

(defvar notes-init-file (concat (getenv "HOME") "/.emacs-notes")
  "* All the notes initialization should be put in this file.
This file is also read by the perl index generator.
If you change this variable, you must change the perl scripts also.")

(defvar notes-directory (concat (getenv "HOME") "/notes")
  "* Directory under which all notes are stored.")

(defvar notes-index-file (concat notes-directory "/index")
  "* File name in which notes subject index is stored.")

(defvar notes-dindex-file (concat notes-directory "/dindex")
  "* File name in which notes date index is stored.")

(defvar notes-directory-structure 1
  "* The directory structure for notes files. Its values can be 
0 => all notes are stored in the variable notes-directory. 
1 => notes are stored by year.
2 => notes are stored by year and month.")

(defvar notes-day-order 0
  "* A notes file name is composed of a day, month and year.
This variable determines the order of the day in date. 
Valid values are 0, 1 or 2 only.")

(defvar notes-month-order 1
  "* A notes file name is composed of a day, month and year.
This variable determines the order of the month in date. 
Valid values are 0, 1 or 2 only.")

(defvar notes-year-order 2
  "* A notes file name is composed of a day, month and year.
This variable determines the order of the month in date. 
Valid values are 0, 1 or 2 only.")

(defvar notes-year-length 4
  "* The length of a notes file year. Valid values are 2 or 4 only.")

(defvar notes-fontify t
  "* Enable notes fontification.")

(defvar notes-subject-read-only t
  "* If t, notes subjects are made read-only.
This disables any accidental updates to a notes subject.
The down side is that if any part of the subject is copied to a note body,
it is read-only and does not allow editing of that part.")

(defvar notes-bold-face 'bold
  "* Face to use for notes-index-mode and notes-mode subjects.
The default face is copied from 'bold.")

;; todo variables
(defvar	notes-todo-prev-day nil
  "* If t, notes-todo is invoked for a new file from notes-goto-prev-day.
A file is new if it does not have any notes in it.
If nil, notes-todo is not invoked.
If not nil and not t, user is asked whether notes-todo should be invoked.")
(defvar	notes-todo-next-day nil
  "* If t, notes-todo is invoked for a new file from notes-goto-next-day.
If nil, notes-todo is not invoked.
If not nil and not t, user is asked whether notes-todo should be invoked.")
(defvar	notes-todo-today t
  "* If t, notes-todo is invoked for a new file from notes-goto-today.
If nil, notes-todo is not invoked.
If not nil and not t, user is asked whether notes-todo should be invoked.")

(defvar	notes-todo-begin-copy-regexp "^CTODO: "
  "* The beginning of the copy todo is recognized by this regexp.")
(defvar	notes-todo-begin-move-regexp "^MTODO: "
  "* The beginning of the move todo is recognized by this regexp.")
(defvar	notes-todo-end-regexp "^\n\n"
  "* The end of both the copy and move todo is recognized by this regexp.")

(defvar	notes-todo-delete-empty-note t
  "* If t, delete note if it is empty after a todo move.
If nil, don't delete note.
If not nil and not t, ask user about deleting the note.")

(defvar notes-history-length 10
  "* The number of notes that are stored in notes-history.")

(defvar notes-output-buffer "*NOTES-OUTPUT*"
  "* Contains the output of concatenating notes.")

(defvar notes-subject-prefix-on-concat "--- "
  "* Prefix prepended to each subject on notes concatenation. 
See \\[notes-concatenate-notes\], and \\[notes-concatenate-note-files\].")

(defvar notes-subject-suffix-on-concat " ---"
  "* Suffix appended to each subject on notes concatenation. 
See \\[notes-concatenate-notes\], and \\[notes-concatenate-note-files\].")

(defvar notes-date-prefix-on-concat "* "
  "* Prefix prepended to each date on notes concatenation. 
See \\[notes-concatenate-notes\], and \\[notes-concatenate-note-files\].")

(defvar notes-date-suffix-on-concat ""
  "* Suffix appended to each date on notes concatenation. 
See \\[notes-concatenate-notes\], and \\[notes-concatenate-note-files\].")

(defvar notes-select-buffer-on-concat nil
  "* If non nil, the notes-output-buffer is selected after notes are
concatenated by \\[notes-concatenate-notes\].
If nil, the notes-output-buffer is just displayed.")

(defvar notes-erase-output-buffer nil
  "* If non nil, the notes-output-buffer is erased, 
every time \\[notes-concatenate-notes\] is invoked.
If nil, the output is appended.")

;;;
;;; Internal variables - users shouldn't change
;;; The defvar is for internal documentation.
;;;

(defvar notes-mode-menu-map nil
  "Notes Menu Map. Internal variable.")

(defvar notes-subject-table (make-vector 127 0)
  "List of subjects for notes subject completion.
Reloaded by loading the notes-index file.")

(defvar notes-date-regexp ""
  "Regexp matching a notes date. Internal variable.")

(defvar notes-tag-regexp ""
  "Regexp matching a notes tag. Internal variable.")

(defvar notes-date-tag-regexp ""
  "Regexp matching links in a note index. Internal variable.")

(defvar notes-day-length 0
  "The length of a notes file day. Internal variable.")

(defvar notes-month-length 0 
  "The length of a notes file month. Internal variable.")

(defvar notes-date-length 0
  "The length of a notes file date. Internal variable.")

(defvar notes-date-order '(() () ())
  "The order of a notes date. Internal variable.")

(defvar notes-date '((day 0 0) (month 0 0) (year 0 0))
  "The second and third values in each sublist 
are the start point and the length of each component in a date.
Internal variable.")

(defvar notes-todo-begin-regexp ""
  "Either the todo copy or move regexp. Internal variable.")

(defvar notes-todo-move-regions nil
  "All the regions that have to be removed from the preivous notes file.
Internal variable.")

(defvar notes-history nil
  "List of notes a user has visited.
Elements of the list consist of args to notes-goto-note.
Internal variable.")

(defun notes-initialize ()
  "Reads the notes init file and sets the notes internal variables
like notes-date, notes-date-length, etc."
  (interactive)
  (if (file-exists-p notes-init-file)
      (load notes-init-file))
  (setq notes-day-length 2)
  (setq notes-month-length 2)
  (setq notes-date-length 0)

  ;; set notes-date-order
  (setcar (nthcdr notes-day-order notes-date-order) 'day) 
  (setcar (nthcdr notes-month-order notes-date-order) 'month) 
  (setcar (nthcdr notes-year-order notes-date-order) 'year) 

  ;; set notes-date
  (let ((i 0))
    (mapcar 
     '(lambda (x)
	(let ((len (symbol-value 
		    (intern (concat "notes-" (symbol-name x) "-length")))))
	  (setcdr (assq x notes-date)
		  (list notes-date-length len))
	  (setq notes-date-length
		(+ notes-date-length len))
	  (setq i (1+ i))))
     notes-date-order))

  (setq notes-date-regexp 
	(concat "\\(" (let ((i 0) regexp)
			(while (< i notes-date-length)
			  (setq regexp (concat regexp "[0-9]"))
			  (setq i (1+ i))) regexp)
		"\\)"))
  (setq notes-tag-regexp "#\\([0-9]+\\|\\)")
  (setq notes-date-tag-regexp 
	(concat notes-date-regexp "\\(\\|" notes-tag-regexp "\\)\\s-"))

  (setq notes-todo-begin-regexp
	(concat "\\(" notes-todo-begin-copy-regexp "\\)\\|\\("
		notes-todo-begin-move-regexp "\\)"))
  ;; do some cleaning up
  (if (and (boundp 'notes-dindex-buffer) 
	   notes-dindex-buffer
	   (get-buffer notes-dindex-buffer))
      (kill-buffer notes-dindex-buffer))
  (if (and (boundp 'notes-index-buffer) 
	   notes-index-buffer
	   (get-buffer notes-index-buffer))
      (kill-buffer notes-index-buffer))
  )

;; initialize on load
(notes-initialize)

(defmacro notes-date-count-regexp (&optional date)
  "Regexp matching a date in the notes date-index file."
  ( `(if (, date)
	 (concat "\\(" (, date) "\\)#\\([0-9]+\\) ")
       (concat notes-date-regexp "#\\([0-9]+\\) "))))

(defmacro notes-subject-regexp (&optional subject)
  "Regexp matching the beginning of a note."
  ;; TODO: the underline should be of length(subject) + 2
  ;; not easy to do when subject is nil
  (` (if (, subject)
	 (concat "^\\* \\(" (, subject) "\\)\n\\-\\-\\-+$")
       ;; "^\\* \\(.*\\)\n\\-+$"
       "^\\* \\(.*\\)\n\\-\\-\\-+$"
       )))

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
      (and (= (nth 2 ndate-a) (nth 2 ndate-b))
	   (< (nth 1 ndate-a) (nth 1 ndate-b)))
      (and (= (nth 2 ndate-a) (nth 2 ndate-b))
	   (= (nth 1 ndate-a) (nth 1 ndate-b))
	   (< (nth 0 ndate-a) (nth 0 ndate-b)))))

(defun notes-add-date (ndate arg)
  "Adds (positive or negative) arg days to ndate and 
returns new normalized date."
  (let ((tmp-ndate ndate)
	new-ndate)
    ;; bump tmp-ndate
    (setcar tmp-ndate (+ arg (car tmp-ndate)))
    ;; encode and decode tmp-ndate
    (setq new-ndate (nthcdr 3 (decode-time 
			       (apply 'encode-time 0 0 0 tmp-ndate))))
    (setcdr (nthcdr 2 new-ndate) nil)
    ;; return new date
    new-ndate))

(defun notes-file-to-date (&optional file-name)
  "Get the date associated with file-name.
If file-name is not specified, the current buffers file-name is used."
    (if file-name
	()
      ;; get the file-name of the current buffer
      (if (null buffer-file-name)
	  (error "notes-file-to-date: buffer has no associated file."))
      (setq file-name (file-name-nondirectory buffer-file-name)))
    ;; check that length of file name is meaningful
    (if (= (length file-name) notes-date-length)
	file-name
      (error (concat "notes-file-to-date: bad file-name: " file-name))))

(defun notes-denormalize-date (ndate)
  "Get the file name associated with  date.
The ndate is normalized and in (day month year) format."
  (let ((cdate ndate)
	(date (make-string notes-date-length ? )))
    (if (= notes-year-length 2)
	;; convert to 2 digit year
	(if (> (nth 2 cdate) 90)
	    (setcar (nthcdr 2 cdate) (- (nth 2 cdate) 1900))
	  (setcar (nthcdr 2 cdate) (- (nth 2 cdate) 2000))))
    ;; now denormalize
    (let ((i 0))
      (mapcar
       '(lambda (x)
	  ;; this is kinda gross
	  (let ((start (nth 1 x))
		(len (nth 2 x)))
	    (setq date (concat 
			(substring date 0 start)
			(format (concat "%0" len "d") (nth i ndate))
			(substring date (+ start len))))
	    (setq i (1+ i))))
       notes-date))
    date))

(defun notes-normalize-date (date)
  "Returns date in (day month year) format with year in four digits"
  (let ((ndate '(0 0 0))
	(i 0))
    (mapcar
     '(lambda (x)
	(setcar (nthcdr i ndate) (string-to-int 
				  (substring date (nth 1 x) 
					     (+ (nth 2 x) (nth 1 x)))))
	(setq i (1+ i)))
     notes-date)
    (if (= notes-year-length 2)
	;; convert to four digit year
	(if (> (nth 2 ndate) 90)
	    (setcar (nthcdr 2 ndate) (+ (nth 2 ndate) 1900))
	  (setcar (nthcdr 2 ndate) (+ (nth 2 ndate) 2000))))
    (copy-sequence ndate)))

(defun notes-directory-path (date &optional absolute)
  "Get the relative directory path to a notes file.
With absolute set, get the absolute path."
  (cond ((= notes-directory-structure 0) (if absolute notes-directory ""))
	((= notes-directory-structure 1) 
	 (concat (if absolute notes-directory "..") "/"
		 (substring date (nth 1 (nth 2 notes-date))
			    (+ (nth 2 (nth 2 notes-date)) 
			       (nth 1 (nth 2 notes-date))))))
	((= notes-directory-structure 2)
	 (concat (if absolute notes-directory "../..") "/"
		 (substring date (nth 1 (nth 2 notes-date))
			    (+ (nth 2 (nth 2 notes-date))
			       (nth 1 (nth 2 notes-date)))) 
		 "/"
		 (substring date (nth 1 (nth 1 notes-date))
			    (+ (nth 2 (nth 1 notes-date))
			       (nth 1 (nth 1 notes-date))))))
	(t 
	 (error "notes-directory-path: bad notes-directory-structure value"))))

(defun notes-read-subject (&optional subject)
  "Read the notes subject to be inserted from the minibuffer.
Completion is possible."
  (interactive
   (progn (notes-index-buffer) ; initializes notes-subject-table if required
	  (list (completing-read "Notes subject: " notes-subject-table))))
  subject)

(defun notes-compose-region (beg end)
  "Fontify a notes region, make read-only etc.
Look at variables notes-fontify and notes-subject-read-only.
This function is currently only invoked for a notes subject.
Although the region is read-only, it is possible to edit at the beginning
of the region. This can mess up a notes subject. If this were disallowed,
then users would not be able to add text before a subject.
Any better solution?"
  (if notes-fontify
      (progn
	(add-text-properties (1- end) end '(rear-nonsticky t))
	;; partial fix to a probable emacs bug
	;; (if notes-subject-read-only
	;;	  if (> beg (point-min)) 
	;;     (add-text-properties beg (1+ beg) 
	;;	    '(front-sticky (read-only)))
	;;	      ))
	(add-text-properties beg end '(face bold))
	(if notes-subject-read-only
	    (add-text-properties beg end '(read-only notes-subject))))))

(defun notes-parse-buffer ()
  "Parses the notes buffer and fontifies note subjects etc."
  (save-excursion
    (goto-char (point-min))
    ;; goto first note
    (if (notes-goto-down-note nil t)
	(let ((modified (buffer-modified-p)));; should always be false
	  (while (progn;;  a do-while loop
		   (notes-mark-subject)
		   ;; fontify region, make read-only etc.
		   (notes-compose-region (point) (mark))
		   ;; goto next note - returns nil when no more exist
		   (notes-goto-down-note)))
	  (and (not modified) (buffer-modified-p)
	       (set-buffer-modified-p nil))))))

(defun notes-make-link (subject date tag)
  "Make a notes link."
  (concat "link: <"
	  (notes-directory-path date) "/"
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
      (error "notes-goto-subject: no subject found.")))
  (buffer-substring-no-properties (match-beginning 1) (match-end 1)))

(defmacro notes-tag (tag)
  (` (if (> (length (, tag)) 0) (concat "#" (, tag)) "")))

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
	(concat subject (notes-tag tag))))))

(defun notes-mark-note (&optional arg)
  "Put mark at the end of the current note and point at the beginning
of the note subject. With non-null prefix arg, the point is placed
at the beginning of the note body.
TODO: non interactive calls should pop-mark also."
  (interactive "P")
  (let (pt)
    (if arg
	(progn (notes-mark-subject) (setq pt (mark)) (pop-mark)))
    (notes-goto-down-note)
    (push-mark (point))
    (if pt (goto-char pt) 
      (notes-goto-up-note)
      (point))))

(defun notes-mark-subject ()
  "Put mark at the end of subject in this note and point at beginning.
The note marked is the one that contains point or follows point.
TODO: non interactive calls should pop-mark also."
  (if (null (notes-goto-subject))
      (error "notes-mark-subject: no subject found."))
  (let ((pt (point)))
    (next-line 2)
    (beginning-of-line)
    (if (looking-at "link: <.*>")
	(progn
	  (next-line 1)
	  (beginning-of-line)))
    (push-mark (point))
    ;; also return value
    (goto-char pt)))

(defun notes-body-empty-p ()
  "Is the body of the note under point empty?"
  (save-excursion
    (notes-mark-note t)
    (and (looking-at "\\s-*")
	 (eq (match-end 0) (mark)))))

(defun notes-link ()
  "Returns the notes link of the note around the current point."
  (save-excursion
    (if (null (notes-goto-subject))
	(error "notes-link: no subject found."))
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
  (let ((opoint (point)))
    (insert "* " subject "\n")
    (insert-char ?- (+ (length subject) 2))
    (insert (concat "\n" (notes-make-link subject date tag) "\n"))
    (notes-compose-region opoint (point))))

(defun notes-free-note (&optional keep-body)
  "Remove the current note. 
With arg., keep the body and remove the subject only."
  (save-excursion
    (let ((inhibit-read-only '(notes-subject)))
      (if keep-body (notes-mark-subject) (notes-mark-note))
      (delete-region (point) (mark))
      (pop-mark))))

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
  "Goto the link around point in the notes file."
  (interactive)
  (if (not (or (looking-at "<")
	       (re-search-backward "<" (point-boln) t)))
      ;; not a link I know about
      (error "notes-goto-link: invalid link under point.")
    ;; try to figure out a link
    (if (looking-at (concat "<\\(.*\\)/\\([0-9]+\\)" 
			    notes-tag-regexp "\\* \\(.*\\)>"))
	;; found a link
	(let ((subject (buffer-substring-no-properties (match-beginning 4)
						       (match-end 4)))
	      (date (buffer-substring-no-properties (match-beginning 2)
						       (match-end 2)))
	      (tag (buffer-substring-no-properties (match-beginning 3)
						       (match-end 3)))
	      (dir (buffer-substring-no-properties (match-beginning 1)
						   (match-end 1))))
	  ;; if "file://" or "file://localhost" is present 
	  ;; at the beginning of dir, strip it ... guess why?
	  (if (string-match "^file://\\(localhost\\|\\)" dir)
	      (setq dir (substring dir (match-end 0))))
	  (notes-goto-note subject date tag t nil nil nil dir))
      (error "notes-goto-link: invalid link under point."))))

(defun notes-goto-mouse-link (e)
  "Goto the link where mouse is clicked."
  (interactive "e")
  (mouse-set-point e)
  (notes-goto-link))

(defun notes-goto-note (subject date tag 
				&optional no-hist no-switch todo no-error dir)
  "Goto the note on date with subject and tag.
If subject is nil, goto date only.
If no-hist is t, then don't add this link to the notes-history list.
If no-switch is t, then do not switch to the new notes buffer.
Instead, the buffer is made ready for editing (via set-buffer).
If todo is t, then invoke notes-todo when a note-less file is being visited.
If todo is not nil and not t, ask user whether notes-todo should be called.
If no-error is t, do not signal error, if the note is not found.
If dir is specified, then the file is assumed to be \"dir/date\"."
  (if (null dir)
      (setq dir (notes-directory-path date t)))
  (let ((file (concat dir "/" date))
	found)
    (if (not (file-directory-p dir))
	;; ask the user if they want to create the directory
      (if (y-or-n-p (concat "Make directory: " dir " "))
	  (make-directory (expand-file-name dir) t)
	(if no-error nil
	  (error (concat "note: " file " not found.")))))
    (if (null no-switch)
	(find-file file)
      (set-buffer (find-file-noselect file)))
    ;; handler for new notes files
    (if (and todo (null (save-excursion (notes-dindex-goto-date date t))))
	(if (or (eq todo t) 
		(y-or-n-p "Invoke notes-todo (default n): "))
	    (notes-todo date)))

    (if (null subject)
	;; this is for going to a specific day and not a note
	(setq found t)
      (goto-char (point-min))
      ;; TODO: this search forward will fail to get to the right spot
      ;; if a string matching this regexp has been added to a
      ;; previous subject in the file. We should check for the notes subject.
      (if (re-search-forward 
	   (concat "^link: <.*" date "#" tag "\\* " subject ">") 
	   (point-max) t)
	  ;; found
	  (progn (setq found t) (notes-goto-subject))
	(if no-error
	    nil
	  (error 
	   (concat "Notes subject: " subject 
		   (if (> (length tag) 0) (concat ", tag: " tag))
		   " not found.")))))
    
    ;; support for goto last note
    (if (and found (null no-hist))
	(let ((hist (list subject date tag t nil nil nil dir))
	      hist-last)
	  (if (equal hist (car notes-history))
	      () ;; don't add identical notes.
	    (setq notes-history (cons hist notes-history))
	    (setq hist-last 
		  (nthcdr (- notes-history-length 1) notes-history))
	    (if hist-last
		(setcdr hist-last nil)))))))

(defun notes-goto-up-note (&optional subject)
  "Go to the beginning of the current note.
If the point is currently at the beginning of a note, go to the note above.
If subject is specified, go up to the beginning of a note with subject."
  (interactive)
  (re-search-backward (notes-subject-regexp subject) (point-min) 'start))

(defun notes-goto-down-note (&optional subject on-next)
  "Go to the beginning of the next note. 
If subject is specified, go down to the beginning of a note with subject.
If on-next is t, then don't move if we are at the beginning of a subject."
  (interactive)
  (let ((regexp (notes-subject-regexp subject)))
    (if (and (null on-next) 
	     (looking-at regexp))
	(goto-char (match-end 0)))
    ;; find next note and leave cursor at section beginning
    (if (re-search-forward regexp (point-max) 'end)
	(goto-char (match-beginning  0)))
    ))

(defun notes-goto-index(&optional arg subject date tag no-error)
  "If arg is nil or zero, goto the index on date and tag.
With positive arg, go to the index arg-times next to date and tag.
With negative arg, go to the index arg-times previous to date and tag.
Returns the new (date, tag) if found."
  (interactive "P")
  (if (not (and subject date))
      ;; initialize subject date and tag
      (let ((subject-tag (notes-subject-tag t)))
	(setq subject (nth 0 subject-tag))
	(setq tag (nth 1 subject-tag))
	(setq date (notes-file-to-date))))
  (if (notes-index-goto-subject subject (interactive-p) no-error)
      (notes-index-goto-relative-date-tag arg date tag)))
  
(defun notes-goto-relative-day(&optional arg no-switch todo)
  "With positive arg, go arg days ahead of current note's date. 
With negative arg, go arg days behind current note's date.
The no-switch and todo arguments are passed to notes-goto-note."
  (interactive "P")
  (let* ((ndate (notes-normalize-date (notes-file-to-date)))
	 (new-ndate (notes-add-date ndate arg))
	 (new-date (notes-denormalize-date new-ndate)))
    (notes-goto-note nil new-date "" nil no-switch todo)))

(defun notes-goto-prev-day(&optional arg no-switch)
  "Go to the notes file of the previous day.
With numeric prefix arg. go that many days back.
See also notes-goto-prev-note-file."
  (interactive "P")
  (notes-goto-relative-day (if arg (- arg) -1) no-switch notes-todo-prev-day))

(defun notes-goto-next-day(&optional arg no-switch)
  "Go to the notes file of the next day.
With numeric prefix arg. go that many days forward.
See also notes-goto-next-note-file."
  (interactive "P")
  (notes-goto-relative-day (if arg arg 1) no-switch notes-todo-next-day))

(defun notes-goto-today ()
  "Go to the notes file of today."
  (interactive)
  (let ((ndate (nthcdr 3 (decode-time)))
	date)
    (setcdr (nthcdr 2 ndate) nil)
    ;; denormalize the date to get the file name
    (setq date (notes-denormalize-date ndate))
    (notes-goto-note nil date "" nil nil notes-todo-today)))

(defun notes-goto-relative-note-file(&optional arg no-switch)
  "With positive arg, go arg files ahead of current notes file. 
With negative arg, go arg files behind of current notes file.
Returns the new date."
  (interactive "P")
  (let ((new-date
	 (save-excursion 
	   (nth 0
		(notes-dindex-goto-relative-date arg (notes-file-to-date))))))
    (if (null new-date)
	(error
	 (concat "notes-goto-relative-note-file: " 
		 (if (> arg 0) "next" "previous") " note file not found."))
      (notes-goto-note nil new-date "" nil no-switch))
    new-date))

(defun notes-goto-prev-note-file(&optional arg no-switch)
  "Go to the previous notes file. With arg. go that many notes files back.
Returns the new date. See also notes-goto-prev-day."
  (interactive "P")
  (notes-goto-relative-note-file (if arg (- arg) -1) no-switch))

(defun notes-goto-next-note-file(&optional arg no-switch)
  "Go to the next notes file. With arg. go that many notes files forward.
Returns the new date. See also notes-goto-next-day."
  (interactive "P")
  (notes-goto-relative-note-file (if arg arg 1) no-switch))

(defun notes-goto-relative-note (&optional arg subject date tag no-switch 
					   no-error)
  "If arg is nil or zero, goto the note on subject date and tag.
With positive arg, goto the note arg-times next to date and tag.
With negative arg, goto the note arg-times previous to date and tag.
Returns the new (date, tag) if found."
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
	(notes-goto-note subject (nth 0 date-tag) (nth 1 date-tag) nil 
			 no-switch nil no-error)
      (if (null no-error)
	  (error (concat "notes-goto-relative-note: " 
			 (if (> arg 0) "next" "previous") 
			 " note not found."))))
    ;; return value
    date-tag))

(defun notes-goto-prev-note (&optional arg subject date tag no-switch no-error)
  "Find the previous note on subject starting from date and tag.
Returns the new (date, tag) if found."
  (interactive "P")
  (notes-goto-relative-note (if arg (- arg) -1) subject date tag no-switch 
			    no-error))

(defun notes-goto-next-note (&optional arg subject date tag no-switch no-error)
  "Find the next note on subject starting from date and tag.
Returns the new (date, tag) if found."
  (interactive "P")
  (notes-goto-relative-note (if arg arg 1) subject date tag no-switch 
			    no-error))

(defun notes-goto-last-note ()
  "Go back to the last note file visited.
Identical note files are not put in the history consecutively."
  (interactive)
  (let ((link (car (cdr notes-history))))
    (or link
	(error "notes-goto-last-note: this is the first note."))
    (setq notes-history (cdr notes-history))
    (apply 'notes-goto-note link)))

;; TODO: should break function up
(defun notes-todo (&optional date)
  "Insert the previous note files todo's into the date file.
See the notes-todo-.*day variables on when it is automatically invoked."
  (interactive)
  (if (null date)
      (setq date (notes-file-to-date)))
  (save-excursion
    (let ((date-buf (current-buffer))
	  (prev-date (notes-goto-prev-note-file 1 t)))
      (if (and prev-date
	       (goto-char (point-min)) ;; always true
	       (notes-goto-down-note nil t)) ;; first note exists
	  ;; start the magic
	  (let ((subject (nth 0 (notes-subject-tag t))) ;; first note
		(bon-point (point))
		(eon-point (point-max))
		(cur-buf (current-buffer))
		next-subject bt-point et-point move subject-inserted)
	    (while 
		(progn;; do-while loop
		  (save-excursion
		    ;; get the end of note and the next subject
		    (if (notes-goto-down-note)
			(progn 
			  (setq next-subject (nth 0 (notes-subject-tag t)))
			  (setq eon-point (point)))
		      (setq eon-point (point-max))
		      (setq next-subject nil)))
		  ;; process all the todo's in the current note
		  (while (re-search-forward notes-todo-begin-regexp eon-point
					    'eon-point)
		    ;; do the copy/move thing for the current todo
		    (setq bt-point (match-beginning 0))
		    (setq move (match-beginning 2))
		    (if (re-search-forward notes-todo-end-regexp eon-point 
					   'eon-point);; goto end of todo
			(setq et-point (match-end 0))
		      (setq et-point (point)))
		    ;; for move, save the regions in the old file
		    (if move
			(setq notes-todo-move-regions
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
		      ;; insert an extra newline
		      (insert "\n")))
		  ;; going to next note: reset variables
		  (setq subject next-subject)
		  (setq bon-point eon-point)
		  (setq subject-inserted nil) 
		  subject))
	    ;; end of processing of all notes
	    ;; for todo-moves - remove regions from old file
	    (let ((modified (buffer-modified-p)))
	      (while notes-todo-move-regions
		(goto-char (car (car notes-todo-move-regions)))
		(apply 'delete-region (car notes-todo-move-regions))

		;; do the notes-todo-delete-empty-note
		(if (and notes-todo-delete-empty-note (notes-body-empty-p))
		    (notes-delete-note nil t))

		(setq notes-todo-move-regions
		      (cdr notes-todo-move-regions)))
	      (and (not modified) (buffer-modified-p)
		   (save-buffer)))
	    )))))

(defun notes-insert-note(&optional subject note-body)
  "Insert a new note for the current date. Asks for the subject."
  (interactive)
  (let* ((subject (if subject
		      subject (call-interactively 'notes-read-subject)))
	 (date (notes-file-to-date))
	 (tag ""))
    ;; we don't currently allow a note insertion 
    ;; if another note with the same subject exists below this note.
    (save-excursion
      (if (notes-goto-down-note subject)
	  (error 
	   (concat "notes-insert-note: can't insert out-of-order note: "
		   subject))))
    ;; check if another note with same subject exists above 
    ;; to get a new tag value
    (save-excursion
      (if (notes-goto-up-note subject)
	  ;; get tag
	  (setq tag (int-to-string (1+ (string-to-int 
					(nth 1 (notes-subject-tag t))))))))

    ;; add a notes index entry 
    (notes-index-insert-note subject date tag)

    ;; add the date to the date-index
    (notes-dindex-insert-note date)

    ;; now make the note body
    (notes-make-note subject date tag note-body)))

(defun notes-delete-note (&optional keep-body no-prompt)
  "Delete the current note for the current date.
With arg, removes the subject only."
  (interactive "P")
  (let* ((date (notes-file-to-date))
	 (subject-tag (notes-subject-tag t))
	 (subject (nth 0 subject-tag))
	 (tag (nth 1 subject-tag)))
    
    (if (if no-prompt     ;; prompt?
	    t (y-or-n-p (concat "Delete note: " subject " ")))
	(progn
	  ;; remove the note subject and optionally the body
	  (notes-free-note keep-body)
	  ;; remove the date from the date-index
	  (notes-dindex-delete-note date)
	  ;; remove the notes index entry
	  (notes-index-delete-note subject date tag)))
    ))
	
(defun notes-rename-note ()
  "Renames the subject of the current note for the current date."
  (interactive)
  (notes-delete-note 'keep-body)
  (notes-insert-note))

(defun notes-user-name ()
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

(defmacro notes-subject-on-concat (subject)
  "Make subject for notes concatenation."
  (` (let ((sub (concat notes-subject-prefix-on-concat (, subject)
			notes-subject-suffix-on-concat)))
       (concat sub "\n" (make-string (length sub) ?-) "\n"))))

(defmacro notes-date-on-concat (date)
  "Make date for notes concatenation."
  (` (let ((d (concat notes-date-prefix-on-concat (, date)
			notes-date-suffix-on-concat)))
       (concat d "\n" (make-string (length d) ?-) "\n"))))

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
  (define-key notes-mode-map "\C-c\C-b" 'notes-goto-prev-note-file) ;; back file
  (define-key notes-mode-map "\C-c\C-f" 'notes-goto-next-note-file) ;; front file

  (define-key notes-mode-map "\C-c\C-j" 'notes-goto-index);; jump!!

  (define-key notes-mode-map "\C-c\C-l" 'notes-goto-last-note)

  (define-key notes-mode-map "\C-c\C-g" 'notes-goto-link)
  (define-key notes-mode-map [M-S-mouse-1] 'notes-goto-mouse-link)

  (define-key notes-mode-map "\C-c/e" 'notes-encrypt-note)
  (define-key notes-mode-map "\C-c/d" 'notes-decrypt-note)

  (define-key notes-mode-map "\C-c\C-k" 'notes-link-as-kill)
  (define-key notes-mode-map [?\C-c ?\C--] 'notes-underline-line)
  (define-key notes-mode-map "\M-\C-h" 'notes-mark-note)
  (define-key notes-mode-map "\C-c\C-z" 'notes-initialize);; zap it in

  (if notes-mode-menu-map
      ()
    (setq notes-mode-menu-map (make-sparse-keymap))
    (define-key notes-mode-map [menu-bar] (make-sparse-keymap))
    (define-key notes-mode-map [menu-bar notes] (cons "Notes" 
						      notes-mode-menu-map))

    (define-key notes-mode-menu-map [notes-initialize] 
      '("Re-Init Notes" . notes-initialize))

    (define-key notes-mode-menu-map [separator-0] '("--"))

    (define-key notes-mode-menu-map [notes-todo] 
      '("Get TODO's" . notes-todo))
    (define-key notes-mode-menu-map [notes-underline-line] 
      '("Underline Line" . notes-underline-line))
    (define-key notes-mode-menu-map [notes-link-as-kill] 
      '("Copy Notes Link" . notes-link-as-kill))
    (define-key notes-mode-menu-map [notes-mark-note] 
      '("Mark Note" . notes-mark-note))

    (define-key notes-mode-menu-map [separator-3] '("--"))

    (define-key notes-mode-menu-map [notes-concatenate-note-files] 
      '("Concat Note Files" . notes-concatenate-note-files))
    (define-key notes-mode-menu-map [notes-concatenate-notes] 
      '("Concat Notes" . notes-concatenate-notes))

    (define-key notes-mode-menu-map [separator-6] '("--"))

    (define-key notes-mode-menu-map [notes-encrypt-note]
      '("Encrypt Note" . notes-encrypt-note))
    (define-key notes-mode-menu-map [notes-decrypt-note]
      '("Decrypt Note" . notes-decrypt-note))

    (define-key notes-mode-menu-map [separator-9] '("--"))

    (define-key notes-mode-menu-map  [notes-rename-note] 
      '("Rename Note" . notes-rename-note))
    (define-key notes-mode-menu-map  [notes-delete-note] 
      '("Delete Note" . notes-delete-note))
    (define-key notes-mode-menu-map  [notes-insert-note] 
      '("Insert Note" . notes-insert-note))

    (define-key notes-mode-menu-map [separator-12] '("--"))

    (define-key notes-mode-menu-map [notes-goto-last-note] 
      '("Goto Last Note" . notes-goto-last-note))
    (define-key notes-mode-menu-map [notes-goto-link] 
      '("Goto Notes Link" . notes-goto-link))
    (define-key notes-mode-menu-map  [notes-goto-index] 
      '("Goto Index" . notes-goto-index))

    (define-key notes-mode-menu-map [separator-15] '("--"))

    (define-key notes-mode-menu-map  [notes-goto-next-day] 
      '("Next Day" . notes-goto-next-day))
    (define-key notes-mode-menu-map  [notes-goto-prev-day] 
      '("Prev Day" . notes-goto-prev-day))

    (define-key notes-mode-menu-map [separator-18] '("--"))

    (define-key notes-mode-menu-map  [notes-goto-next-note-file] 
      '("Next Note File" . notes-goto-next-note-file))
    (define-key notes-mode-menu-map  [notes-goto-prev-note-file] 
      '("Prev Note File" . notes-goto-prev-note-file))

    (define-key notes-mode-menu-map [separator-21] '("--"))

    (define-key notes-mode-menu-map  [notes-goto-next-note] 
      '("Next Note" . notes-goto-next-note))
    (define-key notes-mode-menu-map  [notes-goto-prev-note] 
      '("Prev Note" . notes-goto-prev-note))

    (define-key notes-mode-menu-map [separator-24] '("--"))

    (define-key notes-mode-menu-map [notes-goto-down-note] 
      '("Down Note" . notes-goto-down-note))
    (define-key notes-mode-menu-map [notes-goto-up-note] 
      '("Up Note" . notes-goto-up-note))

    (define-key notes-mode-menu-map [separator-27] '("--"))

    (define-key notes-mode-menu-map [notes-goto-today] 
      '("Today's Note" . notes-goto-today))
    )

  ;; imenu stuff 
  (make-variable-buffer-local 'imenu-prev-index-position-function)
  (make-variable-buffer-local 'imenu-extract-index-name-function)
  (setq imenu-prev-index-position-function 'notes-goto-up-note)
  (setq imenu-extract-index-name-function 'notes-subject-tag)

  (notes-parse-buffer)
  (run-hooks 'notes-mode-hooks)
  )

(run-hooks 'notes-load-hooks)
(provide 'notes)
(if (not (featurep 'notes-index))
    (require 'notes-index))

