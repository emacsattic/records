;;;
;;; notes-vars.el
;;;
;;; $Id: records-vars.el,v 1.2 1997/01/23 00:02:35 ashvin Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

; $Log: records-vars.el,v $
; Revision 1.2  1997/01/23 00:02:35  ashvin
; The first release
;
; Revision 1.1  1996/12/17  22:37:19  asgoel
; Initial revision
;
;;;
;;; The next set of variables are accessed by notesadmin.
;;; Do not set them explicitly since they are set in your
;;; notes initilization file (see notes-init-file) when notesadmin is run.
;;; Beware!
;;;

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

;;;
;;; You are free to play with these variables.
;;; Use M-x set-variable notes-.* (notes- followed by completion) 
;;; to see all these variables.
;;;
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
(defvar	notes-todo-begin-move-regexp "^TODO: "
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

(provide 'notes-vars)

;;; Local Variables:
;;; generated-autoload-file:"notes-load.el"
;;; End:
