;;;
;;; records-vars.el
;;;
;;; $Id: records-vars.el,v 1.3 1997/05/01 21:21:24 ashvin Exp $
;;;
;;; Copyright (C) 1996 by Ashvin Goel
;;;
;;; This file is under the Gnu Public License.

; $Log: records-vars.el,v $
; Revision 1.3  1997/05/01 21:21:24  ashvin
; Changed names from notes to record.
;
; Revision 1.2  1997/01/23 00:02:35  ashvin
; The first release
;
; Revision 1.1  1996/12/17  22:37:19  asgoel
; Initial revision
;
;;;
;;; The next set of variables are accessed by recordsadmin.
;;; Do not set them explicitly since they are set in your
;;; records initilization file (see records-init-file) when recordsadmin is run.
;;; Beware!
;;;

(defvar records-init-file (concat (getenv "HOME") "/.emacs-records")
  "* All the records initialization should be put in this file.
This file is also read by the perl index generator.
If you change this variable, you must change the perl scripts also.")

(defvar records-directory (concat (getenv "HOME") "/records")
  "* Directory under which all records are stored.")

(defvar records-index-file (concat records-directory "/index")
  "* File name in which records subject index is stored.")

(defvar records-dindex-file (concat records-directory "/dindex")
  "* File name in which records date index is stored.")

(defvar records-directory-structure 1
  "* The directory structure for records files. Its values can be 
0 => all records are stored in the variable records-directory. 
1 => records are stored by year.
2 => records are stored by year and month.")

(defvar records-day-order 0
  "* A records file name is composed of a day, month and year.
This variable determines the order of the day in date. 
Valid values are 0, 1 or 2 only.")

(defvar records-month-order 1
  "* A records file name is composed of a day, month and year.
This variable determines the order of the month in date. 
Valid values are 0, 1 or 2 only.")

(defvar records-year-order 2
  "* A records file name is composed of a day, month and year.
This variable determines the order of the month in date. 
Valid values are 0, 1 or 2 only.")

(defvar records-year-length 4
  "* The length of a records file year. Valid values are 2 or 4 only.")

;;;
;;; You are free to play with these variables.
;;; Use M-x set-variable records-.* (records- followed by completion) 
;;; to see all these variables.
;;;
(defvar records-fontify t
  "* Enable records fontification.")

(defvar records-subject-read-only t
  "* If t, records subjects are made read-only.
This disables any accidental updates to a records subject.
The down side is that if any part of the subject is copied to a record body,
it is read-only and does not allow editing of that part.")

(defvar records-bold-face 'bold
  "* Face to use for records-index-mode and records-mode subjects.
The default face is copied from 'bold.")

;; todo variables
(defvar	records-todo-prev-day nil
  "* If t, records-todo is invoked for a new file from records-goto-prev-day.
A file is new if it does not have any records in it.
If nil, records-todo is not invoked.
If not nil and not t, user is asked whether records-todo should be invoked.")
(defvar	records-todo-next-day nil
  "* If t, records-todo is invoked for a new file from records-goto-next-day.
If nil, records-todo is not invoked.
If not nil and not t, user is asked whether records-todo should be invoked.")
(defvar	records-todo-today t
  "* If t, records-todo is invoked for a new file from records-goto-today.
If nil, records-todo is not invoked.
If not nil and not t, user is asked whether records-todo should be invoked.")

(defvar	records-todo-begin-copy-regexp "^CTODO: "
  "* The beginning of the copy todo is recognized by this regexp.")
(defvar	records-todo-begin-move-regexp "^TODO: "
  "* The beginning of the move todo is recognized by this regexp.")
(defvar	records-todo-end-regexp "^\n\n"
  "* The end of both the copy and move todo is recognized by this regexp.")

(defvar	records-todo-delete-empty-record t
  "* If t, delete record if it is empty after a todo move.
If nil, don't delete record.
If not nil and not t, ask user about deleting the record.")

(defvar records-history-length 10
  "* The number of records that are stored in records-history.")

(defvar records-output-buffer "*RECORDS-OUTPUT*"
  "* Contains the output of concatenating records.")

(defvar records-subject-prefix-on-concat "--- "
  "* Prefix prepended to each subject on records concatenation. 
See \\[records-concatenate-records\], and \\[records-concatenate-record-files\].")

(defvar records-subject-suffix-on-concat " ---"
  "* Suffix appended to each subject on records concatenation. 
See \\[records-concatenate-records\], and \\[records-concatenate-record-files\].")

(defvar records-date-prefix-on-concat "* "
  "* Prefix prepended to each date on records concatenation. 
See \\[records-concatenate-records\], and \\[records-concatenate-record-files\].")

(defvar records-date-suffix-on-concat ""
  "* Suffix appended to each date on records concatenation. 
See \\[records-concatenate-records\], and \\[records-concatenate-record-files\].")

(defvar records-select-buffer-on-concat nil
  "* If non nil, the records-output-buffer is selected after records are
concatenated by \\[records-concatenate-records\].
If nil, the records-output-buffer is just displayed.")

(defvar records-erase-output-buffer nil
  "* If non nil, the records-output-buffer is erased, 
every time \\[records-concatenate-records\] is invoked.
If nil, the output is appended.")

(provide 'records-vars)

;;; Local Variables:
;;; generated-autoload-file:"records-load.el"
;;; End:
