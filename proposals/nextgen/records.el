;;; records.el --- records

;; $Id: records.el,v 1.1 2001/05/13 02:08:18 burtonator Exp $

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
;;

(require 'records-vars)
(require 'records-index)
(require 'records-dindex)
(require 'records-util)
(require 'records-search)
(require 'records-mode)
(require 'records-calendar)
(require 'records-metainfo)
(require 'records-format)
(require 'records-status)
(require 'records-rss)
(require 'records-hs)

;;;
;;; Internal variables - users shouldn't change
;;; The defvar is for internal documentation.
;;;
(defconst records-version "1.4.8")

(defvar records-subject-table (make-vector 127 0)
  "List of subjects for records subject completion.
Reloaded by loading the records-index file.")

(defvar records-date-regexp ""
  "Regexp matching a records date. Internal variable.")

(defvar records-tag-regexp ""
  "Regexp matching a records tag. Internal variable.")

(defvar records-date-tag-regexp ""
  "Regexp matching links in a record index. Internal variable.")

(defvar records-day-length 0
  "The length of a records file day. Internal variable.")

(defvar records-month-length 0 
  "The length of a records file month. Internal variable.")

(defvar records-date-length 0
  "The length of a records file date. Internal variable.")

(defvar records-date-order '(() () ())
  "The order of a records date. Internal variable.")

(defvar records-date '((day 0 0) (month 0 0) (year 0 0))
  "The second and third values in each sublist are the start point
and the length of each component in a date. Internal variable.")

(defvar records-todo-begin-regexp ""
  "Either the todo copy or move regexp. Internal variable.")

(defvar records-todo-move-regions nil
  "All the regions that have to be removed from the preivous records file.
Internal variable.")

(defvar records-history nil
  "List of records a user has visited. Elements of the list consist of args
to records-goto-record. Internal variable.")

(defvar records-initialize nil
  "Has function records-initialize been invoked atleast once.
Internal variable.")

;;;###autoload
(defun records-initialize ()
  "Reads the records init file and sets the records internal variables
like records-date, records-date-length, etc."
  (interactive)
  (if (file-exists-p records-init-file)
      (load records-init-file))
  (setq records-day-length 2)
  (setq records-month-length 2)
  (setq records-date-length 0)

  ;; set records-date-order
  (setcar (nthcdr records-day-order records-date-order) 'day) 
  (setcar (nthcdr records-month-order records-date-order) 'month) 
  (setcar (nthcdr records-year-order records-date-order) 'year) 

  ;; set records-date
  (let ((i 0))
    (mapcar 
     '(lambda (x)
        (let ((len (symbol-value 
                    (intern (concat "records-" (symbol-name x) "-length")))))
          (setcdr (assq x records-date)
                  (list records-date-length len))
          (setq records-date-length
                (+ records-date-length len))
          (setq i (1+ i))))
     records-date-order))

  (setq records-date-regexp 
        (concat "\\(" (let ((i 0) regexp)
                        (while (< i records-date-length)
                          (setq regexp (concat regexp "[0-9]"))
                          (setq i (1+ i))) regexp)
                "\\)"))
  (setq records-tag-regexp "#\\([0-9]+\\|\\)")
  (setq records-date-tag-regexp 
        (concat records-date-regexp "\\(\\|" records-tag-regexp "\\)\\s-"))

  (setq records-todo-begin-regexp
        (concat "\\(^" records-todo-begin-copy-regexp "\\)\\|\\(^"
                records-todo-begin-move-regexp "\\)"))

  ;; do some cleaning up
  (if (and (boundp 'records-dindex-buffer) 
           records-dindex-buffer
           (get-buffer records-dindex-buffer))
      (kill-buffer records-dindex-buffer))

  (if (and (boundp 'records-index-buffer) 
           records-index-buffer
           (get-buffer records-index-buffer))
      (kill-buffer records-index-buffer)))

;; load when interactive
(if (null noninteractive)
    (records-initialize))

(defmacro records-date-count-regexp (&optional date)
  "Regexp matching a date in the records date-index file."
  ( `(if (, date)
         (concat "\\(" (, date) "\\)#\\([0-9]+\\) ")
       (concat records-date-regexp "#\\([0-9]+\\) "))))

(defmacro records-subject-regexp (&optional subject)
  "Regexp matching the beginning of a record."
  ;; TODO: the underline should be of length(subject) + 2
  ;; not easy to do when subject is nil
  (` (if (, subject)
         (concat "^\\* \\(" (, subject) "\\)\n\\-\\-\\-+$")
       ;; "^\\* \\(.*\\)\n\\-+$"
       "^\\* \\(.*\\)\n\\-\\-\\-+$"
       )))

(defmacro records-subject-on-concat (subject)
  "Make subject for records concatenation."
  (` (let ((sub (concat records-subject-prefix-on-concat (, subject)
                        records-subject-suffix-on-concat)))
       (concat sub "\n" (make-string (length sub) ?-) "\n"))))

(defmacro records-date-on-concat (date)
  "Make date for records concatenation."
  (` (let ((d (concat records-date-prefix-on-concat (, date)
                      records-date-suffix-on-concat)))
       (concat d "\n" (make-string (length d) ?-) "\n"))))

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

(defun records-date-equalp (date-a date-b)
  "Are two dates equal?"
  (equal date-a date-b))

(defun records-ndate-equalp (ndate-a ndate-b)
  "Are two normalized dates equal?"
  (equal ndate-a ndate-b))

(defun records-ndate-lessp (ndate-a ndate-b)
  "Returns T if ndate-a is less than ndate-b."
  (or (< (nth 2 ndate-a) (nth 2 ndate-b))
      (and (= (nth 2 ndate-a) (nth 2 ndate-b))
           (< (nth 1 ndate-a) (nth 1 ndate-b)))
      (and (= (nth 2 ndate-a) (nth 2 ndate-b))
           (= (nth 1 ndate-a) (nth 1 ndate-b))
           (< (nth 0 ndate-a) (nth 0 ndate-b)))))

(defun records-add-date (ndate arg)
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

(defun records-sane-date (date &optional no-err)
  "Tests if date is sane. Returns date if so or else error. 
If no-err is set, returns nil if date is bogus."
  (if (not (= (length date) records-date-length))
      (if no-err nil
        (error (format "records-sane-date: %s: bad date" date)))
    ;; else
    (let ((i 0) (ret date))
      (while (< i records-date-length)
        (if (or (< (string-to-char (substring date i)) ?0)
                (> (string-to-char (substring date i)) ?9))
            (if no-err (setq ret nil)
              (error (format "records-sane-date: %s: bad date" date))))
        (setq i (+ i 1))) ret)))

(defun records-file-to-date (&optional file-name)
  "Get the date associated with file-name.
If file-name is not specified, the current buffers file-name is used."
  (if file-name
      ()
    ;; get the file-name of the current buffer
    (if (null buffer-file-name)
        (error "records-file-to-date: buffer has no associated file."))
    (setq file-name (file-name-nondirectory buffer-file-name)))
  ;; check that length of file name is meaningful
  (records-sane-date file-name)
  file-name)

(defun records-denormalize-date (ndate)
  "Get the file name associated with  date.
The ndate is normalized and in (day month year) format."
  (let ((cdate ndate)
        (date (make-string records-date-length ? )))
    (if (= records-year-length 2)
        ;; convert to 2 digit year
        (if (< (nth 2 cdate) 2000)
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
                        (format (concat "%0" (int-to-string len) "d") 
                                (nth i ndate))
                        (substring date (+ start len))))
            (setq i (1+ i))))
       records-date))
    date))

(defun records-normalize-date (date)
  "Returns date in (day month year) format with year in four digits"
  (records-sane-date date)
  (let ((ndate '(0 0 0))
        (i 0))
    (mapcar
     '(lambda (x)
        (setcar (nthcdr i ndate) (string-to-int 
                                  (substring date (nth 1 x) 
                                             (+ (nth 2 x) (nth 1 x)))))
        (setq i (1+ i)))
     records-date)
    (if (= records-year-length 2)
        ;; convert to four digit year
        (if (> (nth 2 ndate) 90)
            (setcar (nthcdr 2 ndate) (+ (nth 2 ndate) 1900))
          (setcar (nthcdr 2 ndate) (+ (nth 2 ndate) 2000))))
    (copy-sequence ndate)))

(defun records-directory-path (date &optional absolute)
  "Get the relative directory path to a records file.
With absolute set, get the absolute path."
  (cond ((= records-directory-structure 0) (if absolute records-directory ""))
        ((= records-directory-structure 1) 
         (concat (if absolute records-directory "..") "/"
                 (substring date (nth 1 (nth 2 records-date))
                            (+ (nth 2 (nth 2 records-date)) 
                               (nth 1 (nth 2 records-date))))))
        ((= records-directory-structure 2)
         (concat (if absolute records-directory "../..") "/"
                 (substring date (nth 1 (nth 2 records-date))
                            (+ (nth 2 (nth 2 records-date))
                               (nth 1 (nth 2 records-date)))) 
                 "/"
                 (substring date (nth 1 (nth 1 records-date))
                            (+ (nth 2 (nth 1 records-date))
                               (nth 1 (nth 1 records-date))))))
        (t 
         (error "records-directory-path: bad records-directory-structure value"))))

(defun records-read-subject (&optional subject)
  "Read the records subject from minibuffer.
Completion is possible."
  (interactive
   (progn 
     (records-index-buffer)        ; initializes records-subject-table if required
     (list 
      (completing-read "Records subject: " 
                       records-subject-table nil nil))))
  
  subject)

(defun records-read-date ()
  "Reads the records date from minibuffer.
Ensure date is reasonable."
  (interactive)
  (let ((date (read-from-minibuffer "Records date: "
                                    (cons (records-file-to-date) 0))))
    (while (not (records-sane-date date t)) ; check for date sanity
      (message (format "%s: bad date" date))
      (sleep-for 1)
      (setq date (read-from-minibuffer "Records date: "
                                       (cons (records-file-to-date) 0))))
    date))

(defun records-add-text-properties (beg end)
  "Make a region read-only, etc.
Look at variable records-subject-read-only.
This function is currently only invoked for a records subject.

Although the region is read-only, it is possible to edit at the beginning of
the subject. This can mess up a records subject if anything but a newline is
inserted. We could close the beginning of the region (see start-close), but
then users would not be able to add newlines before a subject, and it screws up
records-encrypt-record and records-decrypt-record. What we need is that
insertion of any character automatically inserts a newline also. TODO"
  (progn
    (if (not running-xemacs)
        ;; emacs has an of-by-one error
        (setq end (1- end)))
    (add-text-properties beg end '(start-open t))
    (if records-subject-read-only
        (add-text-properties beg end '(read-only records-subject)))))

(defun records-remove-text-properties (s) 
  "Remove the text properties of string in a record.
Called when killing a region in records mode."
  ;; length is probably going to be slow

  (let((inhibit-read-only t))
    (remove-text-properties 0 (length s) '(face nil read-only nil) s)))

(defun records-parse-buffer ()
  "Parses the records buffer and fontifies record subjects etc."

  ;;make the header read-only
  (records-format-read-only-header)
  
  (save-excursion
    (goto-char (point-min))
    ;; goto first record
    (if (records-goto-down-record nil t)
        (let ((modified (buffer-modified-p));; should always be false
              point-pair)
          (while (progn;;  a do-while loop
                   (setq point-pair (records-subject-region))
                   ;; fontify region, make read-only etc.
                   (records-add-text-properties (first point-pair) 
                                                (second point-pair))
                   ;; goto next record - returns nil when no more exist
                   (records-goto-down-record)))
          (and (not modified) (buffer-modified-p)
               (set-buffer-modified-p nil))))))

(defun records-make-link (subject date tag)
  "Make a records link."
  (concat "link: "
          (records-directory-path date) "/"
          date "#" tag "* " subject))

(defun records-get-link-info(link)
  "Given a link, gets its subject, date, and tag."

  (string-match (concat "<\\(.*\\)/\\([^/#]+\\)\\(" records-tag-regexp 
                        "\\* \\(.*\\)\\|\\)>")) 

  (let(date tag subject)
  
    
    (setq subject (if (match-beginning 5) 
                      (buffer-substring-no-properties (match-beginning 5) 
                                                      (match-end 5))))

    (setq date (buffer-substring-no-properties (match-beginning 2) 
                                                  (match-end 2)))
    (setq tag (if (match-beginning 4) 
                  (buffer-substring-no-properties (match-beginning 4) 
                                                  (match-end 4))))
    
    (list subject date tag)))

(defun records-goto-subject ()
  "Goto the subject on the current record and return the subject."
  (beginning-of-line)
  (if (looking-at "^\\s-*-+\\s-*$")
      (progn 
        (previous-line 1)
        (beginning-of-line)))
  (if (looking-at (records-subject-regexp))
      ()
    (if (records-goto-up-record) 
        ()
      (error "records-goto-subject: no subject found.")))
  (buffer-substring-no-properties (match-beginning 1) (match-end 1)))

(defmacro records-tag (tag)
  (` (if (> (length (, tag)) 0) (concat "#" (, tag)) "")))

(defun records-subject-tag (&optional no-str)
  "Returns subject#tag of the record where point is located.
If no-str is t, return (subject, tag)."
  (save-excursion
    (let ((subject (records-goto-subject))
          tag)
      (next-line 2)
      (if (re-search-forward records-tag-regexp (point-eoln) t)
          (setq tag (buffer-substring-no-properties 
                     (match-beginning 1) (match-end 1))))
      (if no-str (list subject tag)
        (concat subject (records-tag tag))))))

(defun records-mark-record (&optional arg)
  "Put mark at the end of the current record and point at the beginning
of the record subject. With non-null prefix arg, the point is placed
at the beginning of the record body."
  (interactive "P")
  (let ((point-pair (records-record-region arg)))
    (push-mark (second point-pair) nil t)
    (goto-char (first point-pair))))

(defun records-record-region (&optional arg)
  "Return a pair of points marking the beginning and the end of the current
record. The record marked is the one that contains point or follows
point. With non-null prefix arg, the point is placed at the beginning of the
record body. Note, that the point and the mark in the buffer are not
affected."
  (interactive "P")
  (save-excursion
    (let (begin end)
      (if arg (setq begin (second (records-subject-region))))
      (records-goto-down-record)
      (setq end (point))
      (if begin t 
        (records-goto-up-record)
        (setq begin (point)))
      (list begin end))))

(defun records-subject-region ()
  "Return a pair of points marking the beginning and the end of the current
subject. The record subject marked is the one that contains point or follows
point. Note, that the point and the mark in the buffer are not affected."
  (save-excursion
    (if (null (records-goto-subject))
        (error "records-subject-region: no subject found."))
    (let ((pt (point)))
      (next-line 2)
      (beginning-of-line)
      (if (looking-at "link: .*")
          (progn
            ;;advance to the end of the record
            (re-search-forward records-format-header-end-regexp)))
      
      ;; return beginning and end of subject
      (list pt (match-end 0)))))

(defun records-body-empty-p ()
  "Is the body of the record under point empty?"
  (let ((point-pair (records-record-region t)))
    (save-excursion
      (goto-char (first point-pair))
      (and (looking-at "\\s-*")
           (eq (match-end 0) (second point-pair))))))

(defun records-link ()
  "Returns the records link of the record around the current point."
  (save-excursion
    (if (null (records-goto-subject))
        (error "records-link: no subject found."))
    (next-line 2)
    (beginning-of-line)
    (if (looking-at "link: \\(.*\\)")
        (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))

(defun records-link-as-kill ()
  "Put the records link of the record around the current point in the kill
ring."
  (interactive)
  (kill-new (records-link)))

(defun records-make-record (subject date tag &optional record-body)
  "Make a basic record with it's link name." 
  (if (not (bolp))
      (insert "\n"))
  (let ((opoint (point)))
    (insert "* " subject "\n")
    
    (insert-char ?- (+ (length subject) 2))
    (insert (concat "\n" (records-make-link subject date tag) "\n"))

    (insert "created: "
            (records-timestamp))

    ;;insert the format end
    (insert "\n")
    (insert records-format-header-end-string)
    
    (records-add-text-properties opoint (point))

    ;;output two EOLs before the clear text for better justificiation.
    (insert "\n\n")
    
    (if record-body
        (insert record-body))))

(defun records-free-record (&optional keep-body)
  "Remove the current record. 
With arg., keep the body and remove the subject only."
  (save-excursion
    (let ((inhibit-read-only '(records-subject))
          point-pair)
      (if keep-body 
          (setq point-pair (records-subject-region))
        (setq point-pair (records-record-region)))
      (delete-region (first point-pair) (second point-pair))
      (pop-mark))))

;;;###autoload
(defun records-underline-line ()
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

;; 04/13/1999: http, ftp, mailto and gopher handling code
;;             Thanks to Kaarthik Sivakumar
;; 01/10/2000: dejanews handling code
;;             Thanks to Robert Mihram 
(defun records-goto-link ()
  "Goto the link around point in the records file.
A link can be any of the following. They must be enclosed in <>.
A tag is a number.
1. <dir/date> : a (relative or absolute) pathname 
2. <dir/date#* Subject>
3. <dir/date#tag* Subject>
4. file:// or file://localhost prepended any of the three above
5. http:// or mailto:// or ftp:// gopher://  
   The last case is handled by browse-url-browser-function. 
   Refer to Options/\"Open URL with\" in XEmacs. 
6. <message-id> fetch a article using deja.com.
   Spaces and other funky characters in the url can break this code."
  (interactive)
  (save-excursion
    (if (not (or (looking-at "<")
                 (re-search-backward "<" (point-boln) t)))
        ;; not a link I know about
        (error "records-goto-link: invalid link under point."))
    ;; try to figure out a link
    (cond 
     ((looking-at (concat "<\\(.*\\)/\\([^/#]+\\)\\(" records-tag-regexp 
                          "\\* \\(.*\\)\\|\\)>")) 
      ;; found a link
      (let ((dir (buffer-substring-no-properties (match-beginning 1) 
                                                 (match-end 1)))
            (date (buffer-substring-no-properties (match-beginning 2) 
                                                  (match-end 2)))
            (tag (if (match-beginning 4) 
                     (buffer-substring-no-properties (match-beginning 4) 
                                                     (match-end 4))))
            (subject (if (match-beginning 5) 
                         (buffer-substring-no-properties (match-beginning 5) 
                                                         (match-end 5)))))
        (cond 
         ((string-match "^http:\\|^mailto:\\|^ftp:\\|^gopher:" dir)
          (funcall browse-url-browser-function (concat dir "/" date)))
         ;; if "file://" or "file://localhost" is present 
         ;; at the beginning of dir, strip it ... guess why?
         (t (if (string-match "^file://\\(localhost\\|\\)" dir)
                (setq dir (substring dir (match-end 0))))
            (records-goto-record subject date tag nil nil nil nil dir)))))
     ((looking-at "<\\(\\(http\\|mailto\\|ftp\\|gopher\\):[^>]+\\)>")
      (funcall browse-url-browser-function 
               (buffer-substring-no-properties (match-beginning 1) 
                                               (match-end 1))))
     ((looking-at "<\\([^ \t\n>]+\\)>")
      (funcall browse-url-browser-function 
               (concat
                "http://search.dejanews.com/msgid.xp?MID=%3C"
                (buffer-substring-no-properties (match-beginning 1) 
                                                (match-end 1))
                "%3E&format=threaded")))
     (t (error "records-goto-link: invalid link under point.")))))

(defun records-goto-mouse-link (e)
  "When mouse is clicked on a link, goto the link. 
When mouse is clicked anywhere else, invoke the default mouse binding."
  (interactive "e")
  (condition-case nil
      (progn (mouse-set-point e)
             (records-goto-link))
    (error (funcall (global-key-binding [(button2)]) e)))
  )

(defun records-goto-record (subject date tag 
                                    &optional no-hist no-switch todo no-error dir)
  "Goto the record on date with subject and tag.
If subject is nil, goto date only.
If no-hist is t, then don't add this link to the records-history list.
If no-switch is t, then do not switch to the new records buffer.
Instead, the buffer is made ready for editing (via set-buffer).
If no-switch is 'other, then switch to the new records buffer in another 
window. If todo is t, then invoke records-get-todo when a record-less file is 
being visited. If todo is not nil and not t, ask user whether records-get-todo 
should be called. If no-error is t, do not signal error, if the record is 
not found. If dir is specified, then the file is assumed to be \"dir/date\"."
  (if (null dir)
      (setq dir (records-directory-path date t)))
  (let ((file (concat dir "/" date))
        found)

    (if (not (file-directory-p dir))
  
        ;; ask the user if they want to create a directory for the records.
        (if (y-or-n-p (concat "Make directory: " dir " "))
            (make-directory (expand-file-name dir) t)
          (if no-error nil
            (error (concat "record: " file " not found.")))))
    
    (cond ((null no-switch)
           (find-file file))

          ((eq no-switch 'other)
           (find-file-other-window file))
          
          (t
           (set-buffer (find-file-noselect file))))

    ;; handler for new records files
    (if (null 
         (save-excursion 
           (records-dindex-goto-date date t)))
        (progn
          (records-format-new-file)))
    
    ;; handler for new records files (with todos)
    (if (and todo
             (null 
              (save-excursion 
                (records-dindex-goto-date date t))))
        (if (or (eq todo t) 
                (y-or-n-p "Invoke records-get-todo (default n): "))
            (records-get-todo date)))

    (if (null subject)
        ;; this is for going to a specific day and not a record
        (setq found t)
      (goto-char (point-min))
      ;; TODO: this search forward will fail to get to the right spot
      ;; if a string matching this regexp has been added to a
      ;; previous subject in the file. We should check for the records subject.
      (if (re-search-forward 
           (concat "^link: .*" date "#" tag "\\* " subject "") 
           (point-max) t)
          ;; found
          (progn (setq found t) (records-goto-subject))
        (if no-error
            nil
          (error 
           (concat "Records subject: " subject 
                   (if (> (length tag) 0) (concat ", tag: " tag))
                   " not found.")))))
    
    ;; support for goto last record
    (if (and found (null no-hist))
        (let ((hist (list subject date tag t nil nil nil dir))
              hist-last)
          (if (equal hist (car records-history))
              ();; don't add identical records.
            (setq records-history (cons hist records-history))
            (setq hist-last 
                  (nthcdr (- records-history-length 1) records-history))
            (if hist-last
                (setcdr hist-last nil)))))))

(defun records-goto-up-record (&optional subject)
  "Go to the beginning of the current record.
If the point is currently at the beginning of a record, go to the record above.
If subject is specified, go up to the beginning of a record with subject."
  (interactive)
  (re-search-backward (records-subject-regexp subject) (point-min) 'start))

(defun records-goto-down-record (&optional subject on-next)
  "Go to the beginning of the next record. 
If subject is specified, go down to the beginning of a record with subject.
If on-next is t, then don't move if we are at the beginning of a subject."
  (interactive)
  (let ((regexp (records-subject-regexp subject)))
    (if (and (null on-next) 
             (looking-at regexp))
        (goto-char (match-end 0)))
    ;; find next record and leave cursor at section beginning
    (if (re-search-forward regexp (point-max) 'end)
        (goto-char (match-beginning  0)))
    ))

;;;###autoload
(defun records-goto-index (&optional arg subject date tag no-error)
  "If arg is nil or zero, goto the index on date and tag.
With positive arg, go to the index arg-times next to date and tag.
With negative arg, go to the index arg-times previous to date and tag.
Returns the new (date, tag) if found."
  (interactive "P")
  (if (not (and subject date))
      ;; initialize subject date and tag
      (let ((subject-tag (records-subject-tag t)))
        (setq subject (nth 0 subject-tag))
        (setq tag (nth 1 subject-tag))
        (setq date (records-file-to-date))))
  (if (records-index-goto-subject subject (interactive-p) no-error)
      (records-index-goto-relative-date-tag arg date tag)))
  
(defun records-goto-relative-day (&optional arg no-switch todo)
  "With positive arg, go arg days ahead of current record's date. 
With negative arg, go arg days behind current record's date.
The no-switch and todo arguments are passed to records-goto-record."
  (interactive "P")
  (let* ((ndate (records-normalize-date (records-file-to-date)))
         (new-ndate (records-add-date ndate arg))
         (new-date (records-denormalize-date new-ndate)))
    (records-goto-record nil new-date "" nil no-switch todo)))

(defun records-goto-prev-day (&optional arg no-switch)
  "Go to the records file of the previous day.
With numeric prefix arg. go that many days back.
See also records-goto-prev-record-file."
  (interactive "P")
  (records-goto-relative-day (if arg (- arg) -1) no-switch 
                             records-todo-prev-day)

  (records-show-calendar-auto))

(defun records-goto-next-day (&optional arg no-switch)
  "Go to the records file of the next day.
With numeric prefix arg. go that many days forward.
See also records-goto-next-record-file."
  (interactive "P")
  (records-goto-relative-day (if arg arg 1) no-switch records-todo-next-day)

  (records-show-calendar-auto))

(defun records-todays-date ()
  "Get todays date in the file name format"
  (let ((ndate (nthcdr 3 (decode-time))))
    (setcdr (nthcdr 2 ndate) nil)
    ;; denormalize the date to get the file name
    (records-denormalize-date ndate)))

;;;###autoload
(defun records-goto-today ()
  "Go to the records file of today."
  (interactive)
  (records-goto-record nil (records-todays-date) "" nil nil
                       records-todo-today)

  (records-show-calendar-auto))

(defun records-goto-relative-record-file (&optional arg no-switch no-error)
  "With positive arg, go arg files ahead of current records file. 
With negative arg, go arg files behind of current records file.
Returns the new date."
  (interactive "P")
  (let ((new-date
         (save-excursion 
           (nth 0
                (records-dindex-goto-relative-date arg 
                                                   (records-file-to-date))))))
    (if (null new-date)
        (if (null no-error)
            (error
             (concat "records-goto-relative-record-file: " 
                     (if (> arg 0) "next" "previous") 
                     " record file not found.")))
      (records-goto-record nil new-date "" nil no-switch))
    new-date))

(defun records-goto-prev-record-file(&optional arg no-switch no-error)
  "Go to the previous records file. With arg. go that many records files back.
Returns the new date. See also records-goto-prev-day."
  (interactive "P")
  (records-goto-relative-record-file (if arg (- arg) -1) no-switch no-error))

(defun records-goto-next-record-file(&optional arg no-switch no-error)
  "Go to the next records file. With arg. go that many records files forward.
Returns the new date. See also records-goto-next-day."
  (interactive "P")
  (records-goto-relative-record-file (if arg arg 1) no-switch no-error))

(defun records-goto-relative-record (&optional arg subject date tag no-switch 
                                               no-error)
  "If arg is nil or zero, goto the record on subject date and tag.
With positive arg, goto the record arg-times next to date and tag.
With negative arg, goto the record arg-times previous to date and tag.
Returns the new (date, tag) if found."
  (interactive "P")
  (if (not (and subject date))
      ;; initialize subject date and tag
      (let ((subject-tag (records-subject-tag t)))
        (setq subject (nth 0 subject-tag))
        (setq tag (nth 1 subject-tag))
        (setq date (records-file-to-date))))
  (let ((date-tag 
         (save-excursion
           (records-goto-index arg subject date tag no-error))))
    (if date-tag
        ;; goto the record
        (records-goto-record subject (nth 0 date-tag) (nth 1 date-tag) nil 
                             no-switch nil no-error)
      (if (null no-error)
          (error (concat "records-goto-relative-record: " 
                         (if (> arg 0) "next" "previous") 
                         " record not found."))))
    ;; return value
    date-tag))

(defun records-goto-prev-record (&optional arg subject date tag 
                                           no-switch no-error)
  "Find the previous record on subject starting from date and tag.
Returns the new (date, tag) if found."
  (interactive "P")
  (records-goto-relative-record (if arg (- arg) -1) subject date tag no-switch 
                                no-error))

(defun records-goto-next-record (&optional arg subject date tag 
                                           no-switch no-error)
  "Find the next record on subject starting from date and tag.
Returns the new (date, tag) if found."
  (interactive "P")
  (records-goto-relative-record (if arg arg 1) subject date tag no-switch 
                                no-error))

(defun records-goto-last-record ()
  "Go back to the last record file visited.
Identical record files are not put in the history consecutively."
  (interactive)
  (let ((link (car (cdr records-history))))
    (or link
        (error "records-goto-last-record: this is the first record."))
    (setq records-history (cdr records-history))
    (apply 'records-goto-record link)))

(defun records-insert-record (&optional subject record-body)
  "Insert a new record for the current date. Asks for the subject."
  (interactive)

  (let* ((subject (if subject
                      subject (call-interactively 'records-read-subject)))
         (date (records-file-to-date))
         (tag "")
         (prev-modified (buffer-modified-p)))

    ;; currently, we don't allow a record insertion if another record with 
    ;; the same subject exists below this record.
    (save-excursion
      (if (records-goto-down-record subject t)
          (error 
           (concat "records-insert-record: can't insert out-of-order record: "
                   subject))))

    ;; check if another record with same subject exists above 
    ;; to get a new tag value
    (save-excursion
      (if (records-goto-up-record subject)
          ;; get tag
          (setq tag (int-to-string (1+ (string-to-int 
                                        (nth 1 (records-subject-tag t))))))))

    ;; add a records index entry 
    (records-index-insert-record subject date tag)

    ;; add the date to the date-index
    (records-dindex-insert-record date)

    ;; now make the record body
    (records-make-record subject date tag record-body)
    
    (if (not prev-modified)
        (save-buffer))))

(defun records-delete-empty-record-file ()
  "Delete current buffer and its associated file if the buffer is empty 
and not modified."
  (if (and (not (buffer-modified-p)) (eq (buffer-size) 0))
      (let* ((bname (buffer-name))
             (dir-name (records-directory-path bname t)))
        (kill-buffer nil)
        (delete-file (concat dir-name "/" bname)))))

(defun records-delete-record (&optional keep-body no-prompt)
  "Delete the current record for the current date.
With arg, removes the subject only."
  (interactive "P")
  (let* ((date (records-file-to-date))
         (subject-tag (records-subject-tag t))
         (subject (nth 0 subject-tag))
         (tag (nth 1 subject-tag))
         (prev-modified (buffer-modified-p)))

    
    (if (if no-prompt;; prompt?
            t (y-or-n-p (concat "Delete record '"
                                subject
                                "' created on "
                                (records-metainfo-get "created")
                                " ")))
        (progn

          ;;need to remove all read-only properties first.
          (records-format-read-only-remove-from-record)
          
          ;; remove the record subject and optionally the body
          (records-free-record keep-body)
          (if (not prev-modified)
              (save-buffer))
          ;; remove the date from the date-index
          (records-dindex-delete-record date)
          ;; remove the records index entry
          (records-index-delete-record subject date tag)
          ;; remove empty record and file
          (records-delete-empty-record-file)))))
    
(defun records-rename-record ()
  "Renames the subject of the current record for the current date."
  (interactive)
  (records-delete-record 'keep-body)
  (records-insert-record))

(defun records-move-record (&optional date)
  "Move the current record to date. Prompts for date."
  (interactive)
  (if (null date)
      (setq date (records-read-date))
    (records-sane-date date))
  (let* ((subject-tag (records-subject-tag t))
         (subject (nth 0 subject-tag))
         (point-pair (records-record-region t))
         (record-body 
          (buffer-substring (first point-pair) (second point-pair))))
    (save-excursion
      (records-goto-record nil date "" nil t nil)
      (goto-char (point-max))
      (records-insert-record subject record-body)))
  (records-delete-record nil t))

(run-hooks 'records-load-hooks)

(provide 'records)