;;;
;;; records-w3.html --- code to generate web pages from records
;;;
;;; $Id: records-w3.el,v 1.1 2000/04/17 21:09:30 ashvin Exp $
;;;
;;; Copyright (C) 2000 John Wiegley
;;;
;;; Author: John Wiegley <johnw@gnu.org>
;;; Created: 14 Feb 2000
;;; Version: 1.0
;;; Keywords: data
;;; X-URL: http://www.emacs.org/~johnw/emacs.html
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

(defconst homepage-version "1.0"
  "This version of homepage.")

(defgroup homepage nil
  "Code to generate my homepage."
  :group 'applications)

;;; User Variables:

(defcustom homepage-load-hook nil
  "*A hook that gets run after \"homepage.el\" has been loaded."
  :type 'hook
  :group 'homepage)

(defcustom homepage-records-subjects nil
  "*A list of subjects to be output as HTML."
  :type '(repeat (string :tag "Subject"))
  :group 'homepage)

(defcustom homepage-html-directory nil
  "*Directory in which to store homepages, by default."
  :type 'file
  :group 'homepage)

;;; Internal Variables:

;;; User Functions:

(defun homepage-start-html (filename &optional title)
  (let (body)
    (with-current-buffer (find-file-noselect filename)
      (erase-buffer)
      (insert "<HEAD>\n")
      (insert (format "<TITLE>%s</TITLE>\n" (or title filename)))
      (insert "</HEAD>\n")
      (insert "<BODY>\n")
      (setq body (point-marker))
      (insert "</BODY>\n")
      (insert "</HTML>\n"))
    body))

(defun homepage-insert-bare-html (text)
  "Insert TEXT at point, which is HTML having no paragraph delimeters."
  (let ((beg (point)))
    (insert text)
    (save-excursion
      (save-restriction
	(narrow-to-region beg (point))
	(goto-char beg)
	(while (re-search-forward "^\\s-*$" (1- (point-max)) t)
	  (replace-match "<P>"))))))

;;;###autoload
(defun batch-generate-homepage ()
  (load (car command-line-args-left))
  (setq command-line-args-left (cdr command-line-args-left))
  (records-initialize)
  (homepage-records-to-html (car command-line-args-left)
			    (cdr command-line-args-left)))

;;;###autoload
(defun homepage-records-to-html (&optional directory categories flat days)
  (interactive)
  (let ((include (or categories homepage-records-subjects))
	(now (records-normalize-date (records-todays-date)))
	tocs bodies)
    (save-excursion
      (records-goto-today)
      (condition-case error
	  (while t
	    (goto-char (point-max))
	    (while (not (bobp))
	      (records-goto-up-record)
	      (let ((link (records-link))
		    topic html-file toc-file reg title body date)
		(when (and (string-match
			    "/\\([0-9]+\\)#\\([0-9]*\\)\\*\\s-+\\(.+\\)>\\'" 
                            link)
			   include (member (match-string 3 link) include))
		  (setq date (match-string 1 link)
			topic (match-string 3 link)
			html-file (match-string 2 link))
		  (if (and days
			   (records-ndate-lessp
			    (records-add-date
			     (records-normalize-date date) days) now))
		      (error "Date is beyond range")
		    (unless (and html-file
				 (> (length html-file) 0))
		      (setq html-file "0"))
		    (while (string-match "\\s-+" topic)
		      (setq topic (replace-match "%20" t t topic)))
		    (setq html-file
			  (format "%s/%s-%s-%s.html"
				  (or directory homepage-html-directory)
				  topic date html-file)
			  toc-file
			  (format "%s/%s.html"
				  (or directory homepage-html-directory)
				  topic)
			  reg (records-record-region))
		    (message "Formatting %s" html-file)
		    (unless flat
		      (add-to-list 'bodies html-file))
		    (add-to-list 'tocs toc-file)
		    (save-excursion
		      (goto-char (car reg))
		      (search-forward ">\n\n")
		      (setcar reg (point))
		      (setq title (buffer-substring (car reg)
						    (line-end-position)))
		      (forward-line)
		      (setq body (if (and (not flat)
					  (looking-at "^\\s-*$"))
				     (buffer-substring (1+ (match-end 0))
						       (cadr reg))
				   (buffer-substring (car reg) (cadr reg)))))
		    (unless (get-file-buffer toc-file)
		      (homepage-start-html toc-file))
		    (with-current-buffer (get-file-buffer toc-file)
		      (let ((header (format "<H4>%s</H4>" date))
			    (item (format "<LI><A HREF=\"%s\">%s</A>\n"
					  (file-name-nondirectory html-file)
					  title))
			    end-list-p)
			(goto-char (point-min))
			(unless (re-search-forward (concat header "\n<UL>\n") 
                                                   nil t)
			  (search-forward "<BODY>\n")
			  (insert header "\n<UL>\n")
			  (setq end-list-p t))
			(if (not flat)
			    (insert item)
			  (insert "<LI>\n")
			  (homepage-insert-bare-html body))
			(if end-list-p
			    (insert "</UL>\n"))))
		    (unless flat
		      (let ((body-loc (homepage-start-html html-file title)))
			(with-current-buffer (marker-buffer body-loc)
			  (goto-char body-loc)
			  (homepage-insert-bare-html body))))))))
	    (records-goto-prev-record-file))
	(error nil)))
    (let ((to tocs))
      (while to
	(with-current-buffer (get-file-buffer (car to))
	  (goto-char (point-min))
	  (search-forward "<BODY>\n")
	  (sort-subr
	   t
	   (function
	    (lambda ()
	      (if (re-search-forward "^<H4>" nil t)
		  (goto-char (match-beginning 0))
		(goto-char (point-max)))))
	   (function
	    (lambda ()
	      (re-search-forward "^</UL>\\(\n<\\(H4\\|/BODY\\)>\\)")
	      (goto-char (match-beginning 1))))
	   (function
	    (lambda ()
	      (if (re-search-forward "^<H4>\\([^<]+\\)</H4>" nil t)
		  (match-string 1))))))
	(setq to (cdr to))))
    (let ((files (append bodies tocs)))
      (while files
	(with-current-buffer (get-file-buffer (car files))
	  (save-buffer)
	  (kill-buffer (current-buffer)))
	(setq files (cdr files))))))

;;; Internal Functions:

(provide 'homepage)

(run-hooks 'homepage-load-hook)

;;; homepage.el ends here

