;; $Id: records-rss-publish.el,v 1.2 2001/05/18 15:37:40 burtonator Exp $

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (burton@openprivacy.org)

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: 
;; Version: 1.0.0

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
;; Support for publishing large amounts of text via RSS.  This basically takes
;; large chunks of text and pulls out one paragraph for RSS publication, then it
;; published the rest of the content to your website.


;;; TODO:

;; --write a records-rss-publish-snarf-message which uses the subject of the message
;;   as the title.

;; - need to be smart enought to remove PGP info.

(defvar records-rss-publish-introduction-minlength 200
  "Minimum number of characters that must appear in an introduction.")

(defun records-rss-publish(subject title description)
  "Publish the description via RSS."
  
  ;;get the introduction paragraph to use
  (save-excursion
    (let(url introduction)
      
      (setq introduction (records-rss-publish-get-introduction description))
      
      ;;publish this document get the URL it will be published to.
      (setq url (publish title description))
      
      (records-goto-today)

      (set-buffer (find-file-noselect
                   (records-util-get-filename
                    (records-todays-date))))
      
      (end-of-buffer)
      (records-rss-create-record subject title url)
      
      (insert introduction)
      
      ;;automatically export this record into the RSS index.

      (records-rss-export-current-buffer))))

(defun records-rss-publish-get-introduction(description)
  "Given a description, get a valid introduction string."
  
  ;;if the introduction it too small try to build a bigger paragraph
  (let((introduction ""))

    (if(< (length description) records-rss-publish-introduction-minlength)
        (progn
          ;;if the description is less than out minimum length, just default to
          ;;that.
          (setq introduction description))

      ;;slse, we need to compute an introduction based on the description
      (assert (string-match "^[ ]*$" description records-rss-publish-introduction-minlength)
              nil "Could not find paragraph beginning")
      
      (let(start end)
        (setq start 0)
        
        (setq end (match-beginning 0))
        
        (setq introduction (concat introduction (substring description start end)))))

    introduction))
  
(defun records-rss-publish-region(start end subject title)
  "Publish the given region to RSS via records."
  (interactive
   (list
    (region-beginning)
    (region-end)
    (records-util-completing-read-subject)
    (read-string "Title: ")))

  (let(url description introduction)

    (setq description (buffer-substring-no-properties start end))
    
    (records-rss-publish subject title description)))

(defun records-rss-publish-snarf-message(subject)
  "Snarf the messages in this buffer and use the 'Subject: ' line as the title."
  (interactive
   (list
    (records-util-completing-read-subject)))
  
  (save-excursion
    (let(title address description start end)
      (beginning-of-buffer)

      ;;get the title based on the subject.
      (setq title (records-rss-publish-snarf-message-get-title))

      (setq address (records-rss-publish-snarf-message-get-address))
      
      ;;just search for the start...
      (assert (re-search-forward "^[ ]*$" nil t)
              nil "Could not find beginning of message")

      (assert (re-search-forward "[\n ]*" nil t)
              nil "Could not find first paragraph")

      (setq start (match-end 0))

      ;;don't include the signatuyre.  We need to regexp for "-- "
      (if (re-search-forward "^-- $" nil t)
          (setq end (match-beginning 0))
        (setq end (point-max)))
      
      (setq description (buffer-substring-no-properties start end))

      ;;now add the e-mail to this description so we know where it was sent to...
      (setq description (concat "\n"
                                (format "-- This is an e-mail message sent to %s --" address)
                                "\n\n"
                                description))
      
      (records-rss-publish subject title description)))

  (message "Snarfed message and exported it as RSS."))

(defun records-rss-publish-snarf-message-get-title()
  "Get the title based on it's Subject."
  
  (records-rss-publish-snarf-message-get-header "Subject" "Title: "))

(defun records-rss-publish-snarf-message-get-address()
  "Get the e-mail address based on its To field."
  
  (records-rss-publish-snarf-message-get-header "To" "Address: "))


(defun records-rss-publish-snarf-message-get-header(name prompt)
  "Get the value of the given header using `prompt' if it isn't available."

  (save-excursion
    (let(value regexp)
      (beginning-of-buffer)

      (setq regexp (concat "\\(" name ": \\)\\(.*$\\)"))
      
      ;;get the title based on the subject.
      (if (re-search-forward regexp nil t)
          (setq value (match-string 2))
        (setq value (read-string prompt)))

      value)))
  

(provide 'records-rss-publish)
