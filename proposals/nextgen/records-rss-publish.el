;;; FILE.el --- ADD BASIC DESCRIPTION

;; $Id: records-rss-publish.el,v 1.1 2001/05/17 02:15:27 burtonator Exp $

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

;; write a records-rss-publish-snarf-message which uses the subject of the message
;; as the title.

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

  ;;FIXME: if the introduction it too small try to build a bigger paragraph
  
  (if (string-match "^[ ]*$" description)
      (let(start end)

        (setq start 0)
        
        (setq end (match-beginning 0))
        
        (setq introduction (substring description start end)))
        
    ;;if we can't find a paragraph break in this region just use the default
    (setq introduction description)))
  
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
  "Snarf the messages in this buffer and use the 'Subject: ' line as the
title."
  (interactive
   (list
    (records-util-completing-read-subject)))
  
  (save-excursion
    (let(title description start end)
      (beginning-of-buffer)

      ;;get the title based on the subject.
      (if (re-search-forward "\\(Subject: \\)\\(.*$\\)" nil t)
          (setq title (match-string 2))
        (setq title (read-string "Title: ")))

      ;;just search for the start...
      (assert (re-search-forward "^[ ]*$" nil t)
              nil "Could not find beginning of message")

      (assert (re-search-forward "[\n ]*" nil t)
              nil "Could not find first paragraph")

      (setq start (match-end 0))

      (setq end (point-max))

      (setq description (buffer-substring-no-properties start end))

      (records-rss-publish subject title description))))

(provide 'records-rss-publish)
