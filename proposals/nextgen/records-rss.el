;;; records-rss.el --- RSS support for Records

;; $Id: records-rss.el,v 1.1 2001/05/13 02:08:18 burtonator Exp $

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

(defvar records-rss-export-directory (concat records-directory "/rss")
  "Main directory for RSS export.")

(defvar records-rss-index-file (concat records-rss-export-directory "/index.rss")  
  "File used for RSS index output.")

(defun records-rss-create-record(subject title url)
  "Create an RSS compatible record."
  (interactive
   (list
    (completing-read "Subject: " 
                     records-subject-table)
    (read-string "Title: ")
    (read-string "url: ")))

  (records-insert-record subject)

  (records-metainfo-set "title" title)
  (records-metainfo-set "url" url)
  (records-metainfo-set "rss" "true"))

(defun records-rss-export-current-buffer()
  "Export the current buffer to RSS format."
  (interactive)

  (records-format-require-version "1.0.1")
  
  (records-rss-init)
  
  (save-excursion
    (beginning-of-buffer)

    ;;search for RSS metainfo tags
    (while (re-search-forward "^rss: true$" nil t)

      (let(title url description buffer)

        (setq title (records-metainfo-get "title"))
        (setq url (records-metainfo-get "url"))

        (setq buffer (find-file-noselect records-rss-index-file))

        (records-rss-init-buffer buffer)
        
        (records-rss-export-record title url "description... asdf"
                                   buffer)
        
        )))

  ;;make a symbolic link between the current date and index.rss
  )

(defun records-rss-export-record(title url description buffer)
  "Export the current record and output the XML to the buffer.  "

  (set-buffer buffer)

  (insert "<item>\n")

  ;;title
  (record-rss-insert-element "title" title)
  
  ;;link/url
  (record-rss-insert-element "link" url)
  
  ;;description
  (record-rss-insert-element "description" description)
  
  (insert "</item>\n"))

(defun record-rss-insert-element(name value)
  "Build an XML element and insert it into the current buffer."

  (insert "<" name ">")

  (insert value)

  (insert "</" name ">")

  (insert "\n"))

(defun records-rss-init()
  "Perform any optimization that RSS needs."

  ;;make sure the directory exists.
  (if (not (file-directory-p records-rss-export-directory))
      (make-directory records-rss-export-directory))


  )

(defun records-rss-init-buffer(buffer)
  "Make sure the given buffer has all the necessary RSS/XML information."

  ;;make sure we have beginning XML information

  (set-buffer buffer)
  
  (save-excursion
    (beginning-of-buffer)

    (if (not (re-search-forward "<?xml" nil t))
        (progn

          (beginning-of-buffer)

          ;;doesn't have XML information... insert it.
          (insert "<?xml version=\"1.0\">\n")

          ;;FIXME: need output channel information

          (insert "<rdf:RDF ")
          (insert "xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\ ")
          (insert "xmlns=\"http://purl.org/rss/1.0/\"")
          (insert ">")

          (end-of-buffer)

          (insert "</rdf:RDF>")))))
  

(provide 'records-rss)
