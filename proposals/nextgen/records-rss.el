;;; records-rss.el --- RSS support for Records

;; $Id: records-rss.el,v 1.3 2001/05/14 06:09:21 burtonator Exp $

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
;; This package exports RSS records so that you can publish them onto websites.
;;
;; RSS record are just like normal records except they contain required metainfo
;; for publication and export into RSS format.
;;
;; Basically it exports X records into an 'index' file which contains the users
;; latest RSS records.  All other RSS records are saved into a DATE.rss file in
;; the same export directory.  A contents.rss file contains all the dated
;; records files so that navigation based on dates is supported.   
;;

;;; Format:
;;
;; RSS export is done with the modern RSS 1.0 format:
;;
;; http://groups.yahoo.com/group/rss-dev/files/namespace.html
;;
;; In order for Emacs to parse this out correctly it uses comment tags so that
;; we don't have to embed XML directly lisp.  Since these tags are just XML
;; comments this does not break the XML format at all.
;;
;; Current tags:
;;
;; - <!-- BEGIN ITEMS -->
;;
;;   Marks the beginning of the RSS items 
;;
;; - <!-- END ITEMS -->
;;
;;   Marks the end of the RSS items 

;;; TODO:
;;
;; - Support for images
;;
;; - Support for optional channel info (title, description, etc)
;;
;; - Use the link so that RSS records are not exported multiple times.
;;
;; - Incorporate indentation.

(defvar records-rss-export-directory (concat records-directory "/rss")
  "Main directory for RSS export.")

(defvar records-rss-index-file (concat records-rss-export-directory "/index.rss")  
  "File used for RSS index output.")

(defconst records-rss-tag-begin-items "<!-- BEGIN ITEMS -->"
  "Tag which marks the beginning of RSS items.")

(defconst records-rss-tag-end-items "<!-- END ITEMS -->"
  "Tag which marks the end of RSS items.")


(defun records-rss-create-record(subject title url)
  "Create an RSS compatible record."
  (interactive
   (list
    (completing-read "Subject: " 
                     records-subject-table)
    (read-string "Title: ")
    (read-string "URL: ")))

  (records-insert-record subject)

  (records-metainfo-set "title" title)
  (records-metainfo-set "url" url)
  (records-type-set "rss"))

(defun records-rss-export-current-buffer()
  "Export the current buffer to RSS format."
  (interactive)

  (records-format-require-version "1.0.1")
  
  (records-rss-init)
  
  (save-excursion
    (let((count 0))
      (beginning-of-buffer)

      ;;search for RSS metainfo tags
      (while (re-search-forward "^type: rss$" nil t)
        (let(record-link title url description buffer)

          (setq count (1+ count))

          (setq title (records-metainfo-get "title"))
          (setq url (records-metainfo-get "url"))
          (setq record-link (records-metainfo-get "link"))

          
          (setq buffer (find-file-noselect records-rss-index-file))

          (records-rss-init-buffer buffer)

          (records-rss-export-record record-link
                                     title
                                     url
                                     "FIXME: description... asdf"
                                     buffer)

          ))

      (message "Exported %i RSS record(s)." count)
    
    ))


  (records-rss-save))

(defun records-rss-save()
  "Save any buffers that RSS export might have modified."

  ;;save the index buffer
  (save-excursion
    (set-buffer (find-file-noselect records-rss-index-file))

    (save-buffer)))

(defun records-rss-export-record(record-link title url description buffer)
  "Export the current record and output the XML to the buffer.  "

  (set-buffer buffer)

  (insert "<item>\n")

  (insert "<!-- record-link: " record-link " -->")
  (insert "\n")

  
  ;;title
  (records-rss-insert-element "title" title)
  
  ;;link/url
  (records-rss-insert-element "link" url)
  
  ;;description
  (records-rss-insert-element "description" description)
  
  (insert "</item>\n"))

(defun records-rss-insert-element(name value)
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

  (beginning-of-buffer)
  
  (save-excursion
    (beginning-of-buffer)

    (if (not (re-search-forward "<?xml" nil t))
        (progn

          (beginning-of-buffer)

          ;;doesn't have XML information... insert it.
          (insert "<?xml version=\"1.0\">\n")

          ;;FIXME: need output channel information

          (insert "<rdf:RDF ")
          (insert "xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" ")
          (insert "xmlns=\"http://purl.org/rss/1.0/\"")
          (insert ">")
          (insert "\n\n")

          ;;FIXME: insert <channel> information
          
          (insert records-rss-tag-begin-items "\n")

          (insert "\n\n\n")
          
          (insert records-rss-tag-end-items "\n")
          
          (end-of-buffer)

          (insert "</rdf:RDF>"))))

  (re-search-forward records-rss-tag-begin-items)
  (forward-line 1)
  
  )
  

(provide 'records-rss)
