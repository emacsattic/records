;;; records-rss.el --- RSS support for Records

;; $Id: records-rss.el,v 1.10 2001/05/18 15:37:40 burtonator Exp $

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
;; - Support for XSLT after exporting.
;;
;; - Support for images
;;
;; - Use the link so that RSS records are not exported multiple times.
;;
;; - If a record ALREADY exists in the RSS index delete it and insert a new
;;   record.  This is necessary because if we update a record we want the index
;;   to reflect this.
;;
;; - Is it possible to export RSS in the background???  Maybe when specific
;;   operations are performed.  It only takes a second.
;;
;; - Support images for RSS items.  This might be tough because RSS doesn't
;;   support this and there isn't really an additional namespace I can use.

;;;History:
;;
;; - Mon May 14 2001 01:59 AM (burton@relativity.yi.org): Incorporate
;; indentation so that this can be understood easily.
;;
;; - Mon May 14 2001 02:26 AM (burton@relativity.yi.org): Support for optional
;; channel info (title, description, etc)
;;

(require 'records-rss-publish)

(defcustom records-rss-export-directory (concat records-directory "/rss")
  "Main directory for RSS export."
  :type 'string
  :group 'records-rss)

(defcustom records-rss-channel-title records-owner-name
  "Title to use for RSS channel info."
  :type 'string
  :group 'records-rss)

(defcustom records-rss-channel-description (format "RSS channel for %s (%s)"
                                                   records-owner-name
                                                   records-owner-email)
  "Description to use for RSS channel info."
  :type 'string
  :group 'records-rss)

(defcustom records-rss-images-default ""
  "Default image to use for RSS export."
  :type 'string
  :group 'records-rss)


;;FIXME: maybe I should move this to use some generic metainfo on each subject.
(defcustom records-rss-images-subjects-alist '()
  "Associated list for storing subject to image mappings."
  :type '(alist (cons 'string 'string))
  :group 'records-rss)


(defcustom records-rss-item-default-url "http://www.sourceforge.net/projects/records"
  "Default URL for RSS content.  This is used when you want to export a record
as RSS but don't want to provide a URL.  Use the default if you just want to
export your activity."
  :type 'string
  :group 'records-rss)
  
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
    (read-string "URL: " records-rss-item-default-url)))

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

        (let(record-link subject title url description created buffer)

          (save-excursion
            (setq subject (records-goto-subject)))

          (setq title (records-metainfo-get "title"))
          (setq url (records-metainfo-get "url"))
          (setq record-link (records-metainfo-get "link"))
          (setq created (records-metainfo-get "created"))
          
          (setq description (records-format-get-body))
          
          (save-excursion
            (setq buffer (find-file-noselect records-rss-index-file))
            
            (records-rss-init-buffer buffer)
            
            ;;FIXME: make sure that the link doesn't already exist.
            
            (if (not (records-rss-link-exists record-link))
                (progn 
                  
                  (setq count (1+ count))
                  
                  (records-rss-export-record record-link
                                             subject
                                             title
                                             url
                                             created
                                             description
                                             buffer))))
          ))

      (records-rss-save)
      (message "Exported %i RSS record(s)." count))))

(defun records-rss-link-exists(record-link)
  "Return true if the given link already exists."

  (save-excursion
    (set-buffer (find-file-noselect records-rss-index-file))
    
    (beginning-of-buffer)

    (search-forward record-link nil t)))
  
(defun records-rss-save()
  "Save any buffers that RSS export might have modified."

  ;;save the index buffer
  (save-excursion
    (set-buffer (find-file-noselect records-rss-index-file))

    (save-buffer)))

(defun records-rss-export-record(record-link
                                 subject
                                 title
                                 url
                                 created
                                 description
                                 buffer)
  "Export the current record and output the XML to the buffer.  "

  ;;FIXME: insert the 'created' date
  
  (set-buffer buffer)

  (insert "\n\n")
  
  (insert "<!-- record-link: " record-link " -->")
  (insert "\n")

  (insert "<item>\n")

  ;;title
  (records-rss-insert-element "title" title 1)
  
  ;;link/url
  (records-rss-insert-element "link" url 1)

  ;;add a dublin core 'date' item so that we know when this record was created.

  (records-rss-insert-element "dc:date" created 1)

  ;;(records-rss-insert-element "dc:date" created 1)

  ;;get the image
  (let(image)

    ;;OK.  now try to pull out the images to use for the image-subject alist....
    (setq image (cdr (assoc subject records-rss-images-subjects-alist)))

    (if (and (null image)
             records-rss-images-default)
        (setq image records-rss-images-default))
    
    (records-rss-insert-element "im:image" image))
  
  ;;description

  (insert "<description>")
  (insert "\n\n")

  (insert description)

  (insert "\n\n")
  (insert "</description>")
  
  (insert "</item>\n"))

(defun records-rss-insert-element(name value &optional level)
  "Build an XML element and insert it into the current buffer.  If specified
`level' should contain an integer which species the level at which this node is
nested, default is 0"

  (let(level-string)

    (setq level-string "")

    (if level
        (progn
          (setq level-string (make-string (* level 4) ? ))))
          ;;

    (insert level-string)
    
    (insert "<" name ">")

    (insert value)

    (insert "</" name ">")

    (insert "\n")))

(defun records-rss-init()
  "Perform any optimization that RSS needs."

  ;;make sure the directory exists.
  (if (not (file-directory-p records-rss-export-directory))
      (make-directory records-rss-export-directory)))

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
          (insert "<?xml version=\"1.0\"?>\n")

          (insert "<rdf:RDF ")
          (insert "xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" ")
          (insert "xmlns:dc=\"http://purl.org/dc/elements/1.1/\" ")
          (insert "xmlns:im=\"http://purl.org/rss/1.0/item-images/\" ")
          (insert "xmlns=\"http://purl.org/rss/1.0/\"")
          (insert ">")
          (insert "\n\n")

          ;;insert <channel> information

          (insert "<channel>")
          (insert "\n")

          (records-rss-insert-element "title" records-rss-channel-title 1)

          (insert "<description>")
          (insert "\n")

          (insert records-rss-channel-description)

          (insert "\n")
          (insert "</description>")
          (insert "\n")

          (insert "</channel>")
          (insert "\n")
          
          (insert records-rss-tag-begin-items "\n")

          (insert "\n\n\n")
          
          (insert records-rss-tag-end-items "\n")
          
          (end-of-buffer)

          (insert "</rdf:RDF>"))))

  (re-search-forward records-rss-tag-begin-items)
  (forward-line 1))

(provide 'records-rss)
