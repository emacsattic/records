;;; records-rss.el --- RSS support for Records

;; $Id: records-rss.el,v 1.22 2002/05/27 19:12:26 burtonator Exp $

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
;; - Is it possible to export RSS in the background???  Maybe when specific
;;   operations are performed.  It only takes a second.
;;

;;;History:
;;
;; - Sat May 19 2001 12:50 PM (burton@relativity.yi.org): include additional
;; 'subject' metainfo under a special records namespace.
;;
;; - Sat May 19 2001 12:50 PM (burton@relativity.yi.org): Support images for RSS
;; items.  This might be tough because RSS doesn't support this and there isn't
;; really an additional namespace I can use.
;;
;; - Fri May 18 2001 10:47 PM (burton@relativity.yi.org): Use the link so that
;;   RSS records are not exported multiple times.
;;
;; - Fri May 18 2001 10:47 PM (burton@relativity.yi.org): Support for XSLT after
;;   exporting.
;;
;; - Fri May 18 2001 10:47 PM (burton@relativity.yi.org): Support for images
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
  :type '(repeat
          (cons
           (string :tag "Subject")
           (string :tag "URL")))
  :group 'records-rss)

(defcustom records-rss-item-default-url "http://www.sourceforge.net/projects/records"
  "Default URL for RSS content.  This is used when you want to export a record
as RSS but don't want to provide a URL.  Use the default if you just want to
export your activity."
  :type 'string
  :group 'records-rss)

;; (defcustom records-rss-auto-export nil
;;   "*Automatically export the current buffer when we mark a record as rss."
;;   :type 'boolean
;;   :group 'records-rss)

(defvar records-rss-index-file (concat records-rss-export-directory "/index.rss")
  "File used for RSS index output.")

(defconst records-rss-tag-begin-items "<!-- BEGIN ITEMS -->"
  "Tag which marks the beginning of RSS items.")

(defconst records-rss-tag-end-items "<!-- END ITEMS -->"
  "Tag which marks the end of RSS items.")

(defvar records-rss-export-days 14
  "Number of days over which we should do a full export.")

(defun records-rss-create-record(subject title url)
  "Create an RSS compatible record."
  (interactive
   (list
    (completing-read "Subject: " 
                     records-subject-table)
    (read-string "Title: ")
    (read-string "URL: " records-rss-item-default-url)))

  (records-insert-record subject)

  (records-type-set "rss")
  (records-metainfo-set "url" url)
  (records-metainfo-set "title" title))

(defun records-rss-export-current-buffer(&optional nomessage)
  "Export the current buffer to RSS format and return the number of records
exported.  This should also write a file into ~/.records/records-mode-DATE.rss
containing all records for this date."
  (interactive)

  (when (equal major-mode 'records-mode)
  
    (records-format-require-version "1.0.1")
  
    (records-rss-init)
  
    (save-excursion
      (let((count 0))
        (beginning-of-buffer)

        ;;search for RSS metainfo tags
        (while (re-search-forward "^type: rss$" nil t)

          (let(record-link subject title url description created buffer uuid)

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
            
              (setq count (1+ count))

              (setq uuid (concat (format-time-string "%s") "-" (number-to-string count)))
              
              (records-rss-export-record record-link
                                         subject
                                         title
                                         url
                                         created
                                         description
                                         buffer
                                         uuid))))

        (records-rss-save)

        (when (not nomessage)
          (message "Exported %i RSS record(s)." count))

        count))))

(defun records-rss-export()
  "Try to export the RSS database over the last X days."
  (interactive)

  (message "Exporting RSS records... ")

  ;;backup files first...
  (records-rss-export-backup)
  
  (let(buffer day-index count)
    (save-excursion

      ;;count should add up to the number of records successfully exported.

      (setq count 0)
      
      ;;clear the records buffer...
      
      (setq buffer (find-file-noselect records-rss-index-file))
      
      (set-buffer buffer)

      (erase-buffer)

      ;;this might be redundant
      (records-rss-init-buffer buffer)

      ;;go over each date and export the records...

      (setq day-index records-rss-export-days)
      
      (while (>= day-index 0)
        (let(filename date ndate)

          (setq date (records-add-date (records-normalize-date (records-todays-date))
                                       (- day-index)))

          (setq ndate (records-denormalize-date date))
          
          (setq filename (records-util-get-filename ndate))
          
          (set-buffer (find-file-noselect filename))

          ;;don't do anything if we error out...
          (catch 'error
            (setq count (+ count (records-rss-export-current-buffer))))
          
          (setq day-index (1- day-index))))

      (message "Exported %i total RSS record(s) over the last %i days."
               count
               records-rss-export-days))))

(defun records-rss-export-backup()
  "Backup all necessary files prior to an export."
  
  (records-rss-export-backup-file records-rss-index-file))

(defun records-rss-export-backup-file(filename)
  "Remove the current .bak if it exists, then copy then original file over to
the .bak. "

  (let(backup-filename)

    (setq backup-filename (concat filename ".bak"))

    (if (file-exists-p backup-filename)
        (delete-file backup-filename))

    (copy-file filename backup-filename)))
    
(defun records-rss-link-exists(record-link)
  "Return true if the given link already exists."

  (save-excursion
    (set-buffer (find-file-noselect records-rss-index-file))
    
    (beginning-of-buffer)

    (search-forward record-link nil t)))

(defun records-rss-delete-record-for-update(record-link)
  "Delete the given record so that it can be updated."

  (let(record-begin)

    (when (search-forward record-link nil t)
      
      (setq record-begin (point-at-bol))
      
      (when (re-search-forward "</rss:item>" nil t)
        
        (delete-region record-begin (match-end 0))
        (goto-char record-begin)))))

(defun records-rss-save()
  "Save any buffers that RSS export might have modified."

  ;;save the index buffer
  (save-excursion
    (let((buffer (find-file-noselect records-rss-index-file)))
    
      (set-buffer buffer)

      (save-buffer)

      (kill-buffer buffer))))

(defun records-rss-export-record(record-link
                                 subject
                                 title
                                 url
                                 created
                                 description
                                 buffer
                                 uuid)
  "Export the current record and output the XML to the buffer.  "

  (set-buffer buffer)

  (when (records-rss-link-exists record-link)
    (records-rss-delete-record-for-update record-link))

  (insert "\n\n")
  
  (insert "<!-- record-link: " record-link " -->")
  (insert "\n")

  (insert (concat "<rss:item uuid=\"" uuid "\">\n"))

  ;;title
  (records-rss-insert-element "rss:title" title 1)
  
  ;;link/url
  (records-rss-insert-element "rss:link" url 1 t)

  ;;add a dublin core 'date' item so that we know when this record was created.

  ;;insert the 'created' date

  (records-rss-insert-element "dc:date" created 1)

  ;;(records-rss-insert-element "dc:date" created 1)

  ;;get the image
  (let(image)

    ;;OK.  now try to pull out the images to use for the image-subject alist....
    (setq image (cdr (assoc subject records-rss-images-subjects-alist)))

    (if (and (null image)
             records-rss-images-default)
        (setq image records-rss-images-default))
    
    (records-rss-insert-element "im:image" image 1))

  (records-rss-insert-element "record:subject" subject 1)
  
  ;;description

  (records-rss-insert-element "rss:description" description 0 t t)

  ;;now include XHTML mod-content

  (insert "    <content:items>\n")

  (insert "        <rdf:Bag>\n")
  (insert "            <rdf:li>\n")
  (insert "                <content:item>\n")
  (insert "                    <content:format rdf:resource=\"http://www.w3.org/1999/xhtml\"/>\n")
  (insert "                    <content:encoding rdf:resource=\"http://www.w3.org/TR/REC-xml#dt-wellformed\"/>\n")

  (insert "                    <rdf:value rdf:parseType=\"Literal\" xmlns=\"http://www.w3.org/1999/xhtml\">\n")

  ;;now export the XHTML content here..

  (insert (records-rss-xhtmlify description))
  
  (insert "                    </rdf:value>\n")
  
  (insert "                </content:item>\n")
  (insert "            </rdf:li>\n")
  (insert "        </rdf:Bag>\n")

  (insert "    </content:items>\n")

  (insert "</rss:item>\n"))

(defun records-rss-insert-element(name value &optional level cdata text-format)
  "Build an XML element and insert it into the current buffer.  If specified
`level' should contain an integer which species the level at which this node is
nested, default is 0.  If `cdata' is true then we will output this element as a
CDATA element.  If `text-format' is true, insert \n around beginning and end of
the value.  "

  (let(level-string)

    (setq level-string "")

    (if level
        (progn
          (setq level-string (make-string (* level 4) ? ))))
          ;;

    (insert level-string)

    (if text-format
        (insert "\n"))

    (insert "<" name ">")

    (if cdata
        (insert "<![CDATA["))

    (if text-format
        (insert "\n\n"))
    
    (insert value)

    (if text-format
        (insert "\n\n"))
    
    (if cdata
        (insert "]]>"))
    
    (insert "</" name ">")

    (if text-format
        (insert "\n\n"))

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
          (insert "\n         ")
          (insert "xmlns:dc=\"http://purl.org/dc/elements/1.1/\" ")
          (insert "\n         ")
          (insert "xmlns:im=\"http://purl.org/rss/1.0/item-images/\" ")
          (insert "\n         ")
          (insert "xmlns:record=\"http://records.sourceforge.net/schemas/rss-meta-module/\" ")
          (insert "\n         ")
          (insert "xmlns=\"http://www.w3.org/1999/xhtml\" ")
          (insert "\n         ")
          (insert "xmlns:content=\"http://purl.org/rss/1.0/modules/content/\" ")
          (insert "\n         ")
          (insert "xmlns:rss=\"http://purl.org/rss/1.0/\"")
          (insert ">")
          (insert "\n\n")

          ;;insert <channel> information

          (insert "<rss:channel>")
          (insert "\n")

          (records-rss-insert-element "rss:title" records-rss-channel-title 1)

          (insert "<rss:description>")
          (insert "\n")

          (insert records-rss-channel-description)

          (insert "\n")
          (insert "</rss:description>")
          (insert "\n")

          (insert "</rss:channel>")
          (insert "\n")
          
          (insert records-rss-tag-begin-items "\n")

          (insert "\n\n\n")
          
          (insert records-rss-tag-end-items "\n")
          
          (end-of-buffer)

          (insert "</rdf:RDF>"))))

  (re-search-forward records-rss-tag-begin-items)
  (forward-line 1))

(defun records-rss-xhtmlify(content)
  "Given some `content', assume you want to turn this into XHTML.  This will be
  done by taking logical paragraphs, making them XHTML <p> elements, etc.  When
  complete, return the XHTML content."

  (let((temp-buffer "*records-rss-xhtmlify-temp-buffer*"))

    (save-excursion
      (set-buffer (get-buffer-create temp-buffer))
      
      (erase-buffer)
      
      (insert content)
      
      (records-rss-xhtmlify--entities)
      (records-rss-xhtmlify--quote)
      (records-rss-xhtmlify--bold)
      (records-rss-xhtmlify--items)
      (records-rss-xhtmlify--cite)
      (records-rss-xhtmlify--para)
      (records-rss-xhtmlify--images)

      (buffer-substring (point-min) (point-max)))))

(defun records-rss-xhtmlify--cite()
  "Create anchors to all text based citations."
  (interactive)
  
  (save-excursion
    (beginning-of-buffer)

    (while (re-search-forward "\\[\\([0-9]+\\)\\]" nil t)
      (let(n-cite n-cite-begin n-cite-end cite-url cite-text)
      
        (setq n-cite (match-string 1))

        ;;now get the citation link

        (setq n-cite-begin (match-beginning 0))
        (setq n-cite-end (match-end 0))
        
        (save-excursion

          (when (re-search-forward (concat "^" n-cite "\\. \\(.*\\)$") nil t)

            (setq cite-url (match-string 1))

            ;;delete this citation URL

            (delete-region (match-beginning 0) (match-end 0))))

        ;;now delete the cite number and change the 
        (when cite-url
        
          (save-excursion

            (delete-region n-cite-begin n-cite-end)

            ;;search backward from n-cite-begin to the text that is probably the
            ;;link we need to create

            (goto-char n-cite-begin)

            (when (and (forward-word -1)
                       (re-search-forward "[a-zA-Z0-9_-]+" nil t))

              (setq cite-text (match-string 0))

              (replace-match (concat "<a href=\"" cite-url  "\">" cite-text "</a>")))))))))

(defun records-rss-xhtmlify--quote()
  "Turn all quotes into italicized text."
  (interactive)
  
  (records-rss--match-replace-block "\"" "\"" "<i>" "</i>"))

(defun records-rss-xhtmlify--bold()
  "Turn all bold text into bold XHTML regions."
  (interactive)
  
  (records-rss--match-replace-block "\*" "\*" "<b>" "</b>"))

(defun records-rss--match-replace-block(first-match second-match first-replacement second-replacement)
  "Within a paragraph, search for the `first-match', then the `second-match'.
  Then replace the whole region around `first-replacement' and
  `second-replacement'."

  (save-excursion
    (beginning-of-buffer)

    (let(begin end quote quote-begin quote-end)
    
      (while (records-rss--has-next-para)

        (setq begin (point))

        (records-rss--next-para)
        
        (setq end (point))

        (save-excursion
          (save-restriction
            (narrow-to-region begin end)
            (beginning-of-buffer)

            (when (search-forward first-match nil t)
              
              (setq quote-begin (match-beginning 0))
              
              (when (search-forward second-match nil t)
                
                (setq quote-end (match-end 0))
              
                ;;now replace the quote.
              
                (setq quote (buffer-substring quote-begin quote-end))
              
                (delete-region quote-begin quote-end)

                (goto-char quote-begin)

                (insert (concat first-replacement quote second-replacement))))))))))

(defun records-rss-xhtmlify--para()
  "Turn all text paragraphs into XHTML paragraphs"

  (save-excursion
    (beginning-of-buffer)
    (insert "<p>\n")

    (while (records-rss--next-para)

      (if (records-rss--has-next-para)
          (progn
            (insert "</p>\n")
            (records-util-delete-whitespace-forward)
            (insert "\n<p>\n"))
        (insert "</p>\n")))))

(defun records-rss-xhtmlify--entities()
  "Turn all text entities < > etc, into XHTML &lt; &gt; chars. "

  (save-excursion
    (beginning-of-buffer)

    (save-excursion
      (while (search-forward "<" nil t)
        (replace-match "&lt;")))

    (save-excursion
      (while (search-forward ">" nil t)
        (replace-match "&gt;")))))

(defun records-rss-xhtmlify--items()
  "Turn all text entities < > etc, into XHTML &lt; &gt; chars. "

  (save-excursion
    (beginning-of-buffer)

    (save-excursion
      (while (search-forward "^[ ]-" nil t)
        (replace-match "")
        (goto-char (match-beginning 0))

        (insert "<li>")
        (records-rss--next-para)
        (insert "</li>")))))

(defun records-rss-xhtmlify--images()
  "Turn all text entities < > etc, into XHTML &lt; &gt; chars. "
  (interactive)
  
  (save-excursion
    (beginning-of-buffer)

    (save-excursion

      (while (re-search-forward "http://.*\\(jpg\\|gif\\|png\\)" nil t)
        (let(url)

          (message "FIXME (debug): found!")
          
          (setq url (match-string 0))

          (replace-match (concat "<img src=\"" url "\"/>")))))))

(defun records-rss--next-para()
  "Go to the next paragraph.  Return nil if there are no more paragraphs."

  (let((current-point (point)))

    (forward-paragraph)

    (not (equal current-point (point)))))

(defun records-rss--has-next-para()
  "Return true if we have more paragraphs."

  (let(result)
    (save-excursion

      (setq result (records-rss--next-para)))
    result))

(add-hook 'after-save-hook (lambda()
                            (records-rss-export-current-buffer t)))

(provide 'records-rss)

