;; $Id: publish.el,v 1.3 2001/09/25 06:37:39 burtonator Exp $

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
;; Hm.  I need a way to *easily* publish text to my website.  Maybe I should call
;; this publish.el.  
;;
;;     - given a string, output html and return the URL it was published to.
;;
;;     - maybe support templates so that HTML export can have a single way to
;;       determine the header and footer.  Maybe this should just be two variables.
;;
;;     - run rsync/scp to upload it to my site.  This should be done asynch so that
;;       it doesn't tie up emacs... 
;;
;;     - need to be able to select a region and publish it.
;;
;;     - every publication needs to have a title.
;;
;;     - use <pre> on the publications so that my formatting is correct.
;;

;;     - need the following vars:
;;        
;;         - publish-local-directory: directory where local files should be kept
;;      
;;         - publish-remote-server: server which contains information you want to
;;           publish
;;
;;         - publish-remote-directory: server directory
;;
;;         - publish-remote-user:


;;; Install:
;;
;; - customize the group 'publish
;;
;; - create all directories on the webserver.  You may have to play around with
;;  directory permissions so that all new files can be read by your webserver.

(defcustom publish-local-directory "~/.publish"
  "Directory which publications are kept and synched against."
  :type 'string
  :group 'publish)

(defcustom publish-remote-server "unknown"
  "Server to sync your publications to."
  :type 'string
  :group 'publish)

(defcustom publish-remote-directory "unknown"
  "Directory on server to sync your publications to."
  :type 'string
  :group 'publish)

(defcustom publish-remote-virtual-directory "unknown"
  "Directory on server which your HTTP server exports.  This should be directory
your HTTP server uses based on `publish-remote-directory' Example: '/publish'."
  :type 'string
  :group 'publish)

(defcustom publish-remote-user "unknown"
  "User on server which rsync/ssh can authenticate with.."
  :type 'string
  :group 'publish)

(defcustom publish-template-html-head "<html><body>"
  "String used as a header for your HTML publication"
  :type 'string
  :group 'publish)

(defcustom publish-template-html-tail "</body><body>"
  "String used as a tail for your HTML publication"
  :type 'string
  :group 'publish)

(defcustom publish-template-html-extension "html"
  "String to use for HTML file extensions.  If you want to enable something like
shtml then you should make the change here."
  :type 'string
  :group 'publish)

(defun publish-require-config()
  "Require that this has been setup correctly.  Throw an error if it hasn't."

  (if (or (string-equal publish-remote-server "unknown")
          (string-equal publish-remote-directory "unknown")
          (string-equal publish-remote-virtual-directory "unknown")
          (string-equal publish-remote-user "unknown"))
      (error "Publish is not setup correctly, make sure to customize this")))

(defun publish(title description)
  "Publish the given string given its title and description."
  
  (publish-require-config)

  (if (not (file-exists-p publish-local-directory))
      (make-directory publish-local-directory))

  (save-excursion
    (let(basename filename buffer)

      ;;get the basename which should be the filename sans directory info

      (setq basename (concat (publish-timestamp) "." publish-template-html-extension))
      
      (setq filename (concat  publish-local-directory "/" basename))

      (setq buffer (find-file-noselect filename))

      (set-buffer buffer)

      (erase-buffer) ;;just in case
      
      (insert publish-template-html-head)
      (insert "\n")
      
      (insert "<p><b>")
      (insert "\n")

      (insert title)

      (insert "</b></p>")
      (insert "\n")      

      (insert "\n")
      (insert "<p><pre>")
      (insert "\n")

      (insert description)

      (insert "\n")
      (insert "</pre></p>")
      (insert "\n")
      
      (insert publish-template-html-tail)
      
      (save-buffer)

      (kill-buffer buffer)

      ;;now return the url...

      (concat "http://" publish-remote-server publish-remote-virtual-directory "/" basename))))

(defun publish-timestamp()
  "Get a timestamp we can use to create files with."

  (format-time-string "%m-%d-%Y-%H-%M"))

(defun publish-region(start end title)
  "Publish the given region"
  (interactive
   (list (region-beginning)
         (region-end)
         (read-string "Title: ")))
   
  (let(description)

    (setq description (buffer-substring-no-properties start end))

    (publish title description)))

(defun publish-sync-ask()
  "Ask to sync to the publication server."
  (interactive)

  (if (yes-or-no-p "Synchronize publications now? ")
      (publish-sync)))

(defun publish-sync()
  "Synchronize the local publication directory with the server.  "
  (interactive)
  
  ;; Need exec a command like:
  
  ;;   rsync --recursive \
  ;;       --links \
  ;;       --rsh=ssh \
  ;;       --verbose \
  ;;       --compress \
  ;;       --times \
  ;;       --delete \
  ;;       /usr/local/rsync/ root@relativity.yi.org:/usr/local/rsync/

  (let(buffer-name)

    (setq buffer-name "*publish-sync*")

    (set-buffer (get-buffer-create buffer-name))

    (erase-buffer) ;; just in case
    
    (start-process "publish" buffer-name "rsync" 
                   "--recursive"
                   "--rsh=ssh"
                   "--verbose"
                   "--compress"
                   "--times"
                   "--perms"
                   "--update"
                   (concat (expand-file-name publish-local-directory) "/")
                   (concat publish-remote-user "@" publish-remote-server ":" publish-remote-directory))

    (pop-to-buffer buffer-name)))

(provide 'publish)
