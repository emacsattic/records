;;; records.el ---

;; $Id: records-mode.el,v 1.8 2001/07/09 04:41:32 burtonator Exp $

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
;; Mode specific code for records.

;;; History:
;; 

;;; Code:

(require 'easymenu)
(require 'imenu)


(defvar records-mode-menu-map nil
  "Records Menu Map.  Internal variable.")

(defvar records-mode-use-font-lock t 
  "* Enable fontification in records mode.
If font-lock-auto-fontify is t, records may be fontified even if this
value is nil. In that case, to turn of records fontification, add
\"records-mode\" as an element to the list called 
font-lock-mode-disable-list.")

(defvar records-mode-font-lock-keywords
  '(
    ;; subject face: this regexp obtained from records-subject-regexp
    ("^\\* \\(.*\\)\n\\-\\-\\-+$" . bold)
    ("^\\(END_\\)?ATTACHMENT:?" . font-lock-warning-face) ; attachment entries
    ("^\\(END_\\)?SUPPLEMENTAL:?" . font-lock-warning-face) ; supplemental entries
    ("^\\(END_\\)?TODO:?" . font-lock-warning-face) ; todo entries
    ("\\(^[a-z]+:\\)\\ \\(.*\\)$" 
     (1 font-lock-variable-name-face)
     (2 font-lock-reference-face)) ; metainfo
    ("<\\.\\./\\.\\..*>" 0 font-lock-reference-face) ;; inline links
    ("\\*[a-zA-Z]+\\*" 0 font-lock-keyword-face) ;; *highlighted* *strings*
    ("\\(//.*$\\)" 1 font-lock-comment-face)) ;; // comments
  "* Font-lock keywords for records mode.")

(define-derived-mode records-mode fundamental-mode "Records"
  "Enable records-mode for a buffer. Currently, the documentation of this
mode exists in three places: the INSTALL and README files and the menubar!
The install automates most of the things you need to do to use records. Please
use it!  After that, look at the file records-vars.el for the customization
variables.

The key-bindings of this mode are:
\\{records-mode-map}"

  ;; key-bindings

  (setq comment-start "//")

  ;;(records-menu-initialize)
  
  ;; This code should be run everytime a new records buffer is initialized
  (if running-xemacs
      (progn
        (set-buffer-menubar current-menubar)
        (add-submenu nil (cons "Records" records-mode-menu-map))))

  ;; imenu stuff
  (if (locate-library "imenu")
      (progn
        (make-variable-buffer-local 'imenu-prev-index-position-function)
        (make-variable-buffer-local 'imenu-extract-index-name-function)
        (setq imenu-prev-index-position-function 'records-goto-up-record)
        (setq imenu-extract-index-name-function 'records-subject-tag)))

  (auto-fill-mode 1)
  
  (records-parse-buffer)

  (make-local-hook 'kill-hooks)

  (add-hook 'kill-hooks 'records-remove-text-properties nil t)

  (if (not records-initialize)
      (progn
        (records-initialize)
        (setq records-initialize t)))


  ;; fontification code by Robert Mihram
  (if (and (or (not (boundp 'font-lock-auto-fontify))
               (not font-lock-auto-fontify))
           records-mode-use-font-lock)
      (progn (eval-when-compile (require 'font-lock))
             (make-local-variable 'font-lock-defaults)
             (setq font-lock-defaults '(records-mode-font-lock-keywords))
             (font-lock-mode 1)))
  (run-hooks 'records-mode-hooks)

  ;;save the buffer because read-only settings and fontification, etc may flag
  ;;the buffer as changed.
  (save-buffer))


(define-key records-mode-map "\C-c\C-i" 'records-insert-record)
(define-key records-mode-map "\C-c\C-d" 'records-delete-record)
(define-key records-mode-map "\C-c\C-e" 'records-rename-record)
(define-key records-mode-map "\C-c\C-m" 'records-move-record)

(define-key records-mode-map "\M-\C-a" 'records-goto-up-record)
(define-key records-mode-map "\M-\C-e" 'records-goto-down-record)

(define-key records-mode-map "\C-c\C-p" 'records-goto-prev-record)
(define-key records-mode-map "\C-c\C-n" 'records-goto-next-record)

(define-key records-mode-map "\C-c\C-b" 'records-goto-prev-record-file)
                                        ; back file
(define-key records-mode-map "\C-c\C-f" 'records-goto-next-record-file)
                                        ; front file

(define-key records-mode-map "\C-c\C-y" 'records-goto-prev-day) ; yesterday
(define-key records-mode-map "\C-c\C-t" 'records-goto-next-day) ; tomorrow

(define-key records-mode-map "\C-c\C-g" 'records-goto-link)
(define-key records-mode-map "\C-c\C-l" 'records-goto-last-record)
(define-key records-mode-map "\C-c\C-j" 'records-goto-index) ; jump!!

(define-key records-mode-map "\C-c\C-s" 'records-search-forward)
(define-key records-mode-map "\C-c\C-r" 'records-search-backward)

(define-key records-mode-map [(button2)] 'records-goto-mouse-link)

;; utility functions have C-c/ prefix keys
(define-key records-mode-map "\C-c/i" 'records-ispell-current-record)
(define-key records-mode-map "\C-c/t" 'records-create-todo)
(define-key records-mode-map "\C-c/s" 'records-create-supplemental)
(define-key records-mode-map "\C-c/a" 'records-create-attachment)
(define-key records-mode-map "\C-c/g" 'records-get-todo)
(define-key records-mode-map "\C-c/e" 'records-encrypt-record)
(define-key records-mode-map "\C-c/d" 'records-decrypt-record)
(define-key records-mode-map "\C-c/c" 'records-concatenate-records)
(define-key records-mode-map "\C-c/f" 'records-concatenate-record-files)
(define-key records-mode-map "\C-c/b" 'records-util-browse-url)

;;metadata functions have C-c ' prefix keys
(define-key records-mode-map "\C-c'm" 'records-metainfo-set)
(define-key records-mode-map "\C-c'd" 'records-metainfo-delete)
(define-key records-mode-map "\C-c's" 'records-status-set)

;;misc
(define-key records-mode-map "\C-c\C-c" 'records-show-calendar)
(define-key records-mode-map "\C-c\C-k" 'records-link-as-kill)
(define-key records-mode-map [?\C-c ?\C--] 'records-underline-line)
(define-key records-mode-map "\M-\C-h" 'records-mark-record)
(define-key records-mode-map "\C-c\C-z" 'records-initialize);; zap it in

;;hiding and showing
(define-key records-mode-map "\C-c\C-h\C-a" 'records-hs-show-all)
(define-key records-mode-map  "\C-c\C-h\C-t" 'records-hs-hide-current)
(define-key records-mode-map "\C-c\C-h\C-c" 'records-hs-hide-completed)
(define-key records-mode-map "\C-c\C-h\C-s" 'records-hs-hide-subject)

;;query support
(define-key records-mode-map "\C-c\C-q\C-t" 'records-query-on-status-todo-working)
(define-key records-mode-map "\C-c\C-q\C-c" 'records-query-on-status-completed)
(define-key records-mode-map "\C-c\C-q\C-s" 'records-query-on-subject)
(define-key records-mode-map "\C-c\C-q\C-h" 'records-query-on-header)
(define-key records-mode-map "\C-c\C-q\C-b" 'records-query-on-body)
;;(define-key records-mode-map "\C-c\C-q\C-r" 'records-query-on-body)

;;rss support
(define-key records-mode-map "\C-c/rc" 'records-rss-create-record)
(define-key records-mode-map "\C-c/re" 'records-rss-export-current-buffer)

(setq records-mode-menu-map
      '(["Today's Record" records-goto-today t]
        "--"
        ("Hide/Show"
         ["Show all records" records-hs-show-all t]
         "--"
         ["Hide current record" records-hs-hide-current t]
         ["Hide completed" records-hs-hide-completed t]
         ["Hide subject" records-hs-hide-subject t])
        ("Query"
         ["On 'todo' or 'working' status" records-query-on-status-todo-working t]
         ["On 'completed' status" records-query-on-status-completed t]
         ["On subject" records-query-on-subject t]
         ["On header" records-query-on-header t]
         ["On body" records-query-on-body t]
         "--"
         ["Set number of days to run queries" records-query-set-length t]
         "--"
         ["Show results buffer" (pop-to-buffer records-query-results-buffer-name) t])
        ("RSS"
         ["Create RSS Record" records-rss-create-record t]
         ["Export" records-rss-export-current-buffer t])
        ("Tools"
         ["Create TODO" records-create-todo t]
         ["Create Supplemental" records-create-supplemental t]
         ["Create Attachment" records-create-attachment t]
         ["Get TODO's" records-get-todo t]
         ["Decrypt Record" records-decrypt-record t]
         ["Encrypt Record" records-encrypt-record t]
         ["Concat Records" records-concatenate-records t]
         ["Concat Record Files" records-concatenate-record-files t]
         ["Browse URL" records-util-browse-url t]
         ["iSpell check" records-ispell-current-record t])
        "--"
        ("MetaInfo"
         ["Set MetaInfo" records-metainfo-set t]
         ["Delete MetaInfo" records-metainfo-delete t]
         ["Set Status" records-status-set t])
        ("Subject MetaInfo"
         ["Set Image" records-sm-set-image t])
        "--"
        ["Insert Record" records-insert-record t]
        ["Delete Record" records-delete-record t]
        ["Rename Record" records-rename-record t]
        ["Move Record" records-move-record t]
        "--"
        ["Up Record" records-goto-up-record t]
        ["Down Record" records-goto-down-record t]
        "--"
        ["Prev Record" records-goto-prev-record t]
        ["Next Record" records-goto-next-record t]
        "--"
        ["Prev Record File" records-goto-prev-record-file t]
        ["Next Record File" records-goto-next-record-file t]
        "--"
        ["Prev Day" records-goto-prev-day t]
        ["Next Day" records-goto-next-day t]
        "--"
        ["Goto Records Link" records-goto-link t]
        ["Goto Last Record" records-goto-last-record t]
        ["Goto Index" records-goto-index t]
        "--"
        ["Search Forward" records-search-forward t]
        ["Search Backward" records-search-backward t]
        "--"
        ["Show Calendar" records-show-calendar t]
        ["Mark Record"  records-mark-record t]
        ["Copy Records Link" records-link-as-kill t]
        ["Underline Line" records-underline-line t]
        "--"
        ["Close all unnecessary records buffers" records-close-all-unnecessary-buffers t]
        ["Re-Init Records" records-initialize t]))


(if (not running-xemacs)
    (easy-menu-define records-mode-menu-map records-mode-map "Records"
                      (cons "Records" records-mode-menu-map)))

;; for xemacs
(put 'records-mode 'font-lock-defaults '(records-mode-font-lock-keywords))

(provide 'records-mode)

;;; records-mode.el ends here
