;;; oacs-mmm.el --- OACS MMM extension

;; Copyright (C) 2004  the Code Mill

;; Author: Bart Teeuwisse <bart.teeuwisse@thecodemill.biz>
;; Keywords: convenience, extensions, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Part of the Emacs OACS module. See oacs.el for more information.

;;; Code:

;;; Multiple Majort Mode support for embedded sql in xql files.
(require 'mmm-auto)
(require 'mmm-mode)

;;; Most useful value for mmm-global-mode, is the symbol `maybe',which
;;  causes MMM Mode to turn itself on in precisely those buffers where
;;  it would be useful.
(setq mmm-global-mode 'maybe)

;;; Same background color in submode as major mode
(setq mmm-submode-decoration-level '0)

;;; Automatically select mmm and embedded-sql for XQL files.
(mmm-add-mode-ext-class nil ".xql" 'oacs-xql-embedded-sql)

;;; Define oacs-xql-embedded-sql mode
(mmm-add-classes
      '((oacs-xql-embedded-sql
         :submode sql-mode
         :face mmm-declaration-submode-face
         :front "<querytext>"
         :back "</querytext>"
	 :creation-hook oacs-xql-query-bind-and-send-hook)))

;;; Automatically select mmm and embedded-sql for AOLserver log files.
(if oacs-mmm-ify-log
    (mmm-add-mode-ext-class nil ".log" 'oacs-log-embedded-sql))

;;; Define log-embedded-sql mode
(mmm-add-classes
      '((oacs-log-embedded-sql
         :submode sql-mode
         :face mmm-declaration-submode-face
         :front "sql(.*?):\\s-*\\b"
         :back "\\b\\s-+\\["
	 :creation-hook oacs-log-query-bind-and-send-hook)))

;;; Define oacs-tcl-embedded-sql mode
(mmm-add-classes
      '((oacs-tcl-embedded-sql
         :submode sql-mode
         :face mmm-declaration-submode-face
         :front "db_\\(\\w\\|_\\)+\\(\\s-+\\(\\w\\|_\\)+\\)+\\s-+\\({\\|\"\\)"
         :back "\\(}\\|\"\\)")))
(mmm-add-mode-ext-class 'tcl-mode nil 'oacs-tcl-embedded-sql)

;;; Use 'oacs-query-bind-and-send when in oacs-log-embedded-sql mode
(defun oacs-log-query-bind-and-send-hook ()
  "Use oacs-query-bind-and-send for `comint-input-sender'."
  (add-to-list 'mmm-save-local-variables '(comint-input-sender region))
  (setq comint-input-sender 'oacs-query-bind-and-send)
  (add-to-list 'mmm-save-local-variables '(paragraph-start region))
  (setq paragraph-start "^\\[.*\\|^\\s-+'")
  (setq paragraph-separate "^\\[.*$")
)

;;; Use 'oacs-query-bind-and-send when in oacs-xql-embedded-sql mode
(defun oacs-xql-query-bind-and-send-hook ()
  "Use oacs-query-bind-and-send for `comint-input-sender'."
  (add-to-list 'mmm-save-local-variables '(comint-input-sender region (xml-mode)))
  (setq comint-input-sender 'oacs-query-bind-and-send)
)

;;; .vuh files are really Tcl files
(add-to-list 'auto-mode-alist '("\\.vuh" . tcl-mode))

(defun oacs-query-bind-and-send (proc string)
  "Send to PROC input STRING, maybe replacing bind variables.
Placeholders are words starting with a colon like :this and may
contain underscores like :so_and_so. This function is used for
`comint-input-sender' in `sql-postgres`, `sql-oracle` and `sql-mode`."
  (message "%s" string)
  (while (string-match "[ =]:\\(\\(\\sw\\|_\\)+\\)" string)
    (setq string (replace-match 
		  (concat 
		   "'" (read-from-minibuffer
			(format "Enter value for %s: " (match-string 1 string))
			nil nil nil sql-placeholder-history) "'")
		  t t string)))
  (comint-send-string proc string)
  (comint-send-string proc "\n"))

(defadvice sql-mode (after sql-mode)
  "Use oacs-query-bind-and-send for `comint-input-sender'."
  (make-variable-buffer-local 'comint-input-sender)
  (setq comint-input-sender 'oacs-query-bind-and-send)
)
(ad-activate 'sql-mode)

(defadvice sql-oracle (after sql-oracle)
  "Use oacs-query-bind-and-send for `comint-input-sender'."
  (make-variable-buffer-local 'comint-input-sender)
  (setq comint-input-sender 'oacs-query-bind-and-send)
)
(ad-activate 'sql-oracle)

(defadvice sql-postgres (after sql-postgres)
  "Use oacs-query-bind-and-send for `comint-input-sender'."
  (make-variable-buffer-local 'comint-input-sender)
  (setq comint-input-sender 'oacs-query-bind-and-send)
)
(ad-activate 'sql-postgres)

;;; Redefine sql-send-region so that is uses the comint-input-sender.
(require 'sql)
(defun sql-send-region (start end)
  "Send a region to the SQL process."
  (interactive "r")
  (if (buffer-live-p sql-buffer)
      (save-excursion
	(let ((string (buffer-substring start end)))
	  (if (string-match ";$" string)
	      ()
	    (setq string (concat string ";")))
	  (if (string-match "\n$" string)
	      ()
	    (setq string (concat string "\n")))
	  (funcall comint-input-sender sql-buffer string)
	  (message "Sent string to buffer %s." (buffer-name sql-buffer)))
	(if sql-pop-to-buffer-after-send-region
	    (pop-to-buffer sql-buffer)
	  (display-buffer sql-buffer)))
    (message "%s" "No SQL process started.")))

;;; Store the region send the SQLi process in the history of the SQLi
;;  buffer.
(defadvice sql-send-region (after sql-store-in-history)
  "The region sent to the SQLi process is also stored in the history."
  (let ((history (buffer-substring-no-properties start end)))
    (save-excursion
      (set-buffer sql-buffer)
      (if (and (funcall comint-input-filter history)
               (or (null comint-input-ignoredups)
                   (not (ring-p comint-input-ring))
                   (ring-empty-p comint-input-ring)
                   (not (string-equal (ring-ref comint-input-ring 0)
                                      history))))
          (ring-insert comint-input-ring history))
      (setq comint-save-input-ring-index comint-input-ring-index)
      (setq comint-input-ring-index nil))))
(ad-activate 'sql-send-region)

(provide 'oacs-mmm)
;;; oacs-mmm.el ends here
