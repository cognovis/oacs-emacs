;;; oacs.el --- OpenACS extensions

;; Copyright (C) 2003  the Code Mill

;; Author: Bart Teeuwisse <bart.teeuwisse@thecodemill.biz>
;; Keywords: abbrev, extensions, convenience, tools

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

;; The latest version of oacs is always available from:
;;
;; - http://www.thecodemill.biz/services/oacs/
;;
;; Oacs depends on the psgml/nxml and mmm modules to be
;; installed. Psgml and mmm are available from sourceforge.net:
;; 
;;  - http://psgml.sourceforge.net/
;;  - http://mmm-mode.sourceforge.net/
;;
;; Nxml is an alternative to psgml and available from:
;;
;;
;; Psgml/nxml can be installed along side one another. Oacs defaults
;; to psgml if both are installed unless oacs-preferred-xml-mode is set
;; to nxml.
;; 
;; Color-occur is not required but highly recommended. Especially in
;; combination with the various M-oo* keystrokes. (See oacs-occur.el)
;;
;; - http://www.emacswiki.org/elisp/color-occur.el
;; 
;; Besides oacs specific variables, oacs uses the common Emacs
;; variables:
;;
;;  - user-full-name
;;  - user-mail-address
;;
;; Set them in your .emacs file and your full name and e-mail address
;; will be included in all OpenACS headers.
;;
;; Oacs adds several DTDs to psgml, which in order to use need to be
;; installed. The set consists of common HTML DTDs as well as a DTD
;; for the OpenACS specific XQL format. You can find a complete set as
;; added by oacs at:
;; 
;; - http://www.thecodemill.biz/repository/oacs-dtds.tgz
;;
;; Installation instructions are included in the archive. If you
;; prefer nxml over psgml install XXX instead. Download XXX from:
;;
;; - http://www.thecodemill.biz/repository/oacs-XXX.tgz
;; 
;; Oacs variables can be customized using:
;;
;;  - M-x customize-group oacs
;;  
;; Oacs is intended to have as little side effects as
;; possible. However, it does set various psgml and mmm
;; variables. Should they clash with your preferences simply remove
;; them from oacs.

;;; Code:

;; Load preferred XML mode. Should psgml fail to load -perhaps because
;; psgml wasn't installed- then fall back on Nxml mode. 

(require 'oacs-vars)
(if (equal oacs-preferred-xml-mode 'psgml)
    (condition-case nil
        (require 'oacs-psgml)
      (error (require 'oacs-nxml)))
  (require 'oacs-nxml))
(require 'oacs-mmm)
(require 'oacs-format)
(require 'oacs-occur)
(require 'oacs-headers)
(require 'oacs-abbrevs)
(require 'oacs-comment)
(require 'oacs-error-log)

(defun oacs-mode (&optional arg)
  "Minor mode for editing OpenACS files.

Commands Available:
\\<oacs-mode-map>
\\{oacs-mode-map}

"
  (interactive "P")
  (if (if arg (> (prefix-numeric-value arg) 0) (not oacs-mode))
      (oacs-mode-on)
    (oacs-mode-off)))
(add-to-list 'minor-mode-alist '(oacs-mode " OACS"))

(defun oacs-mode-on ()
  "Turn on OACS Mode. See `oacs-mode'."
  (unless oacs-mode
    (let ((file-name (buffer-file-name)) sub-mode)
      
      ;; Is the file an OpenACS file?

      (if (stringp file-name)
	  (case major-mode
	    (nsd-error-mode 
	     (setq sub-mode 'log))
	    (tcl-mode
	     (if (or (string-match "\.vuh$" file-name)
		     (string-match "www/.*\.tcl$" file-name)
		     (string-match "resources/.*\.tcl$" file-name)
		     (string-match "lib/.*\.tcl$" file-name)
		     (string-match "tcl/.*\.tcl$" file-name))
		 (setq sub-mode 'tcl)))
	    ((list xml-mode nxml-mode)
	     (if (string-match "\.adp$" file-name)
		 (setq sub-mode 'adp)
	       (if (string-match "\.xql$" file-name)
		   (setq sub-mode 'xql))))
	    (sql-mode
	     (if (string-match "sql/[^/]*/?.*\.sql$" file-name)
		 (setq sub-mode 'sql)))))

      ;; Activate OACS sub mode if the file is an OpenACS file.

      (if sub-mode
	  (progn
	    (make-local-variable 'oacs-mode)
	    (make-local-variable 'oacs-mode-map)
	    (setq oacs-mode t)
	    (oacs-define-keymap sub-mode)

	    ;; Only Tcl library files can use the Index menu and the
	    ;; which function mode.

	    (if (and (stringp file-name)
		     (string-match "tcl/.*\.tcl$" file-name))
		(progn
		  
		  ;; Overwrite the tcl-imenu-create-index-function,
		  ;; use the oacs-imenu-generic-expression insted.

		  (setq imenu-create-index-function 'imenu-default-create-index-function)
		  (set (make-local-variable 'imenu-generic-expression)
		       oacs-imenu-tcl-expression)
		  (which-function-mode 1)))
	    (run-hooks 'oacs-mode-hook))))))

(defun oacs-mode-off ()
  "Turn off OACS Mode. See `oacs-mode'."
  (when oacs-mode
    (setq oacs-mode nil)))

(defun oacs-define-keymap (sub-mode)
  "Map OACS key sequences to functions."
  (let ((map (make-sparse-keymap)) 
	(menu-map (make-sparse-keymap "OACS"))
	(menu-list-map (make-sparse-keymap "List"))
	(menu-insert-map (make-sparse-keymap "Insert"))
	(oacs-mode-prefix-map (make-sparse-keymap)))
    (define-key oacs-mode-prefix-map "ml" 'oacs-monitor-error-log)
    (define-key menu-map [monitor] 
      '(menu-item "Monitor server log" oacs-monitor-error-log
		  :help "Monitor an AOLServer error log"))
    (define-key oacs-mode-prefix-map "oo" 'occur)
    (define-key menu-list-map [occur] 
      '(menu-item "Occurrences" occur 
		  :help "List occurrences of a regexp in this buffer"))
    (define-key oacs-mode-prefix-map "ta" 'tags-apropos)
    (define-key menu-list-map [tags-apropos] 
      '(menu-item "Tags Apropos" tags-apropos 
		  :help "Search the tagged definitions for a regexp"))

    ;; Sort Imenu alphabetic.

    (set (make-local-variable 'imenu-sort-function)
	 'imenu--sort-by-name)

    (case sub-mode
      (adp
       (define-key oacs-mode-prefix-map "fb" 'oacs-format-html)
       (define-key oacs-mode-prefix-map "fp" 'find-file-at-point)
       (define-key menu-map [format-buffer] 
	 '(menu-item "Format buffer" oacs-format-html
		     :help "Reformat this buffer to a consistant style"))
       (define-key menu-map [find-file] 
	 '(menu-item "Find file @ point" find-file-at-point
		     :help "Open file, guessing a default from text around point")))
      (log
       (define-key oacs-mode-prefix-map "rl" 'oacs-revert-log)
       (define-key oacs-mode-prefix-map "fp" 'find-file-at-point)
       (define-key oacs-mode-prefix-map "om" 'oacs-occur-dbg)
       (define-key oacs-mode-prefix-map "ob" 'oacs-occur-dbg-bug)
       (define-key oacs-mode-prefix-map "od" 'oacs-occur-dbg-debug)
       (define-key oacs-mode-prefix-map "oe" 'oacs-occur-dbg-error)
       (define-key oacs-mode-prefix-map "of" 'oacs-occur-dbg-fatal)
       (define-key oacs-mode-prefix-map "on" 'oacs-occur-dbg-notice)
       (define-key oacs-mode-prefix-map "ow" 'oacs-occur-dbg-warning)
       (define-key menu-map [revert-log] 
	 '(menu-item "Revert log" oacs-revert-log
		     :help "Revert the buffer to the log file"))
       (define-key menu-map [find-file] 
	 '(menu-item "Find file @ point" find-file-at-point
		     :help "Open file, guessing a default from text around point"))
       (define-key menu-list-map [occur-dbg] 
	 '(menu-item "All messages" oacs-occur-dbg
		     :help "List all YOUR messages in the log"))
       (define-key menu-list-map [occur-dbg-bug] 
	 '(menu-item "Bug messages" oacs-occur-dbg-bug
		     :help "List all YOUR bug level messages in the log"))
       (define-key menu-list-map [occur-dbg-debug] 
	 '(menu-item "Debug messages" oacs-occur-dbg-debug
		     :help "List all YOUR debug level messages in the log"))
       (define-key menu-list-map [occur-dbg-error] 
	 '(menu-item "Error messages" oacs-occur-dbg-error
		     :help "List all YOUR error level messages in the log"))
       (define-key menu-list-map [occur-dbg-fatal] 
	 '(menu-item "Fatal messages" oacs-occur-dbg-fatal
		     :help "List all YOUR fatal level messages in the log"))
       (define-key menu-list-map [occur-dbg-notice] 
	 '(menu-item "Notice messages" oacs-occur-dbg-notice
		     :help "List all YOUR notice level messages in the log"))
       (define-key menu-list-map [occur-dbg-warning] 
	 '(menu-item "Warning messages" oacs-occur-dbg-warning
		     :help "List all YOUR warning level messages in the log")))
      (sql
       (define-key oacs-mode-prefix-map "od" 'oacs-occur-function)
       (define-key oacs-mode-prefix-map "fp" 'find-file-at-point)
       (define-key oacs-mode-prefix-map "fb" 'oacs-format-sql)
       (define-key menu-list-map [occur-function] 
	 '(menu-item "Function definitions" oacs-occur-function
		     :help "List all function definitions in this buffer"))
       (define-key menu-map [find-file] 
	 '(menu-item "Find file @ point" find-file-at-point
		     :help "Open file, guessing a default from text around point"))
       (define-key menu-map [format-buffer] 
	 '(menu-item "Format buffer" oacs-format-sql
		     :help "Reformat this buffer to a consistant style"))
       (set (make-local-variable 'imenu-generic-expression)
	    oacs-imenu-sql-expression))
      (tcl
       (define-key oacs-mode-prefix-map "od" 'oacs-occur-proc)
       (define-key oacs-mode-prefix-map "fp" 'find-file-at-point)
       (define-key oacs-mode-prefix-map "fb" 'oacs-format-tcl)
       (define-key oacs-mode-prefix-map "rd" 'oacs-remove-dbg)
       (define-key menu-list-map [occur-proc] 
	 '(menu-item "Procedure definitions" oacs-occur-proc
		     :help "List all ad_procs, doc_procs and procs in this buffer"))
       (define-key menu-map [format-buffer] 
	 '(menu-item "Format buffer" oacs-format-tcl
		     :help "Reformat this buffer to a consistant style"))
       (define-key menu-map [find-file] 
	 '(menu-item "Find file @ point" find-file-at-point
		     :help "Open file, guessing a default from text around point"))
       (define-key menu-map [remove-dbg]
	 '(menu-item "Remove OACS debug messages" oacs-remove-dbg
		     :help "Remove OACS debug messages from this buffer"))
       (define-insert-map menu-insert-map sub-mode))
      (xql
       (define-key oacs-mode-prefix-map "od" 'oacs-occur-query)
       (define-key menu-list-map [occur-query] 
	 '(menu-item "Query definitions" oacs-occur-query
		     :help "List all query definitions in this buffer"))
       (define-insert-map menu-insert-map sub-mode)))

    ;; Add the prefix key to the key sequences.

    (define-key map oacs-mode-prefix-key oacs-mode-prefix-map)

    ;; Redefine the OACS menu in the menu bar.

    (define-key map [menu-bar oacs] (cons "OACS" menu-map))

    ;; Only add sub menus if they contain menu items.

    (if (not (equal menu-list-map (make-sparse-keymap "List")))
	(progn
	  (define-key map [menu-bar oacs list] (cons "List" menu-list-map))
	  (define-key map [C-S-down-mouse-3] menu-list-map)))
    (if (not (equal menu-insert-map (make-sparse-keymap "Insert")))
	(progn
	  (define-key map [menu-bar oacs insert] (cons "Insert" menu-insert-map))
	  (define-key map [C-S-down-mouse-3] menu-insert-map)))

    ;; Link the OACS menu to Ctrl+Shift+Right mouse click.

    (define-key map [C-S-down-mouse-3] menu-map)

    ;; Copy the working maps over to the global maps.

    (setq oacs-mode-map map))

  ;; Activate the key sequence upon enabling OACS minor mode.

  (make-local-variable 'minor-mode-map-alist)
  (add-to-list 'minor-mode-map-alist (cons 'oacs-mode oacs-mode-map)))

;; Activate the minor oacs-mode in major tcl-mode. The oacs-mode
;; function checks to see if the tcl file is in an OpenACS directory
;; before enabling oacs-mode.

(add-hook 'tcl-mode-hook 'oacs-mode)

;; Activate the minor oacs-mode in major modes also used by
;; OpenACS. See above tcl-mode-hook comments.

(add-hook 'sql-mode-hook 'oacs-mode)
(add-hook 'xml-mode-hook 'oacs-mode)
(add-hook 'nxml-mode-hook 'oacs-mode)
(add-hook 'nsd-error-mode-hook 'oacs-mode)

;; Add Index Imenu to menu bar for major modes: Tcl, SQL. The main XML
;; modes (PSGML and nXML) don't support Imenu.

(add-hook 'tcl-mode-hook 'imenu-add-menubar-index)
(add-hook 'sql-mode-hook 'imenu-add-menubar-index)

(provide 'oacs)
;;; oacs.el ends here

