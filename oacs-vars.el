;;; oacs-vars.el --- Oacs customizable and global variables

;; Copyright (C) 2005  the Code Mill

;; Author: Bart Teeuwisse <bart.teeuwisse@thecodemill.biz>
;; Keywords: extensions

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

;; 

;;; Code:

(defgroup oacs nil
  "OpenACS extensions."
  :version "21.1"
  :link '(emacs-commentary-link "oacs")
  :link '(url-link "http://www.thecodemill.biz/")
  :group 'convenience)

(defcustom oacs-tdom-installed "t"
  "Is tDOM installed on the OpenACS site?"
  :type 'boolean
  :group 'oacs)

(defcustom oacs-base-dir "/var/lib/aolserver/"
  "Base directory of a OpenACS installation."
  :type 'string
  :group 'oacs)

(defcustom oacs-dbg-severity "notice"
  "Default severity for oacs-dbg."
  :type 'string
  :group 'oacs)

(defcustom oacs-oracle-version "8.1.6"
  "Default Oracle database version."
  :type 'string
  :group 'oacs)

(defcustom oacs-postgresql-version "8.4"
  "Default PostgreSQL database version."
  :type 'string
  :group 'oacs)

(defcustom oacs-proc-type "-public"
  "Default procedure type for oacs-proc."
  :type 'string
  :group 'oacs)

(defcustom oacs-mode-prefix-key [(meta ?o)]
  "Prefix key for the OACS Minor Mode Keymap."
  :type 'vector
  :group 'oacs)

(defcustom oacs-error-log-dirs (quote ("/var/log/aolserver"))
  "List of directories that might contain AOLServer error logs."
  :type '(repeat string)
  :group 'oacs)

(defcustom oacs-mmm-ify-log "t"
  "Use MMM mode in the AOLserver server logs? MMM tends to be very
slow on large log files."
  :type 'boolean
  :group 'oacs)

(defcustom oacs-preferred-xml-mode 'psgml
  "Preferred XML mode handler. Psgml or Nxml."
  :type '(choice (const :tag "Psgml-mode" :value psgml)
                 (const :tag "Nxml-mode" :value nxml))
  :group 'oacs)

(defcustom oacs-insert-cvs-id "t"
  "Insert @cvsid $Id$?"
  :type 'boolean
  :group 'oacs)

(defvar oacs-proc-type-history nil
  "History of ad_proc procedure types")

(defvar oacs-proc-name-history nil
  "History of ad_proc procedure names")

(defvar oacs-parameter-history nil
  "History of ad_proc parameters")

(defvar oacs-proc-parameters nil
  "List of parameters entered by the user when defining an ad_proc.")

(defvar oacs-dbg-type-history nil
  "History of debugging severities")

(defvar oacs-oracle-version-history nil
  "History of Oracle database versions")

(defvar oacs-postgres-version-history nil
  "History of PostgreSQL database versions")

(defvar oacs-error-log-history nil
  "History of AOLserver error log files")

(defvar oacs-error-log-prev-point-max nil
  "The location of the end of the error log at the end of the previous
update of the error log. Used to MMM-ify only the new addition to the
log.")

(defvar oacs-mode nil
   "Toggle OACS Minor Mode.")

(defvar oacs-mode-map nil
  "Keymap for OACS Minor Mode.")

(defvar oacs-imenu-tcl-expression 
  '(
    ("Procedures (-warn)" "^\\s-*\\(ad_\\|_doc_\\)?proc\\s-+\\(\\(-\\(public\\|private\\|depreciated\\)\\s-+\\)*-warn\\s-+\\(-\\(public\\|private\\|depreciated\\)\\s-+\\)*\\([^{ 	]*\\)\\)\\s-*" 7)
   ("Procedures (-public)" "^\\s-*\\(ad_\\|_doc_\\)?proc\\s-+\\(\\(-\\(warn\\|private\\|depreciated\\)\\s-+\\)*-public\\s-+\\(-\\(warn\\|private\\|depreciated\\)\\s-+\\)*\\([^{ 	]*\\)\\)\\s-*" 7)
   ("Procedures (-private)" "^\\s-*\\(ad_\\|_doc_\\)?proc\\s-+\\(\\(-\\(warn\\|public\\|depreciated\\)\\s-+\\)*-private\\s-+\\(-\\(warn\\|public\\|depreciated\\)\\s-+\\)*\\([^{ 	]*\\)\\)\\s-*" 7)
   ("Procedures (-depreciated)" "^\\s-*\\(ad_\\|_doc_\\)?proc\\s-+\\(\\(-\\(warn\\|public\\|private\\)\\s-+\\)*-depreciated\\s-+\\(-\\(warn\\|public\\|private\\)\\s-+\\)*\\([^{ 	]*\\)\\)\\s-*" 7)
   (nil "^\\s-*\\(ad_\\|_doc_\\)?proc\\s-+\\(\\(-[^ ]*\\s-+\\)*\\([^{ 	]*\\)\\)\\s-*" 4)
   )
  "Value for `imenu-generic-expression' for Tcl files in Oacs Minor Mode.")

(defvar oacs-imenu-sql-expression 
  '(
    ("Tables" "^\\s-*create\\s-+table\\s-+\\(\\(\\w\\|_\\)+\\)" 1)
    ("Sequences" "^\\s-*create\\s-+sequence\\s-+\\(\\(\\w\\|_\\)+\\)" 1)
    ("Packages" "^\\s-*create\\s-+\\(or\\s-+replace\\s-+\\)?package\\s-+\\(.*\\)$" 2)
    ("Functions" "^\\s-*create\\s-+\\(or\\s-+replace\\s-+\\)?function\\s-+\\(\\(\\w\\|_\\)+\\)" 2)
    )
  "Value for `imenu-generic-expression' for SQL files in Oacs Minor Mode.")

(provide 'oacs-vars)
;;; oacs-vars.el ends here
