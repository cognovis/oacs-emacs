;;; oacs-abbrevs.el --- OACS abbreviations

;; Copyright (C) 2004  the Code Mill

;; Author: Bart Teeuwisse <bart.teeuwisse@thecodemill.biz>
;; Keywords: extensions, tools, convenience

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

;; OACS abbreviations for procedure definitions, debug messages, etc.

;;; Code:

(defun oacs-initial ()
  "Return the developer's initials followed by a space"
  (concat 
   (mapconcat (function (lambda (str) 
			  (substring str 0 1))) 
	      (split-string user-full-name) nil) " "))

(defun oacs-remove-dbg ()
  "Remove OACS debug messages from the current buffer"
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (query-replace-regexp (concat "^\\s-*ns_log.*\"\\(
" (oacs-initial) "-\\{80\\}\\(
" (oacs-initial) ".*\\)*\\)\\{2\\}\"
") "" nil nil nil)))

(define-skeleton oacs-proc
  "Declare an improved Tcl procedure."
  nil
  "ad_proc " (completing-read
	      "Type of procedure: "
	      '(("-public" 0) ("-private" 1))
	      nil t nil 'oacs-proc-type-history 'oacs-proc-type) " "
  (read-string "Procedure name: " nil 'oacs-proc-name-history) | _
  " {" \n >
  (while (not (equal (setq parameter (read-string "Parameter definition: " nil 'oacs-parameter-history)) ""))
    (skeleton-insert '(nil parameter \n >))
    (push parameter oacs-proc-parameters)
    )| _ 
  "} {" > \n  
  > _ \n \n 
  > '(oacs-author) \n 
  > '(oacs-creation-date) \n \n 
  (let* ((oacs-proc-parameters (nreverse oacs-proc-parameters)))
    (while (setq parameter (pop oacs-proc-parameters))
      (skeleton-insert '(nil 
			 > "@param " 
			 (if (string-match "\\([a-zA-Z_0-9]+\\)" parameter)
			     (match-string 1 parameter))
			 "\n\n" )
		       )
      )
    )
  (setq oacs-proc-parameters nil)
  > "@return " \n \n 
  > "@error " ?\n
  "} {" \n ?\n
  "}" \n \n 
  )

(define-skeleton oacs-author
  "Insert the OpenACS @author tag"
  nil
  "@author " user-full-name " (" user-mail-address ")"
  )

(define-skeleton oacs-cvs-id
  "Insert the OpenACS @cvs-id tag"
  nil
  "@cvs-id $Id$"
  )

(define-skeleton oacs-creation-date
  "Insert the OpenACS @creation-date tag"
  nil
  "@creation-date " (format-time-string "%Y-%m-%d" (current-time)))

(define-skeleton oacs-author-stamp
  "Insert an AUTHOR DATE stamp. Where AUTHOR is the capitalized user
name and DATE is the current date of the form %Y-%m-%d"
  nil
  (upcase (user-login-name)) " " (format-time-string "%Y-%m-%d" (current-time)))

(define-skeleton oacs-dbg-log
  "Insert the AOLServer log statement and logging level."
  nil
  "ns_log " (completing-read
	     "Type of log message: "
	     '(("bug" 0) ("debug" 1) ("error" 2) ("fatal" 3) ("notice" 4) ("warning" 5))
	     nil t nil 'oacs-dbg-type-history 'oacs-dbg-severity) " "
  "\"" ?\n)

(define-skeleton oacs-dbg-head
  "Insert the common head of OpenACS debug messages."
  nil
  (oacs-initial) "--------------------------------------------------------------------------------" ?\n
  (oacs-initial) (upcase (user-login-name)) " debugging "
  (save-excursion
    (let ((current-point (point))
	  (regexp "^\\s-*\\(ad_\\|doc_\\)?proc\\(\\s-+-\\w+\\)*\\s-+\\(\\(:*\\w_?\\)+\\)")
	  (bound (re-search-backward "^}\\s-*$" nil t)))

      ; Restore point to the location before searching backwards fo
      ; the last line of the previous proc.

      (goto-char current-point)

      ; Search backwards for the declaration of a Tcl procedure but
      ; don't go beyond a line starting with a closing bracket as the
      ; assumption is that the last closing bracket of procedures
      ; always is the first character of the line.

      (if (re-search-backward regexp bound t)
	  (concat "procedure " (match-string 3))
	(buffer-file-name))))
  | _ ?\n
  (oacs-initial) "--------------------------------------------------------------------------------")

(define-skeleton oacs-dbg-tail
  "Insert the common tail of OpenACS debug messages."
  nil
  (oacs-initial) "--------------------------------------------------------------------------------")

(define-skeleton oacs-dbg
  "Insert an OpenACS debuging message to write selected variable to
the AOLServer log."
  nil
  '(oacs-dbg-log)
  '(oacs-dbg-head)
  ("Paramater name: " ?\n (oacs-initial) str " = '${" str "}'")
  _ ?\n
  '(oacs-dbg-tail) "\""
  )

(define-skeleton oacs-dbg-headers
  "Insert an OpenACS debug message which writes all headers and form
variables to the AOLServer log."
  nil
  "set oacs:msg \"" ?\n
  > '(oacs-dbg-head) ?\n
  > (oacs-initial) "Headers:\"" ?\n
  > "set oacs:headers [ns_conn headers]" \n
  > "for {set oacs:i 0} {${oacs:i} < [ns_set size ${oacs:headers}]} {incr oacs:i} {" \n
  > "set oacs:key [ns_set key ${oacs:headers} ${oacs:i}]" \n
  > "set oacs:value [ns_set value ${oacs:headers} ${oacs:i}]" \n
  > "append oacs:msg \"" \n
  > (oacs-initial) "    ${oacs:key} = '${oacs:value}'\"" ?\n
  > -4 "}" ?\n
  > "append oacs:msg \"" \n (oacs-initial) \n
  > (oacs-initial) "Form variables:\"" ?\n
  > "set oacs:form [ns_getform]" \n
  > "for {set oacs:i 0} {${oacs:i} < [ns_set size ${oacs:form}]} {incr oacs:i} {" \n
  > "set oacs:key [ns_set key ${oacs:form} ${oacs:i}]" \n
  > "set oacs:value [ns_set value ${oacs:form} ${oacs:i}]" \n
  > "append oacs:msg \"" \n
  > (oacs-initial) "    ${oacs:key} = '${oacs:value}'\"" ?\n
  > -4 "}" \n
  > "append oacs:msg \"" \n
  > '(oacs-dbg-tail) "\"" \n
  > '(oacs-dbg-log) "${oacs:msg}\"" ?\n
  )

(define-skeleton oacs-dbg-form
  "Insert an OpenACS debug message which records form errors to the
AOLServer log. Requires a variable named 'form_id' with the name of
the OpenACS form. Recommended practice:

set form_id my_form
template::form create $form_id
template::element create $form_id my_element \
    ...."
  nil
  "if {[info exists ${form_id}:error]} {" ?\n
  > '(oacs-dbg-log)
  > '(oacs-dbg-head) ?\n
  > (oacs-initial) "${form_id}:error = '[array get ${form_id}:error]'" ?\n
  > '(oacs-dbg-tail) "\"" ?\n
  > -4 "}"
  )

(define-skeleton oacs-template-form
  "Insert a template::form declaration."
  nil
  "set form_id " (read-string "Form name: ") ?\n
  > "template::form create $form_id"
  )

;;; Associate OpenACS Tcl abbreviations with tcl-mode
(define-abbrev-table 'tcl-mode-abbrev-table '(
    ("oauthor" "" oacs-author 0)
    ("ocreation" "" oacs-creation-date 0)
    ("ocvs" "" oacs-cvs-id 0)
    ("odbg" "" oacs-dbg 0)
    ("odbgf" "" oacs-dbg-form 0)
    ("odbgh" "" oacs-dbg-headers 0)
    ("oproc" "" oacs-proc 0)
    ("ostamp" "" oacs-author-stamp 0)
    ("oform" "" oacs-template-form 0)
    ))

;;; Associate OpenACS SQL abbreviations with sql-mode
(define-abbrev-table 'sql-mode-abbrev-table '(
    ("oauthor" "" oacs-author 0)
    ("ocreation" "" oacs-creation-date 0)
    ("ocvs" "" oacs-cvs-id 0)
    ("ostamp" "" oacs-author-stamp 0)
    ))

(defun define-insert-map (map sub-mode)
  "Define the appropriate sub menu MAP to the OACS menu bar for the abbreviations
available to the sub mode SUB-MODE."
  (case sub-mode
    (tcl
     (define-key map [insert-ostamp]
       '(menu-item "ostamp" oacs-author-stamp
		   :help "Insert an AUTHOR DATE stamp at point"))
     (define-key map [insert-oproc]
       '(menu-item "oproc" oacs-proc
		   :help "Insert ad_proc at point"))
     (define-key map [insert-odbgh]
       '(menu-item "odbgh" oacs-dbg-headers
		   :help "Insert personal HTTP headers & form variables debug message at point"))
     (define-key map [insert-odbgf]
       '(menu-item "odbgf" oacs-dbg-form
		   :help "Insert personal form template debug message at point"))
     (define-key map [insert-odbg]
       '(menu-item "odbg" oacs-dbg
		   :help "Insert personal debug message at point"))
     (define-key map [insert-ocvs]
       '(menu-item "ocvs" oacs-cvs-id
		   :help "Insert @cvs-id at point"))
     (define-key map [insert-creation]
       '(menu-item "ocreation" oacs-creation-date
		   :help "Insert @creation-date at point"))
     (define-key map [insert-oauthor]
       '(menu-item "oauthor" oacs-author
		   :help "Insert @author at point"))
     )
    (sql
     (define-key map [insert-ostamp]
       '(menu-item "ostamp" oacs-author-stamp
		   :help "Insert an AUTHOR DATE stamp at point"))
     (define-key map [insert-ocvs]
       '(menu-item "ocvs" oacs-cvs-id
		   :help "Insert @cvs-id at point"))
     (define-key map [insert-creation]
       '(menu-item "ocreation" oacs-creation-date
		   :help "Insert @creation-date at point"))
     (define-key map [insert-oauthor]
       '(menu-item "oauthor" oacs-author
		   :help "Insert @author at point"))
     )
    (xql
     (define-key map [insert-ostamp]
       '(menu-item "ostamp" oacs-author-stamp
		   :help "Insert an AUTHOR DATE stamp at point"))
     (define-key map [insert-ocvs]
       '(menu-item "ocvs" oacs-cvs-id
		   :help "Insert @cvs-id at point"))
     (define-key map [insert-creation]
       '(menu-item "ocreation" oacs-creation-date
		   :help "Insert @creation-date at point"))
     (define-key map [insert-oauthor]
       '(menu-item "oauthor" oacs-author
		   :help "Insert @author at point"))
     )))

;;; Always activate the minor abbrev-mode in major tcl-mode
(add-hook 'tcl-mode-hook 'abbrev-mode)

(provide 'oacs-abbrevs)
;;; oacs-abbrevs.el ends here
