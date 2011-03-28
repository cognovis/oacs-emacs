;;; oacs-headers.el --- OACS auto insert headers extension

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

;; OACS auto insert headers. Insert standard headers when creating new
;; OpenACS files.

;;; Code:

;;; Auto insert headers when creating new OpenACS files.
(require 'autoinsert)
(auto-insert-mode)
(setq auto-insert-query nil)
(define-auto-insert "\.adp$" 'oacs-adp-template)
(define-auto-insert "www/.*\.tcl$" 'oacs-tcl-template)
(define-auto-insert "resources/.*\.tcl$" 'oacs-tcl-template)
(define-auto-insert (concat oacs-base-dir ".*/lib/.*\.tcl$") 'oacs-inc-template)
(define-auto-insert "tcl/.*\.tcl$" 'oacs-lib-template)
(define-auto-insert "\.vuh$" 'oacs-vuh-template)
(define-auto-insert "\.xql$" 'oacs-xql-template)
(define-auto-insert "sql/[^/]*/?.*\.sql$" 'oacs-sql-template)

(defun oacs-file-name ()
  "Get the file name of the current buffer relative to the OpenACS
base dir."
  (let ((file-name (buffer-file-name)))
	     (if (string-match (concat oacs-base-dir "/[^/]*/") file-name)
		 (substring file-name (match-end 0))
	       )
	     ))

;;; Skeleton definitions.
(define-skeleton oacs-adp-template
  "Insert master tag with selected property tags into a new .adp page"
  nil
  "<master>" \n >
  (if (string-equal (setq v1 (read-string "Title: (empty is none) " "@title@")) "")
      ""
    (concat "<property name=\"title\">"v1"</property>")) & \n >
  (if (string-equal (setq v1 (read-string "Header stuff: (empty is none) " "@header_stuff@")) "")
      ""
    (concat "<property name=\"header_stuff\">"v1"</property>")) & \n >
  (if (string-equal (setq v1 (read-string "Context: (empty is none) " "@context@")) "")
      ""
    (concat "<property name=\"context\">"v1"</property>")) & \n >
  (if (string-equal (setq v1 (read-string "Focus: (empty is none) " "@focus@")) "")
      ""
    (concat "<property name=\"focus\">"v1"</property>")) & \n ?\n
  _
  )

(define-skeleton oacs-tcl-template
  "Declare the Tcl part of OpenACS web page."
  "Brief documentation: "
  "# " (oacs-file-name) \n \n
  "ad_page_contract {" \n \n
  > str | _ \n \n
  > '(oacs-author) ?\n
  > '(oacs-creation-date) ?\n
  (if (not (null oacs-insert-cvs-id))
      (skeleton-insert '(nil
                         > '(oacs-cvs-id) ?\n)))
  > -4 "} {" \n 
  > _ ?\n
  "} -properties {
} -validate {
} -errors {
}

"
  )

(define-skeleton oacs-lib-template
  "Declare an OpenACS Tcl library."
  nil
  "# " (oacs-file-name) \n \n
  "ad_library {" \n \n
  > _ \n \n
  > '(oacs-author) ?\n
  > '(oacs-creation-date) ?\n
  (if (not (null oacs-insert-cvs-id))
      (skeleton-insert '(nil
                         > '(oacs-cvs-id) ?\n)))
  > -4 "}" \n \n 
  )

(define-skeleton oacs-inc-template
  "Declare a OpenACS <include> page."
  "Brief documentation: "
  "# " (oacs-file-name) ?\n
  "#" ?\n
  "# " str | _ ?\n
  "#" ?\n
  "# " '(oacs-author) ?\n >
  "# " '(oacs-creation-date) ?\n
  (if (not (null oacs-insert-cvs-id))
      (skeleton-insert '(nil
                         "# " '(oacs-cvs-id) ?\n)))
  ?\n
  "foreach required_param {" 
  (while (not (equal (setq parameter (read-string "Required parameter: " nil 'oacs-parameter-history)) ""))
    (skeleton-insert '(nil parameter " "))
    ) & -1
  "} {" \n
   "if {![info exists $required_param]} {" \n
   "return -code error \"$required_param is a required parameter.\"" \n
   -4 "}" \n
   -4 "}" \n
   "foreach optional_param {" 
   (while (not (equal (setq parameter (read-string "Optional parameter: " nil 'oacs-parameter-history)) ""))
     (skeleton-insert '(nil parameter " "))
     ) & -1
   "} {" \n
   "if {![info exists $optional_param]} {" \n
   "set $optional_param {}" \n
   -4 "}" \n
   -4 "}"\n
  _
  )

(define-skeleton oacs-vuh-template
  "Declare a .VUH OpenACS web page."
  "Brief documentation: "
  "# " (oacs-file-name) ?\n
  "#" ?\n
  "# " str | _ ?\n
  "#" ?\n
  "# " '(oacs-author) ?\n >
  "# " '(oacs-creation-date) ?\n
  (if (not (null oacs-insert-cvs-id))
      (skeleton-insert '(nil
                         "# " '(oacs-cvs-id) ?\n)))
  ?\n
  _
  )

(define-skeleton oacs-xql-template
  "Declare an OpenACS xql query definition file."
  nil
  "<?xml version=\"1.0\"?>" ?\n
  (if oacs-tdom-installed
      (concat "<!DOCTYPE queryset PUBLIC \"-//OpenACS//DTD XQL 1.0//EN\" \"http://www.thecodemill.biz/repository/xql.dtd\">")) \n
  (concat "<!-- " (oacs-file-name) " -->") \n
  "<!-- " '(oacs-author) " -->" \n
  "<!-- " '(oacs-creation-date) " -->" \n
  (if (not (null oacs-insert-cvs-id))
      (skeleton-insert '(nil
                         "<!-- " '(oacs-cvs-id) " -->" \n \n)))
  "<queryset>" \n 
  (if (string-match ".*-oracle.xql$" (buffer-file-name))
      (skeleton-insert '(nil 
			 \n \n
			 > "<rdbms>" \n
			 > "<type>oracle</type>" \n
			 "<version>"
			 (read-string "Oracle version: " oacs-oracle-version 'oacs-oracle-version-history)
			 "</version>" \n 
			 -2 "</rdbms>" \n &"\n")
		       )
    )
  (if (string-match ".*-postgresql.xql$" (buffer-file-name))
      (skeleton-insert '(nil
			 \n \n
			 > "<rdbms>" \n
			 > "<type>postgresql</type>" \n
			 "<version>"
			 (read-string "PostgreSQL version: " oacs-postgresql-version 'oacs-postgres-version-history)
			 "</version>" \n 
			 -2 "</rdbms>" \n &"\n")
		       ) 
    )
  > _ ?\n
  "</queryset>" \n
  )

(define-skeleton oacs-sql-template
  "Declare an OpenACS sql data model definition file."
  nil
  "-- " \n
  "-- " (buffer-file-name) ?\n
  "-- " \n
  "-- " '(oacs-author) ?\n
  "-- " '(oacs-creation-date) ?\n
  (if (not (null oacs-insert-cvs-id))
      (skeleton-insert '(nil
                         "-- " '(oacs-cvs-id) ?\n)))
  "--" \n \n
  _
  )

(provide 'oacs-headers)
;;; oacs-headers.el ends here
