;;; oacs-format.el --- OACS formatting extensions

;; Copyright (C) 2004  the Code Mill

;; Author: Bart Teeuwisse <bart.teeuwisse@thecodemill.biz>
;; Keywords: convenience, tools, extensions

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

;; OACS formatting extensions to re-format Tcl, Html, Sql, or Adp code.

;;; Code:

;; HTML/ADP format tools

(defun oacs-quote-tags ()
  "Quote unquoted HTML/XML tags"
  (save-excursion 
    (replace-regexp "=\\([^\\\"][^ >]*?[^\\\"]\\|[^\\\"]\\)\\([ >]\\)" "=\"\\1\"\\2" nil (point-min) (point-max))
    )
  )

(defun oacs-format-empty-element-tags ()
  "Add missing / to empty element tags"
  (save-excursion
    (replace-regexp "\\(<\\(img\\|input\\|maste\\|includ\\|formwidget\\|formhelp\\)[^>]*?[^/]\\)>" "\\1 />" nil (point-min) (point-max))
    (replace-regexp "<hr>" "<hr />" nil (point-min) (point-max))
    (replace-regexp "<br>" "<br />" nil (point-min) (point-max))
    )
  )

(defun oacs-format-separate-tags ()
  "Place adjacent tags on separate lines"
  (save-excursion
    (replace-regexp ">\\s-*<" ">
<" nil (point-min) (point-max))
    )
  )

(defun oacs-format-includes ()
  "Place all include attribubtes on a separate line."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "<include" nil t)
      (replace-regexp "\\s-+\\([^=\"]+=\\)" "
\\1" nil (point) (search-forward-regexp ">" nil t))
      )
    )
  )

(defun oacs-format-html ()
  "Format an HTML page"
  (interactive)
  (oacs-quote-tags)
  (oacs-format-empty-lines 1)
  (oacs-format-empty-element-tags)
  (oacs-format-separate-tags)
  (oacs-format-includes)
  (indent-region (point-min) (point-max) nil)
  )

(defun oacs-format-empty-lines (count)
  "Replace multiple empty lines by COUNT empty lines. Mostly used with
a COUNT of 2 for a single empty line or 1 to remove empty lines
altogether."
  (save-excursion
    (replace-regexp "\\(
\\s-*\\)\\{2,\\}" (make-string count ?\n) nil (point-min) (point-max))
    )
  )

(defun oacs-format-comments ()
  "Make sure that comments are filed and both preceded and followed by
an empty line."
  (save-excursion

    ;; Preceed all comments with an empty line.
 
    (replace-regexp "\\(^[^#]*\\(?:\\w\\|}\\).*
\\)\\(\\s-*#\\)" "\\1
\\2" nil (point-min) (point-max))

    ;; Proceed all comments with an empty line.

    (replace-regexp "\\(^\\s-*#.*
\\)\\(\\s-*\\b[^#
]\\)" "\\1
\\2" nil (point-min) (point-max))

    ;; Remove comments line w/o any text, just #'s and
    ;; whitespaces. They would interfere with the next step: filling
    ;; the paragraphs.

    (replace-regexp "^\\s-*#+\\s-*
" "" nil (point-min) (point-max))

    ;; Remove all leading #'s

    (replace-regexp "#+\\s-*$" "" nil (point-min) (point-max))

    ;; Replace multiple #'s to a single #

    (replace-regexp "##+" "#" nil (point-min) (point-max))

    ;; Fill all comment paragraphs.

    (goto-char (point-min))
    (while (search-forward-regexp "^\\s-*#" nil t)
      (backward-sentence)
      (if (not (equal (point) (point-min)))
	  (fill-paragraph nil)
	)
      (forward-paragraph)
      )
    )
  )

(defun oacs-format-query-as ()
  "Place the keyword as on the same line as the field being selected"
  (save-excursion
    (replace-regexp "\\(
\\|\\s-\\)+as\\b" " as" nil (point-min) (point-max))
    )
  )

(defun oacs-format-operators ()
  "Place the operators =, ==, =>, < and > on the same line as the
field(s) being compared/assigned. "
  (save-excursion
    (replace-regexp "\\(
\\|\\s-\\)+\\(=\\|>\\|<\\)" " \\2" nil (point-min) (point-max))
    )
  )

(defun oacs-format-logical-operators ()
  "Place logical operators || and && at the beginning of a new line."
  (save-excursion
    (replace-regexp "^\\(.*?\\w.*?\\)\\(||\\|&&\\)\\s-*\\\\?\\s-*\n?\\s-+" "\\1
\\2 " nil (point-min) (point-max))
    )
  )

(defun oacs-format-brackets ()
  "Remove spaces before and after brackets."
  (save-excursion

    ;; Remove spaces between ]}, }}, }] and ]] as well as leading
    ;; spaces before ] or }.

    (replace-regexp "\\(]\\|\\b\\|}\\)\\s-+\\(}\\|]\\)" "\\1\\2" nil (point-min) (point-max))

    ;; Remove spaces trailing }.

    (replace-regexp "\\({\\|\\[\\)\\s-+" "\\1" nil (point-min) (point-max))

    ;; Remove empty lines before a closing brace.

    (replace-regexp "\\(
\\s-*\\)+\\(
\\s-*}\\)" "\\2" nil (point-min) (point-max))

    ;; Place closing db_* } bracket at end of query instead of a new line.

    (replace-regexp "\\(db_.*{[^\\}]*\\)
\\s-*}" "\\1}" nil (point-min) (point-max))
    )
  )

(defun oacs-format-proc-call ()
  "Replace multiple spaces before a [ bracket by a single space and
remove all trailing spaces. Replace all spaces before the closing ]
bracket. And place named parameters on a separate line."
  (save-excursion
    (replace-regexp "\\(\\s-\\)+\\(\\[\\)\\(\\s-*\\)" " \\2" nil (point-min) (point-max))
    (replace-regexp "\\(\\s-\\)+\\(\\]\\)" "\\2" nil (point-min) (point-max))
    (goto-char (point-min))
    (while (search-forward-regexp "\\[" nil t)
      (replace-regexp "\\([^\\\\]\\b\\)\\(
\\|\\s-\\)+-" "\\1 \\\\
-" nil (point) (search-forward-regexp "\\]" nil t))
      )
    (indent-region (point-min) (point-max) nil)
    )
  )

(defun oacs-format-proc ()
  "Format layout of ad_proc, proc and proc_doc."
  (let ((proc "\\(?:^\\s-*\\(?:ad_\\)?proc\\(?:_doc\\)?\\)"))
    (save-excursion

      ;; Place an empty line below the ad_proc, proc or proc_doc
      ;; definition.

      (replace-regexp 
       (concat "\\("
	       proc 
	       ".*{\\)

+\\([^
]\\)") "\\1
\\2" nil (point-min) (point-max))

      ;; Remove leading and trailing empty lines around } {

      (replace-regexp "\\(
+\\)\\(} \\(?:-properties \\)?{\\)\\(
*\\)" "
\\2
" nil (point-min) (point-max))
      )
    )
  )

(defun oacs-format-proc-parameters ()
  "Format procedure parameters. Each parameter is placed on a new line."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^\\s-*\\(?:ad_\\)?proc\\(?:_doc\\)?.*?{" nil t)
      (match-end 0)
      (backward-char)
      (let ((start (point))
	    (end (save-excursion
		   (forward-sexp)
		   (point))))
	(save-restriction
	  (forward-char)
	  (narrow-to-region start end)
	  (if (not (looking-at "\n"))
	      (insert "\n")
	    )
	  (while (condition-case nil
		     (progn 
		       (forward-sexp)
		       t)
		   (error nil))
	    (if (not (looking-at "\n"))
		(insert "\n")
	      )
	    )
	  )
	)
      )
    )
  )

(defun oacs-format-page-contract ()
  "Remove empty lines below ad_page_contract."
  (save-excursion
    (replace-regexp "\\(ad_page_contract {\\)\\(
+\\)" "\\1
" nil (point-min) (point-max))
    )
  )

(defun oacs-format-set ()
  "Remove superfluous spaces in 'set' and ad_page_contract
initialization instructions."
  (save-excursion
    (replace-regexp "set\\s-+\\([^ ]+\\)\\s-\\{2,\\}" "set \\1 " nil (point-min) (point-max))
    (replace-regexp "^\\(\\s-+{[^ ]+\\)\\s-\\{2,\\}" "\\1 " nil (point-min) (point-max))
    )
  )

(defun oacs-replace-return ()
  "Replace return with ad-script-abort."
  (save-excursion
    (query-replace-regexp "^\\(\\s-*\\)return;?$" "\\1ad_script_abort" nil (point-min) (point-max))
    )
  )

(defun oacs-format-tcl ()
  "Format a Tcl page"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (delete-trailing-whitespace)
    (oacs-format-empty-lines 2)
    (oacs-format-proc)
    (oacs-format-proc-parameters)
    (oacs-format-comments)
    (oacs-format-query-as)
    (oacs-format-operators)
    (oacs-format-logical-operators)
    (oacs-format-brackets)
    (oacs-format-proc-call)
    (oacs-format-page-contract)
    (oacs-format-set)
    (indent-region (point-min) (point-max) nil)
    (mmm-parse-buffer)
    )
  )

(defun oacs-format-sql-comments ()
  "Make sure that SQL comments are filed and both preceded and followed by
an empty line."
  (save-excursion

    ;; Preceed all comments with an empty line.
 
    (replace-regexp "\\(^[^-]*\\(?:\\w\\|}\\).*
\\)\\(\\s-*--\\)" "\\1
\\2" nil (point-min) (point-max))

    ;; Proceed all comments with an empty line.

    (replace-regexp "\\(^\\s-*--.*
\\)\\(\\s-*\\b[^-
]\\)" "\\1
\\2" nil (point-min) (point-max))

    ;; Fill all comment paragraphs. Commented out because filling
    ;; comments in sql.el doesn't work too well.

;;     (goto-char (point-min))
;;     (while (search-forward-regexp "^\\s-*--" nil t)
;;       (backward-sentence)
;;       (if (not (equal (point) (point-min)))
;; 	  (fill-paragraph nil)
;; 	)
;;       (forward-paragraph)
;;       )
    )
  )

(defun oacs-format-sql ()
  "Format a SQL script"
  (interactive)
  (save-excursion 
    (replace-regexp "^\\(\\s-\\|-\\)\\{4,\\}
" "" nil (point-min) (point-max))
    (replace-regexp "^.*?---.*?
" "" nil (point-min) (point-max))
    (replace-regexp "(\\s-*\\(.*?\\)\\s-*)" "(\\1)" nil (point-min) (point-max))
    (replace-regexp "\\(\\s-\\|
\\)+then" " then" nil (point-min) (point-max))
    (replace-regexp "\\(\\s-\\|
\\)+default " " default " nil (point-min) (point-max))
    (replace-regexp "\\s-+in\\s-+" " in " nil (point-min) (point-max))
    (replace-regexp "\\s-+=>\\s-+" " => " nil (point-min) (point-max))
    (replace-regexp "\\(\\w\\)\\s-\\{2,\\}\\(\\w\\|=\\|!\\)" "\\1 \\2" nil (point-min) (point-max))
    (replace-regexp "^\\(\\s-*\\)\\(select.*\\)\\s-+\\(into \\)" "\\1\\2
\\1\\3" nil (point-min) (point-max))
    (replace-regexp "^\\(\\s-*\\)\\(select.*[^)]\\)
\\s-*\\(into\\)" "\\1\\2
\\1\\3" nil (point-min) (point-max))
    (replace-regexp "^\\(\\s-*\\)\\(into.*[^)]\\)
\\s-*\\(from\\)" "\\1\\2
\\1\\3" nil (point-min) (point-max))
    (replace-regexp "^\\(\\s-*\\)\\(\\(delete\\|from\\).*[^)]\\)
\\s-*\\(where\\)" "\\1\\2
\\1\\4" nil (point-min) (point-max))
    (replace-regexp "^\\(\\s-*\\)\\(update.*[^)]\\)
\\s-*\\(set\\)" "\\1\\2
\\1\\3" nil (point-min) (point-max))
    (replace-regexp "^\\(\\s-*\\)\\(set.*[^)]\\)
\\s-*\\(where\\)" "\\1\\2
\\1\\3" nil (point-min) (point-max))
    (replace-regexp "^\\(\\s-*\\)\\(\\(where\\|and\\|or\\).*[^)]\\)
\\s-*\\(and\\|or\\)" "\\1\\2
\\1\\4" nil (point-min) (point-max))
    (replace-regexp "\\(if.*,\\)
\\s-*" "\\1 " nil (point-min) (point-max))
    (untabify (point-min) (point-max))
    (replace-regexp "^\\s-\\{10\\}\\(\\w\\|-\\)" "        \\1" nil (point-min) (point-max))
    (replace-regexp "^\\s-\\{9\\}\\(\\w\\|-\\)" "        \\1" nil (point-min) (point-max))
    (replace-regexp "^\\s-\\{7\\}\\(\\w\\|-\\)" "        \\1" nil (point-min) (point-max))
    (replace-regexp "^\\s-\\{6\\}\\(\\w\\|-\\)" "        \\1" nil (point-min) (point-max))
    (replace-regexp "^\\s-\\{3\\}\\(\\w\\|-\\)" "    \\1" nil (point-min) (point-max))
    (replace-regexp "^\\s-\\{5\\}\\(\\w\\|-\\)" "    \\1" nil (point-min) (point-max))
    (oacs-format-sql-comments)
    (oacs-format-empty-lines 2)
    )
  )

(provide 'oacs-format)
;;; oacs-format.el ends here
