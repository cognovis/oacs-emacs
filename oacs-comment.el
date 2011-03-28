;;; oacs-comment.el --- OACS tcl comment adaptation

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

;; Adapt the Tcl comment functions so that ad_proc documentation text
;; is classified as comments and can be formatted using auto-fill and
;; M-q.

;;; Code:

(defadvice tcl-in-comment (after tcl-in-comment)
  "Classify ad_proc documentation text as comments so that
tcl-do-fill-paragraph also can be used on the documentation text."
  (save-excursion
    (let ((bound (point))
	  (start (if (not (search-backward-regexp "}[ \t]+{" nil t 2))
		     (goto-char (point-min))
		   )))
      (if (search-forward-regexp "^\\s-*\\(\\(ad_\\)?proc\\(_doc\\)?\\|ad_library\\).*{" bound t)
	  (setq ad-return-value t)
	)
      )
    )
  )
(ad-activate 'tcl-in-comment)

(defadvice tcl-do-fill-paragraph (around fill-proc-documentation)
  "Comment the documentation of ad_proc or proc_doc so that
tcl-do-fill-paragraph can fill the documentation paragraphs. Uncomment
the documentation after filling the paragraph."
  (save-excursion
    (let ((bound (point))

	  ;; Locate the last but one section separator, or if there is
	  ;; none start from the top of the buffer.

	  (start (if (not (search-backward-regexp "}[ \t]+{" nil t 2))
		     (goto-char (point-min))
		   )))

      ;; Check if there is a proc definition between the section
      ;; separator and the current location. 

      (if (not (search-forward-regexp "^\\s-*\\(\\(ad_\\)?proc\\(_doc\\)?\\|ad_library\\).*{" bound t))

	  ;; The current location is NOT in a proc documentation
	  ;; section. Execute tcl-do-fill-paragraph w/o massaging the
	  ;; paragraph first.

	  (progn
	    (goto-char bound)
	    ad-do-it)

	;; The current location is in a proc documentation
	;; section. Comment the section before running
	;; tcl-do-fill-paragraph. Uncomment the section after filling
	;; the paragraph.

	(goto-char bound)
	(backward-up-list)
	(comment-region (save-excursion
			  (forward-to-indentation 1)
			  (point))
			(save-excursion
			  (forward-sexp)
			  (backward-char)
			  (point)))
	(goto-char bound)
	ad-do-it
	(goto-char bound)
	(backward-up-list)
	(uncomment-region (save-excursion
			  (forward-to-indentation 1)
			  (point))
			(save-excursion
			  (forward-sexp)
			  (backward-char)
			  (point)))
	)
      )
    )
  )
(ad-activate 'tcl-do-fill-paragraph)

;;; Always activate the minor tcl-auto-fill-mode in major tcl-mode
(add-hook 'tcl-mode-hook 'tcl-auto-fill-mode)

(provide 'oacs-comment)
;;; oacs-comment.el ends here
