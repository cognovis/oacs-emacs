;;; oacs-occur.el --- OACS occur extensions

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

;; OACS occur extensions to quickly locate debug messages, procedure
;; definitions in OpenACS code or error log.

;; Uses color occur if available. Color occur is highly recommended
;; but not required. Download from:

;;    http://www.emacswiki.org/elisp/color-occur.el

;;; Code:

;; Load color-occur if available. 
(condition-case nil
    (require 'color-occur)
  (error nil))

(defun oacs-occur-dbg (&optional level)
  "Find occurances of your dbg messages."
  (interactive)
  (occur 
   (concat "^\\[[^\\w
]*" level "
-*
" (upcase (user-login-name)) "[^\\[]*" nil)
   )
  )

(defun oacs-occur-dbg-bug ()
  (interactive)
  (oacs-occur-dbg " Bug: ")
  "Find occurances of your bug level dbg messages."
)

(defun oacs-occur-dbg-debug ()
  (interactive)
  (oacs-occur-dbg " Debug: ")
  "Find occurances of your debug level dbg messages."
)

(defun oacs-occur-dbg-error ()
  (interactive)
  (oacs-occur-dbg " Error: ")
  "Find occurances of your error level dbg messages."
)

(defun oacs-occur-dbg-fatal ()
  "Find occurances of your fatal level dbg messages."
  (interactive)
  (oacs-occur-dbg " Fatal: ")
)

(defun oacs-occur-dbg-notice ()
  "Find occurances of your notice level dbg messages."
  (interactive)
  (oacs-occur-dbg " Notice: ")
)

(defun oacs-occur-dbg-warning ()
  "Find occurances of your warning level dbg messages."
  (interactive)
  (oacs-occur-dbg " Warning: ")
)

(defun oacs-occur-proc ()
  "Find occurances of ad_proc, proc, or doc_proc definitions."
  (interactive)
  (occur "^\\s-*\\(ad_\\|doc_\\)?proc\\b")
)

(defun oacs-occur-function ()
  "Find occurances of function definitions."
  (interactive)
  (occur "^\\s-*function\\s-+[^ ]+\\s-+(" nil)
)

(defun oacs-occur-query ()
  "Find occurances of query definitions."
  (interactive)
  (occur "^[ 	]*<\\(full\\|partial\\)query" nil)
)

(provide 'oacs-occur)
;;; oacs-occur.el ends here
