;;; oacs-psgml.el --- OACS PSGML extension

;; Copyright (C) 2004  the Code Mill

;; Author: Bart Teeuwisse <bart.teeuwisse@thecodemill.biz>
;; Keywords: extensions, convenience, tools

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

(require 'psgml)
(autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
(autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
(add-to-list 'auto-mode-alist '("\\.html" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.htm" . sgml-mode))
(add-to-list 'auto-mode-alist '("\\.adp" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xml" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xql" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.info" . xml-mode))
(add-to-list 'sgml-markup-faces 
	     '((start-tag . font-lock-function-name-face)
	       (end-tag . font-lock-function-name-face)
	       (comment . font-lock-comment-face)
	       (pi . bold)
	       (sgml . bold)
	       (doctype . bold)
	       (entity . font-lock-type-face)
	       (shortref . font-lock-function-name-face))
	     )
(setq sgml-set-face t)
(setq-default sgml-indent-data t)
(setq-default sgml-ignore-undefined-elements nil)
(setq-default sgml-auto-activate-dtd t)
(add-to-list 'sgml-custom-dtd
	     '("XQL 1.0"
	       "<?xml version=\"1.0\"?>\n<!DOCTYPE queryset PUBLIC \"-//OpenACS//DTD XQL 1.0//EN\" \"http://www.thecodemill.biz/repository/xql.dtd\">" ))
(add-to-list 'sgml-custom-dtd
	     '("XHTML 1.1"
	       "<?xml version=\"1.0\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"))
(add-to-list 'sgml-custom-dtd
	     '("XHTML 1.0 Frameset"
	       "<?xml version=\"1.0\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" ))
(add-to-list 'sgml-custom-dtd
	     '("XHTML 1.0 Strict"
	       "<?xml version=\"1.0\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" ))
(add-to-list 'sgml-custom-dtd
	     '("XHTML 1.0 Transitional"
	       "<?xml version=\"1.0\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" ))
(add-to-list 'sgml-custom-dtd
	     '("Package 1.0"
	       "<?xml version=\"1.0\"?>\n<!DOCTYPE package PUBLIC \"-//OpenACS//DTD Package 1.0//EN\" \"http://www.thecodemill.biz/repository/package.dtd\">" ))
(add-to-list 'sgml-custom-dtd
	     '("HTML 4.01 Frameset"
	       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/REC-html40/frameset.dtd\">"))
(add-to-list 'sgml-custom-dtd
	     '("HTML 4.01 Strict"
	      "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">"))
(add-to-list 'sgml-custom-dtd
	     '("HTML 4.01 Transitional"
	       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html401/loose.dtd\">"))
(add-to-list 'sgml-custom-dtd
	     '("HTML 3.2"
	       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">"))

(provide 'oacs-psgml)
;;; oacs-psgml.el ends here
