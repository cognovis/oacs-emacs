;;; oacs-error-log.el --- Monitor an AOLServer error log.

;; Copyright (C) 2004  the Code Mill

;; Author: Bart Teeuwisse <bart.teeuwisse@thecodemill.biz>
;; Keywords: tools, processes, extensions

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

(define-generic-mode 'nsd-error-mode
  nil
  '("Bug" "Debug" "Error" "Fatal" "Notice" "Warning")
  '(("^\\(\\[.*\\]\\)" . font-lock-string-face)
    ("\\([^:]:\\{1,1\\}[A-Za-z_0-9]+\\)" . font-lock-variable-name-face)
    ("sql(.*?):\\([^[]*\\)" . font-lock-function-name-face))
  nil
  (list
   (lambda ()))
  "Major mode for AOLServer error logs. 

If you turn on font-lock-mode, you also get nice highlighting.")

(defun oacs-monitor-error-log ()
"Monitor an AOLServer error log in a buffer. SERVER is the name of the AOLServer.

The logfile must be called SERVER-error.log. The directories
oacs-error-log-dirs are sought until one containing a logfile is found.

The buffer is put in Major mode NSD and Minor mode OACS."
  (interactive)
  (let* ((server (read-string
		  "Server: " nil 'oacs-error-log-history))
	 (error-basename (concat server "-error.log"))
	 (error-log-dirs oacs-error-log-dirs)
	 (error-log-dir nil)
	 (error-log "/*"))

    ;; Switch to buffer if the log is already open. If not locate the
    ;; log file and open.

    (when (not (get-buffer error-basename))
      (while (and error-log-dirs (not (file-readable-p error-log)))
	(setq error-log-dir (file-name-as-directory (car error-log-dirs)))
	(setq error-log (concat error-log-dir error-basename))
	(message "Trying %s" error-log)
	(setq error-log-dirs (cdr error-log-dirs)))
      (if (not (file-readable-p error-log))
	  (error "No logfile for %s found" server))

      ;; Open the located error log in nsd-error-mode and move to the
      ;; end of the log. Then turn auto-revert-mode on.

      (message "Monitoring %s" error-log)
      (find-file error-log)
      (goto-char (point-max))
      (nsd-error-mode)
      (auto-revert-mode 1)

      ;; MMM-ify the region that was just added to the log.

      (setq oacs-error-log-prev-point-max (point-max))
      (set-buffer error-basename)
      (add-hook 'after-revert-hook 'oacs-mmm-ify-log-region 1)
      (setq default-directory error-log-dir))
    (set-window-buffer (selected-window) (get-buffer-create error-basename))))

(defun oacs-mmm-ify-log-region ()
  "MMM-ify the region that was just added to the AOLserver error
log. Move the POINT to the end of the buffer if the POINT was at the
end of the buffer before reverting."
  (if (and oacs-mmm-ify-log
	   (< oacs-error-log-prev-point-max (point-max)))
      (progn
	(count-lines-region oacs-error-log-prev-point-max (point-max))
	(mmm-parse-region oacs-error-log-prev-point-max (point-max))
	(if (eq oacs-error-log-prev-point-max (point))
	    (goto-char (point-max)))
	(setq oacs-error-log-prev-point-max (point-max)))))

(defun oacs-revert-log () 
  "Revert the buffer to the log file on disk without asking for
confirmation"
  (interactive)
  (revert-buffer nil t))

;; Add global map so that a log can be opened from any Emacs buffer.

(global-set-key "\M-oml" 'oacs-monitor-error-log)

;; Associate log files in the error log directories with
;; nsd-error-mode

(let* ((error-log-dirs oacs-error-log-dirs))
  (while (and error-log-dirs)
    (add-to-list 'auto-mode-alist (cons (concat (car error-log-dirs) ".*error\\.log") 'nsd-error-mode))
    (setq error-log-dirs (cdr error-log-dirs))))

;; Also associate Daemontools backup log files with Nsd-error-mode.

(add-to-list 'auto-mode-alist '("@.*\\.s" . nsd-error-mode))

(provide 'oacs-error-log)
;;; oacs-error-log.el ends here
