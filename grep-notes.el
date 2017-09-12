;;; grep-notes.el --- quickly display grep results from a notes file related to the current context

;; Filename: grep-notes.el
;; Description: quickly display grep results from a notes file related to the current context
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2017, Joe Bloggs, all rites reversed.
;; Created: 2017-09-12 01:54:08
;; Version: 0.1
;; Last-Updated: Tue Sep 12 02:03:22 2017
;;           By: Joe Bloggs
;;     Update #: 1
;; URL: https://github.com/vapniks/grep-notes
;; Keywords: convenience
;; Compatibility: GNU Emacs 25.2.1
;; Package-Requires: ((cl-lib "2.02"))
;;
;; Features that might be required by this library:
;;
;; cl-lib
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary: 
;;
;; Bitcoin donations gratefully accepted: 1ArFina3Mi8UDghjarGqATeBgXRDWrsmzo
;;
;; The `grep-notes' command provides a quick & easy way to view notes associated
;; with the current context. The `grep-notes-file-assoc' option is used to specify
;; which file to use for grepping based on the current major-mode or some other
;; condition.
;; 
;;;;;;;;

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `grep-notes'
;;    grep for matches to REGEX between STARTLINE and ENDLINE in FILE.
;;    Keybinding: M-x grep-notes
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `grep-notes-default-file'
;;    Default file to use for `grep-notes' command when there is no file associated with 
;;    default = "~/.emacs.d/notes/programming.org"
;;  `grep-notes-file-assoc'
;;    Assoc list of the form (COND . (FILE START END)) for use with `grep-notes' command.
;;    default = nil

;;
;; All of the above can be customized by:
;;      M-x customize-group RET grep-notes RET
;;

;;; Installation:
;;
;; Put grep-notes.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'grep-notes)

;;; History:

;;; Require
(require 'cl-lib)

;;; Code:


(defcustom grep-notes-default-file nil
  "Default file to use for `grep-notes' command when there is no file associated with 
the current buffer in `grep-notes-file-assoc'"
  :group 'grep
  :type 'string)

(defcustom grep-notes-file-assoc nil
  "Assoc list of the form (COND . (FILE START END OPTIONS)) for use with `grep-notes' command.
COND can be either a major-mode symbol or an sexp which evaluates to non-nil
in buffers of the required type. FILE is the file to be grepped. 
START and END are either line numbers or regexps matching start and end positions
of the region to grep, or nil (in which case `point-min'/`point-max' will be used).
OPTIONS is a string containing extra options for grep."
  :group 'grep
  :type '(alist :key-type (choice (symbol :tag "Major-mode")
				  (sexp :tag "sexp"))
		:value-type (list (file :must-match t)
				  (choice (integer :tag "Start line number")
					  (regexp :tag "Start regexp"))
				  (choice (integer :tag "Eng line number")
					  (regexp :tag "End regexp"))
				  (string :tag "Extra grep options"))))

;;;###autoload
(defun grep-notes (regex file &optional startline endline options)
  "Grep for matches to REGEX between STARTLINE and ENDLINE in FILE.

When called interactively REGEX will be prompted for and all other args
will be obtained from `grep-notes-file-assoc' or `grep-notes-default-file'. 
If called with a prefix arg then FILE and OPTIONS will be prompted for.

The whole file will be searched, but matches outside of the regions 
delimited by STARTLINE and ENDLINE will be removed from the results.
STARTLINE and ENDLINE can be either line numbers or regexps matching start 
and end positions of the region to search in. If either of STARTLINE or ENDLINE 
is nil then the start/end of the file will be used respectively.
OPTIONS is a string containing extra options for grep."
  (interactive (let* ((lst (cl-assoc-if (lambda (val) (if (symbolp val)
							  (eq major-mode val)
							(eval val)))
					grep-notes-file-assoc)))
		 (list
		  (read-regexp "Regex "
			       (if mark-active
				   (buffer-substring-no-properties (region-beginning) (region-end))))
		  (if current-prefix-arg (read-file-name "File to grep: ")
		    (or (second lst) grep-notes-default-file))
		  (unless current-prefix-arg (third lst))
		  (unless current-prefix-arg (fourth lst))
		  (if current-prefix-arg (read-string "Extra options for grep: ") (fifth lst)))))
  (with-current-buffer (find-file-noselect file t)
    (setq startline (cond ((numberp startline) startline)
			  ((null startline) (point-min))
			  ((stringp startline)
			   (goto-char (point-min))
			   (line-number-at-pos (or (re-search-forward startline nil t)
						   (point-min)))))
	  endline (cond ((numberp endline) endline)
			((null endline) (point-max))
			((stringp endline)
			 (goto-line (1+ startline))
			 (line-number-at-pos (or (re-search-forward endline nil t)
						 (point-max)))))))
  (grep (concat "grep --color -nH " options " -e '" regex "' '" (expand-file-name file) "'"))
  (with-current-buffer "*grep*"
    (while (get-buffer-process "*grep*")
      (sleep-for 0.3))
    (let ((inhibit-read-only t)
	  (rx (concat (regexp-quote (file-name-nondirectory file)) ":\\([0-9]+\\)"))
	  start end)
      (save-excursion
	(goto-char (point-min))	  
	(save-match-data
	  (while (re-search-forward rx nil t)
	    (let ((linum (string-to-number (match-string 1))))
	      (forward-line 0)
	      (cond ((< linum startline)
		     (unless start (setq start (point))))
		    ((> linum endline)
		     (unless start (setq start (point))))
		    (t (unless (not start)
			 (kill-region start (point))
			 (setq start nil))))
	      (forward-line 1)))
	  (if start (kill-region start (point-max))))))))

(provide 'grep-notes)

;; (org-readme-sync)
;; (magit-push)

;;; grep-notes.el ends here


