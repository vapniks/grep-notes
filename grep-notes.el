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
  "Default file to use for `grep-notes' command.
If nil then `grep-notes' will prompt for the file.
If a directory then `grep-notes' will prompt for a file within that directory."
  :group 'grep
  :type 'file)

(defcustom grep-notes-default-options "-i"
  "Extra options for grep searches when no extra options are given by `grep-notes-file-assoc' entry.
Useful options could be -i (case-insensitive search), and -C <N> (include <N> lines of context)."
  :group 'grep
  :type 'string)

(defcustom grep-notes-file-assoc nil
  "Assoc list of the form (COND . (FILE START END OPTIONS)) for use with `grep-notes' command.
COND can be either a major-mode symbol or an sexp which evaluates to non-nil
in buffers of the required type. FILE is the file to be grepped, or a directory containing
files that the user can choose from when `grep-notes' is used interactively.
START and END are either line numbers or regexps matching start and end positions
of the region to grep, or nil (in which case `point-min'/`point-max' will be used).
OPTIONS is a string containing extra options for grep."
  :group 'grep
  :type '(alist :key-type (choice :tag "   Condition       "
				  (symbol :tag "Major-mode")
				  (sexp :tag "S-expression"))
		:value-type (list :tag "Search options  "
				  (file :must-match t)
				  (repeat :tag "Search within following regions"
					  (cons :tag "Region"
						(choice (integer :tag "Start line number")
							(regexp :tag "Start regexp"))
						(choice (integer :tag "End line number  ")
							(regexp :tag "End regexp  "))))
				  (string :tag "Extra grep options"))))

(defcustom grep-notes-invisibility-spec '(t . (other))
  "Indicate which parts of the *grep* buffer to hide by default.
The car and cdr of this cons cell will be used to set `buffer-invisibility-spec' into 
one of two different states with `grep-notes-toggle-invisibility'.
The car and cdr should be lists containing some/all of the symbols 'other 'path &/or 'linum.
The 'other symbol will hide matches that are outside the region specified
by startline & endline in the call to `grep-notes', the 'paths symbol hides
filepaths at the beginning of each match, and 'linum hides line numbers."
  :group 'grep
  :type '(cons (choice (const :tag "Hide all" t)
		       (set (const :tag "Hide irrelevant matches" other)
			    (const :tag "Hide file paths" path)
			    (const :tag "Hide line numbers" linum)))
	       (choice (const :tag "Hide all" t)
		       (set (const :tag "Hide irrelevant matches" other)
			    (const :tag "Hide file paths" path)
			    (const :tag "Hide line numbers" linum)))))

;;;###autoload
(defun grep-notes-toggle-invisibility nil
  "Toggle which parts of the *grep* buffer are invisible.
Toggles `buffer-invisibility-spec' between the car and cdr of `grep-notes-invisibility-spec'."
  (interactive)
  (setq buffer-invisibility-spec
	(if (equal buffer-invisibility-spec
		   (car grep-notes-invisibility-spec))
	    (cdr grep-notes-invisibility-spec)
	  (car grep-notes-invisibility-spec)))
  (redraw-frame))

;;;###autoload
(defun grep-notes (regex file &optional options regions)
  "Grep for matches to REGEX within REGIONS of FILE.

When called interactively REGEX will be prompted for and all other args
will be obtained from `grep-notes-file-assoc' or `grep-notes-default-file'. 
If called with a prefix arg then FILE and OPTIONS will be prompted for.

The whole file will be searched, but matches outside of the regions 
delimited by pairs of regexps or line numbers in regions will be removed from 
the results. REGIONS is a list of pairs of either line numbers or regexps 
matching start and end positions of the regions to search, in order.
The pairs in REGIONS are processed sequentially so they should be in order.
If the first region has nil for its start entry it is taken to mean the start 
of the file, and if the last region has nil for its end entry it is taken to 
mean the end of the file.

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
		    (or (second lst) grep-notes-default-file
			(read-file-name "File to grep: ")))
		  (if current-prefix-arg (read-string "Extra options for grep: ") (fourth lst))
		  (unless current-prefix-arg (third lst)))))
  (if (file-directory-p file)
      (setq file (read-file-name "File to grep: " file)))
  (with-current-buffer (find-file-noselect file t)
    (goto-char (point-min))
    (setq regions (cl-loop for (start . end) in regions
			   for startline = (cond ((numberp start) start)
						 ((null start) (point-min))
						 ((stringp start)
						  (line-number-at-pos (or (re-search-forward start nil t)
									  (point-min)))))
			   for endline = (cond ((numberp end) end)
					       ((null end) (point-max))
					       ((stringp end)
						(line-number-at-pos (or (re-search-forward end nil t)
									(point-max)))))
			   collect (cons startline endline))))
  (grep (concat "grep --color -nH "
		(if (equal options "") grep-notes-default-options options)
		" -e '" regex "' '" (expand-file-name file) "'"))
  (with-current-buffer "*grep*"
    (while (get-buffer-process "*grep*")
      (sleep-for 0.3))
    (let ((inhibit-read-only t)
	  (rx (concat (regexp-quote (file-name-nondirectory file)) ":\\([0-9]+\\)"))
	  (region (pop regions))
	  hidestart)
      (save-excursion
	(goto-char (point-min))	  
	(save-match-data
	  (while (and region (re-search-forward rx nil t))
	    (let ((linum (string-to-number (match-string 1)))
		  (regionstart (car region))
		  (regionend (cdr region)))
	      (cond ((< linum regionstart)
		     (forward-line 0) 
		     (unless hidestart (setq hidestart (point))))
		    ((> linum regionend)
		     (forward-line 0)
		     (unless hidestart (setq hidestart (point)))
		     (if (> (length regions) 0)
			 (setq region (pop regions))))
		    (t (if hidestart (add-text-properties
				      hidestart (line-beginning-position)
				      '(invisible other)))
		       (add-text-properties
			(line-beginning-position) (- (point) (length (match-string 1)))
			'(invisible path))
		       (add-text-properties
			(- (point) (length (match-string 1))) (1+ (point))
			'(invisible linum))
		       (if hidestart (setq hidestart nil))))
	      (forward-line 1)))
	  (if hidestart (add-text-properties hidestart (point-max) '(invisible other))))))
    (setq buffer-invisibility-spec (car grep-notes-invisibility-spec))
    (local-set-key "t" 'grep-notes-toggle-invisibility)))

(provide 'grep-notes)

;; (org-readme-sync)
;; (magit-push)

;;; grep-notes.el ends here


