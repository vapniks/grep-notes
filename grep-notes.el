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
;; cl-lib man
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
;; with the current context. Notes are specified in `grep-notes-alist', and 
;; these notes are associated with the current context (major-mode or some other condition)
;; in `grep-notes-assoc'.
;; 
;;;;;;;;

;;; Commands:
;;
;; Below is a complete list of commands:
;;
;;  `grep-notes-toggle-invisibility'
;;    Toggle which parts of the *grep* buffer are invisible.
;;    Keybinding: M-x grep-notes-toggle-invisibility
;;  `grep-notes'
;;    Grep for matches to REGEX within associated NOTES defined by `grep-notes-alist'.
;;    Keybinding: C-M-s-n
;;
;;; Customizable Options:
;;
;; Below is a list of customizable options:
;;
;;  `grep-notes-alist'
;;    Assoc list with entries of the form (NAME . (FILE REGIONS OPTIONS)) for use with `grep-notes'. 
;;  `grep-notes-assoc'
;;    Assoc list of the form (COND . NAME (PREFIXES)) for use with `grep-notes' command.
;;    default = nil
;;  `grep-notes-default-options'
;;    Extra options for grep searches when no extra options are given by the `grep-notes-alist' entry.
;;    default = "-i"
;;  `grep-notes-invisibility-spec'
;;    Indicate which parts of the *grep* buffer to hide by default.
;;    default = (quote (t other))
;;  `grep-notes-skip-missing-regions'
;;    If non-nil then if any regexp delimited region cannot be found it will be skipped.
;;    default = nil
;;  `grep-notes-more-manpages'
;;    If non-nil then `grep-notes-guess-manpages' will be less restrictive in its choice of manpages.
;;    default = t

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
(require 'man)

;;; Code:

;; TODO:
;;   1) Option to use helm to display results?
;;   2) Ability to quickly copy and paste parts of results strings (matched by another regexp?)

;; simple-call-tree-info: DONE
(defcustom grep-notes-default-options "-i"
  "Extra options for grep searches when no extra options are given by `grep-notes-alist' entry.
Useful options could be -i (case-insensitive search), and -C <N> (include <N> lines of context)."
  :group 'grep
  :type 'string)

;; simple-call-tree-info: CHECK
(defcustom grep-notes-assoc nil
  "Assoc list of elements of the form (COND . NAME (PREFIXES)) for use with `grep-notes' command.
COND can be either a major-mode symbol, or an sexp which evaluates to non-nil in buffers of the required type.
PREFIXES is an optional list of numbers indicating numeric prefix arguments for selecting these notes (see below).
NAME is a key for selecting notes from `grep-notes-alist'.

If COND matches the current `major-mode' or evaluates to non-nil, then the notes in `grep-notes-alist' with
key equal to NAME will be searched if either PREFIXES is nil (default), or it contains the current numeric
prefix arg. Usually you will leave PREFIXES empty, but if you want more control over which notes to view, 
e.g. for choosing between a fast shallow search or a slow deep search, then you may want to set it.

Note: you can have several different elements matching the same context, but only if the corresponding entries
in `grep-notes-alist' are all grep entries, and not arbitrary function entries."
  :group 'grep
  :type '(repeat (list (choice :tag "Condition "
			       (symbol :tag "Major-mode")
			       (sexp :tag "S-expression"))
		       (string :tag "Name/Description")
		       (repeat :tag "Allowed numeric prefixes (leave empty to match all)"
			       (integer :tag "Prefix")))))

(defvar grep-notes-default-alist
  '(("pdfgrep"
     arbitrary pdfgrep
     ((eval . (let* ((files (file-expand-wildcards
			     (replace-regexp-in-string
			      "/$" "" (ido-read-file-name
				       "PDF files/dir (you can use wildcards): "))))
		     (pat (read-string "Pattern to search for: "))
		     (args (read-string
			    "Args for pdfgrep: "
			    (concat pdfgrep-options " "
				    (when pdfgrep-ignore-case "-i ")
				    (when (cl-some 'file-directory-p files) "-r ")))))
		(concat "pdfgrep " args " " pat " "
			(mapconcat 'identity files " "))))))
    ("grep"
     arbitrary grep
     ((eval . (let* ((files (file-expand-wildcards
			     (replace-regexp-in-string
			      "/$" "" (ido-read-file-name
				       "Text files/dir (you can use wildcards): "))))
		     (pat (read-string "Pattern to search for: "))
		     (args (read-string
			    "Extra args for grep (in addition to --color -nH): "
			    (concat grep-notes-default-options " "
				    (when (cl-some 'file-directory-p files) "-r ")))))
		(concat "grep --color -nH " args "-e '" pat "' "
			(mapconcat 'identity files " ")))))))
  "Default values for `grep-notes-alist' that will always be offered to the user.")

;; simple-call-tree-info: CHECK
(defcustom grep-notes-alist nil
  "Assoc list of note specification entries for use with `grep-notes'.
Each entry can be a list of the form (NAME FILES REGIONS OPTIONS) or (NAME pdfgrep FILES OPTIONS),
 (NAME arbitrary FUNCTION ARGS), or (NAME command CMD PREFIX), where pdfgrep, arbitrary & command 
are symbols indicating the search type.
The first form is used for grep searches, the 2nd for pdfgrep searches, and the third & fourth for 
arbitrary functions & commands (such as `helm-recoll').

NAME should be a short description/label for the associated notes which will be offered to the user 
when `grep-notes' is called with a prefix arg, or when there are no other notes associated with the 
current buffer. 

FILES is the file to be grepped, or a list of such files, or a glob pattern for matching multiple 
files (but in this case REGIONS will not be respected), or in the 1st form only, a function of no args 
which returns any of those things (e.g. `grep-notes-make-manpage-files').

In the first form, REGIONS is a list of region specifications, each of which can be either:
 1) a regexp matching an org header (without initial stars or whitespace, can include tags)
 2) a cons cell of regexps matching the start and end of the region
 3) a cons cell of start and end line numbers for the region
 4) a function which takes the current major-mode as argument and returns one of the 
    aforementioned types. For example if the function is `symbol-name' then the associated
    regions will be org-headers named by the major-mode.
 5) the symbol 'grep-notes-repeat - this will repeat the previous org-header or regexp region 
    specification until no more matches are possible.
If REGIONS is empty then the whole file will be used. 

OPTIONS is an optional string containing extra options for grep/pdfgrep.

In the third form (NAME arbitrary FUNCTION ARGS) FUNCTION can be any function, and ARGS is a list of 
arguments for that function. The members of ARGS can either be values to be treated directly as 
arguments to FUNCTION, or cons cells of the form (eval . ARG) where ARG is a form that will be
evaluated before being passed to FUNCTION. This 3rd form is provided as a convenience so that you can 
use the same user interface for all your document searching needs. 
In the fourth form (NAME command CMD PREFIX) CMD can be any interactive command, and PREFIX is a value 
to set `current-prefix-arg' before calling CMD with `call-interactively'.

NOTE: you cannot mix different type of entries, i.e. when `grep-notes' tries to find notes matching 
the current context using `grep-notes-assoc', if the first matching entry is of type 'pdfgrep, 'arbitrary
or 'command then any other matching entries will be ignored."
  :group 'grep
  :type '(alist :key-type (string :tag "Name/Description")
		:value-type (choice
			     (list :tag "grep file(s)"
				   (choice (file :must-match t)
					   (repeat (file :must-match t))
					   (function :tag "Function"
						     :help-echo
						     "Function with no arguments which returns a filename"))
				   (repeat :tag "Search within following regions"
					   (choice (string :tag "Org header")
						   (cons :tag "Start/end regexps"
							 (regexp :tag "Start regexp")
							 (regexp :tag "End regexp  "))
						   (cons :tag "Line numbers"
							 (integer :tag "Start line number")
							 (integer :tag "End line number  "))
						   (function :tag "Function returning a region spec")
						   (const :tag "Repeat last org-header or regexp spec until no more matches"
							  grep-notes-repeat)))
				   (string :tag "Extra grep options"))
			     (list :tag "pdfgrep pdf file(s)"
				   (const pdfgrep)
				   (choice (file :must-match t)
					   (repeat (file :must-match t)))
				   (string :tag "pdfgrep options"))
			     (list :tag "Arbitrary function"
				   (const arbitrary)
				   (function :tag "Function")
				   (repeat :tag "Args" (choice (sexp :tag "Arg")
							       (cons :tag "Evaled arg"
								     (const eval)
								     (sexp :tag "Arg"
									   :help-echo "This arg will be evalled before being passed to the function")))))
			     (list :tag "Interactive command"
				   (const command)
				   (function :tag "Command")
				   (sexp :tag "Prefix arg")))))

;; simple-call-tree-info: DONE
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

;; simple-call-tree-info: DONE
(defcustom grep-notes-skip-missing-regions nil
  "If non-nil then if any regexp delimited region cannot be found it will be skipped.
Otherwise an error will be thrown (this can happen if you put the regions in the wrong order)."
  :group 'grep
  :type 'boolean)

;; simple-call-tree-info: DONE
(defcustom grep-notes-more-manpages t
  "If non-nil then `grep-notes-guess-manpages' will be less restrictive in its choice of manpages."
  :group 'grep
  :type 'boolean)

;;;###autoload
;; simple-call-tree-info: DONE  
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

;; simple-call-tree-info: DONE
(defun grep-notes-propertize-line (pos offset)
  "Add inivisble 'path & 'linum props to current line in *grep* buffer.
POS is position of end of line number, OFFSET is length of line number."
  (add-text-properties (line-beginning-position)
		       (- pos offset)
		       '(invisible path))
  (add-text-properties (- pos offset) (1+ pos)
		       '(invisible linum)))

;; simple-call-tree-info: DONE
(defun grep-notes-add-props-to-grep (file regions pos)
  "Add invisibility props to lines of *grep* buffer matching FILE.
REGIONS is a list of cons cells defining regions to be left unhidden,
and POS is the position in the file to start searching from.
If REGIONS is nil, all lines will be left unhidden."
  (with-current-buffer "*grep*"
    (let ((inhibit-read-only t)
	  (rx (concat (replace-regexp-in-string ;convert wildcards to regexp in rx (matches filename in *grep* buffer)
		       "\\\\\\?" "."
		       (replace-regexp-in-string
			"\\\\\\*" ".*" (regexp-quote file) nil t)
		       nil t)
		      ":\\([0-9]+\\)"))
	  (region (pop regions))
	  (hidestart pos))
      (save-excursion
	(goto-char pos)
	(if (not region)
	    (while (re-search-forward rx nil t)
	      (grep-notes-propertize-line (point) (length (match-string 1)))
	      (forward-line 1))
	  (save-match-data
	    (while (and region (re-search-forward rx nil t))
	      (let ((linum (string-to-number (match-string 1)))
		    (regionstart (car region))
		    (regionend (cdr region)))
		(cond ((< linum regionstart)
		       (forward-line 1))
		      ((> linum regionend)
		       (forward-line 0)
		       (unless hidestart (setq hidestart (point))) ;indicate that we have left a region
		       (if regions
			   (setq region (pop regions))
			 (setq region nil)
			 (while (re-search-forward rx nil t)) ;skip remaining lines for this file
			 (forward-line 1)))
		      (t (unless (not hidestart) ;if this is the first matching line of the region, hide previous lines
			   (add-text-properties hidestart (line-beginning-position)
						'(invisible other))
			   (setq hidestart nil)) ;indicate that we are in a region
			 (grep-notes-propertize-line (point) (length (match-string 1)))
			 (forward-line 1)))))
	    (if hidestart (add-text-properties hidestart
					       (point) '(invisible other)))))
	(point)))))

;; simple-call-tree-info: DONE
(defun grep-notes-regexp-to-lines (startrx &optional endrx orgheaderp nowarn)
  "Return a cons cell of line numbers matching STARTRX and ENDRX, or nil if none found.
If ENDRX is nil then use the last line instead.
If ORGHEADERP is non-nil then find org-header matching STARTRX, and return the start
and end line numbers of that header and its contents.
If STARTRX can't be found, either throw an error, or display a warning depending
on the value of `grep-notes-skip-missing-regions'. If NOWARN is non-nil and STARTRX
doesn't match then return nil with no error or warning even if `grep-notes-skip-missing-regions' 
is non nil."
  (cl-flet
      ((nomatch (regex) (unless nowarn
			  (let ((type (if orgheaderp "org-header" "region")))
			    (if grep-notes-skip-missing-regions
				(message "Warning: can't find %s matching \"%s\", skipping.."
					 type regex)
			      (error "Cant find %s matching \"%s\"" type regex))
			    nil)))) ;must return nil if there is no match
    (let ((pos (re-search-forward (concat (if orgheaderp "^\\*+\s-*") startrx) nil t))
	  startline endline)
      (if (not pos)
	  (nomatch startrx)
	(setq startline (line-number-at-pos pos))
	(if orgheaderp
	    (progn (org-forward-heading-same-level 1 t)
		   (setq endline (line-number-at-pos))
		   (unless (/= endline startline)
		     (goto-char (point-max))))
	  (if endrx
	      (unless (re-search-forward endrx nil t)
		(nomatch endrx))
	    (goto-char (point-max))))
	(setq endline (line-number-at-pos))
	(cons startline endline)))))

;;;###autoload
;; simple-call-tree-info: CHECK
(defun grep-notes-get-notes (arg)
  "Return a list of notes for the current buffer.
If ARG is a non-nil list (e.g. the value of `current-prefix-arg' when C-u is pressed) 
then the user will be prompted for a set of notes from `grep-notes-alist', otherwise
the notes will be selected automatically using `grep-notes-assoc'. If no associated
notes can be found using `grep-notes-assoc', but there is a member of `grep-notes-alist'
with key \"default\", then that will be used."
  (let ((allnotes (append grep-notes-alist grep-notes-default-alist)))
    (if (and arg (listp arg))
	(list (assoc (completing-read "Notes to search: " (mapcar 'car allnotes))
		     allnotes))
      (or (cl-remove-if 'null
			(mapcar (lambda (val)
				  (let ((test (car val))
					(note (cdr val)))
				    (and (if (symbolp test)
					     (eq major-mode test)
					   (eval test))
					 (or (null (cl-second note))
					     (memq (prefix-numeric-value arg)
						   (cl-second note)))
					 (assoc (car note) allnotes))))
				grep-notes-assoc))
	  (list (assoc "default" allnotes))))))

;;;###autoload
;; simple-call-tree-info: CHECK
(defun grep-notes (notes regex)
  "Grep for matches to REGEX within associated NOTES defined by `grep-notes-alist'.

When called interactively REGEX will be prompted for and NOTES will be obtained from `grep-notes-alist'. 
These NOTES are selected automatically using `grep-notes-assoc', unless a non-numeric prefix arg is used
or no matching notes can be found, in which case the user will be prompted for a set of notes in `grep-notes-alist'.

Note: only grep options from the first matching set of notes will be used."
  (interactive (let* ((notes (grep-notes-get-notes current-prefix-arg))
		      (regex (when (not (memq (cl-second (car notes)) '(arbitrary command)))
			       (read-regexp (format
					     "Find notes in %S matching regexp"
					     (mapcar 'car notes))
					    (if mark-active
						(buffer-substring-no-properties
						 (region-beginning) (region-end))
					      (thing-at-point 'symbol))))))
		 (list notes regex)))
  (let* ((notes (mapcar 'cdr notes))
	 (firstnote (car notes)))
    (cl-case (car firstnote)
      (arbitrary (if (null (caddr firstnote))
		     (funcall (cadr firstnote))
		   (apply (cadr firstnote)
			  (mapcar
			   (lambda (x)
			     (if (and (listp x)
				      (eq (car x) 'eval))
				 (eval (cdr x))
			       x))
			   (caddr firstnote)))))
      (command (let ((current-prefix-arg (caddr firstnote)))
		 (call-interactively (cadr firstnote))))
      (pdfgrep (let ((files (cadr firstnote))
		     (opts (caddr firstnote)))
		 (pdfgrep-mode)
		 (pdfgrep (concat "pdfgrep "
				  (if (or (null opts)
					  (equal opts ""))
				      (concat pdfgrep-options
					      (when pdfgrep-ignore-case " -i "))
				    opts)
				  " \"" regex "\" "
				  (if (listp files)
				      (mapconcat 'identity files " ")
				    files)))))
      (t
       ;; expand elements with multiple files into multiple elements
       (setq notes (cl-loop for (files regions options) in notes
			    if (stringp files) collect (list files regions options)
			    else if (functionp files)
			    nconc (let ((vals (funcall files)))
				    (if (stringp vals)
					(list (list vals regions options))
				      (mapcar (lambda (file) (list file regions options)) vals)))
			    else if (listp files)
			    nconc (mapcar (lambda (file) (list file regions options)) files)
			    else do (error "Invalid file(s) argument: %s" files)))
       (let ((opts (caddar notes)))
	 (grep (concat "grep --color -nH "
		       (if (or (null opts)
			       (equal opts ""))
			   grep-notes-default-options
			 opts)
		       " -e '" regex "' "
		       (mapconcat (lambda (x) (expand-file-name (car x))) notes " "))))
       ;; macro gets args for `grep-notes-regexp-to-lines' from region and puts in start, end, 
       ;; and orgp for use in body
       (macrolet ((getargs (region body)
			   `(destructuring-bind (start end orgp)
				(cond ((stringp ,region)
				       (list ,region nil t))
				      ((and (consp ,region)
					    (or (stringp (car ,region))
						(and (numberp (car ,region))
						     (numberp (cdr ,region)))))
				       (list (car ,region) (cdr ,region) nil))
				      (t (error "Invalid region %s" ,region)))
			      ,body)))
	 (while (get-buffer-process "*grep*") (sleep-for 0.3))
	 (cl-loop for (file regions options) in notes
		  with pos = 1
		  for newregions = nil
		  for regions = (mapcar (lambda (r) (if (functionp r) (funcall r major-mode) r))
					regions) ;this needs to be outside `with-current-buffer'
		  for existingbuf = (get-file-buffer file)
		  do (unless (not (file-exists-p file))
		       (with-current-buffer (find-file-noselect file t)
			 (save-excursion
			   (org-save-outline-visibility nil
			     (goto-char (point-min))
			     (if (or (eq major-mode 'org-mode)
				     outline-minor-mode)
				 (outline-show-all))
			     (cl-loop for region in regions
				      with prevregion = nil
				      do (if (eq region 'grep-notes-repeat)
					     (getargs prevregion
						      (if (numberp start) (error "Invalid repeated region")
							(while (car newregions)
							  (push (grep-notes-regexp-to-lines start end orgp t)
								newregions))))
					   (getargs region
						    (push (grep-notes-regexp-to-lines start end orgp)
							  newregions)))
				      (setq prevregion region)))))
		       (unless existingbuf (kill-buffer (get-file-buffer file))))
		  (setq pos (grep-notes-add-props-to-grep
			     file (cl-remove-if (lambda (r) (or (null r) (null (car r))))
						(nreverse newregions))
			     pos))))
       (with-current-buffer "*grep*" (local-set-key "t" 'grep-notes-toggle-invisibility))
       (setq buffer-invisibility-spec (car grep-notes-invisibility-spec))))))

;; simple-call-tree-info: CHECK
(defun grep-notes-make-manpage-files (&optional names)
  "Return names of temporary files containing contents of manpage(s) in NAMES.
NAMES can be the name of a manpage, or a list of such names, of nil. 
If NAMES is nil then `grep-notes-guess-manpages' will be called to try and guess 
manpage names to use, but if this returns nil then nil will be returned."
  (let ((Man-notify-method 'meek))
    (setq names (cond ((stringp names) (list names))
		      ((null names) (grep-notes-guess-manpages))
		      ((listp names) names)))
    (if names
	(cl-loop for name in names
		 for sanename = (concat (replace-regexp-in-string "[()]" "_" name)
					"_grep-notes_manpage_")
		 for existingfile = (car (directory-files temporary-file-directory
							  t (regexp-quote sanename)))
		 for file = (or existingfile (make-temp-file sanename))
		 unless existingfile
		 do (with-current-buffer (Man-getpage-in-background
					  (Man-translate-references name))
		      (sleep-for 0.1)
		      (while (get-buffer-process (current-buffer))) ;wait for man to finish
		      (write-region nil nil file))
		 collect file))))

;; simple-call-tree-info: CHECK
(defun grep-notes-make-info-files (filenames)
  "Return names of temporary files containing contents of info file in FILENAMES.
FILENAMES can be the name of an info file, or a list of such names."
  (info-initialize)
  (if (stringp filenames) (setq filenames (list filenames)))
  (let* ((filepaths (mapcar 'Info-find-file filenames)))
    (cl-loop for path in filepaths
	     for sanename = (concat (file-name-nondirectory path)
				    "_grep-notes_infofile_")
	     for existingfile = (car (directory-files temporary-file-directory
						      t (regexp-quote sanename)))
	     for file = (or existingfile (make-temp-file sanename))
	     unless existingfile
	     do (with-temp-buffer
		  (insert-file path)
		  (write-region nil nil file))
	     collect file)))

;; simple-call-tree-info: CHECK  can this be improved?
(defun grep-notes-guess-manpages nil
  "Try to guess appropriate manpages for the current context."
  (let* ((rxsuffix (concat "\\(:?(.*)\\)?"
			   (if grep-notes-more-manpages "\\([^[:alpha:]]+.*\\)?")
			   "$"))
	 (parts (split-string buffer-file-name "/"))
	 (etcparts (and parts (cl-member "^etc$" parts :test 'string-match)))
	 (logparts (and parts (cl-member "^log$" parts :test 'string-match)))
	 namesrx)
    (setq namesrx (regexp-opt
		   (let ((name (if etcparts (car (last etcparts))
				 (if logparts (cadr logparts)))))
		     (cond (etcparts
			    (append
			     (list name
				   (file-name-sans-extension name)
				   (replace-regexp-in-string
				    "[^[:alnum:]]+.*$" "" name))
			     (list (cadr etcparts))))
			   (logparts
			    (list name
				  (file-name-sans-extension name)
				  (replace-regexp-in-string
				   "[^[:alnum:]]+.*$" "" name)))
			   ((eq major-mode 'sh-mode)
			    (list (thing-at-point 'symbol)))))))
    (if (not (equal namesrx ""))
	(mapcar 'substring-no-properties
		(Man-completion-table
		 "" (lambda (name)
		      (string-match (concat "^" namesrx rxsuffix) name))
		 t)))))

;;;###autoload
(defun grep-notes-clear-stored-manpages nil
  "Delete all manpages created by `grep-notes-make-manpage-files'.
Use this command if you dont seem to be getting the right results from
searching manpages using `grep-notes'. It may be because the manpages
were not rendered properly, and need to be recreated."
  (interactive)
  (mapcar 'delete-file
	  (directory-files
	   temporary-file-directory t "_grep-notes_manpage_")))

(provide 'grep-notes)

;; (org-readme-sync)
;; (magit-push)

;;; grep-notes.el ends here


