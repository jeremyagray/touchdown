;;; touchdown.el --- Major mode for editing td-agent/fluentd configuration files -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA.
;; Copyright (C) 2021 by Jeremy A GRAY.

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Author: Jeremy A GRAY <gray@flyquackswim.com>
;; Maintainer: Jeremy A GRAY <gray@flyquackswim.com>
;; Created: 2016 as emacs-fluentd-mode, 2021 ported as touchdown-mode
;; URL: https://github.com/jeremyagray/touchdown
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "27"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing td-agent/fluentd configuration files.

;;; Code:

(require 'cl-lib)

(defgroup touchdown nil
  "Major mode for editing fluentd/td-agent configuration files."
  :group 'languages)

;; Syntax table.

(defvar touchdown-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?#  "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?<  "(>"  table)
    (modify-syntax-entry ?>  ")<"  table)
    table)
  "Syntax table for `touchdown-mode', derived from `fundamental-mode'.

Sets the comment delimiters and '<>' as a pair of grouping symbols.")

;; Regular expressions.

(defun touchdown--create-directive-regexp (directive)
  "Return a regular expression matching DIRECTIVE.

Matches any directive type.  Match groups are:

1. Opening bracket.
2. Directive.
3. Tag, if present.
4. Closing bracket.
5. Comment, if present."
  (format
   "^[[:space:]]*\\(</?\\)\\(%s\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"
   directive))

(defun touchdown--create-opening-directive-regexp (directive)
  "Return a regular expression matching opening DIRECTIVE.

Matches any opening directive type.  Match groups are:

1. Opening bracket.
2. Directive.
3. Tag, if present.
4. Closing bracket.
5. Comment, if present."
  (format
   "^[[:space:]]*\\(<\\)\\(%s\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"
   directive))

(defun touchdown--create-closing-directive-regexp (directive)
  "Return a regular expression matching closing DIRECTIVE.

Matches any closing directive type.  Match groups are:

1. Opening bracket.
2. Directive.
3. Closing bracket.
4. Comment, if present."
  (format
   "^[[:space:]]*\\(</\\)\\(%s\\)\\(>\\)[[:space:]]*\\(#.*\\)?$"
   directive))

(defun touchdown--create-options-regexp (options &optional group)
  "Return a regular expression option group representing OPTIONS.

Convert the list OPTIONS into a regular expression option group.  If
GROUP is non-nil, wrap OPTIONS in group or a shy group if GROUP is 1."
  (let ((opts (cdr options))
	(re (car options)))
    (while opts
      (setq re (concat re (format "\\|%s" (car opts)))
	    opts (cdr opts)))
    (cond ((and (equal group 'shy) (not (equal re "")))
	   (setq re (format "\\(?:%s\\)" re)))
	  ((and group (not (equal re "")))
	   (setq re (format "\\(%s\\)" re)))
	  (t
	   t))
    re))

(defconst touchdown--main-directive-regexp
  "^[[:space:]]*\\(</?\\)\\(source\\|match\\|filter\\|system\\|label\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching a main directive.
Matches all parts of a main directive line, including trailing
comments.  Match groups are:

1. Opening bracket.
2. Directive.
3. Tag, if present.
4. Closing bracket.
5. Comment, if present.")

(defconst touchdown--sub-directive-regexp
  "^[[:space:]]*\\(</?\\)\\(\\(?:buffer\\|parse\\|record\\)\\)\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching a subdirective.
Matches all parts of a subdirective line, including trailing comments.
Match groups are:

1. Opening bracket.
2. Directive.
3. Closing bracket.
4. Comment, if any.")

(defconst touchdown--any-directive-regexp
  "^[[:space:]]*\\(</?\\)\\(source\\|match\\|filter\\|system\\|label\\|buffer\\|parse\\|record\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching any directive.
Matches all parts of a directive line, including trailing comments.
Match groups are:

1. Opening bracket.
2. Directive.
3. Tag, if present.
4. Closing bracket.
5. Comment, if present.")

(defconst touchdown--main-directive-opening-regexp
  "^[[:space:]]*\\(<\\)\\(source\\|match\\|filter\\|system\\|label\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching a main directive opening.
Matches all parts of a main directive opening line, including trailing
comments.  Match groups are:

1. Opening bracket.
2. Directive.
3. Tag, if present.
4. Closing bracket.
5. Comment, if present.")

(defconst touchdown--sub-directive-opening-regexp
  "^[[:space:]]*\\(<\\)\\(\\(?:buffer\\|parse\\|record\\)\\)\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching an opening subdirective.
Matches all parts of an opening subdirective line, including trailing
comments.  Match groups are:

1. Opening bracket.
2. Directive.
3. Closing bracket.
4. Comment, if present.")

(defconst touchdown--any-directive-opening-regexp
  "^[[:space:]]*\\(<\\)\\(source\\|match\\|filter\\|system\\|label\\|buffer\\|parse\\|record\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching any opening directive.
Matches all parts of an opening directive line, including trailing
comments.  Match groups are:

1. Opening bracket.
2. Directive.
3. Tag, if present.
4. Closing bracket.
5. Comment, if present.")

(defconst touchdown--main-directive-closing-regexp
  "^[[:space:]]*\\(</\\)\\(source\\|match\\|filter\\|system\\|label\\)\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching a main directive closing.
Matches all parts of a main directive closing line, including trailing
comments.  Match groups are:

1. Opening bracket.
2. Directive.
3. Closing bracket.
4. Comment, if present.")

(defconst touchdown--sub-directive-closing-regexp
  "^[[:space:]]*\\(</\\)\\(\\buffer\\|parse\\|record\\)\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching a subdirective closing.
Matches all parts of a closing subdirective line, including trailing
comments.  Match groups are:

1. Opening bracket.
2. Directive.
3. Closing bracket.
4. Comment, if present.")

(defconst touchdown--any-directive-closing-regexp
  "^[[:space:]]*\\(</\\)\\(source\\|match\\|filter\\|system\\|label\\|buffer\\|parse\\|record\\)\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching a main directive closing.
Matches all parts of a main directive closing line, including trailing
comments.  Match groups are:

1. Opening bracket.
2. Directive.
3. Closing bracket.
4. Comment, if present.")

(defconst touchdown--file-include-regexp
  "^[[:space:]]*\\(@include\\)[[:space:]]*\\(.*\\)[[:space:]]*$"
  "Regular expression for matching a file include.")

(defconst touchdown--parameter-regexp
  "^[[:space:]]*\\([@[:word:]_]+\\)[[:space:]]+\\(.+?\\)\\(?:[[:space:]]*\\|\\(?:[[:space:]]+\\(#.*\\)\\)?\\)?$"
  "Regular expression matching fluentd parameters.

Matches all parts of a parameter line, including trailing comments.
Match groups are:

1. Parameter name.
2. Parameter value.
3. Comment, if present.")

;; Fluentd syntax symbols.

(defun touchdown--parameter-symbol-maker (name type default options required)
  "Create a symbol representing a parameter.

Return a symbol named NAME with the properties type TYPE, default
DEFAULT (nil is no default), options OPTIONS (nil is no options, use
TYPE), and required REQUIRED (boolean).  If TYPE is boolean, options
is automatically set to (t nil).  Parameter lists should be of the
same format as the argument list to enable use of `apply'."
  (let ((sym (make-symbol name)))
    (put sym 'type type)
    (put sym 'default default)
    (put sym 'required required)
    (if (equal 'boolean (get sym 'type))
	(put sym 'options '(t nil))
      (put sym 'options options))
    sym))

(defconst touchdown--plugin-input-tail-parameters
  '(
    '("@log_level"
      'string
      nil
      '("fatal" "error" "warn" "info" "debug" "trace")
      nil)
    '("@type" 'string nil nil t)
    '("emit_unmatched_lines" 'boolean nil nil nil)
    '("enable_stat_watcher" 'boolean t nil nil)
    '("enable_watch_timer" 'boolean t nil nil)
    '("encoding" 'string nil nil nil)
    '("exclude_path" 'string' () nil nil)
    '("follow_inodes" 'boolean nil nil nil)
    '("from_encoding" 'string nil nil nil)
    '("ignore_repeated_permission_error" 'boolean nil nil nil)
    '("limit_recently_modified" "time" nil nil nil)
    '("multiline_flush_interval" "time" nil nil nil)
    '("open_on_every_update" 'boolean nil nil nil)
    '("path" 'string nil nil t)
    '("path_key" 'string nil nil nil)
    '("path_timezone" 'string nil nil nil)
    '("pos_file" 'string nil nil nil)
    '("pos_file_compaction_interval" 'time nil nil nil)
    '("read_bytes_limit_per_second" 'size -1 nil nil)
    '("read_from_head" 'boolean nil nil nil)
    '("read_lines_limit" 'integer 1000 nil nil)
    '("refresh_interval" 'time 60 nil nil)
    '("rotate_wait" 'time 5 nil nil)
    '("skip_refresh_on_startup" 'boolean nil nil nil)
    '("tag" 'string nil nil t))
  "List of fluentd tail input plugin parameters.

Order matches the arguments of `touchdown--parameter-symbol-maker'.")

;; Faces and font lock.

(defface touchdown-directives-face
  '((t (:inherit font-lock-function-name-face)))
  "Face of directive.")

(defface touchdown-subdirectives-face
  '((t (:inherit font-lock-function-name-face)))
  "Face of subdirective.")

(defface touchdown-file-include-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for file includes.")

(defface touchdown-file-include-path-face
  '((t (:inherit font-lock-string-face)))
  "Face for file include path.")

(defface touchdown-tag-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face of tag parameter")

(defface touchdown-parameter-name-face
  '((t (:inherit font-lock-keyword-face)))
  "Face of parameter name")

(defface touchdown-parameter-value-face
  '((t (:inherit font-lock-constant-face)))
  "Face of parameter value")

(defvar touchdown-font-lock-keywords
  `((,touchdown--parameter-regexp (1 'touchdown-parameter-name-face)
                                  (2 'touchdown-parameter-value-face t nil))
    (,touchdown--file-include-regexp (1 'touchdown-file-include-face t nil)
                                     (2 'touchdown-file-include-path-face t nil))
    (,touchdown--main-directive-regexp (1 'touchdown-directives-face)
                                       (2 'touchdown-directives-face nil t)
                                       (3 'touchdown-tag-face nil t)
                                       (4 'touchdown-directives-face nil t))
    (,touchdown--sub-directive-regexp (1 'touchdown-subdirectives-face)
                                      (2 'touchdown-subdirectives-face)
                                      (3 'touchdown-subdirectives-face))))

;; Configuration file verification.

(defcustom
  touchdown-fluentd-dry-run-command
  "rvm system do /usr/sbin/td-agent --dry-run --config"
  "Command to execute the fluentd/td-agent dry-run check.

The command, including any rvm or other environment management
commands necessary, to execute the dry-run check on the file attached
to the current buffer."
  :type 'string)

(defun touchdown--verify-configuration ()
  "Verify a configuration with the fluentd/td-agent dry-run check.

Execute command defined in `touchdown-fluentd-dry-run-command' on the
file attached to the current `touchdown-mode' buffer.  Returns t on
success.  Displays errors in a new temporary buffer."
  (interactive)
  (save-excursion
    (let ((filename buffer-file-name)
	  (temp-buffer-name "*fluentd configuration check*"))
      (get-buffer-create temp-buffer-name)
      (setq buffer-read-only nil)
      (let ((retval (call-process-shell-command
		     (format "%s %s"
			     touchdown-fluentd-dry-run-command
			     filename)
		     nil temp-buffer-name)))
	(cond ((equal retval 0)
	       (message "configuration file successfully parsed"))
	      (t
	       (switch-to-buffer-other-window temp-buffer-name)
	       (special-mode)
	       (setq buffer-read-only nil)))))))

;; Line type and location predicates.

(defun touchdown--file-include-line-p ()
  "Determine if point is on a line containing file include."
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at-p touchdown--file-include-regexp)))

(defun touchdown--opening-directive-line-p ()
  "Determine if point is on a line containing an opening directive."
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at-p touchdown--any-directive-opening-regexp)))

(defun touchdown--closing-directive-line-p ()
  "Determine if point is on a line containing a closing directive."
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at-p touchdown--any-directive-closing-regexp)))

(defun touchdown--directive-closed-p (directive curpoint)
  "Determine if DIRECTIVE is closed before CURPOINT."
  (save-excursion
    (let ((close-directive
	   (touchdown--create-closing-directive-regexp directive))
          (curline (line-number-at-pos curpoint)))
      (when (search-forward close-directive curpoint t)
        (< (line-number-at-pos) curline)))))

;; Line and location data retrieval.

(defun touchdown--closing-directive-name ()
  "Return the name of the current closing directive."
  (save-excursion
    (if (touchdown--closing-directive-line-p)
        (let ()
          (move-beginning-of-line 1)
          (looking-at touchdown--any-directive-closing-regexp)
          (match-string-no-properties 2))
      nil)))

(defun touchdown--opening-directive-name ()
  "Return the name of the current opening directive."
  (save-excursion
    (if (touchdown--opening-directive-line-p)
        (let ()
          (move-beginning-of-line 1)
          (looking-at touchdown--any-directive-opening-regexp)
          (match-string-no-properties 2))
      nil)))

(defun touchdown--opening-directive-tag ()
  "Return the current opening directive tag/label."
  (save-excursion
    (if (touchdown--opening-directive-line-p)
        (let ()
          (move-beginning-of-line 1)
          (looking-at touchdown--any-directive-opening-regexp)
          (match-string-no-properties 3))
      nil)))

;; Indentation.

(defcustom touchdown-indent-level 2
  "Indent level."
  :type 'integer)

(defun touchdown--opening-directive-indentation ()
  "Return the indentation of the current opening directive."
  (save-excursion
    (let ((opening-directive touchdown--any-directive-opening-regexp)
          (curpoint (point)))
      (cond ((touchdown--closing-directive-line-p)
             (let* ((directive (touchdown--closing-directive-name))
                    (opening-directive
		     (touchdown--create-directive-regexp directive)))
               (if (not (re-search-backward opening-directive nil t))
                   (error "Opening directive %s not found" directive)
                 (current-indentation))))
            (t
             (let (finish)
               (while (and (not finish)
                           (re-search-backward opening-directive nil t))
                 (let ((directive (match-string-no-properties 2)))
                   (unless (touchdown--directive-closed-p directive curpoint)
                     (setq finish t))))
               (if (not finish)
                   0
                 (+ (current-indentation) touchdown-indent-level))))))))

(defun touchdown-indent-line ()
  "Indent current line of a fluentd/td-agent configuration."
  (interactive)
  (let ((indent-size (touchdown--opening-directive-indentation)))
    (back-to-indentation)
    (when (/= (current-indentation) indent-size)
      (save-excursion
        (delete-region (line-beginning-position) (point))
        (indent-to indent-size)))
    (when (< (current-column) (current-indentation))
      (back-to-indentation))))

;; Completion.

;; Completion functions are currently using `touchdown--directives' as
;; the only source of terms for completion.  The list should be built
;; from lists of directives, subdirectives, parameter names, parameter
;; values (especially booleans or sets), tags, and labels in a context
;; dependent way so that completion suggests completions from the
;; appropriate set of possibilities.

(defconst touchdown--directives
  '("@include" "<source>" "<match" "<filter>" "<system>" "<label " "</source>" "</match>" "</filter>" "</system>" "</label>")
  "List of fluentd main directives.")

;; From s.el.
(defun touchdown--s-shared-start (s1 s2)
  "Return the longest prefix S1 and S2 have in common."
  (declare (pure t) (side-effect-free t))
  (let ((cmp (compare-strings s1 0 (length s1) s2 0 (length s2))))
    (if (eq cmp t) s1 (substring s1 0 (1- (abs cmp))))))

(defun touchdown--matches-syntax-p (str)
  "Determine if STR is part of a term in the fluentd syntax.

Returns non-nil if STR has a match the fluentd configuration syntax,
nil otherwise."
  (let ((directives touchdown--directives)
        (match-p nil))
    (while directives
      (let ((directive (car directives)))
	(if (string-match-p (regexp-quote str) directive)
	    (setq match-p t
		  directives ())
	  (setq directives (cdr directives)))))
    match-p))

(defun touchdown--try-completion (str predicate try)
  "Determine if STR is part of a term in the fluentd syntax.

Return (ignoring PREDICATE and TRY) nil if STR does not match the
fluentd syntax, t if it matches a term exactly, or the longest common
initial sequence from all possible matches in the fluentd syntax."
  (message "touchdown try completion on string %s" str)
  (let ((matches-data nil)
	(matches))
    (if (touchdown--matches-syntax-p str)
	(let ((directives touchdown--directives))
	  (while directives
	    (let ((directive (car directives)))
              (cond ((equal str directive)
		     (setq directives ()
			   matches-data t))
		    ((string-match-p (regexp-quote str) directive)
		     (setq directives (cdr directives)
			   matches (push directive matches)))
		    (t
		     (setq directives (cdr directives))))))))
    (if (not (equal matches-data t))
	(let ((longest (car matches))
	      (matches (cdr matches)))
	  (while matches
	    (let ((match (car matches)))
	      (setq longest (touchdown--s-shared-start match longest)
		    matches (cdr matches))))
	  (setq matches-data longest)))
    (message "touchdown try completion on string %s returning %s" str matches-data)
    matches-data))

(touchdown--try-completion "@include" nil nil)

(defun touchdown--all-completions (str predicate try)
  "Find all matches for STR in the fluentd syntax.

Return (ignoring PREDICATE and TRY) a list of all possible matches for
STR in the fluentd syntax."
  (message "touchdown all completions on string %s" str)
  (let ((directives touchdown--directives)
	(matches nil))
    (while directives
      (let ((directive (car directives)))
        (cond ((string-match-p (regexp-quote str) directive)
	       (setq directives (cdr directives)
		     matches (push directive matches)))
	      (t
	       (setq directives (cdr directives))))))
    (message "touchdown all completions on string %s returning %s" str matches)
    matches))

(defun touchdown--test-completion (str predicate lmb)
  "Determine if STR is part of a term in the fluentd syntax.

Returns (ignoring PREDICATE and LMB) non-nil if STR has a match the
fluentd configuration syntax, nil otherwise."
  (message "touchdown test completion on string %s" str)
  (let ((directives touchdown--directives)
        (match-p nil))
    (while directives
      (let ((directive (car directives)))
	(if (string-match-p (regexp-quote str) directive)
	    (setq match-p t
		  directives ())
	  (setq directives (cdr directives)))))
    (message "touchdown test completion on string %s returning %s" str match-p)
    match-p))

(defun touchdown--dynamic-completion-table (str)
  "Find all matches for STR in the fluentd syntax.

Return a list of all possible matches for STR in the fluentd syntax."
  (let ((directives touchdown--directives)
	(matches nil))
    (while directives
      (let ((directive (car directives)))
        (cond ((string-match-p (regexp-quote str) directive)
	       (setq directives (cdr directives)
		     matches (push directive matches)))
	      (t
	       (setq directives (cdr directives))))))
    matches))

(defun touchdown--completion-at-point-collection (str predicate try)
  "Return completion data for the fluentd configuration syntax.

If TRY is t, acts as a `try-completion' function and returns nil if
STR has no matches from predicate PREDICATE, t if STR has an exact
match from PREDICATE, or the longest common initial sequence from all
possible matches from PREDICATE.  If TRY is nil, acts as an
`all-completions' function and returns all possible completions of STR
allowed by PREDICATE.  Otherwise, acts as a `test-completion'
function."
  (let ((result nil))
    (message "touchdown capc str %s pred %s try %s" str predicate try)
    (cond ((eq try nil)
	   (message "touchdown.el:  getting try-completion data")
	   (setq result (touchdown--try-completion str predicate try)))
	  ((eq try t)
	   (message "touchdown.el:  getting all-completions data")
	   (setq result (touchdown--all-completions str predicate try)))
	  ((eq try 'lambda)
	   (message "touchdown.el:  getting test-completion data")
	   (setq result (touchdown--test-completion str predicate try)))
	  ((eq try 'metadata)
	   (message "touchdown.el:  getting metadata")
	   (setq result nil))
	  ((eq (car try) 'boundaries)
	   (message "touchdown.el:  getting boundaries")
	   (setq result nil))
	  (t
	   (message "touchdown.el:  got unexpected:  %s" try)
	   (setq result nil)))
    result))

(defun touchdown--completion-at-point ()
  "Touchdown mode completion at point function."
  (list (save-excursion
	  (skip-syntax-backward "w_.(")
	  (point))
	(point)
        #'touchdown--completion-at-point-collection
	:predicate #'touchdown--matches-syntax-p))

;;;###autoload
(define-derived-mode touchdown-mode fundamental-mode "Touchdown"
  "Major mode for editing fluentd/td-agent configuration files."
  (setq font-lock-defaults '((touchdown-font-lock-keywords)))

  (add-hook 'completion-at-point-functions
            #'touchdown--completion-at-point nil 'local)

  (make-local-variable 'touchdown-indent-level)
  (set (make-local-variable 'indent-line-function) 'touchdown-indent-line)

  (set (make-local-variable 'comment-indent-function) 'touchdown-indent-line)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) " ")
  (set (make-local-variable 'comment-inline-offset) 2)
  (set (make-local-variable 'comment-start-skip) "[:space:]*#[:space:]*"))

;;;###autoload
(add-to-list
 'auto-mode-alist
 '("\\(fluentd?\\.conf\\|td-agent\\.conf\\)\\'" . touchdown-mode))

(provide 'touchdown)

;;; touchdown.el ends here
