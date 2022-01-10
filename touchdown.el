;;; touchdown.el --- Major mode for editing td-agent/fluentd configuration files -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA.
;; Copyright (C) 2021-2022 by Jeremy A GRAY.

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

(defun touchdown--create-parameter-regexp (parameter)
  "Return a regular expression matching a fluentd PARAMETER.

Match groups are:

1. Parameter name.
2. Parameter value.
3. Comment, if present."
  (format
   "^[[:space:]]*\\(%s\\)[[:space:]]+\\(.+?\\)\\(?:[[:space:]]*\\|\\(?:[[:space:]]+\\(#.*\\)\\)?\\)?$"
   parameter))

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

(defconst touchdown--boolean-parameter-regexp
  "^[[:space:]]*\\([@[:word:]_]+\\)[[:space:]]+\\(true\\|false\\)\\(?:[[:space:]]*\\|\\(?:[[:space:]]+\\(#.*\\)\\)?\\)?$"
  "Regular expression matching fluentd parameters.

Matches all parts of a parameter line, including trailing comments.
Match groups are:

1. Parameter name.
2. Parameter value.
3. Comment, if present.")

;;; Fluentd syntax symbols.

;; An attepted data structure.
(cl-defstruct
    (touchdown--parameter (:constructor touchdown--parameter-create)
			  (:copier nil))
  name
  type
  default
  options
  required)

(defconst touchdown--plugin-input-tail-parameters
  (list
   (touchdown--parameter-create
    :name "@log_level"
    :type 'string
    :default nil
    :options '("fatal" "error" "warn" "info" "debug" "trace")
    :required nil)
   (touchdown--parameter-create
    :name "@type"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "emit_unmatched_lines"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_stat_watcher"
    :type 'boolean
    :default t
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_watch_timer"
    :type 'boolean
    :default t
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "encoding"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "exclude_path"
    :type 'string'
    :default ()
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "follow_inodes"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "from_encoding"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ignore_repeated_permission_error"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "limit_recently_modified"
    :type "time"
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "multiline_flush_interval"
    :type "time"
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "open_on_every_update"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "path"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "path_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "path_timezone"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "pos_file"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "pos_file_compaction_interval"
    :type 'time
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "read_bytes_limit_per_second"
    :type 'size
    :default -1
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "read_from_head"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "read_lines_limit"
    :type 'integer
    :default 1000
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "refresh_interval"
    :type 'time
    :default 60
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "rotate_wait"
    :type 'time
    :default 5
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "skip_refresh_on_startup"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "tag"
    :type 'string
    :default nil
    :options nil
    :required t)))

(defun touchdown--plugin-input-tail-parameters-names ()
  "Return tail parameter names as a list."
  (let ((params touchdown--plugin-input-tail-parameters)
	(names ()))
    (message "params: %s" params)
    (while params
      (push (touchdown--parameter-name (car params)) names)
      (setq params (cdr params)))
    names))

;; Faces and font lock.

(defface touchdown-directives
  '((t (:inherit font-lock-function-name-face)))
  "Face of directive.")

(defface touchdown-subdirectives
  '((t (:inherit font-lock-function-name-face)))
  "Face of subdirective.")

(defface touchdown-file-include
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for file includes.")

(defface touchdown-file-include-path
  '((t (:inherit font-lock-string-face)))
  "Face for file include path.")

(defface touchdown-tag
  '((t (:inherit font-lock-variable-name-face)))
  "Face of tag parameter")

(defface touchdown-parameter-name
  '((t (:inherit font-lock-keyword-face)))
  "Face of parameter name")

(defface touchdown-parameter-value
  '((t (:inherit font-lock-constant-face)))
  "Face of parameter value")

(defvar touchdown-font-lock-keywords
  `((,touchdown--parameter-regexp (1 'touchdown-parameter-name)
                                  (2 'touchdown-parameter-value t nil))
    (,touchdown--file-include-regexp (1 'touchdown-file-include t nil)
                                     (2 'touchdown-file-include-path t nil))
    (,touchdown--main-directive-regexp (1 'touchdown-directives)
                                       (2 'touchdown-directives nil t)
                                       (3 'touchdown-tag nil t)
                                       (4 'touchdown-directives nil t))
    (,touchdown--sub-directive-regexp (1 'touchdown-subdirectives)
                                      (2 'touchdown-subdirectives)
                                      (3 'touchdown-subdirectives))))

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
    (looking-at touchdown--any-directive-opening-regexp)))

(defun touchdown--closing-directive-line-p ()
  "Determine if point is on a line containing a closing directive."
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at touchdown--any-directive-closing-regexp)))

(defun touchdown--parameter-line-p ()
  "Determine if the current line is a parameter line.

Determine if the current line is a parameter line by matching the
line against `touchdown--parameter-regexp'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at touchdown--parameter-regexp)))

(defun touchdown--boolean-parameter-line-p ()
  "Determine if the current line has a boolean parameter.

Determine if the current line has a boolean parameter by matching the
line against `touchdown--boolean-parameter-regexp', essentially
looking for a parameter value of `true` or `false`."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at touchdown--boolean-parameter-regexp)))

(defun touchdown--directive-closed-p (directive curpoint)
  "Determine if DIRECTIVE is closed before CURPOINT."
  (save-excursion
    (let ((close-directive
	   (touchdown--create-closing-directive-regexp directive))
          (curline (line-number-at-pos curpoint)))
      (when (re-search-forward close-directive curpoint t)
        (< (line-number-at-pos) curline)))))

(defun touchdown--within-directive-p (directive &optional noisy)
  "Determine if the point is currently within DIRECTIVE.

Return t if point is on or after the line of the opening tag of
DIRECTIVE and on or before the line of the closing tag of DIRECTIVE,
or nil otherwise.

If NOISY is not nil, `message' status information during execution."
  (interactive)
  (save-excursion
    (let ((current-line (line-number-at-pos (point)))
	  ;; (current-point (point))
	  (open-label (touchdown--create-opening-directive-regexp directive))
	  (open-line nil)
	  (close-label (touchdown--create-closing-directive-regexp directive))
	  (close-line nil)
	  (status t))
      (if noisy
	  (progn
	    (message "touchdown--within-directive-p:  current-line %s" current-line)
	    (message "touchdown--within-directive-p:  open-label %s" open-label)
	    (message "touchdown--within-directive-p:  close-label %s" close-label)))
      (beginning-of-line)
      (cond ((looking-at open-label)
	     (setq open-line (line-number-at-pos (point))))
	    ((re-search-backward open-label (point-min) t)
	     (setq open-line (line-number-at-pos (point))))
	    (t
	     (if noisy
		 (message "touchdown--within-directive-p:  opening tag %s not found" directive))
	     (setq status nil)))
      (cond ((re-search-forward close-label (point-max) t)
	     (setq close-line (line-number-at-pos (point))))
	    (t
	     (if noisy
		 (message "touchdown--within-directive-p:  closing tag %s not found" directive))
	     (setq status nil)))
      (when status
	  (if (and (>= current-line open-line)
		   (<= current-line close-line))
	      (setq status t)
	    (setq status nil)))
      (if noisy
	  (message "touchdown--within-directive-p:  final status %s" status))
      status)))

(defun touchdown--within-label-p (&optional noisy)
  "Determine if the point is currently within a label directive.

Return t if point is on or after the line containing '<label>' and on
or before the line containing '</label>', or nil otherwise.

If NOISY is not nil, `message' status information during execution."
  (interactive)
  (save-excursion
    (touchdown--within-directive-p "label" noisy)))

(defun touchdown--at-root-level-p (&optional noisy)
  "Determine if the point is currently at the root level.

Determine if the point is currently at the root level, outside of all
other directives.

If NOISY is not nil, `message' status information during execution."
  (interactive)
  (save-excursion
    (let ((directives (touchdown--where-am-i noisy))
	  (status nil))
      (cond ((equal directives nil)
	     (setq status t))
	    (t
	     (setq status nil)))
      (if noisy
	  (message "touchdown--at-root-level-p:  %s" status))
      status)))

;; Line and location data retrieval.

(defun touchdown--where-am-i (&optional noisy)
  "Return the current directive list, or nil for none.

Return the list of nested directives currently containing point, or
nil if the point is not within any directive.

If NOISY is not nil, `message' status information during execution."
  (interactive)
  (save-excursion
    (let ((current-point (point))
	  (current-line (line-number-at-pos (point)))
	  (directives ()))
      (goto-char (point-min))
      (while (and (< (line-number-at-pos (point)) current-line) (not (eobp)))
	(if noisy
	    (message "line: %s current-line: %s" (line-number-at-pos (point)) current-line))
	(when (touchdown--opening-directive-line-p)
	  (progn
	    (if noisy
		(message "found opening %s" (match-string-no-properties 2)))
	    (push (match-string-no-properties 2) directives)))
	(when (touchdown--closing-directive-line-p)
	  (progn
	    (if noisy
		(message "found closing %s" (match-string-no-properties 2)))
	    (setq directives (cdr directives))))
	(forward-line 1))
      (if noisy
	  (message "%s" directives))
      directives)))

(defun touchdown--what-type-am-i (&optional noisy)
  "Return the type for the current directive, or nil for none.

Return the type for the directive currently containing point, or nil
if the point is not within any directive or if the directive does have
a type.

If NOISY is not nil, `message' status information during execution."
  (interactive)
  (save-excursion
    (let ((type-regexp (touchdown--create-parameter-regexp "@type"))
	  (type "")
	  (found nil)
	  (directives (touchdown--where-am-i)))
      (cond ((equal directives nil)
	     (setq type nil))
	    (t
	     (if noisy
		 (message "touchdown--what-type-am-i:  directive %s" (car directives)))
	     (re-search-backward (touchdown--create-opening-directive-regexp (car directives)))
	     (beginning-of-line)
	     (while (and
		     (not (looking-at (touchdown--create-closing-directive-regexp (car directives))))
		     (not (eobp))
		     (not found))
	       (cond ((looking-at type-regexp)
		      (setq type (match-string-no-properties 2)
			    found t))
		     (t
		      (forward-line 1))))
	     (if noisy
		 (message "touchdown--what-type-am-i:  type %s" type))
	     type)))))

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

(defun touchdown--produce-terms ()
  "Return current valid terms."
  (let ((location (car (touchdown--where-am-i))))
    (cond ((equal location nil)
	   touchdown--directives)
	  ((equal location "source")
	   (cond ((equal (touchdown--what-type-am-i) nil)
		  (list "@type" "tag" "<parse>" "</parse>"))
		 ((equal (touchdown--what-type-am-i) "tail")
		  (touchdown--plugin-input-tail-parameters-names))))
	  ((equal location "parse")
	   (list "@type"))
	  (t
	   touchdown--directives))))

(defun touchdown--dynamic-completion-table (str)
  "Return all possible completions for STR.

Return a list of all possible matches for STR in the fluentd syntax considering the current location of the point."
  (let ((directives (touchdown--produce-terms))
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

;;; Utilities.

(defun touchdown-insert-source (prefix)
  "Insert a source block.

Insert an empty source block on the current line, if blank, or the
next if not.  With PREFIX, insert a source block with a blank `@type`
and a parse block with a blank `@type`."
  (interactive "P")
  (save-excursion
    (let ((string "<source>
</source>
"))
      (cond (prefix
	     (setq string "<source>
  @type # add type

  <parse>
    @type # add type
  </parse>
</source>
")))
      (move-beginning-of-line nil)
      (cond ((looking-at "^$")
	     (insert string))
	    (t
	     (forward-line 1)
	     (insert string))))))

(defun touchdown-swap-boolean ()
  "Swap a boolean parameter value.

Swap the value of a boolean parameter if the point is currently on a
line containing a parameter with a boolean value."
  (interactive)
  (save-excursion
    (when (touchdown--boolean-parameter-line-p)
      (beginning-of-line)
      (cond ((equal "true" (match-string-no-properties 2))
	     (re-search-forward "\\btrue\\b" nil t 1)
	     (replace-match "false" nil nil))
	    ((equal "false" (match-string-no-properties 2))
	     (re-search-forward "\\bfalse\\b" nil t 1)
	     (replace-match "true" nil nil))))))

(defun touchdown-indent-buffer ()
  "Indent a touchdown buffer.

Indent the current buffer using the fluentd/td-agent syntax."
  (interactive)
  (save-excursion
    (indent-region (buffer-end -1) (buffer-end 1))))

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
  (set (make-local-variable 'comment-start-skip) "[:space:]*#[:space:]*")

  (define-key touchdown-mode-map "\C-c\C-tbs" 'touchdown-insert-source)
  (define-key touchdown-mode-map "\C-c\C-ti" 'touchdown-indent-buffer)
  (define-key touchdown-mode-map "\C-c\C-tt" 'touchdown-swap-boolean))

;;;###autoload
(add-to-list
 'auto-mode-alist
 '("\\(fluentd?\\.conf\\|td-agent\\.conf\\)\\'" . touchdown-mode))

(provide 'touchdown)

;;; touchdown.el ends here
