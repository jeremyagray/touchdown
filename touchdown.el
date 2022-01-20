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

;; Mode settings.

(defvar touchdown--debug nil
  "Control debugging messages from touchdown functions.

Default is nil, which suppresses debugging information.  Non-nil
enables debugging messages.")

(defun touchdown-toggle-debug ()
  "Toggle the state of `touchdown--debug'.

Toggle the state of `touchdown--debug', message the new state, and
return the new state value."
  (interactive)
  (cond ((equal t touchdown--debug)
         (setq touchdown--debug nil))
        (t
         (setq touchdown--debug t)))
  (message "touchdown--debug is %s" touchdown--debug)
  touchdown--debug)

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

;;; Regular expressions and syntax data.

;; The fluentd syntax consists of an XML-like structure of `<...>` to
;; </...>` delimiters that contain other XML-like delimiters and
;; variable/value lines.  Fluentd calls the top-level XML-like pairs
;; 'directives' (that includes `@include`) and the variable/value
;; lines parameters (at least for ones like `@type`).  Problems arise
;; because like in C, any includes can contain any syntax the
;; top-level file can include, a `<label>` directive can contain
;; `@include`, `<match>`, and `<filter>`, and directives other than
;; `<system>` can contain other XML-like non-directives that are
;; called sections which may contain parameters or sections, ad
;; infinitum.  The top level is also essentially a containing
;; directive, sometimes called `<root>`.

;; To simplify things, all directives and sections will be called
;; sections and will be stored in the `touchdown-section' structure,
;; which will contain parameters in lists of `touchdown--parameter'
;; structures and lists of nested `touchdown--section' structures.
;; Sections will be identified by type, either 'contain' or 'config',
;; with the former indicating an XML-like section and the latter a
;; list of parameters to insert in the current section.

;; Lisp symbols will be named as `touchdown--section-opening-regexp',
;; with the mode name, the syntax type, any qualification, and finally
;; its purpose in the mode.

;; Regular expressions.

;; Specific regular expression generation functions.

(defun touchdown--create-section-regexp (section)
  "Return a regular expression matching SECTION.

Matches any section type.  Match groups are:

1. Opening bracket.
2. Section name.
3. Tag, if present.
4. Closing bracket.
5. Comment, if present."
  (format
   "^[[:space:]]*\\(</?\\)\\(%s\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"
   section))

(defun touchdown--create-section-opening-regexp (section)
  "Return a regular expression matching SECTION opening.

Matches any SECTION opening.  Match groups are:

1. Opening bracket.
2. Section name.
3. Tag, if present.
4. Closing bracket.
5. Comment, if present."
  (format
   "^[[:space:]]*\\(<\\)\\(%s\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"
   section))

(defun touchdown--create-section-closing-regexp (section)
  "Return a regular expression matching SECTION closing.

Matches any SECTION closing.  Match groups are:

1. Opening bracket.
2. Section name.
3. Closing bracket.
4. Comment, if present."
  (format
   "^[[:space:]]*\\(</\\)\\(%s\\)\\(>\\)[[:space:]]*\\(#.*\\)?$"
   section))

(defun touchdown--create-parameter-regexp (parameter)
  "Return a regular expression matching a fluentd PARAMETER.

Match groups are:

1. Parameter name.
2. Parameter value.
3. Comment, if present."
  (format
   "^[[:space:]]*\\(%s\\)[[:space:]]+\\(.+?\\)\\(?:[[:space:]]*\\|\\(?:[[:space:]]+\\(#.*\\)\\)?\\)?$"
   parameter))

(defun touchdown--create-parameter-type-regexp (type)
  "Return a regular expression matching a parameter `@type` TYPE.

Match groups are:

1. Parameter name.
2. Parameter value.
3. Comment, if present."
  (format
   "^[[:space:]]*\\(@type\\)[[:space:]]+\\(%s\\)\\(?:[[:space:]]*\\|\\(?:[[:space:]]+\\(#.*\\)\\)?\\)?$"
   type))

;; REVIEW
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

;; Generic regular expressions.

(defconst touchdown--section-regexp
  "^[[:space:]]*\\(</?\\)\\([[:word:]]+\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching any section opening or closing.

Matches all parts of a section line, including trailing comments.
Match groups are:

1. Opening bracket.
2. Section name.
3. Tag, if present.
4. Closing bracket.
5. Comment, if present.")

(defconst touchdown--section-opening-regexp
  "^[[:space:]]*\\(<\\)\\([[:word:]]+\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching any section opening.

Matches all parts of a section opening line, including trailing
comments.  Match groups are:

1. Opening bracket.
2. Section name.
3. Tag, if present.
4. Closing bracket.
5. Comment, if present.")

(defconst touchdown--section-closing-regexp
  "^[[:space:]]*\\(</\\)\\([[:word:]]+\\)\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching a section closing.

Matches all parts of a section closing line, including trailing
comments.  Match groups are:

1. Opening bracket.
2. Section name.
3. Closing bracket.
4. Comment, if present.")

(defconst touchdown--include-regexp
  "^[[:space:]]*\\(@include\\)[[:space:]]+\\(.+?\\)\\(?:[[:space:]]*\\|\\(?:[[:space:]]+\\(#.*\\)\\)?\\)?$"
  "Regular expression for matching a file include.

Match groups are:

1. Parameter name (`@include`).
2. Parameter value (path).
3. Comment, if present.")

(defconst touchdown--parameter-regexp
  "^[[:space:]]*\\([@[:word:]_]+\\)[[:space:]]+\\(.+?\\)\\(?:[[:space:]]*\\|\\(?:[[:space:]]+\\(#.*\\)\\)?\\)?$"
  "Regular expression matching fluentd parameters.

Matches all parts of a parameter line, including trailing comments.
Match groups are:

1. Parameter name.
2. Parameter value.
3. Comment, if present.")

(defconst touchdown--parameter-boolean-regexp
  "^[[:space:]]*\\([@[:word:]_]+\\)[[:space:]]+\\(true\\|false\\)\\(?:[[:space:]]*\\|\\(?:[[:space:]]+\\(#.*\\)\\)?\\)?$"
  "Regular expression matching fluentd parameters.

Matches all parts of a boolean parameter line, including trailing comments.
Match groups are:

1. Parameter name.
2. Parameter value.
3. Comment, if present.")

;;; Fluentd syntax symbols.

(defconst touchdown--directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory containing touchdown.el, for loading other files.")

(load-file (expand-file-name "syntax.el" touchdown--directory))

;; Faces and font lock.

(defface touchdown-section
  '((t (:inherit font-lock-function-name-face)))
  "Face for section.")

(defface touchdown-include
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for file includes.")

(defface touchdown-include-path
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
    (,touchdown--include-regexp (1 'touchdown-include t nil)
                                (2 'touchdown-include-path t nil))
    (,touchdown--section-regexp (1 'touchdown-section)
                                (2 'touchdown-section nil t)
                                (3 'touchdown-tag nil t)
                                (4 'touchdown-section nil t))))

;; Configuration file verification.

(defcustom
  touchdown-fluentd-dry-run-command
  "rvm system do /usr/sbin/td-agent --dry-run --config"
  "Command to execute the fluentd/td-agent dry-run check.

The command, including any rvm or other environment management
commands necessary, to execute the dry-run check on the file attached
to the current buffer."
  :type 'string)

(defun touchdown-verify-configuration ()
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

(defun touchdown--blank-line-p ()
  "Determine if point is on a blank line.

Blank lines include empty lines or lines with only whitespace.  No
groups are matched."
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at "^[[:space:]]*$")))

(defun touchdown--comment-line-p ()
  "Determine if point is on a line containing only a comment.

Match groups are:

1. Comment."
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at "[[:space:]]*#[[:space:]]*\\(.*\\)[[:space:]]*$")))

(defun touchdown--include-line-p ()
  "Determine if point is on a line containing file include."
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at touchdown--include-regexp)))

(defun touchdown--section-opening-line-p ()
  "Determine if point is on a line opening a section."
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at touchdown--section-opening-regexp)))

(defun touchdown--section-closing-line-p ()
  "Determine if point is on a line closing a section."
  (save-excursion
    (move-beginning-of-line 1)
    (looking-at touchdown--section-closing-regexp)))

(defun touchdown--parameter-line-p ()
  "Determine if the current line is a parameter line.

Determine if the current line is a parameter line by matching the
line against `touchdown--parameter-regexp'."
  (save-excursion
    (beginning-of-line)
    (looking-at touchdown--parameter-regexp)))

(defun touchdown--parameter-type-line-p ()
  "Determine if the current line is a `@type` parameter line."
  (save-excursion
    (beginning-of-line)
    (looking-at (touchdown--create-parameter-regexp "@type"))))

(defun touchdown--parameter-boolean-line-p ()
  "Determine if the current line has a boolean parameter.

Determine if the current line has a boolean parameter by matching the
line against `touchdown--parameter-boolean-regexp', essentially
looking for a parameter value of `true` or `false`."
  (save-excursion
    (beginning-of-line)
    (looking-at touchdown--parameter-boolean-regexp)))

(defun touchdown--section-closed-p (section curpoint)
  "Determine if SECTION is closed before CURPOINT."
  (save-excursion
    (let ((section-closing
           (touchdown--create-section-closing-regexp section))
          (curline (line-number-at-pos curpoint)))
      (when (re-search-forward section-closing curpoint t)
        (< (line-number-at-pos) curline)))))

(defun touchdown--within-section-p (section)
  "Determine if the point is currently within SECTION.

Return t if point is on or after the line of the SECTION opening and
on or before the line of the SECTION closing, or nil otherwise."
  (save-excursion
    (let ((current-line (line-number-at-pos (point)))
          (open-label (touchdown--create-section-opening-regexp section))
          (open-line nil)
          (close-label (touchdown--create-section-closing-regexp section))
          (close-line nil)
          (status t))
      (when touchdown--debug
        (message "touchdown--within-section-p:  current-line %s" current-line)
        (message "touchdown--within-section-p:  open-label %s" open-label)
        (message "touchdown--within-section-p:  close-label %s" close-label))
      (beginning-of-line)
      (cond ((looking-at open-label)
             (setq open-line (line-number-at-pos (point))))
            ((re-search-backward open-label (point-min) t)
             (setq open-line (line-number-at-pos (point))))
            (t
             (when touchdown--debug
               (message "touchdown--within-section-p:  section opening %s not found" section))
             (setq status nil)))
      (cond ((re-search-forward close-label (point-max) t)
             (setq close-line (line-number-at-pos (point))))
            (t
             (when touchdown--debug
               (message "touchdown--within-section-p:  section closing %s not found" section))
             (setq status nil)))
      (when status
        (if (and (>= current-line open-line)
                 (<= current-line close-line))
            (setq status t)
          (setq status nil)))
      (when touchdown--debug
        (message "touchdown--within-section-p:  final status %s" status))
      status)))

(defun touchdown--within-label-p ()
  "Determine if the point is currently within a label section.

Return t if point is on or after the line containing '<label>' and on
or before the line containing '</label>', or nil otherwise."
  (save-excursion
    (touchdown--within-section-p "label")))

(defun touchdown--at-root-level-p ()
  "Determine if the point is currently at the root level.

Determine if the point is currently at the root level, outside of all
other sections."
  (save-excursion
    (let ((sections (touchdown--where-am-i))
          (status nil))
      (cond ((equal sections nil)
             (setq status t))
            (t
             (setq status nil)))
      (when touchdown--debug
        (message "touchdown--at-root-level-p:  %s" status))
      status)))

;; Line and location data retrieval.

(defun touchdown--where-am-i ()
  "Return the current location as a section list, or nil for none.

Return the reverse list of nested sections currently containing point,
or nil if the point is not within any section."
  (save-excursion
    (let ((current-line (line-number-at-pos (point)))
          (sections ()))
      (goto-char (point-min))
      (while (and (< (line-number-at-pos (point)) current-line) (not (eobp)))
        (when touchdown--debug
          (message "touchdown--where-am-i: section list: %s" sections)
          (message "touchdown--where-am-i: line %s, current-line %s" (line-number-at-pos (point)) current-line))
        (when (touchdown--section-opening-line-p)
          (progn
            (when touchdown--debug
                (message "touchdown--where-am-i: found opening %s" (match-string-no-properties 2)))
            (push (match-string-no-properties 2) sections)))
        (when (touchdown--parameter-type-line-p)
          (progn
            (when touchdown--debug
                (message "touchdown--where-am-i: found @type %s" (match-string-no-properties 2)))
            (push (match-string-no-properties 2) sections)))
        (when (touchdown--section-closing-line-p)
          (progn
            (when touchdown--debug
                (message "touchdown--where-am-i: found closing %s" (match-string-no-properties 2)))
            (cond ((equal (match-string-no-properties 2) (car sections))
                   (when touchdown--debug
                     (message "touchdown--where-am-i: no type, popping %s" (car sections)))
                   (setq sections (cdr sections)))
                  ((equal (match-string-no-properties 2) (cadr sections))
                   (when touchdown--debug
                     (message
                      "touchdown--where-am-i: type %s, popping %s"
                      (car sections)
                      (cadr sections)))
                   (setq sections (cddr sections))))))
        (forward-line 1))
      (when touchdown--debug
        (message "touchdown--where-am-i: section list: %s" sections))
      sections)))

(cl-defstruct
    (touchdown--line-description
     (:constructor touchdown--line-description-create)
     (:copier nil))
  type
  name
  tag
  value
  comment)

(defun touchdown--what-am-i ()
  "Return a description for the current line.

Return a description for the current line, including the type of line
\(comment, blank, section, include, parameter\), the name \(for
sections and parameters\), the tag or label for sections, the value
\(open or close for sections, parameter value for parameters\), and
either the whole-line commment or inline-comment as comment."
  (save-excursion
    (let ((line-description nil))
      (cond ((touchdown--section-opening-line-p)
             (setq line-description
                   (touchdown--line-description-create
                    :type "section"
                    :name (match-string-no-properties 2)
                    :tag (match-string-no-properties 3)
                    :value "open"
                    :comment (match-string-no-properties 5))))
            ((touchdown--include-line-p)
             (setq line-description
                   (touchdown--line-description-create
                    :type "include"
                    :name (match-string-no-properties 1)
                    :tag nil
                    :value (match-string-no-properties 2)
                    :comment (match-string-no-properties 3))))
            ((touchdown--parameter-line-p)
             (setq line-description
                   (touchdown--line-description-create
                    :type "parameter"
                    :name (match-string-no-properties 1)
                    :tag nil
                    :value (match-string-no-properties 2)
                    :comment (match-string-no-properties 3))))
            ((touchdown--blank-line-p)
             (setq line-description
                   (touchdown--line-description-create
                    :type "blank"
                    :name nil
                    :tag nil
                    :value nil
                    :comment nil)))
            ((touchdown--comment-line-p)
             (setq line-description
                   (touchdown--line-description-create
                    :type "comment"
                    :name nil
                    :tag nil
                    :value nil
                    :comment (match-string-no-properties 1))))
            ((touchdown--section-closing-line-p)
             (setq line-description
                   (touchdown--line-description-create
                    :type "section"
                    :name (match-string-no-properties 2)
                    :tag nil
                    :value "close"
                    :comment (match-string-no-properties 4)))))
      (when touchdown--debug
        (message "touchdown--what-am-i:  %s" line-description))
      line-description)))

(defun touchdown--what-type-am-i ()
  "Return the type for the current section, or nil for none.

Return the type for the section currently containing point, or nil if
the point is not within any section or if the section does have a
type."
  (save-excursion
    (let ((type-regexp (touchdown--create-parameter-regexp "@type"))
          (type nil)
          (stop nil)
          (sections (touchdown--where-am-i)))
      (when touchdown--debug
        (message
         "touchdown--what-type-am-i:  sections %s; type %s" sections type))
      ;; Check if last section is a plugin.
      (let ((sects (reverse sections))
            (tree touchdown--syntax-tree))
        (while sects
          (setq tree (touchdown--section-subsection tree (car sects))
                sects (cdr sects)))
        (when (equal (touchdown--section-type tree) "config")
          (setq type (car sections)
                sections (cdr sections))))

      ;; (cond ((equal sections nil)
      ;;        (setq type nil))
      ;;       (t
      (unless (or type (not sections))
        (when touchdown--debug
          (message "touchdown--what-type-am-i:  section %s" (car sections)))
        ;; (re-search-backward (touchdown--create-section-opening-regexp (car sections)))
        (or
         (re-search-backward (touchdown--create-section-opening-regexp (car sections)) (point-min) t)
         (re-search-backward (touchdown--create-parameter-type-regexp (car sections)) (point-min) t))
        (beginning-of-line)
        (let ((other nil))
          (while (and
                  (not (eobp))
                  (not stop))
            (forward-line 1)
            (let ((desc (touchdown--what-am-i)))
              (cond
	       ;; No line description.
	       ((equal desc nil))
               ;; @type line, in same section.
               ((and
                 (not other)
                 (equal
                  (touchdown--line-description-type desc)
                  "parameter")
                 (equal
                  (touchdown--line-description-name desc)
                  "@type"))
                (setq type (touchdown--line-description-value desc)
                      stop t))
               ;; Section opening or closing of same type.
               ((and
                 (not other)
                 (equal
                  (touchdown--line-description-type desc)
                  "section")
                 (equal
                  (touchdown--line-description-name desc)
                  (car sections)))
                (setq stop t))
               ;; Section opening of different type.
               ((and
                 (not other)
                 (equal
                  (touchdown--line-description-type desc)
                  "section")
                 (not
                  (equal
                   (touchdown--line-description-name desc)
                   (car sections)))
                 (equal
                  (touchdown--line-description-value desc)
                  "open"))
                (setq other t))
               ;; Section closing of different type.
               ((and
                 other
                 (equal
                  (touchdown--line-description-type desc)
                  "section")
                 (not
                  (equal
                   (touchdown--line-description-name desc)
                   (car sections)))
                 (equal
                  (touchdown--line-description-value desc)
                  "close"))
                (setq other nil)))))))
      (when touchdown--debug
        (message "touchdown--what-type-am-i:  type %s" type))
      type)))

(defun touchdown--section-closing-name ()
  "Return the name of the current section closing."
  (save-excursion
    (if (touchdown--section-closing-line-p)
        (let ()
          (move-beginning-of-line 1)
          (looking-at touchdown--section-closing-regexp)
          (match-string-no-properties 2))
      nil)))

(defun touchdown--section-opening-name ()
  "Return the name of the current section opening."
  (save-excursion
    (if (touchdown--section-opening-line-p)
        (let ()
          (move-beginning-of-line 1)
          (looking-at touchdown--section-opening-regexp)
          (match-string-no-properties 2))
      nil)))

(defun touchdown--section-opening-tag ()
  "Return the tag or label of the current section opening."
  (save-excursion
    (if (touchdown--section-opening-line-p)
        (let ()
          (move-beginning-of-line 1)
          (looking-at touchdown--section-opening-regexp)
          (match-string-no-properties 3))
      nil)))

;; Indentation.

(defcustom touchdown-indent-level 2
  "Indent level."
  :type 'integer)

(defun touchdown--opening-section-indentation ()
  "Return the indentation of the current section opening."
  (save-excursion
    (let ((opening-section touchdown--section-opening-regexp)
          (curpoint (point)))
      (cond ((touchdown--section-closing-line-p)
             (let* ((section (touchdown--section-closing-name))
                    (opening-section
                     (touchdown--create-section-regexp section)))
               (if (not (re-search-backward opening-section nil t))
                   (error "Opening section %s not found" section)
                 (current-indentation))))
            (t
             (let (finish)
               (while (and (not finish)
                           (re-search-backward opening-section nil t))
                 (let ((section (match-string-no-properties 2)))
                   (unless (touchdown--section-closed-p section curpoint)
                     (setq finish t))))
               (if (not finish)
                   0
                 (+ (current-indentation) touchdown-indent-level))))))))

(defun touchdown-indent-line ()
  "Indent current line of a fluentd/td-agent configuration."
  (interactive)
  (let ((indent-size (touchdown--opening-section-indentation)))
    (back-to-indentation)
    (when (/= (current-indentation) indent-size)
      (save-excursion
        (delete-region (line-beginning-position) (point))
        (indent-to indent-size)))
    (when (< (current-column) (current-indentation))
      (back-to-indentation))))

(defun touchdown-indent-buffer ()
  "Indent a touchdown buffer.

Indent the current buffer using the fluentd/td-agent syntax."
  (interactive)
  (save-excursion
    (indent-region (buffer-end -1) (buffer-end 1))))

;;; Completion.

;; Completion functions are currently using `touchdown--directives' as
;; the only source of terms for completion.  The list should be built
;; from lists of directives, subdirectives, parameter names, parameter
;; values (especially booleans or sets), tags, and labels in a context
;; dependent way so that completion suggests completions from the
;; appropriate set of possibilities.

;;; Deprecated manual API completion.

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
  (when touchdown--debug
    (message "touchdown--try-completion: string %s" str))
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
    (when touchdown--debug
      (message "touchdown--try-completion: string %s returning %s" str matches-data))
    matches-data))

(defun touchdown--all-completions (str predicate try)
  "Return all possible completions for STR in the fluentd syntax.

Return (ignoring PREDICATE and TRY) a list of all possible matches for
STR in the fluentd syntax."
  (when touchdown--debug
    (message "touchdown--all-completions: string %s" str))
  (let ((directives touchdown--directives)
        (matches nil))
    (while directives
      (let ((directive (car directives)))
        (cond ((string-match-p (regexp-quote str) directive)
               (setq directives (cdr directives)
                     matches (push directive matches)))
              (t
               (setq directives (cdr directives))))))
    (when touchdown--debug
      (message "touchdown--all-completions: string %s returning %s" str matches))
    matches))

(defun touchdown--test-completion (str predicate lmb)
  "Determine if STR is part of a term in the fluentd syntax.

Returns (ignoring PREDICATE and LMB) non-nil if STR has a match the
fluentd configuration syntax, nil otherwise."
  (when touchdown--debug
    (message "touchdown--test-completion: string %s" str))
  (let ((directives touchdown--directives)
        (match-p nil))
    (while directives
      (let ((directive (car directives)))
        (if (string-match-p (regexp-quote str) directive)
            (setq match-p t
                  directives ())
          (setq directives (cdr directives)))))
    (when touchdown--debug
      (message "touchdown--test-completion: string %s returning %s" str match-p))
    match-p))

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
    (when touchdown--debug
      (message "touchdown--capc: str %s pred %s try %s" str predicate try))
    (cond ((eq try nil)
	   (when touchdown--debug
             (message "touchdown--capc: getting try-completion data"))
           (setq result (touchdown--try-completion str predicate try)))
          ((eq try t)
	   (when touchdown--debug
             (message "touchdown--capf: getting all-completions data"))
           (setq result (touchdown--all-completions str predicate try)))
          ((eq try 'lambda)
	   (when touchdown--debug
             (message "touchdown--capf: getting test-completion data"))
           (setq result (touchdown--test-completion str predicate try)))
          ((eq try 'metadata)
	   (when touchdown--debug
             (message "touchdown--capf: getting metadata"))
           (setq result nil))
          ((eq (car try) 'boundaries)
	   (when touchdown--debug
             (message "touchdown--capf: getting boundaries"))
           (setq result nil))
          (t
	   (when touchdown--debug
             (message "touchdown--capf: got unexpected:  %s" try))
           (setq result nil)))
    result))

;;; Current Emacs mediated (by `completion-table-dynamic`) completion.

(defun touchdown--section-subsection (section sub)
  "Return subsection SUB from SECTION."
  (let ((subs (touchdown--section-sections section))
        (found nil)
        (subsection nil))
    (unless (equal subs nil)
      (while (and subs (not found))
        (cond ((equal sub (touchdown--section-name (car subs)))
               (setq found t
                     subsection (car subs)))
              (t
               (setq subs (cdr subs))))))
    subsection))

(defun touchdown--section-parameters-completions (section)
  "Return the parameters of SECTION as a list of completions."
  (let ((options nil)
        (params (touchdown--parameters-names (touchdown--section-parameters section))))
    (while params
      (push (car params) options)
      (setq params (cdr params)))
    (when touchdown--debug
      (message "touchdown--section-parameters-completions: %s" options))
    options))

(defun touchdown--section-subsection-completions (section)
  "Return the subsections of SECTION as a list of completions."
  (let ((options nil)
        (subs (touchdown--section-sections section)))
    (while subs
      (cond ((equal "contain" (touchdown--section-type (car subs)))
             (push (format "<%s>" (touchdown--section-name (car subs))) options))
             ;; (push (format "</%s>" (touchdown--section-name (car subs))) options))
            (t
             (push (format "@type %s" (touchdown--section-name (car subs))) options)))
      (setq subs (cdr subs)))
    (when touchdown--debug
      (message "touchdown--section-subsection-completions: %s" options))
    options))

(defun touchdown--section-plugin-completions (section type)
  "Return the plugin TYPE of SECTION as a list of completions.

Return the plugin parameters and sections of plugin TYPE as a list of
completions."
  (let ((options nil)
        (subsection (touchdown--section-subsection section type)))
    (unless (equal subsection nil)
      (unless (equal type nil)
        (setq options
              (append
               (touchdown--section-parameters-completions subsection)
               (touchdown--section-subsection-completions subsection)))))
    (when touchdown--debug
      (message "touchdown--section-plugin-completions: %s" options))
    options))

(defun touchdown--section-completions (section type)
  "Return valid completion options from SECTION according to TYPE.

If TYPE is nil, return an option list constructed from the section's
parameters and its subsections.  If TYPE is non-nil, return an option
list constructed from the section's parameters and the plugin matching
the TYPE."
  (let ((options nil))
    (cond ((equal type nil)
           (setq options
                 (append
                  (touchdown--section-parameters-completions section)
                  (touchdown--section-subsection-completions section)
                  (let ((name (touchdown--section-name section)))
                    (unless (equal name "root")
                      (list (format "</%s>" name)))))))
          (t
           (setq options
                 (append
                  (touchdown--section-parameters-completions section)
                  (touchdown--section-plugin-completions section type)))))
    (when touchdown--debug
      (message "touchdown--section-completions: %s" options))
    options))

(defun touchdown--parameter-completions (section name)
  "Return valid completion options from SECTION parameter NAME.

Return completion options from parameter NAME in SECTION.  This will
be a list containing `true` and `false` for boolean parameters, the
default value if one is defined, a list of options for enum parameters
without a default, or nil for everything else."
  (let ((param
          (touchdown--parameter-get section name))
        (options nil))
    (cond ((equal (touchdown--parameter-type param) 'boolean)
           (setq options (list "true" "false")))
          ((touchdown--parameter-default param)
           (setq options (touchdown--parameter-default param)))
          ((touchdown--parameter-options param)
           (setq options (touchdown--parameter-options param)))
          (t
           (setq options nil)))
    options))

(defun touchdown--produce-options ()
  "Return current valid options."
  (save-excursion)
  (when touchdown--debug
    (message "touchdown--produce-options: start"))
  (let ((locations (nreverse (touchdown--where-am-i)))
        (options nil)
        (section touchdown--syntax-tree))
    (cond ((equal locations nil)
           (when touchdown--debug
             (message "root level"))
           (setq options (touchdown--section-completions section nil))
           (when touchdown--debug
             (message "touchdown--produce-options: completions %s" options)))
          (t
           (when touchdown--debug
             (message "touchdown--produce-options: level %s" locations))
           (let ((type (touchdown--what-type-am-i)))
	     (when touchdown--debug
               (message "touchdown--produce-options: type %s, locations %s" type locations))
             (when (equal type (car (reverse locations)))
               (setq locations (reverse (cdr (reverse locations)))))
             (while locations
               (when touchdown--debug
                 (message "touchdown--produce-options: locations %s" locations)
                 (message "touchdown--produce-options: walking to %s" (car locations))
                 (message "touchdown--produce-options: type %s" type))
               (setq section (touchdown--section-subsection section (car locations))
                     locations (cdr locations)))
             (when touchdown--debug
               (message "touchdown--produce-options: completing section %s, type %s" (touchdown--section-name section) type))
             ;; (setq options (touchdown--section-completions section type))
             (let* ((start (progn (skip-syntax-backward "w_.(") (point)))
                    (end (progn (skip-syntax-forward "w_.(") (point)))
                    (str (buffer-substring start end)))
               (if (touchdown--parameter-get section str)
                   (setq options (touchdown--parameter-completions section str))
                 (setq options (touchdown--section-completions section type))))
	     )))
    (when touchdown--debug
      (message "touchdown--produce-options: options %s" options))
    options))

(defun touchdown--dynamic-completion-table (str)
  "Return all possible completions for STR.

Return a list of all possible matches for STR in the fluentd syntax considering the current location of the point."
  (when touchdown--debug
    (message "touchdown--dynamic-completion-table: start"))
  (let ((options (touchdown--produce-options))
        (matches nil))
    (when touchdown--debug
      (message "touchdown--dynamic-completion-table: options %s" options))
    (while options
      (let ((option (car options)))
        (cond ((string-match-p (regexp-quote str) option)
               (setq options (cdr options)
                     matches (push option matches)))
              (t
               (setq options (cdr options))))))
    (when touchdown--debug
      (message "touchdown--dynamic-completion-table: matches %s" matches))
    (nreverse matches)))

(defun touchdown--completion-at-point ()
  "Touchdown mode completion at point function."
  (list (save-excursion
          (skip-syntax-backward "w_.(")
          (point))
        (point)
        ;; Implement try, all, test, metadata, and boundaries.
        ;; #'touchdown--completion-at-point-collection
        ;; :predicate #'touchdown--matches-syntax-p))

        ;; Let emacs implement it.
        (completion-table-dynamic
         #'touchdown--dynamic-completion-table)))

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
    (when (touchdown--parameter-boolean-line-p)
      (beginning-of-line)
      (cond ((equal "true" (match-string-no-properties 2))
             (re-search-forward "\\btrue\\b" nil t 1)
             (replace-match "false" nil nil))
            ((equal "false" (match-string-no-properties 2))
             (re-search-forward "\\bfalse\\b" nil t 1)
             (replace-match "true" nil nil))))))

;;; Mode definition and autoloading.

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
  (define-key touchdown-mode-map "\C-c\C-tt" 'touchdown-swap-boolean)
  (define-key touchdown-mode-map "\C-c\C-tv" 'touchdown-verify-configuration))

;;;###autoload
(add-to-list
 'auto-mode-alist
 '("\\(fluentd?\\.conf\\|td-agent\\.conf\\)\\'" . touchdown-mode))

(provide 'touchdown)

;;; touchdown.el ends here
