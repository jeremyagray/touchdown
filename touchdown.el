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

(defcustom touchdown--debug t
  "Control debugging messages from touchdown functions.

Default is nil, which suppresses debugging information.  Non-nil
enables debugging messages."
  :type 'boolean)

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

(defun touchdown--create-section-regexp (directive)
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

(defun touchdown--create-section-opening-regexp (section)
  "Return a regular expression matching SECTION opening.

Matches any SECTION opening.  Match groups are:

1. Opening bracket.
2. Directive.
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
2. Directive.
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
  "^[[:space:]]*\\(</?\\)\\([[:word:]]\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching any section opening or closing.

Matches all parts of a section line, including trailing comments.
Match groups are:

1. Opening bracket.
2. Directive.
3. Tag, if present.
4. Closing bracket.
5. Comment, if present.")

(defconst touchdown--section-opening-regexp
  "^[[:space:]]*\\(<\\)\\([[:word:]]+\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching any section opening.

Matches all parts of a section opening line, including trailing
comments.  Match groups are:

1. Opening bracket.
2. Directive.
3. Tag, if present.
4. Closing bracket.
5. Comment, if present.")

(defconst touchdown--section-closing-regexp
  "^[[:space:]]*\\(</\\)\\([[:word:]]+\\)\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching a section closing.

Matches all parts of a section closing line, including trailing
comments.  Match groups are:

1. Opening bracket.
2. Directive.
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

;; REVIEW
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

;; REVIEW
(defconst touchdown--sub-directive-regexp
  "^[[:space:]]*\\(</?\\)\\(\\(?:buffer\\|parse\\|record\\)\\)\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching a subdirective.
Matches all parts of a subdirective line, including trailing comments.
Match groups are:

1. Opening bracket.
2. Directive.
3. Closing bracket.
4. Comment, if any.")

;; REVIEW
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

;; REVIEW
(defconst touchdown--sub-directive-opening-regexp
  "^[[:space:]]*\\(<\\)\\(\\(?:buffer\\|parse\\|record\\)\\)\\(>\\)[[:space:]]*\\(#.*\\)?$"
  "Regular expression for matching an opening subdirective.
Matches all parts of an opening subdirective line, including trailing
comments.  Match groups are:

1. Opening bracket.
2. Directive.
3. Closing bracket.
4. Comment, if present.")

;; REVIEW
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

;;; Fluentd syntax symbols.

;; Data structures.
(cl-defstruct
    (touchdown--section (:constructor touchdown--section-create)
			(:copier nil))
  name
  type
  parameters
  sections)

(cl-defstruct
    (touchdown--parameter (:constructor touchdown--parameter-create)
			  (:copier nil))
  name
  type
  default
  options
  required)

(defconst touchdown--system-parameters
  (list
   (touchdown--parameter-create
    :name "workers"
    :type 'integer
    :default 1
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "root_dir"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "log_level"
    :type 'string
    :default nil
    :options '("fatal" "error" "warn" "info" "debug" "trace")
    :required nil)
   (touchdown--parameter-create
    :name "suppress_repeated_stacktrace"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "emit_error_log_interval"
    :type 'time
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ignore_repeated_log_interval"
    :type 'time
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ignore_same_log_interval"
    :type 'time
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "suppress_config_dump"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "log_event_verbose"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "without_source"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "rpc_endpoint"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_get_dump"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "process_name"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_msgpack_time_support"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "file_permission"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "dir_permission"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "strict_config_value"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "disable_shared_socket"
    :type 'boolean
    :default nil
    :options nil
    :required nil)))

(defconst touchdown--system-log-parameters
  (list
   (touchdown--parameter-create
    :name "format"
    :type 'string
    :default "text"
    :options '("text" "json")
    :required nil)
   (touchdown--parameter-create
    :name "time_format"
    :type 'string
    :default "%Y-%m-%d %H:%M:%S %z"
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "rotate_age"
    :type 'integer
    :default 5
    :options '("daily" "weekly" "monthly")
    :required nil)
   (touchdown--parameter-create
    :name "rotate_size"
    :type 'size
    :default 1048576
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_input_metrics"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_size_metrics"
    :type 'boolean
    :default nil
    :options nil
    :required nil)))

(defvar touchdown--section-system-log
  (touchdown--section-create
   :name "log"
   :type "contain"
   :parameters touchdown--system-log-parameters
   :sections nil)
  "Touchdown system log section syntax.")

(defvar touchdown--section-system
  (touchdown--section-create
   :name "system"
   :type "contain"
   :parameters touchdown--system-parameters
   :sections (list touchdown--section-system-log))
  "Touchdown system section syntax.")

(defconst touchdown--plugin-input-tail-parameters
  (list
   (touchdown--parameter-create
    :name "@log_level"
    :type 'string
    :default nil
    :options '("fatal" "error" "warn" "info" "debug" "trace")
    :required nil)
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
    :required nil)))

(defvar touchdown--plugin-input-tail
  (touchdown--section-create
   :name "tail"
   :type "config"
   :parameters touchdown--plugin-input-tail-parameters
   :sections nil)
  "Touchdown tail input plugin syntax.")

(defconst touchdown--plugin-input-forward-parameters
  (list
   (touchdown--parameter-create
    :name "@type"
    :type 'string
    :default "forward"
    :options '("forward")
    :required t)
   (touchdown--parameter-create
    :name "bind"
    :type 'string
    :default "0.0.0.0"
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "tag"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "add_tag_prefix"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "linger_timeout"
    :type 'integer
    :default 0
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "resolve_hostname"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "deny_keepalive"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "send_keepalive_packet"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "chunk_size_limit"
    :type 'size
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "chunk_warn_size_limit"
    :type 'size
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "skip_invalid_event"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "source_address_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "source_hostname_key"
    :type 'string
    :default nil
    :options nil
    :required nil))
  "List of fluentd forward input plugin parameters.

Currently does not include protocol or the transport and security
subdirectives and their subdirectives and parameters.")

(defvar touchdown--plugin-input-forward
  (touchdown--section-create
   :name "forward"
   :type "config"
   :parameters touchdown--plugin-input-forward-parameters
   :sections nil)
  "Touchdown forward input plugin syntax.")

(defconst touchdown--plugin-output-file-parameters
  (list
   (touchdown--parameter-create
    :name "@type"
    :type 'string
    :default "file"
    :options '("file")
    :required t)
   (touchdown--parameter-create
    :name "path"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "append"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "add_path_suffix"
    :type 'boolean
    :default t
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "path_suffix"
    :type 'string
    :default ".log"
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "compress"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "recompress"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "@log_level"
    :type 'string
    :default nil
    :options '("fatal" "error" "warn" "info" "debug" "trace")
    :required nil)
   (touchdown--parameter-create
    :name "symlink_path"
    :type 'string
    :default nil
    :options nil
    :required nil)
   )
  "List of fluentd file output plugin parameters.

Currently does not include the format, inject, or buffer subdirectives
and their subdirectives and parameters.")

(defvar touchdown--plugin-output-file
  (touchdown--section-create
   :name "file"
   :type "config"
   :parameters touchdown--plugin-output-file-parameters
   :sections nil)
  "Touchdown file output plugin syntax.")

(defvar touchdown--section-source
  (touchdown--section-create
   :name "source"
   :type "contain"
   :parameters (list (touchdown--parameter-create
		      :name "@include"
		      :type 'string
		      :default nil
		      :options nil
		      :required nil)
		     (touchdown--parameter-create
		      :name "@tag"
		      :type 'string
		      :default nil
		      :options nil
		      :required nil)
		     (touchdown--parameter-create
		      :name "@id"
		      :type 'string
		      :default nil
		      :options nil
		      :required nil)
		     (touchdown--parameter-create
		      :name "@label"
		      :type 'string
		      :default nil
		      :options nil
		      :required nil))
   :sections (list touchdown--plugin-input-forward
		   touchdown--plugin-input-tail))
  "Touchdown source section syntax.")

(defvar touchdown--section-match
  (touchdown--section-create
   :name "match"
   :type "contain"
   :parameters (list (touchdown--parameter-create
		      :name "@include"
		      :type 'string
		      :default nil
		      :options nil
		      :required nil)
		     (touchdown--parameter-create
		      :name "@type"
		      :type 'string
		      :default nil
		      :options nil
		      :required nil)
		     (touchdown--parameter-create
		      :name "@tag"
		      :type 'string
		      :default nil
		      :options nil
		      :required nil)
		     (touchdown--parameter-create
		      :name "@id"
		      :type 'string
		      :default nil
		      :options nil
		      :required nil)
		     (touchdown--parameter-create
		      :name "@label"
		      :type 'string
		      :default nil
		      :options nil
		      :required nil))
   :sections (list touchdown--plugin-output-file))
  "Touchdown match section syntax.")

(defvar touchdown--section-label
  (touchdown--section-create
   :name "label"
   :type "contain"
   :parameters (list (touchdown--parameter-create
		      :name "@include"
		      :type 'string
		      :default nil
		      :options nil
		      :required nil))
   :sections (list touchdown--section-match))
  "Touchdown label section syntax.")

(defvar touchdown--syntax-tree
  (touchdown--section-create
   :name "root"
   :type "contain"
   :parameters (list (touchdown--parameter-create
		      :name "@include"
		      :type 'string
		      :default nil
		      :options nil
		      :required nil))
   :sections (list touchdown--section-source
		   touchdown--section-match
		   touchdown--section-label
		   touchdown--section-system))
  "The touchdown syntax tree.")

(defun touchdown--parameters-names (parameters)
  "Return a list of parameter names from a list of parameter structures.

Return a list of parameter names corresponding to the PARAMETERS list
of `touchdown--parameter' structures."
  (let ((my-parameters parameters)
	(names ()))
    (while my-parameters
      (push (touchdown--parameter-name (car my-parameters)) names)
      (setq my-parameters (cdr my-parameters)))
    names))

;; Faces and font lock.

(defface touchdown-directives
  '((t (:inherit font-lock-function-name-face)))
  "Face of directive.")

(defface touchdown-subdirectives
  '((t (:inherit font-lock-function-name-face)))
  "Face of subdirective.")

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
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at touchdown--parameter-regexp)))

(defun touchdown--parameter-boolean-line-p ()
  "Determine if the current line has a boolean parameter.

Determine if the current line has a boolean parameter by matching the
line against `touchdown--parameter-boolean-regexp', essentially
looking for a parameter value of `true` or `false`."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at touchdown--parameter-boolean-regexp)))

(defun touchdown--directive-closed-p (directive curpoint)
  "Determine if DIRECTIVE is closed before CURPOINT."
  (save-excursion
    (let ((close-directive
	   (touchdown--create-section-closing-regexp directive))
          (curline (line-number-at-pos curpoint)))
      (when (re-search-forward close-directive curpoint t)
        (< (line-number-at-pos) curline)))))

(defun touchdown--within-directive-p (directive)
  "Determine if the point is currently within DIRECTIVE.

Return t if point is on or after the line of the opening tag of
DIRECTIVE and on or before the line of the closing tag of DIRECTIVE,
or nil otherwise."
  (interactive)
  (save-excursion
    (let ((current-line (line-number-at-pos (point)))
	  (open-label (touchdown--create-section-opening-regexp directive))
	  (open-line nil)
	  (close-label (touchdown--create-section-closing-regexp directive))
	  (close-line nil)
	  (status t))
      (if touchdown--debug
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
	     (if touchdown--debug
		 (message "touchdown--within-directive-p:  opening tag %s not found" directive))
	     (setq status nil)))
      (cond ((re-search-forward close-label (point-max) t)
	     (setq close-line (line-number-at-pos (point))))
	    (t
	     (if touchdown--debug
		 (message "touchdown--within-directive-p:  closing tag %s not found" directive))
	     (setq status nil)))
      (when status
	(if (and (>= current-line open-line)
		 (<= current-line close-line))
	    (setq status t)
	  (setq status nil)))
      (if touchdown--debug
	  (message "touchdown--within-directive-p:  final status %s" status))
      status)))

(defun touchdown--within-label-p ()
  "Determine if the point is currently within a label directive.

Return t if point is on or after the line containing '<label>' and on
or before the line containing '</label>', or nil otherwise."
  (interactive)
  (save-excursion
    (touchdown--within-directive-p "label")))

(defun touchdown--at-root-level-p ()
  "Determine if the point is currently at the root level.

Determine if the point is currently at the root level, outside of all
other directives."
  (interactive)
  (save-excursion
    (let ((directives (touchdown--where-am-i))
	  (status nil))
      (cond ((equal directives nil)
	     (setq status t))
	    (t
	     (setq status nil)))
      (if touchdown--debug
	  (message "touchdown--at-root-level-p:  %s" status))
      status)))

;; Line and location data retrieval.

(defun touchdown--where-am-i ()
  "Return the current directive list, or nil for none.

Return the list of nested directives currently containing point, or
nil if the point is not within any directive."
  (interactive)
  (save-excursion
    (let ((current-line (line-number-at-pos (point)))
	  (directives ()))
      (goto-char (point-min))
      (while (and (< (line-number-at-pos (point)) current-line) (not (eobp)))
	(if touchdown--debug
	    (message "line: %s current-line: %s" (line-number-at-pos (point)) current-line))
	(when (touchdown--section-opening-line-p)
	  (progn
	    (if touchdown--debug
		(message "found opening %s" (match-string-no-properties 2)))
	    (push (match-string-no-properties 2) directives)))
	(when (touchdown--section-closing-line-p)
	  (progn
	    (if touchdown--debug
		(message "found closing %s" (match-string-no-properties 2)))
	    (setq directives (cdr directives))))
	(forward-line 1))
      (if touchdown--debug
	  (message "%s" directives))
      directives)))

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
  (interactive)
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
      (if touchdown--debug
	  (message "touchdown--what-am-i:  %s" line-description))
      line-description)))

(defun touchdown--what-type-am-i ()
  "Return the type for the current directive, or nil for none.

Return the type for the directive currently containing point, or nil
if the point is not within any directive or if the directive does have
a type."
  (interactive)
  (save-excursion
    (let ((type-regexp (touchdown--create-parameter-regexp "@type"))
	  (type "")
	  (found nil)
	  (directives (touchdown--where-am-i)))
      (cond ((equal directives nil)
	     (setq type nil))
	    (t
	     (if touchdown--debug
		 (message "touchdown--what-type-am-i:  directive %s" (car directives)))
	     (re-search-backward (touchdown--create-section-opening-regexp (car directives)))
	     (beginning-of-line)
	     (while (and
		     (not (looking-at (touchdown--create-section-closing-regexp (car directives))))
		     (not (eobp))
		     (not found))
	       (cond ((looking-at type-regexp)
		      (setq type (match-string-no-properties 2)
			    found t))
		     (t
		      (forward-line 1))))
	     (if touchdown--debug
		 (message "touchdown--what-type-am-i:  type %s" type))
	     type)))))

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

(defun touchdown--opening-directive-indentation ()
  "Return the indentation of the current opening directive."
  (save-excursion
    (let ((opening-directive touchdown--any-directive-opening-regexp)
          (curpoint (point)))
      (cond ((touchdown--section-closing-line-p)
             (let* ((directive (touchdown--section-closing-name))
                    (opening-directive
		     (touchdown--create-section-regexp directive)))
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
  "Return all possible completions for STR in the fluentd syntax.

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

(defun touchdown--section-subsection (section name)
  "Return subsection NAME from SECTION."
  (let ((subsections (touchdown--section-sections section))
	(found nil)
	(subsection nil))
    (message "section: %s" section)
    (message "subsections: %s" subsections)
    (while (and subsections (not found))
      (message "checking subsection: %s" (car subsections))
      (message "compare %s to subsection %s" name (touchdown--section-name (car subsections)))
      (cond ((equal name (touchdown--section-name (car subsections)))
	     (setq found t
		   subsection (car subsections)))
	    (t
	     (setq subsections (cdr subsections)))))
    subsection))

(defun touchdown--section-completions (section)
  "Return all valid completion options from SECTION."
  (let ((subsections (touchdown--section-sections section))
	(options nil))
    (let ((params (touchdown--section-parameters section)))
      (while params
	(push (touchdown--parameter-name (car params)) options)
	(setq params (cdr params))))
    (while subsections
      (cond ((equal "config" (touchdown--section-type (car subsections)))
	     (push (concat
		    "@type "
		    (touchdown--section-name (car subsections)))
		   options))
	    ((equal "contain" (touchdown--section-type (car subsections)))
	     (push (format
		    "<%s>"
		    (touchdown--section-name (car subsections)))
		   options)
	     (push (format
		    "</%s>"
		    (touchdown--section-name (car subsections)))
		   options)))
      (setq subsections (cdr subsections)))
    options))

(defun touchdown--subsection-completions (section type)
  "Return all valid completion options from SECTION for TYPE."
  (let ((subsection (touchdown--section-subsection section type))
	(options nil))
    (let ((params (touchdown--section-parameters section)))
      (while params
	(push (touchdown--parameter-name (car params)) options)
	(setq params (cdr params))))
    (setq options (concat options (touchdown--section-completions subsection)))
    options))

(defun touchdown--syntax-get-subtree (tree section)
  "Return SECTION from TREE."
  (let ((sections (touchdown--section-sections tree))
	(subtree nil)
	(found nil))
    (while (and sections (not found))
      (cond ((equal section (touchdown--section-name (car sections)))
	     (setq subtree (car sections)
		   found t))
	    (t
	     (setq sections (cdr sections)))))
    subtree))

(defun touchdown--produce-options ()
  "Return current valid options."
  (interactive)
  (let ((locations (nreverse (touchdown--where-am-i t)))
	(options nil)
	(tree touchdown--syntax-tree))
    (cond ((equal locations nil)
	   (message "root level")
	   (message "completions: %s" (touchdown--section-completions tree))
	   (setq options (touchdown--section-completions tree)))
	  (t
	   (while (and locations tree)
	     (let ((subtree (touchdown--syntax-get-subtree tree (car locations))))
	       (cond ((equal subtree nil)
		      (setq locations nil))
		     (t
		      (setq tree subtree)))
	       (setq locations (cdr locations)))
	     (message "tree: %s" tree))
	   (let ((my-type (touchdown--what-type-am-i)))
	     (message "type: %s" my-type)
	     (cond ((not my-type)
		    (setq options (touchdown--section-completions tree)))
		   (t
		    (setq options (touchdown--subsection-completions tree my-type)))))))
    (message "options: %s" options)
    options))

(defun touchdown--produce-terms ()
  "Return current valid terms."
  (let ((location (car (touchdown--where-am-i))))
    (cond ((equal location nil)
	   touchdown--directives)
	  ((equal location "source")
	   (cond ((equal (touchdown--what-type-am-i) nil)
		  (list "@type" "tag" "<parse>" "</parse>"))
		 ((equal (touchdown--what-type-am-i) "tail")
		  (touchdown--parameters-names touchdown--plugin-input-tail-parameters))
		 ((equal (touchdown--what-type-am-i) "forward")
		  (touchdown--parameters-names touchdown--plugin-input-forward-parameters))))
	  ((equal location "match")
	   (cond ((equal (touchdown--what-type-am-i) "file")
		  (touchdown--parameters-names touchdown--plugin-output-file-parameters))))
	  ((equal location "parse")
	   (list "@type"))
	  (t
	   touchdown--directives))))

(defun touchdown--dynamic-completion-table (str)
  "Return all possible completions for STR.

Return a list of all possible matches for STR in the fluentd syntax considering the current location of the point."
  (let ((directives (touchdown--produce-terms))
	(matches nil))
    (message "directives: %s" directives)
    (while directives
      (let ((directive (car directives)))
        (cond ((string-match-p (regexp-quote str) directive)
	       (setq directives (cdr directives)
		     matches (push directive matches)))
	      (t
	       (setq directives (cdr directives))))))
    (message "matches: %s" matches)
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
  (define-key touchdown-mode-map "\C-c\C-tt" 'touchdown-swap-boolean)
  (define-key touchdown-mode-map "\C-c\C-tv" 'touchdown-verify-configuration))

;;;###autoload
(add-to-list
 'auto-mode-alist
 '("\\(fluentd?\\.conf\\|td-agent\\.conf\\)\\'" . touchdown-mode))

(provide 'touchdown)

;;; touchdown.el ends here
