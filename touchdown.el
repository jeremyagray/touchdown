;;; touchdown.el --- Major mode for highlighting and editing td-agent/fluentd configuration files. -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA.
;; Copyright (C) 2021 by Jeremy A GRAY.

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Author: Jeremy A GRAY <gray@flyquackswim.com>
;; URL: https://github.com/syohex/emacs-fluentd-mode
;; URL: https://github.com/jeremyagray/touchdown
;; Version: 0.01
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

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

;; Major mode for highlighting and editing td-agent/fluentd
;; configuration files.

;;; Code:

(require 'cl-lib)

(defgroup touchdown nil
  "Major mode for editing fluentd/td-agent configuration files."
  :group 'languages)

(defcustom touchdown-indent-level 2
  "Indent level."
  :type 'integer)

(defconst touchdown--opening-xml-directive-regexp
  "^[[:space:]]*\\(<\\(source\\|match\\|filter\\|system\\|label\\)\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*$"
  "Regular expression for matching an opening XML directive."
  :type 'string)

(defconst touchdown--opening-xml-directive-name-regexp
  "^[[:space:]]*<\\(source\\|match\\|filter\\|system\\|label\\)\\(?:[[:space:]]+[^>]+\\)?>[[:space:]]*$"
  "Regular expression for matching an opening XML directive name."
  :type 'string)

(defconst touchdown--opening-xml-directive-tag-regexp
  "^[[:space:]]*<\\(?:source\\|match\\|filter\\|system\\|label\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?>[[:space:]]*$"
  "Regular expression for matching an opening XML directive tag/label."
  :type 'string)

(defconst touchdown--closing-xml-directive-regexp
  "^[[:space:]]*\\(</\\(?:source\\|match\\|filter\\|system\\|label\\)>\\)$"
  "Regular expression for matching a closing XML directive."
  :type 'string)

(defconst touchdown--closing-xml-directive-name-regexp
  "^[[:space:]]*</\\(source\\|match\\|filter\\|system\\|label\\)>$"
  "Regular expression for matching a closing XML directive name."
  :type 'string)

(defconst touchdown--directives-regexp
  "^[[:space:]]*\\(</?\\(source\\|match\\|filter\\|system\\|label\\)\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*$"
  "Regular expression for matching a top level directive."
  :type 'string)

(defconst touchdown--file-include-regexp
  "^[[:space:]]*\\(@include\\)[[:space:]]*\\(.*\\)[[:space:]]*$"
  "Regular expression for matching a file include."
  :type 'string)

(defconst touchdown--parameter-regexp
  "^[[:space:]]*\\([@[:word:]_]+\\)[[:space:]]+\\(.+\\)[[:space:]]*$"
  "Regular expression matching fluentd parameters."
  :type 'string)

(defconst touchdown--xml-subdirectives-regexp
  "^[[:space:]]*\\(</?\\(?:buffer\\|parse\\|record\\)>\\)[[:space:]]*$"
  "Regular expression matching fluentd XML subdirectives."
  :type 'string)

(defface touchdown-directives-face
  '((t (:inherit font-lock-function-name-face)))
  "Face of XML directive.")

(defface touchdown-file-include-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for file includes.")

(defface touchdown-file-include-path-face
  '((t (:inherit font-lock-string-face)))
  "Face for file include path.")

(defface touchdown-tag-parameter
  '((t (:inherit font-lock-variable-name-face)))
  "Face of tag parameter")

(defface touchdown-parameter-name
  '((t (:inherit font-lock-keyword-face)))
  "Face of parameter name")

(defface touchdown-parameter-value
  '((t (:inherit font-lock-constant-face)))
  "Face of parameter value")

(defvar touchdown-font-lock-keywords
  `((,touchdown--directives-regexp (1 'touchdown-directives-face)
                          (2 'touchdown-tag-parameter nil t)
                          (3 'touchdown-directives-face nil t))
    (,touchdown--parameter-regexp (1 'touchdown-parameter-name)
                                (2 'touchdown-parameter-value))
    (,touchdown--file-include-regexp (1 'touchdown-file-include-face)
                                     (2 'touchdown-file-include-face))))

(defun touchdown--opening-xml-directive-line-p ()
  "Determine if point is on an opening XML directive line."
  (save-excursion
    (back-to-indentation)
    (looking-at-p touchdown--opening-xml-directive-regexp)))

(defun touchdown--closing-xml-directive-line-p ()
  "Determine if point is on a line containing a closing XML directive."
  (save-excursion
    (back-to-indentation)
    (looking-at-p touchdown--closing-xml-directive-regexp)))

(defun touchdown--get-closing-xml-directive-name ()
  "Get the name of the current closing XML directive."
  (save-excursion
    (if (touchdown--closing-xml-directive-line-p)
        (let ()
          (back-to-indentation)
          (looking-at touchdown--closing-xml-directive-name-regexp)
          (match-string-no-properties 1))
      nil)))

(defun touchdown--already-closed-p (directive curpoint)
  "Determine if XML directive DIRECTIVE is closed before CURPOINT."
  (save-excursion
    (let ((close-directive (format "</%s>" directive))
          (curline (line-number-at-pos curpoint)))
      (when (search-forward close-directive curpoint t)
        (< (line-number-at-pos) curline)))))

(defun touchdown--get-opening-xml-directive-indentation ()
  "Get the indentation of the current opening XML directive."
  (save-excursion
    (let ((opening-directive "<\\([^/][^ \t\r\n>]+\\)\\(?:\\s-+\\([^>]+\\)\\)?\\(>\\)")
          (curpoint (point)))
      (cond ((touchdown--closing-xml-directive-line-p)
             (let* ((directive (touchdown--get-closing-xml-directive-name))
                    (opening-directive1 (format "^\\s-*<%s\\(?:\\s-\\|>\\)" directive)))
               (if (not (re-search-backward opening-directive1 nil t))
                   (error "Opening XML directive not found")
                 (current-indentation))))
            (t
             (let (finish)
               (while (and (not finish)
                           (re-search-backward opening-directive nil t))
                 (let ((directive (match-string-no-properties 1)))
                   (unless (touchdown--already-closed-p directive curpoint)
                     (setq finish t))))
               (if (not finish)
                   0
                 (+ (current-indentation) touchdown-indent-level))))))))

(defun touchdown--search-closing-xml-directive ()
  "Find the current closing XML directive."
  (let ((closing-xml-directive "</\\([^/]+\\)>")
        (cur-line-end (line-end-position)))
    (save-excursion
      (if (re-search-forward opening-directive nil t)
          (let ((opening-directive (concat "<" (match-string-no-properties 1) 2)))
            (match-string-no-properties 1))
        (let* ((indentation (current-indentation))
               (directive (match-string-no-properties 1))
               (closing-xml-directive (format "</%s>" directive)))
          (if (re-search-forward closing-xml-directive cur-line-end t)
              indentation
            (+ indentation touchdown-indent-level)))))))

(defun touchdown-indent-line ()
  "Indent current line as fluentd configuration."
  (interactive)
  (let ((indent-size (touchdown--get-opening-xml-directive-indentation)))
    (back-to-indentation)
    (when (/= (current-indentation) indent-size)
      (save-excursion
        (delete-region (line-beginning-position) (point))
        (indent-to indent-size)))
    (when (< (current-column) (current-indentation))
      (back-to-indentation))))

(defvar touchdown-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?#  "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?<  "(>"  table)
    (modify-syntax-entry ?>  ")<"  table)
    table))

;;;###autoload
(define-derived-mode touchdown-mode fundamental-mode "Touchdown"
  "Major mode for editing fluentd/td-agent configuration files."
  (setq font-lock-defaults '((touchdown-font-lock-keywords)))

  (make-local-variable 'touchdown-indent-level)
  (set (make-local-variable 'indent-line-function) 'touchdown-indent-line)

  (set (make-local-variable 'comment-start) "#"))

;;;###autoload
(add-to-list
 'auto-mode-alist
 '("\\(fluentd?\\.conf\\|td-agent\\.conf\\)\\'" . touchdown-mode))

(provide 'touchdown-mode)

;;; touchdown.el ends here
