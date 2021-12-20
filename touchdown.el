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

(defconst touchdown--closing-tag-regexp
  "^\\s-*\\(</[^>]+>\\)\\$"
  "Regular expression for matching a closing tag.")

(defconst touchdown--tag-regexp
  "^\\s-*\\(</?[^ \t\r\n>]+\\)\\(?:\\s-+\\([^>]+\\)\\)?\\(>\\)")

(defconst touchdown--parameter-regexp
  "\\([[:word:]_]+\\)\\s-+\\(.+\\)$")

(defface touchdown-tag
  '((t (:inherit font-lock-keyword-face)))
  "Face of TAG")

(defface touchdown-tag-parameter
  '((t (:inherit font-lock-type-face)))
  "Face of tag parameter")

(defface touchdown-parameter-name
  '((t (:inherit font-lock-variable-name-face)))
  "Face of parameter name")

(defface touchdown-parameter-value
  '((t (:inherit font-lock-constant-face)))
  "Face of parameter value")

(defvar touchdown-font-lock-keywords
  `((,touchdown--tag-regexp (1 'touchdown-tag)
                          (2 'touchdown-tag-parameter nil t)
                          (3 'touchdown-tag nil t))
    (,touchdown--parameter-regexp (1 'touchdown-parameter-name)
                                (2 'touchdown-parameter-value))))

(defun touchdown--open-tag-line-p ()
  "Determine if point is on an opening tag line."
  (save-excursion
    (back-to-indentation)
    (looking-at-p "<[^/][^ \t\r\n>]*")))

(defun touchdown--closing-tag-line-p ()
  "Determine if point is on a line containing a closing tag."
  (save-excursion
    (back-to-indentation)
    (looking-at-p touchdown--closing-tag-regexp)))

(defun touchdown--retrieve-close-tag-name ()
  "Find the current closing tag name."
  (save-excursion
    (back-to-indentation)
    (looking-at "</\\([^>]+\\)>")
    (match-string-no-properties 1)))

(defun touchdown--already-closed-p (tagname curpoint)
  "Determine if tag TAGNAME is closed after CURPOINT."
  (save-excursion
    (let ((close-tag (format "</%s>" tagname))
          (curline (line-number-at-pos curpoint)))
      (when (search-forward close-tag curpoint t)
        (< (line-number-at-pos) curline)))))

(defun touchdown--search-open-tag-indentation ()
  "Get the indentation of the current opening tag."
  (save-excursion
    (let ((open-tag "<\\([^/][^ \t\r\n>]+\\)\\(?:\\s-+\\([^>]+\\)\\)?\\(>\\)")
          (curpoint (point)))
      (cond ((touchdown--closing-tag-line-p)
             (let* ((tagname (touchdown--retrieve-close-tag-name))
                    (open-tag1 (format "^\\s-*<%s\\(?:\\s-\\|>\\)" tagname)))
               (if (not (re-search-backward open-tag1 nil t))
                   (error "Opening tag not found")
                 (current-indentation))))
            (t
             (let (finish)
               (while (and (not finish) (re-search-backward open-tag nil t))
                 (let ((tagname (match-string-no-properties 1)))
                   (unless (touchdown--already-closed-p tagname curpoint)
                     (setq finish t))))
               (if (not finish)
                   0
                 (+ (current-indentation) touchdown-indent-level))))))))

(defun touchdown--search-close-tag ()
  "Find the current closing tag."
  (let ((close-tag "</\\([^/]+\\)>")
        (cur-line-end (line-end-position)))
    (save-excursion
      (if (re-search-forward open-tag nil t)
          (let ((open-tag (concat "<" (match-string-no-properties 1) 2)))
            (match-string-no-properties 1))
        (let* ((indentation (current-indentation))
               (tagname (match-string-no-properties 1))
               (close-tag (format "</%s>" tagname)))
          (if (re-search-forward close-tag cur-line-end t)
              indentation
            (+ indentation touchdown-indent-level)))))))

(defun touchdown-indent-line ()
  "Indent current line as fluentd configuration."
  (interactive)
  (let ((indent-size (touchdown--search-open-tag-indentation)))
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
