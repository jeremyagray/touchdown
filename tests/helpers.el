;;; helpers.el --- touchdown-mode test helpers -*- lexical-binding: t; -*-

;; Copyright (C) 2016 by Syohei YOSHIDA.
;; Copyright (C) 2021 by Jeremy A GRAY.

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Author: Jeremy A GRAY <gray@flyquackswim.com>

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

;;; Code:

(require 'ert)

(defmacro with-touchdown-temp-buffer (string &rest body)
  "Create a temporary buffer in `touchdown-mode'.
Create a temporary buffer filled with STRING, move cursor to the
beginning of the buffer, start `touchdown-mode', fontify buffer, and
finally execute any optional statements in BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert ,string)
     (goto-char (point-min))
     (touchdown-mode)
     (font-lock-fontify-buffer)
     ,@body))

(defun read-config-file (file)
  "Return contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun forward-cursor-on (pattern &optional count)
  "Move the cursor forward to the next occurence of PATTERN.
Optionally, move forward COUNT occurences of PATTERN.  Place cursor at
the beginning of PATTERN."
  (let ((case-fold-search nil))
    (re-search-forward pattern nil nil (or count 1)))
  (goto-char (match-beginning 0)))

(defun backward-cursor-on (pattern &optional count)
  "Move the cursor backward to the next occurence of PATTERN.
Optionally, move backward COUNT occurences of PATTERN.  Place cursor
at the beginning of PATTERN."
  (let ((case-fold-search nil))
    (re-search-backward pattern nil nil (or count 1)))
  (goto-char (match-beginning 0)))

(defun face-at-cursor-p (face)
  "Determine if the face at the point is FACE."
  (eq (face-at-point) face))

(provide 'helpers)

;;; helpers.el ends here
