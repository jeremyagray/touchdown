;;; utilities-tests.el --- Utility function tests -*- lexical-binding: t; -*-

;; Copyright (C) 2021 by Jeremy A GRAY.

;; Author: Jeremy A GRAY <gray@flyquackswim.com>
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

;; Utility function tests for touchdown.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

(setq config (read-config-file "tests/fluentd.conf"))

(describe
 "touchdown-swap-boolean"

 (it
  "should swap `true` to `false`"
  (with-touchdown-temp-buffer
   config

   (forward-cursor-on "# touchdown-swap-boolean test")
   (forward-cursor-on "true")
   (touchdown-swap-boolean)
   (let* ((bounds (bounds-of-thing-at-point 'word))
	  (actual (buffer-substring-no-properties (car bounds) (cdr bounds))))
     (expect
      "false"
      :to-equal
      actual))))

 (it
  "should swap `false` to `true`"
  (with-touchdown-temp-buffer
   config

   (forward-cursor-on "# touchdown-swap-boolean test")
   (forward-cursor-on "false")
   (touchdown-swap-boolean)
   (let* ((bounds (bounds-of-thing-at-point 'word))
	  (actual (buffer-substring-no-properties (car bounds) (cdr bounds))))
     (expect
      "true"
      :to-equal
      actual))))

(describe
 "touchdown--parameter-line-p"

 (it
  "should return nil if not a parameter line"
  (with-touchdown-temp-buffer
   config

   (forward-cursor-on "# touchdown--parameter-line-p test")
   (forward-cursor-on "<source>")
   (expect
      (touchdown--parameter-line-p)
      :to-be
      nil)))

 (it
  "should return t if line is a parameter line"
  (with-touchdown-temp-buffer
   config

   (forward-cursor-on "# touchdown--parameter-line-p test")
   (forward-cursor-on "@type")
   (expect
      (touchdown--parameter-line-p)
      :to-be
      t))))

(describe
 "touchdown--boolean-parameter-line-p"

 (it
  "should return nil if not a boolean parameter line"
  (with-touchdown-temp-buffer
   config

   (forward-cursor-on "# touchdown--parameter-line-p test")
   (forward-cursor-on "<source>")
   (expect
      (touchdown--boolean-parameter-line-p)
      :to-be
      nil)))

 (it
  "should return t if line is a boolean parameter line"
  (with-touchdown-temp-buffer
   config

   (forward-cursor-on "# touchdown--parameter-line-p test")
   (forward-cursor-on "read_from_head")
   (expect
      (touchdown--boolean-parameter-line-p)
      :to-be
      t)))))

;;; utilities-tests.el ends here
