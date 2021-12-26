;;; completion-tests.el --- completion tests -*- lexical-binding: t; -*-

;; Copyright (C) 2021 by Jeremy A GRAY.

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

(require 'helpers "tests/helpers.el")
(require 'touchdown-mode "touchdown.el")

(setq config (read-config-file "tests/fluentd.conf"))

;; (touchdown--try-completion "@i" nil nil)
;; (touchdown--try-completion "@inc" nil nil)
;; (touchdown--try-completion "@include" nil nil)
;; (touchdown--try-completion "@includes" nil nil)
;; (touchdown--try-completion "<so" nil nil)
;; (touchdown--try-completion "<sy" nil nil)
;; (touchdown--try-completion "<s" nil nil)

(describe
 "touchdown-mode syntax completion"

 (it
  "should `try-completion' match `@i` exactly"
  (let ((expected (touchdown--try-completion "@i" nil nil))
	(actual t))
    (expect
     expected
     :to-equal
     actual)))

 (it
  "should `try-completion' match `@inc` exactly"
  (let ((expected (touchdown--try-completion "@inc" nil nil))
	(actual t))
    (expect
     expected
     :to-equal
     actual)))

 (it
  "should `try-completion' match `@include` exactly"
  (let ((expected (touchdown--try-completion "@include" nil nil))
	(actual t))
    (expect
     expected
     :to-equal
     actual)))

 (it
  "should `try-completion' match `<so` exactly"
  (let ((expected (touchdown--try-completion "<so" nil nil))
	(actual t))
    (expect
     expected
     :to-equal
     actual)))

 (it
  "should `try-completion' match `<sy` exactly"
  (let ((expected (touchdown--try-completion "<sy" nil nil))
	(actual t))
    (expect
     expected
     :to-equal
     actual)))

 (it
  "should `try-completion' match `<s` with longest match"
  (let ((expected (touchdown--try-completion "<s" nil nil))
	(actual "<s"))
    (expect
     expected
     :to-equal
     actual)))

 (it
  "should `try-completion' match `<sy` exactly"
  (let ((expected (touchdown--try-completion "<sy" nil nil))
	(actual t))
    (expect
     expected
     :to-equal
     actual)))

 (it
  "should not `try-completion' match `@includes`"
  (let ((expected (touchdown--try-completion "@includes" nil nil))
	(actual nil))
    (expect
     expected
     :to-equal
     actual)))

 (it
  "should not `try-completion' match `@file`"
  (let ((expected (touchdown--try-completion "@file" nil nil))
	(actual nil))
    (expect
     expected
     :to-equal
     actual)))

 )

;;; completion-tests.el ends here
