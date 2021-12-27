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

(describe
 "touchdown-mode syntax completion"

 (describe
  "touchdown-mode `try-completion'"

  (it
   "should match `@i` exactly"
   (let ((expected (touchdown--try-completion "@i" nil nil))
	 (actual t))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `@inc` exactly"
   (let ((expected (touchdown--try-completion "@inc" nil nil))
	 (actual t))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `@include` exactly"
   (let ((expected (touchdown--try-completion "@include" nil nil))
	 (actual t))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `<so` exactly"
   (let ((expected (touchdown--try-completion "<so" nil nil))
	 (actual t))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `<sy` exactly"
   (let ((expected (touchdown--try-completion "<sy" nil nil))
	 (actual t))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `<s` with longest match"
   (let ((expected (touchdown--try-completion "<s" nil nil))
	 (actual "<s"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `</so` exactly"
   (let ((expected (touchdown--try-completion "</so" nil nil))
	 (actual t))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `</sy` exactly"
   (let ((expected (touchdown--try-completion "</sy" nil nil))
	 (actual t))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should not match `@includes`"
   (let ((expected (touchdown--try-completion "@includes" nil nil))
	 (actual nil))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should not match `@file`"
   (let ((expected (touchdown--try-completion "@file" nil nil))
	 (actual nil))
     (expect
      expected
      :to-equal
      actual)))

  )

 (describe
  "touchdown-mode `test-completion'"

  (it
   "should return t or nil appropriately"
   (let ((data (list
		'("@i". t)
		'("@inc". t)
		'("@include". t)
		'("@includes". nil)
		'("@file". nil)
		'("<so". t)
		'("<sy". t)
		'("<s". t)
		'("</so". t)
		'("</sy". t)
		'("</s". t)
		)))
     (while data
       (let ((datum (car data)))
	 (let ((expected (touchdown--test-completion (car datum) nil nil))
	       (actual (cdr datum)))
	   (expect
	    expected
	    :to-equal
	    actual)
	   (setq data (cdr data)))))))
  )

 (describe
  "touchdown-mode `all-completions'"

  (it
   "should find `@include` for `@i`"
   (let ((expected (touchdown--all-completions "@i" nil nil))
	 (actual '("@include")))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should find `(\"<system>\" \"<source>\")` for `<s`"
   (let ((expected (touchdown--all-completions "<s" nil nil))
	 (actual '("<system>" "<source>")))
     ;; (print (format "expected:  %s, actual:  %s" expected actual))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should find `(\"</system>\" \"</source>\")` for `<s`"
   (let ((expected (touchdown--all-completions "</s" nil nil))
	 (actual '("</system>" "</source>")))
     ;; (print (format "expected:  %s, actual:  %s" expected actual))
     (expect
      expected
      :to-equal
      actual)))
  )
 )

;;; completion-tests.el ends here
