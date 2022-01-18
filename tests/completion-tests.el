;;; completion-tests.el --- Completion tests -*- lexical-binding: t; -*-

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

;; Completion tests for touchdown.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

(describe
 "touchdown-mode syntax completion"

 (describe
  "touchdown-mode root level completion"

  (it
   "should return the root level completions"
   (with-touchdown-temp-buffer
    "@include root.conf

<source>
</source>
"

   (forward-cursor-on "@include")
   (forward-line 1)
   (message "%s" (touchdown--dynamic-completion-table ""))
   (expect
    (touchdown--dynamic-completion-table "")
    :to-equal
    '("<source>" "<filter>" "<match>" "<label>" "<system>" "@include"))

   (expect
    (touchdown--dynamic-completion-table "@")
    :to-equal
    '("@include"))

   (expect
    (touchdown--dynamic-completion-table "@in")
    :to-equal
    '("@include"))

   (expect
    (touchdown--dynamic-completion-table "@type")
    :to-equal
    nil)

   (expect
    (touchdown--dynamic-completion-table "<s")
    :to-equal
    '("<source>" "<system>"))

   (expect
    (touchdown--dynamic-completion-table "<so")
    :to-equal
    '("<source>"))

   (expect
    (touchdown--dynamic-completion-table "<sy")
    :to-equal
    '("<system>"))

   (expect
    (touchdown--dynamic-completion-table "<f")
    :to-equal
    '("<filter>"))

   (expect
    (touchdown--dynamic-completion-table "<m")
    :to-equal
    '("<match>"))

   (expect
    (touchdown--dynamic-completion-table "<l")
    :to-equal
    '("<label>")))))

 (describe
  "touchdown-mode source completion"

  (it
   "should return the source completions with a source opening"
   (with-touchdown-temp-buffer
    "@include root.conf

<source>

</source>
"

   (forward-cursor-on "<source>")
   (forward-line 1)
   (message "%s" (touchdown--dynamic-completion-table ""))
   (expect
    (touchdown--dynamic-completion-table "")
    :to-equal
    '("</source>" "@type forward" "@type tail" "@label" "@id" "@include")))))

 (describe
  "touchdown-mode `try-completion'"

  (it
   "should match `@i` with the longest match"
   (let ((expected (touchdown--try-completion "@i" nil nil))
	 (actual "@include"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `@inc` with the longest match"
   (let ((expected (touchdown--try-completion "@inc" nil nil))
	 (actual "@include"))
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
   "should match `<so` with the longest match"
   (let ((expected (touchdown--try-completion "<so" nil nil))
	 (actual "<source>"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `<sy` with the longest match"
   (let ((expected (touchdown--try-completion "<sy" nil nil))
	 (actual "<system>"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `<s` with the longest match"
   (let ((expected (touchdown--try-completion "<s" nil nil))
	 (actual "<s"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `</so` with the longest match"
   (let ((expected (touchdown--try-completion "</so" nil nil))
	 (actual "</source>"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `</sy` with the longest match"
   (let ((expected (touchdown--try-completion "</sy" nil nil))
	 (actual "</system>"))
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
      actual))))

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
		'("</s". t))))
     (while data
       (let ((datum (car data)))
	 (let ((expected (touchdown--test-completion (car datum) nil nil))
	       (actual (cdr datum)))
	   (expect
	    expected
	    :to-equal
	    actual)
	   (setq data (cdr data))))))))

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
      actual)))))

;;; completion-tests.el ends here
