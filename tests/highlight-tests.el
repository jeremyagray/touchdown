;;; highlight-tests.el --- syntax highlighting tests -*- lexical-binding: t; -*-

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

(require 'helpers "tests/helpers.el")
(require 'touchdown-mode "touchdown.el")

(setq config (read-config-file "tests/fluentd.conf"))

(describe
 "touchdown-mode syntax highlighting"

 (it
  "should highlight directives with the directives face"
  (with-touchdown-temp-buffer
   config
   (forward-cursor-on "<source>")
   (expect
    (face-at-cursor-p 'touchdown-directives-face)
    :to-equal
    t)
   (forward-cursor-on "</source>")
   (expect
    (face-at-cursor-p 'touchdown-directives-face)
    :to-equal
    t)
   (forward-cursor-on "<match")
   (expect
    (face-at-cursor-p 'touchdown-directives-face)
    :to-equal
    t)
   (forward-cursor-on "</match>")
   (expect
    (face-at-cursor-p 'touchdown-directives-face)
    :to-equal
    t)))

 (it
  "should highlight subdirectives with the subdirectives face"
  (with-touchdown-temp-buffer
   config
   (forward-cursor-on "<parse>")
   (expect
    (face-at-cursor-p 'touchdown-subdirectives-face)
    :to-equal
    t)
   (forward-cursor-on "</parse>")
   (expect
    (face-at-cursor-p 'touchdown-subdirectives-face)
    :to-equal
    t)))

 (it
  "should highlight `@include` with the touchdown-file-include-face"
  (with-touchdown-temp-buffer
   config

   (forward-cursor-on "@include")
   (let ((expected 'touchdown-file-include-face)
	 (actual (face-at-point)))
   (expect expected :to-equal actual))))

 (it
  "should highlight `@include` paths with touchdown-file-include-path-face"
  (with-touchdown-temp-buffer
   config

   (forward-cursor-on "path/to/the/file")
   (let ((expected 'touchdown-file-include-path-face)
	 (actual (face-at-point)))
     ;; This prints, but includes the description from `it.'
     ;; (print (format "expected face:  %s, actual face:  %s" expected actual))
     (expect expected :to-equal actual))))

 (it
  "should highlight tags/labels with the tag face"
  (with-touchdown-temp-buffer
   config
   (forward-cursor-on "match")
   (expect
    (face-at-cursor-p 'touchdown-directives-face)
    :to-equal
    t)
   (forward-cursor-on "myapp\\.access")
   (expect
    (face-at-cursor-p 'touchdown-tag-face)
    :to-equal
    t)
   (forward-cursor-on ">")
   (expect
    (face-at-cursor-p 'touchdown-directives-face)
    :to-equal
    t)))

 (it
  "should highlight parameter names and values with their faces"
  (with-touchdown-temp-buffer
   config
   (forward-cursor-on "@type")
   (expect
    (face-at-cursor-p 'touchdown-parameter-name-face)
    :to-equal
    t)
   (forward-cursor-on "syslog")
   (expect
    (face-at-cursor-p 'touchdown-parameter-value-face)
    :to-equal
    t)
   (forward-cursor-on "port")
   (expect
    (face-at-cursor-p 'touchdown-parameter-name-face)
    :to-equal
    t)
   (forward-cursor-on "27016")
   (expect
    (face-at-cursor-p 'touchdown-parameter-value-face)
    :to-equal
    t)
   (forward-cursor-on "@type")
   (expect
    (face-at-cursor-p 'touchdown-parameter-name-face)
    :to-equal
    t)
   (forward-cursor-on "syslog")
   (expect
    (face-at-cursor-p 'touchdown-parameter-value-face)
    :to-equal
    t)
   (forward-cursor-on "@type")
   (expect
    (face-at-cursor-p 'touchdown-parameter-name-face)
    :to-equal
    t)
   (forward-cursor-on "file")
   (expect
    (face-at-cursor-p 'touchdown-parameter-value-face)
    :to-equal
    t)
   (forward-cursor-on "path")
   (expect
    (face-at-cursor-p 'touchdown-parameter-name-face)
    :to-equal
    t)
   (forward-cursor-on "/var/log/fluent/myapp/access")
   (expect
    (face-at-cursor-p 'touchdown-parameter-value-face)
    :to-equal
    t)))

 (it
  "should highlight inline comments separated by space"
  (with-touchdown-temp-buffer
   config

   ;; Find the right section.
   (forward-cursor-on "# Leading space comments.")

   (forward-cursor-on "# open directive comment")
   (expect
    (face-at-cursor-p 'font-lock-comment-delimiter-face)
    :to-equal
    t)

   (forward-cursor-on "# directive parameter comment")
   (expect
    (face-at-cursor-p 'font-lock-comment-delimiter-face)
    :to-equal
    t)

   (forward-cursor-on "rsy#log")
   (expect
    (face-at-cursor-p 'touchdown-parameter-value-face)
    :to-equal
    t)

   (forward-cursor-on "#log")
   (expect
    (face-at-cursor-p 'touchdown-parameter-value-face)
    :to-equal
    t)

   ;; Expect font-lock-comment-face since the syntactic highlighter
   ;; picks up the first '#' as the comment delimiter and the
   ;; following text as the comment.
   (forward-cursor-on "# parameter with hash comment")
   (expect
    (face-at-cursor-p 'font-lock-comment-face)
    :to-equal
    t)

   (forward-cursor-on "# open subdirective comment")
   (expect
    (face-at-cursor-p 'font-lock-comment-delimiter-face)
    :to-equal
    t)

   (forward-cursor-on "# subdirective parameter comment")
   (expect
    (face-at-cursor-p 'font-lock-comment-delimiter-face)
    :to-equal
    t)

   (forward-cursor-on "# close subdirective comment")
   (expect
    (face-at-cursor-p 'font-lock-comment-delimiter-face)
    :to-equal
    t)

   (forward-cursor-on "# close directive comment")
   (expect
    (face-at-cursor-p 'font-lock-comment-delimiter-face)
    :to-equal
    t)))

 (it
  "should highlight valid inline comments not separated by space"
  (with-touchdown-temp-buffer
   config

   ;; Find the right section.
   (forward-cursor-on "# No space comments.")

   (forward-cursor-on "# open directive comment")
   (let ((expected 'font-lock-comment-delimiter-face)
	 (actual (face-at-point)))
   (expect expected :to-equal actual))

   (forward-cursor-on "# directive parameter comment")
   (let ((expected 'touchdown-parameter-value-face)
	 (actual (face-at-point)))
   (expect expected :to-equal actual))

   (forward-cursor-on "rsy#log")
   (let ((expected 'touchdown-parameter-value-face)
	 (actual (face-at-point)))
   (expect expected :to-equal actual))

   (forward-cursor-on "#log")
   (let ((expected 'touchdown-parameter-value-face)
	 (actual (face-at-point)))
   (expect expected :to-equal actual))

   (forward-cursor-on "# parameter with hash comment")
   (let ((expected 'touchdown-parameter-value-face)
	 (actual (face-at-point)))
   (expect expected :to-equal actual))

   (forward-cursor-on "# open subdirective comment")
   (let ((expected 'font-lock-comment-delimiter-face)
	 (actual (face-at-point)))
   (expect expected :to-equal actual))

   (forward-cursor-on "# subdirective parameter comment")
   (let ((expected 'touchdown-parameter-value-face)
	 (actual (face-at-point)))
   (expect expected :to-equal actual))

   (forward-cursor-on "# close subdirective comment")
   (let ((expected 'font-lock-comment-delimiter-face)
	 (actual (face-at-point)))
   (expect expected :to-equal actual))

   (forward-cursor-on "# close directive comment")
   (let ((expected 'font-lock-comment-delimiter-face)
	 (actual (face-at-point)))
   (expect expected :to-equal actual)))))

;;; highlight-tests.el ends here
