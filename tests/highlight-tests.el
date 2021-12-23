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
  "should highlight includes with the includes face"
  (with-touchdown-temp-buffer
   config
   (forward-cursor-on "@include")
   (expect
    (face-at-cursor-p 'touchdown-file-include-face)
    :to-equal
    t)
   (forward-cursor-on "path/to/the/file")
   (expect
    (face-at-cursor-p 'touchdown-file-include-path-face)
    :to-equal
    t)))

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
    t))))

;;; highlight-tests.el ends here
