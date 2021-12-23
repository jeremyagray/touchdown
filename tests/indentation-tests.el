;;; indentation-tests.el --- syntax indentation tests -*- lexical-binding: t; -*-

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

(require 'touchdown-mode "touchdown.el")

(defun read-config-file (file)
  "Return contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(setq config (read-config-file "tests/fluentd.conf"))

(describe "touchdown-mode syntax indenting"

	  (it "should identify opening directive lines"
	      (with-touchdown-temp-buffer config

	       (forward-cursor-on "<source>")
	       (expect
		(touchdown--opening-directive-line-p)
		:to-equal
		t)

	       (forward-cursor-on "@type")
	       (expect
		(touchdown--opening-directive-line-p)
		:to-equal
		nil)

	       (forward-cursor-on "<parse>")
	       (expect
		(touchdown--opening-directive-line-p)
		:to-equal
		t)

	       (forward-cursor-on "</parse>")
	       (expect
		(touchdown--opening-directive-line-p)
		:to-equal
		nil)

	       (forward-cursor-on "</source>")
	       (expect
		(touchdown--opening-directive-line-p)
		:to-equal
		nil)

	       (forward-cursor-on "<match")
	       (expect
		(touchdown--opening-directive-line-p)
		:to-equal
		t)

	       (forward-cursor-on "</match>")
	       (expect
		(touchdown--opening-directive-line-p)
		:to-equal
		nil)))

	  (it "should find opening directive names"
	      (with-touchdown-temp-buffer config

	       (forward-cursor-on "<source>")
	       (expect
		(touchdown--opening-directive-name)
		:to-equal
		"source")

	       (forward-cursor-on "@type")
	       (expect
		(touchdown--opening-directive-name)
		:to-equal
		nil)

	       (forward-cursor-on "<parse>")
	       (expect
		(touchdown--opening-directive-name)
		:to-equal
		"parse")

	       (forward-cursor-on "</parse>")
	       (expect
		(touchdown--opening-directive-name)
		:to-equal
		nil)

	       (forward-cursor-on "</source>")
	       (expect
		(touchdown--opening-directive-name)
		:to-equal
		nil)

	       (forward-cursor-on "<match")
	       (expect
		(touchdown--opening-directive-name)
		:to-equal
		"match")

	       (forward-cursor-on "</match>")
	       (expect
		(touchdown--opening-directive-name)
		:to-equal
		nil)))

	  (it "should find opening directive tags"
	      (with-touchdown-temp-buffer config

	       (forward-cursor-on "<source>")
	       (expect
		(touchdown--opening-directive-tag)
		:to-equal
		nil)

	       (forward-cursor-on "@type")
	       (expect
		(touchdown--opening-directive-tag)
		:to-equal
		nil)

	       (forward-cursor-on "<parse>")
	       (expect
		(touchdown--opening-directive-tag)
		:to-equal
		nil)

	       (forward-cursor-on "</parse>")
	       (expect
		(touchdown--opening-directive-tag)
		:to-equal
		nil)

	       (forward-cursor-on "</source>")
	       (expect
		(touchdown--opening-directive-tag)
		:to-equal
		nil)

	       (forward-cursor-on "<match")
	       (expect
		(touchdown--opening-directive-tag)
		:to-equal
		"myapp.access")

	       (forward-cursor-on "</match>")
	       (expect
		(touchdown--opening-directive-tag)
		:to-equal
		nil)))

	  (it "should identify closing directive lines"
	      (with-touchdown-temp-buffer config

	       (forward-cursor-on "<source>")
	       (expect
		(touchdown--closing-directive-line-p)
		:to-equal
		nil)

	       (forward-cursor-on "@type")
	       (expect
		(touchdown--closing-directive-line-p)
		:to-equal
		nil)

	       (forward-cursor-on "<parse>")
	       (expect
		(touchdown--closing-directive-line-p)
		:to-equal
		nil)

	       (forward-cursor-on "</parse>")
	       (expect
		(touchdown--closing-directive-line-p)
		:to-equal
		t)

	       (forward-cursor-on "</source>")
	       (expect
		(touchdown--closing-directive-line-p)
		:to-equal
		t)

	       (forward-cursor-on "</match>")
	       (expect
		(touchdown--closing-directive-line-p)
		:to-equal
		t)))

	  (it "should return closing directive names"
	      (with-touchdown-temp-buffer config

	       (forward-cursor-on "</parse>")
	       (expect
		(touchdown--closing-directive-name)
		:to-equal
		"parse")

	       (forward-cursor-on "</source>")
	       (expect
		(touchdown--closing-directive-name)
		:to-equal
		"source")

	       (forward-cursor-on "</match>")
	       (expect
		(touchdown--closing-directive-name)
		:to-equal
		"match"))))

;;; indentation-tests.el ends here
