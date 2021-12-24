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

(require 'helpers "tests/helpers.el")
(require 'touchdown-mode "touchdown.el")

(setq config (read-config-file "tests/fluentd.conf"))

(describe
 "touchdown-mode line identification functions"

 (it
  "should identify opening directive lines"
  (with-touchdown-temp-buffer
   config

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

 (it
  "should find opening directive names"
  (with-touchdown-temp-buffer
   config

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

 (it
  "should find opening directive tags"
  (with-touchdown-temp-buffer
   config

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

 (it
  "should identify closing directive lines"
  (with-touchdown-temp-buffer
   config

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

 (it
  "should return closing directive names"
  (with-touchdown-temp-buffer
   config

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

(describe
 "touchdown-mode line identification functions"

 (it
  "should calculate indentation correctly"
  (with-touchdown-temp-buffer
   (concat "</source>
" config)

   (expect
    (touchdown--opening-directive-indentation)
    :to-throw)))


 (it
  "should indent main directives correctly"
  (with-touchdown-temp-buffer
   config

   (forward-cursor-on "@include")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 0))

   (forward-cursor-on "<source>")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 0))

   (forward-cursor-on "</source>")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 0))

   (forward-cursor-on "<match")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 0))

   (forward-cursor-on "</match>")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 0))))

 (it
  "should indent main directive parameters correctly"
  (with-touchdown-temp-buffer
   config

   (forward-cursor-on "@type")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 1))

   (forward-cursor-on "port")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 1))

   (forward-cursor-on "tag")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 1))

   (forward-cursor-on "<match")
   (forward-cursor-on "@type")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 1))

   (forward-cursor-on "path")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 1))))

 (it
  "should indent nested directives and parameters correctly"
  (with-touchdown-temp-buffer
   config

   (forward-cursor-on "<parse>")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 1))

   (forward-cursor-on "@type")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 2))

   (forward-cursor-on "</parse>")
   (call-interactively 'indent-for-tab-command)
   (= (current-indentation) (* touchdown-indent-level 1)))))

;;; indentation-tests.el ends here
