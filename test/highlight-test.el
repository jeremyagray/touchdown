;;; highlight-test.el --- syntax highlighting tests -*- lexical-binding: t; -*-

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

(ert-deftest tag-and-values-highlight ()
  "highlighting one tag and values"
  (with-touchdown-temp-buffer
    "<source>
  type forward
  port 24224
</source>
"
    (forward-cursor-on "<source>")
    (should (face-at-cursor-p 'touchdown-tag))

    (forward-cursor-on "type")
    (should (face-at-cursor-p 'touchdown-parameter-name))
    (forward-cursor-on "forward")
    (should (face-at-cursor-p 'touchdown-parameter-value))

    (forward-cursor-on "port")
    (should (face-at-cursor-p 'touchdown-parameter-name))
    (forward-cursor-on "24224")
    (should (face-at-cursor-p 'touchdown-parameter-value))

    (forward-cursor-on "</source>")
    (should (face-at-cursor-p 'touchdown-tag))))

(ert-deftest tag-parameter-highlight ()
  "highlighting one tag and values"
  (with-touchdown-temp-buffer
    "<match myapp.access>
  type file
  path /var/log/fluent/access
</match>
"
    (forward-cursor-on "<match")
    (should (face-at-cursor-p 'touchdown-tag))

    (forward-cursor-on "myapp\\.access")
    (should (face-at-cursor-p 'touchdown-tag-parameter))))

;;; highlight-test.el ends here
