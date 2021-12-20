;;; indentation-test.el --- touchdown-mode indentation tests -*- lexical-binding: t; -*-

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
(require 'touchdown-mode "touchdown.el")

(ert-deftest tag-and-values-indent ()
  "one tag and values"
  (with-touchdown-temp-buffer
    "foo
<source>
  type forward
  port 24224
</source>
    bar
"
    (forward-cursor-on "foo")
    (call-interactively 'indent-for-tab-command)
    (= (current-indentation) 0)

    (forward-cursor-on "<source>")
    (call-interactively 'indent-for-tab-command)
    (= (current-indentation) 0)

    (forward-cursor-on "type")
    (call-interactively 'indent-for-tab-command)
    (= (current-indentation) touchdown-indent-level)

    (forward-cursor-on "port")
    (call-interactively 'indent-for-tab-command)
    (= (current-indentation) touchdown-indent-level)

    (forward-cursor-on "</source>")
    (call-interactively 'indent-for-tab-command)
    (= (current-indentation) 0)

    (forward-cursor-on "bar")
    (call-interactively 'indent-for-tab-command)
    (= (current-indentation) 0)))

(ert-deftest nested-tag-indent ()
  "nestead tags and values"
  (with-touchdown-temp-buffer
    "<match tag>
  type forward
  port 24224

  <class>
    name taro
  </class>
</match>
"
    (forward-cursor-on "<match")
    (call-interactively 'indent-for-tab-command)
    (= (current-indentation) 0)

    (forward-cursor-on "<class>")
    (call-interactively 'indent-for-tab-command)
    (= (current-indentation) touchdown-indent-level)

    (forward-cursor-on "name")
    (call-interactively 'indent-for-tab-command)
    (= (current-indentation) (* touchdown-indent-level 2))

    (forward-cursor-on "</class>")
    (call-interactively 'indent-for-tab-command)
    (= (current-indentation) touchdown-indent-level)))

;;; indentation-test.el ends here
