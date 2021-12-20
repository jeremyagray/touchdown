;;; mode.el --- major mode loading tests -*- lexical-binding: t; -*-

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

;; These tests check that the touchdown mode is loaded with both file
;; variables and upon opening appropriately named files.

;;; Code:

(require 'ert)

(ert-deftest should-select-correct-mode-from-top-file-variables ()
  "Check that touchdown mode is selected using file variables."
  (with-touchdown-temp-buffer
    "-*- mode: touchdown -*-
<source>
  type forward
  port 24224
</source>
"
    (set-auto-mode)
    (should
     (equal "Touchdown" mode-name))))

(ert-deftest should-select-correct-mode-from-bottom-file-variables ()
  "Check that touchdown mode is selected using file variables."
  (with-touchdown-temp-buffer
    "<source>
  type forward
  port 24224
</source>

# Local Variables:
# mode: touchdown
# End:
"
    (set-auto-mode)
    (should
     (equal "Touchdown" mode-name))))

(ert-deftest should-select-correct-mode-touchdown-conf ()
  "Check that touchdown mode is selected using auto-mode-alist for file fluentd.conf."
  (with-temp-buffer
    (insert
     "<source>
  type forward
  port 24224
</source>
")
    (write-file "fluentd.conf")
    (find-file "fluentd.conf")
    (should
     (equal "Touchdown" mode-name))
    (delete-file "fluentd.conf")
    (delete-file "fluentd.conf~")))

(ert-deftest should-select-correct-mode-fluent-conf ()
  "Check that touchdown mode is selected using auto-mode-alist for file fluent.conf."
  (with-temp-buffer
    (insert
     "<source>
  type forward
  port 24224
</source>
")
    (write-file "fluent.conf")
    (find-file "fluent.conf")
    (should
     (equal "Touchdown" mode-name))
    (delete-file "fluent.conf")
    (delete-file "fluent.conf~")))

(ert-deftest should-select-correct-mode-td-agent-conf ()
  "Check that touchdown mode is selected using auto-mode-alist for file td-agent.conf."
  (with-temp-buffer
    (insert
     "<source>
  type forward
  port 24224
</source>
")
    (write-file "td-agent.conf")
    (find-file "td-agent.conf")
    (should
     (equal "Touchdown" mode-name))
    (delete-file "td-agent.conf")
    (delete-file "td-agent.conf~")))

;;; mode.el ends here
