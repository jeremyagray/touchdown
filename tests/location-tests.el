;;; location-tests.el --- Location tests -*- lexical-binding: t; -*-

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

;; Location tests.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

(describe
 "touchdown-mode location tests"

 (describe
  "touchdown-mode `touchdown--within-label-p'"

  (setq label-config "<match mongodb>
  @type mongo
  database \"{{ tdagent_log_database }}\"
  collection mongodb
  host localhost
  port 27017
  user \"{{ tdagent_username }}\"
  password \"{{ tdagent_password }}\"
</match>

<label @FLUENT_LOG>
  <match fluent.**>
    @type mongo
    database \"{{ tdagent_log_database }}\"
    collection fluentd
    host localhost
    port 27017
    user \"{{ tdagent_username }}\"
    password \"{{ tdagent_password }}\"
  </match>
</label>

# Leftovers.
<match **>
  @type file
  path \"{{ tdagent_leftovers }}\"
</match>
")

  (it
  "should correctly identify being within a label"
  (with-touchdown-temp-buffer label-config
   (expect
    (touchdown--within-label-p nil)
    :to-be
    nil)
   (forward-line 1)
   (expect
    (touchdown--within-label-p nil)
    :to-be
    nil)
   (forward-cursor-on (regexp-quote "<label"))
   (message "line: %s" (thing-at-point 'line))
   (expect
    (touchdown--within-label-p nil)
    :to-be
    t)
   (forward-line 1)
   (expect
    (touchdown--within-label-p nil)
    :to-be
    t)
   (forward-cursor-on (regexp-quote "</label"))
   (expect
    (touchdown--within-label-p nil)
    :to-be
    t)
   (forward-line 1)
   (expect
    (touchdown--within-label-p nil)
    :to-be
    nil)
   (forward-cursor-on (regexp-quote "</match"))
   (expect
    (touchdown--within-label-p nil)
    :to-be
    nil)))))

;;; location-tests.el ends here
