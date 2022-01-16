;;; syslog.el --- td-agent/fluentd parse plugin syslog syntax data -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 by Jeremy A GRAY.

;; Author: Jeremy A GRAY <gray@flyquackswim.com>
;; Maintainer: Jeremy A GRAY <gray@flyquackswim.com>
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

;; td-agent/fluentd parse plugin syslog syntax data.

;;; Code:

;;; Syslog parse plugin.

;; Parameters.
(defconst touchdown--parse-plugin-syslog-parameters
  (list
   (touchdown--parameter-create
    :name "time_format"
    :type 'string
    :default "%b %d %H:%M:%S"
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "rfc5424time_format"
    :type 'string
    :default "%Y-%m-%dT%H:%M:%S.%L%z"
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "message_format"
    :type 'string
    :default "rfc3164"
    :options '("rfc3164" "rfc5424" "auto")
    :required nil)
   (touchdown--parameter-create
    :name "with_priority"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "parser_type"
    :type 'string
    :default "regexp"
    :options '("regexp" "string")
    :required nil)
   (touchdown--parameter-create
    :name "support_colonless_ident"
    :type 'boolean
    :default t
    :options nil
    :required nil))
  "Fluentd syslog parse plugin parameters.")

;; Section.
(defvar touchdown--parse-plugin-syslog
  (touchdown--section-create
   :name "syslog"
   :type "config"
   :parameters touchdown--parse-plugin-syslog-parameters
   :sections nil)
  "Touchdown syslog parse plugin section.")

;;; syslog.el ends here
