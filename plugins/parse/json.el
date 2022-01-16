;;; json.el --- Fluentd parse plugin json syntax data -*- lexical-binding: t; -*-

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

;; td-agent/fluentd parse plugin json syntax data.

;;; Code:

;;; Json parse plugin.

;; Parameters.
(defconst touchdown--parse-plugin-json-parameters
  (list
   (touchdown--parameter-create
    :name "json_parser"
    :type 'string
    :default "oj"
    :options '("oj" "yajl" "json")
    :required nil)
   (touchdown--parameter-create
    :name "stream_buffer_size"
    :type 'integer
    :default 8192
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "time_type"
    :type 'string
    :default "float"
    :options '("float" "string")
    :required nil)
   (touchdown--parameter-create
    :name "time_format"
    :type 'string
    :default nil
    :options nil
    :required nil))
  "Fluentd json parse plugin parameters.")

;; Section.
(defvar touchdown--parse-plugin-json
  (touchdown--section-create
   :name "json"
   :type "config"
   :parameters touchdown--parse-plugin-json-parameters
   :sections nil)
  "Touchdown json parse plugin section.")

;;; json.el ends here
