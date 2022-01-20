;;; file.el --- Fluentd format file plugin syntax data -*- lexical-binding: t; -*-

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

;; td-agent/fluentd format file plugin syntax data.

;;; Code:

;;; Format file plugin.

;; Parameters.
(defconst touchdown--format-plugin-file-parameters
  (list
   (touchdown--parameter-create
    :name "delimiter"
    :type 'string
    :default "\t"
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "output_tag"
    :type 'boolean
    :default t
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "output_time"
    :type 'boolean
    :default t
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "time_type"
    :type 'string
    :default "string"
    :options '("string")
    :required nil)
   (touchdown--parameter-create
    :name "time_format"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "newline"
    :type 'string
    :default "lf"
    :options '("lf" "crlf")
    :required nil))
  "Touchdown file output plugin format file plugin section parameters.")

;; Section.
(defvar touchdown--format-plugin-file
  (touchdown--section-create
   :name "file"
   :type "config"
   :parameters touchdown--format-plugin-file-parameters
   :sections nil)
  "Touchdown format plugin file section.")

;;; file.el ends here
