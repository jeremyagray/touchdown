;;; inject.el --- Fluentd output plugin inject helper syntax data -*- lexical-binding: t; -*-

;; Copyright (C) 2022 by Jeremy A GRAY.

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

;; Fluentd output plugin inject helper syntax data.

;;; Code:

;;; Inject helper.

;; Parameters.

(defconst touchdown--output-plugin-inject-parameters
  (list
   (touchdown--parameter-create
    :name "@include"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "hostname_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "hostname"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "worker_id_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "tag_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "time_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "time_type"
    :type 'string
    :default "float"
    :options '("float" "unixtime" "unixtime_millis" "unixtime_micros" "unixtime_nanos" "string")
    :required nil)
   (touchdown--parameter-create
    :name "time_format"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "localtime"
    :type 'boolean
    :default t
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "utc"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "timezone"
    :type 'string
    :default nil
    :options nil
    :required nil))
    "Touchdown output plugin inject helper section parameters.")

;; Section.

(defvar touchdown--output-plugin-inject
  (touchdown--section-create
   :name "inject"
   :type "contain"
   :parameters touchdown--output-plugin-inject-parameters
   :sections nil)
  "Touchdown output plugin inject helper section.")

;;; inject.el ends here
