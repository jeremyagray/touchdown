;;; record.el --- Fluentd filter record transformer plugin syntax data -*- lexical-binding: t; -*-

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

;; td-agent/fluentd filter record transformer plugin syntax data.

;;; Code:

;;; Filter record transformer plugin.

;; Parameters.
(defconst touchdown--filter-plugin-record-parameters
  (list
   (touchdown--parameter-create
    :name "enable_ruby"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "auto_typecast"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "renew_record"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "renew_time_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "keep_keys"
    :type 'array
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "remove_keys"
    :type 'array
    :default nil
    :options nil
    :required nil))
  "Fluentd record filter plugin parameters.")

;; Record subsection.
(defconst touchdown--filter-plugin-record-record-parameters
  (list
   (touchdown--parameter-create
    :name "@include"
    :type 'string
    :default nil
    :options nil
    :required nil))
  "Touchdown record filter plugin record section parameters.")

(defvar touchdown--filter-plugin-record-record
  (touchdown--section-create
   :name "record"
   :type "contain"
   :parameters touchdown--filter-plugin-record-record-parameters
   :sections nil)
  "Touchdown record filter plugin record section.")

;; Section.
(defvar touchdown--filter-plugin-record
  (touchdown--section-create
   :name "record_transformer"
   :type "config"
   :parameters touchdown--filter-plugin-record-parameters
   :sections (list touchdown--filter-plugin-record-record))
  "Touchdown record filter plugin syntax.")

;;; record.el ends here
