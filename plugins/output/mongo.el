;;; mongo.el --- Fluentd MongoDB output plugin syntax data -*- lexical-binding: t; -*-

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

;; Fluentd MongoDB output plugin syntax data.

;;; Code:

;;; MongoDB output plugin.

;; Parameters.

(defconst touchdown--output-plugin-mongo-parameters
  (list
   (touchdown--parameter-create
    :name "auth_mech"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "auth_source"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "capped"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "capped_size"
    :type 'size
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "collection"
    :type 'string
    :default "untagged"
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "connection_string"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "database"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "date_keys"
    :type 'array
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "expire_after"
    :type 'time
    :default 0
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "host"
    :type 'string
    :default "localhost"
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "include_tag_key"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "include_time_key"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "journaled"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "mongo_log_level"
    :type 'string
    :default "info"
    :options '("fatal" "error" "warn" "info" "debug" "trace")
    :required t)
   (touchdown--parameter-create
    :name "parse_string_number_date"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "password"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "port"
    :type 'integer
    :default 27017
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "replace_dollar_in_key_with"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "replace_dot_in_key_with"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ssl"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ssl_ca_cert"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ssl_cert"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ssl_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ssl_key_pass_phrase"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ssl_verify"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "time_key"
    :type 'string
    :default "time"
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "user"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "write_concern"
    :type 'integer
    :default nil
    :options nil
    :required nil)
)
  "Fluentd MongoDB output plugin parameters.")

;; Load output helpers.
(load-file (expand-file-name "plugins/output/buffer.el" touchdown--directory))
(load-file (expand-file-name "plugins/output/inject.el" touchdown--directory))

;; Section.

(defvar touchdown--output-plugin-mongo
  (touchdown--section-create
   :name "mongo"
   :type "config"
   :parameters touchdown--output-plugin-mongo-parameters
   :sections (list touchdown--output-plugin-buffer
		   touchdown--output-plugin-inject))
  "Touchdown MongoDB output plugin syntax.")

;;; mongo.el ends here
