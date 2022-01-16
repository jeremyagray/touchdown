;;; tail.el --- td-agent/fluentd input plugin tail syntax data -*- lexical-binding: t; -*-

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

;; td-agent/fluentd input plugin tail syntax data.

;;; Code:

;;; Tail input plugin.

;; Parameters.
(defconst touchdown--input-plugin-tail-parameters
  (list
   (touchdown--parameter-create
    :name "tag"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "path"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "path_timezone"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "exclude_path"
    :type 'array
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "follow_inodes"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "refresh_interval"
    :type 'time
    :default 60
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "limit_recently_modified"
    :type "time"
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "skip_refresh_on_startup"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "read_from_head"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "encoding"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "from_encoding"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "read_lines_limit"
    :type 'integer
    :default 1000
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "read_bytes_limit_per_second"
    :type 'size
    :default -1
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "max_line_size"
    :type 'size
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "multiline_flush_interval"
    :type 'time
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "pos_file"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "pos_file_compaction_interval"
    :type 'time
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "path_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "rotate_wait"
    :type 'time
    :default 5
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_watch_timer"
    :type 'boolean
    :default t
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_stat_watcher"
    :type 'boolean
    :default t
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "open_on_every_update"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "emit_unmatched_lines"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ignore_repeated_permission_error"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "@log_level"
    :type 'string
    :default nil
    :options '("fatal" "error" "warn" "info" "debug" "trace")
    :required nil))
  "Fluentd tail input plugin parameters.")

;; Load parser plugins.
(load "./plugins/parse/json")
(load "./plugins/parse/nginx")
(load "./plugins/parse/regexp")
(load "./plugins/parse/syslog")

;; Parse subsection.
(defconst touchdown--input-plugin-tail-parse-parameters
  (list
   (touchdown--parameter-create
    :name "@include"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "@type"
    :type 'string
    :default nil
    :options nil
    :required t))
  "Touchdown file input plugin tail parse section parameters.")

(defvar touchdown--input-plugin-tail-parse
  (touchdown--section-create
   :name "parse"
   :type "contain"
   :parameters touchdown--input-plugin-tail-parse-parameters
   :sections (list touchdown--parse-plugin-json
		   touchdown--parse-plugin-nginx
		   touchdown--parse-plugin-regexp
		   touchdown--parse-plugin-syslog))
  "Touchdown file input plugin tail parse section.")

;; Section.
(defvar touchdown--input-plugin-tail
  (touchdown--section-create
   :name "tail"
   :type "config"
   :parameters touchdown--input-plugin-tail-parameters
   :sections (list touchdown--input-plugin-tail-parse))
  "Touchdown tail input plugin section.")

;;; tail.el ends here
