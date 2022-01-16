;;; system.el --- td-agent/fluentd system section syntax data -*- lexical-binding: t; -*-

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

;; td-agent/fluentd system section syntax data.

;;; Code:

;;; System parameters and sections.

;; System parameters.
(defconst touchdown--system-parameters
  (list
   (touchdown--parameter-create
    :name "@include"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "workers"
    :type 'integer
    :default 1
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "root_dir"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "log_level"
    :type 'string
    :default "info"
    :options '("fatal" "error" "warn" "info" "debug" "trace")
    :required nil)
   (touchdown--parameter-create
    :name "suppress_repeated_stacktrace"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "emit_error_log_interval"
    :type 'time
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ignore_repeated_log_interval"
    :type 'time
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ignore_same_log_interval"
    :type 'time
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "suppress_config_dump"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "log_event_verbose"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "without_source"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "rpc_endpoint"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_get_dump"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "process_name"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_msgpack_time_support"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "file_permission"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "dir_permission"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "strict_config_value"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "disable_shared_socket"
    :type 'boolean
    :default nil
    :options nil
    :required nil)))

;; System log section.
(defconst touchdown--system-log-parameters
  (list
   (touchdown--parameter-create
    :name "format"
    :type 'string
    :default "text"
    :options '("text" "json")
    :required nil)
   (touchdown--parameter-create
    :name "time_format"
    :type 'string
    :default "%Y-%m-%d %H:%M:%S %z"
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "rotate_age"
    :type 'integer
    :default 5
    :options '("daily" "weekly" "monthly")
    :required nil)
   (touchdown--parameter-create
    :name "rotate_size"
    :type 'size
    :default 1048576
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_input_metrics"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_size_metrics"
    :type 'boolean
    :default nil
    :options nil
    :required nil)))

;; System subsections.
(defvar touchdown--section-system-log
  (touchdown--section-create
   :name "log"
   :type "contain"
   :parameters touchdown--system-log-parameters
   :sections nil)
  "Touchdown system log section syntax.")

;; System section.
(defvar touchdown--section-system
  (touchdown--section-create
   :name "system"
   :type "contain"
   :parameters touchdown--system-parameters
   :sections (list touchdown--section-system-log))
  "Touchdown system section syntax.")

;;; system.el ends here
