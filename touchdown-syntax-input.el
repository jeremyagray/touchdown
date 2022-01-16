;;; touchdown-syntax-input.el --- td-agent/fluentd input plugin syntax data -*- lexical-binding: t; -*-

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

;; td-agent/fluentd input plugin syntax data.

;;; Code:

;;; Tail input plugin.

;; Parameters.
(defconst touchdown--plugin-input-tail-parameters
  (list
   (touchdown--parameter-create
    :name "@log_level"
    :type 'string
    :default nil
    :options '("fatal" "error" "warn" "info" "debug" "trace")
    :required nil)
   (touchdown--parameter-create
    :name "emit_unmatched_lines"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_stat_watcher"
    :type 'boolean
    :default t
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "enable_watch_timer"
    :type 'boolean
    :default t
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "encoding"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "exclude_path"
    :type 'string
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
    :name "from_encoding"
    :type 'string
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
    :name "limit_recently_modified"
    :type "time"
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "multiline_flush_interval"
    :type "time"
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "open_on_every_update"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "path"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "path_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "path_timezone"
    :type 'string
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
    :name "read_bytes_limit_per_second"
    :type 'size
    :default -1
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "read_from_head"
    :type 'boolean
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
    :name "refresh_interval"
    :type 'time
    :default 60
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "rotate_wait"
    :type 'time
    :default 5
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "skip_refresh_on_startup"
    :type 'boolean
    :default nil
    :options nil
    :required nil)))

;; Section.
(defvar touchdown--plugin-input-tail
  (touchdown--section-create
   :name "tail"
   :type "config"
   :parameters touchdown--plugin-input-tail-parameters
   :sections nil)
  "Touchdown tail input plugin syntax.")

;;; Forward input plugin.

;; Parameters.
(defconst touchdown--plugin-input-forward-parameters
  (list
   (touchdown--parameter-create
    :name "@type"
    :type 'string
    :default "forward"
    :options '("forward")
    :required t)
   (touchdown--parameter-create
    :name "bind"
    :type 'string
    :default "0.0.0.0"
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "tag"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "add_tag_prefix"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "linger_timeout"
    :type 'integer
    :default 0
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "resolve_hostname"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "deny_keepalive"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "send_keepalive_packet"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "chunk_size_limit"
    :type 'size
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "chunk_warn_size_limit"
    :type 'size
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "skip_invalid_event"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "source_address_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "source_hostname_key"
    :type 'string
    :default nil
    :options nil
    :required nil))
  "List of fluentd forward input plugin parameters.

Currently does not include protocol or the transport and security
subdirectives and their subdirectives and parameters.")

;; Section.
(defvar touchdown--plugin-input-forward
  (touchdown--section-create
   :name "forward"
   :type "config"
   :parameters touchdown--plugin-input-forward-parameters
   :sections nil)
  "Touchdown forward input plugin syntax.")

;;; touchdown-syntax-input.el ends here
