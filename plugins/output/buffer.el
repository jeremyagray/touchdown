;;; buffer.el --- Fluentd output plugin buffer helper syntax data -*- lexical-binding: t; -*-

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

;; Fluentd output plugin buffer helper syntax data.

;;; Code:

;;; Buffer helper.

;; Parameters.

(defconst touchdown--output-plugin-buffer-parameters
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
    :default '("file" "memory")
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "timekey"
    :type 'time
    :default 86400
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "timekey_wait"
    :type 'time
    :default 600
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "timekey_use_utc"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "timekey_zone"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "chunk_limit_size"
    :type 'size
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "chunk_limit_records"
    :type 'integer
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "total_limit_size"
    :type 'size
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "queue_limit_length"
    :type 'integer
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "chunk_full_threshold"
    :type 'float
    :default 0.95
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "queued_chunks_limit_size"
    :type 'integer
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "compress"
    :type 'string
    :default "text"
    :options '("text" "gzip")
    :required nil)
   (touchdown--parameter-create
    :name "flush_at_shutdown"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "flush_mode"
    :type 'string
    :default "default"
    :options '("default" "lazy" "interval" "immediate")
    :required nil)
   (touchdown--parameter-create
    :name "flush_interval_time"
    :type 'time
    :default 60
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "flush_thread_count"
    :type 'integer
    :default 1
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "flush_thread_interval"
    :type 'float
    :default 1.0
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "flush_thread_burst_interval"
    :type 'float
    :default 1.0
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "delayed_commit_timeout"
    :type 'time
    :default 60
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "overflow_action"
    :type 'string
    :default "throw_exception"
    :options '("throw_exception" "block" "drop_oldest_chunk")
    :required nil)
   (touchdown--parameter-create
    :name "retry_timeout"
    :type 'time
    :default 259200
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "retry_foreve"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "retry_max_times"
    :type 'integer
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "retry_secondary_threshold"
    :type 'float
    :default 0.8
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "retry_type"
    :type 'string
    :default "exponential_backoff"
    :options '("exponential_backoff" "periodic")
    :required nil)
   (touchdown--parameter-create
    :name "retry_wait"
    :type 'time
    :default 1
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "retry_exponential_backoff_base"
    :type 'float
    :default 2.0
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "retry_max_interval"
    :type 'time
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "retry_randomize"
    :type 'boolean
    :default t
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "disable_chunk_backup"
    :type 'boolean
    :default nil
    :options nil
    :required nil))
  "Touchdown output plugin buffer helper section parameters.")

(defvar touchdown--output-plugin-buffer
  (touchdown--section-create
   :name "buffer"
   :type "contain"
   :parameters touchdown--output-plugin-buffer-parameters
   :sections nil)
  "Touchdown output plugin buffer helper section.")

;;; buffer.el ends here
