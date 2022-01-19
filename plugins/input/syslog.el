;;; syslog.el --- Fluentd input plugin syslog syntax data -*- lexical-binding: t; -*-

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

;; Fluentd input plugin syslog syntax data.

;;; Code:

;;; Syslog input plugin.

;; Parameters.
(defconst touchdown--input-plugin-syslog-parameters
  (list
   (touchdown--parameter-create
    :name "@log_level"
    :type 'string
    :default "info"
    :options '("fatal" "error" "warn" "info" "debug" "trace")
    :required nil)
   (touchdown--parameter-create
    :name "tag"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "port"
    :type 'integer
    :default 5140
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "bind"
    :type 'string
    :default "0.0.0.0"
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "protocol_type"
    :type 'string
    :default "udp"
    :options '("udp" "tcp")
    :required nil)
   (touchdown--parameter-create
    :name "message_length_limit"
    :type 'size
    :default 2048
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "frame_type"
    :type 'string
    :default "traditional"
    :options '("traditional" "octet_count")
    :required nil)
   (touchdown--parameter-create
    :name "source_host_key"
    :type 'string
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
    :name "source_hostname_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "resolve_hostname"
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
    :name "source_address_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "severity_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "facility_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "blocking_timeout"
    :type 'time
    :default 0.5
    :options nil
    :required nil))
  "Fluentd forward input syslog parameters.")

;; Load input helper plugins.
(load-file (expand-file-name "plugins/input/parse.el" touchdown--directory))
(load-file (expand-file-name "plugins/input/transport.el" touchdown--directory))

;; Section.
(defvar touchdown--input-plugin-syslog
  (touchdown--section-create
   :name "syslog"
   :type "config"
   :parameters touchdown--input-plugin-syslog-parameters
   :sections (list touchdown--input-plugin-transport
		   touchdown--input-plugin-parse))
  "Touchdown syslog input plugin section.")

;;; syslog.el ends here
