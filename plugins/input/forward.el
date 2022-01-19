;;; forward.el --- Fluentd input plugin forward syntax data -*- lexical-binding: t; -*-

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

;; td-agent/fluentd input plugin forward syntax data.

;;; Code:

;;; Forward input plugin.

;; Parameters.
(defconst touchdown--input-plugin-forward-parameters
  (list
   (touchdown--parameter-create
    :name "port"
    :type 'integer
    :default 24244
    :options nil
    :required nil)
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
  "Fluentd forward input plugin parameters.")

;; Security subsection.

(defconst touchdown--input-plugin-forward-security-parameters
  (list
   (touchdown--parameter-create
    :name "@include"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "self_hostname"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "shared_key"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "user_auth"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "allow_anonymous_source"
    :type 'boolean
    :default t
    :options nil
    :required nil))
  "Touchdown file input plugin forward security section parameters.")

;; User subsection.

(defconst touchdown--input-plugin-forward-security-user-parameters
  (list
   (touchdown--parameter-create
    :name "@include"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "username"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "password"
    :type 'string
    :default nil
    :options nil
    :required t))
  "Touchdown file input plugin forward security user section parameters.")

(defvar touchdown--input-plugin-forward-security-user
  (touchdown--section-create
   :name "user"
   :type "contain"
   :parameters touchdown--input-plugin-forward-security-user-parameters
   :sections nil)
  "Touchdown file input plugin forward security user section.")

;; Client subsection.

(defconst touchdown--input-plugin-forward-security-client-parameters
  (list
   (touchdown--parameter-create
    :name "@include"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "host"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "network"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "shared_key"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "users"
    :type 'array
    :default nil
    :options nil
    :required nil))
  "Touchdown file input plugin forward security client section parameters.")

(defvar touchdown--input-plugin-forward-security-client
  (touchdown--section-create
   :name "client"
   :type "contain"
   :parameters touchdown--input-plugin-forward-security-client-parameters
   :sections nil)
  "Touchdown file input plugin forward security client section.")

;; Security section definition.
(defvar touchdown--input-plugin-forward-security
  (touchdown--section-create
   :name "security"
   :type "contain"
   :parameters touchdown--input-plugin-forward-security-parameters
   :sections (list touchdown--input-plugin-forward-security-user
                   touchdown--input-plugin-forward-security-client))
  "Touchdown file input plugin forward security section.")

;; Load parser plugins.
(load-file (expand-file-name "plugins/parse/json.el" touchdown--directory))
(load-file (expand-file-name "plugins/parse/nginx.el" touchdown--directory))
(load-file (expand-file-name "plugins/parse/regexp.el" touchdown--directory))
(load-file (expand-file-name "plugins/parse/syslog.el" touchdown--directory))

;; Parse subsection.
(defconst touchdown--input-plugin-forward-parse-parameters
  (list
   (touchdown--parameter-create
    :name "@include"
    :type 'string
    :default nil
    :options nil
    :required nil))
  "Touchdown file input plugin forward parse section parameters.")

(defvar touchdown--input-plugin-forward-parse
  (touchdown--section-create
   :name "parse"
   :type "contain"
   :parameters touchdown--input-plugin-forward-parse-parameters
   :sections (list touchdown--parse-plugin-json
                   touchdown--parse-plugin-nginx
                   touchdown--parse-plugin-regexp
                   touchdown--parse-plugin-syslog))
  "Touchdown file input plugin forward parse section.")

;; Load input helper plugins.
(load-file (expand-file-name "plugins/input/transport.el" touchdown--directory))

;; Section.
(defvar touchdown--input-plugin-forward
  (touchdown--section-create
   :name "forward"
   :type "config"
   :parameters touchdown--input-plugin-forward-parameters
   :sections (list touchdown--input-plugin-transport
                   touchdown--input-plugin-forward-security))
  "Touchdown forward input plugin section.")

;;; forward.el ends here
