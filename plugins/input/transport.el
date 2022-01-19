;;; transport.el --- Fluentd input plugin transport helper syntax data -*- lexical-binding: t; -*-

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

;; Fluentd input plugin transport helper syntax data.

;;; Code:

;;; Transport input plugin helper.

;; Parameters.
(defconst touchdown--input-plugin-transport-parameters
  (list
   (touchdown--parameter-create
    :name "@include"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "version"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ciphers"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "insecure"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "ca_path"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "cert_path"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "private_key_path"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "private_key_passphrase"
    :type 'string
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "client_cert_auth"
    :type 'boolean
    :default nil
    :options nil
    :required nil))
  "Touchdown file input plugin transport section parameters.")

(defvar touchdown--input-plugin-transport
  (touchdown--section-create
   :name "transport"
   :type "contain"
   :parameters touchdown--input-plugin-transport-parameters
   :sections nil)
  "Touchdown file input plugin transport section.")

;;; transport.el ends here
