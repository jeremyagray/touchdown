;;; parse.el --- Fluentd parse input plugin helper syntax data -*- lexical-binding: t; -*-

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

;; Fluentd parse input plugin helper syntax data.

;;; Code:

;;; Parse input plugin helper.

;; Load parser plugins.
(load-file (expand-file-name "plugins/parse/json.el" touchdown--directory))
(load-file (expand-file-name "plugins/parse/nginx.el" touchdown--directory))
(load-file (expand-file-name "plugins/parse/regexp.el" touchdown--directory))
(load-file (expand-file-name "plugins/parse/syslog.el" touchdown--directory))

;; Parameters.

(defconst touchdown--input-plugin-parse-parameters
  (list
   (touchdown--parameter-create
    :name "@include"
    :type 'string
    :default nil
    :options nil
    :required nil))
  "Touchdown file input plugin syslog parse section parameters.")

;; Section.

(defvar touchdown--input-plugin-parse
  (touchdown--section-create
   :name "parse"
   :type "contain"
   :parameters touchdown--input-plugin-parse-parameters
   :sections (list touchdown--parse-plugin-json
                   touchdown--parse-plugin-nginx
                   touchdown--parse-plugin-regexp
                   touchdown--parse-plugin-syslog))
  "Touchdown file input plugin syslog parse section.")

;;; parse.el ends here
