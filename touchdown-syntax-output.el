;;; touchdown-syntax-output.el --- td-agent/fluentd output plugin syntax data -*- lexical-binding: t; -*-

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

;; td-agent/fluentd output plugin syntax data.

;;; Code:

;;; File output plugin.

;; Parameters.
(defconst touchdown--plugin-output-file-parameters
  (list
   (touchdown--parameter-create
    :name "@type"
    :type 'string
    :default "file"
    :options '("file")
    :required t)
   (touchdown--parameter-create
    :name "path"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "append"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "add_path_suffix"
    :type 'boolean
    :default t
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "path_suffix"
    :type 'string
    :default ".log"
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "compress"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "recompress"
    :type 'boolean
    :default nil
    :options nil
    :required nil)
   (touchdown--parameter-create
    :name "@log_level"
    :type 'string
    :default nil
    :options '("fatal" "error" "warn" "info" "debug" "trace")
    :required nil)
   (touchdown--parameter-create
    :name "symlink_path"
    :type 'string
    :default nil
    :options nil
    :required nil)
   )
  "List of fluentd file output plugin parameters.

Currently does not include the format, inject, or buffer subdirectives
and their subdirectives and parameters.")

;; Section.
(defvar touchdown--plugin-output-file
  (touchdown--section-create
   :name "file"
   :type "config"
   :parameters touchdown--plugin-output-file-parameters
   :sections nil)
  "Touchdown file output plugin syntax.")

;;; touchdown-syntax-output.el ends here
