;;; file.el --- Fluentd output file plugin syntax data -*- lexical-binding: t; -*-

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

;; td-agent/fluentd output file plugin syntax data.

;;; Code:

;;; Output file plugin.

;; Parameters.
(defconst touchdown--output-plugin-file-parameters
  (list
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
    :default "info"
    :options '("fatal" "error" "warn" "info" "debug" "trace")
    :required nil)
   (touchdown--parameter-create
    :name "symlink_path"
    :type 'string
    :default nil
    :options nil
    :required nil))
  "Fluentd file output plugin parameters.")

;; Load format plugins.
(load-file (expand-file-name "plugins/format/format.el" touchdown--directory))

;; Load output plugins.
(load-file (expand-file-name "plugins/output/buffer.el" touchdown--directory))
(load-file (expand-file-name "plugins/output/inject.el" touchdown--directory))

;; Section.

(defvar touchdown--output-plugin-file
  (touchdown--section-create
   :name "file"
   :type "config"
   :parameters touchdown--output-plugin-file-parameters
   :sections (list touchdown--format-plugin
		   touchdown--output-plugin-buffer
		   touchdown--output-plugin-inject))
  "Touchdown file output plugin syntax.")

;;; file.el ends here
