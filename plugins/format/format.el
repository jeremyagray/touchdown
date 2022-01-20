;;; format.el --- Fluentd format plugin helper syntax data -*- lexical-binding: t; -*-

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

;; Fluentd format plugin helper syntax data.

;;; Code:

;;; Format plugin.

;; Load format plugins.
(load-file (expand-file-name "plugins/format/file.el" touchdown--directory))

;; Parameters.

(defconst touchdown--format-plugin-parameters
  (list
   (touchdown--parameter-create
    :name "@include"
    :type 'string
    :default nil
    :options nil
    :required nil))
  "Touchdown format plugin section parameters.")

(defvar touchdown--format-plugin
  (touchdown--section-create
   :name "format"
   :type "contain"
   :parameters touchdown--format-plugin-parameters
   :sections (list touchdown--format-plugin-file))
  "Touchdown format plugin section.")

;;; format.el ends here
