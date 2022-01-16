;;; nginx.el --- Fluentd parse plugin nginx syntax data -*- lexical-binding: t; -*-

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

;; td-agent/fluentd parse plugin nginx syntax data.

;;; Code:

;;; Nginx parse plugin.

;; Parameters.
(defconst touchdown--parse-plugin-nginx-parameters
  (list
   (touchdown--parameter-create
    :name "expression"
    :type 'string
    :default nil
    :options nil
    :required t)
   (touchdown--parameter-create
    :name "time_format"
    :type 'string
    :default nil
    :options nil
    :required t))
  "Fluentd nginx parse plugin parameters.")

;; Section.
(defvar touchdown--parse-plugin-nginx
  (touchdown--section-create
   :name "nginx"
   :type "config"
   :parameters touchdown--parse-plugin-nginx-parameters
   :sections nil)
  "Touchdown nginx parse plugin section.")

;;; nginx.el ends here
