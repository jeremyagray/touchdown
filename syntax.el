;;; syntax.el --- Fluentd configuration file syntax data -*- lexical-binding: t; -*-

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

;; td-agent/fluentd configuration file syntax data.

;;; Code:

(require 'cl-lib)

;;; Syntax data structures.

;; Section data structure.

(cl-defstruct
    (touchdown--section (:constructor touchdown--section-create)
                        (:copier nil))
  name
  type
  parameters
  sections)

;; Parameter data structure.

(cl-defstruct
    (touchdown--parameter (:constructor touchdown--parameter-create)
                          (:copier nil))
  name
  type
  default
  options
  required)

;;; Functions.

;; Parameter structure functions.

(defun touchdown--parameters-names (parameters)
  "Return a list of parameter names from a list of parameter structures.

Return a list of parameter names corresponding to the PARAMETERS list
of `touchdown--parameter' structures."
  (let ((params parameters)
        (names ()))
    (while params
      (push (touchdown--parameter-name (car params)) names)
      (setq params (cdr params)))
    names))

(defun touchdown--parameter-get (section name)
  "Return a parameter named NAME from SECTION."
  (let ((param nil)
	(stop nil)
	(params (touchdown--section-parameters section)))
    (while (and params (not stop))
      (when (equal (touchdown--parameter-name (car params)) name)
	(setq param (car params)
	      stop t))
      (setq params (cdr params)))
    param))

;;; Parameter lists.

;;; System parameters and sections.

(load-file (expand-file-name "sections/system.el" touchdown--directory))
(load-file (expand-file-name "plugins/filter/record.el" touchdown--directory))
(load-file (expand-file-name "plugins/input/forward.el" touchdown--directory))
(load-file (expand-file-name "plugins/input/syslog.el" touchdown--directory))
(load-file (expand-file-name "plugins/input/tail.el" touchdown--directory))
(load-file (expand-file-name "plugins/output/file.el" touchdown--directory))
(load-file (expand-file-name "plugins/output/mongo.el" touchdown--directory))

;;; Syntax tree.

(defvar touchdown--section-source
  (touchdown--section-create
   :name "source"
   :type "contain"
   :parameters (list (touchdown--parameter-create
                      :name "@include"
                      :type 'string
                      :default nil
                      :options nil
                      :required nil)
                     (touchdown--parameter-create
                      :name "@id"
                      :type 'string
                      :default nil
                      :options nil
                      :required nil)
                     (touchdown--parameter-create
                      :name "@label"
                      :type 'string
                      :default nil
                      :options nil
                      :required nil))
   :sections (list touchdown--input-plugin-forward
                   touchdown--input-plugin-syslog
                   touchdown--input-plugin-tail))
  "Touchdown source section syntax.")

(defvar touchdown--section-filter
  (touchdown--section-create
   :name "filter"
   :type "contain"
   :parameters (list (touchdown--parameter-create
                      :name "@include"
                      :type 'string
                      :default nil
                      :options nil
                      :required nil))
   :sections (list touchdown--filter-plugin-record))
  "Touchdown filter section syntax.")

(defvar touchdown--section-match
  (touchdown--section-create
   :name "match"
   :type "contain"
   :parameters (list (touchdown--parameter-create
                      :name "@include"
                      :type 'string
                      :default nil
                      :options nil
                      :required nil)
                     (touchdown--parameter-create
                      :name "@id"
                      :type 'string
                      :default nil
                      :options nil
                      :required nil)
                     (touchdown--parameter-create
                      :name "@label"
                      :type 'string
                      :default nil
                      :options nil
                      :required nil))
   :sections (list touchdown--output-plugin-file
		   touchdown--output-plugin-mongo))
  "Touchdown match section syntax.")

(defvar touchdown--section-label
  (touchdown--section-create
   :name "label"
   :type "contain"
   :parameters (list (touchdown--parameter-create
                      :name "@include"
                      :type 'string
                      :default nil
                      :options nil
                      :required nil))
   :sections (list touchdown--section-match
		   touchdown--section-filter))
  "Touchdown label section syntax.")

(defvar touchdown--syntax-tree
  (touchdown--section-create
   :name "root"
   :type "contain"
   :parameters (list (touchdown--parameter-create
                      :name "@include"
                      :type 'string
                      :default nil
                      :options nil
                      :required nil))
   :sections (list touchdown--section-source
                   touchdown--section-filter
                   touchdown--section-match
                   touchdown--section-label
                   touchdown--section-system))
  "The touchdown syntax tree.")

;;; syntax.el ends here
