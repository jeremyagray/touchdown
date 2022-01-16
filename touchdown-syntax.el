;;; touchdown-syntax.el --- td-agent/fluentd configuration file syntax data -*- lexical-binding: t; -*-

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
  (let ((my-parameters parameters)
        (names ()))
    (while my-parameters
      (push (touchdown--parameter-name (car my-parameters)) names)
      (setq my-parameters (cdr my-parameters)))
    names))

;;; Parameter lists.

;;; System parameters and sections.

(load "./touchdown-syntax-system")
(load "./touchdown-syntax-input")
(load "./touchdown-syntax-output")

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
                      :name "@tag"
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
   :sections (list touchdown--plugin-input-forward
                   touchdown--plugin-input-tail))
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
   :sections nil)
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
                      :name "@type"
                      :type 'string
                      :default nil
                      :options nil
                      :required nil)
                     (touchdown--parameter-create
                      :name "@tag"
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
   :sections (list touchdown--plugin-output-file))
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

;;; touchdown-syntax.el ends here
