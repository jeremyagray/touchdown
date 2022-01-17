;;; filter-record-tests.el --- Filter record syntax data tests -*- lexical-binding: t; -*-

;; Copyright (C) 2021 by Jeremy A GRAY.

;; Author: Jeremy A GRAY <gray@flyquackswim.com>
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

;; Filter record syntax data tests for touchdown.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

(describe
 "touchdown-mode filter record syntax data"

 (describe
  "touchdown-mode filter section data"

  (it
   "should exist"
   (let* ((section
	   (touchdown--section-subsection touchdown--syntax-tree "filter"))
	  (subsection
	   (touchdown--section-subsection section "record_transformer")))
     (expect
      (touchdown--section-name section)
      :to-equal
      "filter")
     (expect
      (touchdown--section-completions section nil)
      :to-equal
      '("@include" "@type record_transformer"))
     (expect
      (touchdown--section-completions section "record_transformer")
      :to-equal
      '("@include" "enable_ruby" "auto_typecast" "renew_record" "renew_time_key" "keep_keys" "remove_keys" "</record>" "<record>"))
     (expect
      (touchdown--section-name subsection)
      :to-equal
      "record_transformer")))))

;;; filter-record-tests.el ends here
