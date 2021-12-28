;;; regexp-tests.el --- regular expression tests -*- lexical-binding: t; -*-

;; Copyright (C) 2021 by Jeremy A GRAY.

;; Author: Jeremy A GRAY <gray@flyquackswim.com>

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

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown-mode "touchdown.el")

(describe
 "touchdown-mode regular expressions"

 (describe
  "touchdown-mode `touchdown--create-options-regexp'"

  (let ((options '("fatal" "error" "warn" "info" "debug" "trace")))
    (it
     "should return an option group regular expression, without grouping"
     
     (let ((expected (touchdown--create-options-regexp options nil))
	   (actual "fatal\\|error\\|warn\\|info\\|debug\\|trace"))
       (expect
	expected
	:to-equal
	actual)))

    (it
     "should return an option group regular expression"
     
     (let ((expected (touchdown--create-options-regexp options t))
	   (actual "\\(fatal\\|error\\|warn\\|info\\|debug\\|trace\\)"))
       (expect
	expected
	:to-equal
	actual)))

    (it
     "should return an shy option group regular expression"
     
     (let ((expected (touchdown--create-options-regexp options 'shy))
	   (actual "\\(?:fatal\\|error\\|warn\\|info\\|debug\\|trace\\)"))
       (expect
	expected
	:to-equal
	actual))))
  )

 (describe
  "touchdown-mode `touchdown--create-directive-regexp'"

  (it "should return a regular expression matching directive"
      (let ((data (list
		   '("source" . "^[[:space:]]*\\(</?\\)\\(source\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("match" . "^[[:space:]]*\\(</?\\)\\(match\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("filter" . "^[[:space:]]*\\(</?\\)\\(filter\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("system" . "^[[:space:]]*\\(</?\\)\\(system\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("label" . "^[[:space:]]*\\(</?\\)\\(label\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   )))
	(while data
	  (let ((datum (car data)))
	    (let ((expected (touchdown--create-directive-regexp (car datum)))
		  (actual (cdr datum)))
	      (expect
	       expected
	       :to-equal
	       actual)
	      (setq data (cdr data)))))))
  )

 (describe
  "touchdown-mode `touchdown--create-opening-directive-regexp'"

  (it "should return a regular expression matching an opening directive"
      (let ((data (list
		   '("source" . "^[[:space:]]*\\(<\\)\\(source\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("match" . "^[[:space:]]*\\(<\\)\\(match\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("filter" . "^[[:space:]]*\\(<\\)\\(filter\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("system" . "^[[:space:]]*\\(<\\)\\(system\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("label" . "^[[:space:]]*\\(<\\)\\(label\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   )))
	(while data
	  (let ((datum (car data)))
	    (let ((expected
		   (touchdown--create-opening-directive-regexp (car datum)))
		  (actual (cdr datum)))
	      (expect
	       expected
	       :to-equal
	       actual)
	      (setq data (cdr data)))))))
  )

 (describe
  "touchdown-mode `touchdown--create-closing-directive-regexp'"

  (it "should return a regular expression matching an opening directive"
      (let ((data (list
		   '("source" . "^[[:space:]]*\\(</\\)\\(source\\)\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("match" . "^[[:space:]]*\\(</\\)\\(match\\)\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("filter" . "^[[:space:]]*\\(</\\)\\(filter\\)\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("system" . "^[[:space:]]*\\(</\\)\\(system\\)\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("label" . "^[[:space:]]*\\(</\\)\\(label\\)\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   )))
	(while data
	  (let ((datum (car data)))
	    (let ((expected
		   (touchdown--create-closing-directive-regexp (car datum)))
		  (actual (cdr datum)))
	      (expect
	       expected
	       :to-equal
	       actual)
	      (setq data (cdr data)))))))
  )
 )

;;; regexp-tests.el ends here
