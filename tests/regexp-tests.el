;;; regexp-tests.el --- Regular expression tests -*- lexical-binding: t; -*-

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

;; Regular expression constant, predicate, and function tests.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

(setq section-strings (list
		       "<source>"
		       "</source>"
		       "<system>"
		       "</system>"
		       "<label>"
		       "</label>"
		       "<match>"
		       "<match myapp.info>"
		       "<match myapp.info>  # comment"
		       "</match>"
		       "<filter>"
		       "</filter>")
      section-opening-strings (list
			       "<source>"
			       "<system>"
			       "<label>"
			       "<match>"
			       "<match myapp.info>"
			       "<match myapp.info>  # comment"
			       "<filter>")
      section-closing-strings (list
			       "</source>"
			       "</system>"
			       "</label>"
			       "</match>"
			       "</filter>"))

(describe
 "touchdown-mode regular expressions"

 (describe
  "touchdown--section-regexp"

  (it
   "should match sections"
   (let ((options section-strings))
     (while options
       (expect
	(string-match-p touchdown--section-regexp (car options))
	:to-equal
	0)
       (setq options (cdr options)))))

  (it
   "should not match non-sections"
   (let ((options (list "@type" "@include" "@tag" "@log_level")))
     (while options
       (expect
	(string-match-p touchdown--section-regexp (car options))
	:to-equal
	nil)
       (setq options (cdr options))))))

 (describe
  "touchdown--section-opening-regexp"

  (it
   "should match opening sections"
   (let ((options section-opening-strings))
     (while options
       (expect
	(string-match-p touchdown--section-opening-regexp (car options))
	:to-equal
	0)
       (setq options (cdr options)))))

  (it
   "should not match closing sections"
   (let ((options section-closing-strings))
     (while options
       (expect
	(string-match-p touchdown--section-opening-regexp (car options))
	:to-equal
	nil)
       (setq options (cdr options))))))

 (describe
  "touchdown--section-closing-regexp"

  (it
   "should match closing sections"
   (let ((options section-closing-strings))
     (while options
       (expect
	(string-match-p touchdown--section-closing-regexp (car options))
	:to-equal
	0)
       (setq options (cdr options)))))

  (it
   "should not match opening sections"
   (let ((options section-opening-strings))
     (while options
       (expect
	(string-match-p touchdown--section-closing-regexp (car options))
	:to-equal
	nil)
       (setq options (cdr options))))))

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
	actual)))))

 (describe
  "touchdown-mode `touchdown--create-section-regexp'"

  (it "should return a regular expression matching a section"
      (let ((data (list
		   '("source" . "^[[:space:]]*\\(</?\\)\\(source\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("match" . "^[[:space:]]*\\(</?\\)\\(match\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("filter" . "^[[:space:]]*\\(</?\\)\\(filter\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("system" . "^[[:space:]]*\\(</?\\)\\(system\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("label" . "^[[:space:]]*\\(</?\\)\\(label\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"))))
	(while data
	  (let ((datum (car data)))
	    (let ((expected (touchdown--create-section-regexp (car datum)))
		  (actual (cdr datum)))
	      (expect
	       expected
	       :to-equal
	       actual)
	      (setq data (cdr data))))))))

 (describe
  "touchdown-mode `touchdown--create-section-opening-regexp'"

  (it "should return a regular expression matching a section opening"
      (let ((data (list
		   '("source" . "^[[:space:]]*\\(<\\)\\(source\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("match" . "^[[:space:]]*\\(<\\)\\(match\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("filter" . "^[[:space:]]*\\(<\\)\\(filter\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("system" . "^[[:space:]]*\\(<\\)\\(system\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("label" . "^[[:space:]]*\\(<\\)\\(label\\)\\(?:[[:space:]]+\\([^>]+\\)\\)?\\(>\\)[[:space:]]*\\(#.*\\)?$"))))
	(while data
	  (let ((datum (car data)))
	    (let ((expected
		   (touchdown--create-section-opening-regexp (car datum)))
		  (actual (cdr datum)))
	      (expect
	       expected
	       :to-equal
	       actual)
	      (setq data (cdr data))))))))

 (describe
  "touchdown-mode `touchdown--create-section-closing-regexp'"

  (it "should return a regular expression matching a section closing"
      (let ((data (list
		   '("source" . "^[[:space:]]*\\(</\\)\\(source\\)\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("match" . "^[[:space:]]*\\(</\\)\\(match\\)\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("filter" . "^[[:space:]]*\\(</\\)\\(filter\\)\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("system" . "^[[:space:]]*\\(</\\)\\(system\\)\\(>\\)[[:space:]]*\\(#.*\\)?$")
		   '("label" . "^[[:space:]]*\\(</\\)\\(label\\)\\(>\\)[[:space:]]*\\(#.*\\)?$"))))
	(while data
	  (let ((datum (car data)))
	    (let ((expected
		   (touchdown--create-section-closing-regexp (car datum)))
		  (actual (cdr datum)))
	      (expect
	       expected
	       :to-equal
	       actual)
	      (setq data (cdr data)))))))))

;;; regexp-tests.el ends here
