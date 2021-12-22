;;; mode-tests.el --- major mode loading tests -*- lexical-binding: t; -*-

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

;; These tests check that the touchdown mode is loaded with both file
;; variables and upon opening appropriately named files.

;;; Code:


(require 'touchdown-mode "touchdown.el")

(describe "touchdown-mode mode settings"

	  (it "should load touchdown-mode with file header variables"
	      (with-touchdown-temp-buffer
	       "-*- mode: touchdown -*-
<source>
  type forward
  port 24224
</source>
"
	       (set-auto-mode)
	       (expect
		"Touchdown"
		:to-equal
		mode-name)))

	  (it "should load touchdown-mode with file bottom variables"
	      (with-touchdown-temp-buffer
	       "<source>
  type forward
  port 24224
</source>

;; # Local Variables:
;; # mode: touchdown
;; # End:
"
	       (set-auto-mode)
	       (expect
		"Touchdown"
		:to-equal
		mode-name)))

	  (it "should load touchdown-mode for filename fluentd.conf"
	      (with-temp-buffer
		(insert
		 "<source>
  type forward
  port 24224
</source>
")
		(write-file "fluentd.conf")
		(find-file "fluentd.conf")
		(expect
		 "Touchdown"
		 :to-equal
		 mode-name)
		(delete-file "fluentd.conf")
		(delete-file "fluentd.conf~")))

	  (it "should load touchdown-mode for filename fluent.conf"
	      (with-temp-buffer
		(insert
		 "<source>
  type forward
  port 24224
</source>
")
		(write-file "fluent.conf")
		(find-file "fluent.conf")
		(expect
		 "Touchdown"
		 :to-equal
		 mode-name)
		(delete-file "fluent.conf")
		(delete-file "fluent.conf~")))

	  (it "should load touchdown-mode for filename td-agent.conf"
	      (with-temp-buffer
		(insert
		 "<source>
  type forward
  port 24224
</source>
")
		(write-file "td-agent.conf")
		(find-file "td-agent.conf")
		(expect
		 "Touchdown"
		 :to-equal
		 mode-name)
		(delete-file "td-agent.conf")
		(delete-file "td-agent.conf~"))))

;;; mode-tests.el ends here
