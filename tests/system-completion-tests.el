;;; system-completion-tests.el --- System completion tests -*- lexical-binding: t; -*-

;; Copyright (C) 2022 by Jeremy A GRAY.

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

;; System completion tests for touchdown.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

(describe
 "touchdown-mode system completion options"

 (describe
  "touchdown-mode system section completion options"

  (it
   "should exist and be named `system`"
   (let* ((section
           (touchdown--section-subsection touchdown--syntax-tree "system")))
     (expect
      (touchdown--section-name section)
      :to-equal
      "system")))

  (it
   "should provide completion options for `system`"
   (let* ((section
           (touchdown--section-subsection touchdown--syntax-tree "system")))
     (expect
      (touchdown--section-completions section nil)
      :to-equal
      '("@include" "workers" "root_dir" "@log_level" "suppress_repeated_stacktrace" "emit_error_log_interval" "ignore_repeated_log_interval" "ignore_same_log_interval" "suppress_config_dump" "log_event_verbose" "without_source" "rpc_endpoint" "enable_get_dump" "process_name" "enable_msgpack_time_support" "file_permission" "dir_permission" "strict_config_value" "disable_shared_socket" "<log>" "</system>"))))

  (it
   "should provide completion options for `log`"
   (let* ((section
           (touchdown--section-subsection touchdown--syntax-tree "system"))
          (log-subsection
           (touchdown--section-subsection section "log")))
     (expect
      (touchdown--section-completions log-subsection nil)
      :to-equal
      '("format" "time_format" "rotate_age" "rotate_size" "enable_input_metrics" "enable_size_metrics" "</log>")))))

 (describe
  "touchdown-mode should provide parameter completion options"

  (it
   "should provide boolean parameter completion options"
   (let* ((section
           (touchdown--section-subsection touchdown--syntax-tree "system"))
	  (param "suppress_repeated_stacktrace"))
     (expect
      (touchdown--parameter-completions section param)
      :to-equal
      '("true" "false"))))

  (it
   "should provide enum list as completion options for enum parameter without default"
   (let* ((section
           (touchdown--section-create
	    :name "system"
	    :type "contain"
	    :parameters (list
			 (touchdown--parameter-create
			  :name "@log_level"
			  :type 'string
			  :default nil
			  :options '("fatal" "error" "warn" "info" "debug" "trace")
			  :required nil))
	    :sections nil))
	  (param "@log_level"))
     (expect
      (touchdown--parameter-completions section param)
      :to-equal
      '("fatal" "error" "warn" "info" "debug" "trace"))))

  (it
   "should provide default as completion option for enum parameter with default"
   (let* ((section
           (touchdown--section-subsection touchdown--syntax-tree "system"))
	  (param "@log_level"))
     (expect
      (touchdown--parameter-completions section param)
      :to-equal
      "info")))

  (it
   "should return nil on parameter without a default"
   (let* ((section
           (touchdown--section-subsection touchdown--syntax-tree "system"))
	  (param "root_dir"))
     (expect
      (touchdown--parameter-completions section param)
      :to-equal
      nil)))))

;;; system-completion-tests.el ends here
