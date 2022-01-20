;;; parameter-structure-tests.el --- Parameter structure tests -*- lexical-binding: t; -*-

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

;; Parameter structure and associated functions tests.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

(describe
 "touchdown-mode parameter structure"

 (describe
  "touchdown-mode parameter structure functions"

  (describe
   "touchdown-mode parameter structure touchdown--parameter-names function"

   (it
    "should return the list of parameter names"
    (let ((section
	   (touchdown--section-subsection touchdown--syntax-tree "system")))
      (expect
       (touchdown--parameters-names (touchdown--section-parameters section))
       :to-equal
       '("disable_shared_socket" "strict_config_value" "dir_permission" "file_permission" "enable_msgpack_time_support" "process_name" "enable_get_dump" "rpc_endpoint" "without_source" "log_event_verbose" "suppress_config_dump" "ignore_same_log_interval" "ignore_repeated_log_interval" "emit_error_log_interval" "suppress_repeated_stacktrace" "@log_level" "root_dir" "workers" "@include"))))

   (it
    "should return nil for an empty parameter list"
    (let ((section (touchdown--section-create
		    :name "file"
		    :type "config"
		    :parameters nil
		    :sections nil)))
      (expect
       (touchdown--parameters-names (touchdown--section-parameters section))
       :to-equal
       nil)))
   )

  (describe
   "touchdown-mode parameter structure touchdown--parameter-get function"

   (it
    "should return the requested parameter"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (param (touchdown--parameter-get section "@include")))
      (expect
       (touchdown--parameter-name param)
       :to-equal
       "@include")
      (expect
       (touchdown--parameter-type param)
       :to-equal
       'string)
      (expect
       (touchdown--parameter-default param)
       :to-equal
       nil)
      (expect
       (touchdown--parameter-options param)
       :to-equal
       nil)
      (expect
       (touchdown--parameter-required param)
       :to-equal
       nil)))

   (it
    "should return nil for a non-existent parameter"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source")))
      (expect
       (touchdown--parameter-get section "@exclude")
       :to-equal
       nil)))
   )
  )
 )

;;; parameter-structure-tests.el ends here
