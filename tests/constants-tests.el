;;; constants-tests.el --- Touchdown constants tests -*- lexical-binding: t; -*-

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

;; Touchdown constants tests.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

;; Set the test and parent directories during load.
(setq test-directory
      (file-name-directory (or load-file-name (buffer-file-name)))
      parent-directory
      (file-name-directory
       (directory-file-name
	(file-name-directory
	 (or load-file-name (buffer-file-name))))))

(describe
 "touchdown-mode touchdown-directory"

 (describe
 "touchdown-mode touchdown-directory should be the parent of the tests directory"

  (it
   "should be the parent directory"
    (expect
     touchdown--directory
     :to-equal
     parent-directory))))

;;; constants-tests.el ends here
