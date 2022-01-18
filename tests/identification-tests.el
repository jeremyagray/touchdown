;;; identification-tests.el --- line identification tests -*- lexical-binding: t; -*-

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
 "touchdown-mode line identification tests"

 (describe
  "touchdown-mode what-type-am-i"

  (it
   "should return nil without `@type` on closed section"
   (with-touchdown-temp-buffer
    "@include file.conf

<source>

</source>
"
    (forward-cursor-on "<source>")
    (forward-line 1)
    (expect
     (touchdown--what-type-am-i)
     :to-equal
     nil)))

  (it
   "should return nil without `@type` on open section"
   (with-touchdown-temp-buffer
    "@include file.conf

<source>

"
    (forward-cursor-on "<source>")
    (forward-line 1)
    (expect
     (touchdown--what-type-am-i)
     :to-equal
     nil)))

  (it
   "should return nil without `@type` on an open section followed by a closed section of the same kind"
   (with-touchdown-temp-buffer
    "@include file.conf

<source>


<source>
  @type syslog
</source>
"
    (forward-cursor-on "<source>")
    (forward-line 1)
    (expect
     (touchdown--what-type-am-i)
     :to-equal
     nil)))

  (it
   "should return nil without `@type` on an open section followed by a closed section of a different kind"
   (with-touchdown-temp-buffer
    "@include file.conf

<source>


  <parse>
    @type syslog
  </parse>
"
    (forward-cursor-on "<source>")
    (forward-line 1)
    (expect
     (touchdown--what-type-am-i)
     :to-equal
     nil)))

  (it
   "should return nil without `@type` on an open section followed by a closed section of a different kind"
   (with-touchdown-temp-buffer
    "@include file.conf

<source>


  <parse>
    @type syslog
    <source>
      @type tail
    </source>
  </parse>
"
    (forward-cursor-on "<source>")
    (forward-line 1)
    (expect
     (touchdown--what-type-am-i)
     :to-equal
     nil)))

  (it
   "should return type with `@type` on closed section"
   (with-touchdown-temp-buffer
    "@include file.conf

<source>
  @type syslog
</source>
"
    (forward-cursor-on "<source>")
    (forward-line 1)
    (expect
     (touchdown--what-type-am-i)
     :to-equal
     "syslog")))))

;;; identification-tests.el ends here
