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
   "should return nil without `@type` on an open section followed by nested, closed sections of different kinds"
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
     "syslog")))

  (it
   "should return type with `@type` on closed section, after `@type` line"
   (with-touchdown-temp-buffer
    "@include root.conf

<match>
  @type file
  path some_file
</match>
"
    (forward-cursor-on "<match>")
    (forward-line 2)
    (expect
     (touchdown--what-type-am-i)
     :to-equal
     "file"))))

 (describe
  "touchdown-mode what-am-i"

  (it
   "should return nil without `@type` on closed section"
   (with-touchdown-temp-buffer
    "# this is a comment line
@include some/file.conf

<source>
  @type syslog
  tag syslog

  <parse>
    @type syslog
  </parse>
</source>

<label @SYSLOG>
  @id syslog  # another comment
</label>
"
    (forward-cursor-on "#")
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "comment")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       "this is a comment line"))

    (forward-line 1)
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "include")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       "@include")
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       "some/file.conf")
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       nil))

    (forward-line 1)
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "blank")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       nil))

    (forward-line 1)
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "section")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       "source")
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       "open")
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       nil))

    (forward-line 1)
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "parameter")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       "@type")
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       "syslog")
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       nil))

    (forward-line 1)
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "parameter")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       "tag")
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       "syslog")
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       nil))

    (forward-line 2)
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "section")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       "parse")
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       "open")
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       nil))

    (forward-line 1)
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "parameter")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       "@type")
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       "syslog")
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       nil))

    (forward-line 1)
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "section")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       "parse")
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       "close")
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       nil))

    (forward-line 1)
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "section")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       "source")
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       "close")
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       nil))

    (forward-line 2)
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "section")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       "label")
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       "@SYSLOG")
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       "open")
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       nil))

    (forward-line 1)
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "parameter")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       "@id")
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       "syslog")
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       "# another comment"))

    (forward-line 1)
    (let ((desc (touchdown--what-am-i)))
      (expect
       (touchdown--line-description-type desc)
       :to-equal
       "section")
      (expect
       (touchdown--line-description-name desc)
       :to-equal
       "label")
      (expect
       (touchdown--line-description-tag desc)
       :to-equal
       nil)
      (expect
       (touchdown--line-description-value desc)
       :to-equal
       "close")
      (expect
       (touchdown--line-description-comment desc)
       :to-equal
       nil))
      ))))

;;; identification-tests.el ends here
