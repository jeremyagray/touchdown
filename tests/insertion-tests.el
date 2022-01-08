;;; insertion-tests.el --- Code block insertion tests -*- lexical-binding: t; -*-

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

;; Code block insertion tests.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

(describe
 "touchdown-mode code block insertions"

 (describe
  "touchdown-mode `touchdown-insert-source'"

  (it
  "should insert the tags only on a blank line"
  (with-touchdown-temp-buffer "# Start

# Stop"

   (goto-char (point-min))
   (forward-cursor-on (regexp-quote "# Start"))
   (forward-line 1)
   (touchdown-insert-source nil)
   (goto-char (point-min))
   (forward-cursor-on (regexp-quote "<source>"))
   (expect
    (looking-at (regexp-quote "<source>"))
    :to-be
    t)
   (forward-cursor-on (regexp-quote "@type"))
   (expect
    (looking-at (regexp-quote "@type"))
    :to-be
    nil)
   (forward-cursor-on (regexp-quote "<parse>"))
   (expect
    (looking-at (regexp-quote "<parse>"))
    :to-be
    nil)
   (forward-cursor-on (regexp-quote "</parse>"))
   (expect
    (looking-at (regexp-quote "</parse>"))
    :to-be
    nil)
   (forward-cursor-on (regexp-quote "</source>"))
   (expect
    (looking-at (regexp-quote "</source>"))
    :to-be
    t)))

  (it
  "should insert the complete template on a blank line"
  (with-touchdown-temp-buffer "# Start

# Stop"

   (goto-char (point-min))
   (forward-cursor-on (regexp-quote "# Start"))
   (forward-line 1)
   (touchdown-insert-source 1)
   (goto-char (point-min))
   (forward-cursor-on (regexp-quote "<source>"))
   (expect
    (looking-at (regexp-quote "<source>"))
    :to-be
    t)
   (forward-cursor-on (regexp-quote "@type"))
   (expect
    (looking-at (regexp-quote "@type"))
    :to-be
    t)
   (forward-cursor-on (regexp-quote "<parse>"))
   (expect
    (looking-at (regexp-quote "<parse>"))
    :to-be
    t)
   (forward-cursor-on (regexp-quote "@type"))
   (expect
    (looking-at (regexp-quote "@type"))
    :to-be
    t)
   (forward-cursor-on (regexp-quote "</parse>"))
   (expect
    (looking-at (regexp-quote "</parse>"))
    :to-be
    t)
   (forward-cursor-on (regexp-quote "</source>"))
   (expect
    (looking-at (regexp-quote "</source>"))
    :to-be
    t)))

  (it
  "should insert the tags after a non-blank line"
  (with-touchdown-temp-buffer "# Start

# Stop"

   (goto-char (point-min))
   (forward-cursor-on (regexp-quote "# Stop"))
   (touchdown-insert-source nil)
   (goto-char (point-min))
   (forward-cursor-on (regexp-quote "# Stop"))
   (forward-cursor-on (regexp-quote "<source>"))
   (expect
    (looking-at (regexp-quote "<source>"))
    :to-be
    t)
   (forward-cursor-on (regexp-quote "@type"))
   (expect
    (looking-at (regexp-quote "@type"))
    :to-be
    nil)
   (forward-cursor-on (regexp-quote "<parse>"))
   (expect
    (looking-at (regexp-quote "<parse>"))
    :to-be
    nil)
   (forward-cursor-on (regexp-quote "</parse>"))
   (expect
    (looking-at (regexp-quote "</parse>"))
    :to-be
    nil)
   (forward-cursor-on (regexp-quote "</source>"))
   (expect
    (looking-at (regexp-quote "</source>"))
    :to-be
    t)))

  (it
  "should insert the complete template after a non-blank line"
  (with-touchdown-temp-buffer "# Start

# Stop"

   (goto-char (point-min))
   (forward-cursor-on (regexp-quote "# Stop"))
   (touchdown-insert-source 1)
   (goto-char (point-min))
   (forward-cursor-on (regexp-quote "# Stop"))
   (forward-cursor-on (regexp-quote "<source>"))
   (expect
    (looking-at (regexp-quote "<source>"))
    :to-be
    t)
   (forward-cursor-on (regexp-quote "@type"))
   (expect
    (looking-at (regexp-quote "@type"))
    :to-be
    t)
   (forward-cursor-on (regexp-quote "<parse>"))
   (expect
    (looking-at (regexp-quote "<parse>"))
    :to-be
    t)
   (forward-cursor-on (regexp-quote "@type"))
   (expect
    (looking-at (regexp-quote "@type"))
    :to-be
    t)
   (forward-cursor-on (regexp-quote "</parse>"))
   (expect
    (looking-at (regexp-quote "</parse>"))
    :to-be
    t)
   (forward-cursor-on (regexp-quote "</source>"))
   (expect
    (looking-at (regexp-quote "</source>"))
    :to-be
    t)))))

;;; insertion-tests.el ends here
