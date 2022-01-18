;;; source-tests.el --- Source syntax data tests -*- lexical-binding: t; -*-

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

;; Source syntax data tests for touchdown.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

(describe
 "touchdown-mode source syntax data"

 (describe
  "touchdown-mode filter section data"

  (it
   "should exist"
   (let* ((section
	   (touchdown--section-subsection touchdown--syntax-tree "source"))
	  (forward-subsection
	   (touchdown--section-subsection section "forward"))
	  (tail-subsection
	   (touchdown--section-subsection section "tail")))

     (expect
      (touchdown--section-name section)
      :to-equal
      "source")

     (expect
      (touchdown--section-completions section nil)
      :to-equal
      '("@include" "@id" "@label" "@type tail" "@type forward" "</source>"))

     (expect
      (touchdown--section-completions section "forward")
      :to-equal
      `("@include" "@id" "@label" "port" "bind" "tag" "add_tag_prefix" "linger_timeout" "resolve_hostname" "deny_keepalive" "send_keepalive_packet" "chunk_size_limit" "chunk_warn_size_limit" "skip_invalid_event" "source_address_key" "source_hostname_key" "@type security" "@type transport"))

     (expect
      (touchdown--section-completions section "tail")
      :to-equal
      '("@include" "@id" "@label" "tag" "path" "path_timezone" "exclude_path" "follow_inodes" "refresh_interval" "limit_recently_modified" "skip_refresh_on_startup" "read_from_head" "encoding" "from_encoding" "read_lines_limit" "read_bytes_limit_per_second" "max_line_size" "multiline_flush_interval" "pos_file" "pos_file_compaction_interval" "path_key" "rotate_wait" "enable_watch_timer" "enable_stat_watcher" "open_on_every_update" "emit_unmatched_lines" "ignore_repeated_permission_error" "@log_level" "<parse>"))

     (expect
      (touchdown--section-name forward-subsection)
      :to-equal
      "forward")

     (expect
      (touchdown--section-name tail-subsection)
      :to-equal
      "tail")))))

;;; source-tests.el ends here
