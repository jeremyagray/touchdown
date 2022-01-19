;;; input-completion-tests.el --- Input completion tests -*- lexical-binding: t; -*-

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

;; Input completion tests for touchdown.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

(describe
 "touchdown-mode input completion"

 (describe
  "touchdown-mode source section completion"

  (it
   "should exist and be named `source`"
   (let* ((section
	   (touchdown--section-subsection touchdown--syntax-tree "source")))
     (expect
      (touchdown--section-name section)
      :to-equal
      "source")))

  (it
   "should complete a typeless `source`"
   (let* ((section
	   (touchdown--section-subsection touchdown--syntax-tree "source")))
     (expect
      (touchdown--section-completions section nil)
      :to-equal
      '("@include" "@id" "@label" "@type tail" "@type syslog" "@type forward" "</source>"))))

  (it
   "should complete `source` with `@type forward`"
   (let* ((section
	   (touchdown--section-subsection touchdown--syntax-tree "source")))
     (expect
      (touchdown--section-completions section "forward")
      :to-equal
      '("@include" "@id" "@label" "port" "bind" "tag" "add_tag_prefix" "linger_timeout" "resolve_hostname" "deny_keepalive" "send_keepalive_packet" "chunk_size_limit" "chunk_warn_size_limit" "skip_invalid_event" "source_address_key" "source_hostname_key" "<security>" "<transport>"))))

  (it
   "should complete `source` with `@type syslog`"
   (let* ((section
	   (touchdown--section-subsection touchdown--syntax-tree "source")))
     (expect
      (touchdown--section-completions section "syslog")
      :to-equal
      '("@include" "@id" "@label" "@log_level" "tag" "port" "bind" "protocol_type" "message_length_limit" "frame_type" "source_host_key" "emit_unmatched_lines" "source_hostname_key" "resolve_hostname" "send_keepalive_packet" "source_address_key" "severity_key" "facility_key" "blocking_timeout" "<parse>" "<transport>"))))

  (it
   "should complete `source` with `@type tail`"
   (let* ((section
	   (touchdown--section-subsection touchdown--syntax-tree "source")))
     (expect
      (touchdown--section-completions section "tail")
      :to-equal
      '("@include" "@id" "@label" "tag" "path" "path_timezone" "exclude_path" "follow_inodes" "refresh_interval" "limit_recently_modified" "skip_refresh_on_startup" "read_from_head" "encoding" "from_encoding" "read_lines_limit" "read_bytes_limit_per_second" "max_line_size" "multiline_flush_interval" "pos_file" "pos_file_compaction_interval" "path_key" "rotate_wait" "enable_watch_timer" "enable_stat_watcher" "open_on_every_update" "emit_unmatched_lines" "ignore_repeated_permission_error" "@log_level" "<parse>")))))

 (describe
  "touchdown-mode input plugin completion"

  (describe
   "touchdown-mode forward input plugin completion"

   (it
    "should exist and be named `forward`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (forward-subsection
	    (touchdown--section-subsection section "forward")))

      (expect
       (touchdown--section-name forward-subsection)
       :to-equal
       "forward")))

   (it
    "should complete `forward`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (forward-subsection
	    (touchdown--section-subsection section "forward")))

      (expect
       (touchdown--section-completions section "forward")
       :to-equal
       `("@include" "@id" "@label" "port" "bind" "tag" "add_tag_prefix" "linger_timeout" "resolve_hostname" "deny_keepalive" "send_keepalive_packet" "chunk_size_limit" "chunk_warn_size_limit" "skip_invalid_event" "source_address_key" "source_hostname_key" "<security>" "<transport>"))))

   (it
    "should complete `security`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (tail-subsection
	    (touchdown--section-subsection section "forward"))
	   (security-subsection
	    (touchdown--section-subsection tail-subsection "security")))

      (expect
       (touchdown--section-completions security-subsection nil)
       :to-equal
       '("@include" "self_hostname" "shared_key" "user_auth" "allow_anonymous_source" "<client>" "<user>" "</security>"))))

   (it
    "should complete `client`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (forward-subsection
	    (touchdown--section-subsection section "forward"))
	   (security-subsection
	    (touchdown--section-subsection forward-subsection "security"))
	   (client-subsection
	    (touchdown--section-subsection security-subsection "client")))

      (expect
       (touchdown--section-completions client-subsection nil)
       :to-equal
       '("@include" "host" "network" "shared_key" "users" "</client>"))))

   (it
    "should complete `user`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (forward-subsection
	    (touchdown--section-subsection section "forward"))
	   (security-subsection
	    (touchdown--section-subsection forward-subsection "security"))
	   (user-subsection
	    (touchdown--section-subsection security-subsection "user")))

      (expect
       (touchdown--section-completions user-subsection nil)
       :to-equal
       '("@include" "username" "password" "</user>"))))

   (it
    "should complete `transport`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (tail-subsection
	    (touchdown--section-subsection section "forward"))
	   (transport-subsection
	    (touchdown--section-subsection tail-subsection "transport")))

      (expect
       (touchdown--section-completions transport-subsection nil)
       :to-equal
       '("@include" "version" "ciphers" "insecure" "ca_path" "cert_path" "private_key_path" "private_key_passphrase" "client_cert_auth" "</transport>")))))

  (describe
   "touchdown-mode syslog input plugin completion"

   (it
    "should exist and be named `syslog`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (syslog-subsection
	    (touchdown--section-subsection section "syslog")))

      (expect
       (touchdown--section-name syslog-subsection)
       :to-equal
       "syslog")))

   (it
    "should complete `syslog`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (syslog-subsection
	    (touchdown--section-subsection section "syslog")))

      (expect
       (touchdown--section-completions section "syslog")
       :to-equal
      '("@include" "@id" "@label" "@log_level" "tag" "port" "bind" "protocol_type" "message_length_limit" "frame_type" "source_host_key" "emit_unmatched_lines" "source_hostname_key" "resolve_hostname" "send_keepalive_packet" "source_address_key" "severity_key" "facility_key" "blocking_timeout" "<parse>" "<transport>")))))

  (describe
   "touchdown-mode tail input plugin completion"

   (it
    "should exist and be named `tail`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (tail-subsection
	    (touchdown--section-subsection section "tail")))

      (expect
       (touchdown--section-name tail-subsection)
       :to-equal
       "tail")))

   (it
    "should complete `tail`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (tail-subsection
	    (touchdown--section-subsection section "tail")))

      (expect
       (touchdown--section-completions section "tail")
       :to-equal
       '("@include" "@id" "@label" "tag" "path" "path_timezone" "exclude_path" "follow_inodes" "refresh_interval" "limit_recently_modified" "skip_refresh_on_startup" "read_from_head" "encoding" "from_encoding" "read_lines_limit" "read_bytes_limit_per_second" "max_line_size" "multiline_flush_interval" "pos_file" "pos_file_compaction_interval" "path_key" "rotate_wait" "enable_watch_timer" "enable_stat_watcher" "open_on_every_update" "emit_unmatched_lines" "ignore_repeated_permission_error" "@log_level" "<parse>"))))

   (it
    "should complete `parse`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (tail-subsection
	    (touchdown--section-subsection section "tail"))
	   (parse-subsection
	    (touchdown--section-subsection tail-subsection "parse")))

      (expect
       (touchdown--section-completions parse-subsection nil)
       :to-equal
       '("@include" "@type syslog" "@type regexp" "@type nginx" "@type json" "</parse>"))))

   (it
    "should complete `@type syslog`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (tail-subsection
	    (touchdown--section-subsection section "tail"))
	   (parse-subsection
	    (touchdown--section-subsection tail-subsection "parse")))

      (expect
       (touchdown--section-completions parse-subsection "syslog")
       :to-equal
       '("@include" "time_format" "rfc5424time_format" "message_format" "with_priority" "parser_type" "support_colonless_ident"))))

   (it
    "should complete `@type regexp`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (tail-subsection
	    (touchdown--section-subsection section "tail"))
	   (parse-subsection
	    (touchdown--section-subsection tail-subsection "parse")))

      (expect
       (touchdown--section-completions parse-subsection "regexp")
       :to-equal
       '("@include" "expression" "time_key" "time_format"))))

   (it
    "should complete `@type nginx`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (tail-subsection
	    (touchdown--section-subsection section "tail"))
	   (parse-subsection
	    (touchdown--section-subsection tail-subsection "parse")))

      (expect
       (touchdown--section-completions parse-subsection "nginx")
       :to-equal
       '("@include" "expression" "time_format"))))

   (it
    "should complete `@type json`"
    (let* ((section
	    (touchdown--section-subsection touchdown--syntax-tree "source"))
	   (tail-subsection
	    (touchdown--section-subsection section "tail"))
	   (parse-subsection
	    (touchdown--section-subsection tail-subsection "parse")))

      (expect
       (touchdown--section-completions parse-subsection "json")
       :to-equal
       '("@include" "json_parser" "stream_buffer_size" "time_type" "time_format"))))
   )))

;;; input-completion-tests.el ends here
