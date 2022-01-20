;;; output-completion-tests.el --- Output completion tests -*- lexical-binding: t; -*-

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

;; Output completion tests for touchdown.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

(describe
 "touchdown-mode output completion options"

 (describe
  "touchdown-mode match section completion options"

  (it
   "should exist and be named `match`"
   (let* ((section
           (touchdown--section-subsection touchdown--syntax-tree "match")))
     (expect
      (touchdown--section-name section)
      :to-equal
      "match")))

  (it
   "should provide completion options for a `@type`-less `match`"
   (let* ((section
           (touchdown--section-subsection touchdown--syntax-tree "match")))
     (expect
      (touchdown--section-completions section nil)
      :to-equal
      '("@include" "@id" "@label" "@type mongo" "@type file" "</match>"))))

  (it
   "should provide completion options for `match` with `@type file`"
   (let* ((section
           (touchdown--section-subsection touchdown--syntax-tree "match")))
     (expect
      (touchdown--section-completions section "file")
      :to-equal
      '("@include" "@id" "@label" "path" "append" "add_path_suffix" "path_suffix" "compress" "recompress" "@log_level" "symlink_path" "<inject>" "<buffer>" "<format>"))))

  (it
   "should provide completion options for `match` with `@type mongo`"
   (let* ((section
           (touchdown--section-subsection touchdown--syntax-tree "match")))
     (expect
      (touchdown--section-completions section "mongo")
      :to-equal
      '("@include" "@id" "@label" "auth_mech" "auth_source" "capped" "capped_size" "collection" "connection_string" "database" "date_keys" "expire_after" "host" "include_tag_key" "include_time_key" "journaled" "mongo_log_level" "parse_string_number_date" "password" "port" "replace_dollar_in_key_with" "replace_dot_in_key_with" "ssl" "ssl_ca_cert" "ssl_cert" "ssl_key" "ssl_key_pass_phrase" "ssl_verify" "time_key" "user" "write_concern" "<inject>" "<buffer>")))))

 (describe
  "touchdown-mode output plugin completion"

  (describe
   "touchdown-mode file output plugin completion"

   (it
    "should exist and be named `file`"
    (let* ((section
            (touchdown--section-subsection touchdown--syntax-tree "match"))
           (file-subsection
            (touchdown--section-subsection section "file")))
      (expect
       (touchdown--section-name file-subsection)
       :to-equal
       "file")))

   (it
    "should provide completion options for `buffer`"
    (let* ((section
            (touchdown--section-subsection touchdown--syntax-tree "match"))
           (file-subsection
            (touchdown--section-subsection section "file"))
           (buffer-subsection
            (touchdown--section-subsection file-subsection "buffer")))
      (expect
       (touchdown--section-completions buffer-subsection nil)
       :to-equal
       '("@include" "@type" "timekey" "timekey_wait" "timekey_use_utc" "timekey_zone" "chunk_limit_size" "chunk_limit_records" "total_limit_size" "queue_limit_length" "chunk_full_threshold" "queued_chunks_limit_size" "compress" "flush_at_shutdown" "flush_mode" "flush_interval_time" "flush_thread_count" "flush_thread_interval" "flush_thread_burst_interval" "delayed_commit_timeout" "overflow_action" "retry_timeout" "retry_foreve" "retry_max_times" "retry_secondary_threshold" "retry_type" "retry_wait" "retry_exponential_backoff_base" "retry_max_interval" "retry_randomize" "disable_chunk_backup" "</buffer>"))))

   (it
    "should provide completion options for `inject`"
    (let* ((section
            (touchdown--section-subsection touchdown--syntax-tree "match"))
           (file-subsection
            (touchdown--section-subsection section "file"))
           (inject-subsection
            (touchdown--section-subsection file-subsection "inject")))
      (expect
       (touchdown--section-completions inject-subsection nil)
       :to-equal
       '("@include" "hostname_key" "hostname" "worker_id_key" "tag_key" "time_key" "time_type" "time_format" "localtime" "utc" "timezone" "</inject>"))))

   (it
    "should provide completion options for a `@type`-less `format`"
    (let* ((section
            (touchdown--section-subsection touchdown--syntax-tree "match"))
           (file-subsection
            (touchdown--section-subsection section "file"))
           (format-subsection
            (touchdown--section-subsection file-subsection "format")))
      (expect
       (touchdown--section-completions format-subsection nil)
       :to-equal
       '("@include" "@type file" "</format>"))))

   (it
    "should provide completion options for `format` with `@type file`"
    (let* ((section
            (touchdown--section-subsection touchdown--syntax-tree "match"))
           (file-subsection
            (touchdown--section-subsection section "file"))
           (format-subsection
            (touchdown--section-subsection file-subsection "format")))
      (expect
       (touchdown--section-completions format-subsection "file")
       :to-equal
       '("@include" "delimiter" "output_tag" "output_time" "time_type" "time_format" "newline")))))

  (describe
   "touchdown-mode mongo output plugin completion"

   (it
    "should exist and be named `mongo`"
    (let* ((section
            (touchdown--section-subsection touchdown--syntax-tree "match"))
           (mongo-subsection
            (touchdown--section-subsection section "mongo")))

      (expect
       (touchdown--section-name mongo-subsection)
       :to-equal
       "mongo"))))

  (it
   "should provide completion options for `buffer`"
   (let* ((section
           (touchdown--section-subsection touchdown--syntax-tree "match"))
          (mongo-subsection
           (touchdown--section-subsection section "mongo"))
          (buffer-subsection
           (touchdown--section-subsection mongo-subsection "buffer")))
     (expect
      (touchdown--section-completions buffer-subsection nil)
      :to-equal
      '("@include" "@type" "timekey" "timekey_wait" "timekey_use_utc" "timekey_zone" "chunk_limit_size" "chunk_limit_records" "total_limit_size" "queue_limit_length" "chunk_full_threshold" "queued_chunks_limit_size" "compress" "flush_at_shutdown" "flush_mode" "flush_interval_time" "flush_thread_count" "flush_thread_interval" "flush_thread_burst_interval" "delayed_commit_timeout" "overflow_action" "retry_timeout" "retry_foreve" "retry_max_times" "retry_secondary_threshold" "retry_type" "retry_wait" "retry_exponential_backoff_base" "retry_max_interval" "retry_randomize" "disable_chunk_backup" "</buffer>"))))

  (it
   "should provide completion options for `inject`"
   (let* ((section
           (touchdown--section-subsection touchdown--syntax-tree "match"))
          (mongo-subsection
           (touchdown--section-subsection section "mongo"))
          (inject-subsection
           (touchdown--section-subsection mongo-subsection "inject")))
     (expect
      (touchdown--section-completions inject-subsection nil)
      :to-equal
      '("@include" "hostname_key" "hostname" "worker_id_key" "tag_key" "time_key" "time_type" "time_format" "localtime" "utc" "timezone" "</inject>"))))))

;;; output-completion-tests.el ends here
