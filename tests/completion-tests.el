;;; completion-tests.el --- Completion tests -*- lexical-binding: t; -*-

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

;; Completion tests for touchdown.

;;; Code:

(require 'helpers "tests/helpers.el")
(require 'touchdown "touchdown.el")

(describe
 "touchdown-mode syntax completion"

 (describe
  "touchdown-mode root completion"

  (it
   "should complete in root"
   (with-touchdown-temp-buffer
    "@include root.conf

<source>
</source>
"

    (forward-cursor-on "@include")
    (forward-line 1)
    (message "%s" (touchdown--dynamic-completion-table ""))
    (expect
     (touchdown--dynamic-completion-table "")
     :to-equal
     '("@include" "<system>" "<label>" "<match>" "<filter>" "<source>"))

    (expect
     (touchdown--dynamic-completion-table "@")
     :to-equal
     '("@include"))

    (expect
     (touchdown--dynamic-completion-table "@in")
     :to-equal
     '("@include"))

    (expect
     (touchdown--dynamic-completion-table "@type")
     :to-equal
     nil)

    (expect
     (touchdown--dynamic-completion-table "<s")
     :to-equal
     '("<system>" "<source>"))

    (expect
     (touchdown--dynamic-completion-table "<so")
     :to-equal
     '("<source>"))

    (expect
     (touchdown--dynamic-completion-table "<sy")
     :to-equal
     '("<system>"))

    (expect
     (touchdown--dynamic-completion-table "<f")
     :to-equal
     '("<filter>"))

    (expect
     (touchdown--dynamic-completion-table "<m")
     :to-equal
     '("<match>"))

    (expect
     (touchdown--dynamic-completion-table "<l")
     :to-equal
     '("<label>")))))

 (describe
  "touchdown-mode source completion"

  (it
   "should complete an open source section"
   (with-touchdown-temp-buffer
    "@include root.conf

<source>

</source>
"

    (forward-cursor-on "<source>")
    (forward-line 1)
    (message "%s" (touchdown--dynamic-completion-table ""))
    (expect
     (touchdown--dynamic-completion-table "")
     :to-equal
     '("@include" "@id" "@label" "@type tail" "@type syslog" "@type forward" "</source>")))))

 (describe
  "touchdown-mode match completion"

  (it
   "should complete `match`"
   (with-touchdown-temp-buffer
    "@include root.conf

<match>

</match>
"

    (forward-cursor-on "<match>")
    (forward-line 1)
    (expect
     (touchdown--dynamic-completion-table "")
     :to-equal
     '("@include" "@id" "@label" "@type mongo" "@type file" "</match>"))))

  (it
   "should complete `match`->`@type file`"
   (with-touchdown-temp-buffer
    "@include root.conf

<match>
  @type file
  path some_file
</match>
"

    (forward-cursor-on "<match>")
    (forward-line 1)
    (expect
     (touchdown--dynamic-completion-table "")
     :to-equal
      '("@include" "@id" "@label" "path" "append" "add_path_suffix" "path_suffix" "compress" "recompress" "@log_level" "symlink_path" "<inject>" "<buffer>" "<format>"))))

  (it
   "should complete `match`->`@type file` after `@type file` line"
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
     (touchdown--dynamic-completion-table "")
     :to-equal
     '("@include" "@id" "@label" "path" "append" "add_path_suffix" "path_suffix" "compress" "recompress" "@log_level" "symlink_path" "<inject>" "<buffer>" "<format>"))))

  (it
   "should complete `match`->`@type file`->`buffer`"
   (with-touchdown-temp-buffer
    "@include root.conf

<match>
  @type file

  <buffer>

  </buffer>
</match>
"

    (forward-cursor-on "<buffer>")
    (forward-line 1)
    (expect
     (touchdown--dynamic-completion-table "")
     :to-equal
     '("@include" "@type" "timekey" "timekey_wait" "timekey_use_utc" "timekey_zone" "chunk_limit_size" "chunk_limit_records" "total_limit_size" "queue_limit_length" "chunk_full_threshold" "queued_chunks_limit_size" "compress" "flush_at_shutdown" "flush_mode" "flush_interval_time" "flush_thread_count" "flush_thread_interval" "flush_thread_burst_interval" "delayed_commit_timeout" "overflow_action" "retry_timeout" "retry_foreve" "retry_max_times" "retry_secondary_threshold" "retry_type" "retry_wait" "retry_exponential_backoff_base" "retry_max_interval" "retry_randomize" "disable_chunk_backup" "</buffer>"))))

  (it
   "should complete `match`->`@type file`->`inject`"
   (with-touchdown-temp-buffer
    "@include root.conf

<match>
  @type file

  <inject>

  </inject>
</match>
"

    (forward-cursor-on "<inject>")
    (forward-line 1)
    (expect
     (touchdown--dynamic-completion-table "")
     :to-equal
     '("@include" "hostname_key" "hostname" "worker_id_key" "tag_key" "time_key" "time_type" "time_format" "localtime" "utc" "timezone" "</inject>"))))

  (it
   "should complete `match`->`@type file`->`format`"
   (with-touchdown-temp-buffer
    "@include root.conf

<match>
  @type file

  <format>

  </format>
</match>
"

    (forward-cursor-on "<format>")
    (forward-line 1)
    (expect
     (touchdown--dynamic-completion-table "")
     :to-equal
     '("@include" "@type file" "</format>"))))

  (it
   "should complete `match`->`@type file`->`format`->`@type file`"
   (with-touchdown-temp-buffer
    "@include root.conf

<match>
  @type file

  <format>
    @type file
  </format>
</match>
"

    (forward-cursor-on "<format>")
    (forward-line 1)
    (expect
     (touchdown--dynamic-completion-table "")
     :to-equal
     '("@include" "delimiter" "output_tag" "output_time" "time_type" "time_format" "newline"))))

  (it
   "should complete `match`->`@type mongo`"
   (with-touchdown-temp-buffer
    "@include root.conf

<match>
  @type mongo
</match>
"

    (forward-cursor-on "<match>")
    (forward-line 1)
    (expect
     (touchdown--dynamic-completion-table "")
     :to-equal
     '("@include" "@id" "@label" "auth_mech" "auth_source" "capped" "capped_size" "collection" "connection_string" "database" "date_keys" "expire_after" "host" "include_tag_key" "include_time_key" "journaled" "mongo_log_level" "parse_string_number_date" "password" "port" "replace_dollar_in_key_with" "replace_dot_in_key_with" "ssl" "ssl_ca_cert" "ssl_cert" "ssl_key" "ssl_key_pass_phrase" "ssl_verify" "time_key" "user" "write_concern" "<inject>" "<buffer>"))))

  (it
   "should complete `match`->`@type mongo`->`buffer`"
   (with-touchdown-temp-buffer
    "@include root.conf

<match>
  @type mongo

  <buffer>

  </buffer>
</match>
"

    (forward-cursor-on "<buffer>")
    (forward-line 1)
    (expect
     (touchdown--dynamic-completion-table "")
     :to-equal
     '("@include" "@type" "timekey" "timekey_wait" "timekey_use_utc" "timekey_zone" "chunk_limit_size" "chunk_limit_records" "total_limit_size" "queue_limit_length" "chunk_full_threshold" "queued_chunks_limit_size" "compress" "flush_at_shutdown" "flush_mode" "flush_interval_time" "flush_thread_count" "flush_thread_interval" "flush_thread_burst_interval" "delayed_commit_timeout" "overflow_action" "retry_timeout" "retry_foreve" "retry_max_times" "retry_secondary_threshold" "retry_type" "retry_wait" "retry_exponential_backoff_base" "retry_max_interval" "retry_randomize" "disable_chunk_backup" "</buffer>"))))

  (it
   "should complete `match`->`@type mongo`->`inject`"
   (with-touchdown-temp-buffer
    "@include root.conf

<match>
  @type mongo

  <inject>

  </inject>
</match>
"

    (forward-cursor-on "<inject>")
    (forward-line 1)
    (expect
     (touchdown--dynamic-completion-table "")
     :to-equal
     '("@include" "hostname_key" "hostname" "worker_id_key" "tag_key" "time_key" "time_type" "time_format" "localtime" "utc" "timezone" "</inject>")))))

 (describe
  "touchdown-mode parameter completion"

  (it
   "should complete boolean parameters"
   (with-touchdown-temp-buffer
    "@include root.conf

<system>
  suppress_repeated_stacktrace
</system>
"

    (forward-cursor-on "<system>")
    (forward-line 1)
    (end-of-line)
    (expect
     (touchdown--dynamic-completion-table "")
     :to-equal
     '("true" "false")))))

;;   (it
;;    "should complete `true` for a boolean parameter"
;;    (with-touchdown-temp-buffer
;;     "@include root.conf

;; <system>
;;   suppress_repeated_stacktrace t
;; </system>
;; "

;;     (forward-cursor-on "<system>")
;;     (forward-line 1)
;;     (end-of-line)
;;     (expect
;;      (touchdown--dynamic-completion-table "")
;;      :to-equal
;;      "true")))

;;   (it
;;    "should complete `false` for a boolean parameter"
;;    (with-touchdown-temp-buffer
;;     "@include root.conf

;; <system>
;;   suppress_repeated_stacktrace f
;; </system>
;; "

;;     (forward-cursor-on "<system>")
;;     (forward-line 1)
;;     (end-of-line)
;;     (expect
;;      (touchdown--dynamic-completion-table "")
;;      :to-equal
;;      '("false")))))

;;  (describe
;;   "touchdown-mode parameter completion"

;;   (it
;;    "should complete enum parameters"
;;    (with-touchdown-temp-buffer
;;     "@include root.conf

;; <system>
;;   @log_level
;; </system>
;; "
;;     (forward-cursor-on "<system>")
;;     (forward-line 1)
;;     (end-of-line)
;;     (expect
;;      (touchdown--dynamic-completion-table "")
;;      :to-equal
;;      '("fatal" "error" "warn" "info" "debug" "trace"))))

;;   (it
;;    "should complete options on enum parameters"
;;    (with-touchdown-temp-buffer
;;     "@include root.conf

;; <system>
;;   @log_level f
;; </system>
;; "
;;     (forward-cursor-on "<system>")
;;     (forward-line 1)
;;     (end-of-line)
;;     (expect
;;      (touchdown--dynamic-completion-table "")
;;      :to-equal
;;        "fatal"))))

 (describe
  "touchdown-mode `try-completion'"

  (it
   "should match `@i` with the longest match"
   (let ((expected (touchdown--try-completion "@i" nil nil))
         (actual "@include"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `@inc` with the longest match"
   (let ((expected (touchdown--try-completion "@inc" nil nil))
         (actual "@include"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `@include` exactly"
   (let ((expected (touchdown--try-completion "@include" nil nil))
         (actual t))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `<so` with the longest match"
   (let ((expected (touchdown--try-completion "<so" nil nil))
         (actual "<source>"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `<sy` with the longest match"
   (let ((expected (touchdown--try-completion "<sy" nil nil))
         (actual "<system>"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `<s` with the longest match"
   (let ((expected (touchdown--try-completion "<s" nil nil))
         (actual "<s"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `</so` with the longest match"
   (let ((expected (touchdown--try-completion "</so" nil nil))
         (actual "</source>"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should match `</sy` with the longest match"
   (let ((expected (touchdown--try-completion "</sy" nil nil))
         (actual "</system>"))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should not match `@includes`"
   (let ((expected (touchdown--try-completion "@includes" nil nil))
         (actual nil))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should not match `@file`"
   (let ((expected (touchdown--try-completion "@file" nil nil))
         (actual nil))
     (expect
      expected
      :to-equal
      actual))))

 (describe
  "touchdown-mode `test-completion'"

  (it
   "should return t or nil appropriately"
   (let ((data (list
                '("@i". t)
                '("@inc". t)
                '("@include". t)
                '("@includes". nil)
                '("@file". nil)
                '("<so". t)
                '("<sy". t)
                '("<s". t)
                '("</so". t)
                '("</sy". t)
                '("</s". t))))
     (while data
       (let ((datum (car data)))
         (let ((expected (touchdown--test-completion (car datum) nil nil))
               (actual (cdr datum)))
           (expect
            expected
            :to-equal
            actual)
           (setq data (cdr data))))))))

 (describe
  "touchdown-mode `all-completions'"

  (it
   "should find `@include` for `@i`"
   (let ((expected (touchdown--all-completions "@i" nil nil))
         (actual '("@include")))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should find `(\"<system>\" \"<source>\")` for `<s`"
   (let ((expected (touchdown--all-completions "<s" nil nil))
         (actual '("<system>" "<source>")))
     ;; (print (format "expected:  %s, actual:  %s" expected actual))
     (expect
      expected
      :to-equal
      actual)))

  (it
   "should find `(\"</system>\" \"</source>\")` for `<s`"
   (let ((expected (touchdown--all-completions "</s" nil nil))
         (actual '("</system>" "</source>")))
     ;; (print (format "expected:  %s, actual:  %s" expected actual))
     (expect
      expected
      :to-equal
      actual)))))

;;; completion-tests.el ends here
