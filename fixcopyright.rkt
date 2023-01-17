#!/usr/bin/env racket
#lang racket
;;; SPDX-License-Identifier: LGPL-3.0-or-later
;;; SPDX-FileCopyrightText: Copyright © 2021-2023 Tony Garnock-Jones <tonyg@leastfixedpoint.com>

(provide (struct-out fix-files-stats)
         fix-files)

(require file/glob)
(require racket/date)
(require racket/runtime-path)
(require json)

(define-runtime-path licenses-json-path "licenses.json")

(define license-data (with-input-from-file licenses-json-path read-json))

(define ((re p) i) (regexp-match p i))
(define ((re? p) i) (regexp-match? p i))
(define ((s p ins) i) (regexp-replace p i ins))

(define (get-git-config key)
  (string-trim (with-output-to-string
                 (lambda ()
                   (unless (system* "/usr/bin/env" "git" "config" "--get" key)
                     (error 'get-git-config "Could not get git config for ~a" key))))))

(define (is-tracked? f)
  (call-with-output-file "/dev/null" #:exists 'append
    (lambda (sink)
      (parameterize ((current-error-port sink)
                     (current-output-port sink))
        (system* "/usr/bin/env" "git" "ls-files" "--error-unmatch" f)))))

(define (make-copyright who low [hi #f])
  (if (and hi (not (string=? low hi)))
      (format "Copyright © ~a-~a ~a" low hi who)
      (format "Copyright © ~a ~a" low who)))

(struct fix-files-stats (file-count changed-files) #:prefab)

(define DEFAULT-FRONT-MATTER-RE "^#")

(define (fix-files #:file-type-name file-type-name
                   #:file-pattern file-pattern
                   #:leading-comment-re leading-comment-re
                   #:comment-prefix comment-prefix
                   #:license license
                   #:front-matter-re [front-matter-re0 '#:default]
                   #:file-filter [file-filter file-exists?]
                   #:this-year [this-year0 #f]
                   #:user-name [user-name0 #f]
                   #:user-email [user-email0 #f]
                   #:user [user0 #f]
                   #:quiet? [quiet? #f]
                   #:dry-run? [dry-run? #f]
                   #:modify-untracked? [modify-untracked? #f]
                   )

  (define front-matter-re (if (eq? front-matter-re0 '#:default)
                              (pregexp DEFAULT-FRONT-MATTER-RE)
                              front-matter-re0))

  (define user-name (or user-name0 (get-git-config "user.name")))
  (define user-email (or user-email0 (get-git-config "user.email")))
  (define user (or user0 (format "~a <~a>" user-name user-email)))

  (define this-year (or this-year0 (number->string (date-year (current-date)))))

  (define matched-files (filter file-filter (glob file-pattern)))
  (define file-count (length matched-files))
  (define changed-files 0)

  (for [(file-number (in-naturals))
        (f (in-list matched-files))]
    (unless quiet?
      (printf "~a [~a/~a] ~a ..." file-type-name file-number file-count f)
      (flush-output))
    (define all-lines (file->lines f))
    (define-values (front-matter head tail)
      (let*-values (((lines) all-lines)
                    ((front-matter lines) (if front-matter-re
                                              (splitf-at lines (re? front-matter-re))
                                              (values '() lines)))
                    ((head tail) (splitf-at lines (re? leading-comment-re))))
        (values front-matter head tail)))
    (let* ((head (map (s leading-comment-re "") head))
           (head (map (lambda (l)
                        (match (regexp-match "^([^:]+): (.*)$" l)
                          [(list _ k v) (list k v)]
                          [#f (list #f l)]))
                      head))
           (head (if (assoc "SPDX-FileCopyrightText" head)
                     head
                     (cons (list "SPDX-FileCopyrightText" (make-copyright user this-year)) head)))
           (head (if (assoc "SPDX-License-Identifier" head)
                     head
                     (cons (list "SPDX-License-Identifier" license) head)))
           (head (map (lambda (l)
                        (match l
                          [(list "SPDX-FileCopyrightText"
                                 (and (regexp (regexp-quote user-name))
                                      (regexp #px"(\\d{4})-\\d{4}" (list _ low))))
                           (list "SPDX-FileCopyrightText"
                                 (make-copyright user low this-year))]
                          [(list "SPDX-FileCopyrightText"
                                 (and (regexp (regexp-quote user-name))
                                      (regexp #px"\\d{4}" (list low))))
                           (list "SPDX-FileCopyrightText"
                                 (make-copyright user low this-year))]
                          [_ l]))
                      head))
           (head (map (lambda (l)
                        (if (string=? (cadr l) "")
                            (string-trim comment-prefix)
                            (string-append comment-prefix
                                           (match l
                                             [(list #f v) v]
                                             [(list k v) (format "~a: ~a" k v)]))))
                      head))
           (new-lines `(,@front-matter
                        ,@head
                        ""
                        ,@(dropf tail (lambda (l) (string=? (string-trim l) "")))))
           (would-change-if-written? (not (equal? all-lines new-lines)))
           (write-needed? (and would-change-if-written? (or modify-untracked? (is-tracked? f)))))
      (when (and write-needed? (not dry-run?))
        (call-with-atomic-output-file
         f
         (lambda (port _tmp-path)
           (for [(l front-matter)] (displayln l port))
           (for [(l head)] (displayln l port))
           (newline port)
           (for [(l (dropf tail (lambda (l) (string=? (string-trim l) ""))))] (displayln l port)))))
      (if write-needed?
          (begin (set! changed-files (+ changed-files 1))
                 (unless quiet?
                   (printf "\e[41mchanged\e[0m\n")))
          (unless quiet?
            (printf "\r\e[K")))))

  (when (positive? changed-files)
    (unless quiet?
      (printf "~a [~a total files, ~a changed]\n" file-type-name file-count changed-files)))

  (fix-files-stats file-count changed-files))

(module+ main
  (define program-name "fixcopyright")

  (define dry-run? #f)
  (define modify-untracked? #f)
  (define front-matter-re '#:default)
  (define quiet? #f)
  (define file-pattern #f)

  (define -file-type-name #f)
  (define -leading-comment-re #f)
  (define -comment-prefix #f)

  (define-values (license file-type-name leading-comment-re comment-prefix)
    (command-line #:program program-name
                  #:once-each
                  [("-n" "--dry-run")
                   "Do not write back changes to files"
                   (set! dry-run? #t)]
                  [("--modify-untracked")
                   "Modify files not tracked by git as well as those that are"
                   (set! modify-untracked? #t)]
                  [("--front-matter-re") re
                   ((format "Set regular expression used to skip front matter (default: ~a)"
                            DEFAULT-FRONT-MATTER-RE))
                   (set! front-matter-re (pregexp re))]
                  [("--no-front-matter-re")
                   "Disable skipping front matter"
                   (set! front-matter-re #f)]
                  [("--file-pattern") superglob
                   "Glob (with ** allowed as well as * and ?) for matching source files"
                   (set! file-pattern superglob)]
                  [("--preset-racket")
                   "Presets for working with Racket files"
                   (set! file-pattern "**.rkt")
                   (set! -file-type-name "Racket")
                   (set! -leading-comment-re "^;+ *")
                   (set! -comment-prefix ";;; ")]
                  [("--preset-typescript")
                   "Presets for working with TypeScript files"
                   (set! file-pattern "**.ts")
                   (set! -file-type-name "TypeScript")
                   (set! -leading-comment-re "^//+ *")
                   (set! -comment-prefix "/// ")]
                  [("--preset-javascript")
                   "Presets for working with JavaScript files"
                   (set! file-pattern "**.js")
                   (set! -file-type-name "JavaScript")
                   (set! -leading-comment-re "^//+ *")
                   (set! -comment-prefix "/// ")]
                  [("-q" "--quiet")
                   "Disable progress reports during processing"
                   (set! quiet? #t)]
                  #:args (license
                          [file-type-name #f]
                          [leading-comment-re #f]
                          [comment-prefix #f])
                  (values license
                          (or file-type-name -file-type-name)
                          (cond [(or leading-comment-re -leading-comment-re) => pregexp] [else #f])
                          (or comment-prefix -comment-prefix))))

  (define license-details
    (ormap (lambda (L) (and (string-ci=? (hash-ref L 'licenseId) license) L))
           (hash-ref license-data 'licenses)))

  (unless license-details
    (error (string->symbol program-name) "Unknown license ID: ~a" license))

  (unless (and file-type-name file-pattern leading-comment-re comment-prefix)
    (eprintf "Please supply a preset or all of file-type-name, file-pattern, leading-comment-re, and comment-prefix.\n")
    (exit 2))

  (match-define (fix-files-stats total-file-count total-changed-files)
    (fix-files #:file-type-name file-type-name
               #:file-pattern file-pattern
               #:leading-comment-re leading-comment-re
               #:comment-prefix comment-prefix
               #:license (hash-ref license-details 'licenseId)
               #:front-matter-re front-matter-re
               #:quiet? quiet?
               #:dry-run? dry-run?
               #:modify-untracked? modify-untracked?))

  (unless quiet?
    (printf "~a: ~a files examined, ~a ~a\n"
            program-name
            total-file-count
            total-changed-files
            (if dry-run?
                (if (zero? total-changed-files)
                    "changes are needed"
                    "files need to be updated")
                (if (zero? total-changed-files)
                    "changes were needed"
                    "files were updated"))))

  (exit (if (positive? total-changed-files) 1 0)))
