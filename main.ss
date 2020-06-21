;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(import (rnrs base (6))
        (rnrs io simple (6))
        (rnrs lists (6))
        (rnrs programs (6))
        (src compat impl))

(define (usage program)
  (format "\
usage: ~A [-hsv] [CHAPTER]

Runs SICP code and tests. If CHAPTER is omitted, runs all chapters.

options:

  -h, --help       show this help message
  -s, --slow       run slow tests too
  -v, --verbose    verbose output
" program))

(define (die msg)
  (display msg (current-error-port))
  (exit 1))

(define (parse-options args)
  (define (just . opt) (list opt))
  (define (go args options)
    (define (is? . flags) (member (car args) flags))
    (define (add . opt) (go (cdr args) (cons opt options)))
    (cond
      ((null? args) options)
      ((is? "-h" "--help") (just 'help))
      ((is? "-s" "--slow") (add 'slow))
      ((is? "-v" "--verbose") (add 'verbose))
      ((is? "1" "2" "3") (add 'chapter (string->number (car args))))
      (else (just 'error))))
  (go args (just 'chapter 'all)))

(define (run chapter slow verbose)
  (display "Hi\n")
  (load "src/lang/syntax.ss")
  (load "sicptest.ss")
  ; (load "chapter-1.scm")
  ; TODO
  )

(define (main argv)
  (let ((program (car argv))
        (options (parse-options (cdr argv))))
    (cond
      ((assq 'error options)
       (die (usage program)))
      ((assq 'help options)
       (display (usage program)))
      (else
        (run (cadr (assq 'chapter options))
             (assq 'slow options)
             (assq 'verbose options))))))

(main (command-line))
