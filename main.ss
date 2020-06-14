;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(import (rnrs (6))
        (src compat impl))

(define (usage program)
  (format "\
usage: ~A [-hsv] [CHAPTER]

Executes SICP code. If CHAPTER is omitted, runs all chapters.

options:

  -h, --help       show this help message
  -s, --slow       run slow tests too
  -v, --verbose    verbose output
" program))

(define (die msg)
  (display msg (current-error-port))
  (exit 1))

(define (parse-options args)
  (define (go args options)
    (define (is? . flags) (member (car args) flags))
    (define (add opt) (go (cdr args) (cons opt options)))
    (cond
      ((null? args) options)
      ((is? "-h" "--help") '((help)))
      ((is? "-s" "--slow") (add '(slow)))
      ((is? "-v" "--verbose") (add '(verbose)))
      ((is? "1" "2" "3") (add `(chapter ,(string->number (car args)))))
      (else '((error)))))
  (go args '((chapter all))))

(define (run chapter slow verbose)
  (display "Hi\n")
  ; (load "syntax.scm")
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
       (display (usage program))
       (exit 0))
      (else
        (run (assq 'chapter options)
             (assq 'slow options)
             (assq 'verbose options))))))

(main (command-line))
