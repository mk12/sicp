;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(import (rnrs base (6))
        (rnrs control (6))
        (rnrs io simple (6))
        (rnrs lists (6))
        (rnrs programs (6))
        (only (src compat active) format seed-rng)
        (only (src lang core) run-sicp)
        (src chapter-1)
        (src chapter-2)
        (src chapter-3))

(define (usage program)
  (format "\
usage: ~A [-hsv] [FILTER ...]

Runs SICP code and tests. If FILTER is provided, only runs the parts whose id
matches one of the FILTER arguments. For example:

  1       chapter 1
  1 2     chapters 1 and 2
  :1      chapter 1, minus exercises
  :1.2    section 1.2, including subsections
  :1.2.3  subsection 1.2.3
  ?1.12   exercise 1.12

It will also run all transitive dependencies of the specified parts.

options:

  -h, --help       show this help message
  -s, --slow       run slow tests too
  -v, --verbose    verbose output
  -n, --no-color   disable color
" program))

(define (die msg)
  (display msg (current-error-port))
  (exit 1))

(define (parse-options args)
  (define (just . opt) (list opt))
  (define (go args options)
    (define (is? . flags) (member (car args) flags))
    (define (startswith? c)
      (and (> (string-length (car args)) 0)
           (char=? c (string-ref (car args) 0))))
    (define (add . opt) (go (cdr args) (cons opt options)))
    (cond
      ((null? args) options)
      ((is? "-h" "--help") (just 'help))
      ((is? "-s" "--slow") (add 'slow))
      ((is? "-v" "--verbose") (add 'verbose))
      ((is? "-n" "--no-color") (add 'no-color))
      ((not (startswith? #\-)) (add 'filter (car args)))
      (else (just 'error))))
  (go args '()))

(define (collq key alist)
  (define (go alist res)
    (cond
      ((null? alist) res)
      ((eq? key (caar alist))
       (go (cdr alist) (cons (cadar alist) res)))
      (else (go (cdr alist) res))))
  (go alist '()))

(define (run filters slow verbose color)
  chapter-1-effects
  chapter-2-effects
  chapter-3-effects
  (unless (run-sicp filters slow verbose color)
    (exit 1)))

(define (main argv)
  (seed-rng)
  (let ((program (car argv))
        (options (parse-options (cdr argv))))
    (cond
      ((assq 'error options)
       (die (usage program)))
      ((assq 'help options)
       (display (usage program)))
      (else
        (run (collq 'filter options)
             (assq 'slow options)
             (assq 'verbose options)
             (not (assq 'no-color options)))))))

(main (command-line))
