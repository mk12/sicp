;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(import (rnrs base (6))
        (rnrs control (6))
        (rnrs io simple (6))
        (rnrs lists (6))
        (rnrs programs (6))
        (only (src compat) format seed-rng)
        (only (src lang core) run-sicp)
        (src sicp chapter-1)
        (src sicp chapter-2)
        (src sicp chapter-3)
        (src sicp chapter-4)
        (src sicp chapter-5))

(define (usage program)
  (format "\
Usage: ~A [-hvn] [FILTER ...]

Run SICP code and tests

If FILTER is provided, only runs matching modules. For example:

    1       chapter 1
    1 2     chapters 1 and 2
    :1      chapter 1, minus exercises
    :1.2    section 1.2, including subsections
    :1.2.3  subsection 1.2.3
    ?1.12   exercise 1.12

It will also run all transitive dependencies of the selected modules.

Options:
    -h, --help      Show this help message
    -v, --verbose   Enable verbose output
    -n, --no-color  Disable color output
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
    (cond ((null? args) options)
          ((is? "-h" "--help") (just 'help))
          ((is? "-v" "--verbose") (add 'verbose))
          ;; Note: run.sh passes --no-color based on NO_COLOR and isatty.
          ((is? "-n" "--no-color") (add 'no-color))
          ((not (startswith? #\-)) (add 'filter (car args)))
          (else (just 'error))))
  (go args '()))

(define (collq key alist)
  (define (go alist res)
    (cond ((null? alist) res)
          ((eq? key (caar alist))
           (go (cdr alist) (cons (cadar alist) res)))
          (else (go (cdr alist) res))))
  (go alist '()))

(define (run filters verbose color)
  ;; It's important that we register the chapters in order, since this provides
  ;; the default order for running tests. The topological sort step only moves
  ;; entries when it absolutely has to (because of forward dependencies).
  (register-chapter-1)
  (register-chapter-2)
  (register-chapter-3)
  (register-chapter-4)
  (register-chapter-5)
  (unless (run-sicp filters verbose color)
    (exit 1)))

(define (main argv)
  (seed-rng)
  (let ((program (car argv))
        (options (parse-options (cdr argv))))
    (cond ((assq 'error options)
           (die (usage program)))
          ((assq 'help options)
           (display (usage program)))
          (else
           (run (collq 'filter options)
                (assq 'verbose options)
                (not (assq 'no-color options)))))))

(main (command-line))
