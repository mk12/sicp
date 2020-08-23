;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat active)
  (export current-output-port format make-parameter open-output-string
          parameterize patch-output random runtime seed-rng string-contains?
          syntax->location with-output-to-string)
  (import (rnrs base (6))
          (only (racket base)
                current-inexact-milliseconds current-output-port current-seconds
                format make-parameter open-output-string parameterize random
                random-seed remainder path->string syntax-column syntax-line
                syntax-source)
          (only (racket string) string-contains? string-replace)
          (only (racket port)
                with-output-to-string))

  ;; Hack: Racket's r6rs mode uses it's mutable pair library for `cons`, `car`,
  ;; and `cdr` (which are normally named `mcons`, `mcar`, and `mcdr`). This
  ;; leaks when writing lists to strings, because Racket uses braces instead of
  ;; parentheses for mutable lists. To make tests work for all implementations,
  ;; we patch the output only for Racket, and hope there are no examples that
  ;; actually use the characters "{" and "}".
  (define (patch-output s)
    (string-replace (string-replace s "{" "(") "}" ")"))

  (define (runtime)
    (/ (current-inexact-milliseconds) 1e3))

  (define (seed-rng)
    (random-seed (remainder (current-seconds) (expt 2 32))))

  (define (syntax->location s)
    (values (path->string (syntax-source s))
            (syntax-line s)
            (+ 1 (syntax-column s))))) ; convert to 1-based
