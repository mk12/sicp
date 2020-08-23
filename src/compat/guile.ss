;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat active)
  (export current-output-port format make-paramter open-output-string
          parameterize patch-output random runtime seed-rng string-contains?
          syntax->location with-output-to-string)
  (import (rnrs base (6))
          (only (guile)
                *random-state* current-output-port gettimeofday make-parameter
                open-output-string parameterize random
                random-state-from-platform source-property string-contains
                syntax-source with-output-to-string)
          (prefix (only (guile) format) guile-))

  (define (format . args)
    (apply guile-format #f args))

  (define (patch-output s) s)

  (define (runtime)
    (let ((t (gettimeofday)))
      (+ (car t) (/ (cdr t) 1e6))))

  (define (seed-rng)
    (set! *random-state* (random-state-from-platform)))

  (define (string-contains? s1 s2)
    (number? (string-contains s1 s2)))

  (define (syntax->location s)
    (let ((props (syntax-source s)))
      (if props
          (values (source-property props 'filename)
                  (+ 1 (source-property props 'line)) ; convert to 1-based
                  (source-property props 'column))
          ;; Guile doesn't store source properties for individual atoms.
          (values "unknown" 0 0)))))
