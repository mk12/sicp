;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat active)
  (export current-output-port format make-parameter open-output-string
          parameterize random runtime seed-rng string-contains? syntax->location
          with-output-to-string)
  (import (rnrs base (6))
          (only (racket base)
                current-inexact-milliseconds current-output-port current-seconds
                format make-parameter open-output-string parameterize random
                random-seed remainder path->string syntax-column syntax-line
                syntax-source)
          (only (racket string) string-contains?)
          (only (racket port)
                with-output-to-string))

  (define (runtime)
    (/ (current-inexact-milliseconds) 1e3))

  (define (seed-rng)
    (random-seed (remainder (current-seconds) (expt 2 32))))

  (define (syntax->location s)
    (values (path->string (syntax-source s))
            (syntax-line s)
            (+ 1 (syntax-column s))))) ; convert to 1-based
