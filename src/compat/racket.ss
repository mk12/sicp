;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat active)
  (export current-output-port format open-output-string parameterize
          syntax->location with-output-to-string)
  (import (rnrs base (6))
          (only (racket base)
                current-output-port format open-output-string parameterize
                path->string syntax-column syntax-line syntax-source)
          (only (racket port)
                with-output-to-string))

  (define (syntax->location s)
    (values (path->string (syntax-source s))
            (syntax-line s)
            (+ 1 (syntax-column s))))) ; convert to 1-based
