;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat impl)
  (export format load syntax->location)
  (import (rnrs base (6))
          (only (racket base)
                current-namespace format make-base-namespace parameterize
                path->string syntax-column syntax-line syntax-source)
          (prefix (only (racket base) load) racket-))

  (define (load name)
    (parameterize ([current-namespace (make-base-namespace)])
      (racket-load name)))

  (define (syntax->location s)
    (values (path->string (syntax-source s))
            (syntax-line s)
            (syntax-column s))))
