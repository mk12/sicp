;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat active)
  (export current-output-port format open-output-string parameterize
          syntax->location with-output-to-string)
  (import (rnrs base (6))
          (only (guile)
                current-output-port open-output-string parameterize
                source-property syntax-source with-output-to-string)
          (prefix (only (guile) format) guile-))

  (define (format . args)
    (apply guile-format #f args))

  (define (syntax->location s)
    (let ((props (syntax-source s)))
      (if props
          (values (source-property props 'filename)
                  (+ 1 (source-property props 'line)) ; convert to 1-based
                  (source-property props 'column))
          ;; Guile doesn't store source properties for individual atoms.
          (values "unknown" 0 0)))))
