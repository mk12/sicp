;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat active)
  (export format load syntax->location)
  (import (rnrs base (6))
          (only (guile) load source-property syntax-source)
          (prefix (only (guile) format) guile-))

  (define (format . args)
    (apply guile-format #f args))

  (define (syntax->location s)
    (let ((props (syntax-source s)))
      (values (source-property props 'filename)
              (+ 1 (source-property props 'line)) ; convert to 1-based
              (source-property props 'column)))))
