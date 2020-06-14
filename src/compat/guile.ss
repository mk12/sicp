;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat impl)
  (export format)
  (import (rnrs base (6))
          (rename (only (guile) format) (format guile-format)))

  (define (format . args)
    (apply guile-format #f args)))
