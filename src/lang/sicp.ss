;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang sicp)
  (export SICP Chapter Section Subsection Exercise define => ~> slow=> slow~>
          capture-output hide-output)
  (import (rnrs base (6))
          (src lang core)
          (src compat active))

  ;; Captures standard output in a string.
  (define-syntax capture-output
    (syntax-rules ()
      ((_ e* ...)
       (with-output-to-string (lambda () e* ...)))))

  ;; Supresses printing to standard output.
  (define-syntax hide-output
    (syntax-rules ()
      ((_ e* ...)
       (parameterize ((current-output-port (open-output-string)))
         e* ...)))))
