;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang sicp)
  (export SICP Chapter Section Subsection Exercise
          define => ~> slow=> slow~>
          capture-output hide-output
          display format remainder)
  (import (rnrs base (6))
          (only (rnrs io simple (6)) display)
          (only (rnrs r5rs (6)) remainder)
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
