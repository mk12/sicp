;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang sicp)
  (export SICP Chapter Section Exercise
          define => ~> slow=> slow~>
          capture-output hide-output
          display format fxand fxarithmetic-shift-left fxarithmetic-shift-right
          fxxor newline quotient random remainder runtime set-car! set-cdr!
          string-contains? string-count unless when)
  (import (rnrs base (6))
          (only (rnrs arithmetic fixnums (6))
                fxand fxarithmetic-shift-left fxarithmetic-shift-right fxxor)
          (only (rnrs control (6)) unless when)
          (only (rnrs io simple (6)) display newline)
          (only (rnrs mutable-pairs (6)) set-car! set-cdr!)
          (only (rnrs r5rs (6)) quotient remainder)
          (src lang core)
          (src compat active))

  ;; Captures standard output in a string.
  (define-syntax capture-output
    (syntax-rules ()
      ((_ e* ...)
       (patch-output (with-output-to-string (lambda () e* ...))))))

  ;; Supresses printing to standard output.
  (define-syntax hide-output
    (syntax-rules ()
      ((_ e* ...)
       (parameterize ((current-output-port (open-output-string)))
         e* ...))))
  
  ;; Counts the occurrences of a character in a string.
  (define (string-count char s)
    (let ((len (string-length s)))  
      (let loop ((i 0) (count 0))
        (cond ((= i len) count)
              ((char=? (string-ref s i) char) (loop (+ i 1) (+ count 1)))
              (else (loop (+ i 1) count)))))))
