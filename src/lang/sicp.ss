;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang sicp)
  (export SICP Chapter Section Exercise
          define => ~> =$> =!> paste
          capture-output hide-output
          cons-stream delay display force format fxand fxarithmetic-shift-left
          fxarithmetic-shift-right fxxor make-mutex newline parallel-execute
          quotient random read remainder runtime set-car! set-cdr!
          string-contains? string-count unless when)
  (import (rnrs base (6))
          (only (rnrs arithmetic fixnums (6))
                fxand fxarithmetic-shift-left fxarithmetic-shift-right fxxor)
          (only (rnrs control (6)) unless when)
          (only (rnrs io simple (6)) display newline read)
          (only (rnrs mutable-pairs (6)) set-car! set-cdr!)
          (only (rnrs r5rs (6)) delay force quotient remainder)
          (src lang core)
          (src compat active))

  ;; Used in Section 3.5.
  (define-syntax cons-stream
    (syntax-rules ()
      ((_ x y) (cons x (delay y)))))

  ;; Counts the occurrences of a character in a string.
  (define (string-count char s)
    (let ((len (string-length s)))
      (let loop ((i 0) (count 0))
        (cond ((= i len) count)
              ((char=? (string-ref s i) char) (loop (+ i 1) (+ count 1)))
              (else (loop (+ i 1) count)))))))
