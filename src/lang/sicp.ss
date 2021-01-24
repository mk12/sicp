;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang sicp)
  (export SICP Chapter Section Exercise
          define => ~> =?> =$> =!> paste
          capture-output hide-output
          cons-stream delay display eval force format fxand
          fxarithmetic-shift-left fxarithmetic-shift-right fxxor make-mutex
          newline parallel-execute quotient random read remainder runtime
          set-car! set-cdr! string-contains? string-count unless
          user-initial-environment when with-eval)
  (import (rnrs base (6))
          (only (rnrs arithmetic fixnums (6))
                fxand fxarithmetic-shift-left fxarithmetic-shift-right fxxor)
          (only (rnrs control (6)) unless when)
          (only (rnrs eval (6)) environment eval)
          (only (rnrs io simple (6)) display newline read)
          (only (rnrs mutable-pairs (6)) set-car! set-cdr!)
          (only (rnrs r5rs (6)) delay force quotient remainder)
          (src lang core)
          (src compat active))

;; Used in Section 3.5.
(define-syntax cons-stream
  (syntax-rules ()
    ((_ x y) (cons x (delay y)))))

;; Used in Chapter 4.
(define-syntax with-eval
  (syntax-rules ()
    ((_ eval env e* ...)
     (let ((evaluate eval)
           (environment env))
       (evaluate 'e* environment) ...))))

;; Used in Section 4.1.5.
(define user-initial-environment (environment '(rnrs base (6))))

;; Counts the occurrences of a character in a string.
(define (string-count char s)
  (let ((len (string-length s)))
    (let loop ((i 0) (count 0))
      (cond ((= i len) count)
            ((char=? (string-ref s i) char) (loop (+ i 1) (+ count 1)))
            (else (loop (+ i 1) count))))))

) ; end of library
