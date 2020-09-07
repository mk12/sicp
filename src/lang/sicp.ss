;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang sicp)
  (export SICP Chapter Section Exercise
          define =!> => ~> slow=> slow~> paste
          capture-output capture-lines hide-output
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
  
  ;; Like `capture-output`, but splits into a list of lines.
  (define-syntax capture-lines
    (syntax-rules ()
      ((_ e* ...)
       (split-lines (capture-output e* ...)))))

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
              (else (loop (+ i 1) count))))))

  ;; Splits a string into a list of lines, using "\n" as the delimiter, not
  ;; including the delimiter in the result, and not producing empty strings for
  ;; runs of multiple newlines or for newlines at the start/end.
  (define (split-lines s)
    (let ((len (string-length s)))
      (let loop ((i len) (j len) (res '()))
        (define (push)
          (if (< i j) (cons (substring s i j) res) res))
        (cond ((zero? i) (push))
              ((char=? (string-ref s (- i 1)) #\newline)
               (loop (- i 1) (- i 1) (push)))
              (else (loop (- i 1) j res)))))))
