;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang assert)
  (export *success* assert-equal assert-close)
  (import (rnrs base (6)))

  ;; Global success value, set by failed tests to #f. Used for exit status.
  (define *success* #t)

  ;; Asserts that all elements in `vals` are the same, according to `equal?`.
  ;; Expects `exprs` to contain syntax objects for each value in `vals`.
  (define (assert-equal vals exprs)
    (assert-on-pairs
      equal?
      (lambda (v1 v2 e1 e2)
        (format
          (string-append
            "left: ~s\n => ~s\n\n"
            "right: ~s\n => ~s\n\n")
          e1 v1 e2 v2))
      vals
      exprs))

  ;; Asserts that each number in `vals` is very close to the previous one.
  ;; Expects `exprs` to contain syntax objects for each value in `vals`.
  (define (assert-close vals exprs)
    (define (delta x y) (abs (- x y)))
    (define max-delta 1e-10)
    (assert-on-pairs
      (lambda (v1 v2)
        (<= (delta v1 v2) max-delta))
      (lambda (v1 v2 e1 e2)
        (format
          (string-append
            "left: ~s\n => ~s\n\n"
            "right: ~s\n => ~s\n\n"
            "delta: ~s > ~s\n\n")
          e1 v1 e2 v2 (delta v1 v2) max-delta))
      vals
      exprs))

  ;; Asserts that `pred?` holds for each pair of adjacent elements in `vals`.
  ;; Expects `exprs` to contain syntax objects for each value in `vals`. If
  ;; `(pred? v1 v2)` is false, the error message includes the `v1` line number
  ;; and the string `(make-msg v1 v2 d1 d2)`, where `d1` and `d2` are the
  ;; corresponding unevaluated forms as data (not syntax objects).
  (define (assert-on-pairs pred? make-msg vals exprs)
    (for-each
      (lambda (epair vpair)
        (let ((v1 (car vpair))
              (v2 (cdr vpair))
              (e1 (car epair))
              (e2 (cdr epair)))
          (unless (pred? v1 v2)
            (set! *success* #f)
            (let-values (((file line col) (syntax->location e1)))
              (let ((msg (make-msg
                           v1 v2 (syntax->datum e1) (syntax->datum e2))))
                (display
                  (format "~a:~a:~a: assertion failed\n~a"
                          file line col msg)))))))
      (pairs exprs)
      (pairs vals)))

  ;; Returns a list of the adjacent pairs of elements in `xs` For example, given
  ;; the list `(a b c d)` it returns `((a . b) (b . c) (c . d))`.
  (define (pairs xs)
    (cond
      ((null? xs) '())
      ((null? (cdr xs)) '())
      (else (cons (cons (car xs) (cadr xs))
                  (pairs (cdr xs)))))))
