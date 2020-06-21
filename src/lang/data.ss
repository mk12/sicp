;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang types)
  (export add-entry!)
  (import (rnrs base (6))
          (rnrs records syntactic (6)))

  ;; An entry stores code from a chaption, section, subsection, or exercise. It
  ;; consists of a unique symbol id, a kind ('chapter, 'section, etc.), a title
  ;; string (or #f), a list of imports by id, and a thunk -- a function that
  ;; returns a library named id.
  (define-record-type entry
    (fields id kind num title imports thunk))

  ;; A log is an append-only list. It maintains a pointer to the end so that
  ;; appending an element is a constant time operation.
  (define (make-log)
    (cons '() '()))
  (define (empty-log? d)
    (null? (car d)))
  (define (append-log! d x)
    (let ((new-pair (cons x '())))
      (cond
        ((empty-log? d)
         (set-car! d new-pair)
         (set-cdr! d new-pair))
        (else
          (set-cdr! (cdr d) new-pair)
          (set-cdr! d new-pair)))))

  ;; Global log of entries. The SICP macro adds to it.
  (define *entries* (make-log))
  (define (add-entry! ex? num title imports thunk)
    (append-log! *entries*
                 (make-entry (make-tag ex? num) title imports thunk)))

  ;; also routines for topo-sorting
  )
