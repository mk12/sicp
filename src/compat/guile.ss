;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat active)
  (export current-output-port extended-define-syntax format make-mutex
          open-output-string parallel-execute parameterize random runtime
          seed-rng string-contains? syntax->location with-output-to-string)
  (import (rnrs base (6))
          (only (guile)
                *random-state* current-output-port gettimeofday
                open-output-string parameterize random
                random-state-from-platform source-property string-contains
                syntax-source with-output-to-string usleep)
          (only (ice-9 threads)
                call-with-new-thread join-thread lock-mutex unlock-mutex)
          (system syntax)
          (prefix (only (guile) format) guile-)
          (prefix (only (ice-9 threads) make-mutex) guile-))

;; Guile does not support the `(define-syntax (foo x) ...)` syntax.
(define-syntax extended-define-syntax
  (syntax-rules ()
    ((_ (name x) e* ...) (define-syntax name (lambda (x) e* ...)))
    ((_ e* ...) (define-syntax e* ...))))

(define (syntax->location s)
  (let* ((s (if (pair? s) (car s) s))
         (props (syntax-sourcev s)))
    (if props
        (values (vector-ref props 0)
                (vector-ref props 1) ; convert to 1-based
                (vector-ref props 2))
        ;; Older versions of Guile don't store source properties for individual
        ;; atoms.
        (values "unknown" 0 0))))

(define (format . args)
  (apply guile-format #f args))

(define (runtime)
  (let ((t (gettimeofday)))
    (+ (car t) (/ (cdr t) 1e6))))

(define (seed-rng)
  (set! *random-state* (random-state-from-platform)))

(define (string-contains? s1 s2)
  (number? (string-contains s1 s2)))

(define (make-mutex)
  (let ((mutex (guile-make-mutex)))
    (lambda (op)
      (cond ((eq? op 'acquire) (lock-mutex mutex))
            ((eq? op 'release) (unlock-mutex mutex))
            (else (error 'make-mutex "unknown operation" op))))))

(define (parallel-execute . thunks)
  (define (spawn proc)
    (call-with-new-thread
     (lambda ()
       ;; Sleep for up to 1ms to ensure nondeterminism shows up.
       (usleep (random 1000))
       (proc))))
  (for-each join-thread (map spawn thunks)))

) ; end of library
