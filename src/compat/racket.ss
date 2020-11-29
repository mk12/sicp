;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat active)
  (export current-output-port extended-define-syntax format make-mutex
          open-output-string parallel-execute parameterize patch-output random
          runtime seed-rng string-contains? syntax->location
          with-output-to-string)
  (import (for (rnrs base (6)) run expand)
          (only (racket base)
                current-inexact-milliseconds current-output-port current-seconds
                format make-semaphore open-output-string parameterize random
                random-seed remainder path->string semaphore-post semaphore-wait
                sleep syntax-column syntax-line syntax-source thread
                thread-wait)
          (only (racket string) string-contains? string-replace)
          (only (racket port) with-output-to-string))

;; Racket has the `(define-syntax (foo x) ...)` syntax, but not in R6RS mode.
(define-syntax extended-define-syntax
  (syntax-rules ()
    ((_ (name x) e* ...) (define-syntax name (lambda (x) e* ...)))
    ((_ e* ...) (define-syntax e* ...))))

(define (syntax->location s)
  (values (path->string (syntax-source s))
          (syntax-line s)
          (+ 1 (syntax-column s)))) ; convert to 1-based

;; HACK: Racket's r6rs mode uses it's mutable pair library for `cons`, `car`,
;; and `cdr` (which are normally named `mcons`, `mcar`, and `mcdr`). This leaks
;; when writing lists to strings, because Racket uses braces instead of parens
;; for mutable lists. To make tests work for all implementations, we patch the
;; output only for Racket, and hope there are no examples that actually use the
;; characters "{" and "}".
(define (patch-output s)
  (string-replace (string-replace s "{" "(") "}" ")"))

(define (runtime)
  (/ (current-inexact-milliseconds) 1e3))

(define (seed-rng)
  (random-seed (remainder (current-seconds) (expt 2 32))))

;; Racket does not have mutexes, so we implement them in terms of semaphores.
(define (make-mutex)
  (let ((sem (make-semaphore 1)))
    (lambda (op)
      (cond ((eq? op 'acquire) (semaphore-wait sem))
            ((eq? op 'release) (semaphore-post sem))
            (else (error 'make-mutex "unknown operation" op))))))

(define (parallel-execute . thunks)
  (define (spawn proc)
    (thread
     (lambda ()
       ;; Sleep for up to 1ms to ensure nondeterminism shows up.
       (sleep (* 0.001 (random)))
       (proc))))
  (for-each thread-wait (map spawn thunks)))

) ; end of library
