;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src compat active)
  (export current-output-port extended-define-syntax format make-mutex
          open-output-string parallel-execute parameterize random
          run-with-short-timeout runtime seed-rng string-contains?
          syntax->location with-output-to-string)
  (import (for (rnrs base (6)) run expand)
          (only (racket base)
                current-inexact-milliseconds current-output-port current-seconds
                format kill-thread make-semaphore open-output-string
                parameterize random random-seed remainder path->string
                print-mpair-curly-braces semaphore-post semaphore-wait sleep
                syntax-column syntax-line syntax-source thread thread-running?
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
       (sleep (random 0.001))
       (proc))))
  (for-each thread-wait (map spawn thunks)))

(define (run-with-short-timeout thunk)
  (let* ((result '())
         (thd (thread (lambda () (set! result (list (thunk)))))))
    (sleep 0.001)
    (kill-thread thd)
    result))

;; Racket prints mutable pairs with braces instead of parens by default. We
;; disable this to be consistent with other Scheme implementations. This is
;; especially important for `=$>` tests that call `display` on lists.
(print-mpair-curly-braces #f)

) ; end of library
