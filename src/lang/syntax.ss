;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang syntax)
  (export SICP capture-output hide-output
          *entries* ; TODO remove
          )
  (import (rnrs base (6))
          (rnrs control (6))
          (rnrs io simple (6))
          (rnrs mutable-pairs (6))
          (rnrs records syntactic (6))
          (rnrs syntax-case (6))
          (src compat impl))

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
          e* ...))))

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
                  (pairs (cdr xs))))))

  ;; An entry stores code from a part of the textbook. It consists of a unique
  ;; symbol identifier, a kind ('chapter, 'section, 'subsection, or 'exercise),
  ;; a title string (or #f), a list of imported `id`s, and a thunk -- a function
  ;; that returns a library named `id`.
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

  ;; Global log of entries. The `SICP` macro produces calls to `add-entry!`.
  (define *entries* (make-log))
  (define (add-entry! id kind num title imports thunk)
    (append-log! *entries*
                 (make-entry id kind num title imports thunk)))

  ;; A DSL for SICP code samples and exercises.
  (define-syntax SICP (lambda (x)
    ;; Use the `SICP` syntax form in `datum->syntax` calls.
    (define sicp (with-syntax (((s e* ...) x)) #'s))

    ;; Creates an entry id from its kind and num.
    (define (kind-and-num->id kind num)
      (let* ((num-datum (syntax->datum num))
             (num-str ((if (number? num-datum) number->string symbol->string)
                       num-datum)))
        (datum->syntax sicp
          (case (syntax->datum kind)
            ((exercise)
             (string->symbol (string-append "sicp-ex-" num-str)))
            (else
              (string->symbol (string-append "sicp-" num-str)))))))

    ;; Converts a reference in the `(use ...)` syntax to an entry id. It assumes
    ;; that everything has unique numbers (e.g. 1 is a chapter, 1.2 is a section,
    ;; 1.2.3 is a subsection). The exception is exercises -- they can clash with
    ;; sections, so they use a different format: ex-1.2 means exercise 1.2.
    (define (use->id use)
      (let ((str (symbol->string (syntax->datum use))))
        (datum->syntax sicp
          (string->symbol
            (if (string=? "ex-" (substring str 0 3))
              (string-append "sicp-ex-" (substring str 3 (string-length str)))
              (string-append "sicp-" str))))))

    ;; Recursive implementation of the macro. It takes:
    ;; x       - the remaining forms to be processed
    ;; header  - the last seen chapter/section/subsection/exercise header
    ;; exports - names of definitions made since the last header
    ;; body    - code encountered since the last header
    ;; out     - accumulated result of the macro
    (define (go x header exports body out)
      (define (flush)
        ; #`(write '#,body))
        ; #'(display "hi\n"))
        (syntax-case body ()
          (() out)
          (_ #`(#,@out #,(build-entry))))
        ; (if (null? body) out #`(#,@out #,(build-entry)))
        )
      (define (build-entry)
        (with-syntax (((kind num title e* ...) header))
          (let-values (((import-ids imports) (build-imports)))
            (let* ((id (kind-and-num->id #'kind #'num))
                   ; (thunk #`(lambda ()
                   ;            (library (#,id)
                   ;              (export #,@exports)
                   ;              (import #,@imports)
                   ;              #,body)))
                   )
              #`(#,@out
                 (add-entry! '#,id 'kind 'num title '#,import-ids
                             ;#,thunk
                             #f
                             ))))))
      (define (build-imports)
        (let ((uses (syntax-case header (use)
                      ((_ _ (use e* ...)) #'(e* ...))
                      ((_ _ _ (use e* ...)) #'(e* ...))
                      (_ '()))))
          (values
            (map
              (lambda (e)
                (syntax-case e ()
                  ((ref e* ...) (use->id #'ref))
                  (ref (use->id #'ref))))
              uses)
            (map
              (lambda (e)
                (syntax-case e ()
                  ((ref e* ...) #`(only (#,(use->id #'ref)) e* ...))
                  (ref #`(#,(use->id #'ref)))))
              uses))))
      (syntax-case x (Chapter Section Subsection Exercise define => ~>)
        (() (flush))
        (((Chapter e1* ...) e2* ...)
         ; #'(display "chapter\n"))
          (go #'(e2* ...) #'(chapter e1* ...) #'() #'() (flush)))
        (((Section e1* ...) e2* ...)
          (go #'(e2* ...) #'(section e1* ...) #'() #'() (flush)))
        (((Subsection e1* ...) e2* ...)
          (go #'(e2* ...) #'(subsection e1* ...) #'() #'() (flush)))
        (((Exercise e1* ...) e2* ...)
          (go #'(e2* ...) #'(exercise e1* ...) #'() #'() (flush)))
        ((e1 => e2 e* ...)
          (go=> #'(e2 e* ...) #'(e1 e2) header exports body out))
        ((e1 ~> e2 e* ...)
          (go~> #'(e2 e* ...) #'(e1 e2) header exports body out))
        (((define (name . args) e1* ...) e2* ...)
          (identifier? #'name)
          (with-syntax (((def _* ...) x))
            (go #'(e2* ...) header #`(#,@exports name) #`(#,@body def) out)))
        (((define name e1* ...) e2* ...)
          (identifier? #'name)
          (with-syntax (((def _* ...) x))
            (go #'(e2* ...) header #`(#,@exports name) #`(#,@body def) out)))
        ((e e* ...)
         ; #'(write 'e)
          (go #'(e* ...) header exports #`(#,@body e) out)
          )))

    ;; Helper recursive function when parsing `=>` operators.
    (define (go=> x terms header exports body out)
      (syntax-case x (=>)
        ((e2 => e3 e* ...)
          (go=> #'(e3 e* ...) #`(#,@terms e3) header exports body out))
        ((e2 e* ...)
          (let ((new-body
                  #`(#,@body (assert-equal (list #,@terms) #'#,terms))))
            (go #'(e* ...) header exports new-body out)))))

    ;; Helper recursive function when parsing `~>` operators.
    (define (go~> x terms header exports body out)
      (syntax-case x (~>)
        ((e2 ~> e3 e* ...)
          (go=> #'(e3 e* ...) #`(#,@terms e3) header exports body out))
        ((e2 e* ...)
          (let ((new-body
                  #`(#,@body (assert-close (list #,@terms) #'#,terms))))
            (go #'(e* ...) header exports new-body out)))))

    (with-syntax (((_ e* ...) x))
      ; #`(write '(e* ...))
      #`(begin #,@(go #'(e* ...) 'no-header #'() #'() #'()))
      ))))
