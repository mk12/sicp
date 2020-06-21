;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang syntax)
  (export SICP Chapter Section Subsection Exercise define => ~>
          capture-output hide-output
          get-entries ; remove
          )
  (import (rnrs (6))
          (rnrs mutable-pairs (6))
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
  ;; symbol `id`, a kind ('chapter, 'section, 'subsection, or 'exercise), a
  ;; string `num` containing a dotted chapter/section/etc. number like "1.2.3",
  ;; a title string (or #f), a list of imported names from other entries
  ;; formatted as `((id name ...) ...)`, a list of exported names, and a thunk
  ;; that takes all the imported names as one flat list of arguments and returns
  ;; the exported values in the same order as `exports`.
  (define-record-type entry
    (fields id kind num title imports exports thunk))

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
  (define (add-entry! id kind num title imports exports thunk)
    (append-log! *entries*
                 (make-entry id kind num title imports exports thunk)))

  ;; TODO: REMOVE
  (define (get-entries) (car *entries*))

  ;; In order for `SICP` to match on auxiliary keywords, we must export them.
  ;; That implies giving them definitions. We don't need to do this for `define`
  ;; and `=>` because we instead re-export the rnrs definitions (the latter is
  ;; already an auxiliary keyword used in `cond`).
  (let-syntax
    ((auxiliary
       (syntax-rules ()
         ((_ lit ...)
          (begin
            (define-syntax lit
              (lambda (x)
                (syntax-violation #f "incorrect usage of auxiliary keyword" x)))
            ...)))))
    (auxiliary Chapter Section Subsection Exercise ~>))

  ;; A DSL for SICP code samples and exercises.
  (define-syntax SICP (lambda (x)
    ;; Use the `SICP` syntax form in `datum->syntax` calls.
    (define sicp (with-syntax (((s e* ...) x)) #'s))

    ;; Converts a syntax object, assumed to be an identifier, to a string.
    (define (syntax->string s)
      (let ((datum (syntax->datum s)))
        (cond
          ((symbol? datum) (symbol->string datum))
          (else (error 'syntax->string "not a symbol" datum)))))

    ;; Creates an entry id from its kind and num.
    (define (kind-and-num->id kind num)
      (let ((num-str (syntax->string num)))
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
      (let ((str (syntax->string use)))
        (datum->syntax sicp
          (string->symbol
            (if (and (>= (string-length str) 3)
                     (string=? "ex-" (substring str 0 3)))
              (string-append "sicp-ex-" (substring str 3 (string-length str)))
              (string-append "sicp-" str))))))

    ;; Converts the `(use ...)` syntax to a list of parameters for `thunk`.
    (define (build-params uses)
      (with-syntax ((((ref name ...) ...) uses))
        #'(name ... ...)))

    ;; Like `map`, but works better on lists of syntax. In particular, Racket
    ;; reports a contract violation when mapping over #'() with `map`.
    (define (smap f xs)
      (syntax-case xs ()
        (() #'())
        ((e e* ...) (cons (f #'e) (smap f #'(e* ...))))))

    ;; Converts the `(use ...)` syntax to `imports` for the entry.
    (define (build-imports uses)
      (smap (lambda (e)
              (with-syntax (((ref name ...) e))
                #`(#,(use->id #'ref) name ...)))
            uses))

    ;; Recursive implementation of the macro. It takes:
    ;; x       - the remaining forms to be processed
    ;; header  - the last seen chapter/section/subsection/exercise header
    ;; exports - names of definitions made since the last header
    ;; body    - code encountered since the last header
    ;; out     - accumulated result of the macro
    (define (go x header exports body out)
      (define (flush)
        ; #`(write '#,body)
        ; #'(display "hi\n"))
        ; (syntax-case body ()
          ; (() out)
          ; (_ 
          (if (eq? header 'no-header)
            out
            #`(#,@out #,(build-entry))))
      ;)
        ; (if (null? body) out #`(#,@out #,(build-entry)))
        ; )
      (define (build-entry)
        (with-syntax (((kind num _ ...) header))
          (let ((uses (get-uses)))
            #`(add-entry!
                '#,(kind-and-num->id #'kind #'num)
                'kind
                'num
                #,(get-title)
                '#,(build-imports uses)
                '#,exports
                (lambda #,(build-params uses)
                  #,@(smap (lambda (name) #`(define #,name)) exports)
                  #,@body
                  (list #,@exports))))))
      (define (get-title)
        (syntax-case header ()
          ((_ _ title _ ...) (string? (syntax->datum #'title)) #'title)
          (_ #'#f)))
      (define (get-uses)
        (syntax-case header (use)
          ((_ _ (use e* ...)) #'(e* ...))
          ((_ _ _ (use e* ...)) #'(e* ...))
          (_ '())))
      (define (add-export name)
        (if (memq (syntax->datum name)
                  (syntax->datum exports))
          exports
          #`(#,@exports #,name)))
      (syntax-case x (Chapter Section Subsection Exercise define => ~>)
        (() (flush))
        (((Chapter e1* ...) e2* ...)
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
          (with-syntax ((set #'(set! name (lambda args e1* ...))))
            (go #'(e2* ...) header (add-export #'name) #`(#,@body set) out)))
        (((define name e1* ...) e2* ...)
          (identifier? #'name)
          (with-syntax ((set #'(set! name e1* ...)))
            (go #'(e2* ...) header (add-export #'name) #`(#,@body set) out)))
        ((e e* ...)
         ; #'(write 'e)
         ; (begin (write #'(e e* ...)) (newline) (newline)
          (go #'(e* ...) header exports #`(#,@body e) out)
          ; )
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
      ;; PRINT INPUT
      ; #`(write '(e* ...))

      ;; PRINT MACRO OUTPUT
      ; #`(write '(begin #,@(go #'(e* ...) 'no-header #'() #'() #'())))

      ;; PRINT *entries*
      ; #`(begin #,@(go #'(e* ...) 'no-header #'() #'() #'()) (write *entries*))

      ;; NORMAL
      #`(begin #,@(go #'(e* ...) 'no-header #'() #'() #'()))
      ))))
