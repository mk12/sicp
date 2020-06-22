;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src lang core)
  (export SICP Chapter Section Subsection Exercise define => ~> slow=> slow~>
          capture-output hide-output run-sicp)
  (import (rnrs (6))
          (rnrs mutable-pairs (6))
          (src compat active))

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

  ;; Global test counters, used for reporting and for setting the exit status.
  (define *passes* 0)
  (define *fails* 0)
  (define *skips* 0)

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
  ;; corresponding unevaluated forms as data (not syntax objects). Also
  ;; increments `*passes*` or `*fails*` based on the result.
  (define (assert-on-pairs pred? make-msg vals exprs)
    (for-each
      (lambda (epair vpair)
        (let ((v1 (car vpair))
              (v2 (cdr vpair))
              (e1 (car epair))
              (e2 (cdr epair)))
          (cond
            ((pred? v1 v2) (set! *passes* (+ *passes* 1)))
            (else
              (set! *fails* (+ *fails* 1))
              (let-values (((file line col) (syntax->location e1)))
                (let ((msg (make-msg
                            v1 v2 (syntax->datum e1) (syntax->datum e2))))
                  (display
                    (format "~a:~a:~a: assertion failed\n~a"
                            file line col msg))))))))
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

  ;; Global flag indicating whether to run slow tests.
  (define *slow* #f)

  ;; Returns whether to run slow tests. We need a separate function for this
  ;; because using `*slow*` in macros would indirectly export it, meaning it
  ;; would have to be immutable.
  (define (slow-enabled?) *slow*)

  ;; Increments the `*skips*` counter for a skipped slow test.
  (define (skip-slow-test)
    (set! *skips* (+ *skips* 1)))

  ;; Wraps code so that it only executes when `*slow*` is true, and otherwise
  ;; increments the `*skips*` counter.
  (define-syntax when-slow
    (syntax-rules ()
      ((_ e) (if (slow-enabled?) e (skip-slow-test)))))

  ;; An entry stores code from a part of the textbook. It consists of a unique
  ;; symbol `id`, a kind ('chapter, 'section, 'subsection, or 'exercise), a
  ;; string `num` containing a dotted chapter/section/etc. number like "1.2.3",
  ;; a title string (or #f), a list of imported names from other entries
  ;; formatted as `((id name ...) ...)`, a list of exported names, and a thunk
  ;; that takes all the imported names as one flat list of arguments and returns
  ;; the exported values in the same order as `exports`.
  (define-record-type entry
    (fields id kind num title imports exports thunk))

  ;; A queue supports constant time appending to the back and popping from the
  ;; front. It also maintains constant time access to its length.
  (define-record-type (queue make-raw-queue queue?)
    (fields (mutable length) (mutable front) (mutable back)))
  (define (make-queue)
    (make-raw-queue 0 '() '()))
  (define (queue-empty? q)
    (null? (queue-front q)))
  (define (queue-push-back! q x)
    (let ((new-pair (cons x '())))
      (queue-length-set! q (+ (queue-length q) 1))
      (cond
        ((queue-empty? q)
         (queue-front-set! q new-pair)
         (queue-back-set! q new-pair))
        (else
          (set-cdr! (queue-back q) new-pair)
          (queue-back-set! q new-pair)))))
  (define (queue-pop-front! q)
    (if (queue-empty? q)
        (error 'queue-pop-front! "empty queue")
        (let ((result (car (queue-front q))))
          (cond
            ((eq? (queue-front q) (queue-back q))
             (queue-front-set! q '())
             (queue-back-set! q '()))
            (else
              (queue-front-set! q (cdr (queue-front q)))))
          result)))

  ;; Global queue of entries. The `SICP` macro produces calls to `add-entry!`.
  (define *entries* (make-queue))
  (define (add-entry! id kind num title imports exports thunk)
    (queue-push-back! *entries*
                      (make-entry id kind num title imports exports thunk)))

  ;; Returns a hashtable from `id` to objects in `*entries*`.
  (define (entries-by-id)
    (define table (make-eq-hashtable (queue-length *entries*)))
    (for-each
      (lambda (entry)
        (hashtable-set! table (entry-id entry) entry))
      (queue-front *entries*))
    table)

  ;; Returns a hashtable of dependencies in `*entries*` from source `id` to a
  ;; list of dependency `id`s.
  (define (entries-graph)
    (define table (make-eq-hashtable (queue-length *entries*)))
    (for-each
      (lambda (entry)
        (hashtable-set! table (entry-id entry) (map car (entry-imports entry))))
      (queue-front *entries*))
    table)

  ;; Executes the code in `*entries*`. If `slow` is true, runs slow tests. If
  ;; `verbose` is true, prints more verbose information about all tests. If
  ;; `filters` is nonempty, only runs entries whose `id` starts with one of the
  ;; the items in `filters`.
  (define (run-sicp filters slow verbose)
    ;; PRINT INFO
    (write filters) (newline)
    (write slow) (newline)
    (write verbose) (newline)
    (write *entries*) (newline)
    ; (define table (entries-by-id))
    ; (define graph (entries-graph))
    ; (define sources (make-queue))
    ; (define topo (make-queue))
    ; (for-each
    ;   (lambda (entry)
    ;     (when (null? (entry-imports entry))
    ;       (queue-push-back! sources (entry-id entry))))
    ;   (queue-front *entries*))
    ; (let loop ()
    ;   (cond
    ;     ((queue-empty? sources) topo)
    ;     (else
    ;       (let (node (queue-pop-front! sources))
    ;         (queue-push-back! topo node)
    ;         (for-each
    ;           (hashtable-ref graph ))
    ;       )))

    )

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
    (auxiliary Chapter Section Subsection Exercise ~> slow=> slow~>))

  ;; A DSL for SICP code samples and exercises.
  (define-syntax SICP (lambda (x)
    ;; Use the `SICP` syntax form in `datum->syntax` calls.
    (define sicp (with-syntax (((s e* ...) x)) #'s))

    ;; Creates an entry `num` from its `id` syntax. This just chops off the
    ;; sigil used to differentiate chapters/sections/subsections from exercises
    ;; (and also used because `1.1.1` on its own is an invalid R6RS identifier).
    (define (entry-id->num id)
      (let ((str (symbol->string (syntax->datum id))))
        (substring str 1 (string-length str))))

    ;; Recursive implementation of the macro. It takes:
    ;; x       - the remaining forms to be processed
    ;; header  - the last seen chapter/section/subsection/exercise header
    ;; exports - names of definitions made since the last header
    ;; body    - code encountered since the last header
    ;; out     - accumulated result of the macro
    (define (go x header exports body out)
      (define (flush)
          (if (eq? header 'no-header)
            out
            #`(#,@out #,(build-entry))))
      (define (build-entry)
        (with-syntax (((kind id _ ...) header)
                      (((import-id import-name ...) ...) (get-uses))
                      ((export-name ...) exports))
              ;; PRINT THE THUNK
              ; (write (syntax->datum #`(lambda (import-name ... ...)
              ;   (define export-name) ...
              ;   #,@body
              ;   (list export-name ...))))
              ; (newline)(newline)
          #`(add-entry!
              'id
              'kind
              #,(entry-id->num #'id)
              #,(get-title)
              '((import-id import-name ...) ...)
              '#,exports
              (lambda (import-name ... ...)
                (define export-name) ...
                #,@body
                (list export-name ...)))))
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
      (syntax-case x (Chapter Section Subsection Exercise
                      define => ~> slow=> slow~>)
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
         (go=> #f #'(e2 e* ...) #'(e1 e2) header exports body out))
        ((e1 ~> e2 e* ...)
         (go~> #f #'(e2 e* ...) #'(e1 e2) header exports body out))
        ((e1 slow=> e2 e* ...)
         (go=> #t #'(e2 e* ...) #'(e1 e2) header exports body out))
        ((e1 slow~> e2 e* ...)
         (go~> #t #'(e2 e* ...) #'(e1 e2) header exports body out))
        (((define (name . args) e1* ...) e2* ...)
          (identifier? #'name)
          (with-syntax ((set #'(set! name (lambda args e1* ...))))
            (go #'(e2* ...) header (add-export #'name) #`(#,@body set) out)))
        (((define name e1* ...) e2* ...)
          (identifier? #'name)
          (with-syntax ((set #'(set! name e1* ...)))
            (go #'(e2* ...) header (add-export #'name) #`(#,@body set) out)))
        ((e e* ...)
          (go #'(e* ...) header exports #`(#,@body e) out))))

    ;; Helper recursive function when parsing `=>` operators.
    (define (go=> slow x terms header exports body out)
      (syntax-case x (=>)
        ((e2 => e3 e* ...)
          (go=> slow #'(e3 e* ...) #`(#,@terms e3) header exports body out))
        ((e2 e* ...)
          (let ((new-body (add-assertion slow #'assert-equal terms body)))
            (go #'(e* ...) header exports new-body out)))))

    ;; Helper recursive function when parsing `~>` operators.
    (define (go~> slow x terms header exports body out)
      (syntax-case x (~>)
        ((e2 ~> e3 e* ...)
          (go=> slow #'(e3 e* ...) #`(#,@terms e3) header exports body out))
        ((e2 e* ...)
          (let ((new-body (add-assertion slow #'assert-close terms body)))
            (go #'(e* ...) header exports new-body out)))))

    ;; Helper used by `go=>` and `go~>` to add an assertion to `body`.
    (define (add-assertion slow assert terms body)
      (let* ((code #`(#,assert (list #,@terms) #'#,terms))
             (wrapped (if slow #`(when-slow #,code) code)))
        #`(#,@body #,wrapped)))

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
