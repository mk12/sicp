;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

(module helpers ()
  ;; A tag identifies a chapter, section, subsection, or exercise. It consists
  ;; of `exercise?`, #t for exercises; and `num`, a symbol like '1.2.3. This
  ;; representation is used because chapters, sections, and subsections use
  ;; nested `num` values, but exercises are only nested within chapters, so they
  ;; can collide with section `num` values.
  (define-record-type tag
    (fields
      (immutable exercise?)
      (immutable num)))

  ;; Converts a tag to a module name, as a symbol.
  (define (tag->module-name tag)
    (string->symbol
      (string-append
        (if (tag-exercise? tag) "sicp-ex-" "sicp-")
        (symbol->string (tag-num tag)))))

  ;; Converts a syntax element in `(use ...)` to a tag.
  (define (use->tag-and-module use)
    (let* ((sym (syntax->datum use))
           (str (symbol->string sym)))
      (if (string=? "ex-" (substring str 0 3))
          (make-tag #t (string->symbol (substring str 3 (string-length str))))
          (make-tag #f sym))))

  ;; An entry stores code from a chaption, section, subsection, or exercise. It
  ;; consists of a tag, a title string (or #f), a list of tags it imports, and a
  ;; thunk -- a function that returns a module.
  (define-record-type entry
    (fields
      (immutable tag)
      (immutable title)
      (immutable imports)
      (immutable thunk)))

  ;; Global log of entries.
  (define *entries* (make-log))
  (define (add-entry! . args)
    (append-log! *entries* (apply make-entry args)))


        )

(module ((SICP assert-equal assert-close add-entry!)
         capture-output hide-output
         *entries* entry-thunk assert-equal ; TODO remove
         )

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

  ;; Asserts that all elements in `vals` are the same, according to `equal?`.
  ;; Expects `exprs` to contain syntax objects for each value in `vals`.
  (define (assert-equal vals exprs)
    (assert-on-pairs
      equal?
      (lambda (v1 v2 e1 e2)
        (format
          (string-append
            "Assertion failed!\n\n"
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
            "Assertion failed!\n\n"
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
            (syntax-error
              e1
              (make-msg v1 v2 (syntax->datum e1) (syntax->datum e2))))))
      (pairs exprs)
      (pairs vars)))

  ;; Returns a list of the adjacent pairs of elements in `xs` For example, given
  ;; the list `(a b c d)` it returns `((a . b) (b . c) (c . d))`.
  (define (pairs xs)
    (cond
      ((null? xs) '())
      ((null? (cdr xs)) '())
      (else (cons (cons (car xs) (cadr xs))
                  (pairs (cdr xs))))))

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

  ;; A DSL for SICP code samples and exercises.
  (define-syntax (SICP x)
    ;; Recursive implementation of the macro. It takes:
    ;; x       - the remaining forms to be processed
    ;; header  - the last seen chapter/section/subsection/exercise header
    ;; exports - names of definitions made since the last header
    ;; body    - code encountered since the last header
    ;; out     - accumulated result of the macro
    (define (go x header exports body out)
      (define (flush)
        (if (null? body) out #`(#,@out #,(build-entry))))
      (define (build-entry)
        (with-syntax (((exercise? num title e* ...) header))
          (let ((thunk #`(lambda () (module NAME #,exports #,body))))
            #`(#,@out (add-entry! kind num title #,thunk)))))
      (define (imports)
        (let ((uses
                (syntax-case header (use)
                  ((_ _ (use e* ...)) #'(e* ...))
                  ((_ _ _ (use e* ...)) #'(e* ...)))))
          (map
            (lambda (e)
              (syntax-case e ()
                ((ref e* ...) #'(import (only F funs)))
                (ref #'(import ref (make-tag)))))
            exprs)))
      (syntax-case x (Chapter Section Subsection Exercise define => ~>)
        (() (flush))
        (((Chapter e1* ...) e2* ...)
          (go #'(e2* ...) #'(#f e1* ...) #'() #'() (flush)))
        (((Section e1* ...) e2* ...)
          (go #'(e2* ...) #'(#f e1* ...) #'() #'() (flush)))
        (((Subsection e1* ...) e2* ...)
          (go #'(e2* ...) #'(#f e1* ...) #'() #'() (flush)))
        (((Exercise e1* ...) e2* ...)
          (go #'(e2* ...) #'(#t e1* ...) #'() #'() (flush)))
        ((e1 => e2 e* ...)
          (go=> #'(e2 e* ...) #'(e1 e2) header exports body out))
        ((e1 ~> e2 e* ...)
          (go~> #'(e2 e* ...) #'(e1 e2) header exports body out))
        (((define (name . args) e1* ...) e2* ...)
          (with-syntax (((def _* ...) x))
            (go #'(e2* ...) header #`(#,@exports name) #`(#,@body def) out)))
        (((define name e1* ...) e2* ...)
          (identifier? #'name)
          (with-syntax (((def _* ...) x))
            (go #'(e2* ...) header #`(#,@exports name) #`(#,@body def) out)))
        ((e e* ...)
          (helper #'(e* ...) header exports #`(#,@body e) out))))
    (define (go=> x terms header exports body out)
      (syntax-case x (=>)
        ((e2 => e3 e* ...)
          (go=> #'(e3 e* ...) #`(#,@terms e3) header exports body out))
        ((e2 e* ...)
          (let ((new-body
                  #`(#,@body (assert-equal (list #,@terms) #'#,terms))))
            (go #'(e* ...) header exports new-body out)))))
    (define (go~> x terms header exports body out)
      (syntax-case x (~>)
        ((e2 ~> e3 e* ...)
          (go=> #'(e3 e* ...) #`(#,@terms e3) header exports body out))
        ((e2 e* ...)
          (let ((new-body
                  #`(#,@body (assert-close (list #,@terms) #'#,terms))))
            (go #'(e* ...) header exports new-body out)))))
    (with-syntax (((_ e* ...) x))
      (go #'(e* ...) #f #f #f #'()))))
