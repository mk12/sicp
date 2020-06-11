;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

(module ((SICP assert-equal assert-close add-entry!)
         capture-output hide-output
         *entries* entry-thunk assert-equal ; TODO remove
         )

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

  ; (define-syntax foo
  ;   (syntax-rules ()
  ;     ((_ bar e* ...)
  ;      (bar (list e* ...) #'(e* ...)))))

  ;; Asserts that two or more expressions evaluate to the same value, where
  ;; "same" means `equal?`. It only evaluates each expression once.
  (define-syntax assert-pairs
    (lambda (x)
      (define (gen-vars id exprs)
        (map
          (lambda (e)
            (datum->syntax id (gensym)))
          exprs))
      (define (bindings vars exprs)
        (with-syntax (((v* ...) vars)
                      ((e* ...) exprs))
          #'((v* e*) ...)))
      (define (asserts vars exprs)
        (map
          (lambda (vpair epair)
            (with-syntax (((v1 . v2) vpair)
                          ((e1 . e2) epair))
              #'(unless (equal? v1 v2)
                  (syntax-error
                    #'e1
                    (format
                      (string-append
                        "assert-equal failed!"
                        "\n\nleft: ~s\n => ~s"
                        "\n\nright: ~s\n => ~s\n\n")
                      'e1 v1 'e2 v2)))))
          (pairs vars)
          (pairs exprs)))
      (syntax-case x ()
        ((a e1 e2 e* ...)
         (let* ((exprs #'(e1 e2 e* ...))
                (vars (gen-vars #'a exprs)))
           #`(let #,(bindings vars exprs)
               (assert-p #,vars #,exprs)
               #,@(asserts vars exprs)))))))

      ; (syntax-case x ()
      ;   ((_ e1 e2)
      ;   (let ((v1 e1)
      ;         (v2 e2))
      ;     (unless (equal? v1 v2)
      ;       (syntax-error
      ;         #'e1
      ;         (format
      ;           (string-append
      ;             "assert-equal failed!"
      ;             "\n\nleft: ~s\n => ~s"
      ;             "\n\nright: ~s\n => ~s\n\n")
      ;           'e1 v1 'e2 v2)))))))

  ;; Asserts that two inexact numbers are close together.
  (define-syntax assert-close
    (syntax-rules ()
      ((_ e1 e2)
       (let ((v1 e1)
             (v2 e2)
             (delta (abs (- v1 v2)))
             (max-delta 1e-10))
         (when (> delta max-delta)
           (syntax-error
             #'e1
             (format
               (string-append
                 "assert-close failed!"
                 "\n\nleft: ~s\n => ~s"
                 "\n\nright: ~s\n => ~s\n\n"
                 "\n\ndelta: ~s > ~s\n\n")
               'e1 v1 'e2 v2 delta max-delta)))))))

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

  ;; A log is an append-only list.
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

  ;; An entry stores code from a chaption, section, subsection, or exercise. The
  ;; thunk is a lambda that returns a module.
  (define-record-type entry
    (fields
      (immutable kind)
      (immutable num)
      (immutable title)
      (immutable thunk)))

  ;; Global log of entries.
  (define *entries* (make-log))
  (define (add-entry! . args)
    (append-log! *entries* (apply make-entry args)))

  ;; A DSL for SICP code samples and exercises.
  (define-syntax SICP
    (lambda (x)
      (define (finish header code)
        (if (null? code)
            #f #f))
      (define (go x header exports body out)
        (define (flush)
          (if (null? body)
              out
              #`(#,@out)
              ; TODO
              ))
        (syntax-case x (Chapter Section Subsection Exercise define => ~>)
          (() (flush))
          (((Chapter e1* ...) e2* ...)
           (go #'(e2* ...) #'('chapter e1* ...) #'() #'() (flush)))
          (((Section e1* ...) e2* ...)
           (go #'(e2* ...) #'('section e1* ...) #'() #'() (flush)))
          (((Subsection e1* ...) e2* ...)
           (go #'(e2* ...) #'('subsection e1* ...) #'() #'() (flush)))
          (((Exercise e1* ...) e2* ...)
           (go #'(e2* ...) #'('exercise e1* ...) #'() #'() (flush)))
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
           (go=> #'(e3 e* ...) #'(#,@terms e3) header exports body out))
          ((e2 e* ...)
           (let ((new-body #'(#,@body (assert-equal #,@terms))))
             (go #'(e* ...) header exports new-body out)))))
      (define (go~> x terms header exports body out)
        (syntax-case x (~>)
          ((e2 ~> e3 e* ...)
           (go=> #'(e3 e* ...) #'(#,@terms e3) header exports body out))
          ((e2 e* ...)
           (let ((new-body #'(#,@body (assert-close #,@terms))))
             (go #'(e* ...) header exports new-body out)))))

      (with-syntax (((_ e* ...) x))
        (go #'(e* ...) #f #f #f #'()))
      )))
