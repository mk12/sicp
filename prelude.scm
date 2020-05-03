;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.
;;; Procedures available for use in all chapters.

;;;;; Syntax

(define run-slow #f)
(define-syntax slow
  (syntax-rules ()
    ((_ e* ...) (when run-slow e* ...))))

(define-syntax check
  (syntax-rules (=> ~>)
    ((_) (void))
    ((_ e1 => e2 e* ...)
      (begin
        (assert-equal e1 e2)
        (check e2 e* ...)))
    ((_ e1 ~> e2 e* ...)
      (begin
        (assert-close e1 e2)
        (check e2 e* ...)))
    ((_ e e* ...)
      (begin e (check e* ...)))))

(define-syntax assert-equal
  (syntax-rules ()
    ((_ e1 e2)
      (unless (equal? e1 e2)
        (syntax-error
          #'e1
          (format
            "assert-equal failed!\n\nleft: ~s\n => ~s\n\nright: ~s\n => ~s\n\n"
            'e1 e1 'e2 e2))))))

(define-syntax assert-close
  (syntax-rules ()
    ((_ e1 e2)
      (unless (close? e1 e2)
        (syntax-error
          #'e1
          (format
            (string-append
              "assert-close failed!\n\nleft: ~s\n => ~s\n\nright: ~s\n => ~s"
              "\n\ndelta: ~s > ~s\n\n")
            'e1 e1 'e2 e2 (abs (- e1 e2)) epsilon))))))

(define-syntax hide-output
  (syntax-rules ()
    ((_ e* ...)
      (parameterize ([current-output-port (open-output-string)]) e* ...))))

;;;;; Lists

(define (filter pred xs)
  (cond ((null? xs) '())
        ((pred (car xs))
         (cons (car xs) (filter pred (cdr xs))))
        (else (filter pred (cdr xs)))))

(define (any? pred xs)
  (and (not (null? xs))
       (or (pred (car xs))
           (any? pred (cdr xs)))))

(define (every? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (every? (cdr xs)))))

(define (remove x xs)
  (filter (lambda (y) (not (= x y))) xs))

(define (fold-right f init xs)
  (if (null? xs)
    init
    (f (car xs)
       (fold-right f init (cdr xs)))))

(define (fold-left f init xs)
  (if (null? xs)
    init
    (fold-left f (f init (car xs)) (cdr xs))))

(define (reduce f xs)
  (fold-left f (car xs) (cdr xs)))

(define (flat-map f . xss)
  (apply append (apply map f xss)))

;;;;; Math

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (range a b)
  (if (> a b)
    '()
    (cons a (range (+ a 1) b))))

(define epsilon 1e-10)
(define (close? a b)
  (< (abs (- a b)) epsilon))

(random-seed
  (+ 1 (remainder (time-second (current-time))
                  (- (expt 2 32) 2))))
(define random-integer random)
(define (random-real) (random 1.0))

;;;;; Functions

(define (identity x) x)
(define (comp f g) (lambda (x) (f (g x))))
(define (complement pred) (lambda (x) (not (pred x))))
