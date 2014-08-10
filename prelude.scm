;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.
;;; Basic functional procedures for Schemes lacking them.

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
  (filter (lambda (y) (not (= x y))) seq))

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

(define (mappend f . xss)
  (apply append (apply map f xss)))

;;;;; Math

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (range a b)
  (if (> a b)
    '()
    (cons a (range (+ a 1) b))))

(define (random high)
  (if (integer? high)
    (random-integer high)
    (* high (random-real))))

;;;; Other

(define (identity x) x)
(define (comp f g) (lambda (x) (f (g x))))
(define (complement pred) (lambda (x) (not (pred x))))
