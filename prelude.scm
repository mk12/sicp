;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.
;;; Common code available for use in all chapters.

;;;;; Syntax

(define run-slow #f)
(define-syntax slow
  (syntax-rules ()
    ((_ e* ...) (when run-slow e* ...))))

(define-syntax scope
  (syntax-rules ()
    ((_ e* ...) ((lambda () e* ... (void))))))

(define-syntax check
  (syntax-rules (=> ~>)
    ((_) (void))
    ((_ e1 => e2 e* ...)
      (let ()
        (assert-equal e1 e2)
        (check e2 e* ...)))
    ((_ e1 ~> e2 e* ...)
      (let ()
        (assert-close e1 e2)
        (check e2 e* ...)))
    ((_ e e* ...)
      (let () e (check e* ...)))))

(define-syntax assert-equal
  (syntax-rules ()
    ((_ e1 e2)
      (let ((v1 e1)
            (v2 e2))
        (unless (equal? v1 v2)
          (syntax-error
            #'e1
            (format
              (string-append
                "assert-equal failed!"
                "\n\nleft: ~s\n => ~s"
                "\n\nright: ~s\n => ~s\n\n")
              'e1 v1 'e2 v2)))))))

(define-syntax assert-close
  (syntax-rules ()
    ((_ e1 e2)
      (let ((v1 e1)
            (v2 e2))
        (unless (close? v1 v2)
          (syntax-error
            #'e1
            (format
              (string-append
                "assert-close failed!"
                "\n\nleft: ~s\n => ~s"
                "\n\nright: ~s\n => ~s\n\n"
                "\n\ndelta: ~s > ~s\n\n")
              'e1 v1 'e2 v2 (abs (- e1 e2)) epsilon)))))))

(define-syntax capture-output
  (syntax-rules ()
    ((_ e* ...)
      (with-output-to-string (lambda () e* ...)))))

(define-syntax hide-output
  (syntax-rules ()
    ((_ e* ...)
      (parameterize ([current-output-port (open-output-string)]) e* ...))))

;;;;; Lists

(define (any? pred xs)
  (and (not (null? xs))
       (or (pred (car xs))
           (any? pred (cdr xs)))))

(define (every? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (every? (cdr xs)))))

(define (flatmap f . xss)
  (apply append (apply map f xss)))

;;;;; Math

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (average x y)
  (/ (+ x y) 2))

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

;;;;; Other

(define (identity x) x)
(define (comp f g) (lambda (x) (f (g x))))
(define (bool x) (if x #t #f))
(define (complement pred) (comp not pred))

;;;;; Generic map

;;; A global map (associative list) where generic operations are installed,
;;; indexed by operation symbol and argument types.
(define *generic-map* '())

;;; Map entry (key-value pairs) constructor and selectors.
(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

;;; Installs the item into the generic map. The item should implement the
;;; operation represented by op for the given type or type list. If such an item
;;; alredy exists, it will be overwritten.
(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k)
           (cons (make-entry k item) (cdr array)))
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! *generic-map* (put-helper (list op type) *generic-map*)))

;;; Retrieves the operation represented by op for the given type or type list
;;; from the generic map. Returns #f if such an item does not exist.
(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) *generic-map*))
