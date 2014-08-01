;;; global generic operations map
(define *generic-map* '())

;;; map entry constructor/selectors
(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

;;; map accessors
(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k)
           (cons (make-entry k item) (cdr array)))
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! *generic-map* (put-helper (list op type) *generic-map*)))
(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) *generic-map*))

;;; type tags
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))

;;; generic operation application (no coercion)
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
      (apply proc (map contents args))
      (error
        "No method for these types: APPLY-GENERIC"
        (list op type-tags)))))
