;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.
;;; Procedures for working with generic operations.

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

;;;;; Type tags

;;; Attaches a tag to the given contents, returning a tagged object.
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))

;;; Returns the type of a tagged object.
(define (type-tag object)
  (cond ((pair? object) (car object))
        ((number? object) 'scheme-number)
        (else (error "Bad tagged object: TYPE-TAG" object))))

;;; Returns the bare contents of a tagged object.
(define (contents object)
  (cond ((pair? object) (cdr object))
        ((number? object) object)
        (else (error "Bad tagged object: CONTENTS" object))))

;;;;; Generic application

;;; Applies the operation represented by op to the given arguments, using the
;;; generic map to look up the appropriate procedure. Does not coerce args.
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
      (apply proc (map contents args))
      (error "No method for these types: APPLY-GENERIC"
             (list op type-tags)))))
