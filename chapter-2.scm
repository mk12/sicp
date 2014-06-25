;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.
;;; Structure and Interpretation of Computer Programs
;;; Chapter 2: Building Abstractions with Data

;;;;; Section 2.1: Introduction to data abstraction

;;; example 2.1.1 (rational ops)
(define numer car)
(define denom cdr)
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;;; ex 2.1
(define (sgn x)
  (cond ((positive? x) 1)
        ((zero? x) 0)
        ((negative? x) -1)))
(define (make-rat n d)
  (let ((g (gcd n d))
        (s (* (sgn n) (sgn d))))
    (cons (* s (/ (abs n) g))
          (/ (abs d) g))))
(make-rat 5 10)   ; => (1 . 2)
(make-rat -5 10)  ; => (-1 . 2)
(make-rat -5 -10) ; => (1 . 2)
(make-rat 5 -10)  ; => (-1 . 2)

;;; ex 2.2
(define make-point cons)
(define x-point car)
(define y-point cdr)
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
(define (midpoint-segment seg)
  (let ((a (start-segment seg))
        (b (end-segment seg)))
    (make-point
      (/ (+ (x-point a) (x-point b)) 2)
      (/ (+ (y-point a) (y-point b)) 2))))
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(midpoint-segment
  (make-segment (make-point 6 5)
                (make-point 12 13)))
;; => (9 . 9)

;;; ex 2.3
;; two corners
(define make-rect cons)
(define p1-rect car)
(define p2-rect cdr)
(define (width-rect rect)
  (abs (- (x-point (p1-rect rect))
          (x-point (p2-rect rect)))))
(define (height-rect rect)
  (abs (- (y-point (p1-rect rect))
          (y-point (p2-rect rect)))))
;; point and width & height
(define (make-rect p w h)
  (cons p (cons w h)))
(define point-rect car)
(define (width-rect rect)
  (car (cdr rect)))
(define (height-rect rect)
  (cdr (cdr rect)))
;; perimeter and area
(define (perimiter rect)
  (* 2 (+ (width-rect rect)
          (height-rect rect))))
(define (area rect)
  (* (width-rect rect)
     (height-rect rect)))

;;; ex 2.4
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (x y) x)))
(define (cdr z)
  (z (lambda (x y) y)))

;;; ex 2.5
;; Due to the fundamental theorem of arithmetic, 2^a*3^b will always produce a
;; unique product given a unique pair of integers a and b.
(define (cons x y)
  (* (expt 2 x) (expt 3 y)))
(define (count-divides a b)
  (define (count a n)
    (let ((q (/ a b)))
      (if (integer? q)
        (count q (+ n 1))
        n)))
  (count a 0))
(define (car z)
  (count-divides z 2))
(define (cdr z)
  (count-divides z 3))
(car (cons 7 12)) ; => 7
(cdr (cons 7 12)) ; => 12

;;; ex 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (inc n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (+ a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))
(define (count n)
  ((n (lambda (x) (+ x 1))) 0))
(count zero)         ; => 0
(count one)          ; => 1
(count two)          ; => 2
(count (inc two))    ; => 3
(coint (+ one zero)) ; => 1
(coint (+ zero two)) ; => 2
(coint (+ one two))  ; => 3
(coint (+ two two))  ; => 4

;;; extended ex 2.1.4 (interval arithmetic)
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))

;;; ex 2.7
(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

;;; ex 2.8
;; The minimum value the difference could be is the difference of the lower
;; bound of the minuend and the upper bound of the subtrahend. The maximum
;; value would be the difference of the upper bound of the minuend and the
;; lower bound of the subtrahend.
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;;; ex 2.9
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
;; The width of a sum or difference of two intervals is a functin only of the
;; widths of the intervals being added or subtracted. Given intervals x and y,
(define x (make-interval x1 x2))
(define y (make-interval y1 y2))
(= (width x) (/ (- x2 x1) 2))
(= (width y) (/ (- y2 y1) 2))
;; we can calculate the width of the sum:
(width (add-interval x y))
(width (make-interval (+ x1 y1) (+ x2 y2)))
(/ (- (+ x2 y2) (+ x1 y1)) 2)
(/ (+ (- x2 x1) (- y2 y1)) 2)
(+ (/ (- x2 x1) 2) (/ (- y2 y1) 2))
(+ (width x) (width y))
;; The width of the sum is the sum of the widths. This also applies for
;; subtraction because (- x y) is the same as (+ x z) where z is -y. On the
;; other hand, the width of a product or quotient is not a function only of the
;; widths of the intervals being multiplied or divided. A counterexample:
(define x (make-interval 0 10)) ; => width 5
(define y (make-interval 0 2))  ; => width 1
(add-interval x y)              ; => width 10
;; The same input widths, 5 and 1, can produce a different product width,
;; therefore the product width is not a function of only the input widths:
(define x (make-interval -5 5)) ; => width 5
(define y (make-interval -1 1)) ; => width 1
(add-interval x y)              ; => width 5
;; This also applies to divison since any division can be restated as a
;; multiplication problem: (/ x y) becomes (* x (/ y)).

;;; ex 2.10
(define (div-interval x y)
  (let ((y1 (lower-bound y))
        (y2 (upper-bound y)))
    (if (<= y1 0 y2)
      (error "Can't divide by an interval spanning zero.")
      (mul-interval
        x
        (make-interval (/ y2) (/ y1))))))

;;; ex 2.11
(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond
      ((> x1 0)
       (cond
         ((> y1 0) (make-interval (* x1 y1) (* x2 y2)))
         ((< y2 0) (make-interval (* x2 y1) (* x1 y2)))
         (else (make-interval (* x2 y1) (* x2 y2))))
      ((< x2 0)
       (cond
         ((> y1 0) (make-interval (* x1 y2) (* x2 y1)))
         ((< y2 0) (make-interval (* x2 y2) (x1 y1)))
         (else (make-interval (* x1 y2) (* x1 y1)))))
      (else
       (cond
         ((> y1 0) (make-interval (* x1 y2) (* x2 y2)))
         ((< y2 0) (make-interval (* x2 y1) (* x1 y1)))
         (else (make-interval (min (* x1 y2) (x2 y1))
                              (max (* x1 y1) (x2 y2))))))))))

;;; ex 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center x)
  (average (lower-bound x) (upper-bound x)))
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))
(define (percent x)
  (* 100 (/ (width x) (center x))))

;;; ex 2.13
;; Under the assumption of small percent tolerances, there is a simple formula
;; for the approximate percent tolerance of the product of two intervals in
;; terms of the tolerances of the factors: it is their sum. Assuming all numbers
;; are positive, we have
;;     [a,b] × [c,d] = [ac,bd].
;; We can define a single interval in terms of its centre and percentage:
;;     i = [c-cp/100,c+cp/100].
;; Factoring out the centre gives us
;;     i = [c(1-p/100),c(1+p/100)],
;; And multiplying two intervals can now be written as
;;     ij = [ci*cj(1-pi/100)(1-pj/100),ci*cj(1+pi/100)(1+pj/100)].
;; We can expand the bracketed factors:
;;       (1-pi/100)(1-pj/100)
;;     = 1 - pi/100 - pj/100 + pi*pj/10000
;;     = 1 - (pi+pj)/100 + pi*pj/10000.
;;       (1+pi/100)(1+pj/100)
;;     = 1 + pi/100 + pj/100 + pi*pj/10000
;;     = 1 + (pi+pj)/100 + pi*pj/10000.
;; We can substitute this back into the interval multiplication:
;;     ij = [ci*cj(1-(pi+pj)/100+pi*pj/10000),ci*cj(1+(pi+pj)/100+pi*pj/10000)].
;; Since pi and pj are small, we can drop the pi*pj/10000 terms:
;;     ij = [ci*cj(1-(pi+pj)/100),ci*cj(1+(pi+pj)/100)].
;; This is back into centre & percent form. The centre of the product interval
;; is ci*cj, and its percentage uncertainty is pi + pj -- the sum. We can try it
;; out to make sure:
(define i (make-center-percent 30 1))
(define j (make-center-percent 25 3))
(define i*j (mul-interval i j))
(+ (percent i) (percent j))    ; => 4
(exact->inexact (percent i*j)) ; => 3.9988003598920323

;;; ex 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one
      (add-interval (div-interval one r1)
                    (div-interval one r2)))))
;; Lem is right. The uncertainty of the result is different for mathematically
;; equivalent expressions calculated by par1 and par2:
(define r1 (make-center-percent 10000 5))
(define r2 (make-center-percent 330 10))
(percent (par1 r1 r2)) ; => 19.931607019958708
(percent (par2 r1 r2)) ; => 9.841433938087881
;; When we divide an interval by itself, we should get exactly one. Instead, we
;; get an interval whose center is an approximation of one, and it still has a
;; fair amount of uncertainty.
(define i (make-center-percent 5000 2))
(define j (make-center-percent 2500 1))
(center (div-interval i i))  ; => 1.000800320128051 (should be 1)
(percent (div-interval i i)) ; => 3.998400639744109 (should be 0%)
(center (div-interval i j))  ; => 2.000600060006000 (correct)
(percent (div-interval i j)) ; => 2.999400119975999 (correct)

;;; ex 2.15
;; Yes, Eva is right. The interval calculation system produces different
;; intervals for mathematically equivalent expressions. When the expressions are
;; written in such a form that no uncertain variable is repeated, the
;; uncertainty of the result is smaller, and this is the more correct value.
;; This is because, when an uncertain variable is repeated, the interval
;; arithmetic procedures have no way of knowing that they are dealing with the
;; same value twice. They introduce uncertainty in multiple places as if the
;; repetitions were separate measurements. For example, If we manipulate an
;; algebraic expression by dividing a value by itself, we introduce error
;; because the interval arithmetic division does not produce exactly one.

;;; ex 2.16
;; In general, equivalent expressions may lead to different answers because
;; identical intervals are treated indepedently even if they represent the same
;; measurement. This is called the _dependency problem_. For complicated
;; functions, it is not always possible to eliminate repetitions of an interval
;; in the expression, so there is an unwanted expansion in the resulting
;; intervals. It is not possible to write an interval arithmetic package that
;; does not have this shortcoming. The best we can do is attempt to rewrite
;; expressions so that intervals are not repeated (not always possible).

;;;;; Section 2.2: Hierarchical data the closure property

;;; ex 2.17
(define (last-pair xs)
  (if (null? (cdr xs))
    xs
    (last-pair (cdr xs))))
(last-pair (list 23 72 149 34)) ; => (34)

;;; ex 2.18
(define (reverse-rec xs)
  (if (null? xs)
    xs
    (append (reverse (cdr xs))
            (list (car xs)))))
(define (reverse-it xs)
  (define (iter xs ys)
    (if (null? xs)
      ys
      (iter (cdr xs)
            (cons (car xs) ys))))
  (iter xs nil))
(define reverse reverse-it)
(reverse (list 1 4 9 16 25)) ; => (25 16 9 4 1)

;;; ex 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 1/2))
(define first-denom car)
(define except-first-denom cdr)
(define no-more? null?)
(define (cc amount coins)
  (cond
    ((= amount 0) 1)
    ((< amount 0) 0)
    ((no-more? coins) 0)
    (else
      (+ (cc amount
             (except-first-denom coins))
         (cc (- amount (first-denom coins))
             coins)))))
(cc 100 us-coins) ; => 292
(cc 100 uk-coins) ; => 104561
;; The order of the coin value list does not affect the answer produced by cc:
(cc 100 us-coins)            ; => 292
(cc 100 (reverse us-coins))  ; => 292
(cc 100 (list 5 50 1 25 10)) ; => 292
;; This is because the cc algorithm does not assume the coin values are sorted
;; in any particular order. It recurs on the cdr of the list, so it will always
;; be able to reach the end of the list unless it reaches one of the other base
;; cases first.

;;; ex 2.20
(define (same-parity . xs)
  (define (helper pred xs)
    (cond
      ((null? xs) xs)
      ((pred (car xs))
       (cons (car xs)
             (helper pred (cdr xs))))
      (else (helper pred (cdr xs)))))
  (cond
    ((null? xs) xs)
    ((even? (car xs)) (helper even? xs))
    (else (helper odd? xs))))
(same-parity 1 2 3 4 5 6 7) ; => (1 3 5 7)
(same-parity 2 3 4 5 6 7)   ; => (2 4 6)

;;; ex 2.21
(define (square-list-1 xs)
  (if (null? xs)
    nil
    (cons (square x)
          (square-list-1 (cdr xs)))))
(define (square-list-2 xs)
  (map square xs))

;;; ex 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items nil))
;; This reverses the order of the list because he is building up a new list in
;; the reverse order that the original one was constructed. The first element to
;; be consed onto the original list is its last element. Consing in reverse
;; order produces a reversed list.
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items nil))
(square-list (list 1 2 3 4 5)) ; => (((((() . 1) . 4) . 9) . 16) . 25)
;; When Louis interchanges the arguments to cons, it doesn't work because he is
;; trying to cons a list onto a single element. This creates a list structure
;; (in that it is made up of pairs), but this is not a sequence. Louis is trying
;; to use cons to add an element to the end a sequence, but this is not
;; possible. To add something to the end of a sequence, you must walk all the
;; way to its end. He could use append instead of cons to achieve this, but this
;; would end up being much less efficient than the recursive map.

;;; ex 1.23
(define (for-each f xs)
  (cond
    ((null? xs) #t)
    (else (f (car xs))
          (for-each f (cdr xs)))))
(for-each
  (lambda (x)
    (newline)
    (display x))
  (list 57 321 88))
;; 57
;; 321
;; 88

;;; ex 2.24
(list 1 (list 2 (list 3 4))) ; => (1 (2 (3 4)))
;; box-and-pointer structure
; [*|*]--->[*|X]
;  |        |
;  v        \->[*|*]--->[*|X]
;  1            |        |
;               v        \->[*|*]--->[4|X]
;               2            |
;                            v
;                            3
;; tree interpretation
;   /\
;  1 /\
;   2 /\
;    3 4

;;; ex 2.25
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9)))))) ; => 7
(car (car '((7)))) ; => 7
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
  '(1 (2 (3 (4 (5 (6 7)))))))))))))))))) ; => 7

;;; ex 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) ; => (1 2 3 4 5 6)
(cons x y)   ; => ((1 2 3) 4 5 6)
(list x y)   ; => ((1 2 3) (4 5 6))

;;; ex 2.27
(define (deep-reverse-rec x)
  (if (pair? x)
    (append (deep-reverse (cdr x))
            (list (deep-reverse (car x))))
    x))
(define (deep-reverse-it x)
  (if (pair? x)
    (map deep-reverse-it (reverse x))
    x))
(deep-reverse-rec '((1 2) (3 4))) ; => ((4 3) (2 1))
(deep-reverse-it '((1 2) (3 4)))  ; => ((4 3) (2 1))

;;; ex 2.28
(define (fringe t)
  (cond
    ((null? t) t)
    ((pair? (car t))
     (append (fringe (car t))
             (fringe (cdr t))))
    (else (cons (car t)
                (fringe (cdr t))))))
(fringe '((1 2) (3 4)))         ; => (1 2 3 4)
(fringe '((((5) 2) ((3 2) 9)))) ; => (5 2 3 2 9)

;;; ex 2.29
(define make-mobile list)
(define make-branch list)
;; (a) selectors
(define left-branch car)
(define right-branch cadr)
(define branch-length car)
(define branch-structure cadr)
;; (b) total weight
(define (mobile-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (number? struct)
      struct
      (mobile-weight struct))))
;; (c) balance predicate
(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))
(define (mobile-balanced? mobile)
  (and (= (torque (left-branch mobile))
          (torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))
(define (branch-balanced? branch)
  (let ((struct (branch-structure branch)))
    (or (number? struct)
        (mobile-balanced? struct))))
;; (d) We just need to change some of the selectors.
(define make-mobile cons)
(define make-branch cons)
;; new selectors
(define right-branch cdr)
(define branch-structure cdr)

;;; ex 2.30
(define (square-tree-1 t)
  (cond
    ((null? t) nil)
    ((not (pair? t)) (square t))
    (else (cons (square-tree (car t))
                (square-tree (cdr t))))))
(define (square-tree-2 t)
  (map (lambda (t)
         (if (pair? t)
           (square-tree-2 t)
           (square t)))
       t))
(define square-tree square-tree-1)
(square-tree '(1 (2 (3 4) 5) (6 7))) ; => (1 (4 (9 16) 25) (36 49))

;;; ex 2.31
(define (tree-map-1 f t)
  (cond
    ((null? t) nil)
    ((not (pair? t)) (f t))
    (else (cons (tree-map-1 f (car t))
                (tree-map-1 f (cdr t))))))
(define (tree-map-2 f t)
  (map (lambda (t)
         (if (pair? t)
           (tree-map-2 f t)
           (f t)))
       t))
(define tree-map tree-map-1)

;;; ex 2.32
(define (powerset s)
  (if (null? s)
    (list nil)
    (let ((first-item (car s))
          (subsets-rest (subsets (cdr s))))
      (append subsets-rest
              (map (lambda (set) (cons first-item set))
                   rest)))))
;; This works because we can define the powerset recursively like this:
;; 1. The powerset of an empty set is (()).
;; 2. Given a set S and its powerset P(S), the powerset of S' (the set formed by
;;    adding the element x to S) is P(S'), and P(S') is equal to the union of
;;    P(S) and {R ∪ {x} | R ∈ P(S)}.
;; These form the base case and the natural recursion for the poweset procedure,
;; and they are sufficient to construct the powerset of any set.

;;; ssec 2.2.3 (seq ops)
(define (filter pred xs)
  (cond
    ((null? xs) nil)
    ((pred (car xs))
     (cons (car xs) (filter pred (cdr xs))))
    (else (filter pred (cdr xs)))))
(define (reduce op initial xs)
  (if (null? xs)
    initial
    (op (car xs)
        (reduce op initial (cdr xs)))))
(define (range a b)
  (if (> a b)
    nil
    (cons a (range (+ a 1) b))))
(define (leaves t)
  (cond
    ((null? t) nil)
    ((not (pair? t)) (list t))
    (else (append (leaves (car t))
                  (leaves (cdr t))))))
(define (sum-odd-squares t)
  (reduce + 0 (map square (filter odd? (leaves t)))))
(define (even-fibs n)
  (reduce cons nil (filter even? (map fib (range 0 n)))))

;;; ex 2.33
(define (map f xs)
  (reduce (lambda (x y) (cons (f x) y)) nil xs))
(define (append xs ys)
  (reduce cons ys xs))
(define (length xs)
  (reduce (lambda (x n) (+ n 1)) 0 xs))

;;; ex 2.34
(define (horner-eval x coefs)
  (reduce (lambda (coef higher-terms)
            (+ (* higher-terms x) coef))
          0
          coefs))
(horner-eval 2 (list 1 3 0 5 0 1)) ; => 79

;;; ex 2.35
(define (count-leaves t)
  (reduce + 0 (map (lambda (x) 1)
                   (leaves t))))

;;; ex 2.36
(define (reduce-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (reduce op init (map car seqs))
          (reduce-n op init (map cdr seqs)))))

;;; ex 2.37
(define (dot-product v w)
  (reduce + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (u) (dot-product u v)) m))
(define (transpose mat)
  (reduce-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r)
           (map (lambda (c)
                  (dot-product r c))
                cols))
         m)))

;;; ex 2.38
(define (fold-left op init xs)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter init xs))
(define fold-right reduce)
(fold-right / 1 (list 1 2 3))      ; => 3/2
(fold-left / 1 (list 1 2 3))       ; => 1/6
(fold-right list nil (list 1 2 3)) ; => (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))  ; => (((() 1) 2) 3)
;; If op satisfies the commutative property (= (op x y) (op y x)), then
;; fold-right and fold-left will produce the same values for any sequence.

;;; ex 2.39
(define (reverse-l xs)
  (fold-right (lambda (x y) (append y (list x))) nil xs))
(define (reverse-r xs)
  (fold-left (lambda (x y) (cons y x)) nil xs))
(reverse-l (list 1 2 3 4 5)) ; => (5 4 3 2 1)
(reverse-r (list 1 2 3 4 5)) ; => (5 4 3 2 1)

;;; ssec 2.2.3 (nested mappings)
(define (mapcat f xs)
  (reduce append nil (map f xs)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
(define (remove item seq)
  (filter (lambda (x) (not (= x item))) seq))
;; wishful thinking!
(define (permutations s)
  (if (null? s)
    (list nil)
    (mapcat (lambda (x)
              (map (lambda (p) (cons x p))
                   (permutations (remove x s))))
            s)))

;;; ex 2.40
(define (unique-pairs n)
  (mapcat (lambda (i)
            (map (lambda (j) (list i j))
                 (enum-interval 1 (- i 1))))
          (enum-interval 1 n)))

;;; ex 2.41
(define (unique-triples n)
  (mapcat (lambda (i)
            (mapcat (lambda (j)
                      (map (lambda (k) (list i j k))
                           (enum-interval 1 j)))
                    (enum-interval 1 i)))
          (enum-interval 1 n)))
(define (triple-sums n s)
  (filter (lambda (t)
            (= s (+ (car t) (cadr t) (caddr t))))
          (unique-triples n)))

;;; ex 2.42
(define empty-board '())
(define (make-position row col)
  (cons row col))
(define (adjoin-position pos rest-of-queens)
  (cons pos rest-of-queens))
(define get-row car)
(define get-col cdr)
(define (safe? positions)
  (let ((row (get-row (car positions))))
    (define (ssafe? poss over)
      (or (null? poss)
          (let ((rrow (get-row (car poss))))
            (and (not (= rrow row))
                 (not (= rrow (- row over)))
                 (not (= rrow (+ row over)))
                 (ssafe? (cdr poss) (+ over 1))))))
    (or (null? (cdr positions))
        (ssafe? (cdr positions) 1))))
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? positions))
        (mapcat
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                     (make-position new-row k)
                     rest-of-queens))
                 (enum-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;;; ex 2.43
;; The interchange makes the program run slowly because it evaluates the
;; recursive call to queen-cols multiple times. Instead of doing the recursive
;; call and then adjoining all possible new positions to each set of positions
;; for the k-1 case, Louis Reasoner's procedure enumerates the interval for the
;; possible new positions once and then for each one does the same recursive
;; call to get the set of positions for the k-1 case. The original procedure
;; evaluates the enumeration multiple times, which does not significantly affect
;; performance. Evaluating the rercusive call multiple times is wasteful. Louis
;; Reasoner could still use the interchanged version if he bound the value of
;; the recursive call in a let-binding surrounding the flatmap application.

;;; example 2.2.4 (picture language)
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))
(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;;; ex 2.44
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

;;; example 2.2.4 (higher-order ops)
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;;; ex 2.45
(define (split comb split-comb)
  (define (splitter painter n)
    (if (= n 0)
      painter
      (let ((smaller (splitter painter (- n 1))))
        (comb painter (split-comb smaller smaller)))))
  splitter)
(define right-split (split beside below))
(define up-split (split below beside))

;;; example 2.2.4 (frames)
;; This is a curried procedure.
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                (scale-vect (ycor-vect v) (edge2-frame frame))))))

;;; ex 2.46
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)
(define (add-vect u v)
  (make-vect (+ (xcor-vect u) (xcor-vect v))
             (+ (ycor-vect u) (ycor-vect v))))
(define (sub-vect u v)
  (make-vect (- (xcor-vect u) (xcor-vect v))
             (- (ycor-vect u) (ycor-vect v))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;;; ex 2.47
;; first representation
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)
;; second representation
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame cddr)

;;; example 2.2.4 (painters)
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame)
           (start-segment segment))
          ((frame-coord-map frame)
           (end-segment segment))))
      segment-list)))

;;; ex 2.48
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

;;; ex 2.49
;; (a) outline of the frame
(define paint-outline
  (segments->painter
    (list (make-segment (make-vect 0 0) (make-vect 1 0))
          (make-segment (make-vect 0 1) (make-vect 1 1))
          (make-segment (make-vect 0 0) (make-vect 0 1))
          (make-segment (make-vect 1 0) (make-vect 1 1)))))
;; (b) X in the frame (corners)
(define paint-x
  (segments->painter
    (list (make-segment (make-vect 0 0) (make-vect 1 1))
          (make-segment (make-vect 0 1) (make-vect 1 0)))))
;; (c) diamond in the frame (midpoints)
(define paint-diamond
  (segments->painter
    (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
          (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
          (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
          (make-segment (make-vect 0.5 1) (make-vect 1 0.5)))))
;; (d) wave painter
;; I don't feel like doing this.

;;; example 2.2.4 (transforming and combining)
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-orig (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))
(define (flip-vert painter)
  (transform-painter
    painter
    (make-vect 0 1)
    (make-vect 0 0)
    (make-vect 1 1)))
(define (shrink-to-upper-right painter)
  (transform-painter
    painter
    (make-vect 0.5 0.5)
    (make-vect 1 0.5)
    (make-vect 0.5 1)))
(define (rotate90-ccw painter)
  (transform-painter
    painter
    (make-vect 1 0)
    (make-vect 0 0)
    (make-vect 1 1)))
(define (squash-inwards painter)
  (transform-painter
    (make-vect 0 0)
    (make-vect 0.65 0.35)
    (make-vect 0.35 0.65)))
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((paint-left
            (transform-painter
              painter1
              (make-vect 0 0)
              split-point
              (make-vect 0 1)))
          (paint-right
            (transform-painter
              painter2
              split-point
              (make-vect 1 0)
              (make-vect 0.5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;;; ex 2.50
(define (flip-horiz painter)
  (transform-painter
    painter
    (make-vect 1 0)
    (make-vect 0 0)
    (make-vect 1 1)))
(define (rotate180-ccw painter)
  (transform-painter
    painter
    (make-vect 1 1)
    (make-vect 0 1)
    (make-vect 1 0)))
(define (rotate270-ccw painter)
  (tranform-painter
    painter
    (make-vect 0 1)
    (make-vect 0 0)
    (make-vect 1 1)))

;;; ex 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-bottom
            (transform-painter
              painter1
              (make-vect 0 0)
              (make-vect 1 0.5)
              split-point))
          (paint-top
            (transform-painter
              painter2
              split-point
              (make-vect 1 0.5)
              (make-vect 0 1))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))
(define (below painter1 painter2)
  (rotate90-ccw
    (beside
      (rotate270-ccw painter1)
      (rotate270-ccw painter2))))

;;; ex 2.52
;; (a) add a smile to the wave
;; I don't feel like doing this.
;; (b) change the corner-split pattern
(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
          (right (right-split painter (- n 1)))
          (corner (corner-split painter (- n 1))))
      (beside (below painter up)
              (below right corner)))))
;; (c) change orientation of corners in square-limit
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((flipped (flip-horiz quarter)))
      (square-of-four flipped quarter flipped quarter))))

;;;;; Section 2.3: Symbolic data

;;; ex 2.53
(list 'a 'b 'c)                         ; => (a b c)
(list (list 'george))                   ; => ((george))
(cdr '((x1 x2) (y1 y2)))                ; => ((y1 y2))
(cadr '((x1 x2) (y1 y2)))               ; => (y1 y2)
(pair? (car '(a short list)))           ; => #f
(memq 'red '((red shoes) (blue socks))) ; => #f
(memq 'red '(red shoes blue socks))     ; => (red shoes blue socks)

;;; ex 2.54
(define (equal? list1 list2)
  (let ((null1 (null? list1))
        (null2 (null? list2)))
    (or (and null1 null2)
        (and (not (or null1 null2))
             (eq? (car list1) (car list2))
             (equal? (cdr list1) (cdr list2))))))
(equal? '(this is a list) '(this is a list))   ; => #t
(equal? '(this is a list) '(this (is a) list)) ; => #f

;;; ex 2.55
;; The expression ''abracadabra is a shortant for (quote (quote abracadabra)).
;; This is the same as '(quote abracadabra), and it evaluates to a list with two
;; symbols: (quote abracadabra). Taking the car of this gives you the first
;; item, which is the symbol quote. This is what the interpreter prints.

;;; example 2.3.2 (symbolic differentiation)
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multiplicand expr))))
        (else (error "unknown expr type" expr))))
(define variable? symbol?)
(define same-variable? eq?)
(define (=number? expr num)
  (and (number? expr) (= expr num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))
(define (sum? expr)
  (and (pair? expr) (eq? (car expr) '+)))
(define addend cadr)
(define augend caddr)
(define (product? expr)
  (and (pair? expr) (eq? (car expr) '*)))
(define multiplier cadr)
(define multiplicand caddr)
(deriv '(* (* x y) (+ x 3)) 'x) ; => (+ (* x y) (* y (+ x 3)))

;;; ex 2.56
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
           (make-product (multiplier expr)
                         (deriv (multiplicand expr) var))
           (make-product (deriv (multiplier expr) var)
                         (multiplicand expr))))
        ((exponentiation? expr)
         (make-product
           (exponent expr)
           (make-product
             (make-exponentiation (base expr) (- (exponent expr) 1))
             (deriv (base expr) var))))
        (else (error "unknown expr type" expr))))
(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))
(define base cadr)
(define exponent caddr)
(define (make-exponentiation b e)
  (list '** b e))

;;; ex 2.57
(define addend cadr)
(define (augend sum)
  (reduce make-sum 0 (cddr sum)))
(define multiplier cadr)
(define (multiplicand product)
  (reduce make-product 1 (cddr product)))
(deriv '(* x y (+ x 3)) 'x) ; => (+ (* x y) (* y (+ x 3)))

;;; ex 2.58
;; (a) fully parenthesized infix form with two arguments
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))
(define (sum? expr)
  (and (pair? expr) (eq? (cadr expr) '+)))
(define addend car)
(define augend caddr)
(define (product? expr)
  (and (pair? expr) (eq? (cadr expr) '*)))
(define multiplier car)
(define multiplicand caddr)
(deriv '(x + (3 * (x + (y + 2)))) 'x) ; => 4
;; (b) standard algebraic notation
;; This doesn't always work, but it's a start. Actually doing it properly is
;; complicated -- it would be much easier to implement it from scratch rather
;; than just chaning the constructors and selectors.
(define (sum? expr)
  (and (pair? expr) (memq '+ (cdr expr))))
(define (addend expr)
  (define (until-plus expr)
    (if (eq? '+ (car expr))
      '()
      (cons (car expr) (until-plus (cdr expr)))))
  (if (eq? '+ (cadr expr))
    (car expr)
    (until-plus expr)))
(define (augend expr)
  (if (null? (cdddr expr))
    (caddr expr)
    (cddr expr)))
(deriv '(x + 3 * (x + y + 2)) 'x) ; => 4

;;; example 2.3.3 (sets as unordered lists)
(define (element-of-set? x set)
  (and (not (null? set))
       (or (equal? x (car set))
           (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))
(define (intersection-set set1 set2)
  (cond ((null? set1) '())
        ((null? set2) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersect-set (cdr set1) set2))))

;;; ex 2.59
(define (union-set set1 set2)
  (reduce adjoin-set set1 set2))

;;; ex 2.60
;; element-of-set? stays the same
(define adjoin-set cons)
(define union-set append)
;; intersection-set stays the same
;; Efficiency: element-of-set? is still Θ(n); adjoin-set is Θ(1).
;; +------------------+----------+--------+
;; | Function         | no dupes | dupes  |
;; +------------------+----------+--------+
;; | element-of-set   | Θ(n)     | Θ(n)   |
;; | adjoin-set       | Θ(n)     | Θ(1)   |
;; | union-set        | Θ(n^2)   | Θ(n)   |
;; | intersection-set | Θ(n^2)   | Θ(n^2) |
;; +------------------+----------+--------+
;; It looks like it is always more efficient with duplicates. However, the n
;; values become much larger with duplicates for obvious reasons. For small sets
;; and many operator applications, keeping duplicates is better. For large sets
;; and fewer operator applications, eliminating duplicates is better.

;;; example 2.3.3 (sets as ordered lists)
(define (element-of-set? x set)
  (and (not (null? set))
       (<= (car set) x)
       (or (= (car set) x)
           (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1))
          (x2 (car set2)))
      (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2) (intersection-set (cdr set1) set2))
            ((< x2 x2) (intersection-set set1 (cdr set2)))))))

;;; ex 2.61
(define (adjoin-set x set)
  (if (or (null? set) (<= x (car set)))
    (cons x set)
    (cons (car set)
          (adjoin-set x (cdr set)))))

;;; ex 2.62
(define (union-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1))
          (x2 (car set2)))
      (if (< x1 x2)
        (cons x1 (union-set (cdr set1) set2))
        (cons x2 (union-set set1 (cdr set2)))))))
