;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src chapter-1)
  (export prepare-chapter1)
  (import (rnrs base (6))
          (src lang syntax))

(define prepare-chapter1)

(SICP

(Chapter :1 "Building Abstractions with Procedures")

(Section :1.1 "The Elements of Programming")

(Subsection :1.1.1 "Expressions")

486 => 486
(+ 137 349) => 486
(- 1000 334) => 666
(* 5 99) => 495
(/ 10 5) => 2
(+ 2.7 10) => 12.7
(+ 21 35 12 7) => 75
(* 25 4 12) => 1200
(+ (* 3 5) (- 10 6)) => 19
(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6)) => 57

) ; end of SICP
) ; end of library

#|
(Subsection 1.1.2 "Naming and the Environment")

(define size 2)
size => 2
(* 5 size) => 10
(define pi 3.14159)
(define radius 10)
(* pi (* radius radius)) => 314.159
(define circumference (* 2 pi radius))
circumference => 62.8318

(Subsection 1.1.3 "Evaluating Combinations")

(* (+ 2 (* 4 6))
  (+ 3 5 7))
=> 390

(Subsection 1.1.4 "Compound Procedures")

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(sum-of-squares 3 4) => 25
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5) => 136

(Subsection 1.1.5 "The Substitution Model for Procedure Application"
  (use (1.1.4 square sum-of-squares f)))

;; Applicative-order evaluation:
(f 5)
=> (sum-of-squares (+ 5 1) (* 5 2))
=> (+ (square 6) (square 10))
=> (+ (* 6 6) (* 10 10))
=> (+ 36 100)
=> 136

;; Normal-order evaluation:
(f 5)
=> (sum-of-squares (+ 5 1) (* 5 2))
=> (+ (square (+ 5 1)) (square (* 5 2)))
=> (+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
=> (+ (* 6 6 ) (* 10 10))
=> (+ 36 100)
=> 136

(Subsection 1.1.6 "Conditional Expressions and Predicates")

(define (abs x)
  (cond
    ((> x 0) x)
    ((= x 0) 0)
    ((< x 0) (- x))))

(define (abs x)
  (cond
    ((< x 0) (- x))
    (else x)))

(define (abs x)
  (if (< x 0)
    (- x)
    x))

(define (>= x y) (or (> x y) (= x y)))
(define (>= x y) (not (< x y)))

(Exercise 1.1)

10 => 10
(+ 5 3 4) => 12
(- 9 1) => 8
(/ 6 2) => 3
(+ (* 2 4) (- 4 6)) => 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b)) => 19
(= a b) => #f
(if (and (> b a) (< b (* a b))) b a) => 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
=> 16
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
  (+ a 1))
=> 16

(Exercise 1.2)

(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
    (* 3 (- 6 2) (- 2 7)))
=> -37/150

(Exercise 1.3)

(define (f a b c)
  (cond
    ((and (<= a b) (<= a c))
     (+ (* b b) (* c c)))
    ((and (<= b a) (<= b c))
     (+ (* a a) (* c c)))
    ((and (<= c a) (<= c b))
     (+ (* a a) (* b b)))))

(Exercise 1.4)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; The operator evaluates to `+` (addition) when b is positive, and to `-`
;; (subtraction) when b is negative. Subtracting a negative is equivalent to
;; adding its absolute value, so this procedure performs `a + |b|` in all cases.

(Exercise 1.5)

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
; (test 0 (p))

;; With applicative-order evaluation, the expression will never return a value
;; because the interpreter tries to evaluate `(p)` and enters endless recursion.
;;
;; With normal-order evaluation, the expression will evaluate to zero. The `(p)`
;; expression is never evaluated because it is not necessary to do so.

(Subsection 1.1.7 "Example: Square Roots by Newton's Method"
  (use (1.1.4 square)))

(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
~> 3.00009155413138
(sqrt (+ 100 37))
~> 11.704699917758145
(sqrt (+ (sqrt 2) (sqrt 3)))
~> 1.7739279023207892
(square (sqrt 1000))
~> 1000.000369924366

(Exercise 1.6)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5) => 5
(new-if (= 1 1) 0 5) => 0

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

;; When Alyssa attempts to use this to compute square roots, it will not work.
;; The sqrt procedure will never return a value because it gets stuck in
;; sqrt-iter due to infinite recursion. The new-if combination always evaluates
;; the else-clause, which contains the recursive call, so the recursion will
;; never end.

(Exercise 1.7
  (use (1.1.7 sqrt)))

;; The `good-enough?` predicate does not work well for small numbers because the
;; tolerance is a fixed amount. It can't be too small or else it will take too
;; long to compute the square roots of large numbers, but at the same time, it
;; is impossible to use the procedure for values smaller than the tolerance.

(sqrt 0.000002) ~> 0.0312713096020622 ; (should be 0.0014142...)
(square (sqrt 0.000002)) ~> 0.000977894804228028

;; The test is inadequate for very large numbers because, with limited
;; precision, it is impossible to represent small differences between very large
;; numbers. The good-enough? difference will eventually become zero, but it
;; might actually be much greater than the tolerance if calculated with infinite
;; precision.

(define (good-enough? g1 g2)
  (< (/ (abs (- g2 g1)) g1)
     0.001))
(define (sqrt-iter guess x)
  (let ((better (improve guess x)))
    (if (good-enough? guess better)
      better
      (sqrt-iter better x))))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 0.000002) ~> 0.00141421356261785
(square (sqrt 0.000002)) ~> 2.00000000069227e-06

(Exercise 1.8)

(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))
(define (cbrt-iter guess x)
  (let ((better (improve guess x)))
    (if (good-enough? guess better)
      better
      (cbrt-iter better x))))
(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 8) ~> 2

(Subsection 1.1.8 "Procedures as Black-Box Abstractions")

;; The following two procedures should be indistinguishable:
(define (square x) (* x x))
(define (square x) (exp (double (log x))))

;; These should be indistinguishable as well:
(define (square x) (* x x))
(define (square y) (* y y))

;; Using block structure:
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x) (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x) guess
      (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; Without passing `x` around:
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess) guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(section 1.2 "Procedures and the Processes They Generate")

;;; NOTE THIS WAS THE PREVIOUS PLACE I HAD GOTTEN TO

(subsection 1.2.1 "Linear Recursion and Iteration")

(define (factorial-rec n)
  (if (= n 1)
    1
    (* n (factorial-rec (- n 1)))))
(define (factorial-it n)
  (define (helper counter prod)
    (if (> counter n)
      prod
      (helper (+ counter 1) (* prod counter))))
  (helper 1 1))

;;; ex 1.9
(define (r+ a b)
  (if (= a 0)
    b
    (inc (r+ (dec a) b))))
(define (i+ a b)
  (if (= a 0)
    b
    (i+ (dec a) (inc b))))
;; `r+` generates a recursive process.
(check
  (r+ 4 5)
  => (inc (r+ 3 5))
  => (inc (inc (r+ 2 5)))             ; expanding
  => (inc (inc (inc (r+ 1 5))))
  => (inc (inc (inc (inc (r+ 0 5))))) ; 4 deferred operations
  => (inc (inc (inc (inc 5))))
  => (inc (inc (inc 6)))              ; contracting
  => (inc (inc 7))
  => (inc 8)
  => 9)
;; `i+` generates an iterative process.
(check
  (i+ 4 5)
  => (i+ 3 6)
  => (i+ 2 7)
  => (i+ 1 8)
  => (i+ 0 9)
  => 9)

;;; ex 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(check
  (A 1 10) => 1024
  (A 2 4) => 65536
  (A 3 3) => 65536)
(define (f n) (A 0 n))   ; 2n
(define (g n) (A 1 n))   ; 2^n
(define (h n) (A 2 n))   ; 2^2^2^... (n 2s)
(define (k n) (* 5 n n)) ; 5n^2

;;; ssec 1.2.2 (Fibonacci)
(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (fib-it n)
  (define (helper a b count)
    (if (= count 0)
      a
      (helper b (+ a b) (- count 1))))
  (helper 0 1 n))

;;; example (counting change)
(define (first-denom n-kinds)
  (list-ref
    '(1 5 10 25 50)
    (- n-kinds 1)))
(define (count-change amount)
  (define (cc a n)
    (cond
      ((< a 0) 0)
      ((= a 0) 1)
      ((= n 0) 0)
      (else (+ (cc a (- n 1))
               (cc (- a (first-denom n)) n)))))
  (cc amount 5))

;;; ex 1.11
(define (f-rec n)
  (if (< n 3)
    n
    (+ (f-rec (- n 1))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))
(define (f-it n)
  (define (helper a b c counter)
    (if (= counter 0)
      a
      (helper b c (+ c (* 2 b) (* 3 a)) (- counter 1))))
  (helper 0 1 2 n))

;;; ex 1.12
(define (pascal-tri i j)
  (if (or (= j 0) (= j i))
    1
    (+ (pascal-tri (- i 1) (- j 1))
       (pascal-tri (- i 1) j))))

;;; ex 1.13
;; See the relevant section of `proofs/proofs.pdf`.

;;; ex 1.14
;; count-change
;; steps: O(n^5) because there are 5 types of coins
;; space: O(n) because the max depth of the tree grows linearly
;; Remember: for a tree-recursive process, space is proportional to the maximum
;; depth of the tree, and the number of steps is the number of leaves.

;;; ex 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine theta)
  (if (<= (abs theta) 0.1)
    theta
    (p (sine (/ theta 3.0)))))
;; (a) The procedure `p` is evaluated five times when `(sine 12.15)` is eval'd.
(check
  (sine 12.15)
  => (p (sine 4.05))
  => (p (p (sine 1.35)))
  => (p (p (p (sine 0.45))))
  => (p (p (p (p (sine 0.15)))))
  => (p (p (p (p (p (sine 0.05))))))) ; five times when theta <= 0.1
;; (b) The order of growth for sine:
;; During the process, `p` is evaluated n times such that a/3^n <= 0.1. Solving
;; for n gives us n = log(10a)/log(3), therefore the number of steps for sine
;; grows as O(log(n)). The interpreter must maintain the stack for that number
;; of calls to `p`, therfore the space complexity is also O(log(n)).

;;; ssec 1.2.4 (exponentiation)
;; steps O(n), space O(n)
(define (expt-rec b n)
  (if (= n 0)
    1
    (* b (expt-rec b (- n 1)))))
;; steps O(n), space O(1)
(define (expt-it b n)
  (define (helper counter prod)
    (if (= counter 0)
      prod
      (helper (- count 1) (* prod b))))
  (helper n 1))
;; steps O(log(n)), space O(log(n))
(define (fast-expt-rec b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-rec b (/ n 2))))
        (else (* b (fast-expt-rec b (- n 1))))))

;;; ex 1.16
(define (fast-expt-it b n)
  (define (helper a b n)
    (cond ((= n 0) a)
          ((even? n) (helper a (square b) (/ n 2)))
          (else (helper (* a b) b (- n 1)))))
  (helper 1 b n))

;;; ex 1.17
(define (slow-* a b)
  (if (= b 0)
    0
    (+ a (* a (- b 1)))))
(define (fast-*-rec a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-*-rec a (halve b))))
        (else (+ a (fast-*-rec a (- b 1))))))

;;; ex 1.18
(define (fast-*-it a b)
  (define (helper c a b)
    (cond ((= b 0) c)
          ((even? b) (helper c (double a) (halve b)))
          (else (helper (+ c a) a (- b 1)))))
  (helper 0 a b))

;;; ex 1.19
(define (fib n)
  (define (helper a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (helper a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
          (else (helper (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
  (helper 1 0 0 1 n))

;;; ssec 1.2.5 (GCD)
;; iterative; O(log(n)) steps
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;;; ex 1.20
;; applicative order: 4 remainder operations
(check
  (gcd 206 40)
  => (gcd 40 (remainder 206 40))
  => (gcd 40 6)
  => (gcd 6 (remainder 40 6))
  => (gcd 6 4)
  => (gcd 4 (remainder 6 4))
  => (gcd 4 2)
  => (gcd 2 (remainder 4 2))
  => (gcd 2 0)
  => 2)
;; normal order: 18 remainder operations
;; Each `b` gets evaluated once in the `(= b 0)` predicate (14 ops).
;; The final `a` gets evaluated in the end (4 ops).
;; 14 + 4 = 18
(check
  (gcd 206 40)
  => (gcd 40 (remainder 206 40))
  => (gcd (remainder 206 40)
          (remainder 40 (remainder 206 40)))
  => (gcd (remainder 40 (remainder 206 40))
          (remainder (remainder 206 40)
                      (remainder 40 (remainder 206 40))))
  => (gcd (remainder (remainder 206 40)
                      (remainder 40 (remainder 206 40)))
          (remainder (remainder 40 (remainder 206 40))
                      (remainder (remainder 206 40)
                                (remainder 40 (remainder 206 40)))))
  => (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40))))

;;; ssec 1.2.6 (primality)
;; trial division
(define (divides? a b)
  (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (prime? n)
  (= n (smallest-divisor n)))
;; Fermat test
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
        (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else (remainder (* base (expmod base (- exp 1) m))
                        m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))
(define (fast-prime? n times)
  (or (= times 0)
      (and (fermat-test n)
           (fast-prime? n (- times 1)))))

;;; ex 1.21
(check
  (smallest-divisor 199) => 199
  (smallest-divisor 1999) => 1999
  (smallest-divisor 19999) => 7)

;;; ex 1.22
(define (runtime)
  (let ((t (current-time)))
    (+ (time-second t)
       (/ (time-nanosecond t) 1e9))))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
(define (ceil-odd n)
  (+ n (- 1 (remainder n 2))))
(define (search-for-primes a b)
  (define (helper a b)
    (if (<= a b)
      (begin (timed-prime-test a)
             (helper (+ a 2) b))))
  (helper (ceil-odd a) b))
;; 3 primes greater than 1000 (A)
; 1009 *** 4.792213439941406e-5
; 1013 *** 4.291534423828125e-5
; 1019 *** 4.792213439941406e-5
;; 3 primes greater than 10000 (B)
;; 2.3A < B < 2.8A, sqrt(10) = 3.16
; 10007 *** 1.1086463928222656e-4
; 10009 *** 1.1396408081054688e-4
; 10037 *** 1.2302398681640625e-4
;; 3 primes greater than 100000 (C)
;; 3.3B < C < 4.1B, sqrt(10) = 3.16
; 100003 *** 4.010200500488281e-4
; 100019 *** 3.6597251892089844e-4
; 100043 *** 4.558563232421875e-4
;; 3 primes greater than 1000000 (D)
;; 2.8C < D < 3.4C, sqrt(10) = 3.16
;; 23.7A < D < 31.9A, sqrt(1000) = 31.62
; 1000003 *** .0013530254364013672
; 1000033 *** .0011339187622070312
; 1000037 *** .0013699531555175781
;; The data seems to bear out the O(sqrt(n)) prediction. The larger the numbers,
;; the closer the growth between powers of ten is to the square root of ten.
;; This result is compatible with the notion that programs on the machine run in
;; time proportional to the number of steps required for the computation.

;;; ex 1.23
(define (prime? n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (next n)
    (if (= n 2) 3 (+ n 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
  (= n (smallest-divisor n)))
;; 3 primes greater than 1000
; 1009 *** 5.1975250244140625e-5   (1.085x)
; 1013 *** 5.1975250244140625e-5   (1.211x)
; 1019 *** 6.198883056640625e-5    (1.294x)
;; 3 primes greater than 10000
; 10007 *** 1.1491775512695312e-4  (1.037x)
; 10009 *** 1.1801719665527344e-4  (1.036x)
; 10037 *** 1.1897087097167969e-4  (0.967x)
;; 3 primes greater than 100000
; 100003 *** 3.540515899658203e-4  (0.883x)
; 100019 *** 3.490447998046875e-4  (0.954x)
; 100043 *** 3.590583801269531e-4  (0.788x)
;; 3 primes greater than 1000000
; 1000003 *** .0010960102081298828 (0.810x)
; 1000033 *** .001055002212524414  (0.930x)
; 1000037 *** .0010900497436523438 (0.796x)
;; The expectation of half time was not confirmed. In fact, this method is
;; actually slower for primes under 10000. Even for seven-figure primes, this
;; method only shaves off 20% of the time. There was probably some error in
;; these measurements -- the time measured is too small. Other processes on the
;; computer and random factors might have played a role. I was surprised that
;; the new method turned out this bad (for relatively small primes, anyway). By
;; replacing the increment with a call to the next procedure, we add some
;; overhead and a conditional (which imlicates branch prediction), and maybe
;; this outweighed the gain from skipping the even numbers past two.

;;; ex 1.24
(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
    (report-prime (- (runtime) start-time))))
;; 3 primes greater than 1000 (A)
; 1009 *** .003638029098510742
; 1013 *** .003793001174926758
; 1019 *** .003606081008911133
;; 3 primes greater than 10000 (B)
;; 0.988A < B < 1.196A
; 10007 *** .004311084747314453
; 10009 *** .0039730072021484375
; 10037 *** .0037479400634765625
;; 3 primes greater than 100000 (C)
;; 0.893B < C < 1.294B
; 100003 *** .004847049713134766
; 100019 *** .004848003387451172
; 100043 *** .003850221633911133
;; 3 primes greater than 1000000 (D)
;; 0.891C < D < 1.453C
;; 1.138A < D < 1.551A
; 1000003 *** .005592823028564453
; 1000033 *** .004972934722900391
; 1000037 *** .0043179988861083984
;; Since the Fermat test has O(log(n)) grwoth, I expected the time to the primes
;; near 1000000 to be only a bit greater than the time needed to test primes
;; near 1000. The data bears this out -- for each additional order of magnitude
;; of the primes, the time required increases by a small, constant amount.
;; Specifically, primes that are 10 times larger take about 0.001 seconds longer
;; to test using the Fermat method. It should be noted that these results may be
;; dependent on the choice of 100 as the second argument to `fast-prime?` (the
;; exercise did not specify what value to use).

;;; ex 1.25
(define fast-expt fast-expt-it)
(define (new-expmod base exp m)
  (remainder (fast-expt base exp) m))
;; This procedure works, but it is not as efficient. The Fermat test takes much
;; longer using this version of expmod -- longer by three orders of magnitude.
;; While `fast-expt` is reasonably fast, the original `expmod` procedure is much
;; faster. The key to its implmentation is not only successive squaring (which
;; `fast-expt` does as well in this new procedure), but that it calls remainder
;; between each squaring. This new procedure does not, so the value becomes
;; enormous (requiring bignums, which is slow) by the time the remainder is
;; finally taken. Suppose we test the primality of n = 9, choosing a = 5. Using
;; the old definiton of `expmod`, the process will evolve like so:
(check
  (define r remainder)
  (define s square)
  (expmod 5 9 9)
  => (r (* 5 (expmod 5 8 9)) 9)
  => (r (* 5 (r (s (expmod 5 4 9)) 9)) 9)
  => (r (* 5 (r (s (r (s (expmod 5 2 9)) 9)) 9)) 9)
  => (r (* 5 (r (s (r (s (r (s (expmod 5 1 9)) 9)) 9)) 9)) 9)
  => (r (* 5 (r (s (r (s (r (s (r (* 5 (expmod 5 0 9)) 9)) 9)) 9)) 9)) 9)
  => (r (* 5 (r (s (r (s (r (s (r (* 5 1) 9)) 9)) 9)) 9)) 9)
  => (r (* 5 (r (s (r (s (r (s (r 5 9)) 9)) 9)) 9)) 9)
  => (r (* 5 (r (s (r (s (r (s 5) 9)) 9)) 9)) 9)
  => (r (* 5 (r (s (r (s (r 25 9)) 9)) 9)) 9)
  => (r (* 5 (r (s (r (s 7) 9)) 9)) 9)
  => (r (* 5 (r (s (r 49 9)) 9)) 9)
  => (r (* 5 (r (s 4) 9)) 9)
  => (r (* 5 (r 16 9)) 9)
  => (r (* 5 7) 9)
  => (r 35 9)
  => 8
;; Compare this to the evolution of the process using the new `expmod`:
  (expmod 5 9 9)
  => (r (fast-expt 5 9) 9)
  => (r 1953125 9))
;; The original `expmod` doesn't need to deal with numbers anywhere near that
;; size, so it is much more efficient. This number may seem okay, but it will
;; grow exponentially with n (by definition), and will quickly require arbitrary
;; precision integer math (bignum), which is much slower than 32-bit arithmetic.

;;; ex 1.26
;; When the `square` combination is evaluated, the `expmod` combination is
;; evaluated once and then its value is substituted into the `square` compound
;; procedure according to the substitution model. When the squaring is written
;; as an explicit multiplication, the `expmod` combination is evaluated twice.
;; The interpreter has no way of knowing that they will have the same value.
;; This transforms a linear recusive process into a tree-recursive process. The
;; time complexity of this tree-recursive process is O(log(2^n)), or O(n).

;;; ex 1.27
(define (fermat-all? n)
  (define (helper a)
    (if (< a n)
      (and (= (expmod a n n) a)
           (helper (+ a 1)))
      #t))
  (helper 1))
;; These Carmichael numbers pass the Fermat tests for all values of a < n:
(slow
  (check
    (fermat-all? 561) => #t
    (fermat-all? 1105) => #t
    (fermat-all? 1729) => #t
    (fermat-all? 2465) => #t
    (fermat-all? 2821) => #t
    (fermat-all? 6601) => #t))
;; According to the trial divison procedure, none of them are prime:
(check
  (prime? 561) => #f
  (prime? 1105) => #f
  (prime? 1729) => #f
  (prime? 2465) => #f
  (prime? 2821) => #f
  (prime? 6601) => #f)

;;; ex 1.28
(define (square-check x m)
  (let ((sqm (remainder (square x) m)))
    (if (and (not (or (= x 1) (= x (- m 1))))
             (= sqm 1))
      0
      sqm)))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (square-check (expmod base (/ exp 2) m) m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))
(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 2 (random-integer (- n 2)))))
(define (fast-prime? n times)
  (or (= times 0)
      (and (miller-rabin-test n)
           (fast-prime? n (- times 1)))))
(define (p? n) (fast-prime? n 100))
;; known composite numbers
(check
  (p? 32) => #f
  (p? 100) => #f
  (p? 1000004) => #f)
;; known prime numbers
(check
  (p? 5) => #t
  (p? 997) => #t
  (p? 1000037) => #t)
;; known Carmichael numbers
(check
  (p? 561) => #f
  (p? 1105) => #f
  (p? 1729) => #f
  (p? 2465) => #f
  (p? 2821) => #f
  (p? 6601) => #f)

;;;;; Section 1.3: Formulating abstractions with higher-order procedures

;;; ssec 1.3.1 (procedures as args)
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;;; ex 1.29
(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (term k)
      (* (f (+ a (* k h)))
         (if (or (= k 0) (= k n))
           1
           (+ 2 (* 2 (remainder k 2))))))
    (* h (sum term 0 inc n))))
;; The integral procedure is a bit inaccurate, whereas the `simpson` procedure
;; gives the exact answer even when n = 2. This is much better.
(check
  (integral cube 0 1 0.01) ~> 0.24998750000000042
  (integral cube 0 1 0.001) ~> 0.249999875000001
  (simpson cube 0 1 2) => 3/4
  (simpson cube 0 1 100) => 3/4
  (simpson cube 0 1 1000) => 3/4)

;;; ex 1.30
(define (sum term a next b)
  (define (iter a acc)
    (if (> a b)
      acc
      (iter (next a) (+ acc (term a)))))
  (iter a 0))

;;; ex 1.31
(define (product-rec term a next b)
  (if (> a b)
    1
    (* (term a)
       (product-rec term (next a) next b))))
(define (product-it term a next b)
  (define (iter a acc)
    (if (> a b)
      acc
      (iter (next a) (* acc (term a)))))
  (iter a 1))
(define product product-it)
(define (factorial n)
  (product identity 1 inc n))
(check
  (factorial 5) => 120
  (factorial 7) => 5040)
(define (approx-qpi n)
  (define (term k)
    (let ((r (remainder k 2)))
      (/ (+ k 2 (- r))
         (+ k 1 r))))
  (product term 1.0 inc n))
(check
  (* 4 (approx-qpi 10)) ~> 3.2751010413348065
  (* 4 (approx-qpi 100)) ~> 3.1570301764551654
  (* 4 (approx-qpi 1000)) ~> 3.1431607055322552
  (* 4 (approx-qpi 10000)) ~> 3.1417497057380084)

;;; ex 1.32
(define (accumulate-rec combine id term a next b)
  (if (> a b)
    id
    (combine (term a) (accumulate-rec combine id term (next a) next b))))
(define (accumulate-it combine id term a next b)
  (define (iter a acc)
    (if (> a b)
      acc
      (iter (next a) (combine (term a) acc))))
  (iter a id))
(define accumulate accumulate-it)
(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (product term a next b)
  (accumulate * 1 term a next b))

;;; ex 1.33
(define (filtered-accumulate-rec combine pred id term a next b)
  (if (> a b)
    id
    (let ((t (term a))
          (rest (accumulate-rec combine pred id term (next a) next b)))
      (if (pred t)
        rest
        (combine t rest)))))
(define (filtered-accumulate-it combine pred id term a next b)
  (define (iter a acc)
    (if (> a b)
      acc
      (let ((t (term a)))
        (if (pred t)
          (iter (next a) (combine t acc))
          (iter (next a) acc)))))
  (iter a id))
(define filtered-accumulate filtered-accumulate-it)
(define (a-sumsp a b)
  (filtered-accumulate + prime? 0 square a inc b))
(define (b-prodrp n)
  (define (rel-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * rel-prime? 1 identity a inc b))

;;; ex 1.34
(define (f g) (g 2))
(check
  (f square) => 4
  (f (lambda (z) (* z (+ z 1)))) => 6)
;; If we try evaluating the combination `(f f)`, we get the following process:
; (f f)
; (f 2)
; (2 2)
;; This gives an error, since 2 does not evaluate to a procedure. We cannot
;; apply 2 to the argument 2 because that doesn't make any sense.

;;; ssec 1.3.3 (procs as general methods)
;; half-interval method for finding zeros
;; time complexity: O(log(L/T)) where L is original |a - b| and T is tolerance
(define tolerance 0.00001)
(define (close-enough? x y)
  (< (abs (- x y)) tolerance))
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search f neg-point midpoint))
              ((negative? test-value)
               (search f midpoint pos-point))
              (else midpoint))))))
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (errorf 'half-interval-method
              "Values are not of opposite sign: ~s, ~s" a b)))))
(check
  (half-interval-method sin 2.0 4.0) ~> 3.141590118408203)
;; fixed point
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))
(check
  (fixed-point cos 1.0) ~> 0.7390822985224023)

;;; ex 1.35
;; See the relevant section of `proofs/proofs.pdf`.
(check
  (/ (+ 1 (sqrt 5)) 2) ~> 1.618033988749895
  (fixed-point (lambda (x) (+ 1 (/ x))) 42.0) ~> 1.6180328499442242)

;;; ex 1.36
(define (fixed-point-verbose f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))
(define (f x) (/ (log 1000) (log x)))
(hide-output
  (check
    (fixed-point-verbose f 5)
    ~> 4.555539314360711 ; 29 approximations
    (fixed-point-verbose (lambda (x) (average x (f x))) 5)
    ~> 4.5555361005218895)) ; 9 approximations
;; Without average damping, it requires 20 more approximations.

;;; ex 1.37
(define (count-frac-rec n d k)
  (define (helper i)
    (if (= i k)
      0
      (/ (n i)
         (+ (d i) (helper (+ i 1))))))
  (helper 1))
(define (count-frac-it n d k)
  (define (iter i acc)
    (if (zero? i)
      acc
      (iter (- i 1) (/ (n i)
                       (+ (d i) acc)))))
  (iter k 0))
(define count-frac count-frac-it)
(define (always-one i) 1.0)
(define (approx-gr k) (count-frac always-one always-one k))
(check
  (approx-gr 1) ~> 1.0
  (approx-gr 2) ~> 0.5
  (approx-gr 3) ~> 0.6666666666666666
  (approx-gr 4) ~> 0.6000000000000001
  (approx-gr 5) ~> 0.625
  (approx-gr 6) ~> 0.6153846153846154
  (approx-gr 7) ~> 0.6190476190476191
  (approx-gr 8) ~> 0.6176470588235294
  (approx-gr 9) ~> 0.6181818181818182
  (approx-gr 10) ~> 0.6179775280898876
  (approx-gr 11) ~> 0.6180555555555556)
;; When k = 11, the value is accurate to 4 decimal places.

;;; ex 1.38
(define (approx-e k)
  (define (d i)
    (if (zero? (remainder (+ i 1) 3))
      (* 2/3 (+ i 1))
      1))
  (+ 2 (count-frac always-one d k)))
(check
  (approx-e 1) ~> 3.0
  (approx-e 2) ~> 2.6666666666666665
  (approx-e 3) ~> 2.75
  (approx-e 4) ~> 2.7142857142857144
  (approx-e 5) ~> 2.71875
  (approx-e 1000) ~> 2.7182818284590455)

;;; ex 1.39
(define (tan-cf x k)
  (count-frac
    (lambda (i) (if (= i 1) x (- (square x))))
    (lambda (i) (- (* i 2) 1))
    k))
(define pi (* (atan 1) 4))
(check
  (tan-cf (/ pi 3) 1) ~> 1.0471975511965976
  (tan-cf (/ pi 3) 2) ~> 1.650535956338694
  (tan-cf (/ pi 3) 3) ~> 1.7291124259895505
  (tan-cf (/ pi 3) 4) ~> 1.7319971836379957
  (tan-cf (/ pi 3) 5) ~> 1.7320501979592633
  (tan (/ pi 3)) ~> 1.7320508075688767)

;;; ssec 1.3.4 (returning procs)
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (my-sqrt x)
  (fixed-point
    (average-damp (lambda (y) (/ x y)))
    1.0))
(define dx 0.00001)
(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))
(define (newton f guess)
  (let ((df (deriv f)))
    (fixed-point
      (lambda (x) (- x (/ (f x) (df x))))
      guess)))
(define (my-sqrt x)
  (newton
    (lambda (y) (- (square y) x))
    1.0))

;;; ex 1.40
(define (cubic a b c)
  (lambda (x)
    (let ((xx (square x)))
      (+ (* x xx)
         (* a xx)
         (* b x)
         c))))

;;; ex 1.41
(define (double f)
  (lambda (x)
    (f (f x))))
(check
  (((double (double double)) inc) 5)
  => (((double (lambda (f) (double (double f)))) inc) 5)
  => (((lambda (f) (double (double (double (double f))))) inc) 5)
  => ((double (double (double (double inc)))) 5)
  => ((double (double (double (lambda (x) (inc (inc x)))))) 5)
  => ((double (double (lambda (x) (inc (inc (inc (inc x))))))) 5)
  => ((double (lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))) 5)
  => ((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc
       (inc (inc (inc (inc x))))))))))))))))) 5)
  => (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc
       (inc 5))))))))))))))))
  => 21)

;;; ex 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))
(check
  ((compose square inc) 6) => 49)

;;; ex 1.43
(define (repeated f n)
  (if (= n 1)
    f
    (compose (repeated f (- n 1)) f)))
(check
  ((repeated square 2) 5) => 625)

;;; ex 1.44
(define dx 0.1)
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))
(check
  ((smooth square) 2) ~> 4.006666666666667
  (((repeated smooth 5) square) 2) ~> 4.033333333333333)

;;; ex 1.45
(define (my-sqrt x)
  (fixed-point
    (average-damp (lambda (y) (/ x y)))
    1.0))
(define (cbrt x)
  (fixed-point
    (average-damp (lambda (y) (/ x (square y))))
    1.0))
(define (nth-root x n)
  (fixed-point
    ((repeated average-damp
               (floor (/ (log n) (log 2))))
     (lambda (y) (/ x (expt y (- n 1)))))
    1.0))
(check
  (nth-root 4 2) ~> 2.000000000000002
  (nth-root 256 8) ~> 2.0000000000039666
  (nth-root 1048576 20) ~> 1.999999063225966)

;;; ex 1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
      guess
      (iter (improve guess))))
  iter)
(define (my-sqrt x)
  ((iterative-improve
     (lambda (guess)
       (< (abs (- (square guess) x)) tolerance))
     (lambda (guess)
       (average guess (/ x guess))))
   1.0))
(check
  (my-sqrt 2) ~> 1.4142156862745097)
(define (fixed-point f first-guess)
  ((iterative-improve
     (lambda (guess)
       (< (abs (- guess (f guess))) tolerance))
     f)
   first-guess))
(check
  (fixed-point cos 1.0) ~> 0.7390893414033928)
;; This is slightly different from the original fixed-point implementation
;; because it returns `guess` when it's good enough, not `next` (that is, the
;; original always does one more improvement).
|#
