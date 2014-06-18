;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.
;;; (exercises from SICP)

;;;;; Chapter 1: Building Abstractions with Procedures

;;;; Section 1.1: The elements of programming

;;; ex 1.1
10 ; => 10
(+ 5 3 4) ; => 12
(- 9 1) ; => 8
(/ 6 2) ; => 3
(+ (* 2 4) (- 4 6)) ; => -16
(define a 3)
(define b (+ a 1))
(+ a b (* a b)) ; => 19
(= a b) ; => #f
(if (and (> b a) (< b (* a b))) b a) ; => 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;; => 16
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;; => 16

;;; ex 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;;; ex 1.3
(define (ex-1.3 a b c)
  (cond
    ((and (<= a b) (<= a c))
     (+ (* b b) (* c c)))
    ((and (<= b a) (<= b c))
     (+ (* a a) (* c c)))
    ((and (<= c a) (<= c b))
     (+ (* a a) (* b b)))))

;;; ex 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; The operator evaluates to + (addition) when b is positive, and to -
;; (subtraction) when b is negative. Subtracting a negative is equivalent to
;; adding its absolute value, so this procedure performs a + |b| in all cases.

;;; ex 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
(test 0 (p)) ; => no value
;; applicative: The expression will never return a value because the interpreter
;; tries to evaluate (p) and enters endless recursion.
;; normal: The expression will evaluate to zero. The (p) expression is never
;; evaluated because it is not necessary to do so.

;;; example 1.1.7 (Newton sqrt)
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

;;; ex 1.6
;; When Alyssa attempts to use this to compute square roots, it will not work.
;; The sqrt procedure will never return a value because it gets stuck in
;; sqrt-iter due to infinite recursion. The new-if combination always evaluates
;; the else-clause, which contains the recursive call, so the reucursion will
;; never end.

;;; ex 1.7
;; The good-enough? predicate does not work well for small numbers because the
;; tolerance is a fixed amount. It can't be too small or else it will take too
;; long to compute the square roots of large numbers, but at the same time, it
;; is impossible to use the procedure for values smaller than the tolerance.
(sqrt 0.000002) ; => 0.0312713096020622 (should be 0.0014142...)
(square (sqrt 0.000002)) ; => 0.000977894804228028
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
(sqrt 0.000002) ; => 0.00141421356261785
(square (sqrt 0.000002)) ; => 2.00000000069227e-06

;;; ex 1.8
(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))
(define (cbrt-iter guess x)
  (let ((better (improve guess x)))
    (if (good-enough? guess better)
      better
      (cbrt-iter better x))))

;;;; Section 1.2: Procedures and the processes they generate

;;; ssec 1.2.1 (factorial)
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
;; r+ generates a recursive process
(r+ 4 5)
(inc (r+ 3 5))
(inc (inc (r+ 2 5)))             ; expanding
(inc (inc (inc (r+ 1 5))))
(inc (inc (inc (inc (r+ 0 5))))) ; 4 deffered operations
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))              ; contracting
(inc (inc 7))
(inc 8)
9
;; i+ generates an iterative process
(i+ 4 5)
(i+ 3 6)
(i+ 2 7)
(i+ 1 8)
(i+ 0 9)
9

;;; ex 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
(A 1 10) ; => 1024
(A 2 4)  ; => 65536
(A 3 3)  ; => 65536
(define (f n) (A 0 n))   ; 2n
(define (g n) (A 1 n))   ; 2^n
(define (h n) (A 2 n))   ; 2^2^2... (n 2s)
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
;; see exercise-1.13.pdf

;;; ex 1.14
;; count-change
;; steps: Θ(n^5) because there are 5 types of coins
;; space: Θ(n) because the max depth of the tree grows linearly
;; Remember: for a tree-recursive process, space is proportional to the maximum
;; depth of the tree, and the number of steps is the number of 

;;; ex 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine theta)
  (if (<= (abs theta) 0.1)
    theta
    (p (sine (/ theta 3.0)))))
;; (a) The procedure p is evaluated five times when (sine 12.15 is evaluated:
(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05)))))) ; five times when theta ≤ 0.1
;; (b) The order of growth for sine:
;; During the process, p is evaluated n times such that a/3^n ≤ 0.1. Solving for
;; n gives us n = log(10a)/log(3), therefore the number of steps for sine grows
;; as Θ(log(n)).
;; The interpreter must maintain the stack for that number of calls to p,
;; therfore the space complexity is also Θ(log(n)).

;;; ssec 1.2.4 (exponentiation)
;; steps Θ(n), space Θ(n)
(define (expt-rec b n)
  (if (= n 0)
    1
    (* b (expt-rec b (- n 1)))))
;; steps Θ(n), space Θ(1)
(define (expt-it b n)
  (define (helper counter prod)
    (if (= counter 0)
      prod
      (helper (- count 1) (* prod b))))
  (helper n 1))
;; steps Θ(log(n)), space Θ(log(n))
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
;; iterative, steps θ(log(n))
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;;; ex 1.20
;; applicative order: 4 remainder operations
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2
;; normal order: 18 remainder operations
;; Each b gets evaluated once in the (= b 0) predicate (14 ops).
;; The final a gets evaluated in the end (4 ops).
;; 14 + 4 = 18
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd (remainder 206 40)
     (remainder 40 (remainder 206 40)))
(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40))))
(gcd (remainder (remainder 206 40)
                (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40))
                (remainder (remainder 206 40)
                           (remainder 40 (remainder 206 40)))))
(remainder (remainder 206 40)
           (remainder 40 (remainder 206 40)))

;;; ssec 1.2.6 (primality)
;; trial division
(define (prime? n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (smallest-divisor n)
    (find-divisor n 2))
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
(smallest-divisor 199)   ; => 199
(smallest-divisor 1999)  ; => 1999
(smallest-divisor 19999) ; => 7
 
;;; ex 1.22
(define (runtime)
  (time->seconds (current-time)))
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
;; The data seems to bear out the Θ(sqrt(n)) prediction. The larger the numbers,
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
;; actually SLOWER for primes under 10000. Even for seven-figure primes, this
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
;; Since the Fermat test has Θ(log(n)) grwoth, I expected the time to the primes
;; near 1000000 to be only a bit greater than the time needed to test primes
;; near 1000. The data bears this out -- for each additional order of magnitude
;; of the primes, the time required increases by a small, constant amount.
;; Specifically, primes that are 10 times larger take about 0.001 seconds longer
;; to test using the Fermat method. It should be noted that these results may be
;; dependent on the choice of 100 as the second argument to fast-prime? (the
;; exercise did not specify what value to use).

;;; ex 1.25
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
;; This procedure works, but it is not as efficient. The Fermat test takes MUCH
;; longer using this version of expmod -- longer by three orders of magnitude.
;; While fast-expt is reasonably fast, the original expmod procedure is much
;; faster. The key to its implmentation is not only successive squaring (which
;; fast-expt does as well in this new procedure), but that it calls remainder
;; between each squaring. This new procedure does not, so the value becomes
;; enormous (requiring bignums, which is slow) by the time the remainder is
;; finally taken. Suppose we test the primality of n = 9, choosing a = 5.
;; Using the old definiton of expmod, the process will evolve like so:
(define r remainder)
(define s square)
(expmod 5 9 9)
(r (* 5 (expmod 5 8 9)) 9)
(r (* 5 (r (s (expmod 5 4 9)) 9)) 9)
(r (* 5 (r (s (r (s (expmod 5 2 9)) 9)) 9)) 9)
(r (* 5 (r (s (r (s (r (s (expmod 5 1 9)) 9)) 9)) 9)) 9)
(r (* 5 (r (s (r (s (r (s (r (* 5 (expmod 5 0 9)) 9)) 9)) 9)) 9)) 9)
(r (* 5 (r (s (r (s (r (s (r (* 5 1) 9)) 9)) 9)) 9)) 9)
(r (* 5 (r (s (r (s (r (s (r 5 9)) 9)) 9)) 9)) 9)
(r (* 5 (r (s (r (s (r (s 5) 9)) 9)) 9)) 9)
(r (* 5 (r (s (r (s (r 25 9)) 9)) 9)) 9)
(r (* 5 (r (s (r (s 7) 9)) 9)) 9)
(r (* 5 (r (s (r 49 9)) 9)) 9)
(r (* 5 (r (s 4) 9)) 9)
(r (* 5 (r 16 9)) 9)
(r (* 5 7) 9)
(r 35 9)
8
;; Compare this to the evolution of the process using the new expmod:
(expmod 5 9 9)
(r (fast-expt 5 9) 9)
(r 1953125 9)
;; The original expmod doesn't need to deal with numbers anywhere near that
;; size, so it is much more efficient. This number may seem okay, but it will
;; grow exponentially with n (by definition), and will quickly require arbitrary
;; precision integer math (bignum), which is much slower than 32-bit arithmetic.

;;; ex 1.26
;; When the square combination is evaluated, the expmod combiantion is evaluated
;; once and then its value is substituted into the square compound procedure
;; according to the substitution model. When the squaring is written as an
;; explicit multiplication, the expmod combination is evaluated TWICE. The
;; interpreter has no way of knowing that they will have the same value. This
;; transforms a linear recusive process into a tree-recursive process. The time
;; complexity of this tree-recursive process is Θ(log(2^n)), or simply Θ(n).

;;; ex 1.27
(define (fermat-all? n)
  (define (helper a)
    (if (< a n)
      (and (= (expmod a n n) a)
           (helper (+ a 1)))
      #t))
  (helper 1))
;; These Carmichael numbers pass the Fermat tests for all values of a < n:
(fermat-all? 561)  ; => #t
(fermat-all? 1105) ; => #t
(fermat-all? 1729) ; => #t
(fermat-all? 2465) ; => #t
(fermat-all? 2821) ; => #t
(fermat-all? 6601) ; => #t
;; According to the trial divison procedure, none of them are prime:
(prime? 561)  ; => #f
(prime? 1105) ; => #f
(prime? 1729) ; => #f
(prime? 2465) ; => #f
(prime? 2821) ; => #f
(prime? 6601) ; => #f

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
(p? 32)      ; => #f
(p? 100)     ; => #f
(p? 1000004) ; => #f 
;; known prime numbers
(p? 5)       ; => #t
(p? 997)     ; => #t
(p? 1000037) ; => #t
;; known Carmichael numbers
(p? 561)     ; => #f
(p? 1105)    ; => #f
(p? 1729)    ; => #f
(p? 2465)    ; => #f
(p? 2821)    ; => #f
(p? 6601)    ; => #f

;;;; Section 1.3: Formulating abstractions with higher-order procedures

;;; ssec 1.3.1 (procedures as args)
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))
(define (indentity x) x)
(define (inc x) (+ x 1))

;;; ex 1.29
(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (term k)
      (* (f (+ a (* k h)))
         (if (or (= k 0) (= k n))
           1
           (+ 2 (* 2 (remainder k 2))))))
    (* h (sum term 0 inc n))))
;; The integral procedure is a bit inaccurate, whereas the simpson procedure
;; gives the exact answer even when n = 2. This is much better.
(integral cube 0 1 0.01)  ; => 0.24998750000000042
(integral cube 0 1 0.001) ; => 0.249999875000001
(simpson cube 0 1 2)      ; => 3/4
(simpson cube 0 1 100)    ; => 3/4
(simpson cube 0 1 1000)   ; => 3/4

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
(factorial 5) ; => 120
(factorial 7) ; => 5040
(define (approx-qpi n)
  (define (term k)
    (let ((r (remainder k 2)))
      (/ (+ k 2 (- r))
         (+ k 1 r))))
  (product term 1.0 inc n))
(* 4 (approx-qpi 10))    ; => 3.2751010413348065
(* 4 (approx-qpi 100))   ; => 3.1570301764551654
(* 4 (approx-qpi 1000))  ; => 3.1431607055322552
(* 4 (approx-qpi 10000)) ; => 3.1417497057380084

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
(f square) ; => 4
(f (lambda (z) (* z (+ z 1)))) ; => 6
;; If we try evaluating the combination (f f), we get the following process:
(f f)
(f 2)
(2 2)
;; This gives an error, since 2 does not evaluate to a procedure. We cannot
;; apply 2 to the argument 2 because that doesn't make any sense.

;;; ssec 1.3.3 (procs as general methods)
;; half-interval method for finding zeros
;; time complexity: Θ(log(L/T)) where L is original |a-b| and T is tolerance
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
            (error "Values are not of opposite sign" a b)))))
(half-interval-method sin 2.0 4.0) ; => 3.141590118408203
;; fixed point
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))
(fixed-point cos 1.0) ; => 0.7390822985224023

;;; ex 1.35
;; see exercise-1.35.pdf
(fixed-point (lambda (x) (+ 1 (/ x))) 42.0) ; => 1.6180328499442242

;;; ex 1.36
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))
(define (f x) (/ (log 1000) (log x)))
(fixed-point f 5)
4.29202967422018
; ... 27 approximations ...
4.555539314360711
(fixed-point (lambda (x) (average x (f x))) 5)
4.64601483711009
; ... 7 approximations ...
4.5555361005218895
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
(approx-gr 1)  ; => 1.0
(approx-gr 2)  ; => 0.5
(approx-gr 3)  ; => 0.6666666666666666
(approx-gr 4)  ; => 0.6000000000000001
(approx-gr 5)  ; => 0.625
(approx-gr 6)  ; => 0.6153846153846154
(approx-gr 7)  ; => 0.6190476190476191
(approx-gr 8)  ; => 0.6176470588235294
(approx-gr 9)  ; => 0.6181818181818182
(approx-gr 10) ; => 0.6179775280898876
(approx-gr 11) ; => 0.6180555555555556
;; When k = 11, the value is accurate to 4 decimal places.

;;; ex 1.38
(define (approx-e k)
  (define (d i)
    (if (zero? (remainder (+ i 1) 3))
      (* 2/3 (+ i 1))
      1))
  (+ 2 (count-frac always-one d k)))
(approx-e 1)    ; => 3.0
(approx-e 2)    ; => 2.6666666666666665
(approx-e 3)    ; => 2.75
(approx-e 4)    ; => 2.7142857142857144
(approx-e 5)    ; => 2.71875
(approx-e 1000) ; => 2.7182818284590455

;;; ex 1.39
(define (tan-cf x k)
  (count-frac
    (lambda (i) (if (= i 1) x (- (square x))))
    (lambda (i) (- (* i 2) 1))
    k))
(define pi (/ (arctan 1) 4))
(tan-cf (/ pi 3) 1) ; => 1.0471975511965976
(tan-cf (/ pi 3) 2) ; => 1.650535956338694
(tan-cf (/ pi 3) 3) ; => 1.7291124259895505
(tan-cf (/ pi 3) 4) ; => 1.7319971836379957
(tan-cf (/ pi 3) 5) ; => 1.7320501979592633
(tan (/ pi 3))      ; => 1.7320508075688767

;;; ssec 1.3.4 (returning procs)
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (sqrt x)
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
(define (sqrt x)
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
(((double (double double)) inc) 5)
(((double (lambda (f) (double (double f)))) inc) 5)
(((lambda (f) (double (double (double (double f))))) inc) 5)
((double (double (double (double inc)))) 5)
((double (double (double (lambda (x) (inc (inc x)))))) 5)
((double (double (lambda (x) (inc (inc (inc (inc x))))))) 5)
((double (lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))) 5)
((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc x))))))))))))))))) 5)
(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))))
21

;;; ex 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))
((compose square inc) 6) ; => 49

;;; ex 1.43
(define (repeated f n)
  (if (= n 1)
    f
    (compose (repeated f (- n 1)) f)))
((repeated square) 5) ; => 625

;;; ex 1.44
(define dx 0.1)
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))
((smooth square) 2)              ; => 4.006666666666667
(((repeated smooth 5) square) 2) ; => 4.033333333333333

;;; ex 1.45
(define (sqrt x)
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
(nth-root 4 2)        ; => 2.000000000000002
(nth-root 256 8)      ; => 2.0000000000039666
(nth-root 1048576 20) ; => 1.999999063225966

;;; ex 1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
      guess
      (iter (improve guess))))
  iter)
(define (sqrt x)
  ((iterative-improve
     (lambda (guess)
       (< (abs (- square guess) x) tolerance))
     (lambda (guess)
       (average guess (/ x guess))))
   1.0))

;;;;; Chapter 2: Building Abstractions with Data

;;;; Section 2.1: Introduction to data abstraction

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

;;;; Section 2.2: Hierarchical data the closure property

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

;;; ssec 2.2.3 (seqs as conventional interfaces)
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

;;; ssec 2.2.3 (seqs as conventional interfaces)
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
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (mapcat
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enum-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
