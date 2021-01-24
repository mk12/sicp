;;; Copyright 2021 Mitchell Kember. Subject to the CC BY-SA 4.0 License.

#!r6rs

(library (src sicp chapter-1)
  (export chapter-1-effects)
  (import (rnrs base (6))
          (src lang sicp))

(define chapter-1-effects)

(SICP

(Chapter :1 "Building Abstractions with Procedures")

(Section :1.1 "The Elements of Programming")

(Section :1.1.1 "Expressions")

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

(Section :1.1.2 "Naming and the Environment")

(define size 2)
size => 2
(* 5 size) => 10

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius)) ~> 314.159

(define circumference (* 2 pi radius))
circumference ~> 62.8318

(Section :1.1.3 "Evaluating Combinations")

(* (+ 2 (* 4 6))
   (+ 3 5 7))
=> 390

(Section :1.1.4 "Compound Procedures")

(define (square x) (* x x))
(square 21) => 441
(square (+ 2 5)) => 49
(square (square 3)) => 81

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(sum-of-squares 3 4) => 25

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(f 5) => 136

(Section :1.1.5 "The Substitution Model for Procedure Application"
  (use (:1.1.4 f square sum-of-squares)))

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
=> (+ (* 6 6) (* 10 10))
=> (+ 36 100)
=> 136

(Section :1.1.6 "Conditional Expressions and Predicates")

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y) (or (> x y) (= x y)))
(define (>= x y) (not (< x y)))

(Exercise ?1.1)

10 => 10
(+ 5 3 4) => 12
(- 9 1) => 8
(/ 6 2) => 3
(+ (* 2 4) (- 4 6)) => 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b)) => 19
(= a b) => #f
(if (and (> b a) (< b (* a b)))
    b
    a)
=> 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
=> 16

(+ 2 (if (> b a) b a)) => 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
=> 16

(Exercise ?1.2)

(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))
=> -37/150

(Exercise ?1.3)

(define (f a b c)
  (cond ((and (<= a b) (<= a c))
         (+ (* b b) (* c c)))
        ((and (<= b a) (<= b c))
         (+ (* a a) (* c c)))
        ((and (<= c a) (<= c b))
         (+ (* a a) (* b b)))))

(f 1 2 3) => 13
(f 3 2 1) => 13

(Exercise ?1.4)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; The operator evaluates to `+` (addition) when `b` is positive, and to `-`
;; (subtraction) when `b` is negative. Subtracting a negative is equivalent to
;; adding its absolute value, so this procedure returns $a + \abs{b}$ in all
;; cases.

(Exercise ?1.5)

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

; (test 0 (p)) ; never terminates

;; With applicative-order evaluation, the expression will never return a value
;; because the interpreter tries to evaluate `(p)` and enters endless recursion.
;;
;; With normal-order evaluation, the expression will evaluate to zero. The `(p)`
;; expression is never evaluated because it is not necessary to do so.

(Section :1.1.7 "Example: Square Roots by Newton's Method"
  (use (:1.1.4 square)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
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

(Exercise ?1.6
  (use (:1.1.7 good-enough? improve)))

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
;; The `sqrt` procedure will never return a value because it gets stuck in
;; `sqrt-iter` due to infinite recursion. The `new-if` combination always
;; evaluates the else-clause, which contains the recursive call, so the
;; recursion will never end.

(Exercise ?1.7
  (use (:1.1.7 improve sqrt)))

;; The `good-enough?` predicate does not work well for small numbers because the
;; tolerance is fixed. If the number is smaller than the tolerance, the result
;; will be hopelessly inaccurate, like measuring an atom with a yard stick.

(sqrt 0) ~> 0.03125     ; should be 0
(sqrt 1e-20) ~> 0.03125 ; should be 1e-10

;; It is also inadequate for very large numbers. With limited precision, it is
;; impossible to represent small differences between large numbers. This means
;; the algorithm might never terminate, because the guess never satisfies
;; `good-enough?` no matter how many times `improve` is called.

(sqrt 1e14) ~> 1e7
; (sqrt 1e20) ; never terminates

;; Here is an alternative implementation of `sqrt` that watches how `guess`
;; changes from one iteration to the next and stops when the change is a very
;; small fraction of the guess.

(define (good-enough? g1 g2)
  ;; Note: unlike `infinite?`, the negation of `finite?` catches NaN.
  (or (not (finite? g2))
      (< (/ (abs (- g2 g1)) g1)
         0.001)))
(define (sqrt-iter guess x)
  (let ((better (improve guess x)))
    (if (good-enough? guess better)
        guess
        (sqrt-iter better x))))
(set! sqrt (lambda (x) (sqrt-iter 1.0 x)))

;; (We use `set!` because shadowing imports with `define` is [not
;; allowed](:language#imports).)
;;
;; The results for small numbers are much better. For zero, it works because
;; even though the `guess` change is always a 50% reduction, it eventually gets
;; so small that it becomes NaN, and the algorithm terminates.

(sqrt 0) ~> 0
(sqrt 1e-20) ~> 1e-10

;; For large numbers, it works better in that it always terminates. However,
;; the results are less precise because "a small fraction" change in `guess` can
;; be fairly large when the guesses themselves are very large.

(sqrt 1e14) ~> 1.0000029650278373e7
(sqrt 1e20) ~> 1.0000021484861237e10

(Exercise ?1.8
  (use (:1.1.4 square)))

(define (cube x) (* x x x))
(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))
(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))
(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))
(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 8) ~> 2.000004911675504

(Section :1.1.8 "Procedures as Black-Box Abstractions"
  (use (:1.1.7 average)))

;; The following two procedures should be indistinguishable:
(define (square x) (* x x))
(define (square x) (exp (+ (log x) (log x))))

;; These should be indistinguishable as well:
(define (square x) (* x x))
(define (square y) (* y y))

;; Using block structure:
(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x) (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

;; Without passing `x` around:
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(Section :1.2 "Procedures and the Processes They Generate")

(Section :1.2.1 "Linear Recursion and Iteration")

;; Linear recursive process:

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 6)
=> (* 6 (factorial 5))
=> (* 6 (* 5 (factorial 4)))
=> (* 6 (* 5 (* 4 (factorial 3))))
=> (* 6 (* 5 (* 4 (* 3 (factorial 2)))))
=> (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
=> (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
=> (* 6 (* 5 (* 4 (* 3 2))))
=> (* 6 (* 5 (* 4 6)))
=> (* 6 (* 5 24))
=> (* 6 120)
=> 720

;; Linear iterative process:

(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(factorial 6)
=> (fact-iter 1 1 6)
=> (fact-iter 1 2 6)
=> (fact-iter 2 3 6)
=> (fact-iter 6 4 6)
=> (fact-iter 24 5 6)
=> (fact-iter 120 6 6)
=> (fact-iter 720 7 6)
=> 720

(Exercise ?1.9)

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (r+ a b)
  (if (= a 0) b (inc (r+ (dec a) b))))

(define (i+ a b)
  (if (= a 0) b (i+ (dec a) (inc b))))

;; `r+` generates a recursive process:
(r+ 4 5)
=> (inc (r+ 3 5))
=> (inc (inc (r+ 2 5)))             ; expanding
=> (inc (inc (inc (r+ 1 5))))
=> (inc (inc (inc (inc (r+ 0 5))))) ; 4 deferred operations
=> (inc (inc (inc (inc 5))))
=> (inc (inc (inc 6)))              ; contracting
=> (inc (inc 7))
=> (inc 8)
=> 9

;; `i+` generates an iterative process:
(i+ 4 5)
=> (i+ 3 6)
=> (i+ 2 7)
=> (i+ 1 8)
=> (i+ 0 9)
=> 9

(Exercise ?1.10)

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10) => 1024
(A 2 4) => 65536
(A 3 3) => 65536

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

;; `(f n)` computes $2n$, since `(A 0 n) => (* 2 n)`.

;; `(g n)` computes $2^n$, since
;; `(A 1 n) => (A 0 (A 1 (- n 1))) => (f (g (- n 1)))`.

;; `(h n)` computes ${^{n}2}$
;; ([tetration](https://en.wikipedia.org/wiki/Tetration)):
;; `(A 2 n) => (A 1 (A 2 (- n 1))) => (g (h (- n 1)))`.

;; `(k n)` computes $5n^2$, as stated in the exercise.

(Section :1.2.2 "Tree Recursion")

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(fib 6) => 8

(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

(fib 6) => 8

(Section :1.2.2.1 "Example: Counting change")

(define (count-change amount)
  (define (cc a n)
    (cond ((< a 0) 0)
          ((= a 0) 1)
          ((= n 0) 0)
          (else (+ (cc a (- n 1))
                   (cc (- a (first-denomination n)) n)))))
  (cc amount 5))

(define (first-denomination kinds-of-coins)
  (vector-ref '#(1 5 10 25 50) (- kinds-of-coins 1)))

(count-change 100) => 292

(Exercise ?1.11)

;; Recursive process:
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(f 5) => 25

;; Iterative process:
(define (f n)
  (define (iter a b c counter)
    (if (= counter 0)
        a
        (iter b c (+ c (* 2 b) (* 3 a)) (- counter 1))))
  (iter 0 1 2 n))

(f 5) => 25

(Exercise ?1.12)

(define (pascal i j)
  (if (or (= j 0) (= j i))
      1
      (+ (pascal (- i 1) (- j 1))
         (pascal (- i 1) j))))

(pascal 3 0) => 1
(pascal 3 1) => 3
(pascal 3 2) => 3
(pascal 3 3) => 1

(Exercise ?1.13)

;; The constants $\varphi$ and $\psi$ are the solutions to golden ratio equation
;; $x+1=x^2$:
;;
;; $$\varphi=\frac{1+\sqrt5}{2},\qquad\psi=\frac{1-\sqrt5}{2}.$$
;;
;; The Fibonacci sequence is defined recursively by
;;
;; $$\begin{aligned}
;; \Fib(0) &= 0, \\
;; \Fib(1) &= 1, \\
;; \Fib(n) &= \Fib(n-1) + \Fib(n-2).
;; \end{aligned}$$
;;
;; **Lemma.** $\Fib(n)$ is equal to $f(n)=\dfrac{\varphi^n-\psi^n}{\sqrt5}.$
;;
;; First, we will demonstrate three base cases. When $n=0$,
;;
;; $$f(0)=\frac{\varphi^0-\psi^0}{\sqrt5} = \frac{1-1}{\sqrt5} = 0.$$
;;
;; When $n=1$,
;;
;; $$f(1)=\frac{\varphi^1-\psi^1}{\sqrt5} =
;; \frac{\frac{1+\sqrt5}{2}-\frac{1-\sqrt5}{2}}{\sqrt5} =
;; \frac{\frac{2\sqrt5}{2}}{\sqrt5} = 1.$$
;;
;; When $n=2$,
;;
;; $$\begin{aligned}
;; f(2) &= \frac{\varphi^2-\psi^2}{\sqrt5} \\
;; &= \frac{\left(\frac{1+\sqrt5}{2}\right)^2 -
;;    \left(\frac{1-\sqrt5}{2}\right)^2}{\sqrt5} \\
;; &= \frac{\frac{(1+\sqrt5)^2-(1-\sqrt5)^2}{4}}{\sqrt5} \\
;; &= \frac{\left(\left(1+\sqrt5\right)-\left(1-\sqrt5\right)\right)
;;    \left(\left(1+\sqrt5\right)+\left(1-\sqrt5\right)\right)}{4\sqrt5} \\
;; &= \frac{\left(2\sqrt5\right)(2)}{4\sqrt5} \\
;; &= 1.
;; \end{aligned}$$
;;
;; Now comes the inductive step:
;;
;; $$\begin{aligned}
;; f(n-1)+f(n-2) &= \frac{\varphi^{n-1}-\psi^{n-1}}{\sqrt5} +
;; \frac{\varphi^{n-2}-\psi^{n-2}}{\sqrt{5}} \\
;; &= \frac{\left(\varphi^{n-1}+\varphi^{n-2}\right) -
;;    \left(\psi^{n-1}+\psi^{n-2}\right)}{\sqrt5} \\
;; &= \frac{\varphi^n\left(\varphi^{-1}+\varphi^{-2}\right) -
;;    \psi^n\left(\psi^{-1}+\psi^{-2}\right)}{\sqrt5} \\
;; &= \frac{\varphi^n\varphi^{-1}\left(1+\varphi^{-1}\right) -
;;    \psi^n\psi^{-1}\left(1+\psi^{-1}\right)}{\sqrt5} \\
;; &= \frac{\varphi^n\varphi^{-1}\left(\varphi\right) -
;;    \psi^n\psi^{-1}\left(\psi\right)}{\sqrt5} \\
;; &= \frac{\varphi^n-\psi^n}{\sqrt5} \\
;; &= f(n).
;; \end{aligned}$$
;;
;; By induction, $f(n)=\Fib(n)$ for all $n$. $\blacksquare$
;;
;; **Theorem.** $\Fib(n)$ is the closest integer to $\varphi^n/\sqrt5$, where
;; $\varphi=(1+\sqrt5)/2$.
;;
;; For this to hold, the absolute difference must be less than one half:
;;
;; $$\begin{aligned}
;; \abs{\Fib(n)-\frac{\varphi^n}{\sqrt5}} &< \frac12 \\
;; \abs{\frac{\varphi^n-\psi^n}{\sqrt5} - \frac{\varphi^n}{\sqrt5}} &< \frac12\\
;; \abs{-\frac{\psi^n}{\sqrt5}} &< \frac12 \\
;; \frac{\abs{-\psi^n}}{\sqrt5} &< \frac12 \\
;; \abs{\psi^n} &< \frac{\sqrt5}{2} \\
;; \abs{\psi}^n &< \frac{\sqrt5}{2}.
;; \end{aligned}$$
;;
;; Since $\abs{\psi}<0.619<1$, we have $\abs{\psi}^n<1<\dfrac{\sqrt5}{2}$ for
;; all $n$. $\blacksquare$

(Section :1.2.3 "Orders of Growth")

(Exercise ?1.14
  (use (:1.2.2.1 count-change)))

(count-change 11) => 4

;; Here is the process generated by `(count-change 11)`:
;;
;; ```
;; (cc 11 5)
;;   (cc -39 5) => 0
;;   (cc 11 4)
;;     (cc -14 4) => 0
;;     (cc 11 3)
;;       (cc 1 3)
;;         (cc -9 3) => 0
;;         (cc 1 2)
;;           (cc -4 2) => 0
;;           (cc 1 1)
;;             (cc 0 1) => 1
;;             (cc 1 0) => 0
;;       (cc 11 2)
;;         (cc 6 2)
;;           (cc 1 2)
;;             (cc -4 2) => 0
;;             (cc 1 1)
;;               (cc 0 1) => 1
;;               (cc 1 0) => 0
;;           (cc 6 1)
;;             (cc 5 1)
;;               (cc 4 1)
;;                 (cc 3 1)
;;                   (cc 2 1)
;;                     (cc 1 1)
;;                       (cc 0 1) => 1
;;                       (cc 1 0) => 0
;;                     (cc 2 0) => 0
;;                   (cc 3 0) => 0
;;                 (cc 4 0) => 0
;;               (cc 5 0) => 0
;;             (cc 6 0) => 0
;;         (cc 11 1)
;;           (cc 10 1)
;;             (cc 9 1)
;;               (cc 8 1)
;;                 (cc 7 1)
;;                   (cc 6 1)
;;                     (cc 5 1)
;;                       (cc 4 1)
;;                         (cc 3 1)
;;                           (cc 2 1)
;;                             (cc 1 1)
;;                               (cc 0 1) => 1
;;                               (cc 1 0) => 0
;;                             (cc 2 0) => 0
;;                           (cc 3 0) => 0
;;                         (cc 4 0) => 0
;;                       (cc 5 0) => 0
;;                     (cc 6 0) => 0
;;                   (cc 7 0) => 0
;;                 (cc 8 0) => 0
;;               (cc 9 0) => 0
;;             (cc 10 0) => 0
;;           (cc 11 0) => 0
;; ```
;;
;; Orders of growth:
;;
;; - Steps: $\Theta(n^5)$ because there are 5 types of coins.
;; - Space: $\Theta(n)$ because the maximum depth of the tree grows linearly.
;;
;; Remember: for a tree-recursive process, space is proportional to the maximum
;; depth of the tree, and the number of steps is the number of leaves.

(Exercise ?1.15)

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine theta)
  (if (<= (abs theta) 0.1)
      theta
      (p (sine (/ theta 3.0)))))

;; (a) The procedure `p` is evaluated five times for `(sine 12.15)`:
(sine 12.15)
~> (p (sine 4.05))
~> (p (p (sine 1.35)))
~> (p (p (p (sine 0.45))))
~> (p (p (p (p (sine 0.15)))))
~> (p (p (p (p (p (sine 0.05)))))) ; five times until theta <= 0.1

;; (b) During the process, `p` is evaluated $k$ times such that
;; $\theta/3^k\lt0.1$. Solving for $k$ gives $k = \log10\theta/\log3$, thus
;; the number of steps for `sine` grows as $\Theta(\log n)$. The interpreter
;; must maintain the stack for that number of calls to `p`, therefore the space
;; complexity is also $\Theta(\log n)$.

(Section :1.2.4 "Exponentiation"
  (use (:1.1.4 square)))

;; Recursive, naive: $\Theta(n)$ time, $\Theta(n)$ space.
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(expt 2 5) => 32

;; Iterative, naive: $\Theta(n)$ time, $\Theta(1)$ space.
(define (expt b n)
  (define (iter counter prod)
    (if (= counter 0)
        prod
        (iter (- counter 1) (* prod b))))
  (iter n 1))

(expt 2 5) => 32

;; Recursive, successive squaring: $\Theta(\log n)$ time, $\Theta(\log n)$
;; space.
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(fast-expt 2 5) => 32

(Exercise ?1.16
  (use (:1.1.4 square)))

;; Iterative, successive squaring: $\Theta(\log n)$ time, $\Theta(1)$ space.
(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(fast-expt 2 5) => 32
(fast-expt 2 100) => 1267650600228229401496703205376

(Exercise ?1.17)

;; Recursive, naive: $\Theta(n)$ time, $\Theta(n)$ space.
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(* 5 4) => 20

;; These are taken as primitives:
(define (double x) (+ x x))
(define (halve x) (/ x 2))

;; Recursive, successive doubling: $\Theta(\log n)$ time, $\Theta(\log n)$
;; space.
(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-* a (halve b))))
        (else (+ a (fast-* a (- b 1))))))

(fast-* 5 4) => 20

(Exercise ?1.18
  (use (?1.17 double halve)))

;; Iterative, successive doubling: $\Theta(\log n)$ time, $\Theta(1)$ space.
(define (fast-* a b)
  (define (iter c a b)
    (cond ((= b 0) c)
          ((even? b) (iter c (double a) (halve b)))
          (else (iter (+ c a) a (- b 1)))))
  (iter 0 a b))

(fast-* 5 4) => 20

(Exercise ?1.19)

;; Let $T_{pq}(a, b) = (bq + aq + ap, bp + aq)$. Applying $T_{pq}$ twice gives
;;
;; $$\begin{aligned}
;; &\phantom{=} T_{pq}(T_{pq}(a, b)) \\
;; &= T_{pq}(bq + aq + ap, bp + aq) \\
;; &= ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p, \\
;; &\phantom{= (} (bp + aq)p + (bq + aq + ap)q) \\
;; &= (bpq + aq^2 + bq(q + p) + aq(q + p) + ap(q + p), \\
;; &\phantom{= (} bp^2 + aqp + bq^2 + aq^2 + apq) \\
;; &= (bpq + aq^2 + bq^2 + bpq + aq^2 + apq + apq + ap^2, \\
;; &\phantom{= (} bp^2 + apq + bq^2 + aq^2 + apq) \\
;; &= (b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2), \\
;; &\phantom{= (} b(p^2 + q^2) + a(q^2 + 2pq)) \\
;; &= T_{p'q'}(a, b)
;; \end{aligned}$$
;;
;; where $p' = p^2 + q^2$ and $q' = q^2 + 2pq$.

;; Using this, we can implement `fib` with $\Theta(\log n)$ time complexity:

(define (fib n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a
                 b
                 (+ (* p p) (* q q))
                 (+ (* q q) (* 2 p q))
                 (/ count 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))
  (iter 1 0 0 1 n))

(fib 6) => 8
(fib 100) => 354224848179261915075

(Section :1.2.5 "Greatest Common Divisors")

;; Euclid's Algorithm: $\Theta(\log n)$ time.
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40) => 2

(Exercise ?1.20)

;; With applicative order, it performs 4 remainder operations.
(gcd 206 40)
=> (gcd 40 (remainder 206 40))
=> (gcd 40 6)
=> (gcd 6 (remainder 40 6))
=> (gcd 6 4)
=> (gcd 4 (remainder 6 4))
=> (gcd 4 2)
=> (gcd 2 (remainder 4 2))
=> (gcd 2 0)
=> 2

;; With normal order, it performs 18 remainder operations. Each `b` gets
;; evaluated once in the `(= b 0)` predicate (14 operations). The final `a` gets
;; evaluated in the end (4 operations). Together, that makes 18.
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
              (remainder 40 (remainder 206 40)))

(Section :1.2.6 "Example: Testing for Primality")

(Section :1.2.6.1 "Searching for divisors"
  (use (:1.1.4 square)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

;; Trial division: $\Theta(\sqrt n)$ time.
(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 10) => #f
(prime? 13) => #t

(Section :1.2.6.2 "The Fermat test"
  (use (:1.1.4 square) (:1.2.6.1 prime?)))

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
  (try-it (+ 1 (random (- n 1)))))

;; Fermat test: $\Theta(\log n)$ time, probabilistic.
(define (fast-prime? n times)
  (or (= times 0)
      (and (fermat-test n)
           (fast-prime? n (- times 1)))))

(define many-times 10)

;; The Fermat test only produces false positives on composite numbers, not prime
;; numbers, so we can be certain it will return true for 13.
(fast-prime? 13 many-times) => #t
(fast-prime? 10 many-times) =?> [#f #t] ; correct answer is #f

;; The first Carmichael number, 561, fools the Fermat test: no matter how many
;; iterations we use, it will always think it's prime.
(prime? 561) => #f
(fast-prime? 561 many-times) => #t

(Exercise ?1.21
  (use (:1.2.6.1 smallest-divisor)))

(smallest-divisor 199) => 199
(smallest-divisor 1999) => 1999
(smallest-divisor 19999) => 7

(Exercise ?1.22
  (use (:1.2.6.1 prime?)))

(define (timed-prime-test p? n)
  (newline)
  (display n)
  (start-prime-test p? n (runtime)))
(define (start-prime-test p? n start-time)
  (when (p? n)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes p? a b)
  (define (iter a b)
    (when (<= a b)
      (timed-prime-test p? a)
      (iter (+ a 2) b)))
  (iter (if (odd? a) a (+ a 1)) b))

(string-contains?
 (capture-output (search-for-primes prime? 6 10))
 "7 *** ")
=> #t

;; $A=$ time for 3 primes greater than 1,000.
; 1009 *** 4.792213439941406e-5
; 1013 *** 4.291534423828125e-5
; 1019 *** 4.792213439941406e-5

;; $B=$ time for 3 primes greater than 10,000. $(2.3A < B < 2.8A)$
; 10007 *** 1.1086463928222656e-4
; 10009 *** 1.1396408081054688e-4
; 10037 *** 1.2302398681640625e-4

;; $C=$ time for 3 primes greater than 100,000. $(3.3B < C < 4.1B)$
; 100003 *** 4.010200500488281e-4
; 100019 *** 3.6597251892089844e-4
; 100043 *** 4.558563232421875e-4

;; $D=$ time for 3 primes greater than 1,000,000. $(2.8C < D < 3.4C)$
; 1000003 *** .0013530254364013672
; 1000033 *** .0011339187622070312
; 1000037 *** .0013699531555175781

;; The data bears out the $\Theta(\sqrt n)$ prediction. The growth between
;; powers of ten gets closer to $\sqrt{10}\approx3.16$ as $n$ gets larger. This
;; result is compatible with the notion that programs on the machine run in time
;; proportional to the number of steps required for the computation.

(Exercise ?1.23
  (use (:1.1.4 square) (?1.22 search-for-primes)))

;; Trial division, but only testing odd divisors:
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

(string-contains?
 (capture-output (search-for-primes prime? 6 10))
 "7 *** ")
=> #t

;; Time for 3 primes greater than 1,000:
; 1009 *** 5.1975250244140625e-5   (1.085x)
; 1013 *** 5.1975250244140625e-5   (1.211x)
; 1019 *** 6.198883056640625e-5    (1.294x)

;; Time for 3 primes greater than 10,000:
; 10007 *** 1.1491775512695312e-4  (1.037x)
; 10009 *** 1.1801719665527344e-4  (1.036x)
; 10037 *** 1.1897087097167969e-4  (0.967x)

;; Time for 3 primes greater than 100,000:
; 100003 *** 3.540515899658203e-4  (0.883x)
; 100019 *** 3.490447998046875e-4  (0.954x)
; 100043 *** 3.590583801269531e-4  (0.788x)

;; Time for 3 primes greater than 1,000,000:
; 1000003 *** .0010960102081298828 (0.810x)
; 1000033 *** .001055002212524414  (0.930x)
; 1000037 *** .0010900497436523438 (0.796x)

;; The expectation of half time was not confirmed. In fact, this method is
;; actually slower for primes under 10,000. Even for seven-figure primes, it
;; only shaves off 20%. There was probably some error in the measurements; other
;; processes on the computer and random factors might have played a role. Maybe
;; the overhead of calling `next` cancels the benefit of skipping even numbers.

(Exercise ?1.24
  (use (:1.2.6.2 fast-prime?) (?1.22 search-for-primes)))

(define (prime? n) (fast-prime? n 100))

(string-contains?
 (capture-output (search-for-primes prime? 6 10))
 "7 *** ")
=> #t

;; $A=$ time for 3 primes greater than 1,000.
; 1009 *** .003638029098510742
; 1013 *** .003793001174926758
; 1019 *** .003606081008911133

;; $B=$ time for 3 primes greater than 10,000. $(0.988A < B < 1.196A)$
; 10007 *** .004311084747314453
; 10009 *** .0039730072021484375
; 10037 *** .0037479400634765625

;; $C=$ time for 3 primes greater than 100,000. $(0.893B < C < 1.294B)$
; 100003 *** .004847049713134766
; 100019 *** .004848003387451172
; 100043 *** .003850221633911133

;; $D=$ time for 3 primes greater than 1,000,000. $(0.891C < D < 1.453C)$
; 1000003 *** .005592823028564453
; 1000033 *** .004972934722900391
; 1000037 *** .0043179988861083984

;; Since the Fermat test has $\Theta(\log n)$ growth, I expected the time to
;; test primes near 1,000,000 to be only a bit greater than for primes near
;; 1,000. The data bears this out: for each additional order of magnitude of the
;; primes, the time required increases by a small, constant amount.
;; Specifically, primes that are 10 times larger take about 1 millisecond
;; longer. These results may be dependent on the choice of 100 as the second
;; argument to `fast-prime?` (the exercise did not specify what value to use).

(Exercise ?1.25
  (use (:1.1.4 square) (:1.2.4 fast-expt) (:1.2.6.2 expmod)))

(define (alyssa-expmod base exp m)
  (remainder (fast-expt base exp) m))

;; This procedure works, but it is not as efficient. The Fermat test takes much
;; longer using `alyssa-expmod`---longer by three orders of magnitude. The key
;; to the original `expmod` is not only successive squaring (which `fast-expt`
;; does as well in Alyssa's procedure), but also calling `remainder` between
;; each squaring. Alyssa's procedure does not, so the value becomes enormous,
;; requiring bignums. Suppose we test the primality of $n=9$, choosing $a=5$.
;; Using the original `expmod`, the process evolves like so:

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

;; Compare this to the evolution of the process using the Alyssa's procedure:

(alyssa-expmod 5 9 9)
=> (r (fast-expt 5 9) 9)
=> (r 1953125 9)

;; The original `expmod` doesn't need to deal with numbers anywhere near that
;; size. This number may seem okay, but it will grow exponentially with $n$ by
;; definition, and will quickly require arbitrary precision arithmetic, which is
;; much slower than fixnum arithmetic.

(Exercise ?1.26)

;; When the `square` combination is evaluated, the `expmod` combination is
;; evaluated once and then its value is substituted into the `square` compound
;; procedure according to the substitution model. When the squaring is written
;; as an explicit multiplication, the `expmod` combination is evaluated twice.
;; The interpreter has no way of knowing that they will have the same value.
;; This transforms a linear recursive process into a tree-recursive process. The
;; time complexity of this tree-recursive process is $\Theta(\log 2^n)$, or
;; $\Theta(n)$.

(Exercise ?1.27
  (use (:1.2.6.1 prime?) (:1.2.6.2 expmod)))

(define (fermat-all? n)
  (define (iter a)
    (or (>= a n)
        (and (= (expmod a n n) a)
             (iter (+ a 1)))))
  (iter 1))

;; These Carmichael numbers pass the Fermat tests for all values $a < n$:
(fermat-all? 561) => #t
(fermat-all? 1105) => #t
(fermat-all? 1729) => #t
(fermat-all? 2465) => #t
(fermat-all? 2821) => #t
(fermat-all? 6601) => #t

;; According to the trial division procedure, none of them are prime:
(prime? 561) => #f
(prime? 1105) => #f
(prime? 1729) => #f
(prime? 2465) => #f
(prime? 2821) => #f
(prime? 6601) => #f

(Exercise ?1.28
  (use (:1.1.4 square) (:1.2.6.2 many-times)))

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
  (try-it (+ 2 (random (- n 2)))))
(define (fast-prime? n times)
  (or (= times 0)
      (and (miller-rabin-test n)
           (fast-prime? n (- times 1)))))

;; Like the Fermat test in [](:1.2.6), the Miller--Rabin test always returns
;; true for primes but has probabilistic behavior for composites numbers:

(fast-prime? 13 many-times) => #t
(fast-prime? 10 many-times) =?> [#f #t] ; correct answer is #f

;; Unlike the Fermat test, it is not fooled by Carmichael numbers:

(fast-prime? 561 many-times) =?> [#f #t] ; correct answer is #f

(Section :1.3 "Formulating Abstractions with Higher-Order Procedures")

(define (cube x) (* x x x))

(Section :1.3.1 "Procedures as Arguments"
  (use (:1.3 cube)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b) (sum cube a inc b))
(sum-cubes 1 10) => 3025

(define (identity x) x)
(define (sum-integers a b) (sum identity a inc b))
(sum-integers 1 10) => 55

(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))
(* 8 (pi-sum 1 1000)) ~> 3.139592655589783

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01) ~> .24998750000000042
(integral cube 0 1 0.001) ~> .249999875000001

(Exercise ?1.29
  (use (:1.3 cube) (:1.3.1 inc sum)))

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (term k)
      (* (f (+ a (* k h)))
         (if (or (= k 0) (= k n))
             1
             (+ 2 (* 2 (remainder k 2))))))
    (* h 1/3 (sum term 0.0 inc n))))

;; The integral procedure is a bit inaccurate, whereas the `simpson` procedure
;; gives the exact answer even when n = 2 (note the `=>` rather than `~>`), at
;; least for this particular integral.

(simpson cube 0 1 2) => 0.25

(Exercise ?1.30)

(define (sum term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (+ acc (term a)))))
  (iter a 0))

(Exercise ?1.31
  (use (:1.3.1 identity inc)))

;; (a) Recursive
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;; (b) Iterative
(define (product term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (* acc (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(factorial 5) => 120
(factorial 7) => 5040

(define (approx-pi n)
  (define (term k)
    (let ((r (remainder k 2)))
      (/ (+ k 2 (- r))
         (+ k 1 r))))
  (* 4 (product term 1.0 inc n)))

(approx-pi 00010) ~> 3.2751010413348065
(approx-pi 00100) ~> 3.1570301764551654
(approx-pi 01000) ~> 3.1431607055322552
(approx-pi 10000) ~> 3.1417497057380084

(Exercise ?1.32
  (use (:1.3.1 identity inc)))

;; (a) Recursive
(define (accumulate combine id term a next b)
  (if (> a b)
      id
      (combine (term a)
               (accumulate combine id term (next a) next b))))

;; (b) Iterative
(define (accumulate combine id term a next b)
  (define (iter a acc)
    (if (> a b)
        acc
        (iter (next a) (combine (term a) acc))))
  (iter a id))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(sum identity 1 inc 10) => 55
(product identity 1 inc 10) => 3628800

(Exercise ?1.33
  (use (:1.1.4 square) (:1.3.1 identity inc) (?1.23 prime?)))

(define (filtered-accumulate combine pred? id term a next b)
  (define (iter a acc)
    (cond ((> a b) acc)
          ((pred? a)
           (iter (next a) (combine (term a) acc)))
          (else (iter (next a) acc))))
  (iter a id))

;; (a) Sum of squares of primes in the interval `a` to `b`
(define (sum-squared-primes a b)
  (filtered-accumulate + prime? 0 square a inc b))

(sum-squared-primes 10 15) => 290

;; (b) Product of positive integers below `n` relatively prime to `n`
(define (product-rel-prime n)
  (define (rel-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * rel-prime? 1 identity 1 inc (- n 1)))

(product-rel-prime 10) => (* 3 7 9)

(Section :1.3.2 "Constructing Procedures Using `Lambda`"
  (use (:1.1.4 square)))

((lambda (x y z) (+ x y (square z))) 1 2 3) => 12

(Exercise ?1.34
  (use (:1.1.4 square)))

(define (f g) (g 2))

(f square) => 4
(f (lambda (z) (* z (+ z 1)))) => 6

;; If we try evaluating the combination `(f f)`, we get the following process:
; (f f)
; (f 2)
; (2 2)
;; This gives an error, since 2 does not evaluate to a procedure. We cannot
;; apply 2 to the argument 2 because that doesn't make any sense.

(Section :1.3.3 "Procedures as General Methods")

(Section :1.3.3.1 "Finding roots of equations by the half-interval method"
  (use (:1.1.7 average)))

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

(define tolerance 0.001)
(define (close-enough? x y) (< (abs (- x y)) tolerance))

;; Half interval method: $\Theta(\log(|a-b|/\text{tolerance}))$ time.
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error 'half-interval-method
                  "values are not of opposite sign"
                  a b)))))

(half-interval-method sin 2.0 4.0)
~> 3.14111328125
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)
~> 1.89306640625
(half-interval-method cos -1 1)
=!> "values are not of opposite sign: -1 1"

(Section :1.3.3.2 "Finding fixed points of functions"
  (use (:1.1.7 average)))

(define tolerance 0.00001)
(define (close-enough? x y) (< (abs (- x y)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1.0) ~> 0.7390822985224023

;; Without average damping (does not converge)
(define (sqrt x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

;; With average damping (converges)
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(sqrt 2) ~> 1.4142135623746899

(Exercise ?1.35
  (use (:1.3.3.2 fixed-point)))

;; See proofs.pdf for the proof that the golden ratio is a fixed point of the
;; transformation x -> 1 + 1/x.

(define golden-ratio (/ (+ 1 (sqrt 5)) 2))

golden-ratio
~> 1.6180339887498950
(fixed-point (lambda (x) (+ 1 (/ x))) 1.0)
~> 1.6180327868852458

(Exercise ?1.36
  (use (:1.1.7 average) (:1.3.3.2 close-enough?)))

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

;; Without average damping, it takes 28 approximations.
(hide-output (fixed-point-verbose f 5)) ~> 4.555539314360711
(string-count #\newline (capture-output (fixed-point-verbose f 5))) => 28

;; With average damping, it takes only 8 approximations.
(define (f-damped x) (average x (f x)))
(hide-output (fixed-point-verbose f-damped 5)) ~> 4.5555361005218895
(string-count #\newline (capture-output (fixed-point-verbose f-damped 5))) => 8

(Exercise ?1.37
  (use (?1.35 golden-ratio)))

;; (a) Recursive
(define (cont-frac n d k)
  (define (helper i)
    (if (= i k)
        0
        (/ (n i)
           (+ (d i) (helper (+ i 1))))))
  (helper 1))

;; (b) Iterative
(define (cont-frac n d k)
  (define (iter i acc)
    (if (zero? i)
        acc
        (iter (- i 1) (/ (n i)
                         (+ (d i) acc)))))
  (iter k 0))

(define (always-one i) 1.0)
(define (approx-gr k) (cont-frac always-one always-one k))

;; When k = 11, the value is accurate to 4 decimal places.
(approx-gr 01) ~> 1.0
(approx-gr 02) ~> 0.5
(approx-gr 03) ~> 0.6666666666666666
(approx-gr 04) ~> 0.6000000000000001
(approx-gr 05) ~> 0.625
(approx-gr 06) ~> 0.6153846153846154
(approx-gr 07) ~> 0.6190476190476191
(approx-gr 08) ~> 0.6176470588235294
(approx-gr 09) ~> 0.6181818181818182
(approx-gr 10) ~> 0.6179775280898876
(approx-gr 11) ~> 0.6180555555555556

(define (round4 x) (* 1e-4 (truncate (* 1e4 x))))
(round4 (approx-gr 11)) ~> (round4 (/ golden-ratio))

(Exercise ?1.38
  (use (?1.37 always-one cont-frac)))

(define (approx-e k)
  (define (d i)
    (if (zero? (remainder (+ i 1) 3))
        (* 2/3 (+ i 1))
        1))
  (+ 2 (cont-frac always-one d k)))

(approx-e 1) ~> 3.0
(approx-e 2) ~> 2.6666666666666665
(approx-e 3) ~> 2.75
(approx-e 4) ~> 2.7142857142857144
(approx-e 5) ~> 2.71875
(approx-e 1000) ~> 2.7182818284590455

(Exercise ?1.39
  (use (:1.1.4 square) (?1.37 cont-frac)))

(define (tan-cf x k)
  (cont-frac
   (lambda (i) (if (= i 1) x (- (square x))))
   (lambda (i) (- (* i 2) 1))
   k))

(define quarter-pi (atan 1))

(tan-cf quarter-pi 1) ~> 0.7853981633974483
(tan-cf quarter-pi 2) ~> 0.9886892399342050
(tan-cf quarter-pi 3) ~> 0.9997876809149684
(tan-cf quarter-pi 4) ~> 0.9999978684156948
(tan-cf quarter-pi 5) ~> 0.9999999865263550
(tan quarter-pi) ~> 1

(Section :1.3.4 "Procedures as Returned Values"
  (use (:1.1.4 square) (:1.1.7 average) (:1.3.3.2 fixed-point)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10) => 55

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cbrt x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(Section :1.3.4.1 "Newton's method"
  (use (:1.1.4 square) (:1.3.3.2 fixed-point)))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (cube x) (* x x x))
((deriv cube) 5) ~> 75.00014999664018

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(Section :1.3.4.2 "Abstractions and first-class procedures"
  (use (:1.1.4 square) (:1.1.7 average) (:1.3.3.2 fixed-point)
       (:1.3.4 average-damp) (:1.3.4.1 newton-transform)))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))

(Exercise ?1.40
  (use (:1.1.4 square) (:1.3.4.1 newtons-method)))

(define (cubic a b c)
  (lambda (x)
    (let ((xx (square x)))
      (+ (* x xx)
         (* a xx)
         (* b x)
         c))))

(newtons-method (cubic -3 1 1) 1.0) ~> 1
((cubic -3 1 1) 1) ~> 0

(Exercise ?1.41
  (use (:1.3.1 inc)))

(define (double f)
  (lambda (x)
    (f (f x))))

(((double (double double)) inc) 5)
=> (((double (lambda (f) (double (double f)))) inc) 5)
=> (((lambda (f) (double (double (double (double f))))) inc) 5)
=> ((double (double (double (double inc)))) 5)
=> ((double (double (double (lambda (x) (inc (inc x)))))) 5)
=> ((double (double (lambda (x) (inc (inc (inc (inc x))))))) 5)
=> ((double (lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))) 5)
=> ((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc
      (inc (inc (inc (inc x))))))))))))))))) 5) ; NOALIGN
=> (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc
     (inc 5)))))))))))))))) ; NOALIGN
=> 21

(Exercise ?1.42
  (use (:1.1.4 square) (:1.3.1 inc)))

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6) => 49

(Exercise ?1.43
  (use (:1.1.4 square) (?1.42 compose)))

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1)) f)))

((repeated square 2) 5) => 625

(Exercise ?1.44
  (use (:1.1.4 square) (?1.43 repeated)))

(define dx 0.1)
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

((smooth square) 2) ~> 4.006666666666667
(((repeated smooth 5) square) 2) ~> 4.033333333333333

(Exercise ?1.45
  (use (:1.3.3.2 fixed-point) (:1.3.4 average-damp) (?1.43 repeated)))

;; We need to average-damp floor(log2(n)) times.
(define (nth-root x n)
  (fixed-point
   ((repeated average-damp
              (floor (/ (log n) (log 2))))
    (lambda (y) (/ x (expt y (- n 1)))))
   1.0))

(nth-root 4 2) ~> 2.000000000000002
(nth-root 256 8) ~> 2.0000000000039666
(nth-root 1048576 20) ~> 1.999999063225966

(Exercise ?1.46
  (use (:1.1.4 square) (:1.1.7 average) (:1.3.3.2 tolerance)))

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt x)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- (square guess) x)) tolerance))
    (lambda (guess)
      (average guess (/ x guess))))
   1.0))

(sqrt 2) ~> 1.4142156862745097

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- guess (f guess))) tolerance))
    f)
   first-guess))

;; This is slightly different from the original `fixed-point` implementation
;; because it returns `guess` when it's good enough, not `next` (that is, the
;; original always does one more improvement).
(fixed-point cos 1.0) ~> 0.7390893414033928

) ; end of SICP
) ; end of library
