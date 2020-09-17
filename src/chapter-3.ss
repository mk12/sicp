;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src chapter-3)
  (export chapter-3-effects)
  (import (rnrs base (6))
          (src lang sicp)
          (only (src chapter-2) chapter-2-effects))

;; Introduce a dependency on the previous chapter so that it executes first.
(define chapter-3-effects chapter-2-effects)

(SICP

(Chapter :3 "Modularity, Objects, and State")

(Section :3.1 "Assignment and Local State")

(Section :3.1.1 "Local State Variables")

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(withdraw 25) => 75
(withdraw 25) => 50
(withdraw 60) => "Insufficient funds"
(withdraw 15) => 35

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(new-withdraw 25) => 75
(new-withdraw 25) => 50
(new-withdraw 60) => "Insufficient funds"
(new-withdraw 15) => 35

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 50) => 50
(W2 70) => 30
(W2 40) => "Insufficient funds"
(W1 40) => 10

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error 'make-account "unknown request" m))))
  dispatch)

(define acc (make-account 100))
((acc 'withdraw) 50) => 50
((acc 'withdraw) 60) => "Insufficient funds"
((acc 'deposit) 40) => 90
((acc 'withdraw) 60) => 30
((acc 'floof)) =!> "unknown request: floof"

(Exercise ?3.1)

(define (make-accumulator amount)
  (lambda (increment)
    (set! amount (+ amount increment))
    amount))

(define A (make-accumulator 5))
(A 10) => 15
(A 10) => 25

(Exercise ?3.2)

(define (make-monitored f)
  (let ((n-calls 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) n-calls)
            ((eq? x 'reset-count) (set! n-calls 0))
            (else (set! n-calls (+ n-calls 1))
                  (f x))))))

(define s (make-monitored sqrt))
(s 100) => 10
(s 'how-many-calls?) => 1
(s 25) => 5
(s 'how-many-calls?) => 2
(s 'reset-count)
(s 'how-many-calls?) => 0

(Exercise ?3.3)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error 'make-account "unknown request" m)))
        (lambda (x) "Incorrect password")))
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40) => 60
((acc 'some-other-password 'deposit) 50) => "Incorrect password"

(Exercise ?3.4)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((consecutive-wrong 0))
    (define (dispatch p m)
      (if (eq? p password)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error 'make-account "unknown request" m)))
          (lambda (x)
            (set! consecutive-wrong (+ consecutive-wrong 1))
            (if (> consecutive-wrong 7)
                (call-the-cops)
                "Incorrect password"))))
    dispatch))

(define (call-the-cops) "PUT YOUR HANDS UP!")

(define acc (make-account 'right 100))
((acc 'wrong 'withdraw) 100) => "Incorrect password"
((acc 'wrong 'withdraw) 100) => "Incorrect password"
((acc 'wrong 'withdraw) 100) => "Incorrect password"
((acc 'wrong 'withdraw) 100) => "Incorrect password"
((acc 'wrong 'withdraw) 100) => "Incorrect password"
((acc 'wrong 'withdraw) 100) => "Incorrect password"
((acc 'wrong 'withdraw) 100) => "Incorrect password"
((acc 'wrong 'withdraw) 100) => "PUT YOUR HANDS UP!"

(Section :3.1.2 "The Benefits of Introducing Assignment")

;; Tausworthe PRNG: https://stackoverflow.com/a/23875298
(define random-init 1)
(define (rand-update x0)
  (let* ((x1 (fxxor x0 (fxarithmetic-shift-right x0 13)))
         (x2 (fxxor x1 (fxarithmetic-shift-left x1 18))))
     (fxand x2 #x7fffffff)))

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else (iter (- trials-remaining 1)
                      trials-passed))))
  (iter trials 0))

(Exercise ?3.5
  (use (:1.1.4 square) (:3.1.2 monte-carlo rand)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (let ((test (lambda ()
                (pred (random-in-range x1 x2)
                      (random-in-range y1 y2)))))
    (* (monte-carlo trials test)
       (- x2 x1)
       (- y2 y1))))

(define (estimate-pi trials)
  (let ((pred (lambda (x y)
                (<= (+ (square x) (square y)) 1))))
    (estimate-integral pred -1 1 -1 1 trials)))

(Exercise ?3.6
  (use (:3.1.2 random-init rand-update)))

(define rand
  (let ((x random-init))
    (lambda (message)
      (cond ((eq? message 'generate)
             (set! x (rand-update x))
             x)
            ((eq? message 'reset)
             (lambda (new-x)
               (set! x new-x)))
            (else (error 'rand "message not recognized" message))))))

(number? (rand 'generate)) => #t
(rand 'reset)
(number? (rand 'generate)) => #t

(Section :3.1.3 "The Costs of Introducing Assignment")

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
(define W (make-simplified-withdraw 25))
(W 20) => 5
(W 10) => -5

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))
(define D (make-decrementer 25))
(D 20) => 5
(D 10) => 15

;; Substitution analysis of `make-decrementer`:
((make-decrementer 25) 20)
=> ((lambda (amount) (- 25 amount)) 20)
=> (- 25 20)
=> 5

;; Faulty substitution analysis of `make-simplified-withdraw`:
(define balance)
((make-simplified-withdraw 25) 20) => 5
; =>
((lambda (amount) (set! balance (- 25 amount)) 25) 20)
=> (begin (set! balance (- 25 20)) 25)
=> 25

(Section :3.1.3.1 "Sameness and change"
  (use (:3.1.1 make-account)
       (:3.1.3 make-decrementer make-simplified-withdraw)))

;; `D1` and `D2` are the same.
(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))
(D1 20) => (D2 20) => (D1 20) => (D2 20)

;; `W1` and `W2` are surely not the same.
(define W1 (make-simplified-withdraw 25))
(define W2 (make-simplified-withdraw 25))
(W1 20) => 5
(W1 20) => -15
(W2 20) => 5

;; Distinct accounts:
(define peter-acc (make-account 100))
(define paul-acc (make-account 100))
((peter-acc 'withdraw) 25) => 75
((paul-acc 'withdraw) 25) => 75

;; Joint account:
(define peter-acc (make-account 100))
(define paul-acc peter-acc)
((peter-acc 'withdraw) 25) => 75
((paul-acc 'withdraw) 25) => 50

(Section :3.1.3.2 "Pitfalls of imperative programming"
  (use (:1.2.1 factorial)))

(define (imperative-factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

(imperative-factorial 10) => (factorial 10)

(Exercise ?3.7
  (use (?3.3 make-account)))

(define (make-joint pp-acc password new-password)
  (lambda (p m)
    (pp-acc (if (eq? p new-password) password #f) m)))

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((peter-acc 'rosebud 'withdraw) 10) => "Incorrect password"
((paul-acc 'open-sesame 'withdraw) 10) => "Incorrect password"
((peter-acc 'open-sesame 'withdraw) 10) => 90
((paul-acc 'rosebud 'withdraw) 10) => 80
((peter-acc 'open-sesame 'withdraw) 10) => 70

(Exercise ?3.8)

(define f
  (let ((x 0))
    (lambda (y)
      (let ((old-x x))
        (set! x y)
        old-x))))

(let ((result (+ (f 0) (f 1))))
  (or (= result 0) (= result 1)))
=> #t

(Section :3.2 "The Environment Model of Evaluation")

(Section :3.2.1 "The Rules for Evaluation")

(define square (lambda (x) (* x x)))
(square 5) => (* 5 5) => 25

;; Environment diagram:
;                 _____________
; global env --> | square: --+ |
;                |___________|_|<-+
;                 ^          |    |
;                 |          V    |
;         E1 -->[x: 5]     [*|*]--+
;                           V
;                  parameters: x
;                        body: (* x x)

(Section :3.2.2 "Applying Simple Procedures"
  (use (:3.2.1 square)))

(define (sum-of-squares x y) (+ (square x) (square y)))
(define (f a) (sum-of-squares (+ a 1) (* a 2)))
(f 5) => (sum-of-squares (+ 5 1) (* 5 2)) => (+ (square 6) (square 10)) => 136

;; Environment diagram:
;                 _____________________
; global env --> | sum-of-squares: ... |
;                | square: ...         |
;                | f: ...              |<------------+
;                |_____________________|<+           |
;   (f 5)        ^           ^           |           |
;                |           |           |           |
;          E1->[a: 5]  E2->[x: 6]  E3->[x: 6]  E4->[x:10]
;                          [y: 7]
;   (sum-of-squares  (+ (square x)   (* x x)     (* x x)
;     (+ a 1)           (square y))
;     (* a 2)

(Exercise ?3.9)

(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))

;; Six environments are created in the recursive version:
(factorial 6)                                  ; E1 -> [n: 6]
=> (* 6 (factorial 5))                         ; E2 -> [n: 5]
=> (* 6 (* 5 (factorial 4)))                   ; E3 -> [n: 4]
=> (* 6 (* 5 (* 4 (factorial 3))))             ; E4 -> [n: 3]
=> (* 6 (* 5 (* 4 (* 3 (factorial 2)))))       ; E5 -> [n: 2]
=> (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1)))))) ; E6 -> [n: 1]
=> 720

(define (factorial n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;; Eight environments are created in the iterative version:
(factorial 6)          ; E1 -> [n: 6]
=> (fact-iter 1 1 6)   ; E2 -> [p: 1,   c: 1, m: 6]
=> (fact-iter 1 2 6)   ; E3 -> [p: 1,   c: 2, m: 6]
=> (fact-iter 2 3 6)   ; E4 -> [p: 2,   c: 3, m: 6]
=> (fact-iter 6 4 6)   ; E5 -> [p: 6,   c: 4, m: 6]
=> (fact-iter 24 5 6)  ; E6 -> [p: 24,  c: 5, m: 6]
=> (fact-iter 120 6 6) ; E7 -> [p: 120, c: 6, m: 6]
=> (fact-iter 720 7 6) ; E8 -> [p: 720, c: 7, m: 6]
=> 720

(Section :3.2.3 "Frames as the Repository of Local State")

(Exercise ?3.10)

;; With or without the explicit state variable, `make-withdraw` creates objects
;; with the same behaviour. The only difference with the explicit variable in
;; the let-form is that there is an extra environment. Applying `make-withdraw`
;; creates E1 to bind 100 to `initial-amount`, and then the let-form desugars to
;; a lambda application, creating a new environment E2. This environment holds
;; `balance`, beginning with the same value as `initial amount`. When we
;; evaluate `(W1 20)`, we create the environment E3 that binds `amount` to 20.
;; The assignment in the code for W2 changes the value of `balance` from 100 to
;; 80. The value of `initial-amount` remains the same, because it was never
;; changed in a `set!` assignment. The behaviour is no different with the
;; let-form because, although we are now saving the original balance, we aren't
;; doing anything with it. We can't access it outside of the procedure.
;                ____________________
; global env -->| make-withdraw: ... |
;               | W2: ---------+     |
;               | W1:          |     |<--------------------+
;               |_|____________|_____|               E3    |
;                 |        ^   |                      [initial-amount: 100]
;                 |  E1    |   +--------------->[*|*]      ^
;                 |   [initial-amount: 100]      | |   E4  |
;                 |        ^                     | +--->[balance: 60]
;                 V    E2  |                     |         ^
;               [*|*]-->[balance: 80]            |         |
;                V              ^                |         |
;       parameters: amount }<---|----------------+         |
;             body: ...    }    |                          |
;                               |          (W2 40) [amount: 40]
;                               |
;      (W1 20) [amount: 20]-----+

(Section :3.2.4 "Internal Definitions")

(Exercise ?3.11
  (use (:3.1.1 make-account)))

;; First, we just have a procedure bound in the global environment.
; global env --> [make-account: ...]

(define acc (make-account 50))

;; Now, we have `acc` in the global frame as well. It is bound to a procedure
;; whose environment pointer points to E1, the environment created when we
;; evaluated `(make-account 50)`. It first bound the formal parameter `balance`
;; to 50, and then three internal procedures were defined and bound in the same
;; frame. One of them, `dispatch`, points to the same procedure as `acc`.
;                 __________________
; global env --> | make-account: ...|
;                | acc:             |
;                |__|_______________|
;                   |           ^
;                   V      E1___|____________
;                 [*|*]---->| balance: 50    |<-----+
;                  V        | withdraw:------|-->[*|*]
;         parameters: m     | deposit:-------|-+  +-----> parameters: amount
;               body: ...   | dipspatch:-+   | |                body: ...
;            ~~~~~~~~~~~~<---------------+   | +->[*|*]
;                           |________________|<----|-+
;                                                  +----> parameters: amount
;                                                               body: ...

((acc 'deposit) 40) => 90

;; First we evaluate `(acc 'deposit)`. We create E2 to bind `m` to the symbol
;; `deposit`, and then we evaluate the body of `acc`, which is the same as the
;; body of `dispatch`. The enclosing environment of E2 is E1, because that is
;; pointed to by the procedure. This application returns the value of `deposit`
;; from E1. Now we evaluate `((#<deposit> 40)`. We create E3 to bind `amount` to
;; the value 40, and the enclosing environment is E1 (pointed to by the
;; procedure `deposit`). This finally assigns 90 to `balance` in E1, and then
;; returns that value.
; E2 [m: deposit]--+
;                  +----> E1 [balance:90, ...]
; E3 [amount: 40]--+

((acc 'withdraw) 60) => 30

;; This is almost the same, except the procedure returns the `withdraw`
;; procedures instead. I am reusing the names E2 and E3 because they have been
;; used and are no longer relevant, since nothing poitns to them.
; E2 [m: withdraw]--+
;                   +---> E1 [balance: 30, ...]
; E3 [amount: 60]---+
;; All this time, the local state for `acc` is kept in E1, the environment
;; originally created to apply the `make-account` procedure. If we define
;; another account with `(define acc2 (make-account 100))`, it will have its own
;; environment containing `balance` and bindings for the internal procedures.
;; The only thing shared between `acc` and `acc2` is (possibly) the code for the
;; internal procedures, including `dispatch`, which the accounts really are.
;; This sharing is an implementation detail, though.

(Section :3.3 "Modeling with Mutable Data")

(Section :3.3.1 "Mutable List Structure")

(define x '((a b) c d))
(define y '(e f))

(set-car! x y)
x => (cons y (cdr x)) => '((e f) c d)

(define x '((a b) c d))
(define y '(e f))

(set-cdr! x y)
x => (cons (car x) y) => '((a b) e f)

(Exercise ?3.12)

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

z => '(a b c d)
(cdr x) => '(b)
; x->[*|*]->[*|X]
;     |      |
;     V      V
;     a      b

(define w (append! x y))
w => '(a b c d)
(cdr x) => '(b c d)
;                 y
;                 |
; x->[*|*]->[*|*]->[*|*]->[*|X]
; w/  |      |      |      |
;     V      V      V      V
;     a      b      c      d

(Exercise ?3.13
  (use (?3.12 last-pair)))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
(cadddr z) => 'a
;    +-------------------+
;    V                   |
; z->[*|*]->[*|*]->[*|*]-+
;     |      |      |
;     V      V      V
;     a      b      c

;; If we try to compute `(last-pair z)`, we will never finish because the list
;; is not null-terminated and so `null?` will never be true. We will be stuck in
;; an infinite recursion.

(Exercise ?3.14)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; In general, `mystery` reverses the list `x`. It does this by walking through
;; the list, setting the `cdr` of each pair to point to the previous pair
;; instead of the next. For the very first pair, it sets the `cdr` to null.

(define v '(a b c d))
; v->[*|*]->[*|*]->[*|*]->[*|X]
;     |      |      |      |
;     V      V      V      V
;     a      b      c      d

(define w (mystery v))
v => '(a)
w => '(d c b a)
; v->[*|X]<-[*|*]<-[*|*]<-[*|*]<-w
;     |      |      |      |
;     V      V      V      V
;     a      b      c      d

;; These box-and-pointer diagrams make it obvious that `mystery` simply changes
;; the directions of all the arrows.

(Section :3.3.1.1 "Sharing and identity")

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x) (set-car! (car x) 'wow) x)

z1 => '((a b) a b)
(set-to-wow! z1) => '((wow b) wow b)
z2 => '((a b) a b)
(set-to-wow! z2) => '((wow b) a b)

(eq? (car z1) (cdr z1)) => #t
(eq? (car z2) (cdr z2)) => #f

(Exercise ?3.15)

;; In `z1`, the `car` and `cdr` both point to `x`:
; z1->[*|*]
;      | |
;      V V
;  x->[*|*]->[*|X]
;      |      |
;      V      V
;      a      b

;; After `set-to-wow!`, the `a` becomes `wow` for both `car` and `cdr`:
; z1->[*|*]
;      | |
;      V V
;  x->[*|*]->[*|X]
;      |      |
;      V      V
;     wow     b

;; In `z2`, the `car` and `cdr` point to different cons cells:
; z2->[*|*]->[*|*]->[*|X]
;      |      |      |
;      |      +-> a  +-> b
;      V
;    [*|*]->[*|X]
;     |      |
;     +-> a  +-> b

;; After `set-to-wow!`, the `a` becomes `wow` only for the `car`:
; z2->[*|*]->[*|*]->[*|X]
;      |      |      |
;      |      +-> a  +-> b
;      V
;    [*|*]--->[*|X]
;     |        |
;     +-> wow  +-> b

(Exercise ?3.16)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; The procedure `count-pairs` is wrong because it assumes there is no sharing.
;; The lists below named `N-M` have `N` pairs, but they use sharing so that
;; `count-pairs` think they have `M` pairs.

(define one-1 (cons 'a '()))
(define two-2 (cons 'a (cons 'b '())))
(define two-3 (cons one-1 one-1))
(define three-3 (cons 'a (cons 'a (cons 'a '()))))
(define three-4 (cons 'a (cons one-1 one-1)))
(define three-5 (cons two-2 two-2))
(define three-7 (cons two-3 two-3))

(count-pairs one-1) => 1
(count-pairs two-2) => 2
(count-pairs two-3) => 3
(count-pairs three-3) => 3
(count-pairs three-4) => 4
(count-pairs three-5) => 5
(count-pairs three-7) => 7

(Exercise ?3.17
  (use (:2.3.1 memq) (?3.16 one-1 two-2 two-3 three-3 three-4 three-5 three-7)))

(define (count-pairs x)
  (let ((seen '()))
    (define (iter x)
      (if (or (not (pair? x)) (memq x seen))
          0
          (begin (set! seen (cons x seen))
                 (+ (iter (car x))
                    (iter (cdr x))
                    1))))
    (iter x)))

(count-pairs one-1) => 1
(count-pairs two-2) => 2
(count-pairs two-3) => 2
(count-pairs three-3) => 3
(count-pairs three-4) => 3
(count-pairs three-5) => 3
(count-pairs three-7) => 3

(Exercise ?3.18
  (use (:2.3.1 memq) (?3.13 make-cycle)))

(define (cycle? ls)
  (define (iter ls seen)
    (and (pair? ls)
         (or (memq ls seen)
             (iter (cdr ls) (cons ls seen)))))
  (if (iter ls '()) #t #f))

(cycle? (list 1 2 3)) => #f
(cycle? (make-cycle (list 1 2 3))) => #t

(Exercise ?3.19
  (use (?3.13 make-cycle)))

;; This is Floyd's cycle-finding algorithm (the tortoise and the hare).
(define (cycle? ls)
  (define (iter t h)
    (and (pair? h)
         (pair? (cdr h))
         (or (eq? t h)
             (iter (cdr t) (cddr h)))))
  (and (pair? ls)
       (iter ls (cdr ls))))

(cycle? (list 1 2 3)) => #f
(cycle? (make-cycle (list 1 2 3))) => #t

(Section :3.3.1.2 "Mutation is just assignment")

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error 'cons "undefined operation" m))))
  dispatch)

(define (car p) (p 'car))
(define (cdr p) (p 'cdr))
(define (set-car! p v) ((p 'set-car!) v) p)
(define (set-cdr! p v) ((p 'set-cdr!) v) p)

(define z (cons 'a 'b))
(car z) => 'a
(cdr z) => 'b
(set-car! z 'c)
(car z) => 'c
(set-cdr! z 'd)
(cdr z) => 'd

(Exercise ?3.20
  (use (:3.3.1.2 cons car cdr set-car!)))

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x) => 17

;; Following the arrows in the environment diagram below, we see that the `cdr`
;; of `z` is the same pair pointed to by `x`. By changing the `car` of this pair
;; to 17, we change `x` from `(1 2)` to `(17 2)`.
;               ______________
; global env ->| x: -+  z: ---|-----------------+
;              |_____|________|<----------------|------+
;                    |    ^                     V     _|___________
;                    | E1_|___________        [*|*]->| set-x!: ... |
;                    |  | x: 1   y: 2 |        |     | set-y!: ... |
;                    |  | set-x!: ... |        V     | dispatch: --|-+
;                    |  | set-y!: ... |  params: m   | x:+   y:+   | |
;                    |  | dispatch:+  |    body: ... |___|_____|___| |
;                    |  |__________|__|  ~~~~~~~~~~~     |     |     |
;                    |   ^         |            ^--------|-----|-----+
;                    |   |         |                     |     |
;                    | +-|-----<---+---------<-----------+--<--+
;                    | | |
;                    V V |
;                  [*|*]-+
; paramters: m   }<-+
;      body: ... }

(Section :3.3.2 "Representing Queues")

(define front-ptr car)
(define rear-ptr cdr)
(define set-front-ptr! set-car!)
(define set-rear-ptr! set-cdr!)

(define (empty-queue? q) (null? (front-ptr q)))
(define (make-queue) (cons '() '()))

(define (front-queue q)
  (if (empty-queue? q)
      (error 'front-queue "called with an empty queue" q)
      (car (front-ptr q))))

(define (insert-queue! q x)
  (let ((new-pair (cons x '())))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (*stack-mode*
           (set-cdr! new-pair (front-ptr q))
           (set-front-ptr! q new-pair))
          (else
           (set-cdr! (rear-ptr q) new-pair)
           (set-rear-ptr! q new-pair)
           q))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error 'delete-queue! "called with an empty queue" q))
        (else (set-front-ptr! q (cdr (front-ptr q)))
              q)))

;; Make the queue behave as a stack (FILO insertion). Used in Exercise 3.32.
(define *stack-mode* #f)
(define (enable-stack-mode) (set! *stack-mode* #t))
(define (disable-stack-mode) (set! *stack-mode* #f))

(Exercise ?3.21
  (use (:3.3.2 make-queue delete-queue! front-ptr insert-queue!)))

(define q1 (make-queue))
(insert-queue! q1 'a) => '((a) a)
(insert-queue! q1 'b) => '((a b) b)
(delete-queue! q1) => '((b) b)
(delete-queue! q1) => '(() b)

;; Eva Lu Ator points out that Lisp is trying to print the list structure that
;; makes up the queue. It doesn't know anything special about our queue
;; representation. The interpreter's response isn't a list of things in the
;; queue, it is the queue as we decided to represent it. It is a bit more clear
;; if we print the lists in dotted cons notation:
; list repr.      front    rear
; ((a) a)     =   ((a)   . (a))
; ((a b) b)   =   ((a b) . (b))
; ((b) b)     =   ((b)   . (b))
; (() b)      =   (()    . (b))
;; Now, it is clear that Lisp is showing us the front pointer and the rear
;; pointer, interpreting both as ordinary lists. This works fine for the front
;; pointer, and in fact we can just look at it by itself to see everything in
;; our queue. The rear pointer is always displayed as a list with one item
;; because the `cdr` of the last item is always null. Even when we delete all
;; the items, we still see the last item in the queue because of the way we
;; implemented `delete-queue!`.

(define (print-queue q)
  (display (front-ptr q))
  (newline))

(insert-queue! q1 'c)
(insert-queue! q1 'd)
(print-queue q1) =/> "(c d)\n"

(Exercise ?3.22)

;; It's interesting how I ended up using `dispatch` like you would use the
;; `this` keyword in object-oriented languages. Another interesting point is the
;; application of procedures in `dispatch`. For procedures that take arguments
;; other than the queue itself, like for insertion, we have to return the
;; procedure that can then be applied to the argument(s). In this case, the
;; rest of the operations take no other arguments. It might be more consistent
;; to return a procedure of zero arguments -- then we would need double
;; parentheses, like `((my-queue 'front-queue))` -- but this seems a bit
;; strange. Instead, we apply the procedure right away in `dispatch` and
;; pass on the return value.

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (insert! x)
      (let ((new-pair (cons x '())))
        (cond ((empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               dispatch)
              (else (set-cdr! rear-ptr new-pair)
                    (set! rear-ptr new-pair)
                    dispatch))))
    (define (delete!)
      (if (empty?)
          (error 'delete! "called with an empty queue")
          (begin (set! front-ptr (cdr front-ptr))
                 dispatch)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty?))
            ((eq? m 'front-queue)
             (if (empty?)
                 (error 'front-queue "called with an empty queue")
                 (car front-ptr)))
            ((eq? m 'insert-queue!) insert!)
            ((eq? m 'delete-queue!) (delete!))
            (else (error 'make-queue "undefined operation" m))))
    dispatch))

(define q (make-queue))
(q 'empty-queue?) => #t
((q 'insert-queue!) 'a)
((q 'insert-queue!) 'b)
(q 'empty-queue?) => #f
(q 'front-queue) => 'a
(q 'delete-queue!)
(q 'front-queue) => 'b
(q 'delete-queue!)
(q 'empty-queue?) => #t
(q 'front-queue) =!> "called with an empty queue"
(q 'delete-queue!) =!> "called with an empty queue"

(Exercise ?3.23)

;; I have implemented the deque as a doubly-linked list. Instead of pointing to
;; the next element, the `cdr` of each item is a pair whose `car` points to the
;; previous item and whose `cdr` points to the next. We call the items nodes:

(define (make-node x prev next) (cons x (cons prev next)))
(define (data-node node) (car node))
(define (prev-node node) (cadr node))
(define (next-node node) (cddr node))
(define (set-prev! node prev) (set-car! (cdr node) prev))
(define (set-next! node next) (set-cdr! (cdr node) next))

(define (front-ptr dq) (car dq))
(define (rear-ptr dq)  (cdr dq))
(define (set-front-ptr! dq x) (set-car! dq x))
(define (set-rear-ptr!  dq x) (set-cdr! dq x))
(define (make-deque) (cons '() '()))

(define (empty-deque? dq) (null? (front-ptr dq)))
(define (front-deque dq)
  (if (empty-deque? dq)
      (error 'front-deque "called with an empty deque" dq)
      (data-node (front-ptr dq))))
(define (rear-deque dq)
  (if (empty-deque? dq)
      (error 'rear-deque "called with an empty deque" dq)
      (data-node (rear-ptr dq))))

(define (front-insert-deque! dq x)
  (cond ((empty-deque? dq)
         (let ((node (make-node x '() '())))
           (set-front-ptr! dq node)
           (set-rear-ptr! dq node)
           dq))
        (else
         (let* ((old-front (front-ptr dq))
                (new-front (make-node x '() old-front)))
           (set-prev! old-front new-front)
           (set-front-ptr! dq new-front)
           dq))))

(define (rear-insert-deque! dq x)
  (cond ((empty-deque? dq)
         (front-insert-deque! dq x))
        (else
         (let* ((old-rear (rear-ptr dq))
                (new-rear (make-node x old-rear '())))
           (set-next! old-rear new-rear)
           (set-rear-ptr! dq new-rear)
           dq))))

(define (front-delete-deque! dq)
  (cond ((empty-deque? dq)
         (error 'front-delete-deque! "called with an empty deque" dq))
        (else
         (let* ((old-front (front-ptr dq))
                (new-front (next-node old-front)))
           (cond ((null? new-front)
                  (set-front-ptr! dq '())
                  (set-rear-ptr! dq '())
                  dq)
                 (else (set-prev! new-front '())
                       (set-front-ptr! dq new-front)
                       dq))))))

(define (rear-delete-deque! dq)
  (cond ((empty-deque? dq)
         (error 'rear-delete-deque! "called with an empty deque" dq))
        (else
         (let* ((old-rear (rear-ptr dq))
                (new-rear (prev-node old-rear)))
           (cond ((null? new-rear)
                  (front-delete-deque! dq))
                 (else (set-next! new-rear '())
                       (set-rear-ptr! dq new-rear)
                       dq))))))

(define (print-deque dq)
  (define (iter node first)
    (when (not (null? node))
      (when (not first) (display ", "))
      (display (data-node node))
      (iter (next-node node) #f)))
  (display "[")
  (iter (front-ptr dq) #t)
  (display "]")
  (newline))

(define dq (make-deque))
(empty-deque? dq) => #t
(print-deque dq) =/> "[]\n"
(front-insert-deque! dq 'b)
(empty-deque? dq) => #f
(rear-insert-deque! dq 'c)
(front-insert-deque! dq 'a)
(print-deque dq) =/> "[a, b, c]\n"
(rear-delete-deque! dq)
(print-deque dq) =/> "[a, b]\n"
(front-delete-deque! dq)
(print-deque dq) =/> "[b]\n"
(rear-delete-deque! dq)
(empty-deque? dq) => #t
(front-deque dq) =!> "called with an empty deque"
(front-delete-deque! dq) =!> "called with an empty deque"
(rear-delete-deque! dq) =!> "called with an empty deque"

(Section :3.3.3 "Representing Tables")

(Section :3.3.3.1 "One-dimensional tables")

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table))))))

(define (make-table) (list '*table*))

(define t (make-table))
(lookup 'a t) => #f
(insert! 'a 1 t)
(lookup 'a t) => 1

(Section :3.3.3.2 "Two-dimensional tables"
  (use (:3.3.3.1 assoc make-table)))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1 (cons key-2 value))
                        (cdr table))))))

(define t (make-table))
(lookup 'a 'b t) => #f
(insert! 'a 'b 1 t)
(lookup 'a 'b t) => 1
(insert! 'a 'c 2 t)
(insert! 'x 'x 3 t)
(lookup 'a 'c t) => 2
(lookup 'x 'x t) => 3

(Section :3.3.3.3 "Creating local tables"
  (use (:3.3.3.1 assoc)))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record (cdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table))))))
    (define (reset!)
      (set-cdr! local-table '()))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'reset-proc!) reset!)
            (else (error 'make-table "unknown operation" m))))
    dispatch))

;; This is used extensively in Chapter 2.
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define reset (operation-table 'reset-proc!))

(Exercise ?3.24)

;; Other than the argument `same-key?` and the internal procedure `assoc`, this
;; is the same code as in Section 3.3.3.3.
(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) #f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record (cdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
                (set-cdr! record value)
                (set-cdr! subtable
                          (cons (cons key-2 value)
                                (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1 (cons key-2 value))
                          (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error 'dispatch "unknown operation" m))))
    dispatch))

(define table (make-table (lambda (x y) (< (abs (- x y)) 10))))
((table 'insert-proc!) 0 0 'a)
((table 'lookup-proc) 0 0) => 'a
((table 'lookup-proc) 2 3) => 'a
((table 'lookup-proc) -9 9) => 'a

(Exercise ?3.25
  (use (:3.3.3.1 assoc make-table)))

;; A table is a pair `(key . records)`, where `records` is an alist. Each alist
;; entry is either `(key . value)`. If `value` is a list, then the entry is a
;; subtable. The root key is the symbol `*table*`. To allow storing values at a
;; key which is a prefix of other keys, we use a sentinel key `()`.

(define (lookup keys table)
  (let ((value (cdr table)))
    (cond ((null? keys)
           (if (list? value)
               (lookup '(()) table)
               value))
          ((list? value)
           (let ((subtable (assoc (car keys) value)))
             (and subtable (lookup (cdr keys) subtable))))
          (else #f))))

(define (insert! keys value table)
  (define (iter keys table)
    (let ((old-value (cdr table)))
      (cond ((null? keys) (set-cdr! table value))
            ((list? old-value)
             (let ((subtable (assoc (car keys) old-value)))
               (if subtable
                   (iter (cdr keys) subtable)
                   (let ((new-subtable (cons (car keys) '())))
                     (set-cdr! table (cons new-subtable old-value))
                     (iter (cdr keys) new-subtable)))))
            (else (set-cdr! table (list (cons '() old-value)))
                  (iter keys table)))))
  (iter keys table))

(define table (make-table))

(insert! '(a) 1 table)
(insert! '(a b c) 2 table)
(insert! '(d) 3 table)
(insert! '(e f) 4 table)
(insert! '(e g h) 5 table)

(lookup '(a) table) => 1
(lookup '(a b c) table) => 2
(lookup '(d) table) => 3
(lookup '(e f) table) => 4
(lookup '(e g h) table) => 5

(lookup '(a b) table) => #f
(lookup '(a b c d) table) => #f
(lookup '(z) table) => #f

(Exercise ?3.26
  (use (:3.3.3.1 make-table)))

;; A table is a pair whose `cdr` is a node. A node is either null or has the
;; form `((key . value) . (left . right))` where `left` and `right` are nodes.

(define (lookup key table)
  (define (iter node)
    (if (null? node)
        #f
        (let ((node-key (caar node)))
          (cond ((= key node-key) (cdar node))
                ((< key node-key) (iter (cadr node)))
                ((> key node-key) (iter (cddr node)))))))
  (iter (cdr table)))

(define (insert! key value table)
  (define (iter node set-child! parent)
    (if (null? node)
        (set-child! parent (cons (cons key value) (cons '() '())))
        (let ((node-key (caar node)))
          (cond ((= key node-key) (set-cdr! (car node) value))
                ((< key node-key) (iter (cadr node) set-car! (cdr node)))
                ((> key node-key) (iter (cddr node) set-cdr! (cdr node)))))))
  (iter (cdr table) set-cdr! table))

(define table (make-table))

(insert! 0 'a table)
(insert! 25 'b table)
(insert! -3 'c table)
(insert! -4 'd table)
(insert! 7 'e table)

(lookup 0 table) => 'a
(lookup 25 table) => 'b
(lookup -3 table) => 'c
(lookup -4 table) => 'd
(lookup 7 table) => 'e

(Exercise ?3.27
  (use (:3.3.3.1 make-table) (?3.26 insert! lookup)))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((cached (lookup x table)))
        (or cached
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize
    (lambda (n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (memo-fib (- n 1))
                     (memo-fib (- n 2))))))))

(memo-fib 6) => 8
(memo-fib 100) => 354224848179261915075

;; See whiteboard/exercise-3.27.jpg for the environment diagram.

;; The memoized procedure `memo-fib` computes the nth Fibonacci number in a
;; number of steps proportional to `n` because it takes the sum of `n` numbers.
;; When we evaluate `(memo-fib n)`, a tree-recursive process is generated and it
;; descends until it reaches 0 and 1, the base cases of the recursive Fibonacci
;; implementation. The results for these inputs are placed in the table, and
;; then `(memo-fib 2)` requires only one step, the addition of 0 and 1, because
;; the values are taken from the table. In general, we descend to the bottom of
;; the tree once and then ascend it, never again going down and reaching
;; duplicate leaves. This is twice `n` steps, so it grows as O(n).

;; If we had defined `memo-fib` as `(memoize fib)`, it would not work because
;; recursive calls would use `fib`, not `memo-fib`, and so we would still have
;; an exponential number of steps. However, this aspect of the memoization would
;; still work: if you evaluated `(memo-fib 42)` twice, the second time would
;; take only the step of looking up a value in the table.

(Section :3.3.4 "A Simulator for Digital Circuits"
  (use (:3.3.4.1 and-gate inverter) (:3.3.4.2 make-wire) (?3.28 or-gate)))

(define (half-adder a b sum carry)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b carry)
    (inverter carry e)
    (and-gate d e sum)))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder a b s c1)
    (half-adder c-in s sum c2)
    (or-gate c1 c2 c-out)))

(Section :3.3.4.1 "Primitive function boxes"
  (use (:3.3.4.2 add-action! get-signal set-signal!) (:3.3.4.3 after-delay)))

(define inverter-delay 2)
(define and-gate-delay 3)

(define (inverter input output)
  (add-action!
    input
    (lambda ()
      (let ((new-signal (logical-not (get-signal input))))
        (after-delay
          inverter-delay
          (lambda () (set-signal! output new-signal)))))))

(define (and-gate a b out)
  (define (action)
    (let ((new-signal (logical-and (get-signal a) (get-signal b))))
      (after-delay
        and-gate-delay
        (lambda () (set-signal! out new-signal)))))
  (add-action! a action)
  (add-action! b action))

(define (logical-not a) (- 1 a))
(define (logical-and a b) (* a b))

(Exercise ?3.28
  (use (:3.3.4.2 add-action! get-signal set-signal!) (:3.3.4.3 after-delay)))

(define or-gate-delay 5)

(define (or-gate a b out)
  (define (action)
    (let ((new-signal (logical-or (get-signal a) (get-signal b))))
      (after-delay
        or-gate-delay
        (lambda () (set-signal! out new-signal)))))
  (add-action! a action)
  (add-action! b action))

(define (logical-or a b) (- (+ a b) (* a b)))

(Exercise ?3.29
  (use (:3.3.4.1 and-gate and-gate-delay inverter inverter-delay)
       (:3.3.4.2 make-wire) (?3.28 or-gate-delay)))

(define (or-gate a b out)
  (let ((na (make-wire))
        (nb (make-wire))
        (c (make-wire)))
    (inverter a na)
    (inverter b nb)
    (and-gate na nb c)
    (inverter c out)))

(define compound-or-gate-delay
  (+ and-gate-delay (* 2 inverter-delay)))

(Exercise ?3.30
  (use (:3.3.4 full-adder) (:3.3.4.1 and-gate-delay inverter-delay)
       (:3.3.4.2 make-wire set-signal!) (?3.28 or-gate-delay)))

;; Adds binary numbers `as` and `bs` in little endian order.
(define (ripple-carry-adder as bs ss carry)
  (define (iter as bs c-in ss)
    (if (null? (cdr as))
        (full-adder (car as) (car bs) c-in (car ss) carry)
        (let ((c (make-wire)))
          (full-adder (car as) (car bs) c-in (car ss) c)
          (iter (cdr as) (cdr bs) c (cdr ss)))))
  (cond ((not (= (length as) (length bs) (length ss)))
         (error 'ripple-carry-adder "bit width mismatch" as bs ss))
        ((null? as)
         (error 'ripple-carry-adder "bit width must be at least 1" as))
        (else (let ((c-in (make-wire)))
                (set-signal! c-in 0)
                (iter as bs c-in ss)))))

(define half-adder-delay
  (+ (max or-gate-delay
          (+ and-gate-delay inverter-delay))
     or-gate-delay))

(define full-adder-delay
  (+ (* 2 half-adder-delay)
     or-gate-delay))

(define (ripple-carry-adder-delay n)
  (* n full-adder-delay))

(Section :3.3.4.2 "Representing wires"
  (use (:3.3.4.5 add-to-agenda!)))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-signal! s)
      (when (not (= signal-value s))
        (set! signal-value s)
        (call-each action-procedures)))
    (define (add-action! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'add-action!) add-action!)
            (else (error 'dispatch "unknown operation" m))))
    dispatch))

(define (call-each procs) (for-each (lambda (f) (f)) procs))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire s) ((wire 'set-signal!) s))
(define (add-action! wire a) ((wire 'add-action!) a))

(Section :3.3.4.3 "The agenda"
  (use (:3.3.4.5 add-to-agenda! empty-agenda? first-agenda-item make-agenda
                 remove-first-agenda-item! reset-agenda! simulation-time)))

(define the-agenda (make-agenda))
(define (reset) (reset-agenda! the-agenda))

(define (after-delay delay-time action)
  (add-to-agenda! (+ delay-time (simulation-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (unless (empty-agenda? the-agenda)
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(Section :3.3.4.4 "A sample simulation"
  (use (:3.3.4 half-adder)
       (:3.3.4.2 add-action! get-signal make-wire set-signal!)
       (:3.3.4.3 propagate reset the-agenda) (:3.3.4.5 simulation-time)))

(define (probe name wire)
  (add-action!
    wire
    (lambda ()
      (display (format "\n~a ~a New-value = ~a"
                       name (simulation-time the-agenda) (get-signal wire))))))

(reset)
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
=/> ["sum 0 New-value = 0"]
(probe 'carry carry)
=/> ["carry 0 New-value = 0"]
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
=/> ["sum 8 New-value = 1"]
(set-signal! input-2 1)
(propagate)
=/> ["carry 11 New-value = 1"
     "sum 16 New-value = 0"]

(Exercise ?3.31
  (use (:3.3.4 half-adder) (:3.3.4.1 inverter)
       (:3.3.4.2 get-signal make-wire set-signal!) (:3.3.4.3 propagate reset)
       (:3.3.4.4 probe)))

;; We can make wires that do not call actions immediately by wrapping actions
;; in procedures that do nothing on their first call.

(define (make-bad-wire)
  (let ((wire (make-wire)))
    (lambda (m)
      (if (not (eq? m 'add-action!))
          (wire m)
          (lambda (action)
            ((wire 'add-action!)
              (let ((first #t))
                (lambda ()
                  (if first
                      (set! first #f)
                      (action))))))))))

;; Consider an inverter between two wires:

(reset)
(define a (make-bad-wire))
(define b (make-bad-wire))
(get-signal a) => 0
(get-signal b) => 0
(inverter a b)

;; Since `add-action!` is not calling the procedure right away, we've now added
;; an action to `a` but it has not been executed. The signals haven't changed:

(get-signal a) => 0
(get-signal b) => 0

;; This is an incorrect state. To fix it, we'd have to flip `a` on and off.
;; Therefore, we must execute actions right after adding them to ensure the
;; circuit is in a stable state.

;; Let's trace through the previous example without calling actions when they
;; are added. Nothing is printed when we call `probe` because the probe action
;; is not called immediately. Nothing is printed when we propagate setting
;; `input-1` to 1 either: it flows through the OR gate, but not through the AND
;; gate because the latter does not know its other input is 1 (that would have
;; required propagating the 0 from `input-2` through the other AND & NOT gates).
;; Finally, when we set `input-2` to 1 and propagate, it flows through the
;; circuit leaving `sum` at 0 (still no printing), but changing `carry` to 1
;; (which becomes the only thing printed).

(reset)
(define input-1 (make-bad-wire))
(define input-2 (make-bad-wire))
(define sum (make-bad-wire))
(define carry (make-bad-wire))
(probe 'sum sum) =/> ""
(probe 'carry carry) =/> ""
(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate) =/> ""
(set-signal! input-2 1)
(propagate) =/> ["carry 11 New-value = 1"]

(Section :3.3.4.5 "Implementing the agenda"
  (use (:3.3.2 delete-queue! empty-queue? front-queue insert-queue!
               make-queue)))

(define make-time-segment cons)
(define segment-time car)
(define segment-queue cdr)

(define (make-agenda) (list 0))
(define (simulation-time agenda) (car agenda))
(define (set-simulation-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda)  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (reset-agenda! agenda)
  (set-simulation-time! agenda 0)
  (set-segments! agenda '()))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments)) action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
          agenda
          (cons (make-new-time-segment time action)
                segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (when (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error 'first-agenda-item "agenda is empty")
    (let ((first-seg (first-segment agenda)))
      (set-simulation-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(Exercise ?3.32
  (use (:3.3.2 disable-stack-mode enable-stack-mode) (:3.3.4.1 and-gate)
       (:3.3.4.2 make-wire set-signal!) (:3.3.4.3 reset propagate)
       (:3.3.4.4 probe)))

;; The FIFO order of procedure queues for each segment must be used because it
;; causes actions to be executed in the same order as they were triggered. If
;; actions A1, A2, and A3 occur in that order, they will be inserted and popped
;; in that order. Executing them in reverse order leads to different, incorrect
;; behaviour. Consider an and-gate whose inputs change from 0, 1 to 1, 0:

(reset)
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(set-signal! a 0)
(set-signal! b 1)
(and-gate a b c)
(probe 'c c)
 =/> ["c 0 New-value = 0"]
(propagate) =/> ""
(set-signal! a 1)
(set-signal! b 0)
(propagate)
=/> ["c 6 New-value = 1"
     "c 6 New-value = 0"]

;; The value of `c` goes to 1, but settles to 0 once all actions are processed.
;; If we use a stack (FILO) rather than a queue (FIFO) for actions, there will
;; be a mismatch between the execution order and signal calculations. This is
;; because gates calculate `new-signal` immediately, and only delay setting the
;; output to that value. With FILO behaviour, `(set-signal! a 1)` will create an
;; action to set `c` to 1 (since `a` and `b` are 1). Then `(set-signal! b 0)`
;; will create an action to set `c` to 0, the correct final value. But the
;; actions execute in reverse order, so `c` ends up incorrectly at 1:

(enable-stack-mode)

(reset)
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(set-signal! a 0)
(set-signal! b 1)
(and-gate a b c)
(probe 'c c) =/> ["c 0 New-value = 0"]
(propagate) =/> ""
(set-signal! a 1)
(set-signal! b 0)
(propagate) =/> ["c 6 New-value = 1"]

(disable-stack-mode)

(Section :3.3.5 "Propagation of Constraints")

(Section :3.3.5.1 "Using the constraint system"
  (use (:3.3.5.2 adder constant multiplier probe)
       (:3.3.5.3 forget-value! make-connector set-value!)))

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)))

(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)
=/> ["Probe: Celsius temp = 25"
     "Probe: Fahrenheit temp = 77"]
(set-value! F 212 'user)
=!> "contradiction: 77 212"
(forget-value! C 'user)
=/> ["Probe: Celsius temp = ?"
     "Probe: Fahrenheit temp = ?"]
(set-value! F 212 'user)
=/> ["Probe: Fahrenheit temp = 212"
     "Probe: Celsius temp = 100"]

(Section :3.3.5.2 "Implementing the constraint system"
  (use (:3.3.5.3 connect forget-value! get-value has-value? set-value!)))

(define (adder a b sum)
  (define (process-new-value)
    (cond ((and (has-value? a) (has-value? b))
           (set-value! sum (+ (get-value a) (get-value b)) me))
          ((and (has-value? a) (has-value? sum))
           (set-value! b (- (get-value sum) (get-value a)) me))
          ((and (has-value? b) (has-value? sum))
           (set-value! a (- (get-value sum) (get-value b)) me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error 'adder "unknown request" request))))
  (connect a me)
  (connect b me)
  (connect sum me)
  me)

(define (multiplier x y product)
  (define (process-new-value)
    (cond ((or (and (has-value? x) (zero? (get-value x)))
               (and (has-value? y) (zero? (get-value y))))
           (set-value! product 0 me))
          ((and (has-value? x) (has-value? y))
           (set-value! product (* (get-value x) (get-value y)) me))
          ((and (has-value? x) (has-value? product))
           (set-value! y (/ (get-value product) (get-value x)) me))
          ((and (has-value? y) (has-value? product))
           (set-value! x (/ (get-value product) (get-value y)) me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! x me)
    (forget-value! y me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error 'multiplier "unknown request" request))))
  (connect x me)
  (connect y me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error 'constant "unknown request" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (display (format "\nProbe: ~a = ~a" name value)))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (print-probe (get-value connector)))
          ((eq? request 'I-lost-my-value)
           (print-probe "?"))
          (else (error 'probe "unknown request" request))))
  (connect connector me)
  me)

(Section :3.3.5.3 "Representing connectors"
  (use (:2.3.1 memq)))

;; Moved from Section 3.3.5.2 to avoid import cycle.
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))
    (define (set-value! new-val setter)
      (cond ((not (has-value? me))
             (set! value new-val)
             (set! informant setter)
             (for-each-except setter inform-about-value constraints))
            ((not (= value new-val))
             (error 'set-value! "contradiction" value new-val))
            (else 'ignored)))
    (define (forget-value! retractor)
      (cond ((eq? retractor informant)
             (set! informant #f)
             (for-each-except retractor inform-about-no-value constraints))
            (else 'ignored)))
    (define (connect new-constraint)
      (unless (memq new-constraint constraints)
        (set! constraints (cons new-constraint constraints)))
      (when (has-value? me)
        (inform-about-value new-constraint)))
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'get-value) value)
            ((eq? request 'set-value!) set-value!)
            ((eq? request 'forget-value!) forget-value!)
            ((eq? request 'connect) connect)
            (else (error 'make-connector "unknown operation" request))))
    me))

(define (for-each-except exception proc ls)
  (define (iter items)
    (unless (null? items)
      (unless (eq? (car items) exception)
        (proc (car items)))
      (iter (cdr items))))
  (iter ls))

(define (has-value? connector) (connector 'has-value?))
(define (get-value connector) (connector 'get-value))
(define (set-value! connector val who) ((connector 'set-value!) val who))
(define (forget-value! connector who) ((connector 'forget-value!) who))
(define (connect connector constraint) ((connector 'connect) constraint))

(Exercise ?3.33
  (use (:3.3.5.2 adder constant multiplier probe)
       (:3.3.5.3 forget-value! make-connector set-value!)))

(define (averager a b c)
  (let ((u (make-connector))
        (w (make-connector)))
    (adder a b u)
    (multiplier c w u)
    (constant 2 w)))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(averager a b c)
(probe "a" a)
(probe "c" c)

(set-value! a 10 'user)
=/> ["Probe: a = 10"]
(set-value! b 20 'user)
=/> ["Probe: c = 15"]
(forget-value! a 'user)
=/> ["Probe: a = ?"
     "Probe: c = ?"]
(set-value! c 99 'user)
=/> ["Probe: c = 99"
     "Probe: a = 178"]

(Exercise ?3.34
  (use (:3.3.5.2 multiplier probe)
       (:3.3.5.3 forget-value! make-connector set-value!)))

(define (bad-squarer a b)
  (multiplier a a b))

;; At first glance, Louis Reasoner's constraint seems okay:

(define a (make-connector))
(define b (make-connector))
(probe "a" a)
(probe "b" b)
(bad-squarer a b)
(set-value! a 5 'user)
=/> ["Probe: b = 25"
     "Probe: a = 5"]

;; However, going the other way doesn't work:

(forget-value! a 'user)
=/> ["Probe: b = ?"
     "Probe: a = ?"]
(set-value! b 49 'user)
=/> ["Probe: b = 49"]

;; Upon reflection, it would be remarkable if it performed a square root without
;; us ever coding the algorithm! The problem is, `multiplier` is too general.
;; Louis Reasoner's constraint does not take advantage of the extra information
;; specific to multiplications of a number to itself. It doesn't know that the
;; multiplicand and the multiplier are the same connector.

(Exercise ?3.35
  (use (:1.1.4 square) (:3.3.5.2 probe)
       (:3.3.5.3 connect forget-value! get-value has-value? make-connector
                 set-value!)))

(define (squarer a b)
  (define (process-new-value)
    (cond ((has-value? a)
           (set-value! b (square (get-value a)) me))
          ((has-value? b)
           (if (< (get-value b) 0)
               (error 'squarer "square less than 0" (get-value b))
               (set-value! a (sqrt (get-value b)) me)))))
  (define (process-forget-value)
    (forget-value! b)
    (forget-value! a)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error 'squarer "unknown request" request))))
  (connect a me)
  (connect b me)
  me)

(define a (make-connector))
(define b (make-connector))
(probe "a" a)
(squarer a b)
(set-value! b 49 'user) =/> ["Probe: a = 7"]

(Exercise ?3.36)

;; See whiteboard/exercise-3.36.jpg for the environment diagram.

(Exercise ?3.37
  (use (:3.3.5.2 constant adder multiplier probe) (:3.3.5.3 make-connector set-value!)))

(define (celsius->fahrenheit x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (cv val) (let ((c (make-connector))) (constant val c) c))
(define (c+ x y) (let ((z (make-connector))) (adder x y z) z))
(define (c- x y) (let ((z (make-connector))) (adder y z x) z))
(define (c* x y) (let ((z (make-connector))) (multiplier x y z) z))
(define (c/ x y) (let ((z (make-connector))) (multiplier y z x) z))

(define C (make-connector))
(define F (celsius->fahrenheit C))
(probe "C" C)
(set-value! F 212 'user) =/> ["Probe: C = 100"]

(Section :3.4 "Concurrency: Time Is of the Essence")

(Section :3.4.1 "The Nature of Time in Concurrent Systems")

(Exercise ?3.38)

(define balance 100)
(define (peter) (set! balance (+ balance 10)))
(define (paul) (set! balance (- balance 20)))
(define (mary) (set! balance (- balance (/ balance 2))))

;; (a) Out of six permutations, there are four possible values: 35, 40, 45, 50.

(set! balance 100) (peter) (paul) (mary) balance => 45
(set! balance 100) (peter) (mary) (paul) balance => 35
(set! balance 100) (paul) (peter) (mary) balance => 45
(set! balance 100) (paul) (mary) (peter) balance => 50
(set! balance 100) (mary) (peter) (paul) balance => 40
(set! balance 100) (mary) (paul) (peter) balance => 40

;; (b) If the system allows the processes to be interleaved, you could also get
;; results equivalent to leaving out one or more of the assignments, where the
;; new value is overwritten before being read. In `mary`, the value divided by
;; 2 could also be different from the value being subtracted from.

(define (in? x xs)
  (and (not (null? xs))
       (or (= (car xs) x) (in? x (cdr xs)))))

(set! balance 100)
(parallel-execute peter paul mary)
(in? balance '(25 30 35 40 45 50 55 60 80 90 110)) => #t

(Section :3.4.2 "Mechanisms for Controlling Concurrency")

(Section :3.4.2.1 "Serializing access to shared state"
  (use (:3.4.2.3 make-serializer) (?3.38 in?)))

;; Without serialization, there are five possible values:
(define x 10)
(parallel-execute
  (lambda () (set! x (* x x)))
  (lambda () (set! x (+ x 1))))
(in? x '(11 100 101 110 121)) => #t

;; With serialization, it narrows to two possible values:
(define x 10)
(let ((s (make-serializer)))
  (parallel-execute
    (s (lambda () (set! x (* x x))))
    (s (lambda () (set! x (+ x 1))))))
(in? x '(101 121)) => #t

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
                balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error 'make-account "unknown request" m))))
  dispatch))

(Exercise ?3.39
  (use (:3.4.2.3 make-serializer) (?3.38 in?)))

(define x 10)
(let ((s (make-serializer)))
  (parallel-execute
    (lambda () (set! x ((s (lambda () (* x x))))))
    (s (lambda () (set! x (+ x 1))))))

;; Three of the five values are still possible:
(in? x '(101   ; squared, then incremented
         121   ; incremented, then squared
         100)) ; incremented between squarer read and write
=> #t

(Exercise ?3.40
  (use (:2.2.3.1 filter) (:2.2.3.2 permutations)
       (:3.3.3.1 make-table insert! lookup) (:3.4.2.3 make-serializer)
       (?3.38 in?)))

(define x 10)
(parallel-execute
;; process S:  3        1 2
  (lambda () (set! x (* x x)))
;; process C:  7        4 5 6
  (lambda () (set! x (* x x x))))

;; There are five possible values:
(in? x '(100 1000 10000 100000 1000000)) => #t

;; We will now demonstrate how the steps can interleave to produce these values.
;; There are seven relevant steps: three in S, four in C. Steps 1, 2, 4, 5, and
;; 6 are reads; steps 3 and 7 are writes.

(define steps-s-read '(1 2))
(define steps-s-write '(3))
(define steps-c-read '(4 5 6))
(define steps-c-write '(7))
(define steps-s (append steps-s-read steps-s-write))
(define steps-c (append steps-c-read steps-c-write))
(define steps (append steps-s steps-c))

;; Only certain permutations of steps are valid orderings:

(define (good-interleave? p)
  (define (iter latest-s latest-c p)
    (or (null? p)
        (and (in? (car p) steps-s)
             (> (car p) latest-s)
             (iter (car p) latest-c (cdr p)))
        (and (in? (car p) steps-c)
             (> (car p) latest-c)
             (iter latest-s (car p) (cdr p)))))
  (iter 0 0 p))

(define possible-orders
  (filter good-interleave? (permutations steps)))

(length possible-orders) => 35

;; For a given ordering of steps, we can simulate the execution:

(define (execute-steps steps x)
  (define (iter s-reads c-reads steps x)
    (cond ((null? steps) x)
          ((in? (car steps) steps-s-read)
           (iter (cons x s-reads) c-reads (cdr steps) x))
          ((in? (car steps) steps-c-read)
           (iter s-reads (cons x c-reads) (cdr steps) x))
          ((in? (car steps) steps-s-write)
           (iter s-reads c-reads (cdr steps) (apply * s-reads)))
          ((in? (car steps) steps-c-write)
           (iter s-reads c-reads (cdr steps) (apply * c-reads)))
          (else (error 'execute-steps "unknown step number" (car steps)))))
  (iter '() '() steps x))

(define final-values
  (map (lambda (ss) (execute-steps ss 10)) possible-orders))

final-values
=> '(1000000 100000 10000 1000 100 100000 10000
     1000 100 10000 1000 100 1000 100
     10000 100000 10000 1000 100 10000 1000
     100 1000 100 10000 10000 1000 100
     1000 100 10000 1000 100 10000 1000000)

;; There are many duplicates, so let's count the occurrences.

(define (group xs)
  (let ((occurence-table (make-table)))
    (define (iter xs)
      (if (null? xs)
          occurence-table
          (let ((n (lookup (car xs) occurence-table)))
            (insert! (car xs)
                     (if n (+ n 1) 1)
                     occurence-table)
            (iter (cdr xs)))))
    (iter xs)))

(group final-values)
=> '(*table* (100 . 10)
             (1000 . 10)
             (10000 . 10)
             (100000 . 3)
             (1000000 . 2))

;; The largest value, 1000000, can be obtained in two ways:
(execute-steps '(1 2 3 4 5 6 7) 10) => 1000000 ; squared, then cubed
(execute-steps '(4 5 6 7 1 2 3) 10) => 1000000 ; cubed, then squared

;; If we serialize the procedures, we always get that value:
(define x 10)
(let ((s (make-serializer)))
    (parallel-execute
      (s (lambda () (set! x (* x x))))
      (s (lambda () (set! x (* x x x))))))
  x => 1000000

  (Exercise ?3.41
    (use (:3.4.2.1 make-account)))

  ;; Ben Bitdiddle is wrong. It is unnecessary to serialize access to the bank
  ;; balance because it would make no difference. If we serialize it, then the
  ;; value will be read either before or after (in sequence) it is written,
  ;; assuming someone is withdrawing or depositing concurrently. However, if we
  ;; don't serialize it, we still get one value or the other. There is nothing
  ;; that can be interleaved because reading the balance takes only one step,
  ;; assuming the Scheme implementation considers this a thread-safe operation.

  (Exercise ?3.42)

  ;; This is a safe change to make. Each bank account still has one serializer and
  ;; the deposit and withdraw procedures returned from the dispatcher are always
  ;; protected by it. It makes no difference in what concurrency is allowed. If it
  ;; did, then the specification of `make-serializer` must be incorrect.

  (Section :3.4.2.2 "Complexity of using multiple shared resources"
    (use (:3.4.2.3 make-serializer)))

  (define (exchange account1 account2)
    (let ((difference (- (account1 'balance)
                        (account2 'balance))))
      ((account1 'withdraw) difference)
      ((account2 'deposit) difference)))

  (define (make-account-and-serializer balance)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount)) balance)
    (let ((balance-serializer (make-serializer)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'balance) balance)
              ((eq? m 'serializer) balance-serializer)
              (else (error 'make-account "unknown request" m))))
      dispatch))

  (define (deposit account amount)
    (let ((s (account 'serializer))
          (d (account 'deposit)))
      ((s d) amount)))

  (define (serialized-exchange account1 account2)
    (let ((serializer1 (account1 'serializer))
          (serializer2 (account2 'serializer)))
      ((serializer1 (serializer2 exchange))
     account1
     account2)))

(Exercise ?3.43)

;; The balances in the accounts start out as $10, $20, and $30. Exchanging
;; balances A and B works by taking D = A - B, and then withdrawing to get A' =
;; A - D = B and depositing to get B' = B + D = A. Going from {A, B} to {B, A},
;; we can see that any sequence of exchanges preserves the set {A, B}, and in
;; this particular case {10, 20, 30}.
;;
;; Using the first version of `exchange`, where only individual deposits and
;; withdrawals are serialized (by account), the {10, 20, 30} set will not be
;; preserved. For example, let us refer to concurrent processes P and Q:

(define (exchange acc1 acc2)
  (let ((diff (- (acc1 'balance)    ; (1)
                 (acc2 'balance)))) ; (2)
    ((acc1 'withdraw) diff)         ; (3)
    ((acc2 'deposit) diff)))        ; (4)

;; Suppose accounts A, B, C begin with $10, $20, $30. Process P exchanges A and
;; B, process Q exchanges A and C. We interleave steps as P1, Q1, Q2, Q3, Q4,
;; P2, P3, P4. P finds the balance of A to be $10. Then Q carries out all its
;; steps: A and C are exchanged, and P's read operation does not affect this.
;; Therefore A, B, C now have balances $30, $20, $10. Now P carries out its
;; three remaining steps. It finds B to have $20. It calculates 10 - 20 = -10.
;; It withdraws -10 from A, leaving A with a balance of 30 - (-10) = 40. It
;; deposits this in B, leaving B with a balance of 20 + (-10) = 10. Now the
;; balances of A, B, C are $40, $10, $10. Thus {10, 20, 30} is not preserved.
;; But it does preserve the sum 10 + 20 + 30 = 40 + 10 + 10 = 60. This would be
;; the case even if `diff` was a random number, because deposits and withdrawals
;; are serialized, and we deposit and withdraw the same `diff`.
;;
;; If we used the original implementation but changed `make-account` so that it
;; did no serializing, the sum would not be preserved. This is because the steps
;; of the deposits and withdrawals of concurrent processes will interleave, and
;; we have already seen the issues this produces earlier.

(Exercise ?3.44)

(define (transfer from-acc to-acc amount)
  ((from-acc 'withdraw) amount) ; (1)
  ((to-acc 'deposit) amount))   ; (2)

;; Ben Bitdiddle is correct. This procedure will behave correctly, even with
;; multiple concurrent transfers involving the same accounts. Suppose we
;; transfer $10 between A and B (process P), and concurrently transfer $20
;; between A and C (process Q). P1 and Q1 will be in the same serialization set,
;; because they both withdraw from A. P2 and Q2 will neither be in that set nor
;; in each other's set. One deposits to B, and the other deposits to C. They
;; cannot interfere with each other.
;;
;; The essential difference between the transfer problem and the exchange
;; problem is that the exchange amount depends on the current balances, and so
;; it must include steps that read the balance, introducing a hole into which a
;; concurrent step can be interleaved, unless the whole exchange is serialized.

(Exercise ?3.45)

;; Louis Reasoner is wrong. The problem with automatically serializing all
;; deposits and withdrawals is that, when we create our own multi-step
;; operations such as the exchange or the transfer, and we serialize them, we
;; end up with nested serialization, i.e. deadlock.

(Section :3.4.2.3 "Implementing serializers")

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (lambda args
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val)))))

;; We cannot use this implementation because not all the Schemes we're targeting
;; support atomics for a proper implementation of `test-and-set!`. Instead, we
;; define `make-mutex` in src/compat to use the Scheme's own threading library.
(define (make-mutex-from-scratch)
  (let ((cell (list #f)))
    (lambda (m)
      (cond ((eq? m 'acquire)
             (let retry () (when (test-and-set! cell) (retry))))
            ((eq? m 'release) (clear! cell))))))

(define (clear! cell)
  (set-car! cell #f))
(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

(Exercise ?3.46
  (use (:3.4.2.3 make-mutex-from-scratch) (?3.38 in?)))

(define make-mutex make-mutex-from-scratch)
(paste (:3.4.2.3 make-serializer))

;; Suppose we execute this using the non-atomic `test-and-set!`:
(define x 100)
(let ((s (make-serializer)))
  (parallel-execute
    ;;              2            1
    (s (lambda () (set! x (+ 100 x)))) ; process P
    ;;              2        1
    (s (lambda () (set! x (/ x 2)))))) ; process Q

;; There are four possible values:
(in? x '(100  ; P1, P2, Q1, Q2 (serial)
         150  ; Q1, Q2, P1, P2 (serial)
         200  ; P1, Q1, Q2, P2 (interleaved)
         50)) ; Q1, P1, P2, Q2 (interleaved)

;; The interleaved results can happen since with the non-atomic `test-and-set!`,
;; both processes can acquire the mutex at the same time. Both check if the cell
;; is set, find it is #f, and then both set it to #t.

(Exercise ?3.47
  (use (:3.4.2.3 clear! test-and-set!) (?3.38 in?)))

;;; (a) Sempahore in terms of mutexes

;; Note: This assumes that any thread can acquire/release the mutex. It does not
;; work with mutex implementations that only allow the owner to release it.
(define (make-semaphore n)
  (let ((count n)
        (count-mutex (make-mutex))
        (queue-mutex (make-mutex)))
    (queue-mutex 'acquire) ; starts out locked
    (lambda (m)
      (cond ((eq? m 'acquire)
             (count-mutex 'acquire)
             (set! count (- count 1))
             (when (< count 0)
               (count-mutex 'release)
               (queue-mutex 'acquire))
             (count-mutex 'release))
            ((eq? m 'release)
             (count-mutex 'acquire)
             (set! count (+ count 1))
             (if (<= count 0)
                 (queue-mutex 'release)
                 (count-mutex 'release)))
            (else (error 'make-semaphore "unexpected message" m))))))

;; We can't test this because not all Schemes we support have the required
;; property of allowing any thread to release another's mutex.

;;; (b) Semaphore in terms of atomic `test-and-set!` operations

(define (make-semaphore-from-scratch n)
  (let ((count n)
        (cell (list #f)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (let retry () (when (test-and-set! cell) (retry)))
             (cond ((> count 0)
                    (set! count (- count 1))
                    (clear! cell))
                   (else (clear! cell)
                         (the-semaphore 'acquire)))) ; busy wait
            ((eq? m 'release)
             (let retry () (when (test-and-set! cell) (retry)))
             (set! count (+ 1 count))
             (clear! cell))
            (else (error 'make-semaphore-from-scratch "unexpected message" m))))
    the-semaphore))

;; We can't test this because our `test-and-set!` is not actually atomic.

(Section :3.4.2.4 "Deadlock")

;; One way to avoid deadlock is to give each account a unique identification
;; number, and write procedures like `exchange` so that they always try to
;; acquire the mutex for the lower-numbered account first.

(Exercise ?3.48
  (use (:3.4.2.2 exchange) (:3.4.2.3 make-serializer)))

;; Before we locked the `exchange` operation using the serializers of both
;; accounts. This can lead to deadlock if lock sequences A, B and B, A are
;; interleaved such that both processes are trying to acquire the mutex that the
;; other has already acquired. They wait forever, and neither is released. This
;; problem is solved when we lock accounts in a particular order because that
;; interleaving wouldn't work. Both processes would have the lock sequence A, B,
;; and the second process cannot acquire A after the first already has. The
;; first is then free to acquire B, perform its operations, and release both.

(define *uuid* 0)
(define (gen-uuid)
  (set! *uuid* (+ *uuid* 1))
  *uuid*)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (let ((id (gen-uuid))
        (s (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) s)
            ((eq? m 'identifier) id)
            (else (error 'make-account "unknown request" m))))
    dispatch))

(define (serialized-exchange a1 a2)
  (let ((s1 (a1 'serializer))
        (s2 (a2 'serializer)))
    ((if (< (a1 'identifier) (a2 'identifier))
       (s1 (s2 exchange))
       (s2 (s1 exchange)))
     a1
     a2)))

;; This is safe from deadlocks:
(define a1 (make-account 10))
(define a2 (make-account 20))
(parallel-execute
  (lambda () (serialized-exchange a1 a2))
  (lambda () (serialized-exchange a2 a1)))
(a1 'balance) => 10
(a2 'balance) => 20

(Exercise ?3.49)

;; The deadlock avoidance mechanism used in Exercise 3.48 would not work with
;; `(contrived-exchange acc)`, which exchanges the balance of `acc` with that of
;; the account whose balance is closest to the balance of `acc`. We must either
;; always lock `acc` first (without the ordering mechanism of 3.48, allowing
;; deadlocks), or we must lock after accessing `acc`, creating a hole into which
;; other operations can be interleaved.

(Section :3.4.2.5 "Concurrency, time, and communication")

;; Concurrency is hard. It is intimately tied to communication. There may be
;; cases where the "real" value (e.g. account balance) are irrelevant or
;; meaningless except at special synchronization points.

(Section :3.5 "Streams")

(Section :3.5.1 "Streams Are Delayed Lists")

(define (stream-ref s n)
  (if (zero? n)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map f s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (f (stream-car s))
                   (stream-map f (stream-cdr s)))))
(define (stream-for-each f s)
  (unless (stream-null? s)
    (f (stream-car s))
    (stream-for-each f (stream-cdr s))))

(define (display-stream s) (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

;; `cons-stream` is defined in src/lang/sicp.ss.
(define the-empty-stream '())
(define stream-null? null?)
(define (stream-car s) (car s ))
(define (stream-cdr s) (force (cdr s)))

(Section :3.5.1.1 "The stream implementation in action"
  (use (:3.5.1 stream-car stream-cdr stream-null? the-empty-stream)
       (?1.23 prime?)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

;; Abbreviations to save space below.
(define a stream-car)
(define d stream-cdr)
(define i stream-enumerate-interval)
(define f stream-filter)
(define p? prime?)

(a (d (f p? (i 10000 1000000))))
=> (a (d (f p? (cons 10000 (delay (i 10001 1000000))))))
;; ... 10001 through 10006 ...
=> (a (d (f p? (cons 10007 (delay (i 10008 1000000))))))
=> (a (d (cons 10007 (delay (f p? (cons 10008 (delay (i 10009 1000000))))))))
=> (a (f p? (cons 10008 (delay (i 10009 1000000)))))
=> (a (f p? (cons 10009 (delay (i 10010 1000000)))))
=> (a (cons 10009 (delay (f p? (cons 10010 (delay (i 10011 1000000)))))))
=> 10009

(Section :3.5.1.2 "Implementing delay and force")

;; `delay` is a special form such that `(delay EXPR)` is syntactic sugar for
;; `(lambda () EXPR)`. `force` can be implemented as procedure, as done below.
;; However, outside this section we use the Scheme implementation's versions.

(define (force delayed-object) (delayed-object))

(define (memo-proc proc)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (unless already-run?
        (set! result (proc))
        (set! already-run? #t))
        result)))

;; We would then define `(delay EXPR)` to be `(memo-proc (lambda () EXPR))`.

(Exercise ?3.50
  (use (:3.5.1 stream-car stream-cdr stream-null? the-empty-stream)))

(define (stream-map f . ss)
  (if (stream-null? (car ss))
      the-empty-stream
      (cons-stream
        (apply f (map stream-car ss))
        (apply stream-map f (map stream-cdr ss)))))

(Exercise ?3.51
  (use (:3.5.1 display-line stream-map stream-ref)
       (:3.5.1.1 stream-enumerate-interval)))

(define (show x) (display-line x) x)

(define x)
(set! x (stream-map show (stream-enumerate-interval 0 10)))
=/> ["0"]

(stream-ref x 5) =/> ["1" "2" "3" "4" "5"]
(stream-ref x 5) => 5
(stream-ref x 7) =/> ["6" "7"]
(stream-ref x 7) => 7

(Exercise ?3.52
  (use (:3.5.1 display-stream stream-map stream-ref)
       (:3.5.1.1 stream-enumerate-interval stream-filter)))

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
sum => 1
(define y (stream-filter even? seq))
sum => 6
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
sum => 10
(stream-ref y 7) => 136
sum => 136
(display-stream z) =/> ["10" "15" "45" "55" "105" "120" "190" "210"]

;; Yes, responses would differ if `delay` did not use memoization. The stream
;; `seq` would get its `cdr` evaluated multiple times, and it would be different
;; each time because `sum` keeps increasing. Instead of 1, 6, 10, 120, it would
;; be 1, 6, 15, 162. Displaying `z` would only show 15. The rest of `seq` gets
;; generated using a much higher `sum`, none of which are divisible by 5.

(Section :3.5.2 "Infinite Streams"
  (use (:3.5.1 stream-car stream-cdr stream-ref) (:3.5.1.1 stream-filter)))

(define (stream-take s n)
  (cond ((zero? n) '())
        (else (cons (stream-car s) (stream-take (stream-cdr s) (- n 1))))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ 1 n))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7))) integers))
(stream-ref no-sevens 100) => 117

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
(stream-take fibs 10) => '(0 1 1 2 3 5 8 13 21 34)

(define (sieve s)
  (cons-stream
    (stream-car s)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car s))))
             (stream-cdr s)))))
(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50) => 233

(Section :3.5.2.1 "Defining streams implicitly"
  (use (:1.1.4 square) (:3.5.1 stream-car stream-cdr stream-ref)
       (:3.5.1.1 stream-filter)
       (:3.5.2 divisible? integers-starting-from stream-take)
       (?3.50 stream-map)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))
(stream-take integers 10) => '(1 2 3 4 5 6 7 8 9 10)

(define fibs
  (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
(stream-take fibs 10) => '(0 1 1 2 3 5 8 13 21 34)

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (negate-stream stream) (scale-stream stream -1))

(define double
  (cons-stream 1 (scale-stream double 2)))
(stream-take double 10) => '(1 2 4 8 16 32 64 128 256 512)

(define primes
  (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))
(define (prime? n)
  (define (iter ps)
    (or (> (square (stream-car ps)) n)
        (and (not (divisible? n (stream-car ps)))
             (iter (stream-cdr ps)))))
  (iter primes))
(stream-ref primes 50) => 233

(Exercise ?3.53
  (use (:3.5.2 stream-take) (:3.5.2.1 add-streams)))

(define s (cons-stream 1 (add-streams s s)))

;; It produces all powers of two, just like `double` in Section 3.5.2.1. This
;; can be seen from the fact that `x * 2` (scaling a stream by 2) is the same as
;; `x + x` (adding a stream to itself).

(stream-take s 10) => '(1 2 4 8 16 32 64 128 256 512)

(Exercise ?3.54
  (use (:3.5.2 integers-starting-from stream-take) (?3.50 stream-map)))

(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))
(stream-take factorials 5) => '(1 2 6 24 120)

(Exercise ?3.55
  (use (:3.5.1 stream-car stream-cdr) (:3.5.2 stream-take)
       (:3.5.2.1 add-streams integers)))

(define (partial-sums s)
  (define self (cons-stream (stream-car s) (add-streams self (stream-cdr s))))
  self)
(stream-take (partial-sums integers) 10) => '(1 3 6 10 15 21 28 36 45 55)

(Exercise ?3.56
  (use (:3.5.1 stream-car stream-cdr stream-null?) (:3.5.2 stream-take)
       (:3.5.2.1 scale-stream)))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((x1 (stream-car s1))
                    (x2 (stream-car s2)))
                (cond ((< x1 x2) (cons-stream x1 (merge (stream-cdr s1) s2)))
                      ((> x1 x2) (cons-stream x2 (merge s1 (stream-cdr s2))))
                      (else (cons-stream
                              x1
                              (merge (stream-cdr s1) (stream-cdr s2)))))))))
(define S
  (cons-stream 1 (merge (scale-stream S 2)
                        (merge (scale-stream S 3)
                               (scale-stream S 5)))))

(stream-take S 10) => '(1 2 3 4 5 6 8 9 10 12)

(Exercise ?3.57)

;; See proofs.pdf for the proof that the number of additions when computing the
;; nth Fibonacci number using `fibs` from Section 3.5.2.1 would be exponentially
;; greater if `delay` is implemented without memoization.

(Exercise ?3.58
   (use (:3.5.2 stream-take)))

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

;; This procedure produces the stream of digits in the base given by `radix`
;; that represent the quotient of `num` and `den`. It does it without using
;; floating-point operations.

(stream-take (expand 1 7 10) 10) => '(1 4 2 8 5 7 1 4 2 8)
(inexact (/ 1 7)) ~> 0.14285714285714285
(stream-take (expand 3 8 10) 10) => '(3 7 5 0 0 0 0 0 0 0)
(inexact (/ 3 8)) ~> 0.375

(Exercise ?3.59
  (use (:2.2.3.1 accumulate) (:3.5.2 integers-starting-from stream-take)
       (:3.5.2.1 negate-stream) (?3.50 stream-map)))

;;; (a) Integration

(define (integrate-series power-series)
  (stream-map / power-series (integers-starting-from 1)))

;;; (b) Exponential, sine, cosine

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define cosine-series
  (cons-stream 1 (negate-stream (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;;; Evaluating series

(define (eval-series s x n)
  (let ((terms (stream-map (lambda (c k) (* c (expt x k)))
                           s
                           (integers-starting-from 0))))
    (accumulate + 0.0 (stream-take terms n))))

(eval-series exp-series 1.0 15) ~> 2.7182818285

(Exercise ?3.60
  (use (:3.5.1 stream-car stream-cdr) (:3.5.2.1 add-streams scale-stream)
       (?3.59 cosine-series eval-series sine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

;; Verifying that sin^2(x) + cos^2(x) = 1. We can rely on the result being
;; exactly 1, not just approximately 1, because the identity is satisfied all
;; the way in the partial sums, not just in the limit.
(eval-series (add-streams (mul-series sine-series sine-series)
                          (mul-series cosine-series cosine-series))
             (random 100)
             15)
=> 1.0

(Exercise ?3.61
  (use (:3.5.1 stream-car stream-cdr) (:3.5.2.1 negate-stream scale-stream)
       (?3.60 mul-series)))

(define (invert-unit-series s)
  (cons-stream
    1
    (negate-stream (mul-series (stream-cdr s)
                               (invert-unit-series s)))))

;; Note: `invert-series` requires a nonzero constant term.
(define (invert-series s)
  (let ((constant (stream-car s)))
    (cond ((= constant 0) (error 'invert-series "division by zero" s))
          ((= constant 1) (invert-unit-series s))
          (else (scale-stream
                  (invert-unit-series (scale-stream s (/ constant)))
                  (/ constant))))))

(Exercise ?3.62
  (use (:3.5.1 stream-car) (?3.59 cosine-series eval-series sine-series)
       (?3.60 mul-series) (?3.61 invert-series)))

(define (div-series s1 s2)
  (let ((den-constant (stream-car s2)))
    (cond ((zero? den-constant) (error 'div-series "division by zero" s1 s2))
          (else (mul-series s1 (invert-series s2))))))

(define tangent-series (div-series sine-series cosine-series))

(eval-series tangent-series (atan 0.123) 10) ~> 0.123

(Section :3.5.3 "Exploiting the Stream Paradigm")

(Section :3.5.3.1 "Formulating iterations as stream processes"
  (use (:1.1.4 square) (:1.1.7 improve)
       (:3.5.1 stream-car stream-cdr stream-ref) (:3.5.2.1 scale-stream)
       (?3.50 stream-map) (?3.55 partial-sums)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (improve guess x))
                  guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n) (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(stream-ref (sqrt-stream 2) 10)
~> 1.414213562373095
(stream-ref pi-stream 10)
~> 3.232315809405594
(stream-ref (euler-transform pi-stream) 10)
~> 3.1417360992606667
(stream-ref (accelerated-sequence euler-transform pi-stream) 8)
~> 3.141592653589793

;; The 11th term is NaN, probably because a denominator becomes extremely tiny.
(stream-ref (accelerated-sequence euler-transform pi-stream) 10)
=> +nan.0

(Exercise ?3.63
  (use (:1.1.7 improve) (?3.50 stream-map)))

(define (sqrt-stream x)
  (cons-stream
    1.0
    (stream-map (lambda (guess) (improve guess x))
                (sqrt-stream x))))

;; This implementation is less efficient because it doesn't take advantage of
;; memoization. It improves the first guess to get the second item, but then it
;; must do that all over again in the recursive call to improve the second
;; gues when it calls `(sqrt-stream x)`. If our implementation of `delay` didn't
;; use the optimization provided by `memo-proc`, then there would be no
;; difference in efficiency between the original procedure and this one.

(Exercise ?3.64
  (use (:3.5.1 stream-car stream-cdr) (:3.5.3.1 sqrt-stream)))

(define (stream-limit s tolerance)
  (define (iter prev s)
    (let ((next (stream-car s)))
      (if (< (abs (- prev next)) tolerance)
          next
          (iter next (stream-cdr s)))))
  (iter (stream-car s) (stream-cdr s)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.1) ~> 1.4166666666666665
(sqrt 2 0.0001) ~> 1.4142135623746899

(Exercise ?3.65
  (use (:3.5.1 stream-ref) (:3.5.2.1 negate-stream)
       (:3.5.3.1 accelerated-sequence euler-transform) (?3.55 partial-sums)))

(define (ln-2-summands n)
  (cons-stream (/ 1.0 n) (negate-stream (ln-2-summands (+ n 1)))))
(define ln-2-stream
  (partial-sums (ln-2-summands 1)))

;; This converges fairly slowly on its own:
(stream-ref ln-2-stream 10) ~> 0.7365440115440116

;; The accelerated sequence converges much faster:
(define accel-ln-2-stream (accelerated-sequence euler-transform ln-2-stream))
(stream-ref accel-ln-2-stream 8) ~> (log 2)

(Section :3.5.3.2 "Infinite streams of pairs"
  (use (:3.5.1 stream-car stream-cdr stream-null?) (:3.5.2 integers stream-take)
       (?3.50 stream-map)))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (cond ((stream-null? s1) s2)
        (else (cons-stream (stream-car s1)
                           (interleave s2 (stream-cdr s1))))))

(define integer-pairs (pairs integers integers))
(define first-10-pairs (stream-take integer-pairs 10))
first-10-pairs => '((1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6))

(Exercise ?3.66
  (use (:2.2.3.1 enumerate-interval) (:3.5.1 stream-car stream-cdr stream-ref)
       (:3.5.2 divisible?) (:3.5.3.2 first-10-pairs integer-pairs)))

;; Let f(n) be the nth pair in the stream, starting at 0.
;;
;;     f(0)  = (1,1); f(n) = (1,_) when n = 1  + 2k
;;     f(2)  = (2,2); f(n) = (2,_) when n = 4  + 4k
;;     f(6)  = (3,3); f(n) = (3,_) when n = 10 + 8k
;;     f(14) = (4,4); f(n) = (4,_) when n = 22 + 16k
;;     ...
;;
;; The next row (where the first number in the pair is incremented once) always
;; gets updated half as often as the previous row. In general,
;;
;;     f(2^x - 2) = (x,x);
;;     f(2^x - 2 + 2^(x-1) + (k - 1)2^x) = (x,x+k), k > 0.
;;
;; We can write a function to find the index of a given pair:

(define (pair->index pair)
  (let* ((x (car pair))
         (k (- (cadr pair) x)))
    (cond ((zero? k) (- (expt 2 x) 2))
          (else (- (* (expt 2 (- x 1))
                      (+ (* 2 k) 1))
                   2)))))

(pair->index '(1 100)) => 197
(pair->index '(99 100)) => 950737950171172051122527404030
(pair->index '(100 100)) => (- (expt 2 100) 2)

;; We can confirm that it's correct for small indices:

(map pair->index first-10-pairs) => (enumerate-interval 0 9)
(stream-ref integer-pairs 197) => '(1 100)
(define random-pair (list (+ 1 (random 5)) (+ 1 (random 50))))
(define random-index (pair->index random-pair))
(<= random-index 1454) => #t ; ensuring it's not too large
(stream-ref integer-pairs random-index) => random-pair

;; Going the other way, from index to pair, is possible without generating the
;; stream. But there is no simple closed form solution. For an index n, the
;; first element of the pair (from which we can easily solve for the second) is
;; given by https://oeis.org/A091090.

(define (index->pair n)
  ;; Maintains the invariants a = 2^x, b = 3*2^(x-1).
  (define (iter x a b)
    (cond ((divisible? (- n (- b 2)) a)
           (list x (+ x (/ (+ n 2 (* -3/2 a)) a) 1)))
          (else (iter (+ x 1) (* a 2) (* b 2)))))
  (let ((x (log (+ n 2) 2)))
    (cond ((integer? x) (list (exact x) (exact x)))
          (else (iter 1 2 3)))))

(map index->pair (enumerate-interval 0 9)) => first-10-pairs
(index->pair 197) => '(1 100)
(index->pair random-index) => random-pair

(Exercise ?3.67
  (use (:3.5.1 stream-car stream-cdr) (:3.5.2 integers stream-take)
       (:3.5.3.2 interleave) (?3.50 stream-map)))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (interleave
        (stream-map (lambda (x) (list x (stream-car t)))
                    (stream-cdr s))
        (pairs (stream-cdr s) (stream-cdr t))))))

(stream-take (pairs integers integers) 10)
=> '((1 1) (1 2) (2 1) (1 3) (2 2) (1 4) (3 1) (1 5) (2 3) (1 6))

(Exercise ?3.68
  (use (:3.5.1 stream-car stream-cdr) (:3.5.3.2 interleave) (?3.50 stream-map)))

(define (pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))

;; No, this will not work: there will be an infinite loop. The recursive call
;; `(pairs (stream-cdr s) (stream-cdr t))` is not delayed by a `cons-stream`,
;; so the procedure will never return.

(Exercise ?3.69
  (use (:1.1.4 square) (:3.5.1 stream-car stream-cdr) (:3.5.1.1 stream-filter)
       (:3.5.2 integers) (:3.5.3.2 interleave pairs) (?3.50 stream-map)))

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (stream-cdr (pairs t u)))
      (triples (stream-cdr s)
              (stream-cdr t)
              (stream-cdr u)))))

(define pythagorean-triples
  (stream-filter
    (lambda (x)
      (= (+ (square (car x)) (square (cadr x))) (square (caddr x))))
    (triples integers integers integers)))

(stream-car pythagorean-triples) => '(3 4 5)

(Exercise ?3.70
  (use (:3.5.1 stream-car stream-cdr stream-null?) (:3.5.1.1 stream-filter)
       (:3.5.2 divisible? integers stream-take) (?3.50 stream-map)))

(define (merge-weighted s1 s2 weight)
  (define (merge s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else (let ((x1 (stream-car s1))
                      (x2 (stream-car s2)))
                  (if (<= (weight x1) (weight x2))
                      (cons-stream x1 (merge (stream-cdr s1) s2))
                      (cons-stream x2 (merge s1 (stream-cdr s2))))))))
  (merge s1 s2))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car s))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

;; (a) Pairs of positive integers (i, j) with i <= j ordered by i + j.
(define (a-weight p) (apply + p))
(define a-stream (weighted-pairs integers integers a-weight))
(define a-10 (stream-take a-stream 10))
a-10 => '((1 1) (1 2) (1 3) (2 2) (1 4) (2 3) (1 5) (2 4) (3 3) (1 6))
(map a-weight a-10) => '(2 3 4 4 5 5 6 6 6 7)

;; (b) Pairs of positive integers (i, j) with i <= j, where neither i nor j is
;; divisible by 2, 3, or 5, and the pairs are ordered by 2i + 3j + 5ij.
(define (b-weight p)
  (+ (* 2 (car p))
    (* 3 (cadr p))
    (* 5 (car p) (cadr p))))
(define b-stream
  (let ((filtered
         (stream-filter
           (lambda (x)
             (not (or (divisible? x 2) (divisible? x 3) (divisible? x 5))))
           integers)))
    (weighted-pairs filtered filtered b-weight)))
(define b-10 (stream-take b-stream 10))
b-10 => '((1 1) (1 7) (1 11) (1 13) (1 17) (1 19) (1 23) (1 29) (1 31) (7 7))
(map b-weight b-10) => '(10 58 90 106 138 154 186 234 250 280)

(Exercise ?3.71
  (use (:1.3 cube) (:3.5.1 stream-car stream-cdr) (:3.5.2 integers stream-take)
       (?3.70 weighted-pairs)))

(define (weight p) (+ (cube (car p)) (cube (cadr p))))
(define (analyze pairs)
  (let* ((w1 (weight (stream-car pairs)))
          (w2 (weight (stream-car (stream-cdr pairs)))))
    (if (= w1 w2)
        (cons-stream w1 (analyze (stream-cdr (stream-cdr pairs))))
        (analyze (stream-cdr pairs)))))
(define ramanujan-numbers
  (analyze (weighted-pairs integers integers weight)))

(stream-take ramanujan-numbers 6) => '(1729 4104 13832 20683 32832 39312)

(Exercise ?3.72
  (use (:1.1.4 square) (:3.5.1 stream-car stream-cdr)
       (:3.5.2 integers stream-take) (?3.70 weighted-pairs)))

(define (weight p) (+ (square (car p)) (square (cadr p))))
(define (analyze pairs)
  (let* ((x1 (stream-car pairs))
         (x2 (stream-car (stream-cdr pairs)))
         (x3 (stream-car (stream-cdr (stream-cdr pairs))))
         (w1 (weight x1)))
    (if (= w1 (weight x2) (weight x3))
        (cons-stream
          (list w1 x1 x2 x3)
          (analyze (stream-cdr (stream-cdr (stream-cdr pairs)))))
        (analyze (stream-cdr pairs)))))
(define thrice-square-sums
  (analyze (weighted-pairs integers integers weight)))

(stream-take thrice-square-sums 3)
=> '((325 (1 18) (6 17) (10 15))
     (425 (5 20) (8 19) (13 16))
     (650 (5 25) (11 23) (17 19)))

(Section :3.5.3.3 "Streams as signals"
  (use (:3.5.2.1 add-streams scale-stream)))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

) ; end of SICP
) ; end of library
