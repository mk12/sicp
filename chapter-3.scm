;;; Copyright 2014 Mitchell Kember. Subject to the MIT License.
;;; Structure and Interpretation of Computer Programs
;;; Chapter 3: Modularity, Objects, and State

;;;;; Section 3.1: Assignment and local state

;;; ex 3.1
(define (make-accumulator amount)
  (lambda (increment)
    (set! amount (+ amount increment))
    amount))
(define A (make-accumulator 5))
(A 10) ; => 15
(A 10) ; => 25

;;; ex 3.2
(define (make-monitored f)
  (let ((n-calls 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) n-calls)
            ((eq? x 'reset-count) (set! n-calls 0))
            (else (set! n-calls (+ n-calls 1))
                  (f x))))))
(define s (make-monitored sqrt))
(s 100)              ; => 10
(s 'how-many-calls?) ; => 1

;;; ex 3.3
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
            (else (error "Unknown request: MAKE-ACCOUNT" m)))
      (lambda (_) "Incorrect password")))
  dispatch)
(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)    ; => 60
((acc 'some-other-password 'deposit) 50) ; => "Incorrect password"

;;; ex 3.4
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
              (else (error "Unknown request: MAKE-ACCOUNT" m)))
        (lambda (_)
          (set! consecutive-wrong (+ consecutive-wrong 1))
          (if (> consecutive-wrong 7)
            (call-the-cops)
            "Incorrect password"))))
    dispatch))
(define (call-the-cops) "PUT YOUR HANDS UP!")
(define acc (make-account 'securw 100))
((acc 'secure 'withdraw) 100) ; => "Incorrect password"
((acc 'secure 'withdraw) 100) ; => "Incorrect password"
((acc 'secure 'withdraw) 100) ; => "Incorrect password"
((acc 'secure 'withdraw) 100) ; => "Incorrect password"
((acc 'secure 'withdraw) 100) ; => "Incorrect password"
((acc 'secure 'withdraw) 100) ; => "Incorrect password"
((acc 'secure 'withdraw) 100) ; => "Incorrect password"
((acc 'secure 'withdraw) 100) ; => "PUT YOUR HANDS UP!"

;;; ssec 3.1.2 (benefits of assignment)
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

;;; ex 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random-real)))))
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
(estimate-pi 100000.0) ; => 3.14016

;;; ex 3.6
(define rand
  (let ((x random-init))
    (lambda (message)
      (cond ((eq? message 'generate)
             (set! x (rand-update x))
             x)
            ((eq? message 'reset)
             (lambda (new-x)
               (set! x new-x)))
            (else (error "message not recognized: RAND" (list message)))))))

;;; ex 3.7
(define (make-joint pp-acc password new-password)
  (lambda (p m)
    (if (eq? p new-password)
      (pp-acc password m)
      (error "Incorrect password"))))

;;; ex 3.8
(define f
  (let ((x 0))
    (lambda (y)
      (let ((old-x x))
        (set! x y)
        old-x))))

;;;;; Section 3.2: The environment model of evaluation

;;; ex 3.9
(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))
;; Six environments are created:
; E1 -> [n: 6]
; E2 -> [n: 5]
; E3 -> [n: 4]
; E4 -> [n: 3]
; E5 -> [n: 2]
; E6 -> [n: 1]
(define (factorial n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count) product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))
;; Eight environments are created:
; E1 -> [n: 6]
; E2 -> [p: 1,   c: 1, m: 6]
; E3 -> [p: 1,   c: 2, m: 6]
; E4 -> [p: 2,   c: 3, m: 6]
; E5 -> [p: 6,   c: 4, m: 6]
; E6 -> [p: 24,  c: 5, m: 6]
; E7 -> [p: 120, c: 6, m: 6]
; E8 -> [p: 720, c: 7, m: 6]

;;; ex 3.10
;; With or without the explicit state variable, `make-withdraw` creates objects
;; with the same behaviour. The only difference with the expllicit variable in
;; the let-form is that there is an extra environment. Applying `make-writhdraw`
;; creates E1 to bind 100 to `initial-amount`, and then the let-form dsugars to
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

;;; ex 3.11
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
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)
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
((acc 'deposit) 40) ; => 90
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
((acc 'withdraw) 60) ; => 30
;; This is almost the same, except the procedure returns the `withdraw`
;; procedures instead. I am reusing the names E2 and E3 because they have been
;; used and are no longer relevant, since nothing poitns to them.
; E2 [m: withdraw]--+
;                   +---> E1 [balance: 30, ...]
; E3 [amount: 60]---+
;; All this time, the local state for `acc` is kept in E1, the environment
;; originally created to apply the `make-account` procedure. If we define
;; another account with `(define acc2 (make-account 100))`, it will have its own
;; environment containing `balance` and bindings for the interal procedures. The
;; only thing shared between `acc` and `acc2` is (possibly) the code for the
;; internal procedures, including `dispatch`, which the accounts really are.
;; This sharing is an implementation detail, though.

;;;;; Section 3.3: Modeling with mutable data

;;; ex 3.12
(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z ; => (a b c d)
(cdr x) ; => (b)
; x->[*|*]->[*|X]
;     |      |
;     V      V
;     a      b
(define w (append! x y))
w ; => (a b c d)
(cdr x) ; => (b c d)
;                 y
;                 |
; x->[*|*]->[*|*]->[*|*]->[*|X]
; w/  |      |      |      |
;     V      V      V      V
;     a      b      c      d

;;; ex 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))
;    +-------------------+
;    V                   |
; z->[*|*]->[*|*]->[*|*]-+
;     |      |      |
;     V      V      V
;     a      b      c
;; If we try to compute `(last-pair z)`, we will never finish because the list
;; is not null-terminated and so `null?` will never be true. We will be stuck in
;; an infinite recursion.

;;; ex 3.14
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
(define v (list 'a 'b 'c 'd))
; v->[*|*]->[*|*]->[*|*]->[*|X]
;     |      |      |      |
;     V      V      V      V
;     a      b      c      d
(define w (mystery v))
v ; => (a)
w ; => (d c b a)
; v->[*|X]<-[*|*]<-[*|*]<-[*|*]<-w
;     |      |      |      |
;     V      V      V      V
;     a      b      c      d
;; These box-and-pointer diagrams make it obvious that `mystery` simply changes
;; the directions of all the arrows.
