;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src chapter-3)
  (export chapter-3-effects)
  (import (rnrs base (6))
          (src lang sicp)
          (only (src chapter-2) chapter-2-effects))

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
        (lambda (_) "Incorrect password")))
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
          (lambda (_)
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

(Section :3.2.1 "The Benefits of Introducing Assignment")

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
  (use (:1.1.4 square) (:3.2.1 monte-carlo rand)))

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
  (use (:3.2.1 random-init rand-update)))

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

) ; end of SICP
) ; end of library
#|
;;; ex 3.7
(define (make-joint pp-acc password new-password)
  (lambda (p m)
    (if (eq? p new-password)
      (pp-acc password m)
      (error 'make-joint "Incorrect password"))))

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
;; with the same behaviour. The only difference with the explicit variable in
;; the let-form is that there is an extra environment. Applying `make-writhdraw`
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
          (else (errorf 'dispatch "Unknown request: ~s" m))))
  dispatch)
;; First, we just have a procedure bound in the global environment.
; global env --> [make-account: ...]
(check
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
  ((acc 'withdraw) 60) => 30)
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
(define (my-append x y)
  (if (null? x)
    y
    (cons (car x) (my-append (cdr x) y))))
(define (my-append! x y)
  (set-cdr! (my-last-pair x) y)
  x)
(define (my-last-pair x)
  (if (null? (cdr x))
    x
    (my-last-pair (cdr x))))
(check
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
  (cdr x) => '(b c d))
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
(check
  (define z (make-cycle (list 'a 'b 'c)))
  (cadddr z) => 'a)
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
(check
  (define v (list 'a 'b 'c 'd))
; v->[*|*]->[*|*]->[*|*]->[*|X]
;     |      |      |      |
;     V      V      V      V
;     a      b      c      d
  (define w (mystery v))
  v => '(a)
  w => '(d c b a))
; v->[*|X]<-[*|*]<-[*|*]<-[*|*]<-w
;     |      |      |      |
;     V      V      V      V
;     a      b      c      d
;; These box-and-pointer diagrams make it obvious that `mystery` simply changes
;; the directions of all the arrows.

;;; ex 3.15
(define (set-to-wow! x) (set-car! (car x) 'wow) x)
(check
  (define x (list 'a 'b))
  (define z1 (cons x x))
  z1 => '((a b) a b)
;; The `car` and the `cdr` of `z1` both point to `x`.
; z1->[*|*]
;      | |
;      V V
;  x->[*|*]->[*|X]
;      |      |
;      V      V
;      a      b
  (set-to-wow! z1) => '((wow b) wow b)
;; Since the `cdr` points to the same `x`, its `a` also becomes `wow`. In
;; `set-to-wow!`, it makes no difference whether we use `(car x)` or `(cdr x)`
;; as the first argument to `set-car!` because they are the same in this case.
; z1->[*|*]
;      | |
;      V V
;  x->[*|*]->[*|X]
;      |      |
;      V      V
;     wow     b
  (define z2 (cons (list 'a 'b) (list 'a 'b)))
  z2 => '((a b) a b)
;; This is a straightforward list that happens to look "the same" as `z1`.
; z2->[*|*]->[*|*]->[*|X]
;      |      |      |
;      |      +-> a  +-> b
;      V
;    [*|*]->[*|X]
;     |      |
;     +-> a  +-> b
  (set-to-wow! z2) => '((wow b) a b))
;; Since the `car` and `cdr` of `z2` point to different `(a b)` lists (there is
;; no sharing), only the first `a` changes to `wow`.
; z2->[*|*]->[*|*]->[*|X]
;      |      |      |
;      |      +-> a  +-> b
;      V
;    [*|*]--->[*|X]
;     |        |
;     +-> wow  +-> b

;;; ex 3.16
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))
;; The procedure `count-pairs` is wrong because it assumes there is no sharing.
;; All of the lists named `three-N` consist of three pairs, but `count-pairs`
;; thinks that they have `N` pairs.
(define p (cons 'a '())) ; a single pair
(define three-3 (cons 'a (cons 'a (cons 'a '())))) ; three pairs
(define three-4 (cons 'a (cons p p))) ; two pairs + p = three pairs
(define ab (cons 'a (cons 'b '()))) ; two pairs
(define three-5 (cons ab ab)) ; one pair + ab = three pairs
(define aa (cons p p)) ; one pair + p = two pairs
(define three-7 (cons aa aa)) ; one pair + aa = three pairs
(check
  (count-pairs three-3) => 3
  (count-pairs three-4) => 4
  (count-pairs three-5) => 5
  (count-pairs three-7) => 7)

;;; ex 3.17
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
(check
  (count-pairs three-3) => 3
  (count-pairs three-4) => 3
  (count-pairs three-5) => 3
  (count-pairs three-7) => 3)

;;; ex 3.18
(define (cycle? ls)
  (define (iter ls seen)
    (and (pair? ls)
         (or (memq ls seen)
             (iter (cdr ls) (cons ls seen)))))
  (bool (iter ls '())))
(check
  (cycle? (list 1 2 3)) => #f
  (cycle? (make-cycle (list 1 2 3))) => #t)

;;; ex 3.19
;; This is Floyd's cycle-finding algorithm (the tortoise and the hare).
(define (cycle? ls)
  (define (iter t h)
    (and (pair? h)
         (pair? (cdr h))
         (or (eq? t h)
             (iter (cdr t) (cddr h)))))
  (and (pair? ls)
       (iter ls (cdr ls))))
(check
  (cycle? (list 1 2 3)) => #f
  (cycle? (make-cycle (list 1 2 3))) => #t)

;;; ssec 3.3.1 (mutation is just assignment)
(define (my-cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (errorf 'dispatch "Undefined operation: ~s" m))))
  dispatch)
(define (my-car p) (p 'car))
(define (my-cdr p) (p 'cdr))
(define (my-set-car! p v) ((p 'set-car!) v) p)
(define (my-set-cdr! p v) ((p 'set-cdr!) v) p)

;;; ex 3.20
(check
  (define x (my-cons 1 2))
  (define z (my-cons x x))
  (my-set-car! (my-cdr z) 17)
  (my-car x) => 17)
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

;;; ssec 3.3.2 (representing queues)
(define front-ptr car)
(define rear-ptr cdr)
(define set-front-ptr! set-car!)
(define set-rear-ptr! set-cdr!)
(define (empty-queue? q) (null? (front-ptr q)))
(define (make-queue) (cons '() '()))
(define (front-queue q)
  (if (empty-queue? q)
    (errorf 'front-queue "Called with an empty queue: ~s" q)
    (car (front-ptr q))))
(define (insert-queue! q x)
  (let ((new-pair (cons x '())))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (else
            (set-cdr! (rear-ptr q) new-pair)
            (set-rear-ptr! q new-pair)
            q))))
(define (delete-queue! q)
  (cond ((empty-queue? q)
         (errorf 'delete-queue! "Called with an empty queue: ~s" q))
        (else (set-front-ptr! q (cdr (front-ptr q)))
              q)))

;;; ex 3.21
(check
  (define q1 (make-queue))
  (insert-queue! q1 'a) => '((a) a)
  (insert-queue! q1 'b) => '((a b) b)
  (delete-queue! q1) => '((b) b)
  (delete-queue! q1) => '(() b))
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

;;; ex 3.22
;; It's interesting how I ended up using `dispatch` like you would use the
;; `this` keyword in object-oriented languages. Another interesting point is the
;; application of procedures in `dispatch`. For procedures that take arguments
;; other than the queue itself, like for insertion, we have to return the
;; procedure that can then be applied to the argument(s). In this case, the
;; rease of the operations take no other arguments. It might be more consistent
;; to return a procedure of zero arguments -- then we would need double
;; parentheses, like `((my-queue 'front-queue))` -- but this seems a bit
;; strange. Instead, we apply the procedure right away in `dispatch` and
;; pass on the return value.
(define (make-queue-object)
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
        (error 'delete! "Called with an empty queue")
        (begin (set! front-ptr (cdr front-ptr))
               dispatch)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty?))
            ((eq? m 'front-queue)
             (if (empty?)
               (error 'front-queue "Called with an empty queue")
               (car front-ptr)))
            ((eq? m 'insert-queue!) insert!)
            ((eq? m 'delete-queue!) (delete!))
            (else (errorf 'dispatch "Undefined operation: ~s" m))))
    dispatch))

;;; ex 3.23
;; I have implemented the deqeue as a doubly-linked list. Instead of pointing to
;; the next element, the `cdr` of each item is a pair whose `car` points to the
;; previous item and whose `cdr` points to the next. We call the items nodes:
(define (make-node x prev next)
  (cons x (cons prev next)))
(define (data-node node) (car node))
(define (prev-node node) (cadr node))
(define (next-node node) (cddr node))
(define (set-prev! node prev) (set-car! (cdr node) prev))
(define (set-next! node next) (set-cdr! (cdr node) next))
;; deque representation
(define (front-ptr dq) (car dq))
(define (rear-ptr dq)  (cdr dq))
(define (set-front-ptr! dq x) (set-car! dq x))
(define (set-rear-ptr!  dq x) (set-cdr! dq x))
(define (make-deque) (cons '() '()))
;;; deque operations
(define (empty-deque? dq) (null? (front-ptr dq)))
(define (front-deque dq)
  (if (empty-deque? dq)
    (errorf 'front-deque "Called with an empty deque: ~s" dq)
    (data-node (front-ptr dq))))
(define (rear-deque dq)
  (if (empty-deque? dq)
    (errorf 'rear-deque "Called with an empty deque: ~s" dq)
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
         (errorf 'front-delete-deque! "Called with an empty deque: ~s" dq))
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
         (errorf 'rear-delete-deque "Called with an empty deque: ~s" dq))
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
    (cond
      ((not (null? node))
       (if (not first) (display ", "))
       (display (data-node node))
       (iter (next-node node) #f))))
  (display "[")
  (iter (front-ptr dq) #t)
  (display "]")
  (newline))

;;; ssec 3.3.3 (representing tables)
;; one-dimesional tables
(define (table-lookup key table)
  (let ((record (table-assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))
(define (table-assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (table-assoc key (cdr records)))))
(define (table-insert! key value table)
  (let ((record (table-assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value)
                      (cdr table)))))
  'ok)
(define (make-table) (list '*table*))
;; two-dimensional tables
(define (table-2d-lookup key-1 key-2 table)
  (let ((subtable (table-assoc key-1 (cdr table))))
    (if subtable
      (let ((record (table-assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          #f))
      #f)))
(define (table-2d-insert! key-1 key-2 value table)
  (let ((subtable (table-assoc key-1 (cdr table))))
    (if subtable
      (let ((record (table-assoc key-2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr! subtable
                    (cons (cons key-2 value)
                          (cdr subtable)))))
      (set-cdr! table
                (cons (list key-1 (cons key-2 value))
                      (cdr table)))))
  'ok)
;; procedural implementation
(define (make-table-object)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (table-assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (table-assoc key-2 (cdr subtable))))
            (if record (cdr record) #f))
          #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (table-assoc key-1 (cdr table))))
        (if subtable
          (let ((record (table-assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! table
                    (cons (list key-1 (cons key-2 value))
                          (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (errorf 'dispatch "Unknown operation: ~s" m))))
    dispatch))
(define operation-table (make-table-object))
(define get-operation (operation-table 'lookup-proc))
(define put-operation (operation-table 'insert-proc!))

;;; ex 3.24
;; Other than the argument `same-key?` and the interal procedure `assoc`, this
;; is the same code as above.
(define (make-table-custom-eq same-key?)
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
                          (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (errorf 'dispatch "Unknown operation: ~s" m))))
    dispatch))

;;; ex 3.25
;; The generalized n-dimensional table procedures are implemented recursively.
;; On each recursive call, a key is stripped off the keys list and a deeper
;; subtable is entered. The procedures `lookup` and `insert!` have an argument
;; named `tor`, which means "table or record." A table is treated as a record
;; whose key is its name and whose value is a list of records. The base case for
;; the null list of keys is not necessarily useful, but it makes sense. The
;; dimensions of the table do not need to be consistent, but be careful: If you
;; have a value stored at `'(a b)` and then insert something at `'(a b c)`, that
;; value will be overwitten with a fresh subtable for `c`.
(scope
  (define record-key car)
  (define record-val cdr)
  (define set-key! set-car!)
  (define set-val! set-cdr!)
  (define make-record cons)
  (define table-name record-key)
  (define table-records record-val)
  (define set-name! set-key!)
  (define set-records! set-val!)
  (define make-table make-record)
  (define (make-empty-table) (make-table '*table* '()))
  (define (table-assoc key records)
    (cond ((null? records) #f)
          ((equal? key (record-key (car records)))
          (car records))
          (else (table-assoc key (cdr records)))))
  (define (table-lookup keys tor)
    (let ((val (record-val tor)))
      (if (null? keys)
        val
        (if (list? val) ; tor is a table, val is a list of records
          (let ((subtor (table-assoc (car keys) val)))
            (if subtor
              (table-lookup (cdr keys) subtor)
              #f))
          #f))))
  (define (table-insert! keys value tor)
    (define (iter keys tor)
      (if (null? keys)
        (set-val! tor value)
        (let ((records (table-records tor)))
          (if (list? records)
            (let ((subtor (table-assoc (car keys) records)))
              (if subtor
                (iter (cdr keys) subtor)
                (let ((new-subtor (make-table (car keys) '())))
                  (set-records! tor (cons new-subtor records))
                  (iter (cdr keys) new-subtor))))
            (begin (set-val! tor '())
                  (iter keys tor))))))
    (iter keys tor)))

;;; ex 3.26
;; In our current implementation, a table is the pair `(cons n rs)` where `n` is
;; the name of the table and `rs` is a list of records, each record being a key
;; consed to a value. With a binary tree implementation, instead of a list of
;; records, we could have three things: a record (consisting of a key and a
;; value), a left branch, and a right branch. Records down the left branch have
;; smaller keys, and records down the right branch have larger keys. The keys
;; can be anything, but the procdure `compare` must be defined on them such that
;; `(compare k1 k2)` returns either `'=`, `'<`, or `'>`. We wouldn't need to
;; have `'*table*` in this setup because we don't need to maintain an identity
;; for a changing "first record." We can always navigate down the tree to insert
;; a new record.
(scope
  (define record-key car)
  (define record-val cdr)
  (define set-key! set-car!)
  (define set-val! set-cdr!)
  (define make-record cons)
  (define table-record car)
  (define table-left cadr)
  (define table-right cddr)
  (define set-record! set-car!)
  (define (set-left!  table left)  (set-car! (cdr table) left))
  (define (set-right! table right) (set-cdr! (cdr table) right))
  (define (make-tree-table record left right)
    (cons record (cons left right)))
  (define (make-empty-table)
    (make-tree-table '() '() '()))
  (define (make-singleton key value)
    (make-table (make-record key value) '() '()))
  (define (empty-table? table)
    (null? (table-record table)))
  (define (table-lookup key table)
    (define (iter table)
      (if (or (null? table) (empty-table? table))
        #f
        (let* ((record (table-record table))
              (order (compare key (record-key record))))
          (cond ((eq? order '=) (record-val record))
                ((eq? order '<) (iter (table-left table)))
                ((eq? order '>) (iter (table-right table)))))))
    (iter table))
  (define (table-insert! key value table)
    (define (iter table)
      (cond
        ((null? table)
        (errorf 'table-insert!
          "Cannot insert (~s, ~s) into null table ~s" key value table))
        ((empty-table? table)
        (set-record! table (make-record key value)))
        (else
          (let* ((record (table-record table))
                (order (compare key (record-key record))))
            (cond ((eq? order '=)
                  (set-val! record value))
                  ((eq? order '<)
                  (let ((subtable (table-left table)))
                    (if (null? subtable)
                      (set-left! table (make-singleton key value))
                      (iter subtable))))
                  ((eq? order '>)
                  (let ((subtable (table-right table)))
                    (if (null? subtable)
                      (set-right! table (make-singleton key value))
                      (iter subtable)))))))))
    (iter table)))

;;; ex 3.27
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((cached (table-lookup x table)))
        (or cached
            (let ((result (f x)))
              (table-insert! x result table)
              result))))))
(define memo-fib
  (memoize
    (lambda (n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (memo-fib (- n 1))
                     (memo-fib (- n 2))))))))
;; For the environment diagram, see `whiteboard/exercise-3.27.jpg`.
;; The memoized procedure `memo-fib` computes the nth Fibonacci number in a
;; number of steps proportional to `n` because it simply takes the sum of `n`
;; numbers. When we evaluate `(memo-fib n)`, a tree-recursive process is
;; generated and the tree descends until it reaches 0 and 1. These are the base
;; cases of the recursive Fibonacci implementation. The results for these inputs
;; are placed in the table, and then `(memo-fib 2)` requires only one step, the
;; addition of 0 and 1, because the values are taken from the cache table. The
;; pattern continues: `(memo-fib 3)` recurses on 1 and 2, and both return early
;; because they are in the table. In general, we descend to the bottom of the
;; tree once and then ascent it, never again going down and reaching duplicate
;; leaves. This is twice `n` steps, so it grows as O(n). If we had defined
;; `memo-fib` as `(memoize fib)`, this would not work because recursive calls
;; would use `fib`, not `memo-fib`, and so we would still have an exponential
;; number of steps. However, this aspect of the memoization would still work: if
;; you evaluated `(memo-fib 42)` twice, the second time would take only the step
;; of looking up a value in the table.

;;; ssec 3.3.4 (digital circuit simulation)
(define (half-adder a b sum carry)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b carry)
    (inverter carry e)
    (and-gate d e sum)
    'ok))
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder a b s c1)
    (half-adder c-in s sum c2)
    (or-gate c1 c2 c-out)
    'ok))
(define (inverter input output)
  (add-action!
    input
    (lambda ()
      (let ((new-signal (logical-not (get-signal input))))
        (after-delay
          inverter-delay
          (lambda ()
            (set-signal! output new-signal))))))
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))
(define (and-gate a b out)
  (define (action)
    (let ((new-signal (logical-and (get-signal a) (get-signal b))))
      (after-delay
        and-gate-delay
        (lambda ()
          (set-signal! out new-signal)))))
  (add-action! a action)
  (add-action! b action)
  'ok)
(define (logical-and a b)
  (if (and (= a 1) (= b 1)) 1 0))

;;; ex 3.28
(define (or-gate a b out)
  (define (action)
    (let ((new-signal (logical-or (get-signal a) (get-signal b))))
      (after-delay
        or-gate-delay
        (lambda ()
          (set-signal! out new-signal)))))
  (add-action! a action)
  (add-action! b action)
  'ok)
(define (logical-or a b)
  (if (or (= a 1) (= b 1)) 1 0))

;;; ex 3.29
(scope
  (define (or-gate a b out)
    (let ((na (make-wire))
          (nb (make-wire))
          (c (make-wire)))
      (inverter a na)
      (inverter b nb)
      (and-gate na nb c)
      (inverter c out)
      'ok))
  (define (or-gate-delay)
    (+ and-gate-delay
      (* 2 inverter-delay))))

;;; ex 3.30
;; The ripple carry adder circuit adds binary numbers in little endian order.
;; The first wire in `as` represents the least significant bit of the number.
(define (ripple-carry-adder as bs ss carry)
  (define (iter as bs c-in ss)
    (if (null? (cdr as))
      (full-adder (car as) (car bs) c-in (car ss) carry)
      (let ((c (make-wire)))
        (full-adder (car as) (car bs) c-in (car ss) c)
        (iter (cdr as) (cdr bs) c (cdr ss)))))
  (define (fail msg)
    (errorf 'ripple-carry-adder
      "~s: as=~s bs=~s ss=~s carry=~s" msg as bs ss carry))
  (cond ((not (= (length as) (length bs) (length ss)))
         (fail "Number size mismatch"))
        ((null? as)
         (fail "Cannot add zero bits"))
        (else (let ((c-in (make-wire)))
                (set-signal! c-in 0)
                (iter as bs c-in ss)
                'ok))))
(define (half-adder-delay)
  (+ (max or-gate-delay
          (+ and-gate-delay inverter-delay))
     or-gate-delay))
(define (full-adder-delay)
  (+ (* 2 (half-adder-delay))
     or-gate-delay))
(define (ripple-carry-adder-delay n)
  (* n (full-adder-delay)))

;;; ssec 3.3.4 (representing wires)
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-signal! s)
      (if (not (= signal-value s))
        (begin (set! signal-value s)
               (call-each action-procedures))
        'done))
    (define (add-action! proc)
      (set! action-procedures
        (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'add-action!) add-action!)
            (else (errorf 'dispatch "Unknown operation: ~s" m))))
    dispatch))
(define (call-each procs)
  (if (null? procs)
    'done
    (begin ((car procs))
           (call-each (cdr procs)))))
(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire s) ((wire 'set-signal!) s))
(define (add-action! wire a) ((wire 'add-action!) a))
(define (after-delay delay-time action)
  (add-to-agenda! (+ delay-time (simulation-time the-agenda))
                  action
                  the-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

;;; ssec 3.3.4 (implementing the agenda)
(define segment-time car)
(define segment-queue cdr)
(define make-time-segment cons)
(define (make-agenda) (list 0))
(define (simulation-time agenda) (car agenda))
(define (set-simulation-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda)  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
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
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error 'first-agenda-item "Agenda is empty")
    (let ((first-seg (first-segment agenda)))
      (set-simulation-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

;;; ssec 3.3.4 (sample simulation)
;; This comes before the previous section, but we can't evaluate it in the book
;; order because the agenda was not implemented yet.
(define (probe name wire)
  (add-action!
    wire
    (lambda ()
      (newline)
      (display name)
      (display " ")
      (display (simulation-time the-agenda))
      (display " New-value = ")
      (display (get-signal wire)))))
(define the-agenda)
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
(check
  (set! the-agenda (make-agenda))
  (define input-1 (make-wire))
  (define input-2 (make-wire))
  (define sum (make-wire))
  (define carry (make-wire))
  (capture-output (probe 'sum sum))
  => "\nsum 0 New-value = 0"
  (capture-output (probe 'carry carry))
  => "\ncarry 0 New-value = 0"
  (half-adder input-1 input-2 sum carry) => 'ok
  (set-signal! input-1 1) => 'done
  (capture-output (propagate))
  => "\nsum 8 New-value = 1"
  (set-signal! input-2 1) => 'done
  (capture-output (propagate))
  => "\ncarry 11 New-value = 1\nsum 16 New-value = 0")

;;; ex 3.31
;; We can change `make-wire` so that it does not call actions immediately when
;; they are added by wrapping the action in a procedure that does nothing the
;; first time it is called:
(check
  (set! the-agenda (make-agenda))
  (define old-make-wire make-wire)
  (define (make-wire)
    (let ((wire (old-make-wire)))
      (lambda (m)
        (if (not (eq? m 'add-action!))
          (wire m)
          (lambda (action)
            ((wire 'add-action!)
              (let ((first #t))
                (lambda ()
                  (if first
                    (begin (set! first #f) 'done)
                    (action))))))))))
;; We have to call the action right after registering it because the wire could
;; either have the signal 0 or 1 when we add the action. Suppose we are just
;; talking about an inverter: `(inverter a b)`. This adds an action to `a` such
;; that whenever the signal of `a` changes, we execute `(set-signal! b s)`
;; where `s` is the logical negation of the new signal of `a`. Now, suppose we
;; have `a` and `b` defined like this:
  (define a (make-wire))
  (define b (make-wire))
;; We can check their signals:
  (get-signal a) => 0
  (get-signal b) => 0
;; Now we add the inverter:
  (inverter a b) => 'ok
;; Since `add-action!` is implemented so that it doesn't call the action
;; procedure right away, we've now added an action to `a` but it has not been
;; executed. Therefore the signals haven't changed:
  (get-signal a) => 0
  (get-signal b) => 0
;; This is an incorrect state: if `a` is 0, `b` should be `1`. But we wuold
;; have to flip `a` on and back off for this to fix itself. We must always
;; execute the action right after adding it to ensure that the circuit is
;; always in a stable state. Otherwise, the initial values don't get
;; propagated. A more realistic model would execute the actions continuously;
;; we only execute them at the beginning and on changes because it is a waste
;; to do more, since function boxes have referential transparency (the same
;; input will always produce the same output).
;;
;; Let's trace through the previous example without calling actions when they
;; are added. Nothing is printed when we call `probe` because the printing
;; action is not called immediately. Nothing is printed when we propagate
;; setting `input-1` to 1 either: it flows through the OR gate, but not through
;; the AND gate because the latter does not know its other input is 1 (that
;; would have required propagating the 0 from `input-2` through the other AND
;; gate and the inverter). Finally, when we set `input-2` to 1 and propagate,
;; it flows through the circuit leaving `sum` at 0 (still no printing), but
;; changing `carry` to 1 (which becomes the only thing printed).
  (define input-1 (make-wire))
  (define input-2 (make-wire))
  (define sum (make-wire))
  (define carry (make-wire))
  (capture-output (probe 'sum sum))
  => ""
  (capture-output (probe 'carry carry))
  => ""
  (half-adder input-1 input-2 sum carry) => 'ok
  (set-signal! input-1 1) => 'done
  (capture-output (propagate))
  => ""
  (set-signal! input-2 1) => 'done
  (capture-output (propagate))
  => "\ncarry 11 New-value = 1")

;;; ex 3.32
;; The FIFO order for the queue of procedures for each segment must be used
;; because this causes the actions to be executed in the same order as they
;; were triggered and added to the agenda. If actions A1, A2, and A3 occur in
;; that order, they will be inserted into the queue in that order, and they
;; will also pop out in that order. Executing the procedures in reverse order
;; leads to different, incorrect behaviour. Consider the case of an and-gate
;; whose inputs change from 0, 1 to 1, 0:
(check
  (set! the-agenda (make-agenda))
  (define a (make-wire))
  (define b (make-wire))
  (define c (make-wire))
  (set-signal! a 0) => 'done
  (set-signal! b 1) => 'done
  (and-gate a b c) => 'ok
  (capture-output (probe 'c c))
  => "\nc 0 New-value = 0"
  (capture-output (propagate))
  => ""
  (set-signal! a 1) => 'done
  (set-signal! b 0) => 'done
  (capture-output (propagate))
  => "\nc 6 New-value = 1\nc 6 New-value = 0")
;; The value of `c` goes to 1, but settles on 0 once all actions are processed.
;; If we use a stack (FILO) rather than a queue (FIFO) for the actions, there
;; will be a mismatch between the order of execution and the calculation of wire
;; values when adding actions. Recall the definition of and-gate:
(define (and-gate a b out)
  (define (action)
    ;; This logical-and gets evaluated right away.
    (let ((new-signal (logical-and (get-signal a) (get-signal b))))
      (after-delay
        and-gate-delay
        ;; Only this part goes into the agenda. `new-signal` is not calculated
        ;; on the spot; it is from the let-binding.
        (lambda ()
          (set-signal! out new-signal)))))
  (add-action! a action)
  (add-action! b action)
  'ok)
;; When actions have FILO behaviour, `(set-signal! a 1)` will create an action
;; to set `c` to 1 (since `a` and `b` are 1). Then `(set-signal! b 0)` will
;; create an action to set `c` to 0, the correct final value. But the actions
;; exeucte in reverse order, so `c` ends up incorrectly at 1. We can test this:
(fluid-let
  ((empty-queue? (comp null? car))
   (make-queue (lambda () (cons '() '())))
   (front-queue caar)
   (insert-queue!
    (lambda (q x)
      (set-car! q (cons x (car q)))))
   (delete-queue!
    (lambda (q)
      (set-car! q (cdar q)))))
  (check
    (set! the-agenda (make-agenda))
    (define a (make-wire))
    (define b (make-wire))
    (define c (make-wire))
    (set-signal! a 0) => 'done
    (set-signal! b 1) => 'done
    (and-gate a b c) => 'ok
    (capture-output (probe 'c c))
    => "\nc 0 New-value = 0"
    (capture-output (propagate))
    => ""
    (set-signal! a 1) => 'done
    (set-signal! b 0) => 'done
    (capture-output (propagate))
    => "\nc 6 New-value = 1"))

;; *** OLD SPOT I HAD GOTTEN TO

;;; ssec 3.3.5 (using the constraint system)
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
    (constant 32 y)
    'ok))
(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F) ; => ok
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user)
;; => Probe: Celsius temp = 25
;;    Probe: Fahrenheit temp = 77
;;    done
(set-value! F 212 'user)
;; => Error! Contradiction (77 212)
(forget-value! C 'user)
;; => Probe: Celsius temp = ?
;;    Probe: Fahrenheit temp = ?
;;    done
(set-value! F 212 'user)
;; => Probe: Fahrenheit temp = 212
;;    Probe: Celsius temp = 100
;;    done

;;; ssec 3.3.5 (implementing the constraint system)
(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)
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
    (cond ((eq? request 'I-have-a-value)  (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
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
    (cond ((eq? request 'I-have-a-value)  (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))
  (connect x me)
  (connect y me)
  (connect product me)
  me)
(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (print-probe (get-value connector)))
          ((eq? request 'I-lost-my-value)
           (print-probe "?"))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;;; ssec 3.3.5 (representing connectors)
(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints '()))
    (define (set-value! new-val setter)
      (cond ((not (has-value? me))
             (set! value new-val)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value new-val))
             (error "Contradiction" (list value new-val)))
            (else 'ignored)))
    (define (forget-value! retractor)
      (if (eq? retractor informant)
        (begin (set! informant #f)
               (for-each-except retractor
                                inform-about-no-value
                                constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints
          (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant #t #f))
            ((eq? request 'get-value) value)
            ((eq? request 'set-value!) set-value!)
            ((eq? request 'forget-value!) forget-value!)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR" request))))
    me))
(define (for-each-except exception proc ls)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (proc (car items))
                (loop (cdr items)))))
  (loop ls))
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'get-value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget-value!) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;;; ex 3.33
(define (averager a b c)
  (let ((u (make-connector))
        (w (make-connector)))
    (adder a b u)
    (multiplier c w u)
    (constant 2 w)
    'ok))

;;; ex 3.34
(define (bad-squarer a b)
  (multiplier a a b))
;; At first glance, Louis Reasoner's constraint seems okay:
(define a (make-connector))
(define b (make-connector))
(probe 'b b)
(bad-squarer a b)
(set-value! a 5 'user)
;; => Probe: b = 25
;;    done
;; However, going the other way doesn't work. If we set `b`, the constraint
;; does not figure out `a`. It would be remarkable if it did a square root
;; algorithm without us ever writing code for that! The problem is that
;; multiplier is too general. Louis Reasoner's constraint does not take
;; advantage of the extra information specific to multiplications of a number to
;; itself. It doesn't know that the multiplicand and the multiplier are the
;; same connector.

;;; ex 3.35
(define (square x) (* x x))
(define (squarer n n-squared)
  (define (process-new-value)
    (cond ((has-value? n)
           (set-value! n-squared (square (get-value n)) me))
          ((has-value? n-squared)
           (if (< (get-value n-squared) 0)
             (error "Square less than 0: SQUARER" (get-value n-squared))
             (set-value! n (sqrt (get-value n-squared)) me)))))
  (define (process-forget-value!)
    (forget-value! n-squared)
    (forget-value! n)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect n me)
  (connect n-squared me)
  me)
(define a (make-connector))
(define b (make-connector))
(probe 'a a)
(squarer a b)
(set-value! b 25 'user)
;; => Probe: a = 5
;;    done

;;; ex 3.36
;; For the environment diagram, see `whiteboard/exercise-3.36.jpg`.

;;; ex 3.37
(define (celsius->fahrenheit x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))
(define (cv k)
  (let ((c (make-connector)))
    (set-value! c k cv)
    c))
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

;;;;; Section 3.4: Concurrency: time is of the essence

;;; ex 3.38
(define balance 100)
;; These three assignments are executed concurrently:
(set! balance (+ balance 10))
(set! balance (- balance 20))
(set! balance (- balance (/ balance 2)))
;; (a) Possible values:
(or (= balance 35)
    (= balance 40)
    (= balance 45)
    (= balance 50))
;; (b) These other values are equivalent to what you can get when you leave out
;; one or more of the assignments, since the new value is overwritten before
;; being read. We could have 110, 90, 80, 55, ....

;;; ssec 3.4.2 (serializers)
(define x 10)
(parallel-execute
  (lambda () (set! x (* x x)))
  (lambda () (set! x (+ x 1))))
;; There are five possible final values:
(or (= x 101) (= x 121) (= x 110) (= x 11) (= x 100))
;; Using a serializer, we narrow it down:
(define x 10)
(define s (make-serializer))
(paralell-execute
  (s (lambda () (set! x (* x x))))
  (s (lambda () (set! x (+ x 1)))))
;; There are just two possible values:
(or (= x 101) (= x 121))

;;; ex 3.39
(define x 10)
(define s (make-serializer))
(parallel-execute
  (lambda () (set! x ((s (lambda () (* x x))))))
  (s (lambda () (set! x (+ x 1)))))
;; Three of the five values are still ossible:
(or (= x 101)  ; squared, then incremented
    (= x 121)  ; incremented, then squared
    (= x 100)) ; incremented between squarer read and write

;;; ex 3.40
(define x 10)
(parallel-execute
  (lambda () (set! x (* x x)))
;; process S:  3        1 2
  (lambda () (set! x (* x x x))))
;; process C:  7        4 5 6
;; There are seven relevant steps taken above: three in S, four in C. Steps 1,
;; 2, 4, 5, and 6 are reads; steps 3 and 7 are writes. This conccurent
;; execution is not serialized at all, so the steps can interleave.
(define steps-s-read '(1 2))
(define steps-s-write '(3))
(define steps-c-read '(4 5 6))
(define steps-c-write '(7))
(define steps-s (append steps-s-read steps-s-write))
(define steps-c (append steps-c-read steps-c-write))
(define steps (append steps-s steps-c))
;; We will calculate all possible orderings using permutations and a filter.
(define (permutations xs)
  (if (null? xs)
    '(())
    (flat-map
      (lambda (p)
        (map (lambda (q) (cons p q))
             (permutations (remove p xs))))
      xs)))
(define (good-interleave? p)
  (define (iter latest-s latest-c p)
    (or (null? p)
        (and (memv (car p) steps-s)
             (> (car p) latest-s)
             (iter (car p) latest-c (cdr p)))
        (and (memv (car p) steps-c)
             (> (car p) latest-c)
             (iter latest-s (car p) (cdr p)))))
  (iter 0 0 p))
(define possible-orders
  (filter good-interleave?
          (permutations steps)))
(length possible-orders) ; => 35
;; There are 35 possibilities. Some may produce the same value, so there are 35
;; or less possible final values for `x`. We will automate this part as well:
(define (execute-steps steps x)
  (define (iter s-reads c-reads steps x)
    (cond ((null? steps) x)
          ((memv (car steps) steps-s-read)
           (iter (cons x s-reads) c-reads (cdr steps) x))
          ((memv (car steps) steps-c-read)
           (iter s-reads (cons x c-reads) (cdr steps) x))
          ((memv (car steps) steps-s-write)
           (iter s-reads c-reads (cdr steps) (apply * s-reads)))
          ((memv (car steps) steps-c-write)
           (iter s-reads c-reads (cdr steps) (apply * c-reads)))
          (else (error "Unknown step number: EXECUTE-STEPS" (car steps)))))
  (iter '() '() steps x))
;; Now, we can see the values that produced by all 35 possibilities:
(define final-values
  (map (lambda (ss) (execute-steps ss 10))
     possible-orders))
final-values
;; => (1000000 100000 10000 1000 100 100000 10000
;;     1000 100 10000 1000 100 1000 100
;;     10000 100000 10000 1000 100 10000 1000
;;     100 1000 100 10000 10000 1000 100
;;     1000 100 10000 1000 100 10000 1000000)
;; Quite a range of different values! But there are some duplicates. Let's see
;; how many of each value there is.
(define (group xs)
  (let ((occurence-table (make-table)))
    (define (iter xs)
      (if (null? xs)
        occurence-table
        (let ((n (table-lookup (car xs) occurence-table)))
          (insert! (car xs)
                   (if n (inc n) 1)
                   occurence-table)
          (iter (cdr xs)))))
    (iter xs)))
(group final-values)
;; => (*table* (100     . 10)
;;             (1000    . 10)
;;             (10000   . 10)
;;             (100000  .  3)
;;             (1000000 .  2))
;; The smaller values are more frequent. The largest value, 1000000, is the
;; correct value, and we can check that:
(execute-steps '(1 2 3 4 5 6 7) 10) ; => 1000000
;; This is squaring and then cubing. Notice there are 2 occurences of this
;; value. The only other way is to cube and then square:
(execute-steps '(4 5 6 7 1 2 3) 10) ; => 1000000
;; If we serialize the two procedures, this is the only value we get.

;;; ex 3.41
;; Ben Bitdiddle is dead wrong. It would be unnecessary to serialize access to
;; the bank balance because it would make no difference. If we serialize it,
;; then the value will be read either before or after (in sequence) it is
;; written, assuming someone is withdrawing or depositing concurrently.
;; However, if we don't serialize it, we still get one value or the other.
;; There is nothing that can be interleaved because reading the balance takes
;; only one step. Any problems below this level are handled in the hardware.
;; Therefore it is useless to serialize access to the bank balance. Now, you
;; might think that it matters when we withdraw or deposit an amount which is a
;; function of the present balance. Consider this:
(define account (make-account 100))
(parallel-execute
  (lambda () ((account 'deposit) 10))
  (lambda () ((account 'withdraw) (account 'balance))))
;; We should think that the balance ends up either 0 or 10. If I have 10 in my
;; pocket at the beginning, then the total 110 should be conserved. Either I
;; put 10 in first, and then withdraw the 110, leaving 0 in the bank and 110 in
;; my pocket; or I withdraw the 100 first, and then deposit 10, leaving 10 in
;; the bank and 100 in my pocket. We need serialization for this, because there
;; are other possibilities due to the fact that the deposit can happen in
;; between reading of the balance and the withdrawal. (Note that the deposit
;; cannot interleave with the withdrawal because they are serialized.) However,
;; Ben Bitdiddle is still wrong, because serializing access to the balance
;; wouldn't change a think. It would only seal up the balance access, still
;; leaving a hole between that and the withdrawal. We would have to create a
;; procedure that does both, and then serialize that. In any case, Ben is wrong.

;;; ex 3.42
;; This is a safe change to make. Each bank account still has one serializer and
;; the deposit and withdraw procedures returned from the dispatcher are always
;; protected by it. It makes no difference in what concurrency is allowed. If it
;; did, then the specification of `make-serializer` must be incorrect.

;;; ex 3.43
;; The balances in the three accounts start out as $10, $20, and $30. The
;; exchange of balances A and B works by calculating D = A - B, and then
;; withdrawing to get A' = A - D and depositing to get B' = B + D. Substituting
;; for D, we have A' = A - (A - B) = B, and B' = B + (A - B) = A. The new value
;; of A is A' = B, and the new value of B is B' = A. The initial set of balances
;; {A,B} and the final set of balances {B,A} are equal. This proves that the
;; account balances remain $10, $20, and $30 in some order after an exchange.
;; With `serialized-exchange`, the exchanging is serialized on both accounts.
;; This means that if A and B are beight exchanged, no other exchange involving
;; either A or B can occur until it is finished. If one exchange preserves the
;; set of balances {10,20,30}, then so will another, and another: a sequence of
;; exchanges, or serialized concurrent exchanges, will preserve {10,20,30}.
;; Using the first definition of the account-exchange program, where only the
;; individual deposits and withdrawals are serialized (and only on one account),
;; the {10,20,30} set will not be preserved. This is because the four steps of
;; the exchange can interleave with the four steps of another. Let us refer to
;; the steps of concurrent processes P and Q by P1, Q1, P2, Q2, etc.:
(define (exchange acc1 acc2)
  (let ((diff (- (acc1 'balance)    ; (1)
                 (acc2 'balance)))) ; (2)
    ((acc1 'withdraw) diff)         ; (3)
    ((acc2 'deposit) diff)))        ; (4)
;; Suppose the three accounts A, B, and C begin with $10, $20, and $30
;; respectively. Process P exchanges A and B while concurrent process Q
;; exchanges A and C. We interleave the steps like so: P1, Q1, Q2, Q3, Q4, P2,
;; P3, P4. Now, P finds the balance of A to be $10. Then Q carries out all its
;; steps: A and C are exchanged, and P's read operation does not affect this.
;; Therefore A, B, and C now have balances $30, $20, and $10 respectively. Now
;; process P carries out its three remaining steps. It finds B to have $20. It
;; calculates the difference `(- 10 20)`, which is -10. It withdraws this from
;; A, leaving A with a balance of 30 - (-10), or 40. It deposits this in B,
;; leaving B with a balance of 20 + (-10) = 10. Now the balances of A, B, and C
;; are $40, $10, and $10 respectively. This is {10,20,30}. This proves that the
;; balances {10,20,30} are not preserved with the original implementation. Now,
;; although it violates this condition, it does preserve the sum of the
;; balances, $60. This is the case in our example: 10 + 20 + 30 = 40 + 10 + 10.
;; It is true in general because, no matter how wrong the value of `diff` is, it
;; is calculated once only. It is then withdrawn from one account and deposited
;; into another, and these two operations are serialized. The sum of the
;; balances is preserved, because A' + B' = B + A = A + B. No interleaving of
;; the four exchanging steps can alter this. It matters only that the individual
;; withdrawals and deposits are serialized. This proves that the original
;; implementation preserves the sum of the balances. Finally, if we used the
;; original implementation but changed `make-account` so that it did no
;; serializing, the sum would not be preserved. This is because the steps
;; of the deposits and withdrawals of concurrent processes will interleave, and
;; we have already seen that this does not ensure that the total amount of money
;; is preserved. I will not bother detailing an example for this case.

;;; ex 3.44
(define (transfer from-acc to-acc amount)
  ((from-acc 'withdraw) amount) ; (1)
  ((to-acc 'deposit) amount))   ; (2)
;; Ben Bitdiddle is correct. This procedure will behave correctly, even with
;; multiple conccurent transfers involving the same accounts. Suppose we
;; transfer $10 between A and B (process P), and concurrently transfer $20
;; between A and C (process Q). P1 and Q1 will be in the same serialization set,
;; because they both withdraw from A. P2 and Q2 will neither be in that set nor
;; in each other's set. One deposits to B, and the other deposits to C. They
;; cannot interfere with each other. We can safely ignore P2 and Q2 -- all we
;; care about is that they will happen at some time. That leaves us with P1 and
;; Q1. These are serialized, so we either have P1 followed by Q1, or the other
;; way around. We either withdraw $10 from A and then $20 from A, or we do it in
;; the opposite order. In either case, A loses $30. B is guaranteed to gain $10,
;; and C will likewise gain $20. Correctness is ensured. Louis Reasoner is
;; wrong; we do not need a more sophisticated method. The essential difference
;; between the transfer problem and the exchange problem is that the exchange
;; amount depends on the current balances, and so it must include steps that
;; read the balance, thereby introducing a hole into which a concurrent step can
;; be interleaved, unless the whole exchange is serialized. The `transfer`
;; procedure takes `amount` as an argument, and there is no way this can be
;; messed up. The serialization of deposits and withdrawals guarantees that the
;; deposit and withdrawal of `amount` will behave correctly. It should be noted
;; that if `(from-account 'balance)` is passed as the `amount`, or some
;; similar expression involving a reading of the balance, then this argument is
;; invalid and we would need serialization as in the exchange example. I have
;; been assuming that `amount` is a constant.

;;; ex 3.45
;; Louis Reasoner is wrong. The problem with automatically serializing all
;; deposits and withdrawals is that, when we create our own multi-step
;; operations such as the exchange or the transfer, and we serialize them, we
;; end up with nested serialization. When `serialized-exchange` is called, it
;; will never return. It will be stuck forever, because as soon as it tries
;; withdraw from the first account, the system will not be able to proceed
;; because the account is already locked. It will try to wait for the exchanging
;; procedure to finish before it makes the withdrawal, which is impossible.

;;; ssec 3.4.2 (implementing serializers)
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (lambda args
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val)))))
(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell #f))

(define (test-and-set! cell)
  (if (car cell)
    #t
    (begin (set-car! cell #t)
           #f)))

;;; ex 3.46
;; Suppose we use the above implementation of `test-and-set!` and execute this:
(let ((x 100)
      (s (make-serializer)))
  (parallel-execute
    (s (lambda () (set! x (+ 100 x)))) ; process P
    (s (lambda () (set! x (/ x 2)))))) ; process Q
;; Properly serialized, we would expect only two possibilities:
(or (= x 100)  ; add 100, then divide by 2
    (= x 150)) ; divide by 2, then add 100
;; Since `test-and-set!` is not atomic, the mutex will not work. Processes P and
;; Q will execute in parallel. Here is a representation of the serialized
;; procedure that `s` creates for process P:
(lambda ()
  (mutex 'acquire)
  (let ((val (set! x (+ 100 x)))) ; (*)
    (mutex 'release)
    val))
;; We expect the line marked by (*) to be atomic. That is, since the mutex is
;; locked, Q should not be able to acquire the mutex and do anything during this
;; time. This is false, because of the way we implemented `test-and-set!`. When
;; `(mutex 'acquire)` is executed, `test-and-set!` is called on the cell:
(define (test-and-set! cell)
  (if (car cell)              ; (1)
    #t
    (begin (set-car! cell #t) ; (2)
           #f)))
;; Each process performs two operations when they test and set. We have P1, P2,
;; Q1, and Q2. Since the procedure is not atomic, these can be interleaved.
;; Suppose both processes try to acquire the mutex at the same time, and we get
;; P1, Q1, Q2, P2. Both P1 and Q1 find the contents of the cell to be false.
;; Process Q sets it to `#t` in Q2, locking the mutex, and then P2 locks the
;; already-locked mutex. Both processes think they have the lock. Now everything
;; will happen in parallel and it is as if we hadn't used a serializer. We could
;; end up with a final value of 50 for `x`, for example.

;;; ex 3.47
;; in terms of mutexes
(define (make-semaphore n)
  (let ((mutex (make-mutex))
        (users 0))
    (lambda (m)
      (cond ((eq? m 'acquire)
             (let ((u (inc users)))
              (if (>= u n)
                (mutex 'acquire))
              (set! users u)))
            ((eq? m 1) users)
            ((eq? m 'release)
             (if (= users n)
               (mutex 'release))
             (set! users (dec users)))))))
;; in terms of atomic `test-and-set!` operations
(define (make-semaphore n)
  (let ((users 0)
        (cell (list #f)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (let ((u (inc users)))
               (if (>= u n)
                 (if (test-and-set! cell)
                   (the-semaphore 'acquire)))
               (set! users u)))
            ((eq? m 'release)
             (if (= users n)
               (clear! cell))
             (set! users (dec users)))))
    the-semaphore))

;;; ex 3.48
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
  (set! *uuid* (inc *uuid*))
  *uuid*)
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((id (gen-uuid))
        (s (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) s)
            ((eq? m 'identifier) id)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))
(define (exchange a1 a2)
  (let ((diff (- (a1 'balance) (a2 'balance))))
    ((a1 'withdraw) diff)
    ((a2 'deposit) diff)))
(define (serialized-exchange a1 a2)
  (let ((s1 (a1 'serializer))
        (s2 (a2 'serializer)))
    ((if (< (a1 'identifier) (a2 'identifier))
       (s1 (s2 exchange))
       (s2 (s1 exchange)))
     a1
     a2)))

;;; ex 3.49
;; The deadlock avoidance mechanism used in 3.48 would not work with
;; `(contrived-exchange acc)`, which exchanges the balance of `acc` with that of
;; the account whose balance is closest to the balance of `acc`. We must either
;; always lock `acc` first (without the ordering mechanism of 3.48, allowing
;; deadlocks), or we must lock after accessing `acc`, creating a hole into which
;; other operations can be interleaved.

;;;;; Section 3.5: Streams

;;; ssec 3.5.1 (streams are delayed lists)
(define the-empty-stream '())
(define stream-null? null?)
(define (stream-ref s n)
  (if (zero? n)
    (stream-car s)
    (stream-ref (stream-cdr s) (dec n))))
(define (stream-map f s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (f (stream-car s))
                 (stream-map f (stream-cdr s)))))
(define (stream-for-each f s)
  (if (stream-null? s)
    'done
    (begin (f (stream-car s))
           (stream-for-each f (stream-cdr s)))))
(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (display x) (newline))
(define (stream-car s) (car s ))
(define (stream-cdr s) (force (cdr s)))
(define-syntax cons-stream
  (syntax-rules ()
    ((_ x y) (cons x (delay y)))))

;;; ssec 3.5.1 (implementing promises)
(define (memo-proc proc)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
               (set! already-run? #t)
               result)
        result))))
(define-syntax delay
  (syntax-rules ()
    ((_ e) (memo-proc (lambda () e)))))
(define (force p) (p))

;;; ex 3.50
(define (stream-map proc . ss)
  (if (null? (car ss))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car ss))
      (apply stream-map
             (cons proc (map stream-cdr ss))))))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (scale-stream s k)
  (stream-map (lambda (x) (* x k)) s))
(define (negate-stream s)
  (stream-map - s))

;;; ex 3.51
(define (show x) (display-line x) x)
(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))
;; 0
;; => (0 . #<promise #2>)
(stream-ref x 5)
;; 1
;; 2
;; 3
;; 4
;; 5
;; => 5
(stream-ref x 7)
;; 6
;; 7
;; => 7

;;; ex 3.52
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
;; [sum = 1]
(define y (stream-filter even? seq))
;; [sum = 6]
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
;; [sum = 10]
(stream-ref y 7) ; => 120
;; [sum = 120]
(display-stream z)
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210
;; => done
;; Yes, the responses would differ if we had not memoized the procedure created
;; by `delay`. The stream `seq` would get its `cdr` evaluated multiple times,
;; and it would be different each time because `sum` would keep getting added
;; to. Instead of 1, 6, 10, and 120, we would see 1, 6, 15, and 162. Then, when
;; we display `z`, we would see only one element, 15. The rest of seq gets
;; generated using a much higher `sum`, and none of those end up being divisible
;; by five.

;;; ssec 3.5.2 (infinite streams)
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (inc n))))
(define (sieve s)
  (cons-stream
    (stream-car s)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car s))))
             (stream-cdr s)))))
(define primes (sieve (integers-starting-from 2)))

;;; ssec 3.5.2 (defining streams implicitly)
(define primes
  (cons-stream
    2
    (stream-filter prime? (integers-starting-from 3))))
(define (prime? n)
  (define (iter ps)
    (or (> (square (stream-car ps)) n)
        (and (not (divisible? n (stream-car ps)))
             (iter (stream-cdr ps)))))
  (iter primes))

;;; ex 3.53
(define s (cons-stream 1 (add-streams s s)))
;; This produces all powers of two. It is similar to an earlier example:
(= (add-streams s s) (scale-stream s 2))

;;; ex 3.54
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define from-two (integers-starting-from 2))
(define factorials
  (cons-stream 1 (mul-streams factorials from-two)))

;;; ex 3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (partial-sums s) (stream-cdr s))))
;; The following procedure is more efficient because it uses itself via
;; corecursion rather than calling the procedure recursively, which would
;; prevent the memoizing from working. I could be wrong.
(define (partial-sums s)
  (define ps
    (cons-stream (stream-car s)
                 (add-streams ps (stream-cdr s))))
  ps)

;;; ex 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream
                     s1car
                     (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream
                     s2car
                     (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream
                      s1car
                      (merge (stream-cdr s1)
                             (stream-cdr s2)))))))))
(define S
  (cons-stream 1 (merge (scale-stream S 2)
                        (merge (scale-stream S 3)
                               (scale-stream S 5)))))

;;; ex 3.57
(define fibs
  (cons-stream
    0
    (cons-stream 1 (add-streams fibs (stream-cdr fibs)))))
;; See the relevant section of `proofs/proofs.pdf`.

;;; ex 3.58
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))
;; This procedure produces the stream of digits in the base given by `radix`
;; that represent the quotient of `num` and `den`. It does it without using
;; floating-point operations.
(expand 1 7 10)          ; => (1 4 2 8 5 7 ...)
(exact->inexact (/ 1 7)) ; => .14285714285714285
(expand 3 8 10)          ; => (3 7 5 0 0 0 ...)
(exact->inexact (/ 3 8)) ; => .375

;;; ex 3.59
;; (a) integration
(define (integrate-series ps)
  (stream-map / ps (integers-starting-from 1)))
;; (b) exponential, sine, cosine
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))
(define cosine-series
  (cons-stream 1 (negate-stream (integrate-series sine-series))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;;; ex 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

;; ex 3.61
(define (invert-unit-series s)
  (cons-stream
    1
    (negate-stream (mul-series (stream-cdr s)
                               (invert-unit-series s)))))
;; `invert-series` requires a nonzero constant term.
(define (invert-series s)
  (let ((fst (stream-car s)))
    (if (= fst 1)
      (invert-unit-series s)
      (scale-stream
        (invert-unit-series (scale-stream s (/ fst)))
        (/ fst)))))

;;; ex 3.62
(define (div-series s1 s2)
  (let ((den-car (stream-car s2)))
    (if (zero? den-car)
      (error "Can't divide by zero: DIV-SERIES" (list s1 s2))
      (mul-series s1 (invert-series s2)))))
(define tangent-series
  (div-series sine-series cosine-series))

;;; ssec 3.5.3 (iterations as stream procs)
(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)
(define (pi-summands n)
  (cons-stream (/ n)
               (stream-map - (pi-summands (+ n 2)))))
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

;;; ex 3.63
(define (sqrt-stream x)
  (cons-stream
    1.0
    (stream-map
      (lambda (guess) (sqrt-improve guess x))
      (sqrt-stream x))))
;; This implementation is less efficient because it doesn't take advantage of
;; memoization, so it does redundant computation. This procedure improves the
;; first guess to get the second item, but then it must do that all over again
;; in the recursive call to improve the second guess. This is because calling
;; `(sqrt-stream x)` creates a new stream, distinct from the one being returned
;; in the context of the current procedure evaluation. It will have it's own
;; cons cell to begin it, and therefore values in one will not be memoized in
;; the other. In general, to get the next element of the stream, we must restart
;; from the beginning every time. If our implementation of `delay` didn't use
;; the optimization provided by `memo-proc`, then there would be no difference
;; in efficiency between the original procedure and this one.

;;; ex 3.64
(define (stream-limit s tolerance)
  (define (iter prev s)
    (let ((next (stream-car s)))
      (if (< (abs (- prev next)) tolerance)
        next
        (iter next (stream-cdr s)))))
  (iter (stream-car s) (stream-cdr s)))
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;;; ex 3.65
(define (ln-2-summands n)
  (cons-stream (/ n)
               (negate-stream (ln-2-summands (inc n)))))
(define ln-2-stream
  (partial-sums (ln-2-summands 1)))
;; This converges fairly slowly on its own:
(log 2) ; => 0.6931471805599453
(display-stream (stream-map exact->inexact ln-2-stream))
;; => 1.0
;;    0.5
;;    0.8333333333333334
;;    0.5833333333333334
;;    0.7833333333333333
;;    0.6166666666666667
;;    0.7595238095238095
;;    0.6345238095238095
;;    0.7456349206349207
;;    0.6456349206349207
;;    ...
;; The accelerated sequence converges much faster:
(define accel-ln-2 (accelerated-sequence euler-transform ln-2-stream))
(display-stream (stream-map exact->inexact accel-ln-2))
;; => 1.0
;;    0.7
;;    0.6932773109243697
;;    0.6931488693329254
;;    0.6931471960735491
;;    0.6931471806635636
;;    0.6931471805604039
;;    0.6931471805599444
;;    0.6931471805599435
;;    0.6931471805599453
;; The tenth item looks identical to the printed value of `(log 2)`. That's
;; sixteen decimal places.

;;; ssec 3.5.3 (infinite streams of pairs)
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))
(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1)))))

;;; ex 3.66
;; Let f(n) represent the nth pair in the stream, starting from one.
;; f(1)  = (1,1); f(i) = (1,x) when i = 2  + 2N
;; f(3)  = (2,2); f(i) = (2,x) when i = 5  + 4N
;; f(7)  = (3,3); f(i) = (3,x) when i = 11 + 8N
;; f(15) = (4,4); f(i) = (4,x) when i = 23 + 16N
;; ...
;; The next row (where the first number in the pair is incremented once) always
;; gets updated half as often as the previous row.
;; In general,
;; f(2^x - 1) = (x,x);
;; f(2^x - 1 + (2N-1)*2^(x-1)) = (x,x+N), N > 0.
;; The pair (1,100) is the 198th pair in the stream.
;; The pair (99,100) is the 9.507e29th in the stream.
;; The pair (100,100) is (2^100-1)th in the stream.
;; See `whiteboard/exercise-3.66.jpg` for the function from indices to pairs,
;; and its inverse (from pairs to indices).

;;; ex 3.67
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (interleave
        (stream-map (lambda (x) (list (stream-car t) x))
                    (stream-cdr s))
        (pairs (stream-cdr s) (stream-cdr t))))))

;;; ex 3.68
(define (pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))
;; No, this will not work, because there will be an infinite loop. The recursive
;; call `(pairs (stream-cdr s) (stream-cdr t))` is not delayed by a
;; `cons-stream`, so the procedure will never return.

;;; ex 3.69
(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x))
                  (pairs t u))
      (triples (stream-cdr s)
               (stream-cdr t)
               (stream-cdr u)))))
(define (pythagorean-triples)
  (stream-filter
    (lambda (x)
      (= (+ (square (car x) (cadr x))) (square (caddr x))))
    (triples integers integers integers)))

;;; ex 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (if (<= (weight s1car) (weight s2car))
              (cons-stream s1car
                           (merge (stream-cdr s1) s2))
              (cons-stream s2car
                           (merge s1 (stream-cdr s2))))))))
(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car s))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))
(define part-a
  (weighted-pairs integers
                  integers
                  (lambda (p) (apply + p))))
(define part-b
  (define not-235
    (stream-filter
      (lambda (x)
        (not (or (divisible? x 2)
                 (divisible? x 3)
                 (divisible? x 5))))
      integers))
  (weighted-pairs not-235
                  not-235
                  (lambda (p)
                    (+ (* 2 (car p))
                       (* 3 (cadr p))
                       (* 5 (car p) (cadr p))))))

;;; ex 3.71
(define ramanujan-numbers
  (define (cube x)
    (* x x x))
  (define (weight a b)
    (+ (cube a) (cube b)))
  (define (analyze pairs)
    (let ((w1 (weight (stream-car pairs)))
          (w2 (weight (stream-car (stream-cdr pairs)))))
      (if (= w1 w2)
        (stream-cons
          (list w1 (stream-car pairs) (stream-car (stream-cdr pairs)))
          (analyze (stream-cdr (stream-cdr pairs))))
        (analyze (stream-cdr pairs)))))
  (analyze (weighted-pairs integers integers weight)))
;; first six Ramanujan numbers: 1729, 4104, 13832, 20683, 32832, 39312

;;; ex 3.72
(define thrice-square-sums
  (define (weight a b)
    (+ (square a) (square b)))
  (define (analyze pairs)
    (let ((w1 (weight (stream-car pairs)))
          (w2 (weight (stream-car (stream-cdr pairs))))
          (w3 (weight (stream-car (stream-cdr (stream-cdr pairs))))))
      (if (= w1 w2 w3)
        (stream-cons
          (list w1
                (stream-car pairs)
                (stream-car (stream-cdr pairs))
                (stream-car (stream-cdr (stream-cdr pairs))))
          (analyze (stream-cdr (stream-cdr (stream-cdr pairs)))))
        (analyze (stream-cdr pairs)))))
  (analyze (weighted-pairs integers integers weight)))

;; ex 3.73
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)
|#
