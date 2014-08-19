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

;;; ex 3.15
(define (set-to-wow! x) (set-car! (car x) 'wow) x)
(define x (list 'a 'b))
(define z1 (cons x x))
z1 ; => ((a b) a b)
;; The `car` and the `cdr` of `z1` both point to `x`.
; z1->[*|*]
;      | |
;      V V
;  x->[*|*]->[*|X]
;      |      |
;      V      V
;      a      b
(set-to-wow! z1) ; => ((wow b) wow b)
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
z2 ; => ((a b) a b)
;; This is a straightforward list that happens to look "the same" as `z1`.
; z2->[*|*]->[*|*]->[*|X]
;      |      |      |
;      |      +-> a  +-> b
;      V
;    [*|*]->[*|X]
;     |      |
;     +-> a  +-> b
(set-to-wow! z2) ; => ((wow b) a b)
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
(count-pairs three-3) ; => 3
(define three-4 (cons 'a (cons p p))) ; two pairs + p = three pairs
(count-pairs three-4) ; => 4
(define ab (cons 'a (cons 'b '()))) ; two pairs
(define three-5 (cons ab ab)) ; one pair + ab = three pairs
(count-pairs three-5) ; => 5
(define aa (cons p p)) ; => one pair + p = two pairs
(define three-7 (cons aa aa)) ; => one pair + aa = three pairs
(count-pairs three-7) ; => 7

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
(count-pairs three-3) ; => 3
(count-pairs three-4) ; => 3
(count-pairs three-5) ; => 3
(count-pairs three-7) ; => 3

;;; ex 3.18
(define (cycle? ls)
  (define (iter ls seen)
    (and (pair? x)
         (or (memq ls seen)
             (iter (cdr ls) (cons ls seen)))))
  (iter ls '()))

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

;;; ssec 3.3.1 (mutation is just assignment)
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation: CONS" m))))
  dispatch)
(define (car p) (p 'car))
(define (cdr p) (p 'cdr))
(define (set-car! p v) ((p 'set-car!) v) p)
(define (set-cdr! p v) ((p 'set-cdr!) v) p)

;;; ex 3.20
(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x) ; => 17
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
(define (make-queue) (cons '() ()))
(define (front-queue q)
  (if (empty-queue? q)
    (error "FRONT called with an empty queue" q)
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
         (error "DELETE! called with an empty queue" q))
        (else (set-front-ptr! q (cdr (front-ptr q)))
              q)))

;;; ex 3.21
(define q1 (make-queue))
(insert-queue! q1 'a) ; => ((a) a)
(insert-queue! q1 'b) ; => ((a b) b)
(delete-queue! q1)    ; => ((b) b)
(delete-queue! q1)    ; => (() b)
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
        (error "DELETE! called with an empty queue")
        (begin (set! front-ptr (cdr front-ptr))
               dispatch)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty?))
            ((eq? m 'front-queue)
             (if (empty?)
               (error "FRONT called with an empty queue")
               (car front-ptr)))
            ((eq? m 'insert-queue!) insert!)
            ((eq? m 'delete-queue!) (delete!))
            (else (error "Undefined operation: MAKE-QUEUE" m))))
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
    (error "FRONT called with an empty deque" dq)
    (data-node (front-ptr dq))))
(define (rear-deque dq)
  (if (empty-deque? dq)
    (error "REAR called with an empty deque" dq)
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
         (error "FRONT-DELETE! called with an empty deque" dq))
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
         (error "REAR-DELETE! called with an empty deque" dq))
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
