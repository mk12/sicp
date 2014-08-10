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
