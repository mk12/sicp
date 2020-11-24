;;; Copyright 2020 Mitchell Kember. Subject to the MIT License.

#!r6rs

(library (src chapter-4)
  (export chapter-4-effects)
  (import (rnrs base (6))
          (src lang sicp)
          (only (src chapter-3) chapter-3-effects))

;; Introduce a dependency on the previous chapter so that it executes first.
(define chapter-4-effects chapter-3-effects)

(SICP

(Chapter :4 "Metalinguistic Abstraction")

(Section :4.1 "The Metacircular Evaluator")

(Section :4.1.1 "The Core of the Evaluator"
  (use (:4.1.2 application? assignment-value assignment-variable assignment?
               begin-actions begin? definition-value definition-variable
               definition? first-exp first-operand if-alternative if-consequent
               if-predicate if? lambda-body lambda-parameters lambda? last-exp?
               no-operands? operands operator quoted? rest-exps rest-operands
               self-evaluating? text-of-quotation variable? )
       (:4.1.2.1 cond? cond->if)
       (:4.1.2.2 apply-primitive-procedure primitive-implementation
                 primitive-procedure?)
       (:4.1.3.1 true?)
       (:4.1.3.2 compound-procedure? make-procedure procedure-body
                 procedure-environment procedure-parameters)
       (:4.1.3.3 define-variable! extend-environment lookup-variable-value
                 make-environment set-variable-value!)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error 'eval "unknown expression type" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else (error 'apply "unknown procedure type" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env))

(define env (make-environment))
(eval 1 env) => 1
(eval "hi" env) => "hi"
(eval ''a env) => 'a
(eval 'x env) =!> "unbound variable"
(eval '(define x 1) env)
(eval 'x env) => 1
(eval '(set! x 2) env)
(eval 'x env) => 2
(eval '(if "truthy" "yes" "no") env) => "yes"
(eval '(cond (else 1)) env) => 1
(eval '(begin 1 2 3) env) => 3
(eval '((lambda () "hi")) env) => "hi"
(eval '((lambda (x y) y) 1 2) env) => 2
(eval #\a env) =!> "unknown expression type"

(Exercise ?4.1
  (use (:4.1.2 application? assignment-value assignment-variable assignment?
               begin-actions begin? definition-value definition-variable
               definition? first-exp first-operand if-alternative if-consequent
               if-predicate if? lambda-body lambda-parameters lambda? last-exp?
               no-operands? operands operator quoted? rest-exps rest-operands
               self-evaluating? text-of-quotation variable? )
       (:4.1.2.1 cond? cond->if)
       (:4.1.2.2 apply-primitive-procedure primitive-implementation
                 primitive-procedure?)
       (:4.1.3.1 true?)
       (:4.1.3.2 compound-procedure? make-procedure procedure-body
                 procedure-environment procedure-parameters)
       (:4.1.3.3 define-variable! extend-environment lookup-variable-value
                 make-environment set-variable-value!)))

;; Paste everything except `list-of-values`:
(paste (:4.1.1 eval apply eval-if eval-sequence eval-assignment
               eval-definition))

;; Evaluates operands from left to right:
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
        (cons first-value
              (list-of-values (rest-operands exps) env)))))

(define env (make-environment))
(eval '(define x 0) env)
(eval '((lambda (a b) x) (set! x "left") (set! x "right")) env) => "right"

;; Evaluates operands from right to left:
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest-values))))

(define env (make-environment))
(eval '(define x 0) env)
(eval '((lambda (a b) x) (set! x "left") (set! x "right")) env) => "left"

(Section :4.1.2 "Representing Expressions")

(define (self-evaluating? exp) (or (boolean? exp) (number? exp) (string? exp)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag) (and (pair? exp) (eq? (car exp) tag)))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      #f))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(sequence->exp '()) => '()
(sequence->exp '(1)) => 1
(sequence->exp '(1 2)) => '(begin 1 2)

(Section :4.1.2.1 "Derived expressions"
  (use (:4.1.2 make-if sequence->exp tagged-list?)))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      #f ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error 'expand-clauses "else clause isn't last" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(cond->if '(cond ((< x 0) -1)
                 ((= x 0) 0)
                 ((> x 0) 1)
                 (else impossible)))
=> '(if (< x 0) -1 (if (= x 0) 0 (if (> x 0) 1 impossible)))

(cond->if '(cond (else foo)
                 ((= x 1) bar)))
=!> "else clause isn't last"

(Section :4.1.2.2 "Primitive procedures"
  (use (:4.1.2 tagged-list?)))

;; Moved here from Section 4.1.4 to avoid import cycle.
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(apply-primitive-procedure (list 'primitive car) (list '(a . b))) => 'a

(Exercise ?4.2
  (use (:4.1.1 apply list-of-values eval-if eval-sequence eval-assignment
               eval-definition)
       (:4.1.2 assignment? begin-actions begin? definition? if? lambda-body
               lambda-parameters lambda? quoted? self-evaluating? tagged-list?
               text-of-quotation variable?)
       (:4.1.2.1 cond? cond->if) (:4.1.3.2 make-procedure)
       (:4.1.3.3 lookup-variable-value make-environment)))

;; (a) Lous is wrong -- moving the clause for procedure applications further up
;; will break evaluation. For example, it will treat `(define x 3)` as an
;; application of the operator `define` to the operands `x` and `3`. This is
;; because `application?` is implemented simply as `pair?`. We assume a pair is
;; an application after ruling out all the special forms.

;; (b) We can change the syntax so that applications start with `call`:

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

(paste (:4.1.1 eval))

(define env (make-environment))
(eval '((lambda () "hi")) env) =!> "unknown expression type"
(eval '(call (lambda () "hi")) env) => "hi"
(eval '(call (lambda (x) x) 1) env) => 1

(Exercise ?4.3
  (use (:1.3.1 identity) (:2.4.3 using) (:3.3.3.3 get put)
       (:4.1.2 assignment-value assignment-variable begin-actions
               definition-value definition-variable first-exp first-operand
               if-alternative if-consequent if-predicate lambda-body
               lambda-parameters last-exp? no-operands? operands operator
               rest-exps rest-operands text-of-quotation)
       (:4.1.2.1 cond->if)
       (:4.1.2.2 apply-primitive-procedure primitive-implementation
                 primitive-procedure?)
       (:4.1.3.1 true?)
       (:4.1.3.2 compound-procedure? make-procedure procedure-body
                 procedure-environment procedure-parameters)
       (:4.1.3.3 define-variable! extend-environment lookup-variable-value
                 make-environment set-variable-value!)))

(define (eval exp env)
  (let ((proc (get 'eval (type-tag exp))))
    (if proc
        (proc exp env)
        ((get 'eval 'call) exp env))))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((boolean? datum) 'boolean)
        ((number? datum) 'number)
        ((string? datum) 'string)
        ((symbol? datum) 'symbol)
        (else (error 'type-tag "unknown expression type" datum))))

;; Paste everything except `eval`:
(paste (:4.1.1 apply list-of-values eval-if eval-sequence eval-assignment
               eval-definition))

(define (install-eval-package)
  (define (on-exp f) (lambda (exp env) (f exp)))
  (put 'eval 'boolean (on-exp identity))
  (put 'eval 'number (on-exp identity))
  (put 'eval 'string (on-exp identity))
  (put 'eval 'symbol lookup-variable-value)
  (put 'eval 'quote (on-exp text-of-quotation))
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda
    (lambda (exp env)
      (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
  (put 'eval 'begin
    (lambda (exp env) (eval-sequence (begin-actions exp) env)))
  (put 'eval 'cond
    (lambda (exp env) (eval (cond->if exp) env)))
  (put 'eval 'call
    (lambda (exp env)
      (apply (eval (operator exp) env)
             (list-of-values (operands exp) env)))))

(using install-eval-package)

(define env (make-environment))
(eval 1 env) => 1
(eval "hi" env) => "hi"
(eval ''a env) => 'a
(eval 'x env) =!> "unbound variable"
(eval '(define x 1) env)
(eval 'x env) => 1
(eval '(set! x 2) env)
(eval 'x env) => 2
(eval '(if "truthy" "yes" "no") env) => "yes"
(eval '(cond (else 1)) env) => 1
(eval '(begin 1 2 3) env) => 3
(eval '((lambda () "hi")) env) => "hi"
(eval '((lambda (x y) y) 1 2) env) => 2
(eval #\a env) =!> "unknown expression type"

(Exercise ?4.4
  (use (:2.4.3 using) (:3.3.3.3 put) (:4.1.3.1 false? true?)
       (:4.1.3.3 make-environment) (?4.3 eval install-eval-package)))

(define (install-and-or-package)
  (define (eval-and exps env)
    (cond ((null? exps) #t)
          ((null? (cdr exps)) (eval (car exps) env))
          ((true? (eval (car exps) env)) (eval-and (cdr exps) env))
          (else #f)))
  (define (eval-or exps env)
    (cond ((null? exps) #f)
          (else (let ((value (eval (car exps) env)))
                  (cond ((true? value) value)
                        (else (eval-or (cdr exps) env)))))))
  (put 'eval 'and (lambda (exp env) (eval-and (cdr exp) env)))
  (put 'eval 'or (lambda (exp env) (eval-or (cdr exp) env))))

(using install-eval-package install-and-or-package)

(define env (make-environment))
(eval '(and) env) => #t
(eval '(and 1) env) => 1
(eval '(and 1 2) env) => 2
(eval '(and 2 #f 1) env) => #f
(eval '(and #f #f #f) env) => #f
(eval '(or) env) => #f
(eval '(or 1) env) => 1
(eval '(or 1 2) env) => 1
(eval '(or 2 #f 1) env) => 2
(eval '(or #f #f #f) env) => #f

(Exercise ?4.5
  (use (:2.4.3 using) (:3.3.3.3 put)
       (:4.1.2.1 cond-actions cond-clauses cond-else-clause? cond-predicate)
       (:4.1.3.1 true?) (:4.1.3.3 make-environment)
       (?4.3 apply eval eval-sequence install-eval-package)))

(define (cond-arrow-clause? clause)
  (and (not (null? (cdr clause)))
       (eq? '=> (cadr clause))))
(define (cond-arrow-recipient clause) (caddr clause))

(define (install-extended-cond-package)
  ;; Implementing this as a derived expression is hard because it requires
  ;; generating a let/lambda to avoid evaluating the predicate twice. I've
  ;; instead used direct evaluation, but without error handling, so for example
  ;; it will accept malformed clauses if they come after the true one.
  (define (eval-cond clauses env)
    (cond ((null? clauses) #f)
          ((cond-arrow-clause? (car clauses))
           (let ((value (eval (cond-predicate (car clauses)) env)))
             (if (true? value)
                 (apply (eval (cond-arrow-recipient (car clauses)) env)
                        (list value)))))
          ((or (cond-else-clause? (car clauses))
               (true? (eval (cond-predicate (car clauses)) env)))
           (eval-sequence (cond-actions (car clauses)) exp))
          (else (eval-cond (cdr clauses) env))))
  (put 'eval 'cond (lambda (exp env) (eval-cond (cond-clauses exp) env))))

(using install-eval-package install-extended-cond-package)

(define env (make-environment))
(eval '(cond (else 1)) env) => 1
(eval '(cond (#f 1)) env) => #f
(eval '(cond (#f 1) (2 => (lambda (x) x))) env) => 2
(eval '(cond (2 => (lambda (x) "hi")) (#f 1)) env) => "hi"

(Exercise ?4.6
  (use (:2.4.3 using) (:3.3.3.3 put) (:4.1.3.3 make-environment)
       (?4.3 eval install-eval-package)))

(define (let-bindings exp) (cadr exp))
(define (let-actions exp) (cddr exp))
(define (binding-variable exp) (car exp))
(define (binding-value exp) (cadr exp))
(define (let->combination exp)
  (cons (cons 'lambda
              (cons (map binding-variable (let-bindings exp))
                    (let-actions exp)))
        (map binding-value (let-bindings exp))))

(define (install-let-package)
  (put 'eval 'let (lambda (exp env) (eval (let->combination exp) env))))

(using install-eval-package install-let-package)

(define env (make-environment))
(eval '(let () 1) env) => 1
(eval '(let ((x "hi")) x) env) => "hi"
(eval '(let ((x 1) (y 2)) x y) env) => 2
;; Show that the value is only evaluated once:
(eval '(define f (lambda () "hi")) env)
(eval '(let ((x (set! f (f)))) x x f) env) => "hi"

(Exercise ?4.7)

;; TODO

(Exercise ?4.8)

;; TODO

(Exercise ?4.9)

;; TODO

(Exercise ?4.10)

;; TODO

(Section :4.1.3 "Evaluator Data Structures")

(Section :4.1.3.1 "Testing of predicates")

(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))

(Section :4.1.3.2 "Representing procedures"
  (use (:4.1.2 tagged-list?)))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(Section :4.1.3.3 "Operations on environments")

(define the-empty-environment '())
(define (make-environment . args)
  (let ((base (cond ((null? args) the-empty-environment)
                    ((null? (cdr args)) (car args))
                    (else (error 'make-environment "invalid args" args)))))
    (extend-environment '() '() base)))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (cond ((= (length vars) (length vals))
         (cons (make-frame vars vals) base-env))
        ((< (length vars) (length vals))
         (error 'extend-environment "too many arguments" vars vals))
        (else (error 'extend-environment "too few arguments" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error 'lookup-variable-value "unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error 'set-variable-value! "unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define env1 (make-environment))
(lookup-variable-value 'x env1) =!> "unbound variable"
(set-variable-value! 'x 1 env1) =!> "unbound variable"
(define-variable! 'x 1 env1)
(define-variable! 'y 2 env1)
(lookup-variable-value 'x env1) => 1
(lookup-variable-value 'y env1) => 2

(define env2 (make-environment env1))
(lookup-variable-value 'x env2) => 1
(lookup-variable-value 'y env2) => 2
(define-variable! 'x "apple" env2)
(set-variable-value! 'y "orange" env2)
(lookup-variable-value 'x env2) => "apple"
(lookup-variable-value 'y env2) => "orange"
;; `x` is still 1 in `env1` because we shadowed it with a new `x` in `env2`:
(lookup-variable-value 'x env1) => 1
;; `y` is "orange" in `env1` because `set-variable-value!` changed it in `env1`:
(lookup-variable-value 'y env1) => "orange"

(Exercise ?4.11)

;; TODO

(Exercise ?4.12)

;; TODO

(Exercise ?4.13)

;; TODO

(Section :4.1.4 "Running the Evaluator as a Program"
  (use (:4.1.1 eval)
       (:4.1.3.2 compound-procedure? procedure-body procedure-parameters)
       (:4.1.3.3 define-variable! extend-environment the-empty-environment)))

;; The textbook defines variables `true` and `false` in the initial environment.
;; We don't do that because we use the self-evaluating booleans #t and #f.
(define (setup-environment)
  (extend-environment (primitive-procedure-names)
                      (primitive-procedure-objects)
                      the-empty-environment))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

; (driver-loop)
; ;;; M-Eval input:
; (define (append x y) (if (null? x)
; y
; (cons (car x) (append (cdr x) y)))) ;;; M-Eval value:
; ok
; ;;; M-Eval input:
; (append '(a b c) '(d e f)) ;;; M-Eval value:
; (a b c d e f)

(Exercise ?4.14)

;; TODO

(Section :4.1.5 "Data as Programs")

) ; end of SICP
) ; end of library
