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
               self-evaluating? text-of-quotation variable?)
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
         (eval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameters procedure)
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
(eval 'x env) =!> "unbound variable: x"
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
               self-evaluating? text-of-quotation variable?)
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
(define (make-assignment var val) (list 'set! var val))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))
(define (make-definition variable value) (list 'define variable value))
(define (make-lambda-definition name parameters body)
  (cons 'define (cons (cons name parameters) body)))
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
(define (make-begin actions) (cons 'begin actions))

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
        ((null? datum) (error 'type-tag "invalid syntax ()"))
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
(eval 'x env) =!> "unbound variable: x"
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

;; Implementing this as a derived expression is hard because it requires
;; generating a let/lambda to avoid evaluating the predicate twice. I've instead
;; used direct evaluation, but without error handling, so for example it will
;; ignore malformed clauses if they come after the selected clause.
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

(define (install-extended-cond-package)
  (put 'eval 'cond (lambda (exp env) (eval-cond (cond-clauses exp) env))))

(using install-eval-package install-extended-cond-package)

(define env (make-environment))
(eval '(cond (else 1)) env) => 1
(eval '(cond (#f 1)) env) => #f
(eval '(cond (#f 1) (2 => (lambda (x) x))) env) => 2
(eval '(cond (2 => (lambda (x) "hi")) (#f 1)) env) => "hi"

(Exercise ?4.6
  (use (:2.4.3 using) (:3.3.3.3 put) (:4.1.2 make-lambda)
       (:4.1.3.3 make-environment) (?4.3 eval install-eval-package)))

(define (let-bindings exp) (cadr exp))
(define (let-actions exp) (cddr exp))
(define (binding-variable exp) (car exp))
(define (binding-value exp) (cadr exp))
(define (make-let bindings body) (cons 'let (cons bindings body)))

(define (let->combination exp)
  (cons (make-lambda (map binding-variable (let-bindings exp))
                     (let-actions exp))
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

(Exercise ?4.7
  (use (:2.4.3 using) (:3.3.3.3 put) (:4.1.2 make-begin)
       (:4.1.3.3 make-environment) (?4.3 eval install-eval-package)
       (?4.6 let-bindings let-actions install-let-package make-let)))

;; It is sufficient to expand let* to nested let expressions in the new
;; evaluation clause. It will get expanded to lambdas by the recursive eval.
(define (let*->nested-lets exp)
  (define (iter bindings)
    (cond ((null? bindings) (make-begin (let-actions exp)))
          (else (make-let (list (car bindings))
                          (list (iter (cdr bindings)))))))
  (iter (let-bindings exp)))

(define (install-let*-package)
  (put 'eval 'let* (lambda (exp env) (eval (let*->nested-lets exp) env))))

(using install-eval-package install-let-package install-let*-package)

(define env (make-environment))
(eval '(let* () 1) env) => 1
(eval '(let* ((x "hi")) x) env) => "hi"
(eval '(let ((x 1) (y x)) y) env) =!> "unbound variable: x"
(eval '(let* ((x 1) (y x)) y) env) => 1

(Exercise ?4.8
  (use (:2.4.3 using) (:3.3.3.3 put) (:4.1.2 make-lambda make-lambda-definition)
       (:4.1.3.3 make-environment) (?4.3 eval install-eval-package)
       (?4.6 binding-value binding-variable let->combination)))

(define (named-let? exp) (symbol? (cadr exp)))
(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-actions exp) (cdddr exp))

(define (named-let->combination exp)
  (list (make-lambda
         '()
         (list (make-lambda-definition
                (named-let-name exp)
                (map binding-variable (named-let-bindings exp))
                (named-let-actions exp))
               (cons (named-let-name exp)
                     (map binding-value (named-let-bindings exp)))))))

(define (install-named-let-package)
  (put 'eval 'let
       (lambda (exp env)
         (eval ((if (named-let? exp) named-let->combination let->combination)
                exp)
               env))))

(using install-eval-package install-named-let-package)

(define env (make-environment))
(eval '(let () 1) env) => 1
(eval '(let ((x "hi")) x) env) => "hi"
(eval '(let foo ((x 1)) x) env) => 1
(eval '(let foo ((x #t)) (if x (foo #f) "done")) env) => "done"

(Exercise ?4.9
  (use (:2.4.3 using) (:3.3.3.3 put)
       (:4.1.2 make-begin make-if make-lambda make-lambda-definition)
       (:4.1.3.3 make-environment) (?4.3 eval install-eval-package)))

;; A do loop looks like `(while TEST EXP ...)`. It repeatedly executes the EXP
;; expressions as long as TEST evaluates to true (as in `true?`).

(define (while-test exp) (cadr exp))
(define (while-actions exp) (cddr exp))

(define (while->combination exp)
  (list (make-lambda
         '(test body)
         (list (make-lambda-definition
                'loop
                '()
                (list (make-if '(test)
                               (make-begin (list '(body) '(loop)))
                               #f)))
               '(loop)))
        (make-lambda '() (list (while-test exp)))
        (make-lambda '() (while-actions exp))))

(define (install-while-package)
  (put 'eval 'while (lambda (exp env) (eval (while->combination exp) env))))

(using install-eval-package install-while-package)

(define env (make-environment))
(eval '(define x #t) env)
(eval '(define y 1) env)
(eval '(while x (set! x #f) (set! y 2)) env)
(eval 'y env) => 2

(Exercise ?4.10
  (use (:4.1.1 apply list-of-values eval-sequence eval-assignment
               eval-definition)
       (:4.1.2 application? assignment? begin-actions begin? definition?
               lambda-body lambda-parameters lambda? operands operator quoted?
               self-evaluating? text-of-quotation variable?)
       (:4.1.2.1 cond? cond->if) (:4.1.3.1 true?) (:4.1.3.2 make-procedure)
       (:4.1.3.3 lookup-variable-value make-environment)))

;; We can change the syntax for if expressions to resemble the C ternary
;; operator: `(if TEST THEN ELSE)` becomes `(TEST ? THEN : ELSE)`.

(define (if? exp)
  (and (pair? exp)
       (= (length exp) 5)
       (eq? '? (cadr exp))
       (eq? ': (cadddr exp))))
(define (if-predicate exp) (car exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) (car (cddddr exp)))
(define (make-if predicate consequent alternative)
  (list predicate '? consequent ': alternative))

;; Note: Only pasting the minimal amount to make the tests below work, so that
;; the import list doesn't have to be so long.
(paste (:4.1.1 eval eval-if))

(define env (make-environment))
(eval '(#t ? 1 : 2) env) => 1
(eval '(#f ? 1 : 2) env) => 2

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
(define (set-first-frame! env frame) (set-car! env frame))

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
(lookup-variable-value 'x env1) =!> "unbound variable: x"
(set-variable-value! 'x 1 env1) =!> "unbound variable: x"
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

(Exercise ?4.11
  (use (:4.1.3.3 enclosing-environment first-frame set-first-frame!
                 the-empty-environment)))

(define make-binding cons)
(define binding-variable car)
(define binding-value cdr)
(define set-binding-value! set-cdr!)

(paste (:4.1.3.3 make-environment))

(define (extend-environment vars vals base-env)
  (cond ((= (length vars) (length vals))
         (cons (map cons vars vals) base-env))
        ((< (length vars) (length vals))
         (error 'extend-environment "too many arguments" vars vals))
        (else (error 'extend-environment "too few arguments" vars vals))))

(define (lookup-variable-value var env)
  (define (scan frame)
    (cond ((null? frame)
           (lookup-variable-value var (enclosing-environment env)))
          ((eq? var (binding-variable (car frame)))
           (binding-value (car frame)))
          (else (scan (cdr frame)))))
  (if (eq? env the-empty-environment)
      (error 'lookup-variable-value "unbound variable" var)
      (scan (first-frame env))))

(define (set-variable-value! var val env)
  (define (scan frame)
    (cond ((null? frame)
           (set-variable-value! var val (enclosing-environment env)))
          ((eq? var (binding-variable (car frame)))
           (set-binding-value! (car frame) val))
          (else (scan (cdr frame)))))
  (if (eq? env the-empty-environment)
      (error 'set-variable-value! "unbound variable" var)
      (scan (first-frame env))))

(define (define-variable! var val env)
  (define (scan frame)
    (cond ((null? frame)
           (set-first-frame! env (cons (make-binding var val)
                                       (first-frame env))))
          ((eq? var (binding-variable (car frame)))
           (set-binding-value! (car frame) val))
          (else (scan (cdr frame)))))
  (scan (first-frame env)))

(define env1 (make-environment))
(define env2 (make-environment env1))
(lookup-variable-value 'x env1) =!> "unbound variable: x"
(define-variable! 'x 1 env1)
(define-variable! 'y 2 env1)
(define-variable! 'x "apple" env2)
(set-variable-value! 'y "orange" env2)
(lookup-variable-value 'x env2) => "apple"
(lookup-variable-value 'y env2) => "orange"
(lookup-variable-value 'x env1) => 1
(lookup-variable-value 'y env1) => "orange"

(Exercise ?4.12
  (use (:4.1.3.3 add-binding-to-frame! enclosing-environment first-frame
                 frame-values frame-variables make-environment
                 the-empty-environment)))

(define (traverse env var action otherwise)
  (if (eq? env the-empty-environment)
      (error 'traverse "unbound variable" var)
      (let ((frame (first-frame env)))
        (define (scan vars vals)
          (cond ((null? vars) (otherwise frame (enclosing-environment env)))
                ((eq? var (car vars)) (action vals))
                (else (scan (cdr vars) (cdr vals)))))
        (scan (frame-variables frame) (frame-values frame)))))

(define (lookup-variable-value var env)
  (traverse env
            var
            (lambda (vals) (car vals))
            (lambda (frame parent) (lookup-variable-value var parent))))

(define (set-variable-value! var val env)
  (traverse env
            var
            (lambda (vals) (set-car! vals val))
            (lambda (frame parent) (set-variable-value! var val parent))))

(define (define-variable! var val env)
  (traverse env
            var
            (lambda (vals) (set-car! vals val))
            (lambda (frame parent) (add-binding-to-frame! var val frame))))

(define env1 (make-environment))
(define env2 (make-environment env1))
(lookup-variable-value 'x env1) =!> "unbound variable: x"
(define-variable! 'x 1 env1)
(define-variable! 'y 2 env1)
(define-variable! 'x "apple" env2)
(set-variable-value! 'y "orange" env2)
(lookup-variable-value 'x env2) => "apple"
(lookup-variable-value 'y env2) => "orange"
(lookup-variable-value 'x env1) => 1
(lookup-variable-value 'y env1) => "orange"

(Exercise ?4.13
  (use (:2.4.3 using) (:3.3.3.3 put)
       (:4.1.3.3 first-frame frame-variables frame-values make-environment
                 make-frame set-first-frame!)
       (?4.3 eval install-eval-package)))

;; The special form `make-unbound!` removes the binding of a symbol only if it
;; exists in the first frame of the current environment. Removing bindings from
;; enclosing environments would be too dangerous and error-prone.

(define (eval-make-unbound exp env)
  (unbind-variable! (cadr exp) env))
(define (unbind-variable! var env)
  (let ((frame (first-frame env)))
    (define (scan parent-vars parent-vals vars vals)
      (cond ((null? vars) (error 'unbind-variable! "unbound variable" var))
            ((eq? var (car vars))
             (cond ((null? parent-vars)
                    (set-first-frame! env (make-frame (cdr vars) (cdr vals))))
                   (else (set-cdr! parent-vars (cdr vars))
                         (set-cdr! parent-vals (cdr vals)))))
            (else (scan vars vals (cdr vars) (cdr vals)))))
    (scan '() '() (frame-variables frame) (frame-values frame))))

(define (install-make-unbound-package)
  (put 'eval 'make-unbound! eval-make-unbound))

(using install-eval-package install-make-unbound-package)

(define env (make-environment))
(eval '(define x 1) env)
(eval '(define y 2) env)
(eval 'x env) => 1
(eval 'y env) => 2
(eval '(make-unbound! x) env)
(eval 'x env) =!> "unbound variable: x"
(eval 'y env) => 2
(eval '(make-unbound! y) env)
(eval 'y env) =!> "unbound variable: y"

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

(Exercise ?4.14)

;; Louis's `map` fails with compound procedures because it attempts to apply an
;; object created by `make-procedure` in the underlying Lisp. (It would work
;; if he only passed other primitive procedures, like `(map car pairs)`.)
;; Eva's `map` works because evaluation of `map` stays in the metacircular
;; evaluator, which is capable of evaluating its own procedure format.

(Section :4.1.5 "Data as Programs")

(eval '(* 5 5) user-initial-environment)
=> (eval (cons '* (list 5 5)) user-initial-environment)
=> 25

(Exercise ?4.15)

(define (halts? p a)
  (error 'halts? "the halting problem is impossible to solve"))

(define (run-forever) (run-forever))
(define (try p) (if (halts? p p) (run-forever) 'halted))

;; It is impossible for `halts?` to correctly determine whether `(p a)` halts.
;; Consider the expression `(try try)`. Suppose it halts. But then
;; `(halts? p p) => (halts? try try) => #t`, so `(try try) => (run-forever)`.
;; So it must run forever instead. But then `(halts? try try) => #f`, so
;; `(try try) => 'halted`, a contradiction. This completes the proof.

(Section :4.1.6 "Internal Definitions")

(Exercise ?4.16
  (use (:2.4.3 using) (:3.3.3.3 put)
       (:4.1.2 definition-value definition-variable lambda-body
               lambda-parameters make-assignment make-lambda)
       (:4.1.3.2 make-procedure) (:4.1.3.3 make-environment)
       (?4.3 eval install-eval-package) (?4.12 traverse)))

;; (a) Change `lookup-variable-value`

(define (lookup-variable-value var env)
  (traverse env
            var
            (lambda (vals)
              (if (eq? (car vals) '*unassigned*)
                  (error 'lookup-variable-value
                         "illegal use of internal definition"
                         var)
                  (car vals)))
            (lambda (frame parent) (lookup-variable-value var parent))))

;; (b) Write `scan-out-defines`

(define (scan-out-defines body)
  ;; `(revmap f src dst)` behaves like `(append (reverse (map f src)) dst)`.
  (define (revmap f src dst)
    (cond ((null? src) dst)
          (else (revmap f (cdr src) (cons (f (car src)) dst)))))
  (define (iter body defines)
    (cond ((and (not (null? body)) (pair? (car body)) (eq? 'define (caar body)))
           (iter (cdr body) (cons (car body) defines)))
          ((null? defines) body)
          (else (let ((params (revmap definition-variable defines '()))
                      (args (map (lambda (d) ''*unassigned*) defines))
                      (body (revmap (lambda (d)
                                      (make-assignment (definition-variable d)
                                                       (definition-value d)))
                                    defines
                                    body)))
                  (list (cons (make-lambda params body) args))))))
  (iter body '()))

(scan-out-defines '()) => '()
(scan-out-defines '(1)) => '(1)
(scan-out-defines '((define x 1)))
=> '(((lambda (x) (set! x 1)) '*unassigned*))
(scan-out-defines '((define x 1) (define y 2) #f))
=> '(((lambda (x y) (set! x 1) (set! y 2) #f) '*unassigned* '*unassigned*))
(scan-out-defines '((define (f x) x)))
=> '(((lambda (f) (set! f (lambda (x) x))) '*unassigned*))
;; We assume that all internal definitions come first.
(scan-out-defines '(1 (define x 1))) => '(1 (define x 1))

;; (c) Install `scan-out-defines` in the interpreter

;; Placing it in `make-procedure` is better because it will only scan out the
;; internal definitions once per procedure. Putting it in `procedure-body` would
;; mean it scans out every time the procedure is evaluated.

(define (install-internal-definition-package)
  (put 'eval 'symbol lookup-variable-value)
  (put 'eval 'lambda
       (lambda (exp env)
         (make-procedure (lambda-parameters exp)
                         (scan-out-defines (lambda-body exp))
                         env))))

(using install-eval-package install-internal-definition-package)

(define env (make-environment))
(eval '(define (foo)
         (define (a) "hi")
         (define (b c) c)
         (b (a)))
      env)
(eval '(foo) env) => "hi"
(eval '((lambda ()
          (define x y)
          (define y 1)
          #f))
      env)
=!> "illegal use of internal definition: y"

(Exercise ?4.17)

;; Code before:

; (lambda <vars>
;   (define u <e1>)
;   (define v <e2>)
;   <e3>)

;; Environment before:

; global env <-- E1 [<vars>, u, v]

;; Code after:

; (lambda <vars>
;   (let ((u '*unassigned*)
;         (v '*unassigned*))
;     (set! u <e1>)
;     (set! v <e2>)
;     <e3>))

;; Environment after:

; global env <-- E1 [<vars] <-- E2 [u, v]

;; There is an extra frame due to the `let` expression, which gets transformed
;; into a `lambda`. This extra frame can never make a difference in the behavior
;; of a correct program, since the new `let` scope completely encloses the body.
;; If one of the internal definitions shadows a parameter, say `x`, then
;; `(set! x 1)` would have different effects in the two cases -- but not in any
;; way that could be detected, because there is no code between the two scopes.
;;
;; We can avoid the extra frame by instead making the following transformation:

; (lambda <vars>
;   (define u '*unassigned*)
;   (define v '*unassigned*)
;   (set! u <e1>)
;   (set! v <e2>)
;   <e3>)

(Exercise ?4.18)

;; No, the `solve` procedure from Section 3.5.4 will not work if internal
;; definitions are scanned out using this alternative strategy. We get:

; (define (solve f yo dt)
;   (let ((y '*unassigned*) (dy '*unassigned*))
;     (let ((a (integral (delay dy) y0 dt)) (b (stream-map f y)))
;       (set! y a)
;       (set! dy b)
;       y)))

;; The problem is with the evaluation of `(stream-map f y)`. Although streams
;; are lazy, they still evaluate the first term right away. This cannot be done
;; when `y` is still '*unassigned*.
;;
;; Yes, the procedure will work if scanning out as shown in the text. We get:

; (define (solve f yo dt)
;   (let ((y '*unassigned*) (dy '*unassigned*))
;     (set! y (integral (delay dy) y0 dt))
;     (set! dy (stream-map f y))
;     y)))

;; This time, `y` has been initialized by the time `stream-map` is called.

(Exercise ?4.19)

;; I support Alyssa's view. The fact that this case is tricky means that, even
;; if there are good theoretical grounds for a particular semantics, code like
;; this is likely to cause confusion and bugs. It is better to simply disallow
;; it, resulting in code that is more clear.
;;
;; I cannot think of an easy way to implement Eva's preference. If you think of
;; variables as nodes and uses of other variables as directed edges, then the
;; set of internal definitions form a graph, and producing Eva's behavior
;; requires topologically sorting the graph.

(Exercise ?4.20
  (use (:2.4.3 using) (:3.3.3.3 put) (:4.1.2 make-assignment)
       (:4.1.3.3 make-environment) (?4.3 eval install-eval-package)
       (?4.6 binding-value binding-variable install-let-package make-let)))

;; (a) Implement `letrec` as a derived expression:

(define letrec-bindings cadr)
(define letrec-actions cddr)

(define (letrec->let exp)
  (let ((vars (map binding-variable (letrec-bindings exp)))
        (vals (map binding-value (letrec-bindings exp))))
    (make-let (map (lambda (var) (list var ''*unassigned*)) vars)
              (append (map make-assignment vars vals)
                      (letrec-actions exp)))))

(define (install-letrec-package)
  (put 'eval 'letrec (lambda (exp env) (eval (letrec->let exp) env))))

(using install-eval-package install-let-package install-letrec-package)

(define env (make-environment))
(eval '(letrec () 1) env) => 1
(eval '(letrec ((x 2)) x) env) => 2
(eval '(letrec ((x 3) (y x)) y) env) => 3
(eval '(letrec ((f (lambda (x) (if x (f #f) "done")))) (f #t)) env) => "done"

;; (b) Louis is wrong. When evaluating `(f 5)`, where `f` is defined:

(define (f x)
  (letrec ((even? (lambda (n)
                    (if (= n 0) #t (odd? (- n 1)))))
           (odd? (lambda (n)
                   (if (= n 0) #f (even? (- n 1))))))
    "rest of body of f"))

;; We have the following environment:

; global env [f: ...] <-- E1 [x: 5] <-- E2 [even?, odd?]
;                                       ^     |     |
;                                       |     V     V
;                                       +---[*|*]-[*|*]

;; But when using `let` instead of `letrec`, we have:

; global env [f: ...] <-- E1 [x: 5] <-- E2 [even?, odd?]
;                         ^                   |     |
;                         |                   V     V
;                         +-----------------[*|*]-[*|*]

;; So references to `even?` inside `odd?` and vice versa will not resolve.

(Exercise ?4.21)

;; (a) Check that it works, and devise an analogous expression for Fibonacci.

((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
 10)
=> 3628800

((lambda (n)
   ((lambda (fib) (fib fib n))
    (lambda (f k) (if (<= k 1) k (+ (f f (- k 1)) (f f (- k 2)))))))
 10)
=> 55

;; (b) Fill in the missing expressions for the even/odd mutual recursion.

(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1))))))

(map f '(0 1 2 3 4 5)) => '(#t #f #t #f #t #f)

(Section :4.1.7 "Separating Syntactic Analysis from Execution")

) ; end of SICP
) ; end of library
