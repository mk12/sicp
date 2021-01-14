;;; Copyright 2021 Mitchell Kember. Subject to the CC BY-SA 4.0 License.

#!r6rs

(library (src sicp chapter-4)
  (export chapter-4-effects)
  (import (rnrs base (6))
          (src lang sicp)
          (only (src sicp chapter-3) chapter-3-effects))

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
       (:4.1.2.1 cond->if cond?)
       (:4.1.2.2 apply-primitive-procedure primitive-procedure?)
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

(define (apply proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         (eval-sequence
          (procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else (error 'apply "unknown procedure type" proc))))

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
       (:4.1.2.1 cond->if cond?)
       (:4.1.2.2 apply-primitive-procedure primitive-procedure?)
       (:4.1.3.1 true?)
       (:4.1.3.2 compound-procedure? make-procedure procedure-body
                 procedure-environment procedure-parameters)
       (:4.1.3.3 define-variable! extend-environment lookup-variable-value
                 make-environment set-variable-value!)))

;; Paste everything except `list-of-values`:
(paste (:4.1.1 apply eval eval-assignment eval-definition eval-if
               eval-sequence))

;; Evaluates operands from left to right:
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
        (cons first-value
              (list-of-values (rest-operands exps) env)))))

(with-eval eval (make-environment)
  (define x 0)
  ((lambda (a b) x) (set! x "left") (set! x "right")))
=> "right"

;; Evaluates operands from right to left:
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest-values))))

(with-eval eval (make-environment)
  (define x 0)
  ((lambda (a b) x) (set! x "left") (set! x "right")))
=> "left"

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
  (use (:4.1.1 apply eval-assignment eval-definition eval-if eval-sequence
               list-of-values)
       (:4.1.2 assignment? begin-actions begin? definition? if? lambda-body
               lambda-parameters lambda? quoted? self-evaluating? tagged-list?
               text-of-quotation variable?)
       (:4.1.2.1 cond->if cond?) (:4.1.3.2 make-procedure)
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
       (:4.1.2.2 apply-primitive-procedure primitive-procedure?)
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
(paste (:4.1.1 apply eval-assignment eval-definition eval-if eval-sequence
               list-of-values))

(define (eval-pkg)
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

(using eval-pkg)

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
       (:4.1.3.3 make-environment) (?4.3 eval eval-pkg)))

(define (and-or-pkg)
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

(using eval-pkg and-or-pkg)

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
       (?4.3 apply eval eval-pkg eval-sequence)))

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

(define (extended-cond-pkg)
  (put 'eval 'cond (lambda (exp env) (eval-cond (cond-clauses exp) env))))

(using eval-pkg extended-cond-pkg)

(define env (make-environment))
(eval '(cond (else 1)) env) => 1
(eval '(cond (#f 1)) env) => #f
(eval '(cond (#f 1) (2 => (lambda (x) x))) env) => 2
(eval '(cond (2 => (lambda (x) "hi")) (#f 1)) env) => "hi"

(Exercise ?4.6
  (use (:2.4.3 using) (:3.3.3.3 put) (:4.1.2 make-lambda tagged-list?)
       (:4.1.3.3 make-environment) (?4.3 eval eval-pkg)))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-actions exp) (cddr exp))
(define (binding-variable exp) (car exp))
(define (binding-value exp) (cadr exp))
(define (make-let bindings body) (cons 'let (cons bindings body)))

(define (let->combination exp)
  (cons (make-lambda (map binding-variable (let-bindings exp))
                     (let-actions exp))
        (map binding-value (let-bindings exp))))

(define (let-pkg)
  (put 'eval 'let (lambda (exp env) (eval (let->combination exp) env))))

(using eval-pkg let-pkg)

(define env (make-environment))
(eval '(let () 1) env) => 1
(eval '(let ((x "hi")) x) env) => "hi"
(eval '(let ((x 1) (y 2)) x y) env) => 2
;; Show that the value is only evaluated once:
(eval '(define f (lambda () "hi")) env)
(eval '(let ((x (set! f (f)))) x x f) env) => "hi"

(Exercise ?4.7
  (use (:2.4.3 using) (:3.3.3.3 put) (:4.1.2 make-begin)
       (:4.1.3.3 make-environment) (?4.3 eval eval-pkg)
       (?4.6 let-actions let-bindings let-pkg make-let)))

;; It is sufficient to expand let* to nested let expressions in the new
;; evaluation clause. It will get expanded to lambdas by the recursive eval.
(define (let*->nested-lets exp)
  (define (iter bindings)
    (cond ((null? bindings) (make-begin (let-actions exp)))
          (else (make-let (list (car bindings))
                          (list (iter (cdr bindings)))))))
  (iter (let-bindings exp)))

(define (let*-pkg)
  (put 'eval 'let* (lambda (exp env) (eval (let*->nested-lets exp) env))))

(using eval-pkg let-pkg let*-pkg)

(define env (make-environment))
(eval '(let* () 1) env) => 1
(eval '(let* ((x "hi")) x) env) => "hi"
(eval '(let ((x 1) (y x)) y) env) =!> "unbound variable: x"
(eval '(let* ((x 1) (y x)) y) env) => 1

(Exercise ?4.8
  (use (:2.4.3 using) (:3.3.3.3 put) (:4.1.2 make-lambda make-lambda-definition)
       (:4.1.3.3 make-environment) (?4.3 eval eval-pkg)
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

(define (named-let-pkg)
  (put 'eval 'let
       (lambda (exp env)
         (eval ((if (named-let? exp) named-let->combination let->combination)
                exp)
               env))))

(using eval-pkg named-let-pkg)

(define env (make-environment))
(eval '(let () 1) env) => 1
(eval '(let ((x "hi")) x) env) => "hi"
(eval '(let foo ((x 1)) x) env) => 1
(eval '(let foo ((x #t)) (if x (foo #f) "done")) env) => "done"

(Exercise ?4.9
  (use (:2.4.3 using) (:3.3.3.3 put)
       (:4.1.2 make-begin make-if make-lambda make-lambda-definition)
       (:4.1.3.3 make-environment) (?4.3 eval eval-pkg)))

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

(define (while-pkg)
  (put 'eval 'while (lambda (exp env) (eval (while->combination exp) env))))

(using eval-pkg while-pkg)

(define env (make-environment))
(eval '(define x #t) env)
(eval '(define y 1) env)
(eval '(while x (set! x #f) (set! y 2)) env)
(eval 'y env) => 2

(Exercise ?4.10
  (use (:4.1.1 apply eval-assignment eval-definition eval-sequence
               list-of-values)
       (:4.1.2 application? assignment? begin-actions begin? definition?
               lambda-body lambda-parameters lambda? operands operator quoted?
               self-evaluating? text-of-quotation variable?)
       (:4.1.2.1 cond->if cond?) (:4.1.3.1 true?) (:4.1.3.2 make-procedure)
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
       (:4.1.3.3 first-frame frame-values frame-variables make-environment
                 make-frame set-first-frame!)
       (?4.3 eval eval-pkg)))

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

(define (make-unbound-pkg)
  (put 'eval 'make-unbound! eval-make-unbound))

(using eval-pkg make-unbound-pkg)

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
       (:4.1.3.3 define-variable! extend-environment make-environment
                 the-empty-environment)))

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
        (list 'null? null?)
        ;; The textbook stops here, but I include a few more primitives.
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        (list 'list list)
        (list 'newline newline)
        (list 'display display)))
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

(define env (setup-environment))
(eval '(null? '()) env) => #t
(eval '(null? '(1)) env) => #f
(eval '(car (cons 1 2)) env) => 1
(eval '(cdr (cons 1 2)) env) => 2
(eval '(= (+ 1 2) 3 (- 10 7)) env) => #t

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
       (?4.3 eval eval-pkg) (?4.12 traverse)))

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

(define (internal-definition-pkg)
  (put 'eval 'symbol lookup-variable-value)
  (put 'eval 'lambda
       (lambda (exp env)
         (make-procedure (lambda-parameters exp)
                         (scan-out-defines (lambda-body exp))
                         env))))

(using eval-pkg internal-definition-pkg)

(with-eval eval (make-environment)
  (define (foo)
    (define (a) "hi")
    (define (b c) c)
    (b (a)))
  (foo))
=> "hi"

(with-eval eval (make-environment)
  ((lambda ()
     (define x y)
     (define y 1)
     #f)))
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
       (:4.1.3.3 make-environment) (?4.3 eval eval-pkg)
       (?4.6 binding-value binding-variable let-pkg make-let)))

;; (a) Implement `letrec` as a derived expression:

(define letrec-bindings cadr)
(define letrec-actions cddr)

(define (letrec->let exp)
  (let ((vars (map binding-variable (letrec-bindings exp)))
        (vals (map binding-value (letrec-bindings exp))))
    (make-let (map (lambda (var) (list var ''*unassigned*)) vars)
              (append (map make-assignment vars vals)
                      (letrec-actions exp)))))

(define (letrec-pkg)
  (put 'eval 'letrec (lambda (exp env) (eval (letrec->let exp) env))))

(using eval-pkg let-pkg letrec-pkg)

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

(Section :4.1.7 "Separating Syntactic Analysis from Execution"
  (use (:4.1.2 application? assignment-value assignment-variable assignment?
               begin-actions begin? definition-value definition-variable
               definition? if-alternative if-consequent if-predicate if?
               lambda-body lambda-parameters lambda? operands operator quoted?
               self-evaluating? text-of-quotation variable?)
       (:4.1.2.1 cond->if cond?)
       (:4.1.2.2 apply-primitive-procedure primitive-procedure?)
       (:4.1.3.1 true?)
       (:4.1.3.2 compound-procedure? make-procedure procedure-body
                 procedure-environment procedure-parameters)
       (:4.1.3.3 define-variable! extend-environment lookup-variable-value
                 make-environment set-variable-value!)))

(define (eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else (error 'analyze "unknown expression type" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error 'analyze "empty sequence")
        (loop (car procs) (cdr procs)))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else (error 'execute-application "unknown procedure type" proc))))

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

(Exercise ?4.22
  (use (:4.1.2 application? assignment-value assignment-variable assignment?
               begin-actions begin? definition-value definition-variable
               definition? if-alternative if-consequent if-predicate if?
               lambda-body lambda-parameters lambda? operands operator quoted?
               self-evaluating? variable?)
       (:4.1.2.1 cond->if cond?)
       (:4.1.2.2 apply-primitive-procedure primitive-procedure?)
       (:4.1.3.1 true?)
       (:4.1.3.2 compound-procedure? make-procedure procedure-body
                 procedure-environment procedure-parameters)
       (:4.1.3.3 define-variable! extend-environment make-environment
                 set-variable-value!)
       (:4.1.7 analyze-quoted analyze-self-evaluating analyze-variable)
       (?4.6 let->combination let?)))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze-let exp))
        ((application? exp) (analyze-application exp))
        (else (error 'analyze "unknown expression type" exp))))

;; Paste everything except `analyze`:
(paste (:4.1.7 analyze-application analyze-assignment analyze-definition
               analyze-if analyze-lambda analyze-sequence eval
               execute-application))

(define (analyze-let exp) (analyze (let->combination exp)))

(define env (make-environment))
(eval '(let () 1) env) => 1
(eval '(let ((x "hi")) x) env) => "hi"
(eval '(let ((x 1) (y 2)) x y) env) => 2
;; Show that the value is only evaluated once:
(eval '(define f (lambda () "hi")) env)
(eval '(let ((x (set! f (f)))) x x f) env) => "hi"

(Exercise ?4.23)

;; In the case of a procedure body with a single expression, Alyssa's program
;; would result in the following process during evaluation (after analysis):

; (proc)
; ...
; ((lambda (env) (execute-sequence procs env)) env)
; (null? (cdr procs)) => #t
; ((car procs) env)
; (*analyzed-proc* env)

;; The program on the text, on the other hand, would jump straight to invoking
;; the analyzed procedure, without calling `null?` or `car`:

; (proc)
; ...
; (*analyzed-proc* env)

;; The differnce is more noticeable for a procedure body with two expressions.
;; With Alyssa's program:

; (proc)
; ...
; ((lambda (env) (execute-sequence procs env)) env)
; (null? (cdr procs)) => #f
; ((car procs) env)
; (*analyzed-proc-1* env)
; (execute-sequence (cdr procs) env)
; (null? (cdr procs)) => #t
; ((car procs) env)
; (*analyzed-proc-2* env)

;; With the program in the text:

; (proc)
; ...
; ((lambda (env) (*analyzed-proc-1* env) (*analyze-proc-2* env)) env)
; (*analyzed-proc-1* env)
; (*analyzed-proc-1* env)

(Exercise ?4.24
  (use (:4.1.1 eval) (:4.1.4 setup-environment) (:4.1.7 analyze)))

(define (new-eval exp env) ((analyze exp) env))

(define (benchmark definition n)
  (define (bench eval)
    (let ((env (setup-environment))
          (code (list (caadr definition) n)))
      (eval definition env)
      (let ((start (runtime)))
        (eval code env)
        (- (runtime) start))))
  (let ((old-time (bench eval))
        (new-time (bench new-eval)))
    (format "old: ~ss\nnew: ~ss\nestimated analysis time: ~s%\n"
            old-time
            new-time
            ;; In the limit as expressions are re-evaluated many times, the
            ;; single analysis in `new-time` is negligible. Thus `new-time` over
            ;; `old-time` gives approximately the fraction spent in evaluation,
            ;; and subtracting from 1 gives the fraction spent in analysis.
            (round (* 100 (- 1 (/ new-time old-time)))))))

(define factorial
  '(define (factorial n)
     (if (= n 1) 1 (* n (factorial (- n 1))))))

(define strange
  '(define (strange n)
     (define x ((lambda (x) x) (lambda (x) x)))
     (cond ((= n 0) 'done)
           (else "self-evaluating"
                 'quoted
                 strange
                 (set! n (- n 1))
                 (if 'cond (strange n) (strange n))
                 ((lambda () (+ n n n)))
                 (begin (strange n) (strange n))))))

(string? (benchmark factorial 1)) => #t
(string? (benchmark strange 1)) => #t

;; For small factorial inputs, the new evaluation strategy is better:

; (display (benchmark factorial 100))
; old: 9.399999999981645e-5s
; new: 5.699999999997374e-5s
; estimated analysis time: 39.0%

;; However, for large inputs, it's actually much slower!

; (display (benchmark factorial 10000))
; old: 0.06239299999999992s
; new: 0.11110200000000003s
; estimated analysis time: -78.0%

;; For *very* large inputs, they become more similar:

; (display (benchmark factorial 40000))
; old: 0.9885509999999997s
; new: 1.0609169999999999s
; estimated analysis time: -7.0%

;; The `strange` procedure spends about half its evaluation time in analysis:

; (display (benchmark strange 13))
; old: 3.0934s
; new: 1.577433s
; estimated analysis time: 49.0%

(Section :4.2 "Variations on a Scheme --- Lazy Evaluation")

(define (try a b) (if (= a 0) 1 b))

;; With lazy evaluation, this would return 1 instead of raising an error.
(try 0 (/ 1 0)) =!> "/"

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(Exercise ?4.25
  (use (:4.2 unless)))

(define (factorial n)
  (unless (= n 1) (* n (factorial (- n 1))) 1))

; (factorial 5) ; never terminates

;; This never terminates in our applicative-order Scheme because the call to
;; `unless` always evaluates all parameters, including the recursive call to
;; `factorial` when `n` is 1. In a normal-order language, it would work, since
;; the recursive call in the base case is not used and hence not evaluated.

(Exercise ?4.26
  (use (:2.4.3 using) (:3.3.3.3 put) (:4.1.4 setup-environment)
       (?4.3 eval eval-pkg)))

;; Ben is correct. You can implement `unless` as a special form:

(define (unless->if exp)
  (list 'if (cadr exp) (cadddr exp) (caddr exp)))
(define (unless-pkg)
  (put 'eval 'unless (lambda (exp env) (eval (unless->if exp) env))))

(using eval-pkg unless-pkg)

(define env (setup-environment))
(eval '(unless #f "hi" undefined-variable) env) => "hi"

;; But Alyssa also has a point: this is just syntax, so it can't be used with
;; higher-order procedures. Here is an example where that would be useful:

(with-eval eval env
  (define (map f as bs cs)
    (cond ((null? as) '())
          (else (cons (f (car as) (car bs) (car cs))
                      (map f (cdr as) (cdr bs) (cdr cs))))))
  (define uppercase '(#t #f))
  (map unless uppercase '(a b) '(A B)))
=!> "unbound variable: unless"

(Section :4.2.2 "An Interpreter with Lazy Evaluation")

(Section :4.2.2.1 "Modifying the evaluator"
  (use (:2.4.3 using) (:3.3.3.3 put)
       (:4.1.2 first-operand if-alternative if-consequent if-predicate
               no-operands? operands operator rest-operands)
       (:4.1.2.2 apply-primitive-procedure primitive-procedure?)
       (:4.1.3.1 true?)
       (:4.1.3.2 compound-procedure? procedure-body procedure-environment
                 procedure-parameters)
       (:4.1.3.3 extend-environment) (:4.1.4 setup-environment)
       (:4.2.2.2 actual-value memo-delay-it)
       (?4.3 eval eval-pkg eval-sequence)))

(define (eval-call exp env)
  (apply (actual-value (operator exp) env)
         (operands exp)
         env))

(define (apply proc args env)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc (list-of-arg-values args env)))
        ((compound-procedure? proc)
         (eval-sequence
          (procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              (list-of-delayed-args args env)
                              (procedure-environment proc))))
        (else (error 'apply "unknown procedure type" proc))))

(define (list-of-arg-values exps env)
  (cond ((no-operands? exps) '())
        (else (cons (actual-value (first-operand exps) env)
                    (list-of-arg-values (rest-operands exps) env)))))

(define (list-of-delayed-args exps env)
  (cond ((no-operands? exps) '())
        (else (cons (memo-delay-it (first-operand exps) env)
                    (list-of-delayed-args (rest-operands exps) env)))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (lazy-eval-pkg)
  (put 'eval 'call eval-call)
  (put 'eval 'if eval-if))

(using eval-pkg)
(with-eval eval (setup-environment)
  (define (try a b) (if (= a 0) 1 b))
  (try 0 (/ 1 0)))
=!> "/"

;; Note: With the lazy evaluator, we use `actual-value` for the top-level REPL
;; rather than `eval`. Thus we have a fourth condition for forcing expressions
;; (in addition to operands, if-predicates, and primitive procedure operands).
(using eval-pkg lazy-eval-pkg)
(with-eval actual-value (setup-environment)
  (define (try a b) (if (= a 0) 1 b))
  (try 0 (/ 1 0)))
=> 1

(Section :4.2.2.2 "Representing thunks"
  (use (:4.1.2 tagged-list?) (?4.3 eval)))

;; Moved here from Section 4.2.2.1 to avoid import cycle.
(define (actual-value exp env) (force-it (eval exp env)))

;; Support both non-memoized and memoized thunks, to make Exercise 4.31 easier.
(define (delay-it exp env) (list 'thunk exp env))
(define (memo-delay-it exp env) (list 'memo-thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (memo-thunk? obj) (tagged-list? obj 'memo-thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value thunk) (cadr thunk))

(define (force-it obj)
  (cond ((thunk? obj) (actual-value (thunk-exp obj) (thunk-env obj)))
        ((evaluated-thunk? obj) (thunk-value obj))
        ((memo-thunk? obj)
         (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        (else obj)))

(Exercise ?4.27
  (use (:2.4.3 using) (:4.1.4 setup-environment) (:4.2.2.1 lazy-eval-pkg)
       (:4.2.2.2 actual-value thunk?) (?4.3 eval-pkg)))

(using eval-pkg lazy-eval-pkg)
(define env (setup-environment))

(with-eval actual-value env
  (define count 0)
  (define (id x) (set! count (+ count 1)) x)
  (define w (id (id 10))))

;; When we defined `w`, it forced the outer `id` operand, resulting in `count`
;; being incremented. The `(id 10)` argument is delayed.
(actual-value 'count env) => 1

;; Using `actual-value` forces the full evaluation of `w` to 10.
(actual-value 'w env) => 10

;; Forcing `w` caused the inner `(id 10)` to increment `count` again.
(actual-value 'count env) => 2

(Exercise ?4.28
  (use (:2.4.3 using) (:4.1.2 operands operator) (:4.1.4 setup-environment)
       (:4.2.2.1 apply lazy-eval-pkg) (:4.2.2.2 actual-value force-it)
       (?4.3 eval eval-pkg)))

(using eval-pkg lazy-eval-pkg)

;; Here is an example that demonstrates the need for forcing the operand:
(define exp '(((lambda (x) x) car) (cons 1 2)))
(define env (setup-environment))

(define (do-it-with f)
  (force-it (apply (f (operator exp) env) (operands exp) env)))

;; With `actual-value` (the normal behavior), it works fine.
(do-it-with actual-value) => 1

;; But with `eval`, the operator `((lambda (x) x) car)` remains a thunk, whereas
;; `apply` expects either a primitive or compound procedure object.
(do-it-with eval) =!> "apply: unknown procedure type"

(Exercise ?4.29
  (use (:1.2.2 fib) (:2.4.3 using) (:4.1.4 setup-environment)
       (:4.2.2.1 lazy-eval-pkg) (:4.2.2.2 actual-value) (?4.3 eval-pkg)))

;; This program would be much slower without memoization, since it would
;; re-evaluate then `(fib 100)` thunk five times.
(define (five-times-fib-hundred)
  (let ((x (fib 100)))
    (+ x x x x x)))

(using eval-pkg lazy-eval-pkg)
(define env (setup-environment))

(with-eval actual-value env
  (define count 0)
  (define (id x) (set! count (+ count 1)) x)
  (define (square x) (* x x))
  (square (id 10)))
=> 100

;; With memoization, this is 1. Without memoization, it would be 2.
(actual-value 'count env) => 1

(Exercise ?4.30
  (use (:2.4.3 using) (:3.3.3.3 put)
       (:4.1.2 begin-actions first-exp last-exp? operands operator rest-exps)
       (:4.1.2.2 apply-primitive-procedure primitive-procedure?)
       (:4.1.3.2 compound-procedure? procedure-body procedure-environment
                 procedure-parameters)
       (:4.1.3.3 extend-environment) (:4.1.4 setup-environment)
       (:4.2.2.1 lazy-eval-pkg list-of-arg-values list-of-delayed-args)
       (:4.2.2.2 actual-value) (?4.3 eval eval-pkg)))

(paste (:4.2.2.1 apply eval-call))

;; Cy D. Fect proposes the following change:
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
(define (proposed-sequence-pkg)
  (put 'eval 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
  (put 'eval 'call eval-call))

(using eval-pkg lazy-eval-pkg)
(define env (setup-environment))

(with-eval actual-value env
  ;; Ben Bitdiddle's example program:
  (define (for-each proc items)
    (if (null? items)
        'done
        (begin (proc (car items))
               (for-each proc (cdr items)))))
  (define (bens-example)
    (for-each (lambda (x) (newline) (display x))
              (list 57 321 88)))
  ;; Cy D. Fect's example program:
  (define (p1 x)
    (set! x (cons x '(2))) x)
  (define (p2 x)
    (define (p e) e x)
    (p (set! x (cons x '(2))))))

;; (a) Ben is right about the behavior of `for-each` in his example. Evaluating
;; `(proc (car items))` is sufficient to enact the side effects in `proc`. There
;; is no need to force a returned thunk because it does not return anything.

(using eval-pkg lazy-eval-pkg)
(actual-value '(bens-example) env) =$> ["57" "321" "88"]

;; (b) With the original `eval-sequence`, `(p2 1)` has an unexpected result.
;; With Cy's proposed change, `p2` behaves the same as `p1`.

(using eval-pkg lazy-eval-pkg)
(actual-value '(p1 1) env) => '(1 2)
(actual-value '(p2 1) env) => 1

(using eval-pkg lazy-eval-pkg proposed-sequence-pkg)
(actual-value '(p1 1) env) => '(1 2)
(actual-value '(p2 1) env) => '(1 2)

;; (c) Cy is write: his proposed `eval-sequence` does not affect part (a). All
;; it does is force the return value of `display` and `newline`. These are void,
;; not thunks, so forcing is a no-op.

(using eval-pkg lazy-eval-pkg proposed-sequence-pkg)
(actual-value '(bens-example) env) =$> ["57" "321" "88"]

;; (d) I prefer Cy's approach because otherwise `begin` blocks, or procedures
;; with multiple expressions in the body, are useless with the lazy evaluator.
;; This does not totally solve the side-effects confusion, but it helps.

(Exercise ?4.31
  (use (:2.4.3 using) (:3.3.3.3 put)
       (:4.1.2 first-operand no-operands? operands operator rest-operands)
       (:4.1.2.2 apply-primitive-procedure primitive-procedure?)
       (:4.1.3.2 compound-procedure? procedure-body procedure-environment
                 procedure-parameters)
       (:4.1.3.3 extend-environment) (:4.1.4 setup-environment)
       (:4.2.2.1 eval-if list-of-arg-values)
       (:4.2.2.2 actual-value delay-it force-it memo-delay-it)
       (?4.3 eval-pkg eval-sequence)))

(paste (:4.2.2.1 eval-call))

(define (procedure-parameter-names p)
  (map (lambda (x) (if (pair? x) (cadr x) x))
       (procedure-parameters p)))

(define (list-of-args exps params env)
  (if (no-operands? exps)
      '()
      (let ((f (cond ((symbol? (car params)) actual-value)
                     ((eq? (caar params) 'lazy) delay-it)
                     ((eq? (caar params) 'lazy-memo) memo-delay-it)
                     (else (error 'list-of-args
                                  "invalid parameter"
                                  (car params))))))
        (cons (f (first-operand exps) env)
              (list-of-args (rest-operands exps) (cdr params) env)))))

(define (apply proc args env)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc (list-of-arg-values args env)))
        ((compound-procedure? proc)
         (eval-sequence
          (procedure-body proc)
          (extend-environment
           (procedure-parameter-names proc)
           (list-of-args args (procedure-parameters proc) env)
           (procedure-environment proc))))
        (else (error 'apply "unknown procedure type" proc))))

(define (explicit-lazy-pkg)
  (put 'eval 'call eval-call)
  (put 'eval 'if eval-if))

(using eval-pkg explicit-lazy-pkg)
(define env (setup-environment))

(with-eval actual-value env
  (define (try a b) (if (= a 0) 1 b))
  (define (try-lazy a (lazy b)) (if (= a 0) 1 b))
  (define (try-lazy-memo a (lazy-memo b)) (if (= a 0) 1 b))
  (define (double x) (+ x x))
  (define (double-lazy (lazy x)) (+ x x))
  (define (double-lazy-memo (lazy-memo x)) (+ x x)))

(actual-value '(try 0 (/ 1 0)) env) =!> "/"
(actual-value '(try-lazy 0 (/ 1 0)) env) => 1
(actual-value '(try-lazy-memo 0 (/ 1 0)) env) => 1

(actual-value '(double (begin (display "x") 1)) env) =$> "x"
(actual-value '(double-lazy (begin (display "x") 1)) env) =$> "xx"
(actual-value '(double-lazy-memo (begin (display "x") 1)) env) =$> "x"

(Section :4.2.3 "Streams as Lazy Lists"
  (use (:2.4.3 using)
       (:4.1.3.2 compound-procedure? procedure-environment procedure-parameters)
       (:4.1.3.3 lookup-variable-value make-environment)
       (:4.1.4 setup-environment) (:4.2.2.1 lazy-eval-pkg)
       (:4.2.2.2 actual-value) (?4.3 eval-pkg)))

;; Used in Exercise 4.34.
(define (lazy-cons? exp)
  (and (compound-procedure? exp)
       (equal? (procedure-parameters exp) '(*lazy-cons*))))
(define (lazy-cons-car exp)
  (lookup-variable-value 'car (procedure-environment exp)))
(define (lazy-cons-cdr exp)
  (lookup-variable-value 'cdr (procedure-environment exp)))

(using eval-pkg lazy-eval-pkg)
(define lazy-list-env (setup-environment))

(with-eval actual-value lazy-list-env
  (define (cons car cdr) (lambda (*lazy-cons*) (*lazy-cons* car cdr)))
  (define (car z) (z (lambda (p q) p)))
  (define (cdr z) (z (lambda (p q) q)))
  (define (list-ref items n)
    (if (= n 0) (car items) (list-ref (cdr items) (- n 1))))
  (define (map proc items)
    (if (null? items)
        '()
        (cons (proc (car items)) (map proc (cdr items)))))
  (define (scale-list items factor)
    (map (lambda (x) (* x factor)) items))
  (define (add-lists list1 list2)
    (cond ((null? list1) list2) ((null? list2) list1)
          (else (cons (+ (car list1) (car list2))
                      (add-lists (cdr list1) (cdr list2))))))
  (define ones (cons 1 ones))
  (define integers (cons 1 (add-lists ones integers)))
  (list-ref integers 17))
=> 18

(with-eval actual-value (make-environment lazy-list-env)
  (define (integral integrand initial-value dt)
    (define int
      (cons initial-value
            (add-lists (scale-list integrand dt) int)))
    int)
  (define (solve f y0 dt)
    (define y (integral dy y0 dt))
    (define dy (map f y))
    y)
  (list-ref (solve (lambda (x) x) 1 0.001) 1000))
~> 2.716923932235896

(Exercise ?4.32
  (use (:2.4.3 using) (:4.1.3.3 make-environment) (:4.2.2.1 lazy-eval-pkg)
       (:4.2.2.2 actual-value) (:4.2.3 lazy-list-env) (?4.3 eval-pkg)))

(using eval-pkg lazy-eval-pkg)
(define env (make-environment lazy-list-env))

;; This example works equally well with the streams from Chapter 3.
(with-eval actual-value env
  (define (scan xs)
    (cons (car xs)
          (map (lambda (x) (+ x (car xs))) (scan (cdr xs)))))
  (define pascal (cons ones (map scan pascal)))
  (define (choose n k) (list-ref (list-ref pascal (- n k)) k))
  (choose 7 2))
=> 21

;; This example takes advantage of the extra laziness of lazy lists. With the
;; streams from Chapter 3, it would display "012345" instead of "5".
(with-eval actual-value env
  (define (from n) (cons (display n) (from (+ n 1))))
  (define display-ints (from 0))
  (list-ref display-ints 5))
=$> "5"

(Exercise ?4.33
  (use (:2.4.3 using) (:3.3.3.3 put) (:4.1.2 text-of-quotation)
       (:4.1.3.3 make-environment) (:4.2.2.1 lazy-eval-pkg)
       (:4.2.2.2 actual-value) (:4.2.3 lazy-list-env) (?4.3 eval eval-pkg)))

(define (quoted->lazy x)
  (cond ((pair? x) (list 'cons (quoted->lazy (car x)) (quoted->lazy (cdr x))))
        ;; Note: We can't simply move the `eval` call inside `quoted->lazy` and
        ;; then omit it in this branch, since we might have recursed into a cons
        ;; structure. We need this "dumb-quote" escape hatch.
        (else (list 'dumb-quote x))))

(define (lazy-quote-pkg)
  (put 'eval 'quote
       (lambda (exp env) (eval (quoted->lazy (text-of-quotation exp)) env)))
  (put 'eval 'dumb-quote
       (lambda (exp env) (text-of-quotation exp))))

(define env (make-environment lazy-list-env))

(using eval-pkg lazy-eval-pkg)
(actual-value ''() env) => '()
(actual-value ''a env) => 'a
(actual-value '(car '(a b c)) env) =!> "apply: unknown procedure type"

(using eval-pkg lazy-eval-pkg lazy-quote-pkg)
(actual-value ''() env) => '()
(actual-value ''a env) => 'a
(actual-value '(car '(a b c)) env) => 'a

(Exercise ?4.34
  (use (:2.4.3 using) (:4.1.3.3 make-environment) (:4.2.2.1 lazy-eval-pkg)
       (:4.2.2.2 actual-value evaluated-thunk? force-it memo-thunk? thunk-exp)
       (:4.2.3 lazy-cons-car lazy-cons-cdr lazy-cons? lazy-list-env)
       (?4.3 eval eval-pkg)))

(define (show exp)
  (define (go exp ad dd space parens)
    (cond ((and (null? exp) (not parens)))
          ((lazy-cons? exp)
           (when space (display " "))
           (when parens (display "("))
           (if (zero? ad)
               (display "...")
               (go (lazy-cons-car exp) (- ad 1) dd #f #t))
           (if (zero? dd)
               (display " ...")
               (let* ((exp-cdr (lazy-cons-cdr exp))
                      (dot (not (or (null? exp-cdr) (pair? exp-cdr)))))
                 (when dot (display ". "))
                 (go exp-cdr ad (- dd 1) #t dot)))
           (when parens (display ")")))
          ((or (memo-thunk? exp) (evaluated-thunk? exp))
           (go (force-it exp) ad dd space parens))
          (else (display exp))))
  (go exp 9 9 #f #t))

(using eval-pkg lazy-eval-pkg)

(define env (make-environment lazy-list-env))
(with-eval actual-value env
  (define x (cons 1 (cons 2 (cons 3 '()))))
  (define ones (cons 1 ones))
  (define one-two (cons 1 (cons 2 one-two))))

(show (actual-value 'x env)) =$> "(1 2 3)"
(show (actual-value 'ones env)) =$> "(1 1 1 1 1 1 1 1 1 1 ...)"
(show (actual-value 'one-two env)) =$> "(1 2 1 2 1 2 1 2 1 2 ...)"

(Section :4.3 "Variations on a Scheme --- Nondeterministic Computing")

(Exercise ?4.35)

(Exercise ?4.54)

(Section :4.4 "Logic Programming")

(Exercise ?4.55)

(Exercise ?4.79)

) ; end of SICP
) ; end of library
