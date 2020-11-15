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
       (:4.1.3.1 false? true?)
       (:4.1.3.2 compound-procedure? make-procedure procedure-body
                 procedure-environment procedure-parameters)
       (:4.1.3.3 define-variable! extend-environment lookup-variable-value
                 set-variable-value!)))

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

;; TODO: tests

(Exercise ?4.1
  (use (:4.1.1 eval list-of-values)
       (:4.1.2 first-operand no-operands? rest-operands)))

(define (list-of-values-ltr exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
        (cons first-value
              (list-of-values (rest-operands exps) env)))))

(define (list-of-values-rtl exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-values (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest-values))))

;; TODO: tests

(Section :4.1.2 "Representing Expressions")

(define (self-evaluating? exp) (or (number? exp) (string? exp)))
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
      '#f))
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
      '#f ; no else clause
      (let ((first (car clauses))
        (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error 'expand-clauses "else clause isn't last" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; TODO: tests

(Section :4.1.2.2 "Primitive procedures"
  (use (:4.1.2 tagged-list?)))

;; Moved here from Section 4.1.4 to avoid import cycle.
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

(Exercise ?4.2)

;; TODO

(Exercise ?4.3)

;; TODO

(Exercise ?4.4)

;; TODO

(Exercise ?4.5)

;; TODO

(Exercise ?4.6)

;; TODO

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

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

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

;; TODO: tests

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

(define (setup-environment)
  (let ((initial-env
        (extend-environment (primitive-procedure-names)
                            (primitive-procedure-objects)
                            the-empty-environment)))
    (define-variable! '#t #t initial-env)
    (define-variable! '#f #f initial-env)
    initial-env))

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
