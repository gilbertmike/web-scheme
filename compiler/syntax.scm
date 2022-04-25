;;;; Scheme syntax definition from SICP chapter 5.
;;;; Loaded by compiler.scm (for use by compiler), and by eceval-support.scm
;;;;  (for simulation of eceval machine operations)

(define (special-form-predicate tag)
  (lambda (expr)
    (and (pair? expr)
         (enriched-symbol-base=? (car expr) tag))))

(define (self-evaluating? x)
  (or (number? x) (string? x) (char? x) (boolean? x)))

(define (variable? exp)
  (or (symbol? exp) (enriched-symbol? exp)))

(define (text-of-quotation exp) (cadr exp))

(define (assignment-variable exp)
  (cond ((symbol? (cadr exp))
         (cadr exp))
        ((enriched-symbol? (cadr exp))
         (enriched-symbol-base (cadr exp)))
	      (else
         (error "Syntax error: set!" exp))))

(define (assignment-value exp) (caddr exp))

(define (definition-variable exp)
  (cond ((symbol? (cadr exp))
         (cadr exp))
        ((enriched-symbol? (cadr exp))
         (enriched-symbol-base (cadr exp)))
	      (else
         (error "Syntax error: define" exp))))

(define (definition-value exp)
  (cond ((symbol? (cadr exp))
         (caddr exp))
        ((enriched-symbol? (cadr exp))
         (caddr exp))
        (else
         (error "Syntax error: define" exp))))

(define (lambda-parameters exp)
  (let loop ((rest (cadr exp)))
    (cond ((null? rest) '())
          ((pair? rest)
           (cons (enriched-symbol-base (car rest))
                 (loop (cdr rest))))
          (else (enriched-symbol-base rest)))))

(define (lambda-body exp) (cddr exp))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp) '#f))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
