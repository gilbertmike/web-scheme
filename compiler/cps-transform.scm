;;;; Converting a program to continutation-passing-style.
;;;;
;;;; This allows trivial implementations of call/cc, dynamic-winds, and other
;;;; special tricks!
;;;; Assumes the input program only has the following forms:
;;;; - lambda
;;;; - set!
;;;; - define (only global).
;;;; - if
;;;; - quote (as a constant)

;;; Determines if an expression is a special form by looking at the first
;;; symbol. Assumes this symbol has not been redefined in the lexical scope --
;;; the expander should take care of that.
(define (c:special-form-predicate tag)
  (lambda (expr)
    (and (list? expr)
         (enriched-symbol-base=? (car expr) tag))))

(define (c:constant? expr)
  (or (s:self-evaluating? expr)
      (and (pair? expr)
           (enriched-symbol-base=? (car expr)
                                   'quote))))

(define (c:variable? expr)
  (or (symbol? expr)
      (enriched-symbol? expr)))

;;; Variables and constants are self-cps already -- they cannot be broken down
;;; further into sequential steps.
(define (c:self-cps? expr)
  (or (c:constant? expr)
      (c:variable? expr)))

(define (c:continuation? expr) #t)

(define (c:cps-default expr cont)
  (list cont expr))

(define c:cps
  (simple-generic-procedure 'c:cps 3 c:cps-default))

(define (c:cps-set! expr cont)
  (let ((var (cadr expr))
        (value (caddr expr)))
    (if (c:self-cps? value)
        `(,cont ,expr)
        (let ((result (generate-variable-name 'cps-result)))
          (c:cps value
                 `(lambda (,result)
                    (,cont (set! ,var ,result))))))))

(define-generic-procedure-handler c:cps
  (match-args (c:special-form-predicate 'set!) c:continuation?)
  c:cps-set!)

(define (c:cps-if expr cont)
  (let ((predicate (cadr expr))
        (cps-consequence (c:cps (caddr expr) cont))
        (cps-alternative (let ((alternative (cadddr expr)))
                           )))
    (if (c:self-cps? predicate)
        `(if ,predicate
             ,cps-consequence
             ,cps-alternative))))

(pp
 (c:cps '(set! x (+ 1 1)) '%halt))


