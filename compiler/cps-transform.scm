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

(define (k:constant? expr)
  (or (self-evaluating? expr)
      (and (pair? expr)
           (enriched-symbol-base=? (car expr)
                                   'quote))))

;;; Variables and constants are self-cps already -- they cannot be broken down
;;; further into sequential steps.
(define (k:self-cps? expr)
  (or (k:constant? expr)
      (variable? expr)))

(define (k:continuation? expr) #t)

(define (k:cps-default expr cont)
  (if (k:self-cps? expr)
      (list cont expr)
      (k:cps-application expr cont)))

(define k:cps
  (simple-generic-procedure 'k:cps 3 k:cps-default))

(define (k:cps-set! expr cont)
  (let ((var (cadr expr))
        (value (caddr expr)))
    (if (k:self-cps? value)
        `(,cont ,expr)
        (let ((result (generate-variable-name 'cps-result-set)))
          (k:cps value
                 `(lambda (,result)
                    (,cont (set! ,var ,result))))))))

(define-generic-procedure-handler k:cps
  (match-args (special-form-predicate 'set!) k:continuation?)
  k:cps-set!)

(define (k:cps-define expr cont)
  (let ((var (cadr expr))
        (value (caddr expr)))
    (if (k:self-cps? value)
        `(,cont ,expr)
        (let ((result (generate-variable-name 'cps-result-set)))
          (k:cps value
                 `(lambda (,result)
                    (,cont (define ,var ,result))))))))

(define-generic-procedure-handler k:cps
  (match-args (special-form-predicate 'define) k:continuation?)
  k:cps-define)

(define (k:cps-if expr cont)
  (let ((predicate (cadr expr))
        (cps-c-a (map (lambda (expr) (k:cps expr cont))
                      (cddr expr))))
    (if (k:self-cps? predicate)
        `(if ,predicate
             ,@cps-c-a)
        (let ((result (generate-variable-name 'cps-result-if)))
          (k:cps predicate
                 `(lambda (,result)
                    (if ,result
                        ,@cps-c-a)))))))

(define-generic-procedure-handler k:cps
  (match-args (special-form-predicate 'if) k:continuation?)
  k:cps-if)

(define (k:cps-begin expr cont)
  (let ((first (cadr expr))
        (rest (cddr expr)))
    (if (null? rest)
        (k:cps first cont)
        (let ((result (generate-variable-name 'cps-result-begin)))
          (k:cps first
                 `(lambda (,result)
                    ,(k:cps-begin `(begin ,@rest) cont)))))))

(define-generic-procedure-handler k:cps
  (match-args (special-form-predicate 'begin) k:continuation?)
  k:cps-begin)

(define (k:cps-lambda expr cont)
  (list cont
        (let ((cont-var (generate-variable-name 'cont)))
          `(lambda ,(cons cont-var (cadr expr))
             ,(k:cps (caddr expr) cont-var)))))

(define-generic-procedure-handler k:cps
  (match-args (special-form-predicate 'lambda) k:continuation?)
  k:cps-lambda)

(define k:primitive-operators
  '(= eq? eqv? + - * / cons cdr car list))

(define (k:cps-application expr cont)
  (define (cps-operands operator cont operands rest)
    (if (null? rest)
        (if (memq (if (enriched-symbol? operator)
                      (enriched-symbol-base operator)
                      operator)
                  k:primitive-operators)
            (list cont (cons operator (reverse operands)))
            (cons operator (cons cont (reverse operands))))
        (let ((first (car rest)))
          (if (k:self-cps? first)
              (cps-operands operator cont (cons first operands) (cdr rest))
              (let ((result (generate-variable-name 'cps-result-operand)))
                (k:cps first
                       `(lambda (,result)
                          ,(cps-operands operator cont
                                         (cons result operands)
                                         (cdr rest)))))))))
  (let ((operator (car expr))
        (operands (cdr expr)))
    (if (k:self-cps? operator)
        (cps-operands operator cont '() operands)
        (let ((result (generate-variable-name 'cps-result-operator)))
          (k:cps operator
                 `(lambda (,result)
                    ,(cps-operands result cont '() operands)))))))

(define (k:cps-lambda-application expr cont)
  (define (cps-operands cps-body rest rest-params)
    (cond ((null? rest) cps-body)
          ((symbol? rest)
           (error "n-ary lambda is not currently supported by CPS transform!."))
          (else
           (let ((first (car rest))
                 (name (car rest-params)))
             (k:cps first
                    `(lambda (,name)
                       ,(cps-operands cps-body (cdr rest)
                                      (cdr rest-params))))))))
  (let* ((operator (car expr))
         (params (cadr operator))
         (body (k:cps (caddr operator) cont)))
    (cps-operands body (cdr expr) params)))

(define (k:lambda-application? expr)
  (and (pair? expr)
       (pair? (car expr))
       (enriched-symbol-base=? (caar expr) 'lambda)
       ;; Right now only handles the case where the parameter is a proper list.
       (list? (cadar expr))))

(define-generic-procedure-handler k:cps
  (match-args k:lambda-application? k:continuation?)
  k:cps-lambda-application)

(define (cps-transform expr)
  (k:cps expr '%halt))
