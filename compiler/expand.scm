;;;; Expands complex specials forms into simpler special forms.

;; TODO: replace with something machine-independent
(load "~/web-scheme/common/load.scm")

(define (special-form-name exp) (car exp))

(define (special-form? exp tag)
  (and (pair? exp)
       (eq? (special-form-name exp) tag)))

(define (expand-default exp)
  (if (pair? exp) ; Probably application.
      (map s:expand exp)
      exp))

;;; s for syntax.
(define s:expand (simple-generic-procedure 's:expand 1 expand-default))

(define (define? exp) (special-form? exp 'define))
(define (define-object exp) (cadr exp))
(define (define-body exp) (cddr exp))
(define (nested-define? exp)
  (and (define? exp)
       (not (symbol? (define-object exp)))))

(define (expand-define exp)
  (if (nested-define? exp)
      (expand-nested-define exp)
      (expand-simple-define exp)))

(define (expand-simple-define exp)
  `(define ,(define-object exp)
     ,@(map s:expand (define-body exp))))

(define (expand-nested-define exp)
  (let ((actual-object (car (define-object exp)))
        (parameters (cdr (define-object exp))))
    (let ((expanded `(define ,actual-object
                       (lambda ,parameters
                         ,@(define-body exp)))))
      (s:expand expanded))))

(define-generic-procedure-handler s:expand
  (match-args define?)
  expand-nested-define)

(define (if? exp) (special-form? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequence exp) (caddr exp))
(define (if-alternative exp) (cadddr exp))

(define (expand-if exp)
  `(if ,(s:expand (if-predicate exp))
       ,(s:expand (if-consequence exp))
       ,(s:expand (if-alternative exp))))

(define-generic-procedure-handler s:expand
  (match-args if?)
  expand-if)

(define (cond? exp) (special-form? exp 'cond))
(define (cond-branches exp) (cdr exp))
(define (cond-branch-predicate exp) (car exp))
(define (cond-branch-consequence exp) (cadr exp))

(define (expand-cond exp)
  (let ((branches (cond-branches exp)))
    (if (pair? branches)
        (let ((first-branch (car branches)))
          (let ((first-predicate (cond-branch-predicate first-branch))
                (first-consequence (cond-branch-consequence first-branch)))
            (if (eq? first-predicate 'else)
                (s:expand first-consequence)
                (s:expand
                 `(if ,first-predicate
                      ,first-consequence
                      (cond ,@(cdr branches)))))))
        '#!unspecific)))

(define-generic-procedure-handler s:expand
  (match-args cond?)
  expand-cond)

(define (case? exp) (special-form? exp 'case))
(define (case-candidate exp) (cadr exp))
(define (case-branches exp) (cddr exp))
(define (case-branch-data exp) (car exp))
(define (case-branch-consequence exp) (cadr exp))

(define (expand-case exp)
  (let ((temp-var (generate-uninterned-symbol 'case-var)))
    (let ((result
           `(let ((,temp-var ,(case-candidate exp)))
              (cond
               ,@(map (lambda (branch)
                        (let ((data (case-branch-data branch)))
                          (if (eq? data 'else)
                              `(else ,(case-branch-consequence branch))
                              ;; TODO: what if user redefines memq?
                              `((memq ,temp-var (quote ,data))
                                ,(case-branch-consequence branch)))))
                      (case-branches exp))))))
      (s:expand result))))

(define-generic-procedure-handler s:expand
  (match-args case?)
  expand-case)

(define (lambda? exp) (special-form? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (expand-lambda exp)
  `(lambda ,(lambda-parameters exp)
     ,@(map s:expand (lambda-body exp))))

(define-generic-procedure-handler s:expand
  (match-args lambda?)
  expand-lambda)

(define (simple-let? exp)
  (and (special-form? exp 'let)
       (pair? (cadr exp))))
(define (simple-let-bindings exp) (cadr exp))
(define (simple-let-body exp) (cddr exp))
(define (let-binding-name exp) (car exp))
(define (let-binding-value exp) (cadr exp))

(define (expand-simple-let exp)
  (let ((bindings (simple-let-bindings exp)))
    (let ((names (map let-binding-name bindings))
          (values (map let-binding-value bindings)))
      (let ((result
             `((lambda ,names ,@(simple-let-body exp))
               ,@values)))
        (s:expand result)))))

(define-generic-procedure-handler s:expand
  (match-args simple-let?)
  expand-simple-let)
