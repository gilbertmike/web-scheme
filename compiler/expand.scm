;;;; Expands complex specials forms into simpler special forms.

;; TODO: replace with something machine-independent
(load "~/web-scheme/common/load.scm")

(define (special-form-name exp) (car exp))

(define (special-form? exp tag)
  (and (pair? exp)
       (eq? (special-form-name exp) tag)))

;;; s for syntax.
(define s:expand
  (simple-generic-procedure
   's:expand 1
   (lambda (tree) tree)))

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
      (expand-define expanded))))

(define-generic-procedure-handler s:expand
  (match-args define?)
  expand-nested-define)

(define (if? exp) (special-form? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequence exp) (caddr exp))
(define (if-alternative exp) (cadddr exp))

(define (expand-if exp)
  '(if ,(s:expand (if-predicate exp))
       ,(s:expand (if-consequence exp))
       ,(s:expand (if-alternative exp))))

(define-generic-procedure-handler s:expand
  (match-args if?)
  expand-if)


