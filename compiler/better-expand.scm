;;;; A hygenic and extensible expander.
;;;; Reference: "Syntactic Abstraction: The syntax-case expander", by K. Dybvig.
;;;;
;;;; Deviations from the paper:
;;;; * functions are prefixed with s: (s for syntax!)
;;;; * unlabeled identifiers are considered global bindings instead of an error
;;;; * support n-ary lambda parameters and multi-statement-bodies
;;;; * reference information maintained as metadata of enriched symbols.

(define-record-type <syntax-object>
  (make-syntax-object expr wrap)
  syntax-object?
  (expr syntax-object-expr)
  (wrap syntax-object-wrap))

(define-record-type <mark> (make-mark) mark?)

(define-record-type <label> (make-label) label?)

(define-record-type <subst>
  (make-subst sym marks label)
  subst?
  (sym subst-sym)
  (marks subst-marks)
  (label subst-label))

(define-record-type <binding>
  (make-binding type value)
  binding?
  (type binding-type)
  (value binding-value))

(define (generate-variable-name prefix)
  (generate-uninterned-symbol prefix))

(define %the-top-mark (make-mark))

(define (s:top-marked? wrap)
  (any (lambda (x) (eq? x %the-top-mark)) wrap))

(define (s:strip x)
  (cond ((syntax-object? x)
         (if (s:top-marked? (syntax-object-wrap x))
             (syntax-object-expr x)
             (s:strip (syntax-object-expr x))))
        ((pair? x)
         (let ((car-stripped (s:strip (car x)))
               (cdr-stripped (s:strip (cdr x))))
           (if (and (eq? car-stripped (car x))
                    (eq? cdr-stripped (cdr x)))
               x       ; Return the original x to avoid unnecessary allocations.
               (cons car-stripped cdr-stripped))))
        (else x)))

(define (s:identifier? x)
  (and (syntax-object? x)
       (or (symbol? (syntax-object-expr x))
           (enriched-symbol? (syntax-object-expr x)))))

(define (s:self-evaluating? x)
  (or (number? x) (string? x) (char? x) (boolean? x)))

(define (s:wrap-marks wrap)
  (filter mark? wrap))

(define (s:add-mark mark x)
  (s:extend-wrap (list mark) x))

(define (s:add-subst id label x)
  (s:extend-wrap
   (list (make-subst
          (syntax-object-expr id)
          (s:wrap-marks (syntax-object-wrap id))
          label))
   x))

(define (s:extend-wrap wrap x)
  (if (syntax-object? x)
      (make-syntax-object
       (syntax-object-expr x)
       (s:join-wraps wrap (syntax-object-wrap x)))
      (make-syntax-object x wrap)))

;; Apply wrap1 over wrap2
(define (s:join-wraps wrap1 wrap2)
  (cond ((null? wrap1) wrap2)
        ((null? wrap2) wrap1)
        (else
         (let loop ((first (car wrap1)) (rest (cdr wrap1)))
           (if (null? rest)
               (if (and (mark? first) (eq? (car wrap2) first))
                   (cdr wrap2) ;; Like marks cancel.
                   (cons first wrap2))
               (cons first (loop (car rest) (cdr rest))))))))

(define (s:same-marks? marks1 marks2)
  (list= eq? marks1 marks2))

(define (s:environment? env)
  ;; More strictly speaking -- should be an association list.
  (pair? env))

(define (s:extend-env label binding env)
  (cons (cons label binding) env))

(define (s:error culprit message)
  (error message culprit))

(define (s:id-binding id env)
  (let ((label (s:id-label id)))
    (if label
        (s:label-binding id label env)
        (make-binding 'global (syntax-object-expr id)))))

(define (s:id-label id)
  (let ((sym (syntax-object-expr id))
        (wrap (syntax-object-wrap id)))
    (let search ((wrap wrap) (marks (s:wrap-marks wrap)))
      (if (null? wrap)
          #f
          (let ((first (car wrap)))
            (if (mark? first)
                (search (cdr wrap) (cdr marks))
                (if (and (enriched-symbol-base=? (subst-sym first) sym)
                         (s:same-marks? (subst-marks first) marks))
                    (subst-label first)
                    (search (cdr wrap) marks))))))))

(define (s:label-binding id label env)
  (let ((result (assq label env)))
    (if result
        (cdr result)
        (s:error id "identifier introduced is invisible in output context"))))

(define s:expand
  (simple-generic-procedure
   's:expand 3
   (lambda (x env menv)
     (let ((stripped (s:strip x)))
       (if (s:self-evaluating? stripped)
           stripped
           (s:error x "invalid syntax"))))))

(define (s:refer-to x binding)
  (let ((value (binding-value binding)))
    (make-enriched-symbol
     (enriched-symbol-base value)
     (list (cons 'original (syntax-object-expr x))
           (cons 'reference value)))))

(define (s:expand-identifier x env menv)
  (let ((binding (s:id-binding x env)))
    (case (binding-type binding)
      ((lexical global) (s:refer-to x binding))
      (else (s:error x "invalid syntax")))))

(define-generic-procedure-handler s:expand
  (match-args s:identifier? s:environment? s:environment?)
  s:expand-identifier)

(define (s:pair? x)
  (and (syntax-object? x)
       (pair? (syntax-object-expr x))))

(define (s:car x)
  (s:extend-wrap (syntax-object-wrap x)
                 (car (syntax-object-expr x))))

(define (s:cdr x)
  (s:extend-wrap (syntax-object-wrap x)
                 (cdr (syntax-object-expr x))))

(define (s:null? x)
  (null? (syntax-object-expr x)))

(define (s:identifier-application? x)
  (and (s:pair? x)
       (s:identifier? (s:car x))))

(define (s:expand-identifier-application x env menv)
  (let ((binding (s:id-binding (s:car x) env)))
    (case (binding-type binding)
      ((macro) (s:expand (s:expand-macro (binding-value) x) env menv))
      ((lexical global)
       (cons (s:refer-to (s:car x) binding)
             (s:expand-exprs (s:cdr x) env menv)))
      ((core) (s:expand-core (binding-value binding) x env menv))
      (else (s:error x "invalid syntax")))))

(define-generic-procedure-handler s:expand
  (match-args s:identifier-application? s:environment? s:environment?)
  s:expand-identifier-application)

(define (s:expand-exprs xs env menv)
  (if (s:null? xs)
      '()
      (cons (s:expand (s:car xs) env menv)
            (s:expand-exprs (s:cdr xs) env menv))))

(define (s:compound-application? x)
  (and (s:pair? x)
       (not (s:identifier? (s:car x)))))

(define (s:expand-compound-application x env menv)
  (s:expand-exprs x env menv))

(define-generic-procedure-handler s:expand
  (match-args s:compound-application? s:environment? s:environment?)
  s:expand-compound-application)

(define (s:expand-macro expander x)
  (let ((m (make-mark)))
    (s:add-mark m (expander (s:add-mark m x)))))

(define (s:expand-core expander x env menv)
  (expander x env menv))

(define (s:expand-quote x env menv)
  `(quote ,(s:strip (s:car (s:cdr x)))))

(define (s:expand-if x env menv)
  ;; TODO: check length of (s:cdr x).
  `(if ,@(s:expand-exprs (s:cdr x) env menv)))

;; Also works for improper lists.
(define (s:map->list f x)
  (cond ((s:null? x) '())
        ((s:pair? x) (cons (f (s:car x)) (s:map->list f (s:cdr x))))
        (else (f x))))

(define (s:expand-lambda x env menv)
  (let ((params (s:car (s:cdr x)))
        (bodies (s:cdr (s:cdr x))))
    (let ((names
           (s:map->list
            (lambda (param)
              (let ((label (make-label))
                    (name
                     (make-enriched-symbol
                      (generate-variable-name (s:strip param))
                      (list (cons 'original param)))))
                (set! bodies (s:add-subst param label bodies))
                (set! env (s:extend-env label
                                        (make-binding 'lexical name)
                                        env))
                name))
            params)))
      `(lambda ,names
         ;; TODO: internal defines... a pain in the ass.
         ,@(s:expand-exprs bodies env menv)))))

(define (s:initial-wrap-and-env)
  (let ((bindings
         `((quote . ,(make-binding 'core s:expand-quote))
           (if . ,(make-binding 'core s:expand-if))
           (lambda . ,(make-binding 'core s:expand-lambda)))))
    (let ((labels (map (lambda (x) (make-label)) bindings)))
      (cons
       `(,@(map (lambda (sym label)
                  (make-subst sym (list %the-top-mark) label))
                (map car bindings)
                labels)
         ,%the-top-mark)
       (map cons labels (map cdr bindings))))))

(define (syntactic-expand x)
  (let ((wrap-env (s:initial-wrap-and-env)))
    (s:expand (make-syntax-object x (car wrap-env))
              (cdr wrap-env)
              (cdr wrap-env))))

(pp
 (syntactic-expand
  '((lambda (x z)
      (display x y)
      (if x
          x
          z)
      '(lambda (lambda)
        (lambda x x)
        1))
    x)))
