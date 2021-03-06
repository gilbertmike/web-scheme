;;;; A hygenic and extensible expander.
;;;; Reference: "Syntactic Abstraction: The syntax-case expander", by K. Dybvig.
;;;;
;;;; Deviations from the paper:
;;;; * functions are prefixed with s: (s for syntax!)
;;;; * unlabeled identifiers are considered global bindings instead of an error
;;;; * support n-ary lambda parameters and multi-statement-bodies
;;;; * reference information maintained as metadata of enriched symbols.

;;;; Data structures.

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


;;;; Utilities.

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


;;;; Marks-related functions.

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


;;;; Compile-time environments.

(define (s:environment? env)
  ;; More strictly speaking -- should be an association list.
  (pair? env))

(define (s:extend-env label binding env)
  (cons (cons label binding) env))

(define (s:error culprit message)
  (error message culprit))

(define (s:id-binding id env)
  (let ((forced-binding
         (and (enriched-symbol? (syntax-object-expr id))
              (enriched-symbol-metadata-ref
               (syntax-object-expr id)
               'forced-binding))))
    (if forced-binding
        forced-binding
        (let ((label (s:id-label id)))
          (if label
              (s:label-binding id label env)
              (make-binding 'global (syntax-object-expr id)))))))

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


;;;; Expansion utilities.

(define s:expand
  (simple-generic-procedure
   's:expand 3
   (lambda (x env menv)
     (let ((stripped (s:strip x)))
       (if (self-evaluating? stripped)
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


;;;; Syntax object-based analogs to common list functions.

(define (s:pair? x)
  (and (syntax-object? x)
       (pair? (syntax-object-expr x))))

(define (s:car x)
  (s:extend-wrap (syntax-object-wrap x)
                 (car (syntax-object-expr x))))

(define (s:cdr x)
  (s:extend-wrap (syntax-object-wrap x)
                 (cdr (syntax-object-expr x))))

(define (s:cadr x) (s:car (s:cdr x)))

(define (s:cddr x) (s:cdr (s:cdr x)))

(define (s:null? x)
  (null? (syntax-object-expr x)))

;; Also works for improper lists.
(define (s:map f x)
  (cond ((s:null? x) '())
        ((s:pair? x) (cons (f (s:car x)) (s:map f (s:cdr x))))
        (else (f x))))


;;;; Expanding application.

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
  ((if (syntax-object? xs) s:map map) (lambda (x) (s:expand x env menv)) xs))

(define (s:compound-application? x)
  (and (s:pair? x)
       (not (s:identifier? (s:car x)))))

(define (s:expand-compound-application x env menv)
  (s:expand-exprs x env menv))

(define-generic-procedure-handler s:expand
  (match-args s:compound-application? s:environment? s:environment?)
  s:expand-compound-application)


;;;; Core form expanders.

;;; Right now we don't have a way to define macro expanders yet, but we keep the
;;; interface for the time being.
(define (s:expand-macro expander x)
  (let ((m (make-mark)))
    (s:add-mark m (expander (s:add-mark m x)))))

(define (s:expand-core expander x env menv)
  (expander x env menv))

(define (s:expand-quote x env menv)
  `(quote ,(s:strip (s:cadr x))))

(define (s:expand-if x env menv)
  ;; TODO: check length of (s:cdr x).
  `(if ,@(s:expand-exprs (s:cdr x) env menv)))

(define (s:expand-lambda x env menv)
  (let ((params (s:cadr x))
        (bodies (s:cddr x)))
    (let ((names
           (s:map
            (lambda (param)
              (let ((label (make-label))
                    (name (make-enriched-symbol
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
         ;; The problem is to recognize the bindings created by these defines,
         ;; and add them to env.
         ,(s:expand-lambda-bodies bodies env menv)))))

(define (s:expand-lambda-bodies bodies env menv)
  (define (define? expr)
    (and (s:pair? expr)
         (let ((first (s:car expr)))
           (and (s:identifier? first)
                (enriched-symbol-base=? (syntax-object-expr first) 'define)
                (let ((binding (s:id-binding first env)))
                  (eq? (binding-type binding) 'core))))))
  (let collect-define ((first (s:car bodies)) (rest (s:cdr bodies)) (defs '()))
    (if (s:null? first)
        (s:error bodies "Lambda body can't be empty of just define's."))
    (if (define? first)
        (let ((unnested (s:unnest-define first)))
          (collect-define (s:car rest) (s:cdr rest)
                          (cons (s:cdr unnested) defs)))
        (let ((body
               (if (s:null? rest)
                   first
                   (s:datum->syntax
                    `(,(s:core 'begin) ,first ,@(s:push-down rest))))))
          (if (null? defs)
              (s:expand body env menv)
              (s:expand
               (s:datum->syntax
                `(,(s:core 'letrec*) ,defs ,body))
               env menv))))))


;;;; More expansion utilities.

;;; Enforces a binding.
(define (s:force-bind id expander)
  (enriched-symbol-augment
   id
   (list (cons 'forced-binding (make-binding 'core expander)))))

;;; E.g. (s:core 'lambda) creates an identifier that unambiguously refers to
;;; the core lambda special form.
(define (s:core sym)
  (make-syntax-object sym s:initial-wrap))

;;; Sometimes we want to construct some syntactic form with quasiquoting and
;;; pass it to a specific core expander. However the expander only accepts
;;; syntax objects. This convenient function wraps a list into a syntax object
;;; with no wraps at all.
(define (s:datum->syntax x)
  (make-syntax-object x '()))

;;; Push down marks in a syntactic object by one level and make the outer most
;;; level a list, so it can be used in conjunction with ,@ in quasiquotes.
(define (s:push-down x)
  (s:map (lambda (y) y) x))


;;;; Expanding all flavors of let.

(define (s:expand-let x env menv)
  (if (s:identifier? (s:cadr x))
      (s:expand-named-let x env menv)
      (s:expand-unnamed-let x env menv)))

(define (s:expand-unnamed-let x env menv)
  (let ((bindings (s:cadr x))
        (body (s:cddr x)))
    (let ((vars (s:map s:car bindings))
          (exprs (s:map s:cadr bindings)))
      `(,(s:expand
          (s:datum->syntax
           `(,(s:core 'lambda)
             ,vars ,@(s:push-down body)))
          env menv)
        ,@(s:expand-exprs exprs env menv)))))

(define (s:expand-named-let x env menv)
  (let ((name (s:cadr x))
        (params (s:car (s:cddr x)))
        (body (s:cdr (s:cddr x))))
    (let ((params (s:map s:car params))
          (init-vals (s:map s:cadr params)))
      (let ((proc (s:datum->syntax
                   `(,(s:core 'lambda)
                     ,params ,@(s:push-down body)))))
        (s:expand
         (s:datum->syntax
          `(,(s:core 'letrec)
            ((,name ,proc))
            (,name ,@init-vals)))
         env menv)))))

(define (s:expand-let* x env menv)
  (let ((bindings (s:cadr x))
        (body (s:cddr x)))
    (let ((first (s:car bindings))
          (rest (s:cdr bindings)))
      (s:expand
       (s:datum->syntax
        (if (s:null? rest)
            `(,(s:core 'let) ,bindings ,@(s:push-down body))
            `(,(s:core 'let) (,first)
              (,(s:core 'let*) ,rest ,@(s:push-down body)))))
       env menv))))

;;;; TODO: this is actually letrec*.
(define (s:expand-letrec x env menv)
  (let ((bindings (s:cadr x))
        (body (s:cddr x)))
    (let ((vars (s:map s:car bindings))
          (exprs (s:map s:cadr bindings)))
      (let ((init-void (map (lambda (var) `(,var #f)) vars))
            (set-exprs (map (lambda (var expr)
                              (list (s:core 'set!)
                                    var expr))
                            vars exprs)))
        (s:expand (s:datum->syntax
                   `(,(s:core 'let)
                     ,init-void
                     ,@set-exprs
                     ,@(s:push-down body)))
                  env menv)))))


;;;; Expanding define.

(define (s:unnest-define x)
  (let ((object (s:cadr x))
        (body (s:cddr x)))
    (if (s:identifier? object)
        x
        (s:unnest-define
         (s:datum->syntax
          `(,(s:car x) ,(s:car object) ; (s:car x) is define -- reuse that.
            (,(s:core 'lambda)
             ,(s:cdr object)
             ,@(s:push-down body))))))))

;;; Should only be called in the global environment.
(define (s:expand-define x env menv)
  (let ((x (s:unnest-define x)))
    `(define ,@(s:expand-exprs (s:cdr x) env menv))))


;;;; Expanding cond and case.

(define (s:else? x env)
  (and (s:identifier? x)
       (let ((binding (s:id-binding x env)))
         (eq? (binding-type binding) 'core-aux))))

;;; TODO: support =>
(define (s:expand-cond x env menv)
  (let ((clauses (s:cdr x)))
    (if (s:null? clauses)
        '#f
        ;; `#!unspecific
        (let ((first (s:car clauses)))
          (let ((first-predicate (s:car first))
                (first-consequence
                 ;; Ensure the consequence is a single expression.
                 (if (s:null? (s:cddr first))
                     (s:cadr first)
                     (s:datum->syntax
                      `(,(s:core 'begin)
                        ,@(s:push-down (s:cdr first)))))))
            (if (s:else? first-predicate env)
                (s:expand first-consequence env menv)
                (s:expand
                 (s:datum->syntax
                  `(,(s:core 'if) ,first-predicate
                    ,first-consequence
                    (,(s:core 'cond) ,@(s:push-down (s:cdr clauses)))))
                 env menv)))))))

(define (s:expand-case x env menv)
  (define temp (generate-variable-name 'case-var))
  (define (case-branch branch)
    (let ((datum (s:car branch))
          (body (s:cdr branch)))
      (cond ((s:else? datum env)
             `(,datum ,@(s:push-down body)))
            ((and (s:pair? datum)
                  (s:null? (s:cdr datum)))
             `((eq? ,temp (,(s:core 'quote) ,(s:car datum)))
               ,@(s:push-down body)))
            (else
             `((memq ,temp (,(s:core 'quote) ,datum))
               ,@(s:push-down body))))))
  (let ((expr (s:cadr x))
        (cases (s:cddr x)))
    (s:expand
     (s:datum->syntax
      `(,(s:core 'let) ((,temp ,expr))
        (,(s:core 'cond)
         ,@(s:map case-branch cases))))
     env menv)))


;;;; Expanding quasiquotes.

(define-record-type <qq-component>
  (%make-qq-component type expr)
  qq-component?
  (type %qq-component-type)
  (expr %qq-component-expr))

(define (s:qq-single x) (%make-qq-component 'single x))
(define (s:qq-splicing x) (%make-qq-component 'splicing x))
(define (s:qq-expr x) (%qq-component-expr x))
(define (s:qq-single? x)
  (eq? (%qq-component-type x) 'single))

(define (s:expand-qq-list x d env menv)
  (let ((elems (s:map (lambda (y)
                        (s:expand-qq y d env menv))
                      x)))
    (if (and (list? elems)
             (every s:qq-single? elems))
        `(list ,@(map s:qq-expr elems))
        (let ((append-args
               (let loop ((rest elems))
                 (cond ((null? rest) '())
                       ((pair? rest)
                        (cons (if (s:qq-single? (car rest))
                                  `(list ,(s:qq-expr (car rest)))
                                  (s:qq-expr (car rest)))
                              (loop (cdr rest))))
                       (else (if (s:qq-single? rest)
                                 (list (s:qq-expr rest))
                                 (s:error (s:qq-expr rest)
                                          "inappropriate use of ,@")))))))
          `(append ,@append-args)))))

(define (s:expand-qq x d env menv)
  (cond ((s:pair? x)
         (let ((first (syntax-object-expr (s:car x))))
           (cond ((enriched-symbol-base=? 'quasiquote first)
                  (s:qq-single (s:expand-qq-list x (+ d 1) env menv)))
                 ((enriched-symbol-base=? 'unquote first)
                  (if (= d 0)
                      (s:qq-single (s:expand (s:cadr x) env menv))
                      (s:qq-single (s:expand-qq-list x (- d 1) env menv))))
                 ((enriched-symbol-base=? 'unquote-splicing first)
                  (if (= d 0)
                      (s:qq-splicing (s:expand (s:cadr x) env menv))
                      (s:qq-single (s:expand-qq-list x (- d 1) env menv))))
                 (else
                  (s:qq-single (s:expand-qq-list x d env menv))))))
        ((s:identifier? x)
         (s:qq-single `(quote ,(s:strip x))))
        (else
         (s:qq-single (s:strip x)))))

(define (s:expand-quasiquote x env menv)
  (let ((atom (s:expand-qq (s:cadr x) 0 env menv)))
    (if (s:qq-single? atom)
        (s:qq-expr atom)
        (s:error (s:qq-expr atom) "inappropriate use of ,@"))))


;;;; A few other forms and we are done!

(define (s:expand-begin x env menv)
  `(begin ,@(s:expand-exprs (s:cdr x) env menv)))

(define (s:expand-set! x env menv)
  (let ((dest (s:cadr x))
        (expr (s:car (s:cddr x))))
    (if (s:identifier? dest)
        (let ((binding (s:id-binding dest env)))
          (case (binding-type binding)
            ((lexical global)
             `(set! ,(s:refer-to dest binding)
                    ,(s:expand expr env menv)))
            (else
             (s:error dest "can only set lexical/globally bound variables"))))
        (s:error dest "can only set! identifier"))))

(define (s:expand-and x env menv)
  (let ((conds (s:cdr x)))
    (if (s:null? conds)
        (s:error x "(and ...) must have at least one condition")
        (let ((first (s:car conds))
              (rest (s:cdr conds)))
        (if (s:null? rest)
            (s:expand first env menv)
            (s:expand
             (s:datum->syntax
              `(,(s:core 'if) ,first
                (,(s:core 'and) ,@(s:push-down rest))
                #f))
             env menv))))))

(define (s:expand-or x env menv)
  (let ((conds (s:cdr x)))
    (if (s:null? conds)
        (s:error x "(or ...) must have at least one condition")
        (let ((first (s:car conds))
              (rest (s:cdr conds)))
          (if (s:null? rest)
              (s:expand first env menv)
              (s:expand
               (s:datum->syntax
                `(,(s:core 'if) ,first
                  #t
                  (,(s:core 'or) ,@(s:push-down rest))))
               env menv))))))


;;;; Defining the initial wraps and environment.

(define s:initial-wrap)
(define s:initial-env)

(let ((bindings
       `((quote . ,(make-binding 'core s:expand-quote))
         (quasiquote . ,(make-binding 'core s:expand-quasiquote))
         (if . ,(make-binding 'core s:expand-if))
         (let . ,(make-binding 'core s:expand-let))
         (let* . ,(make-binding 'core s:expand-let*))
         (letrec . ,(make-binding 'core s:expand-letrec))
         (letrec* . ,(make-binding 'core s:expand-letrec))
         (define . ,(make-binding 'core s:expand-define))
         (set! . ,(make-binding 'core s:expand-set!))
         (and . ,(make-binding 'core s:expand-and))
         (or . ,(make-binding 'core s:expand-or))
         (else . ,(make-binding 'core-aux 'else))
         (begin . ,(make-binding 'core s:expand-begin))
         (lambda . ,(make-binding 'core s:expand-lambda))
         (case . ,(make-binding 'core s:expand-case))
         (cond . ,(make-binding 'core s:expand-cond)))))
  (let ((labels (map (lambda (x) (make-label)) bindings)))
    (set! s:initial-wrap
          `(,@(map (lambda (sym label)
                     (make-subst sym (list %the-top-mark) label))
                   (map car bindings)
                   labels)
            ,%the-top-mark))
    (set! s:initial-env
          (map cons labels (map cdr bindings)))))

(define (syntactic-expand x)
  (s:expand (make-syntax-object x s:initial-wrap)
            s:initial-env
            s:initial-env))
