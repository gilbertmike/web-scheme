(define (list . lst) lst)
(define (length lst)
  (define (iter rest ans)
    (if (null? rest)
        ans
        (iter (cdr rest) (+ ans 1))))
  (iter lst 0))
(define (map f lst)
  (if (null? lst) '() (cons (f (car lst)) (map f (cdr lst)))))
(define (for-each f lst)
  (if (null? lst) 'ok (begin (f (car lst)) (for-each f (cdr lst)))))
(define (append-two a b)
  (if (null? a) b (cons (car a) (append-two (cdr a) b))))
(define (append* lsts)
  (cond ((null? lsts) '())
        ((null? (cdr lsts)) (car lsts))
        (else (append-two (car lsts)
                          (append* (cdr lsts))))))
(define (append . lsts) (append* lsts))
(define (memq item items)
  (cond ((null? items) #f)
        ((eq? (car items) item) items)
        (else (memq item (cdr items)))))
(define (reverse lst)
  (define (iter sofar rest)
    (if (null? rest)
        sofar
        (iter (cons (car rest) sofar) (cdr rest))))
  (iter '() lst))

(define (caar pair) (car (car pair)))
(define (caaar pair) (car (car (car pair))))
(define (caaaar pair) (car (car (car (car pair)))))
(define (cdaaar pair) (cdr (car (car (car pair)))))
(define (cdaar pair) (cdr (car (car pair))))
(define (cadaar pair) (car (cdr (car (car pair)))))
(define (cddaar pair) (cdr (cdr (car (car pair)))))
(define (cdar pair) (cdr (car pair)))
(define (cadar pair) (car (cdr (car pair))))
(define (caadar pair) (car (car (cdr (car pair)))))
(define (cdadar pair) (cdr (car (cdr (car pair)))))
(define (cddar pair) (cdr (cdr (car pair))))
(define (caddar pair) (car (cdr (cdr (car pair)))))
(define (cdddar pair) (cdr (cdr (cdr (car pair)))))
(define (cadr pair) (car (cdr pair)))
(define (caadr pair) (car (car (cdr pair))))
(define (caaadr pair) (car (car (car (cdr pair)))))
(define (cdaadr pair) (cdr (car (car (cdr pair)))))
(define (cdadr pair) (cdr (car (cdr pair))))
(define (cadadr pair) (car (cdr (car (cdr pair)))))
(define (cddadr pair) (cdr (cdr (car (cdr pair)))))
(define (cddr pair) (cdr (cdr pair)))
(define (caddr pair) (car (cdr (cdr pair))))
(define (caaddr pair) (car (car (cdr (cdr pair)))))
(define (cdaddr pair) (cdr (car (cdr (cdr pair)))))
(define (cdddr pair) (cdr (cdr (cdr pair))))
(define (cadddr pair) (car (cdr (cdr (cdr pair)))))
(define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))

#|
(define (generate-c*r-definitions n)
  (define (generate-definition desc)
    `(define (,(apply symbol-append 'c (append desc '(r))) pair)
       ,(let loop ((rest desc))
          (if (null? rest)
              'pair
              `(,(symbol-append 'c (car rest) 'r)
                ,(loop (cdr rest)))))))
  (define (generate-definitions sofar)
    (cond ((<= (length sofar) 1)
           (append (generate-definitions (cons 'a sofar))
                   (generate-definitions (cons 'd sofar))))
          ((> (length sofar) n) '())
          (else
           (cons (generate-definition sofar)
                 (append (generate-definitions (cons 'a sofar))
                         (generate-definitions (cons 'd sofar)))))))
  (generate-definitions '()))
(for-each write-line (generate-c*r-definitions 4))
|#

(define (char? x) #f)

(define *extend-generic-procedure-message* (list 'extend))

(define (match-args . predicates)
  (lambda (args)
    (let loop ((rest args) (rest-p predicates))
      (or (null? rest)
          (and ((car rest-p) (car rest))
               (loop (cdr rest) (cdr rest-p)))))))

(define (simple-generic-procedure name arity default)
  (let ((handlers '()))
    (lambda args
      (let ((len (length args)))
        (cond ((and (= len 3)
                    (eq? (car args) *extend-generic-procedure-message*))
               (let ((predicate (cadr args))
                     (handler (caddr args)))
                 (set! handlers (cons (cons predicate handler)
                                      handlers))))
              ((not (= len arity))
               (error name "Arity mismatch!"))
              (else
               (let loop ((to-check handlers))
                 (if (null? to-check)
                     (apply default args)
                     (if ((caar to-check) args)
                         (apply (cdar to-check) args)
                         (loop (cdr to-check)))))))))))

(define (define-generic-procedure-handler procedure predicate handler)
  (procedure *extend-generic-procedure-message* predicate handler))

;;; No, no enriched symbol for the time being.
(define (enriched-symbol? sym) #f)
(define (enriched-symbol-base sym) sym)
(define (enriched-symbol-base=? a b) (eq? a b))
