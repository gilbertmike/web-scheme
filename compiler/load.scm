(define (load-here path)
  (load (merge-pathnames (directory-pathname (current-load-pathname))
                         (->pathname path))))

;; Load "../common/load.scm".
(let ((path (current-load-pathname)))
  (let ((directory (pathname-directory path)))
    (let ((common-directory
           (append (drop-right directory 1)
                   (list "common"))))
      (load (pathname-new-directory path common-directory)))))

(load-here "symbol.scm")
(load-here "syntax.scm")
(load-here "expand.scm")
(load-here "cps-transform.scm")
(load-here "compiler.scm")

(define (compile-to program cps? port)
  (let* ((expanded (syntactic-expand program))
         (cps-ed (if cps? (cps-transform expanded) expanded))
         (compiled (compile-program cps-ed)))
    (for-each (lambda (instr)
                (pp instr port))
              compiled)))

(define (compile-to-file filename prog cps?)
  (call-with-output-file
      filename
    (lambda (file) (compile-to prog cps? file))))

#|
(compile-to-file
 "example/cps-generator.rma"
 '(begin
    (define (for-each f lst)
      (if (null? lst) 'ok (begin (f (car lst)) (for-each f (cdr lst)))))
    (define (generate-one-element-at-a-time lst)
      (define (control-state return)
        (for-each
         (lambda (element)
           (set! return (call/cc
                         (lambda (resume-here)
                           (set! control-state resume-here)
                           (return element)))))
         lst)
        (return 'you-fell-off-the-end))
      (define (generator)
        (call/cc control-state))
      generator)
    (define generate-digit
      (generate-one-element-at-a-time '(0 1 2)))
    (pp (generate-digit))
    (pp (generate-digit))
    (pp (generate-digit))
    (pp (generate-digit)))
 #t)

(compile-to-file
 "example/apply-test.scm"
 '(begin
    (pp (apply + '(1 2 3)))
    (pp (apply (lambda x x) '(1 2 3)))
    (pp (apply (lambda x x) '()))
    (pp (apply (lambda x x) 1 2 '(1 2 3))))
  #f)

(compile-to-file
 "example/fib.rma"
 '(begin
    (define (fib n)
      (cond ((= n 0) 1)
            ((= n 1) 1)
            (else (+ (fib (- n 1)) (fib (- n 2))))))
    (fib (input)))
 #f)

(compile-to-file
 "example/fib-cps.rma"
 '(begin
    (define (fib n)
      (cond ((= n 0) 1)
            ((= n 1) 1)
            (else (+ (fib (- n 1)) (fib (- n 2))))))
    (fib (input)))
 #t)

(compile-to-file
 "example/fact.rma"
 '(letrec ((fact
            (lambda (n)
              (if (= n 0) 1 (* n (fact (- n 1)))))))
    (fact (input)))
 #f)

(compile-to-file
 "example/fact-cps.rma"
 '(letrec ((fact
            (lambda (n)
              (if (= n 0) 1 (* n (fact (- n 1)))))))
    (fact (input)))
 #t)
|#
