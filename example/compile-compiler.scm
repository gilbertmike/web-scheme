;;; Compile the compiler!

(define (read-exprs input)
  (define (iter sofar cur)
    (if (eof-object? cur)
        (reverse sofar)
        (iter (cons cur sofar) (read input))))
  (iter '() (read input)))

(define (read-exprs-from-file filename)
  (call-with-input-file filename read-exprs))

(call-with-output-file
    "example/compiler.scm"
  (lambda (output)
    (pp
     `(begin
        ,@(read-exprs-from-file "example/compatibility-layer.scm")
        ,@(read-exprs-from-file "compiler/syntax.scm")
        ,@(read-exprs-from-file "compiler/compiler.scm")
        (for-each write-line
                  (caddr (compile (input) 'val 'halt))))
     output)))

(call-with-input-file
    "example/compiler.scm"
  (lambda (input)
    (call-with-output-file
        "example/compiler.rma"
      (lambda (output)
        (compile-to (read input) #f output)))))

;; ((lambda (fact) (set! fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 6)) #f)
