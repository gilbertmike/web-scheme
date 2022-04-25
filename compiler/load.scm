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
         (compiled (compile cps-ed 'val 'halt)))
    (for-each (lambda (instr)
                (pp instr port))
              (caddr compiled))))

#|
(compile-to
 '(letrec ((fact
            (lambda (n)
              (if (= n 0) 1 (* n (fact (- n 1)))))))
    (fact 4))
 #f (current-output-port))
|#

