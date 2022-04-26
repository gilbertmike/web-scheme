;;; Compile the compiler!

(call-with-input-file
    "example/compiler.scm"
  (lambda (input)
    (call-with-output-file
        "example/compiler.rma"
      (lambda (output)
        (compile-to (read input) #f output)))))
