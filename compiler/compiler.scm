;;;; The register-machine compiler, from SICP section 5.5

(define (c:expression? exp) #t)
(define (c:target? reg) (symbol? reg))
(define (c:linkage? link) (symbol? link))

(define compile
  (simple-generic-procedure
   'compile 3
   (lambda (exp target linkage)
     (if (pair? exp)
         (compile-application exp target linkage)
         (error "Unknown expression type -- COMPILE" exp)))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))


;;; Linkage code.

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence
          '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence
          '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))


;;; Simple expressions.

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '() (list target)
    `((assign ,target (const ,exp))))))

(define-generic-procedure-handler
  compile
  (match-args self-evaluating? c:target? c:linkage?)
  compile-self-evaluating)

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '() (list target)
    `((assign ,target (const ,(cadr exp)))))))

(define-generic-procedure-handler
  compile
  (match-args (special-form-predicate 'quote) c:target? c:linkage?)
  compile-quoted)

(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '(env) (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,(enriched-symbol-base exp))
              (reg env))))))

(define-generic-procedure-handler
  compile
  (match-args variable? c:target? c:linkage?)
  compile-variable)

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (make-instruction-sequence
                  '(env val) (list target)
                  `((perform (op set-variable-value!)
                             (const ,var)
                             (reg val)
                             (reg env))
                    (assign ,target (const ok))))))))

(define-generic-procedure-handler
  compile
  (match-args (special-form-predicate 'set!) c:target? c:linkage?)
  compile-assignment)

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (make-instruction-sequence
                  '(env val) (list target)
                  `((perform (op define-variable!)
                             (const ,var)
                             (reg val)
                             (reg env))
                    (assign ,target (const ok))))))))

(define-generic-procedure-handler
  compile
  (match-args (special-form-predicate 'define) c:target? c:linkage?)
  compile-definition)


;;; Conditional expressions.

(define (c:make-label name) (generate-uninterned-symbol name))

(define (compile-if exp target linkage)
  (let ((t-branch (c:make-label 'true-branch))
        (f-branch (c:make-label 'false-branch))
        (after-if (c:make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence
                      '(val) '()
                      `((test (op false?) (reg val))
                        (branch (label ,f-branch))))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))

(define-generic-procedure-handler
  compile
  (match-args (special-form-predicate 'if) c:target? c:linkage?)
  compile-if)


;;; Sequences.

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next)
                  (compile-sequence (rest-exps seq) target linkage))))

(define (compile-begin expr target linkage)
  (compile-sequence (begin-actions expr) target linkage))

(define-generic-procedure-handler
  compile
  (match-args (special-form-predicate 'begin) c:target? c:linkage?)
  compile-begin)

;;; Lambda expressions.

(define (compile-lambda exp target linkage)
  (let ((proc-entry (c:make-label 'entry))
        (after-lambda (c:make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(define-generic-procedure-handler
  compile
  (match-args (special-form-predicate 'lambda) c:target? c:linkage?)
  compile-lambda)

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))


;;; Combinations.

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
                                   '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence
                      '(val argl) '(argl)
                      '((assign argl
                                (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))


;;; Applying procedures.

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (c:make-label 'primitive-branch))
        (compiled-branch (c:make-label 'compiled-branch))
        (after-call (c:make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl)
           (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

;;; Applying compiled procedures.

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (c:make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))


(define all-regs '(env proc val argl continue))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

(define (compile-apply-proc)
  (if #f
      '((assign val (op make-compiled-procedure) (label apply-entry) (reg env))
        (perform (op define-variable!) (const apply) (reg val) (reg env))
        (goto (label after-apply))
        apply-entry
        (assign proc (op car) (reg argl))
        (assign argl (op make-apply-argl) (reg argl))
        (test (op primitive-procedure?) (reg proc))
        (branch (label apply-primitive-branch))
        apply-compiled-branch
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
        apply-primitive-branch
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (goto (reg continue))
        after-apply)
      '()))

(define (compile-program prog)
  (let ((result (compile prog 'val 'halt)))
    `(,@(compile-apply-proc)
      ,@(caddr result))))
