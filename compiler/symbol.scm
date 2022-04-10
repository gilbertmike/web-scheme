;;;; Enriched symbolic metadata.
;;;;
;;;; Symbols in the program can carry various metadata. For example, knowing
;;;; where a symbol is in the source file enables the runtime to emit useful
;;;; error messages. IDE-like programs may be interested in knowing where a
;;;; particular symbol gets defined, and that reference can be stored as
;;;; metadata too. In addition, since syntactic expansion usually rename
;;;; symbols, it may also be a good idea to store the original symbol names.

(define-record-type <enriched-symbol>
  (make-enriched-symbol base metadata)
  enriched-symbol?
  (base %enriched-symbol-base)
  (metadata enriched-symbol-metadata))

(define (enriched-symbol-base esym)
  (if (enriched-symbol? esym)
      (%enriched-symbol-base esym)
      esym))

(define (enriched-symbol-metadata-ref esym key)
  (let ((result (assq key (enriched-symbol-metadata esym))))
    (and result (cdr result))))

(define (->enriched-symbol sym)
  (if (enriched-symbol? sym)
      sym
      (make-enriched-symbol sym '())))

(define (enriched-symbol-augment esym metadata)
  (if (enriched-symbol? esym)
      (make-enriched-symbol (enriched-symbol-base esym)
                            (append metadata
                                    (enriched-symbol-metadata esym)))
      (make-enriched-symbol esym metadata)))

(define (enriched-symbol-base=? a b)
  (eq? (enriched-symbol-base a)
       (enriched-symbol-base b)))

(define enriched-symbol-show-as-symbol #t)

;; Protects our eyes.
(let ((standard-printer
       (standard-print-method enriched-symbol-base)))
  (define-print-method
    enriched-symbol?
    (lambda (esym port)
      (if enriched-symbol-show-as-symbol
          (display (enriched-symbol-base esym) port)
          (standard-printer esym port)))))

