#| -*-Scheme-*-

Copyright (C) 2019, 2020, 2021 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

;;;; Collections

(define (make-hash-table-store make-table)
  (let ((table (make-table)))
    (define (get-keys)
      (hash-table-keys table))
    (define (has? key)
      (hash-table-exists? table key))
    (define (get key)
      (hash-table-ref table key))
    (define (put! key metadata)
      (hash-table-set! table key metadata))

    (lambda (operator)
      (case operator
        ((get-keys) get-keys)
        ((has?) has?)
        ((get) get)
        ((put!) put!)
        (else (error "Unknown operator:" operator))))))

(define (make-metadata-association)
  (let* ((store
          (make-hash-table-store make-key-weak-eqv-hash-table))
         (base-has? (store 'has?))
         (base-get (store 'get))
         (base-put! (store 'put!)))

    (define (put! key metadata)
      (if (base-has? key)
          (let ((metadata* (base-get key)))
            (if (not (eqv? metadata* metadata))
                (error "Can't change metadata for:"
                       key metadata metadata*))))
      (base-put! key metadata))

    (lambda (operator)
      (case operator
        ((put!) put!)
        (else (store operator))))))
