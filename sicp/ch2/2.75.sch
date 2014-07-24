#!/usr/bin/env racket
#lang planet neil/sicp

(define (apply-generic op arg) (arg op))

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;test
(define x (make-from-mag-ang 10 0.785))

(apply-generic 'real-part x)
(apply-generic 'imag-part x)
(apply-generic 'magnitude x)
(apply-generic 'angle x)
(apply-generic 'access x)
