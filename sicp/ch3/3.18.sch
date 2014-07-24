#!/usr/bin/env racket
#lang planet neil/sicp

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (cycle? x)
  (let ((traversed nil))
    (define (aux x)
      (cond ((not (pair? x)) false)
            ((memq x traversed) true)
            (else (set! traversed (cons x traversed))
                  (aux (cdr x)))))
    (aux x)))

;test
(cycle? '(a b c))
(cycle? (make-cycle '(a b c)))
