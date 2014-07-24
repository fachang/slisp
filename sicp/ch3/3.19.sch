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
  (define (cdr-n n x)
    (cond ((null? x) nil)
          ((= n 0) x)
          (else (cdr-n (- n 1) (cdr x)))))
  (define (aux x n)
    (let ((next-x (cdr-n n x)))
      (cond ((null? next-x) false)
            ((eq? x next-x) true)
            (else (aux next-x (+ n 1))))))
  (aux x 1))

;test
(cycle? '(a b c))
(cycle? (make-cycle '(a b c)))
