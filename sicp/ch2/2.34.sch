#!/usr/bin/env racket
#lang planet neil/sicp

(define (accumulate f init seq)
  (if (null? seq)
      init
      (f (car seq) 
         (accumulate f init (cdr seq)))))

;exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeficient higher-terms)
                (+ this-coeficient (* higher-terms x)))
              0
              coefficient-sequence))

;test
(horner-eval 2.0 (list 1 3 0 5 0 1))    ;79
