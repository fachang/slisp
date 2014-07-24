#!/usr/bin/env racket
#lang planet neil/sicp

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt x)
  ((iterative-improve (lambda (guess)
                        (< (abs (- (* guess guess) x)) 
                           0.001))
                      (lambda (guess)
                        (/ (+ guess (/ x guess)) 
                           2)))
  x))

;test
(sqrt 25.0)    ;5

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess)
                        (< (abs (- (f guess) guess))
                           0.00001))
                      f)
   first-guess))

;test
(fixed-point cos 1.0)    ;0.739
