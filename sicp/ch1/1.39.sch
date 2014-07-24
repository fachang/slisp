#!/usr/bin/env racket
#lang planet neil/sicp

(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (iter (+ i 1))))))
  (iter 1))

;exercise 1.39
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (- (* x x))))
  (cont-frac n (lambda (i) (- (* 2 i) 1)) k))

;test
(tan-cf 3.14 100)
(tan 3.14)

(tan-cf 1.57 100)
(tan 1.57)

(tan-cf 0.0 100)
(tan 0.0)
