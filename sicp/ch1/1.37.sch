#!/usr/bin/env racket
#lang planet neil/sicp

;part a
(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (iter (+ i 1))))))
  (iter 1))

; 1/φ ~ 0.618
(define (test n)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             n))

(test 10)
(test 11);    ~ 0.618

;part b
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n k) (+ (d k) result)))))
  (iter (- k 1) (/ (n k) (d k))))
