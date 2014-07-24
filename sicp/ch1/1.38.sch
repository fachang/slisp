#!/usr/bin/env racket
#lang planet neil/sicp

(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (iter (+ i 1))))))
  (iter 1))

;exercise 1.38
(define (d i)
  (if (= (remainder (+ i 1) 3) 0)
      (* 2 (+ i 1) (/ 1 3))
      1))

;e - 2 ~ 0.718
(cont-frac (lambda (i) 1.0) d 10)
(cont-frac (lambda (i) 1.0) d 100)


