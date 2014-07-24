#!/usr/bin/env racket
#lang planet neil/sicp

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square x)
  (* x x))

(define (change-rate x y)
  (/ (- y x) x))

(define (good-enough? guess x)
  (< (abs (change-rate guess 
                       (improve guess x))) 
     0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;test
(sqrt 1e12)
(sqrt 1e-12)
