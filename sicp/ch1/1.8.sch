#!/usr/bin/env racket
#lang planet neil/sicp

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (improve guess x)
  (/ (+ (/ x 
           (square guess))
        (* 2 guess))
     3))

(define (change-rate x y)
  (/ (- y x) x))

(define (good-enough? guess x)
  (< (abs (change-rate guess (improve guess x)))
     0.001))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x)
                      x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))

;test
(cube-root (cube 1e2))
(cube-root (cube 1e3))
(cube-root (cube 1e4))
