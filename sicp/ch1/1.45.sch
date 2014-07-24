#!/usr/bin/env racket
#lang planet neil/sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;exercise 1.45
;with some experiments, the times of average-damp and the maximum n-th has the 
;relation:
;
; times    n-th
;     1    2, 3
;     2    2, 3, ..., 7
;     3    2, 3, ..., 15
;     4    2, 3, ..., 63
;
;thus the times can be found by rounding log(2, n-th)

(define (nth-root x n)
  (let ((a (floor (/ (log n) (log 2)))))
    (fixed-point ((repeated average-damp a)
                  (lambda (y) (/ x (expt y (- n 1)))))
                 1.0)))

;test
(nth-root 10e10 100)
(nth-root 10e10 1000)
(nth-root 10e10 10000)
