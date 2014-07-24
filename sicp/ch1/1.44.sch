#!/usr/bin/env racket
#lang planet neil/sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;exercise 1.44
(define dx 0.7)

(define (smooth f)
  (lambda (x)
    ( / (+ (f (- x dx)) 
           (f x) 
           (f (+ x dx)))
        3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;test
((n-fold-smooth sin 1) 1.57)
((n-fold-smooth sin 2) 1.57)
((n-fold-smooth sin 3) 1.57)
