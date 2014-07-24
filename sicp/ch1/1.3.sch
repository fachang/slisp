#!/usr/bin/env racket
#lang planet neil/sicp

(define (square n) (* n n))

(define (f a b c)
  (cond ((and (> b a) (> c a)) (+ (square b) (square c)))
        ((and (> a b) (> c b)) (+ (square a) (square c)))
        (else (+ (square b) (square c)))))

;test
(= (f 1 2 2) 8)
(= (f 1 1 2) 5)
