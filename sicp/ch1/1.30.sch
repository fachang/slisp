#!/usr/bin/env racket
#lang planet neil/sicp

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;test
(define (sum-integers a b)
  (define (identity x) x)
  (define (inc n) (+ n 1))
  (sum identity a inc b))

(= (sum-integers 1 10) 55)
