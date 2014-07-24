#!/usr/bin/env racket
#lang planet neil/sicp

;part a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (define (identity x) x)
  (define (inc n) (+ n 1))
  (product identity 1 inc n))

(define (compute-pi n)
  (define (inc n) (+ n 1))
  (define (term index)
    (/ (* 4.0 index index)
       (- (* 4.0 index index) 1)))
  (* 2 (product term 1 inc n)))

(compute-pi 10e0)
(compute-pi 10e1)
(compute-pi 10e2)
(compute-pi 10e3)

;part b
(define (product-iter term a next b)
  (define (iter n result)
    (if (> n b)
        result
        (iter (next n) (* result (term n)))))
  (iter a 1))
