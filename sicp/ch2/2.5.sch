#!/usr/bin/env racket
#lang planet neil/sicp

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (number-divide n d)
  (if (not (= (remainder n d) 
              0))
      0
      (+ 1 (number-divide (/ n d) d))))

(define (car z)
  (number-divide z 2))

(define (cdr z)
  (number-divide z 3))

;test
(define (test-car a b)
  (= a (car (cons a b))))

(define (test-cdr a b)
  (= b (cdr (cons a b))))

(test-car 5 2)
(test-car 3 6)
(test-car 7 9)

(test-cdr 5 2)
(test-cdr 3 6)
(test-cdr 7 9)
