#!/usr/bin/env racket
#lang planet neil/sicp

(define (double x)
  (* 2 x))

(define (half x)
  (/ x 2))

(define (multiply a b)
  (cond ((= b 0) 0)
        ((even? b) (multiply (double a) (half b)))
        (else (+ a (multiply a (- b 1))))))

;test
(define (test a b)
  (= (multiply a b) (* a b)))

(test 1039 9382)
(test 2092 19872)
(test 2017 42831)
