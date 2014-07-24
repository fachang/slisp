#!/usr/bin/env racket
#lang planet neil/sicp

(define (double x)
  (* 2 x))

(define (half x)
  (/ x 2))

(define (multiply-iter a b c)
  (cond ((= b 0) c)
        ((even? b) (multiply-iter (double a) (half b) c))
        (else (multiply-iter a (- b 1) (+ c a)))))

(define (multiply a b)
  (multiply-iter a b 0))

;test
(define (test a b)
  (= (multiply a b) (* a b)))

(test 1039 9382)
(test 2092 19872)
(test 2017 42831)
