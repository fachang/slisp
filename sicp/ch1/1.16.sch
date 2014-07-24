#!/usr/bin/env racket
#lang planet neil/sicp

(define (square x)
  (* x x))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

;test
(define (test a b)
  (= (fast-expt a b) (expt a b)))

(test 25 77)
(test 60 19)
(test 78 20)
