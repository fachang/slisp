#!/usr/bin/env racket
#lang planet neil/sicp

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor-iter n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (smallest-divisor-iter n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (smallest-divisor-iter n 2))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
