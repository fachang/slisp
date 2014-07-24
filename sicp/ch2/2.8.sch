#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-interval a b) (cons a b))

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))

;exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;test
(define i1 (make-interval 2.5 3.7))

(define i2 (make-interval 1.1 4.2))

(sub-interval i1 i2)    ;[-1.7, 2.6]
