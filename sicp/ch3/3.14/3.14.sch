#!/usr/bin/env racket
#lang planet neil/sicp

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x nil))

(define v '(a b c d))
v

(define w (mystery v))
v
w
