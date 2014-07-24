#!/usr/bin/env racket
#lang planet neil/sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

;exercise 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (square x)
  (* x x))

((repeated square 2) 5)
