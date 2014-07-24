#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-accumulator init)
  (lambda (x)
    (set! init (+ init x))
    init))

;test
(define A (make-accumulator 5))

(A 10)
(A 10)
