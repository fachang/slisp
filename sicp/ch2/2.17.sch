#!/usr/bin/env racket
#lang planet neil/sicp

(define (last-pair l)
  (if (null? (cdr l))
      l
      (last-pair (cdr l))))

;test
(define (test l)
  (display (last-pair l))
  (newline))

(test (list 1 2 3))
(test (list 1))
(test (list (list 1 2) (list 3 4)))
