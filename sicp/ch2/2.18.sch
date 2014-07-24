#!/usr/bin/env racket
#lang planet neil/sicp

(define (reverse l)
  (if (null? l)
      nil
      (append (reverse (cdr l))
              (list (car l)))))

;test
(define (test l)
  (display (reverse l))
  (newline))

(test (list 1 2 3 4))
(test (list (list 1 2) (list 3 4)))
