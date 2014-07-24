#!/usr/bin/env racket
#lang planet neil/sicp

(define (square-list1 items)
  (if (null? items)
      nil
      (cons (expt (car items) 2)
            (square-list1 (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

;test
(display (square-list1 (list 1 2 3 4)))
(newline)
(display (square-list2 (list 1 2 3 4)))
(newline)
