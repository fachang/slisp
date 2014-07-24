#!/usr/bin/env racket
#lang planet neil/sicp

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= (car set) x) set)
        ((> (car set) x) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;test
(define (test x set)
  (display (adjoin-set x set))
  (newline))

(test 0 '(1 3 5 7))
(test 2 '(1 3 5 7))
(test 4 '(1 3 5 7))
(test 6 '(1 3 5 7))
(test 8 '(1 3 5 7))
