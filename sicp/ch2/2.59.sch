#!/usr/bin/env racket
#lang planet neil/sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))
;test
(define (test set1 set2)
  (display (union-set set1 set2))
  (newline))

(test '(1 2 3) '(3 4 5))
(test '(92 3 1 8 4) '(29 3 74 4 1))
