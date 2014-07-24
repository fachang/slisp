#!/usr/bin/env racket
#lang planet neil/sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))
;time complexity: Θ(n)

(define (adjoin-set x set)
  (cons x set))
;time complexity: Θ(1)

(define (union-set set1 set2)
  (append set1 set2))
;time-complexity: Θ(n)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) nil)
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;time-complextiy: Θ(n^2)

;test
(define (test-adjoin x set)
  (display (adjoin-set x set))
  (newline))

(define (test-union set1 set2)
  (display (union-set set1 set2))
  (newline))

(define (test-intersection set1 set2)
  (display (intersection-set set1 set2))
  (newline))

(test-adjoin 1 '(1 2 1 3 4 4))
(test-union '(1 2 1) '(1 2 1 3 4 4))
(test-intersection '(1 2 3 4 5) '(1 2 1 3 4 4))

;in case which 1) memory is large enough, and 2) most of the set operations are
;adjoin-set and union-set, this duplicate representation would be preferred.
