#!/usr/bin/env racket
#lang planet neil/sicp

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (cons (car set2)
                    (union-set set1 (cdr set2))))))

;test
(define (test set1 set2)
  (display (union-set set1 set2))
  (newline))

(test '(1 2 3) '(4 5 6))
(test '(2 3 4) '(3 4 5))
(test '(1 5 6) '(2 3 4))
