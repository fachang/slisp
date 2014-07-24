#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-interval a b) (cons a b))

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))

;exercise 2.12
(define (make-center-percent c p)
  (let ((i (* c (/ p 100.0))))
    (make-interval (- c i)
                   (+ c i))))

(define (center z)
  (/ (+ (upper-bound z) (lower-bound z)) 
     2))

(define (percent z)
  (let ((c (center z))
        (u (upper-bound z)))
    (* (/ (- u c) c) 
       100.0)))

;test
(percent (make-center-percent 102.5 3.5))
