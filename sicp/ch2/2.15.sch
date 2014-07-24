#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-interval a b) (cons a b))

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;exercise 2.10
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (<= 0 (upper-bound y)))
      (error "divider interval includes zero.")
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

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

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;exercise 2.15
(define A (make-center-percent 47 5))
(define B (make-center-percent 10 5))

(percent (par1 A B))
(percent (par2 A B))

;the tolerance of the second version is smaller. the reason is every interval
;operation will increase the overall tolerance, except one operand is certain
;(like (make-interval 1 1) in par2). reducing repeated terms also reduces the 
;number of interval operations, thus the final result will be "better".
