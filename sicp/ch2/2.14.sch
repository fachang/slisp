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

;exercise 2.14
(define A (make-center-percent 47 5))
(define B (make-center-percent 10 5))

(center (div-interval A A))
(percent (div-interval A A))

(center (div-interval A B))
(percent (div-interval A B))

;interval A/A is 1.005 (not 1.0). let A = [a, b], then
;A/A becomes [a, b]/[a, b] = [a, b]*[1/b, 1/a] = [a/b, b/a],
;let a = c-c*t, b= c+c*t, the center is (a/b + b/a) / 2 = ...
;= (1+t^2)/(1-t^2), which is slighly larger than one.
