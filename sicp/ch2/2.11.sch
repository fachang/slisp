#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-interval a b) (cons a b))

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))

(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xh (upper-bound x))
        (yl (lower-bound y))
        (yh (upper-bound y)))
    (define (p v)
      (>= v 0))
    (define (n v)
      (< v 0))
    (define (sign? xl-sign? xh-sign? yl-sign? yh-sign?)
      (and (xl-sign? xl) 
           (xh-sign? xh) 
           (yl-sign? yl) 
           (yh-sign? yh)))
    (cond ((sign? n n n n) 
           (make-interval (* xh yh) (* xl yl)))
          ((sign? n n n p) 
           (make-interval (* xl yh) (* xl yl)))
          ((sign? n n p p)
           (make-interval (* xl yh) (* xh yl)))
          ((sign? n p n n)
           (make-interval (* xh yl) (* xl yl)))
          ((sign? n p n p)
           (make-interval (min (* xh yl) (* xl yh))
                          (max (* xh yh) (* xl yl))))
          ((sign? n p p p)
           (make-interval (* xl yh) (* xh yh)))
          ((sign? p p n n)
           (make-interval (* xh yl) (* xl yh)))
          ((sign? p p n p)
           (make-interval (* xh yl) (* xh yh)))
          ((sign? p p p p)
           (make-interval (* xl yl) (* xh yh))))))

;test
(define i1 (make-interval -6 -1))

(define i2 (make-interval -1 7))

(define i3 (make-interval 3 4))

(mul-interval i1 i1)
(mul-interval i1 i2)
(mul-interval i1 i3)
(mul-interval i2 i1)
(mul-interval i2 i2)
(mul-interval i2 i3)
(mul-interval i3 i1)
(mul-interval i3 i2)
(mul-interval i3 i3)
