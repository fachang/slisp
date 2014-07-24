#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v w)
  (map + v w))

(define (sub-vect v w)
  (map - v w))

(define (scale-vect s v)
  (map (lambda (x) (* s x)) v))

;test
(define x (make-vect 5.0 2.5))

(define y (make-vect 2.0 3.5))

(add-vect x y)
(sub-vect x y)
(scale-vect 2 x)
