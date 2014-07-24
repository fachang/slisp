#!/usr/bin/env racket
#lang planet neil/sicp

;exercise 2.46
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

;exercise 2.48
(define (make-segment start end)
  (list start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cadr seg))

;test
(define s (make-segment (make-vect 0.0 0.0) 
                        (make-vect 1.0 1.0)))

(start-segment s)
(end-segment s)
