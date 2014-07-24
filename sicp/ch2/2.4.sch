#!/usr/bin/env racket
#lang planet neil/sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;test
(= (car (cons 1 2)) 1)
(= (cdr (cons 1 2)) 2)

;verify
;(cdr (cons x y))
;(cdr (lambda (m) (m x y)))
;((lambda (m) (m x y)) (lambda (p q) q))
;((lambda (p q) q) x y)
;y
