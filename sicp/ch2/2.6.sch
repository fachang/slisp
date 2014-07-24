#!/usr/bin/env racket
#lang planet neil/sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))

;verify
(define (combine f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (combine f (repeated f (- n 1)))))

(define (church-numeral n)
  ((repeated add-1 n) zero))

(define (church-numeral-number n)
  (define (inc n)
    (+ 1 n))
  ((n inc) 0))

(church-numeral-number (church-numeral 10))
(church-numeral-number (add (church-numeral 100) 
                            (church-numeral 125)))
