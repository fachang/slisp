#!/usr/bin/env racket
#lang planet neil/sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (intergal f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))

(define (cube x) (* x x x))

;exercise 1.29
(define (intergal-simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (inc n) (+ n 1))
  (define (coefficient index)
    (cond ((or (= index 0) (= index n)) 1.0)
          ((even? index) 2.0)
          (else 4.0)))
  (define (term index) (* (coefficient index) (y index)))
  (* (/ h 3.0)
     (sum term 0 inc n)))

(intergal cube 0 1 0.01)
(intergal-simpson cube 0 1 100)

(intergal cube 0 1 0.001)
(intergal-simpson cube 0 1 1000)
