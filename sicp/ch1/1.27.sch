#!/usr/bin/env racket
#lang planet neil/sicp

(define (square x)
  (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) 
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) 
                    m))))

(define (fermat-test n a)
    (= (expmod a n n) a))

(define (test-iter n a)
  (cond ((= a n) false)
        ((fermat-test n a) (fermat-test n (+ a 1)))
        (else false)))

(define (test n)
  (test-iter n 1))

;Carmichael number: number which is not prime but fools fermat-test
(test 561)
(test 1105)
(test 1729)
(test 2465)
(test 2821)
(test 6601)