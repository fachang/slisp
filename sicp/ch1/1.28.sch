#!/usr/bin/env racket
#lang planet neil/sicp

(define (checked-square x m)
  (if (and (not (or (= x 1) 
                    (= x (- m 1))))
           (= (remainder (* x x) m) 1))
      0
      (* x x)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (checked-square (expmod base (/ exp 2) m) m) 
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) 
                    m))))

(define (miller-rabin-test n a)
    (= (expmod a (- n 1) n) 1))

(define (test-iter n a)
  (cond ((= a n) false)
        ((miller-rabin-test n a) (miller-rabin-test n (+ a 1)))
        (else false)))

(define (test n)
  (test-iter n 1))

;test
(test 1000)    ;false
(test 1009)    ;true
(test 1013)    ;true

;Carmichael number: number which is not prime but fools fermat-test
(test 561)
(test 1105)
(test 1729)
(test 2465)
(test 2821)
(test 6601)