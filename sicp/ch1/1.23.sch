#!/usr/bin/env racket
#lang planet neil/sicp

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (smallest-divisor-iter n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (smallest-divisor-iter n (next test-divisor)))))

(define (smallest-divisor n)
  (smallest-divisor-iter n 2))

(define (prime? n)
  (= (smallest-divisor n) n))

;exercise 1.22
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (timed-prime-test n)
  (display n)
  (start-prime-test n (runtime))
  (newline))

(timed-prime-test 1009)    ;ratio: 7/5 = 1.4
(timed-prime-test 1013)    ;ratio: 6/5 = 1.2
(timed-prime-test 1019)    ;ratio: 6/5 = 1.2

(timed-prime-test 10007)    ;ratio: 17/12 = 1.42
(timed-prime-test 10009)    ;ratio: 18/12 = 1.5
(timed-prime-test 10037)    ;ratio: 17/12 = 1.42

(timed-prime-test 100003)    ;ratio: 55/34 = 1.62
(timed-prime-test 100019)    ;ratio: 52/34 = 1.53
(timed-prime-test 100043)    ;ratio: 52/34 = 1.53

(timed-prime-test 1000003)    ;ratio: 171/103 = 1.66
(timed-prime-test 1000033)    ;ratio: 160/103 = 1.55
(timed-prime-test 1000037)    ;ratio: 160/103 = 1.55

;the ratio of speeds of the two algorithm is not 2. although
;(next test-divisor) reduce half of the amount of test divisors, the if
;expression in the new procedure introduce extra overhead.
