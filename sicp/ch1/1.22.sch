#!/usr/bin/env racket
#lang planet neil/sicp

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor-iter n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (smallest-divisor-iter n (+ test-divisor 1)))))

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

(define (search-for-primes-iter n count)
  (cond ((and (prime? n) (> count 0))
         (timed-prime-test n)
         (search-for-primes-iter (+ n 2) (- count 1)))
        ((> count 0)
         (search-for-primes-iter (+ n 2) count))))

(define (search-for-primes lower number-of-primes)
  (if (even? lower)
      (search-for-primes-iter (+ lower 3) number-of-primes)
      (search-for-primes-iter (+ lower 2) number-of-primes)))

(search-for-primes 1000 3)    ;runtime: 6
(search-for-primes 10000 3)    ;runtime: 17
(search-for-primes 100000 3)    ;runtime: 51
(search-for-primes 1000000 3)    ;runtime: 158

;√10 ~ 3.16, 158/51 ~ 3.1, the result is consistance with the prediction,
;and the runtime is proportional to the number of steps at least on this
;machine.
