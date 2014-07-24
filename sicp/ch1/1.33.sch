#!/usr/bin/env racket
#lang planet neil/sicp

(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filtered-accumulate filter combiner null-value
                                                   term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value
                                   term (next a) next b))))

;part a
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

(define (sum-of-prime a b)
  (define (inc n) (+ n 1))
  (filtered-accumulate prime? + 0 square a inc b))

;test
(define (test a b result)
  (= (sum-of-prime a b) result))

(test 2 6 38)    ;2^2 + 3^2 + 5^2 = 38
(test 2 10 87)    ;2^2 + 3^2 + 5^2 + 7^2 = 87

;part b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-of-relative-prime n)
  (define (relative-prime? a)
    (= (gcd n a) 1))
  (define (identity x) x)
  (define (inc n) (+ n 1))
  (filtered-accumulate relative-prime? * 1 identity 1 inc (- n 1)))

;test
(define (test-2 n result)
  (= (product-of-relative-prime n) result))

(test-2 10 189)
(test-2 11 3628800)    ;actually 10!

