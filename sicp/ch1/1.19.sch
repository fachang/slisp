#!/usr/bin/env racket
#lang planet neil/sicp

(define (fast-fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fast-fib-iter a
                        b
                        (+ (* p p) (* q q))    ;compute p'
                        (+ (* q q) (* 2 p q))    ;compute q'
                        (/ count 2)))
        (else (fast-fib-iter (+ (* b q) (* a q) (* a p))
                             (+ (* b p) (* a q))
                             p
                             q
                             (- count 1)))))

(define (fast-fib n)
  (fast-fib-iter 1 0 0 1 n))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

;test
(define (test n)
  (= (fast-fib n) (fib n)))

(test 10)
(test 15)
(test 20)
