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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;exercise 1.22
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))))

(define (timed-prime-test n)
  (display n)
  (start-prime-test n (runtime))
  (newline))

(timed-prime-test 1009)    ;time: 340
(timed-prime-test 1013)    ;time: 318
(timed-prime-test 1019)    ;time: 359

(timed-prime-test 10007)    ;time: 428
(timed-prime-test 10009)    ;time: 409
(timed-prime-test 10037)    ;time: 435

(timed-prime-test 100003)    ;time: 497
(timed-prime-test 100019)    ;time: 578
(timed-prime-test 100043)    ;time: 457

(timed-prime-test 1000003)    ;time: 775
(timed-prime-test 1000033)    ;time: 738
(timed-prime-test 1000037)    ;time: 727

;log(10e6)/log(10e3) = 2, the average runtime for values near 10e3 is 339,and 
;747 for values near 10e6, the ratio is 747/339 ~ 2.20, which is close to 2
