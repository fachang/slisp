#lang planet neil/sicp
(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(define (expt-iter product b n)
  (cond ((= n 0) product)
        ((even? n) (expt-iter product (square b) (/ n 2)))
        (else (expt-iter (* product b) b (- n 1)))))

(define (fast-expt b n)
  (expt-iter 1 b n))

(define (test b n)
  (= (fast-expt b n) (expt b n))
     )
(fast-expt  2 10)
(test 2 10)
;(expt-iter 2 4)
;(square 2)
;(even? 5)