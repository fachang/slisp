#lang planet neil/sicp
;recursive
(define (f-recur n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f-recur(- n 1)) (* (f-recur(- n 2)) 2) (* (f-recur(- n 3)) 3)))))

(f-recur 6)
(define (iter a b c count)
  (if (= count 2)
      c
      (iter b c (+ c (* b 2) (* a 3)) (- count 1))))

(define (f-iter n)
     (iter 0 1 2 n))

(f-iter 6)