#lang planet neil/sicp
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(define (f2-iter a b c count)
  (if (= count 2)
      c
      (f2-iter b 
               c 
               (+ c (* 2 b) (* 3 a)) 
               (- count 1))))

(define (f2 n)
  (if (< n 3)
      n
      (f2-iter 0 1 2 n)))

;test
(define (test n)
  (= (f n) (f2 n)))

(test 10)
(test 11)
(test 12)
