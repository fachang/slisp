#lang planet neil/sicp
(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))
(define (even? n)
  (= (remainder n 2) 0))

(define (multiply a b)
  (cond ((= b 0) 0)
        ((even? b) (multiply (double a) (halve b)))
        (else (+ (multiply a (- b 1)) a))))

(define (test a b)
  (= (multiply a b) (* a b)))

;(multiply 5 6)
(test 5 6)
(test 5656565 44446)
(test 5515789 783123)
(test 4545452 6)
(test 5 78987897897)