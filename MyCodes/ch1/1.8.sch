
#lang planet neil/sicp
(define (average x y)
  (/ (+ (/ x (* y y)) (* 2 y)) 3))

(define (improve guess x)
  (average guess (/ x guess)))

(define (cube x)
  (* x x x))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))



(define (cube-iter guess x)
  (if (good-enough? guess x)
          guess
          (cube-iter (improve guess x)
                     x)))

(define (cube-root x)
  (cube-iter 1.0 x))

(cube-root 9);3

