#lang planet neil/sicp
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))



(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9);3

(define (new-if predicate then-clause else-clause)
  (cond  (predicate then-clause)
         (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x)
                     x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

;(new-sqrt 9)

;;;correct version by using cond
(define (new-sqrt-iter-2 guess x)
  (cond ((good-enough? guess x) guess)
        ((new-sqrt-iter-2 (improve guess x) x))))

(define (new-sqrt-2 x)
  (new-sqrt-iter-2 1.0 x))

(new-sqrt-2 9)