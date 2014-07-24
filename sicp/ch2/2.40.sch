#!/usr/bin/env racket
#lang planet neil/sicp

(define (filter p? seq)
  (cond ((null? seq) nil)
        ((p? (car seq)) (cons (car seq) (filter p? (cdr seq))))
        (else (filter p? (cdr seq)))))

(define (accumulate f init seq)
  (if (null? seq)
      init
      (f (car seq)
         (accumulate f init (cdr seq)))))

(define (enum-interval a b)
  (if (> a b)
      nil
      (cons a
            (enum-interval (+ a 1) b))))

(define (flatmap f seq)
  (accumulate append nil (map f seq)))

(define (prime? n)
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
  (= (smallest-divisor n) n))

;exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (y) (list x y))
                  (enum-interval 1 (- x 1))))
           (enum-interval 1 n)))

(define (prime-sum-pairs n)
  (define (prime-sum? p)
    (prime? (+ (car p) (cadr p))))
  (define (make-pair-sum p)
    (list (car p) (cadr p) (+ (car p) (cadr p))))
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(display (prime-sum-pairs 6))
(newline)
