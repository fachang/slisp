#!/usr/bin/env racket
#lang planet neil/sicp

(define (count-pairs x)
  (let ((traversed nil))
    (define (aux x)
      (if (or (not (pair? x)) (memq x traversed))
          0
          (begin (set! traversed (cons x traversed))
               (+ (aux (car x))
                  (aux (cdr x))
                  1))))
    (aux x)))

(define a1 (cons 0 0))
(define a2 (cons a1 0))
(define a3 (cons a2 0))
(count-pairs a3)    ;3 -> 3

(define b1 (cons 0 0))
(define b2 (cons b1 b1))
(define b3 (cons b2 0))
(count-pairs b3)    ;4 -> 3

(define c1 (cons 0 0))
(define c2 (cons c1 c1))
(define c3 (cons c2 c2))
(count-pairs c3)    ;7 -> 3

(define d1 (cons 0 0))
(define d2 (cons d1 0))
(define d3 (cons d2 0))
(set-cdr! d1 d3)
(count-pairs d3)    ;infinite loop -> 3
