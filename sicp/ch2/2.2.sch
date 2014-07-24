#!/usr/bin/env racket
#lang planet neil/sicp

;point
(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;segment
(define (make-segment sp ep) (cons sp ep))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (define (selected-average selector x y)
    (/ (+ (selector x) (selector y))
       2))
  (make-point (selected-average x-point (start-segment s) (end-segment s))
              (selected-average y-point (start-segment s) (end-segment s))))

;test
(print-point
 (midpoint-segment
  (make-segment (make-point 0.0 3.0)
                (make-point 2.0 9.5))))
