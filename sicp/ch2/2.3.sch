#!/usr/bin/env racket
#lang planet neil/sicp

;point
(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

;segment
(define (make-segment sp ep) (cons sp ep))

(define (start-segment s) (car s))

(define (end-segment s) (cdr s))

;rectangle - ver1
;(define (make-rectangle left-upper-point width height)
;  (cons left-upper-point (cons width height)))
;
;(define (width-rectangle r) (car (cdr r)))
;
;(define (height-rectangle r) (cdr (cdr r)))

;rectangle - ver2
(define (make-rectangle lu-point rl-point)
  (cons lu-point rl-point))

(define (width-rectangle r)
  (- (x-point (cdr r)) (x-point (car r))))

(define (height-rectangle r) 
  (- (y-point (car r)) (y-point (cdr r))))

;operator of rectangle
(define (perimeter-rectangle r)
  (let ((w (width-rectangle r)) 
        (h (height-rectangle r)))
    (* (+ w h) 2)))

(define (area-rectangle r)
  (let ((w (width-rectangle r)) 
        (h (height-rectangle r)))
    (* w h)))

;test
;(define rect (make-rectangle (make-point 10.0 10.0) 2.0 3.0))
(define rect (make-rectangle (make-point 10.0 10.0) (make-point 12.0 7.0)))

(perimeter-rectangle rect)

(area-rectangle rect)
