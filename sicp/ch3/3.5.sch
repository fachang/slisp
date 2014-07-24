#!/usr/bin/env racket
#lang planet neil/sicp

(define (random-in-range low high)
  (let ((range (- high low))
        (K 4294967087))
    (let ((R (random K)))
      (+ low (* range (/ R K))))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (test)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (let ((area (* (- x2 x1) (- y2 y1))))
    (* area (monte-carlo-test trials test))))

(define (monte-carlo-test trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else 
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;test
(define (in-unit-circle? x y)
  (< (+ (* x x) (* y y)) 1))

(define (estimate-pi trials)
  (estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0 trials))

(estimate-pi 10)
(estimate-pi 100)
(estimate-pi 1000)
(estimate-pi 10000)
(estimate-pi 100000)
