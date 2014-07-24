#!/usr/bin/env racket
#lang racket

;to fix segments->painter error
(require (planet neil/sicp:1:7/main))
(require compatibility/mlist)

(define (segments->painter-fix segments-list)
  (segments->painter (mlist->list segments-list)))

;exercise 2.49
(define outline
  (segments->painter-fix
   (list
    (make-segment (make-vect 0.0 0.0) (make-vect 0.0 0.99))
    (make-segment (make-vect 0.0 0.99) (make-vect 0.99 0.99))
    (make-segment (make-vect 0.99 0.99) (make-vect 0.99 0.0))
    (make-segment (make-vect 0.99 0.0) (make-vect 0.0 0.0)))))

(define cross
  (segments->painter-fix
   (list
    (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
    (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0)))))

(define diamond
  (segments->painter-fix
   (list
    (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))
    (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5))
    (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))
    (make-segment (make-vect 0.5 0.0) (make-vect 0.0 0.5)))))

(define wave
  (segments->painter-fix
   (list
    ;legs
    (make-segment (make-vect 0.2 0.0) (make-vect 0.3 0.4))
    (make-segment (make-vect 0.4 0.0) (make-vect 0.5 0.3))
    (make-segment (make-vect 0.5 0.3) (make-vect 0.6 0.0))
    (make-segment (make-vect 0.7 0.4) (make-vect 0.8 0.0))
    ;hands
    (make-segment (make-vect 0.3 0.4) (make-vect 0.25 0.5))
    (make-segment (make-vect 0.25 0.5) (make-vect 0.1 0.4))
    (make-segment (make-vect 0.1 0.4) (make-vect 0.0 0.55))
    (make-segment (make-vect 0.0 0.85) (make-vect 0.15 0.6))
    (make-segment (make-vect 0.15 0.6) (make-vect 0.2 0.65))
    (make-segment (make-vect 0.7 0.4) (make-vect 1.0 0.2))
    (make-segment (make-vect 1.0 0.4) (make-vect 0.65 0.65))
    ;head
    (make-segment (make-vect 0.2 0.65) (make-vect 0.35 0.65))
    (make-segment (make-vect 0.35 0.65) (make-vect 0.25 0.8))
    (make-segment (make-vect 0.25 0.8) (make-vect 0.35 1.0))
    (make-segment (make-vect 0.65 0.65) (make-vect 0.6 0.65))
    (make-segment (make-vect 0.6 0.65) (make-vect 0.7 0.8))
    (make-segment (make-vect 0.7 0.8) (make-vect 0.6 1.0)))))

;test
(paint outline)
(paint cross)
(paint diamond)
(paint wave)
