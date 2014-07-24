#!/usr/bin/env racket
#lang racket

;to fix segments->painter error
(require (planet neil/sicp:1:7/main))
(require compatibility/mlist)

(define (segments->painter-fix segments-list)
  (segments->painter (mlist->list segments-list)))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (top-right (corner-split painter (- n 1))))
          (below (beside painter bottom-right)
                 (beside top-left top-right))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;exercise 2.49
;exercise 2.52
;part a
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
    (make-segment (make-vect 0.7 0.8) (make-vect 0.6 1.0))
    ;smile
    (make-segment (make-vect 0.35 0.9) (make-vect 0.4 0.9))
    (make-segment (make-vect 0.3 0.75) (make-vect 0.45 0.8)))))

;test
(paint wave)

;part b
(define (corner-split-modified painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (top-right (corner-split-modified painter (- n 1))))
          (below (beside painter bottom-right)
                 (beside top-left top-right))))))

;test
(paint (corner-split-modified wave 2))

;part c
(define (square-limit-modified painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))

;test
(paint (square-limit-modified einstein 2))
