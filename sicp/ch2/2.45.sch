#!/usr/bin/env racket
#lang planet neil/sicp

(define (split pre post)
  (define (split-painter painter n)
    (if (= n 0)
        painter
        (let ((rest (split-painter painter (- n 1))))
          (pre painter (post rest rest)))))
  split-painter)

(define up-split (split below beside))
(define right-split (split beside below))

;test
(paint (up-split einstein 2))
(paint (right-split einstein 2))
