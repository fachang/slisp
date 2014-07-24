#!/usr/bin/env racket
#lang planet neil/sicp

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;test
(paint (up-split einstein 2))
