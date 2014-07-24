#!/usr/bin/env racket
#lang planet neil/sicp

(define (my-flip-horiz painter)
  ((transform-painter (make-vect 1.0 0.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0)) 
   painter))

(define (my-rotate180 painter)
  ((transform-painter (make-vect 1.0 1.0)
                      (make-vect 0.0 1.0)
                      (make-vect 1.0 0.0))
   painter))

(define (my-rotate270 painter)
  ((transform-painter (make-vect 0.0 1.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
   painter))

;test
(paint einstein)
(paint (my-flip-horiz einstein))
(paint (my-rotate180 einstein))
(paint (my-rotate270 einstein))
