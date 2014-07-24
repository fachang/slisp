#!/usr/bin/env racket
#lang planet neil/sicp

(define (my-below1 bottom-painter up-painter)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-buttom 
           ((transform-painter (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point)
            bottom-painter))
          (paint-up 
           ((transform-painter split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))
            up-painter)))
      (lambda (frame)
        (paint-buttom frame)
        (paint-up frame)))))

(define (my-below2 bottom-painter up-painter)
  (rotate270 (beside (rotate90 up-painter) 
                     (rotate90 bottom-painter))))

;test
(paint (my-below1 einstein einstein))
(paint (my-below2 einstein einstein))
