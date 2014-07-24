#!/usr/bin/env racket
#lang planet neil/sicp

(define (accumulate f init seq)
  (if (null? seq)
      init
      (f (car seq) 
         (accumulate f init (cdr seq)))))

;exercise 2.35
(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (x)
                     (if (not (pair? x))
                         1
                         (count-leaves x))) 
                   tree)))

;test
(count-leaves (list (list 1 2) 
                    3 
                    (list 4 (list 5 6))))
