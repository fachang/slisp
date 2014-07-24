#!/usr/bin/env racket
#lang planet neil/sicp

(define (tree-map f tree)
  (map (lambda (x)
         (if (pair? x)
             (tree-map f x)
             (f x)))
       tree))

(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))

;test
(display (square-tree (list 1 2 (list 3 4))))
(newline)
