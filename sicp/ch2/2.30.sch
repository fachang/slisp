#!/usr/bin/env racket
#lang planet neil/sicp

(define (square x) (* x x))

(define (square-tree1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree2 x)
             (square x)))
       tree))

;test
(display (square-tree1 (list 1 2 (list 3 4))))
(newline)
(display (square-tree2 (list 1 2 (list 3 4))))
(newline)
