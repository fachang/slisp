#!/usr/bin/env racket
#lang planet neil/sicp

(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

;test
(define (test x)
  (display (fringe x))
  (newline))

(define x (list (list 1 2) (list 3 4)))

(test x)
(test (list x x))
