#!/usr/bin/env racket
#lang planet neil/sicp

(define (equal? a b)
  (or (and (pair? a) 
           (pair? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
      (and (not (pair? a))
           (not (pair? b))
           (eq? a b))))

;test
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
