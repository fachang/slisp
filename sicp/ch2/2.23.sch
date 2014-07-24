#!/usr/bin/env racket
#lang planet neil/sicp

(define (for-each f l)
  (cond ((not (null? l))
         (f (car l))
         (for-each f (cdr l)))))

;test
(for-each (lambda (x) (display x) (newline))
          (list 57 321 88))
