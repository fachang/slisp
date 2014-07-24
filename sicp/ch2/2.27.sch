#!/usr/bin/env racket
#lang planet neil/sicp

(define (reverse l)
  (if (null? l)
      nil
      (append (reverse (cdr l))
              (list (car l)))))

(define (deep-reverse l)
  (cond ((null? l) nil)
        ((pair? (car l)) (append (deep-reverse (cdr l))
                                 (list (deep-reverse (car l)))))
        (else (append (deep-reverse (cdr l))
                      (list (car l))))))

;test
(define x (list (list 1 2) (list 3 4)))

(display (reverse x))
(newline)
(display (deep-reverse x))
(newline)
