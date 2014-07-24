#!/usr/bin/env racket
#lang planet neil/sicp

(define (fold-right f init seq)
  (if (null? seq)
      init
      (f (car seq)
         (fold-right f init (cdr seq)))))

(define (fold-left f init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (f result (car rest))
              (cdr rest))))
  (iter init seq))

;exercise 2.39
(define (reverse1 seq)
  (fold-right (lambda (x rest) (append rest (list x))) nil seq))

(define (reverse2 seq)
  (fold-left (lambda (rest x) (cons x rest)) nil seq))

(display (reverse1 (list 1 2 3 4)))
(newline)
(display (reverse2 (list 1 2 3 4)))
(newline)
