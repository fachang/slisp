#!/usr/bin/env racket
#lang planet neil/sicp

(define (accumulate f init seq)
  (if (null? seq)
      init
      (f (car seq)
         (accumulate f init (cdr seq)))))

;exercise 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))
;test
(display (map (lambda (x) (* x x)) (list 1 2 3 4)))
(newline)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;test
(display (append (list 1 2 3 4) (list 5 6 7 8)))
(newline)

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;test
(display (length (list 1 2 3 4)))
(newline)
