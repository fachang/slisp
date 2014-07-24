#!/usr/bin/env racket
#lang planet neil/sicp

(define (accumulate f init seq)
  (if (null? seq)
      init
      (f (car seq)
         (accumulate f init (cdr seq)))))

;exercise 2.36
(define (accumulate-n f init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate f init (map car seqs))
            (accumulate-n f init (map cdr seqs)))))

;test
(define x (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)
                (list 10 11 12)))

(display (accumulate-n + 0 x))
(newline)