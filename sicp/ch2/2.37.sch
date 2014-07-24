#!/usr/bin/env racket
#lang planet neil/sicp

(define (accumulate f init seq)
  (if (null? seq)
      init
      (f (car seq)
         (accumulate f init (cdr seq)))))

(define (accumulate-n f init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate f init (map car seqs))
            (accumulate-n f init (map cdr seqs)))))

;exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

;test
(define v (list 1 2 3))

(define m (list (list 1 2 3) (list 4 5 6)))

(display (dot-product v v))
(newline)
(display (matrix-*-vector m v))
(newline)
(display (matrix-*-matrix m (transpose m)))
(newline)
