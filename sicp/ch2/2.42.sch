#!/usr/bin/env racket
#lang planet neil/sicp

(define (filter p? seq)
  (cond ((null? seq) nil)
        ((p? (car seq)) (cons (car seq) (filter p? (cdr seq))))
        (else (filter p? (cdr seq)))))

(define (accumulate f init seq)
  (if (null? seq)
      init
      (f (car seq)
         (accumulate f init (cdr seq)))))

(define (enum-interval a b)
  (if (> a b)
      nil
      (cons a
            (enum-interval (+ a 1) b))))

(define (flatmap f seq)
  (accumulate append nil (map f seq)))

;exercise 2.42
(define (queens board-size)
  (define empty-board nil)
  (define (adjoin-position new-row col rest-of-queens)
    (cons new-row rest-of-queens))
  (define (safe? col positions)
    (let ((kth-row (car positions))
          (kth-col col))
      (define (iter col rest)
        (or (null? rest)
            (let ((row (car rest)))
              (and (not (= kth-row row))
                   (not (= (abs (- kth-row row)) (- kth-col col)))
                   (iter (- col 1) (cdr rest))))))
      (iter (- kth-col 1) (cdr positions))))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enum-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;test
(display (queens 4))
(newline)
(display (queens 5))
(newline)
