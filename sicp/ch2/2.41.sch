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

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enum-interval 1 (- i 1))))
           (enum-interval 1 n)))

;exercise 1.41
(define (triple-sum-pairs n s)
  (define (triple-sum? p)
    (= (accumulate + 0 p) s))
  (define (make-pair-sum p)
    (append p (list (accumulate + 0 p))))
  (define (enum-pairs)
    (flatmap (lambda (i)
               (map (lambda (j) (append (list i) j))
                    (unique-pairs (- i 1))))
             (enum-interval 1 n)))
  (map make-pair-sum
       (filter triple-sum? (enum-pairs))))

(display (triple-sum-pairs 12 12))
(newline)
