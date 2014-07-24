#!/usr/bin/env racket
#lang planet neil/sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x) (cons (car s) x)) rest)))))

;test
(display (subsets (list 1 2 3)))
(newline)

;explanation
;subsets of (1 2 3) can be divided into two cases: subset contains 1 and
;subset does not contain 1. the first case is subsets of (2 3) and the
;second case is subsets of (2 3), with each subset inserting 1. this
;suggests that the original problem can be divided in two case, until
;reaching nil(the only subset is also nil), thus the above procedure works.
