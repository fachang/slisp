#!/usr/bin/env racket
#lang planet neil/sicp

;let the rows and columns equal to
;
;            0,0
;         1,0   1,1
;      2,0   2,1   2,2
;
;in the pascal triangle

(define (pascal row column)
  (if (or (= column 0) (= column row))
      1
      (+ (pascal (- row 1) (- column 1))
         (pascal (- row 1) column))))
;test
;            0,0                        1
;         1,0   1,1                   1   1
;      2,0   2,1   2,2              1   2   1
;   3,0   3,1   3,2   3,3         1   3   3   1 
;4,0   4,1   4,2   4,3   4,4    1   4   6   4   1

(= (pascal 4 1) 4)
(= (pascal 4 2) 6)
(= (pascal 4 3) 4)
