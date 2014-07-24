#!/usr/bin/env racket
#lang planet neil/sicp

(define (same-parity n . l)
    (let ((r (remainder n 2)))
      (define (iter l)
        (cond ((null? l) nil)
              ((= (remainder (car l) 2) r)
               (cons (car l) (iter (cdr l))))
              (else (iter (cdr l)))))
      (iter l)))

;test
(display (same-parity 1 2 3 4 5 6 7))
(newline)
(display (same-parity 2 3 4 5 6 7 8))
(newline)
