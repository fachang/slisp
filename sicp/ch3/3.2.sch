#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-monitored f)
  (let ((cnt 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) cnt)
            ((eq? x 'reset-count) (set! cnt 0))
            (else (set! cnt (+ cnt 1))
                  (f x))))))

;test
(define s (make-monitored sqrt))

(s 100)

(s 'how-many-calls?)
