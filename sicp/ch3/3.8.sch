#!/usr/bin/env racket
#lang planet neil/sicp

(define f
  (let ((pre 0)
        (res 0))
    (lambda (x)
      (set! res pre)
      (set! pre x)
      res)))

;test
(+ (f 0) (f 1))    ;0
;(+ (f 1) (f 0))    ;1
