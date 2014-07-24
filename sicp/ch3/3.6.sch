#!/usr/bin/env racket
#lang planet neil/sicp

(define rand-init 1015568748)
(define (rand-update x)
  (let ((a 1664525)
        (b 1013904223)
        (m (expt 2 32)))
    (modulo (+ (* a x) b) m)))

(define rand
  (let ((x rand-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (set! x (rand-update x))
             x)
            ((eq? m 'reset)
             (lambda (init)
               (set! x init)
               'reset))
            (else
             (error "Unknown request -- RAND"
                    m))))))

;test
(rand 'generate)
(rand 'generate)
(rand 'generate)

((rand 'reset) 1)
(rand 'generate)
(rand 'generate)
(rand 'generate)

((rand 'reset) 1)
(rand 'generate)
(rand 'generate)
(rand 'generate)
