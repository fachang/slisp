#!/usr/bin/env racket
#lang planet neil/sicp

(#%require math/number-theory)

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (display x)
  (newline))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;exercise 3.51
(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
;display 0

(stream-ref x 5)
;display 1, 2, ..., 5
;eval to 5

(stream-ref x 7)
;display 6, 7
;eval to 7

;(stream-map show (stream-enumerate-interval 0 10)) constructs the following
;stream:
;
;(cons-stream 0
;             (stream-map show (stream-enumerate-interval 1 10)))
;
;the intermediate procedure (show 0) evaluates to 0, and prints 0 on screen.
;
;(stream-ref x 5) evaluates to:
;
;(cons-stream 1
;             (stream-map show (stream-enumerate-interval 2 10)))
;
; ... ...
;
;(cons-stream 5
;             (stream-map show (stream-enumerate-interval 6 10)))
;
;this sequence of evaluations should print 1, 2, ..., 5
;on the screen, and the last stream evaluates to 5 via stream-car.
;
;(stream-ref x 7) is similar to (stream-ref x 5), as above. however, the values
;printed on the screen are only 6 and 7. the reason is that stream is actually
;constructed with (cons a (delay b)), where delay is implemented to be
;"memoized" for efficiency. that is, for
;
;(cons-stream 0
;             (stream-map show (stream-enumerate-interval 1 10)))
;
;we may take it as
;
;(cons 0 (delayed <proc>))
;
;once the CDR is evaluated, we may think it becomes
;
;(cons 0 (cons 1 (delayed <proc>)))
;
;that is, the value of evaluating (delayed <proc>) is memoized, which is
;(cons 1 (delayed <proc>)), so calling (stream-cdr (cons 0 (delayed <proc>)))
;will not print 1 again.
