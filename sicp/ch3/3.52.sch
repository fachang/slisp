#!/usr/bin/env racket
#lang planet neil/sicp

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

;exercise 3.52
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
sum    ;sum = 1

(define y (stream-filter even? seq))
sum    ;sum = 6
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
sum    ;sum = 10

(stream-ref y 7)
sum    ;sum = 136

(display-stream z)
sum    ;sum = 210

;if the memoized version of delay is used, we can take seq as:
;
;(stream 1 (map-s accum (stream 2 - 20))) and sum = 1
;
;when the rest of entries of seq is evaluated, the stream becomes
;
;(stream 1 3 (map-s accum (stream 3 - 20))) and sum = 3
;(stream 1 3 6 (map-s accum (stream 4 - 20))) and sum = 6
;(stream 1 3 6 10 (map-s accum (stream 5 - 20))) and sum = 10
;
;according the memoization mechanism, when stream y and z try to extract value
;from seq, the delayed procedure is forced to be evaluated, and the result is
;memoized. that is, if the value of seq is already evaluated, delay simply
;returns the evaluated result, thus sum will always follow the latest evaluated
;entry of seq.
;
;if delay without memoization is used, the result would be different. even
;displaying seq ten times prints ten different sequences.
