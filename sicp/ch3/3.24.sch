#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-table same-key?)
  (let ((table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? (caar records) key) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key)
      (let ((record (assoc key (cdr table))))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record value)
            (set-cdr! table
                      (cons (cons key value) (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;test
(define t (make-table (lambda (x y) (< (abs (- x y)) 
                                       0.01))))

((t 'insert!) 1.34 'david)
((t 'insert!) 2.05 'john)

((t 'lookup) 1.345)
((t 'lookup) 1.35)
((t 'lookup) 2.00)
