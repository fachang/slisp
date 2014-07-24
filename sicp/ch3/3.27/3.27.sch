#!/usr/bin/env racket
#lang planet neil/sicp

;exercise 3.25
(define (make-table)
  (let ((table (cons '*table* (cons nil nil))))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? (caar records) key) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup keys)
      (define (iter keys table)
        (let ((key (car keys))
              (rest-keys (cdr keys)))
          (if (null? rest-keys)
              (let ((record (assoc key (cadr table))))
                (if record
                    (cdr record)
                    false))
              (let ((subtable (assoc key (cddr table))))
                (if subtable
                    (iter rest-keys subtable)
                    false)))))
      (iter keys table))
    (define (insert! keys value)
      (define (iter keys table)
        (let ((key (car keys))
              (rest-keys (cdr keys)))
          (if (null? rest-keys)
              (let ((record (assoc key (cadr table))))
                (if record
                    (set-cdr! record value)
                    (set-car! (cdr table) (cons (cons key value)
                                                 (cadr table)))))
              (let ((subtable (assoc key (cddr table))))
                (if subtable
                    (iter rest-keys subtable)
                    (begin
                      (set-cdr! (cdr table) (cons (cons key (cons nil nil))
                                                   (cddr table)))
                      (iter rest-keys (caddr table))))))))
      (iter keys table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;exercise 3.27
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((pre-result ((table 'lookup) (list x))))
        (or pre-result
            (let ((result (f x)))
              ((table 'insert!) (list x) result)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

;test
(memo-fib 10)
(memo-fib 100)
