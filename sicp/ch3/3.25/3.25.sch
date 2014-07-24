#!/usr/bin/env racket
#lang planet neil/sicp

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

;test
(define t (make-table))

((t 'insert!) '(a) 'a)
((t 'insert!) '(c) 'c)
((t 'insert!) '(a b) 'ab)
((t 'insert!) '(a b c) 'abc)
((t 'insert!) '(a a a a a) 'aaaaa)

((t 'lookup) '(a))
((t 'lookup) '(c))
((t 'lookup) '(a b))
((t 'lookup) '(a b c))
((t 'lookup) '(a a a a))
((t 'lookup) '(a a a a a))
