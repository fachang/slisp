#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-table)
  (define table nil)
  
  (define (get index1 index2)
    (define (iter-index1 table)
      (cond ((null? table) false)
            ((equal? (caar table) index1) (iter-index2 (cdar table)))
            (else (iter-index1 (cdr table)))))
    (define (iter-index2 op-list)
      (cond ((null? op-list) false)
            ((equal? (caar op-list) index2) (cadar op-list))
            (else (iter-index2 (cdr op-list)))))
    (iter-index1 table))
  
  (define (put! index1 index2 item)
    (define (iter-index1 table)
      (cond ((null? table) (list (cons index1 (iter-index2 nil))))
            ((equal? (caar table) index1) 
             (cons (cons index1 (iter-index2 (cdar table))) (cdr table)))
            (else (cons (car table) (iter-index1 (cdr table))))))
    (define (iter-index2 op-list)
      (cond ((null? op-list) (list (list index2 item)))
            ((equal? (caar op-list) index2)
             (cons (list index2 item) (cdr op-list)))
            (else (cons (car op-list) (iter-index2 (cdr op-list))))))
    (set! table (iter-index1 table)))
  
  (lambda (op)
    (cond ((eq? op 'get) get)
          ((eq? op 'put!) put!)
          (else (error "Unknow operation -- TABLE" op)))))

(define op-table (make-table))

(define (get op type) 
  ((op-table 'get) op type))

(define (put op type item)
  ((op-table 'put!) op type item))

(define coercion-table (make-table))

(define (get-coercion type1 type2)
  ((coercion-table 'get) type1 type2))

(define (put-coercion type1 type2 item)
  ((coercion-table 'put!) type1 type2 item))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (define (convert-to type seq)
    (let ((types (map type-tag seq)))
      (let ((trans (map (lambda (t)
                          (if (eq? t type)
                              (lambda (x) x)
                              (get-coercion t type)))
                        types)))
        (if (memq false trans)
            false
            (map (lambda (x y) (apply x (list y))) trans seq)))))
  
  (define (coercion-apply-generic type-tags)
    (if (null? type-tags)
        (error "No method for these types"
               (list op (map type-tag args)))
        (let ((converted (convert-to (car type-tags) args)))
          (if converted
              (let ((proc (get op (map type-tag converted))))
                (if proc
                    (apply proc (map contents converted))
                    (coercion-apply-generic (cdr type-tags))))
              (coercion-apply-generic (cdr type-tags))))))
  
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (coercion-apply-generic type-tags)))))

;test
(put-coercion 'a 'c (lambda (x) (attach-tag 'c (contents x))))
(put-coercion 'b 'c (lambda (x) (attach-tag 'c (contents x))))
(put 'op '(c c c) *)

(apply-generic 'op (cons 'a 1) (cons 'a 2) (cons 'c 3))
(apply-generic 'op (cons 'c 1) (cons 'b 2) (cons 'a 3))
(apply-generic 'op (cons 'a 1) (cons 'b 2) (cons 'b 3))

;assume there is operation f for '(type1 type2 type2), and type1 can be
;converted to type2. consider (apply-generic 'f type1 type1 type2),
;apply-generic will try to call (f type2 type2 type2), but not 
;(f type1 type2 type2), thus an error will occur.
