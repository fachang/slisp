#!/usr/bin/env racket
#lang planet neil/sicp

;double linked list operation table
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

;symbolic differentiation
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (exp base exponent))
        (else (list '** base exponent))))

;exercise 2.73b
(define (install-deriv-sum)
  (define (deriv-sum ops var)
    (let ((addend (car ops))
          (augend (cadr ops)))
      (make-sum (deriv addend var)
                (deriv augend var))))
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-deriv-product)
  (define (deriv-product ops var)
    (let ((multiplier (car ops))
          (multiplicand (cadr ops)))
      (make-sum
       (make-product multiplier
                     (deriv multiplicand var))
       (make-product (deriv multiplier var)
                     multiplicand))))
  (put 'deriv '* deriv-product)
  'done)

;exercise 2.73c
(define (install-deriv-exponentiation)
  (define (deriv-exponentiation ops var)
    (let ((base (car ops))
          (exponent (cadr ops)))
      (make-product
       exponent
       (make-product (make-exponentiation
                      base
                      (make-sum exponent -1))
                     (deriv base var)))))
  (put 'deriv '** deriv-exponentiation)
  'done)

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(install-deriv-sum)
(install-deriv-product)
(install-deriv-exponentiation)

;test
(define (test exp var)
  (display (deriv exp var))
  (newline))

(test '(** x 2) 'x)
(test '(* a (** x 2)) 'x)
(test '(+ (* a (** x 2)) (* b x)) 'x)
