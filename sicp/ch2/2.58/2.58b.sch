#!/usr/bin/env racket
#lang planet neil/sicp

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;exercise 2.58b
(define (operator exp)
  (define (iter exp op-list)
    (cond ((null? op-list) nil)
          ((memq (car op-list) exp) (car op-list))
          (else (iter exp (cdr op-list)))))
  (iter exp '(+ * **)))

(define (elements-before x list)
  (if (eq? (car list) x)
      nil
      (cons (car list) (elements-before x (cdr list)))))

(define (elements-after x list)
  (cdr (memq x list)))

(define (expression-before x exp)
  (let ((res (elements-before x exp)))
    (if (= (length res) 1)
        (car res)
        res)))

(define (expression-after x exp)
  (let ((exp (elements-after x exp)))
    (if (= (length exp) 1)
        (car exp)
        exp)))

(define (make-sum a1 a2)
  (cond ((and (=number? a1 0)) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x) (eq? (operator x) '+))

(define (addend s) (expression-before '+ s))

(define (augend s) (expression-after '+ s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x) (eq? (operator x) '*))

(define (multiplier p) (expression-before '* p))

(define (multiplicand p) (expression-after '* p))

;exercise 2.56
;exercise 2.58b
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (exp base exponent))
        (else (list base '** exponent))))

(define (exponentiation? x) (eq? (operator x) '**))

(define (base e) (expression-before '** e))

(define (exponent e) (expression-after '** e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product (make-exponentiation
                         (base exp)
                         (make-sum (exponent exp) -1))
                        (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;test
(define (test exp var)
  (display (deriv exp var))
  (newline))

(test '(x + 3 * (x + y + z)) 'x)
(test '((x * y) ** (x + y + 3)) 'x)
