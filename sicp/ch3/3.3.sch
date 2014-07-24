#!/usr/bin/env racket
#lang planet neil/sicp

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p password)) (lambda x "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else "Unknown request -- MAKE-ACCOUNT"
                m)))
  dispatch)

;test
(define acc (make-account 600 'secret))

((acc 'secret 'deposit) 100)
((acc 'ssssss 'deposit) 100)
((acc 'secret 'withdraw) 300)
((acc 'ssssss 'withdrae) 300)
((acc 'secret 'withdraw) 600)
