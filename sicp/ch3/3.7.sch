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

(define (make-joint account old-password new-password)
  (define (dispatch p m)
    (cond ((not (eq? p new-password)) (lambda x "Incorrect password"))
          (else (account old-password m))))
  dispatch)

;test
(define peter-acc 
  (make-account 1000 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'withdraw) 50)

((paul-acc 'open-sesame 'withdraw) 50)
((paul-acc 'rosebud 'withdraw) 50)

((peter-acc 'open-sesame 'withdraw) 50)
