#!/usr/bin/env racket
#lang planet neil/sicp

(#%require "sim.sch" "cell.sch")

;test
(define a3 (make-wire))
(define a2 (make-wire))
(define a1 (make-wire))
(define b3 (make-wire))
(define b2 (make-wire))
(define b1 (make-wire))
(define s3 (make-wire))
(define s2 (make-wire))
(define s1 (make-wire))
(define c (make-wire))

(probe 's3 s3)
(probe 's2 s2)
(probe 's1 s1)

(ripple-carry-adder (list a1 a2 a3) (list b1 b2 b3) (list s1 s2 s3) c)

(set-signal! a3 1)
(set-signal! a2 0)
(set-signal! a1 1)
(set-signal! b3 0)
(set-signal! b2 0)
(set-signal! b1 1)
(propagate)

(set-signal! a3 0)
(set-signal! a2 1)
(set-signal! a1 1)
(set-signal! b3 0)
(set-signal! b2 1)
(set-signal! b1 1)
(propagate)
